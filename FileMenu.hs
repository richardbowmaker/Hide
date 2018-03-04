module FileMenu
(
    updateSaveMenus,
    openSourceFileEditor,
    setSourceFileFocus,
    writeSourceFileEditor,
    updateStatus,
    writeSourceFile,
    fileSaveAs,         
    fileSave,
    fileSaveAll,
    fileClose,
    fileCloseAll,
    fileOpen,
    closeTab,
    closeEditor
) where 
    
import Control.Concurrent 
import Control.Concurrent.STM
import Control.Monad (liftM) 
import qualified Data.ByteString.Char8 as BS (ByteString, hGetLine, readFile, pack, putStrLn, writeFile)
import qualified Data.ByteString as BS (append)
import Data.List (find, findIndex)
import Data.Word (Word64)
import Foreign.C.String (CString, withCString)
import Graphics.Win32.GDI.Types (HWND)
import Graphics.UI.WX
import Graphics.UI.WXCore
import System.FilePath.Windows (takeFileName)
import System.IO
import Text.Printf (printf)

-- project imports

import qualified Constants as CN
import qualified EditMenu as EM
import EditorNotebook
import Misc
import qualified Scintilla as SC
import ScintillaConstants 
import qualified Session as SS

-- updates the enabled state of the Save, SaveAs and SaveAll menus                                
updateSaveMenus :: SS.Session -> IO ()   
updateSaveMenus ss = do
 
    fs <- SS.ssReadSourceFiles ss
    
    if (length fs > 0) then do
    
        ic <- enbSelectedSourceFileIsClean ss       
        set (SS.ssMenuListGet ss CN.menuFileSave)      [enabled := not ic]        
        set (SS.ssMenuListGet ss CN.menuFileSaveAs)    [enabled := True]       
        b <- allFilesClean fs
        set (SS.ssMenuListGet ss CN.menuFileSaveAll)   [enabled := not b]
        set (SS.ssMenuListGet ss CN.menuFileClose)     [enabled := True]
        set (SS.ssMenuListGet ss CN.menuFileCloseAll)  [enabled := True]    
        return ()
        
    else do
    
        set (SS.ssMenuListGet ss CN.menuFileSave)      [enabled := False]
        set (SS.ssMenuListGet ss CN.menuFileSaveAs)    [enabled := False]
        set (SS.ssMenuListGet ss CN.menuFileClose)     [enabled := False]
        set (SS.ssMenuListGet ss CN.menuFileCloseAll)  [enabled := False]
        set (SS.ssMenuListGet ss CN.menuFileSaveAll)   [enabled := False]
        return ()
       
    where allFilesClean fs = do
            b <- doWhileTrueIO (\sf -> SC.scnIsClean $ SS.sfEditor sf) fs
            return (b)
    
closeEditor :: SS.Session -> SS.SourceFile -> IO Bool
closeEditor ss sf = do

    let e = SS.sfEditor sf      
    ic <- SC.scnIsClean e
    
    if ic then do
                
        closeTab ss sf         
        return (True)

    else do
    
        -- file is dirty so prompt the user if they want to save it
        enbSelectTab ss sf
        b <- confirmDialog (SS.ssFrame ss) 
                "Heyho" 
                ("Do you wish to save the file: ?\n" ++ (show $ SS.sfFilePathString sf))
                True
                
        if b then do
            
           -- save file
            b <- fileSave ss sf
            
            if b then do
            
                closeTab ss sf         
                return (True)

            -- veto close, don't know how to do this yet ??
            else return (False) 
    
        else do
            closeTab ss sf
            return (True)
          
closeTab :: SS.Session -> SS.SourceFile -> IO ()
closeTab ss sf = do
    
    -- close down scintilla editor
    let e = SS.sfEditor sf
    SC.scnDisableEvents e
    SC.scnClose e
    
    -- remove source file from project
    SS.prUpdate ss (\pr -> SS.prCreate (findAndRemove (SS.sfIsSame sf) (SS.prFiles pr)))
    
    -- update status bar
    updateStatus ss
    
    return ()

fileOpen :: SS.Session -> (SS.TextWindow -> SC.SCNotification -> IO ()) -> String -> IO ()
fileOpen ss callback fp = do

    fs <- SS.ssReadSourceFiles ss
    b <- SS.ssIsOpeningState fs
   
    if b then do

        -- set 1st slot
        let sf' = SS.sfSetFilePath (head fs) fp
        let nb = SS.ssEditors ss
        writeSourceFileEditor sf'
        auiNotebookSetPageText nb 0 (takeFileName fp)
        SS.prUpdate ss (\_ -> SS.prCreate [sf'])
        return ()          
    
    else do
    
        if (SS.sfIsInList fp fs) then do
           
            -- if already in file list then just switch focus to editor
            setSourceFileFocus ss fp
            return ()
            
         else do

            -- existing file so add to list, create window and set focus
            sf' <- openSourceFileEditor ss fp callback 
            writeSourceFileEditor sf'
            return ()          


fileCloseAll :: SS.Session -> IO ()
fileCloseAll ss = do 
    SS.ssReadSourceFiles ss >>= doWhileTrueIO (fileClose ss)
    updateSaveMenus ss    
    EM.updateEditMenus ss
    return ()

fileClose :: SS.Session -> SS.SourceFile -> IO Bool
fileClose ss sf = do

    b <- closeEditor ss sf
    
    if b then do
    
        -- remove page from notebook
        mix <- enbGetTabIndex ss sf 
        case mix of
            Just ix -> do
                let nb = SS.ssEditors ss        
                auiNotebookDeletePage nb ix  
                return (True)
            Nothing -> do
                SS.ssDebugError ss "fileClose, no tab for source file"
                return (True)
        
    else return (False)
    
fileSaveAll :: SS.Session -> IO (Bool)
fileSaveAll ss = do
        b <- SS.ssReadSourceFiles ss >>= doWhileTrueIO (fileSave ss)
        return (b)

-- if file is dirty then writes it to file
-- if no filename has been set then file save as is called
-- returns false if user cancelled        
fileSave :: SS.Session -> SS.SourceFile -> IO Bool
fileSave ss sf = do

    let e = SS.sfEditor sf      
    ic <- SC.scnIsClean e
    
    if (ic) then do
        return (True)
    else       
        case (SS.sfFilePath sf) of
            Just fp -> do
                writeSourceFile sf
                return (True)
            
            -- source file has no name, so prompt user for one
            Nothing -> do
                b <- fileSaveAs ss sf
                return (b)
                   
-- File Save As, returns False if user opted to cancel the save 
fileSaveAs :: SS.Session -> SS.SourceFile -> IO Bool
fileSaveAs ss sf = do
   
    -- ensure source file is displayed
    enbSelectTab ss sf
    
    -- prompt user for name to save to
    let mf = SS.ssFrame ss                   -- wxFD_SAVE wxFD_OVERWRITE_PROMPT
    fd <- fileDialogCreate mf "Save file as" "." "" "*.hs" (Point 100 100) 0x6
    rs <- dialogShowModal fd 
    
    case rs of
--        wxID_OK -> do
-- ?? don't know how to fix pattern match against a function    
        5100 -> do    
            fp <- fileDialogGetPath fd
            
            -- save new name to mutable project data
            let sf' = SS.sfSetFilePath sf fp
            SS.sfUpdate ss sf'
            writeSourceFile sf'
            
            -- update tab name
            enbSetTabText ss sf'
            return (True)
            
        --wxID_CANCEL -> do
        5101 -> do
            return (False)
            
        otherwise  -> do
            return (True)
  
-- writes file to disk and sets editor to clean
writeSourceFile :: SS.SourceFile -> IO ()
writeSourceFile sf = do
    let e = SS.sfEditor sf
    case (SS.sfFilePath sf) of
        Just fp -> do
            SC.scnGetAllText e >>= BS.writeFile fp
            SC.scnSetSavePoint e
            return ()
        
        Nothing -> do
            -- bug, shouldn't end up here
            return ()
            
-- display line count, cursor position etc. 
updateStatus :: SS.Session -> IO ()   
updateStatus ss = do
    let st = SS.ssStatus ss
    fs <- SS.ssReadSourceFiles ss
    if (length fs > 0) then do
        sf <- enbGetSelectedSourceFile ss   
        (l, lp, dp, lc, cc) <- SC.scnGetPositionInfo $ SS.sfEditor sf
        set st [text:= (printf "Line: %d Col: %d, Lines: %d Pos: %d Size: %d" (l+1) (lp+1) lc dp cc)]
        return ()
    else do
        set st [text:= ""]
        return ()           
    
writeSourceFileEditor :: SS.SourceFile -> IO ()
writeSourceFileEditor sf = do
    let mfp = SS.sfFilePath sf
    case mfp of
        Just fp -> do
            text <- BS.readFile fp
            let e = SS.sfEditor sf
            SC.scnSetText e text
            SC.scnSetSavePoint e
            return ()
        Nothing -> return () -- error

setSourceFileFocus :: SS.Session -> String -> IO ()
setSourceFileFocus ss fp = do
    msf <- SS.sfGetSourceFile ss fp
    case (msf) of
        Just sf -> do
            let nb = SS.ssEditors ss
            ix <- auiNotebookGetPageIndex nb $ SS.sfPanel sf
            auiNotebookSetSelection nb ix 
            return ()
        Nothing -> return ()


openSourceFileEditor :: SS.Session -> String -> (SS.TextWindow -> SC.SCNotification -> IO ()) -> IO (SS.SourceFile)
openSourceFileEditor ss fp callback = do

    let nb = SS.ssEditors ss

    -- create panel with scintilla editor inside
    p <- panel nb []
    hwnd <- windowGetHandle p
    scn <- SC.scnCreateEditor hwnd
    SC.scnConfigureHaskell scn

    -- add panel to notebook
    auiNotebookAddPage nb p (takeFileName fp) False 0
    ta <- auiSimpleTabArtCreate
    auiNotebookSetArtProvider nb ta

    -- add text window to project
    let tw = newtw scn p hwnd (SC.scnGetHwnd scn) fp
    SS.twUpdate ss (\tws -> SS.twCreate (tw : (SS.txWindows tws)))

    scn' <- SC.scnSetEventHandler scn $ callback tw
    SC.scnEnableEvents scn'
        
    -- add source file to project
    sf <- SS.sfCreate p scn' (Just fp) Nothing
    SS.prUpdate ss (\pr -> SS.prSetFiles pr (sf:(SS.prFiles pr)))

    -- set focus to new page
    ix <- auiNotebookGetPageIndex nb p
    auiNotebookSetSelection nb ix 

    return (sf) 

    where   newtw scn panel hwndp hwnd fp = (SS.createTextWindow
                                (SS.createSourceWindowType scn)
                                panel
                                hwndp
                                hwnd
                                [   
                                    (SS.createMenuFunction CN.menuEditUndo          (SC.scnUndo scn)                (SC.scnCanUndo scn)),
                                    (SS.createMenuFunction CN.menuEditRedo          (SC.scnRedo scn)                (SC.scnCanRedo scn)),
                                    (SS.createMenuFunction CN.menuEditCut           (SC.scnCut scn)                 (liftM not $ SC.scnSelectionIsEmpty scn)),
                                    (SS.createMenuFunction CN.menuEditCopy          (SC.scnCopy scn)                (liftM not $ SC.scnSelectionIsEmpty scn)),
                                    (SS.createMenuFunction CN.menuEditPaste         (SC.scnPaste scn)               (SC.scnCanPaste scn)),
                                    (SS.createMenuFunction CN.menuEditSelectAll     (SC.scnSelectAll scn)           (return True)),
                                    (SS.createMenuFunction CN.menuEditFind          (EM.editFind ss scn)            (return True)),
                                    (SS.createMenuFunction CN.menuEditFindForward   (EM.editFindForward ss scn)     (return True)),
                                    (SS.createMenuFunction CN.menuEditFindBackward  (EM.editFindBackward ss scn)    (return True))
                                ]
                                (SC.scnGetFocus scn)
                                (Just fp))
