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
import Scintilla
import ScintillaConstants
import Session

-- updates the enabled state of the Save, SaveAs and SaveAll menus                                
updateSaveMenus :: Session -> IO ()   
updateSaveMenus ss = do
 
    fs <- ssReadSourceFiles ss
    
    if (length fs > 0) then do
    
        ic <- enbSelectedSourceFileIsClean ss       
        set (ssMenuListGet ss CN.menuFileSave)      [enabled := not ic]        
        set (ssMenuListGet ss CN.menuFileSaveAs)    [enabled := True]       
        b <- allFilesClean fs
        set (ssMenuListGet ss CN.menuFileSaveAll)   [enabled := not b]
        set (ssMenuListGet ss CN.menuFileClose)     [enabled := True]
        set (ssMenuListGet ss CN.menuFileCloseAll)  [enabled := True]    
        return ()
        
    else do
    
        set (ssMenuListGet ss CN.menuFileSave)      [enabled := False]
        set (ssMenuListGet ss CN.menuFileSaveAs)    [enabled := False]
        set (ssMenuListGet ss CN.menuFileClose)     [enabled := False]
        set (ssMenuListGet ss CN.menuFileCloseAll)  [enabled := False]
        set (ssMenuListGet ss CN.menuFileSaveAll)   [enabled := False]
        return ()
       
    where allFilesClean fs = do
            b <- doWhileTrueIO (\sf -> scnIsClean $ sfEditor sf) fs
            return (b)
    
closeEditor :: Session -> SourceFile -> IO Bool
closeEditor ss sf = do

    let e = sfEditor sf      
    ic <- scnIsClean e
    
    if ic then do
                
        closeTab ss sf         
        return (True)

    else do
    
        -- file is dirty so prompt the user if they want to save it
        enbSelectTab ss sf
        b <- confirmDialog (ssFrame ss) 
                "Heyho" 
                ("Do you wish to save the file: ?\n" ++ (show $ sfFilePathString sf))
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
          
closeTab :: Session -> SourceFile -> IO ()
closeTab ss sf = do
    
    -- close down scintilla editor
    let e = sfEditor sf
    scnDisableEvents e
    scnClose e
    
    -- remove source file from project
    prUpdate ss (\pr -> prCreate (findAndRemove (sfIsSame sf) (prFiles pr)))
    
    -- update status bar
    updateStatus ss
    
    return ()

fileOpen :: Session -> (SCNotification -> IO ()) -> String -> IO ()
fileOpen ss callback fp = do

    fs <- ssReadSourceFiles ss
    b <- ssIsOpeningState fs
   
    if b then do

        -- set 1st slot
        let sf' = sfSetFilePath (head fs) fp
        let nb = ssEditors ss
        writeSourceFileEditor sf'
        auiNotebookSetPageText nb 0 (takeFileName fp)
        prUpdate ss (\_ -> prCreate [sf'])
        return ()          
    
    else do
    
        if (sfIsInList fp fs) then do
           
            -- if already in file list then just switch focus to editor
            setSourceFileFocus ss fp
            return ()
            
         else do

            -- existing file so add to list, create window and set focus
            sf' <- openSourceFileEditor ss fp callback
            writeSourceFileEditor sf'
            return ()          


fileCloseAll :: Session -> IO ()
fileCloseAll ss = do 
    ssReadSourceFiles ss >>= doWhileTrueIO (fileClose ss)
    updateSaveMenus ss    
    EM.updateEditMenus ss
    return ()

fileClose :: Session -> SourceFile -> IO Bool
fileClose ss sf = do

    b <- closeEditor ss sf
    
    if b then do
    
        -- remove page from notebook
        mix <- enbGetTabIndex ss sf 
        case mix of
            Just ix -> do
                let nb = ssEditors ss        
                auiNotebookDeletePage nb ix  
                return (True)
            Nothing -> do
                ssDebugError ss "fileClose, no tab for source file"
                return (True)
        
    else return (False)
    
fileSaveAll :: Session -> IO (Bool)
fileSaveAll ss = do
        b <- ssReadSourceFiles ss >>= doWhileTrueIO (fileSave ss)
        return (b)

-- if file is dirty then writes it to file
-- if no filename has been set then file save as is called
-- returns false if user cancelled        
fileSave :: Session -> SourceFile -> IO Bool
fileSave ss sf = do

    let e = sfEditor sf      
    ic <- scnIsClean e
    
    if (ic) then do
        return (True)
    else       
        case (sfFilePath sf) of
            Just fp -> do
                writeSourceFile sf
                return (True)
            
            -- source file has no name, so prompt user for one
            Nothing -> do
                b <- fileSaveAs ss sf
                return (b)
                   
-- File Save As, returns False if user opted to cancel the save 
fileSaveAs :: Session -> SourceFile -> IO Bool
fileSaveAs ss sf = do
   
    -- ensure source file is displayed
    enbSelectTab ss sf
    
    -- prompt user for name to save to
    let mf = ssFrame ss                   -- wxFD_SAVE wxFD_OVERWRITE_PROMPT
    fd <- fileDialogCreate mf "Save file as" "." "" "*.hs" (Point 100 100) 0x6
    rs <- dialogShowModal fd 
    
    case rs of
--        wxID_OK -> do
-- ?? don't know how to fix pattern match against a function    
        5100 -> do    
            fp <- fileDialogGetPath fd
            
            -- save new name to mutable project data
            let sf' = sfSetFilePath sf fp
            sfUpdate ss sf'
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
writeSourceFile :: SourceFile -> IO ()
writeSourceFile sf = do
    let e = sfEditor sf
    case (sfFilePath sf) of
        Just fp -> do
            scnGetAllText e >>= BS.writeFile fp
            scnSetSavePoint e
            return ()
        
        Nothing -> do
            -- bug, shouldn't end up here
            return ()
            
-- display line count, cursor position etc. 
updateStatus :: Session -> IO ()   
updateStatus ss = do
    let st = ssStatus ss
    fs <- ssReadSourceFiles ss
    if (length fs > 0) then do
        sf <- enbGetSelectedSourceFile ss   
        (l, lp, dp, lc, cc) <- scnGetPositionInfo $ sfEditor sf
        set st [text:= (printf "Line: %d Col: %d, Lines: %d Pos: %d Size: %d" (l+1) (lp+1) lc dp cc)]
        return ()
    else do
        set st [text:= ""]
        return ()           
    
writeSourceFileEditor :: SourceFile -> IO ()
writeSourceFileEditor sf = do
    let mfp = sfFilePath sf
    case mfp of
        Just fp -> do
            text <- BS.readFile fp
            let e = sfEditor sf
            scnSetText e text
            scnSetSavePoint e
            return ()
        Nothing -> return () -- error

setSourceFileFocus :: Session -> String -> IO ()
setSourceFileFocus ss fp = do
    msf <- sfGetSourceFile ss fp
    case (msf) of
        Just sf -> do
            let nb = ssEditors ss
            ix <- auiNotebookGetPageIndex nb $ sfPanel sf
            auiNotebookSetSelection nb ix 
            return ()
        Nothing -> return ()


openSourceFileEditor :: Session -> String -> (SCNotification -> IO ()) -> IO (SourceFile)
openSourceFileEditor ss fp callback = do

    let nb = ssEditors ss

    -- create panel with scintilla editor inside
    p <- panel nb []
    hwnd <- windowGetHandle p
    scn <- scnCreateEditor hwnd
    scnConfigureHaskell scn
    scn' <- scnSetEventHandler scn callback
    scnEnableEvents scn'

    -- add panel to notebook
    auiNotebookAddPage nb p (takeFileName fp) False 0
    ta <- auiSimpleTabArtCreate
    auiNotebookSetArtProvider nb ta

    -- add source file to project
    sf <- sfCreate p scn' (Just fp) Nothing
    prUpdate ss (\pr -> prSetFiles pr (sf:(prFiles pr)))
          
    -- set focus to new page
    ix <- auiNotebookGetPageIndex nb p
    auiNotebookSetSelection nb ix 

    return (sf) 
  