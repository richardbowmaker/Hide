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
    closeEditor,
    newFile
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
import qualified EditorNotebook as EN
import qualified Misc as MI
import qualified Scintilla as SC
import qualified ScintillaConstants as SC
import qualified Session as SS

-- updates the enabled state of the Save, SaveAs and SaveAll menus                                
updateSaveMenus :: SS.Session -> IO ()   
updateSaveMenus ss = do
 
    tws <- SS.twGetWindows ss
    
    if (length tws > 0) then do
    
        ic <- EN.enbSelectedSourceFileIsClean ss       
        set (SS.ssMenuListGet ss CN.menuFileSave)      [enabled := not ic]        
        set (SS.ssMenuListGet ss CN.menuFileSaveAs)    [enabled := True]       
        b <- allFilesClean tws
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
       
    where allFilesClean tws = MI.doWhileTrueIO (\tw -> SS.twIsClean tw) tws
   
closeEditor :: SS.Session -> SS.TextWindow -> SC.ScnEditor -> IO Bool
closeEditor ss tw scn = do
    ic <- SS.twIsClean tw           
    if ic then do                       
        closeTab ss tw scn        
        return True
    else do            
        -- file is dirty so prompt the user if they want to save it
        EN.enbSelectTab ss tw
        b <- confirmDialog (SS.ssFrame ss) 
            CN.programTitle 
            ("Do you wish to save the file: ?\n" ++ (show $ SS.twFilePathToString tw))
            True                        
        if b then do                   
           -- save file
            b <- fileSave ss tw                  
            if b then do                   
                closeTab ss tw scn        
                return True
            -- veto close, don't know how to do this yet ??
            else return False            
        else do
            closeTab ss tw scn
            return True
          
closeTab :: SS.Session -> SS.TextWindow -> SC.ScnEditor -> IO ()
closeTab ss tw scn = do   
    -- close down scintilla editor
    SC.scnDisableEvents scn
    SC.scnClose scn    
    -- remove source file from project
    SS.twRemoveWindow ss tw    
    -- update status bar
    updateStatus ss    
    return ()

fileOpen :: SS.Session -> (SS.TextWindow -> SC.SCNotification -> IO ()) -> String -> IO ()
fileOpen ss callback fp = do
    mtw <- SS.twGetSourceFileWindow ss fp
    case mtw of
        Just tw -> setSourceFileFocus ss fp -- just set focus to editor
        Nothing -> do       
            -- existing file so add to list, create window and set focus
            (tw, scn) <- openSourceFileEditor ss fp callback 
            writeSourceFileEditor ss tw scn         

fileCloseAll :: SS.Session -> IO ()
fileCloseAll ss = do 
    SS.twGetWindows ss >>= MI.doWhileTrueIO (fileClose ss)
    updateSaveMenus ss    
    return ()

fileClose :: SS.Session -> SS.TextWindow -> IO Bool
fileClose ss tw = do
    case SS.twGetEditor tw of
        Just scn -> do
            b <- closeEditor ss tw scn   
            if b then do   
                -- remove page from notebook
                mix <- EN.enbGetTabIndex ss tw 
                case mix of
                    Just ix -> do
                        let nb = SS.ssEditors ss        
                        auiNotebookDeletePage nb ix  
                        return True
                    Nothing -> do
                        SS.ssDebugError ss "fileClose, no tab for source file"
                        return True       
            else return False
        Nothing -> do
            SS.ssDebugError ss "fileClose, text window does not have scintilla editor"
            return False
    
fileSaveAll :: SS.Session -> IO (Bool)
fileSaveAll ss = do
    b <- SS.twGetWindows ss >>= MI.doWhileTrueIO (fileSave ss)
    return b

-- if file is dirty then writes it to file
-- if no filename has been set then file save as is called
-- returns false if user cancelled        
fileSave :: SS.Session -> SS.TextWindow -> IO Bool
fileSave ss tw = do    
    case SS.twGetEditor tw of
        Just scn -> do
            ic <- SS.twIsClean tw   
            if ic then return True
            else       
                case SS.twFilePath tw of
                    Just fp -> do
                        writeSourceFile ss tw
                        return True            
                    -- source file has no name, so prompt user for one
                    Nothing -> do
                        b <- fileSaveAs ss tw
                        return b
        Nothing -> do
            SS.ssDebugError ss "fileSave, text window does not have scintilla editor"
            return False
                   
-- File Save As, returns False if user opted to cancel the save 
fileSaveAs :: SS.Session -> SS.TextWindow -> IO Bool
fileSaveAs ss tw = do   
    -- ensure source file is displayed
    EN.enbSelectTab ss tw   
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
            case SS.twSetFilePath tw fp of
                Just tw' -> do
                    SS.twUpdateWindow ss tw'
                    writeSourceFile ss tw'                   
                    -- update tab name
                    EN.enbSetTabText ss tw'
                    return True 
                Nothing -> do
                    SS.ssDebugError ss "fileSaveAs:: twSetFilePath returned nothing"
                    return True
        --wxID_CANCEL -> do
        5101 -> do
            return False           
        otherwise  -> do
            return True
  
-- writes file to disk and sets editor to clean
writeSourceFile :: SS.Session -> SS.TextWindow -> IO ()
writeSourceFile ss tw = do
    case SS.twGetEditor tw of
        Just scn -> do
            case SS.twFilePath tw of
                Just fp -> do
                    SC.scnGetAllText scn >>= BS.writeFile fp
                    SC.scnSetSavePoint scn
                    return ()       
                Nothing -> do
                    -- bug, shouldn't end up here
                    SS.ssDebugError ss "writeSourceFile:: invalid text window, no source file name"
                    return ()
        Nothing -> do
            SS.ssDebugError ss "writeSourceFile, text window does not have scintilla editor"
            return ()

-- display line count, cursor position etc. 
updateStatus :: SS.Session -> IO ()   
updateStatus ss = do
    let st = SS.ssStatus ss
    tws <- SS.twGetWindows ss
    if (length tws > 0) then do
        mtw <- EN.enbGetSelectedSourceFile ss
        case mtw of
            Just tw -> do
                case SS.twGetEditor tw of
                    Just scn -> do
                        (l, lp, dp, lc, cc) <- SC.scnGetPositionInfo scn
                        set st [text:= (printf "Line: %d Col: %d, Lines: %d Pos: %d Size: %d" (l+1) (lp+1) lc dp cc)]
                        return ()
                    Nothing -> do
                        SS.ssDebugError ss "updateStatus:: text window did not have a scintilla editor"
            Nothing -> do
                set st [text:= ""]
                return ()           
    else do
        set st [text:= ""]
        return ()           
    
writeSourceFileEditor :: SS.Session -> SS.TextWindow -> SC.ScnEditor -> IO ()
writeSourceFileEditor ss tw scn = do
    case SS.twFilePath tw of
        Just fp -> do
            text <- BS.readFile fp
            SC.scnSetText scn text
            SC.scnSetSavePoint scn
            return ()
        Nothing -> SS.ssDebugError ss "writeSourceFileEditor:: text window did not have a file name"

setSourceFileFocus :: SS.Session -> String -> IO ()
setSourceFileFocus ss fp = do
    mtw <- SS.twGetSourceFileWindow ss fp
    case mtw of
        Just tw -> do
            let nb = SS.ssEditors ss
            ix <- auiNotebookGetPageIndex nb $ SS.twPanel tw
            auiNotebookSetSelection nb ix 
            return ()
        Nothing -> return ()

openSourceFileEditor :: SS.Session -> String -> (SS.TextWindow -> SC.SCNotification -> IO ()) -> IO (SS.TextWindow, SC.ScnEditor)
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
    SS.twUpdate ss (\tws -> tw : tws)

    -- enable events
    SC.scnSetEventHandler scn $ callback tw
    SC.scnEnableEvents scn
         
    -- set focus to new page
    ix <- auiNotebookGetPageIndex nb p
    auiNotebookSetSelection nb ix 

    return (tw, scn)

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
                                (SC.scnIsClean scn)
                                (Just fp))

newFile :: SS.Session -> (SS.TextWindow -> SC.SCNotification -> IO ()) -> IO SS.TextWindow  
newFile ss callback = do
    
    let nb = SS.ssEditors ss

    -- create panel with scintilla editor inside
    p <- panel nb []
    hwnd <- windowGetHandle p
    scn <- SC.scnCreateEditor hwnd
    SC.scnConfigureHaskell scn

    -- add panel to notebook
    auiNotebookAddPage nb p "..." False 0

    let tw = newtw scn p hwnd (SC.scnGetHwnd scn)
    SS.twUpdate ss (\tws -> tw : tws)

    -- enable events
    SC.scnSetEventHandler scn $ callback tw
    SC.scnEnableEvents scn
    SC.scnSetSavePoint scn
      
    return tw
    
    where   newtw scn panel hwndp hwnd = (SS.createTextWindow
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
                                (SC.scnIsClean scn)
                                Nothing)