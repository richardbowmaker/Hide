module FileMenu
(
    closeTab,
    closeEditor,
    onFileOpen,
    onFileNew,
    fileCloseAll,
    fileSave,
    fileOpen
) where 
    
import Control.Concurrent 
import Control.Concurrent.STM
import Control.Monad (liftM, liftM2)
import Data.Bits ((.&.), (.|.)) 
import qualified Data.ByteString.Char8 as BS (ByteString, hGetLine, readFile, pack, putStrLn, unpack, writeFile)
import qualified Data.ByteString as BS (append)
import Data.List (find, findIndex)
import Data.Word (Word64)
import Foreign.C.String (CString, withCString)
import Graphics.Win32.GDI.Types (HWND)
import Graphics.UI.WX
import Graphics.UI.WXCore
import System.FilePath.Windows (takeFileName, takeDirectory)
import System.IO
import Text.Printf (printf)

-- project imports

import qualified Compile as CP
import qualified Constants as CN
import qualified EditMenu as EM
import qualified EditorNotebook as EN
import qualified Ghci as GH
import qualified Misc as MI
import qualified Scintilla as SC
import qualified ScintillaProxyImports as SI
import qualified Session as SS

openSourceFileEditor :: SS.Session -> String -> IO (SS.HideWindow, SC.Editor)
openSourceFileEditor ss fp = do

    let nb = SS.ssEditors ss

    -- create panel with scintilla editor inside
    p <- panel nb []
    hwnd <- windowGetHandle p
    scn <- SC.createEditor hwnd
    SC.configureHaskell scn

    -- add panel to notebook
    auiNotebookAddPage nb p (takeFileName fp) False 0
    ta <- auiSimpleTabArtCreate
    auiNotebookSetArtProvider nb ta

    -- add text window to project
    hw <- createHideWindow ss scn p hwnd (SC.getHwnd scn) (Just fp)
    SS.hwUpdate ss (\hws -> hw : hws)

    -- enable events
    SC.setEventHandler scn $ scnCallback ss hw
    SC.enableEvents scn
    SC.setMarginSensitive scn CN.symbolMargin True
         
    -- set focus to new page
    ix <- auiNotebookGetPageIndex nb p
    auiNotebookSetSelection nb ix 

    createPopupMenu scn
    return (hw, scn)

createHideWindow :: SS.Session -> SC.Editor -> Panel() -> HWND -> HWND -> Maybe String -> IO SS.HideWindow
createHideWindow ss scn panel phwnd hwnd mfp = do
    let tw = SS.createTextWindow (SS.createSourceWindowType scn) panel phwnd hwnd mfp
    return $ SS.createHideWindow tw (tms tw)

    where   tms tw =

                SS.createTextMenus                    
                    [
                        (SS.createMenuFunction CN.menuFileClose         (onFileClose ss tw scn)         (return True)),
                        (SS.createMenuFunction CN.menuFileCloseAll      (onFileCloseAll ss)             (return True)),
                        (SS.createMenuFunction CN.menuFileSave          (onFileSave ss tw scn)          (liftM not $ SC.isClean scn)),
                        (SS.createMenuFunction CN.menuFileSaveAs        (onFileSaveAs ss tw scn)        (return True)),
                        (SS.createMenuFunction CN.menuFileSaveAll       (onFileSaveAll ss)              (liftM not $ allFilesClean ss)),
                        (SS.createMenuFunction CN.menuEditUndo          (SC.undo scn)                   (SC.canUndo scn)),
                        (SS.createMenuFunction CN.menuEditRedo          (SC.redo scn)                   (SC.canRedo scn)),
                        (SS.createMenuFunction CN.menuEditCut           (SC.cut scn)                    (liftM not $ SC.selectionIsEmpty scn)),
                        (SS.createMenuFunction CN.menuEditCopy          (SC.copy scn)                   (liftM not $ SC.selectionIsEmpty scn)),
                        (SS.createMenuFunction CN.menuEditPaste         (SC.paste scn)                  (SC.canPaste scn)),
                        (SS.createMenuFunction CN.menuEditSelectAll     (SC.selectAll scn)              (liftM (>0) $ SC.getTextLen scn)),
                        (SS.createMenuFunction CN.menuEditFind          (EM.editFind ss tw scn)         (return True)),
                        (SS.createMenuFunction CN.menuEditFindForward   (EM.editFindForward ss tw scn)  (return True)),
                        (SS.createMenuFunction CN.menuEditFindBackward  (EM.editFindBackward ss tw scn) (return True)),
                        (SS.createMenuFunction CN.menuEditSort          (SC.sortSelectedText scn)       (liftM not $ SC.selectionIsEmpty scn)),
                        (SS.createMenuFunction CN.menuBuildCompile      (CP.onBuildCompile ss tw scn)   (liftM not $ SS.ssTestState ss SS.ssStateCompile)),
                        (SS.createMenuFunction CN.menuBuildBuild        (CP.onBuildBuild ss tw scn)     (liftM not $ SS.ssTestState ss SS.ssStateCompile)),
                        (SS.createMenuFunction CN.menuBuildRebuild      (return ())                     (liftM not $ SS.ssTestState ss SS.ssStateCompile)),
                        (SS.createMenuFunction CN.menuBuildClean        (return ())                     (liftM not $ SS.ssTestState ss SS.ssStateCompile)),
                        (SS.createMenuFunction CN.menuDebugRun          (CP.cpDebugRun ss tw)           (liftM not $ SS.ssTestState ss SS.ssStateDebugging)),
                        (SS.createMenuFunction CN.menuDebugGhci         (GH.onDebugGhci ss tw scn)      (liftM not $ SS.ssTestState ss SS.ssStateCompile))
                    ]
                    (SC.getFocus scn)
                    (SC.isClean scn)
                    (getStatusInfo scn)
            

createPopupMenu :: SC.Editor -> IO ()
createPopupMenu scn = do
    SC.usePopup scn SC.sC_POPUP_NEVER
    SC.addPopupMenuItem scn 1000 "Cut"          (\scn _ -> SC.cut scn)              (\scn _ -> liftM MI.boolToInt $ liftM not $ SC.selectionIsEmpty scn)
    SC.addPopupMenuItem scn 1001 "Copy"         (\scn _ -> SC.copy scn)             (\scn _ -> liftM MI.boolToInt $ liftM not $ SC.selectionIsEmpty scn)
    SC.addPopupMenuItem scn 1002 "Paste"        (\scn _ -> SC.paste scn)            (\scn _ -> liftM MI.boolToInt $ SC.canPaste scn)
    SC.addPopupMenuItem scn 1003 "Select All"   (\scn _ -> SC.selectAll scn)        (\scn _ -> liftM MI.boolToInt $ liftM (>0) $ SC.getTextLen scn)
    SC.addPopupMenuItem scn 1004 "Sort"         (\scn _ -> SC.sortSelectedText scn) (\scn _ -> liftM MI.boolToInt $ liftM not $ SC.selectionIsEmpty scn)

-- File Open
onFileOpen :: SS.Session -> IO ()
onFileOpen ss = do                    
    fd <- fileDialogCreate 
        (SS.ssFrame ss) 
        "Open file" 
        "." "" 
        "*.hs" 
        (Point 100 100) 
        (wxOPEN .|. wxFILE_MUST_EXIST)
    ans <- dialogShowModal fd
    if ans == wxID_OK
    then do
        fp <- fileDialogGetPath fd 
        fileOpen ss fp   
        return ()
    else
        return ()

onFileNew :: SS.Session -> IO ()
onFileNew ss = do
    hw <- newFile ss 
    EN.enbSelectTab ss $ SS.hwWindow hw      
    return ()

onFileClose :: SS.Session -> SS.TextWindow -> SC.Editor -> IO ()
onFileClose ss tw scn = fileClose ss tw scn >> return ()

onFileCloseAll :: SS.Session -> IO ()
onFileCloseAll ss = fileCloseAll ss >> return ()

onFileSave :: SS.Session -> SS.TextWindow -> SC.Editor -> IO ()
onFileSave ss tw scn = fileSave ss tw scn >> return ()

onFileSaveAll :: SS.Session -> IO ()
onFileSaveAll ss = fileSaveAll ss >> return ()

onFileSaveAs :: SS.Session -> SS.TextWindow -> SC.Editor -> IO ()
onFileSaveAs ss tw scn = fileSaveAs ss tw scn >> return ()

newFile :: SS.Session -> IO SS.HideWindow  
newFile ss = do
    
    let nb = SS.ssEditors ss

    -- create panel with scintilla editor inside
    p <- panel nb []
    hwnd <- windowGetHandle p
    scn <- SC.createEditor hwnd
    SC.configureHaskell scn

    -- add panel to notebook
    auiNotebookAddPage nb p "..." False 0

    hw <- createHideWindow ss scn p hwnd (SC.getHwnd scn) Nothing
    SS.hwUpdate ss (\hws -> hw : hws)

    -- enable events
    SC.setEventHandler scn $ scnCallback ss hw 
    SC.enableEvents scn
    SC.setSavePoint scn
      
    createPopupMenu scn
    return hw
   
allFilesClean :: SS.Session -> IO Bool
allFilesClean ss = SS.hwFindWindows ss SS.hwIsSourceFile >>= MI.doWhileTrueIO SS.hwIsClean 

closeEditor :: SS.Session -> SS.TextWindow -> SC.Editor -> IO Bool
closeEditor ss tw scn = do
    ic <- SC.isClean scn          
    if ic then do                       
        closeTab ss tw scn        
        return True
    else do            
        -- file is dirty so prompt the user if they want to save it
        EN.enbSelectTab ss tw
        b <- confirmDialog (SS.ssFrame ss) 
            CN.programTitle 
            ("Do you wish to save the file: ?\n" ++ (SS.twFilePathToString tw))
            True                        
        if b then do                   
           -- save file
            b <- fileSave ss tw scn                 
            if b then do                   
                closeTab ss tw scn        
                return True
            -- veto close, don't know how to do this yet ??
            else return False            
        else do
            closeTab ss tw scn
            return True
          
closeTab :: SS.Session -> SS.TextWindow -> SC.Editor -> IO ()
closeTab ss tw scn = do   
    -- close down scintilla editor
    SC.disableEvents scn
    SC.close scn    
    -- remove source file from project
    SS.twRemoveWindow ss tw    
    -- clear status bar
    updateStatus ss ""    
    return ()

fileOpen :: SS.Session -> String -> IO ()
fileOpen ss fp = do
    SS.ssDebugInfo ss $ "FileMenu.fileOpen: " ++ fp
    mhw <- SS.hwFindSourceFileWindow ss fp
    case mhw of
        Just hw -> setSourceFileFocus ss fp -- just set focus to editor
        Nothing -> do       
            -- existing file so add to list, create window and set focus
            (hw, scn) <- openSourceFileEditor ss fp
            loadEditor ss hw scn
            SC.grabFocus scn

fileCloseAll :: SS.Session -> IO Bool
fileCloseAll ss = SS.hwFindWindows ss SS.hwIsSourceFile >>= MI.doWhileTrueIO (\hw -> do
                        case SS.hwGetEditor hw of 
                            Just scn -> fileClose ss (SS.hwWindow hw) scn
                            Nothing  -> do
                                SS.ssDebugError ss "fileCloseAll:: no scintilla editor for source file"
                                return True)

fileClose :: SS.Session -> SS.TextWindow -> SC.Editor -> IO Bool
fileClose ss tw scn = do
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

fileSaveAll :: SS.Session -> IO Bool
fileSaveAll ss = SS.hwFindWindows ss SS.hwIsSourceFile >>= MI.doWhileTrueIO (\hw -> do
                        case SS.hwGetEditor hw of 
                            Just scn -> fileSave ss (SS.hwWindow hw) scn
                            Nothing  -> do
                                SS.ssDebugError ss "fileSaveAll:: no scintilla editor for source file"
                                return True)
  
-- if file is dirty then writes it to file
-- if no filename has been set then file save as is called
-- returns false if user cancelled        
fileSave :: SS.Session -> SS.TextWindow -> SC.Editor -> IO Bool
fileSave ss tw scn = do    
    ic <- SC.isClean scn  
    if ic then return True
    else do
        case SS.twFilePath tw of
            Just fp -> do
                writeSourceFile ss tw scn
                return True            
            -- source file has no name, so prompt user for one
            Nothing -> do
                b <- fileSaveAs ss tw scn
                return b
 
-- File Save As, returns False if user opted to cancel the save 
fileSaveAs :: SS.Session -> SS.TextWindow -> SC.Editor -> IO Bool
fileSaveAs ss tw scn = do   
    -- prompt user for name to save to
    fd <- fileDialogCreate 
        (SS.ssFrame ss) 
        "Save file as" 
        (maybe "." takeDirectory mfp) 
        (maybe "" id mfp )
        "*.hs" 
        (Point 100 100) 
        (wxSAVE .|. wxOVERWRITE_PROMPT)
    rs <- dialogShowModal fd    
    case rs of
--        wxID_OK -> do
-- ?? don't know how to fix pattern match against a function    
        5100 -> do    
            fp <- fileDialogGetPath fd           
            -- save new name to mutable project data
            mtw' <- SS.twFindAndSetFilePath ss tw (Just fp)
            case mtw' of
                Just tw' -> do
                    writeSourceFile ss tw' scn                  
                    -- update tab name
                    EN.enbSetTabText ss tw'
                    return True
                Nothing -> return False
 
        --wxID_CANCEL -> do
        5101 -> do
            return False           
        otherwise  -> do
            return True

    where mfp = SS.twFilePath tw
  
-- writes file to disk and sets editor to clean
writeSourceFile :: SS.Session -> SS.TextWindow -> SC.Editor -> IO ()
writeSourceFile ss tw scn = do
    case SS.twFilePath tw of
        Just fp -> do
            SC.getAllText scn >>= BS.writeFile fp
            SC.setSavePoint scn
            return ()       
        Nothing -> do
            -- bug, shouldn't end up here
            SS.ssDebugError ss "writeSourceFile:: invalid text window, no source file name"
            return ()

getStatusInfo :: SC.Editor -> IO String   
getStatusInfo scn = do
    (l, lp, dp, lc, cc) <- SC.getPositionInfo scn
    return (printf "Line: %d Col: %d, Lines: %d Pos: %d Size: %d" (l+1) (lp+1) lc dp cc)  
    
updateStatus :: SS.Session -> String -> IO ()
updateStatus ss s = do
    let st = SS.ssStatus ss
    set st [text:= s]
    
loadEditor :: SS.Session -> SS.HideWindow -> SC.Editor -> IO ()
loadEditor ss hw scn = do
    case SS.hwFilePath hw of
        Just fp -> do
            text <- BS.readFile fp
            SC.setText scn text
            SC.setSavePoint scn
            return ()
        Nothing -> SS.ssDebugError ss "writeSourceFileEditor:: text window did not have a file name"

setSourceFileFocus :: SS.Session -> String -> IO ()
setSourceFileFocus ss fp = do
    mhw <- SS.hwFindSourceFileWindow ss fp
    case mhw of
        Just hw -> do
            let tw = SS.hwWindow hw
            ix <- auiNotebookGetPageIndex nb $ SS.twPanel tw
            auiNotebookSetSelection nb ix 
            return ()
        Nothing -> return ()

    where nb = SS.ssEditors ss
            
-----------------------------------------------------------------
-- Scintilla callback
-----------------------------------------------------------------

scnCallback :: SS.Session -> SS.HideWindow -> SC.Editor -> SC.SCNotification -> IO ()
scnCallback ss hw scn sn 
    | evt ==  SC.sCN_UPDATEUI = do
            SS.twStatusInfo (SS.hwMenus hw) >>= updateStatus ss 
            if  ( (.&.) (fromIntegral (SI.snUpdated sn) :: Int) 
                        (fromIntegral SC.sC_UPDATE_SELECTION :: Int)) > 0 then
                updateMenus ss hw scn
            else
                return ()
    | evt == SC.sCN_SAVEPOINTREACHED    = updateMenus ss hw scn
    | evt == SC.sCN_SAVEPOINTLEFT       = updateMenus ss hw scn              
    | evt == SC.sCN_FOCUSIN             = updateMenus ss hw scn           
    | evt == SC.sCN_FOCUSOUT            = updateMenus ss hw scn
    | evt == SC.sCN_MARGINCLICK         = GH.toggleBreakPoint ss hw scn sn
    | otherwise = return ()

    where evt = (fromIntegral (SI.notifyGetCode sn) :: Int)

updateMenus :: SS.Session -> SS.HideWindow -> SC.Editor -> IO ()
updateMenus ss hw scn = do
    f <- SC.getFocus scn 
    if f then do 
        setm ss tms CN.menuFileClose        
        setm ss tms CN.menuFileCloseAll        
        setm ss tms CN.menuFileSave        
        setm ss tms CN.menuFileSaveAs        
        setm ss tms CN.menuFileSaveAll        
        setm ss tms CN.menuEditUndo          
        setm ss tms CN.menuEditRedo          
        setm ss tms CN.menuEditCut           
        setm ss tms CN.menuEditCopy          
        setm ss tms CN.menuEditPaste         
        setm ss tms CN.menuEditSelectAll     
        setm ss tms CN.menuEditFind          
        setm ss tms CN.menuEditFindForward   
        setm ss tms CN.menuEditFindBackward  
        setm ss tms CN.menuEditSort          
        setm ss tms CN.menuEditClear          
        setm ss tms CN.menuBuildCompile
        setm ss tms CN.menuBuildBuild
        setm ss tms CN.menuBuildRebuild
        setm ss tms CN.menuBuildClean
        setm ss tms CN.menuDebugRun
        setm ss tms CN.menuDebugGhci
    else do
        setm' ss CN.menuFileClose         (return False) (return ())
        setm' ss CN.menuFileCloseAll      (return False) (return ())
        setm' ss CN.menuFileSave          (return False) (return ())
        setm' ss CN.menuFileSaveAs        (return False) (return ())
        setm' ss CN.menuFileSaveAll       (return False) (return ())
        setm' ss CN.menuEditUndo          (return False) (return ())
        setm' ss CN.menuEditCut           (return False) (return ())
        setm' ss CN.menuEditCopy          (return False) (return ())
        setm' ss CN.menuEditPaste         (return False) (return ())
        setm' ss CN.menuEditSelectAll     (return False) (return ())
        setm' ss CN.menuEditFind          (return False) (return ())
        setm' ss CN.menuEditFindForward   (return False) (return ())
        setm' ss CN.menuEditFindBackward  (return False) (return ())
        setm' ss CN.menuEditSort          (return False) (return ())
        setm' ss CN.menuEditClear         (return False) (return ())
        setm' ss CN.menuBuildCompile      (return False) (return ())
        setm' ss CN.menuBuildBuild        (return False) (return ())
        setm' ss CN.menuBuildRebuild      (return False) (return ())
        setm' ss CN.menuBuildClean        (return False) (return ())
        setm' ss CN.menuDebugRun          (return False) (return ())
        setm' ss CN.menuDebugGhci         (return False) (return ())

        where   setm :: SS.Session -> SS.TextMenus -> Int -> IO ()
                setm ss tw mid = setm' ss mid (SS.tmGetMenuEnabled tw mid) (SS.tmGetMenuFunction tw mid)
 
                setm' :: SS.Session -> Int -> IO Bool -> IO () -> IO ()
                setm' ss mid me mf = do 
                    e <- me
                    set (SS.ssMenuListGet ss mid) [on command := mf, enabled := e]

                tms = SS.hwMenus hw