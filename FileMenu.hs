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
import qualified Menus as MN
import qualified Misc as MI
import qualified Scintilla as SC
import qualified ScintillaProxyImports as SI
import qualified Session as SS

openSourceFileEditor :: SS.Session -> String -> IO (SS.TextWindow, SC.Editor)
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
    let tw = SS.createSourceTextWindow scn p hwnd (SC.getHwnd scn) 
            (Just fp) (SC.grabFocus scn) (SC.getFocus scn) (SC.isClean scn) (getStatusInfo scn)
    SS.twUpdate ss (\tws -> tw : tws)

    -- enable events
    SC.setEventHandler scn $ scnCallback ss tw
    SC.enableEvents scn
    SC.setMarginSensitive scn CN.symbolMargin True
         
    -- set focus to new page
    ix <- auiNotebookGetPageIndex nb p
    auiNotebookSetSelection nb ix 

    createPopupMenu scn
    return (tw, scn)

-- create the menu handlers
createMenuHandlers :: SS.Session -> SC.Editor -> SS.TextWindow -> Maybe String -> MN.HideMenuHandlers 
createMenuHandlers ss scn tw mfp = 
    [MN.createMenuHandler MN.menuFileClose          hwnd (onFileClose ss tw scn)         (return True),
     MN.createMenuHandler MN.menuFileCloseAll       hwnd (onFileCloseAll ss)             (return True),
     MN.createMenuHandler MN.menuFileSave           hwnd (onFileSave ss tw scn)          (liftM not $ SC.isClean scn),
     MN.createMenuHandler MN.menuFileSaveAs         hwnd (onFileSaveAs ss tw scn)        (return True),
     MN.createMenuHandler MN.menuFileSaveAll        hwnd (onFileSaveAll ss)              (liftM not $ allFilesClean ss),
     MN.createMenuHandler MN.menuEditUndo           hwnd (SC.undo scn)                   (SC.canUndo scn),
     MN.createMenuHandler MN.menuEditRedo           hwnd (SC.redo scn)                   (SC.canRedo scn),
     MN.createMenuHandler MN.menuEditCut            hwnd (SC.cut scn)                    (liftM not $ SC.selectionIsEmpty scn),
     MN.createMenuHandler MN.menuEditCopy           hwnd (SC.copy scn)                   (liftM not $ SC.selectionIsEmpty scn),
     MN.createMenuHandler MN.menuEditPaste          hwnd (SC.paste scn)                  (SC.canPaste scn),
     MN.createMenuHandler MN.menuEditSelectAll      hwnd (SC.selectAll scn)              (liftM (>0) $ SC.getTextLen scn),
     MN.createMenuHandler MN.menuEditFind           hwnd (EM.editFind ss tw scn)         (return True),
     MN.createMenuHandler MN.menuEditFindForward    hwnd (EM.editFindForward ss tw scn)  (return True),
     MN.createMenuHandler MN.menuEditFindBackward   hwnd (EM.editFindBackward ss tw scn) (return True),
     MN.createMenuHandler MN.menuEditSort           hwnd (SC.sortSelectedText scn)       (liftM not $ SC.selectionIsEmpty scn),
     MN.createMenuHandler MN.menuBuildCompile       hwnd (CP.onBuildCompile ss tw scn)   (liftM not $ SS.ssTestState ss SS.ssStateCompile),
     MN.createMenuHandler MN.menuBuildBuild         hwnd (CP.onBuildBuild ss tw scn)     (liftM not $ SS.ssTestState ss SS.ssStateCompile),
     MN.createMenuHandler MN.menuBuildRebuild       hwnd (return ())                     (liftM not $ SS.ssTestState ss SS.ssStateCompile),
     MN.createMenuHandler MN.menuBuildClean         hwnd (return ())                     (liftM not $ SS.ssTestState ss SS.ssStateCompile),
     MN.createMenuHandler MN.menuDebugRun           hwnd (CP.cpDebugRun ss tw)           (liftM not $ SS.ssTestState ss SS.ssStateDebugging),
     MN.createMenuHandler MN.menuDebugGhci          hwnd (GH.onDebugGhci ss tw scn)      (liftM not $ SS.ssTestState ss SS.ssStateCompile)]

    where hwnd = SS.twHwnd tw

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
    mfn <- SI.winOpenFileDialog 
            (SS.ssFrame ss) 
            "Open a file" 
            "." 
            "*.hs" 
            "Haskell file" 
            0x1000 -- file must exist
    case mfn of 
        Just fp -> fileOpen ss fp   
        Nothing -> return ()

onFileNew :: SS.Session -> IO ()
onFileNew ss = do
    tw <- newFile ss 
    EN.enbSelectTab ss tw    
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

newFile :: SS.Session -> IO SS.TextWindow  
newFile ss = do
    
    let nb = SS.ssEditors ss

    -- create panel with scintilla editor inside
    p <- panel nb []
    hwnd <- windowGetHandle p
    scn <- SC.createEditor hwnd
    SC.configureHaskell scn

    -- add panel to notebook
    auiNotebookAddPage nb p "..." False 0

    let tw = SS.createSourceTextWindow scn p hwnd (SC.getHwnd scn) 
            Nothing  (SC.grabFocus scn) (SC.getFocus scn) (SC.isClean scn) (getStatusInfo scn)
    SS.twUpdate ss (\tws -> tw : tws)

    -- set the menu handlers
    let mhs = createMenuHandlers ss scn tw Nothing
    SS.ssSetMenuHandlers ss mhs

    -- enable events
    SC.setEventHandler scn $ scnCallback ss tw 
    SC.enableEvents scn
    SC.setSavePoint scn
      
    createPopupMenu scn
    return tw
   
allFilesClean :: SS.Session -> IO Bool
allFilesClean ss = SS.twFindWindows ss SS.twIsSourceFile >>= MI.doWhileTrueIO SS.twIsClean 

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
    SS.ssDisableMenuHandlers ss (SS.twHwnd tw)
    return ()

fileOpen :: SS.Session -> String -> IO ()
fileOpen ss fp = do
    SS.ssDebugInfo ss $ "FileMenu.fileOpen: " ++ fp
    mtw <- SS.twFindSourceFileWindow ss fp
    case mtw of
        Just tw -> setSourceFileFocus ss fp -- just set focus to editor
        Nothing -> do       
            -- existing file so add to list, create window and set focus
            (tw, scn) <- openSourceFileEditor ss fp
            loadEditor ss tw scn
            SC.grabFocus scn

fileCloseAll :: SS.Session -> IO Bool
fileCloseAll ss = SS.twFindWindows ss SS.twIsSourceFile >>= MI.doWhileTrueIO (\tw -> do
                        case SS.twGetEditor tw of 
                            Just scn -> fileClose ss tw scn
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
                SS.ssDisableMenuHandlers ss (SS.twHwnd tw)  
                return True
            Nothing -> do
                SS.ssDebugError ss "fileClose, no tab for source file"
                return True       
    else return False

fileSaveAll :: SS.Session -> IO Bool
fileSaveAll ss = SS.twFindWindows ss SS.twIsSourceFile >>= MI.doWhileTrueIO (\tw -> do
                        case SS.twGetEditor tw of 
                            Just scn -> fileSave ss tw scn
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
    mfn <- SI.winSaveFileDialog 
            (SS.ssFrame ss) 
            "Save file as" 
            (maybe "." takeDirectory mfp)  
            "*.hs" 
            "Haskell file" 
            (maybe "" id mfp)
            0x02 -- overwrite prompt
    case mfn of 
        Just fp -> do
            mtw' <- SS.twFindAndSetFilePath ss tw (Just fp)
            case mtw' of
                Just tw' -> do
                    writeSourceFile ss tw' scn                  
                    -- update tab name
                    EN.enbSetTabText ss tw'
                    return True
                Nothing -> return False
        Nothing -> return False
        
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
    
loadEditor :: SS.Session -> SS.TextWindow -> SC.Editor -> IO ()
loadEditor ss tw scn = do
    case SS.twFilePath tw of
        Just fp -> do
            text <- BS.readFile fp
            SC.setText scn text
            SC.setSavePoint scn
            return ()
        Nothing -> SS.ssDebugError ss "writeSourceFileEditor:: text window did not have a file name"

setSourceFileFocus :: SS.Session -> String -> IO ()
setSourceFileFocus ss fp = do
    mtw <- SS.twFindSourceFileWindow ss fp
    case mtw of
        Just tw -> do
            ix <- auiNotebookGetPageIndex nb $ SS.twPanel tw
            auiNotebookSetSelection nb ix 
            return ()
        Nothing -> return ()

    where nb = SS.ssEditors ss
            
-----------------------------------------------------------------
-- Scintilla callback
-----------------------------------------------------------------

scnCallback :: SS.Session -> SS.TextWindow -> SC.Editor -> SC.SCNotification -> IO ()
scnCallback ss tw scn sn 
    | evt ==  SC.sCN_UPDATEUI = do
            SS.twStatusInfo tw >>= updateStatus ss 
            if  ( (.&.) (fromIntegral (SI.snUpdated sn) :: Int) 
                        (fromIntegral SC.sC_UPDATE_SELECTION :: Int)) > 0 then
                updateMenus ss tw scn
            else
                return ()
    | evt == SC.sCN_SAVEPOINTREACHED    = updateMenus ss tw scn
    | evt == SC.sCN_SAVEPOINTLEFT       = updateMenus ss tw scn              
    | evt == SC.sCN_FOCUSIN             = updateMenus ss tw scn           
    | evt == SC.sCN_FOCUSOUT            = updateMenus ss tw scn
    | evt == SC.sCN_MARGINCLICK         = GH.toggleBreakPoint ss tw scn sn
    | otherwise = return ()

    where evt = (fromIntegral (SI.notifyGetCode sn) :: Int)

updateMenus :: SS.Session -> SS.TextWindow -> SC.Editor -> IO ()
updateMenus ss tw scn = do
    f <- SC.getFocus scn 
    if f then do 
        -- set the menu handlers
        let mhs = createMenuHandlers ss scn tw Nothing
        SS.ssSetMenuHandlers ss mhs
    else do
        SS.ssDisableMenuHandlers ss (SS.twHwnd tw)