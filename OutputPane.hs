
module OutputPane
( 
    addLine,
    addText,
    clear,
    closeOutputWindow,
    createOutputWindow,
    getSelectedGhci,
    gotoNextError,
    gotoPreviousError,
    openOutputWindow,
    withEditor
) where 
    
import Control.Concurrent.STM (atomically, readTVar)
import Control.Monad (liftM)
import qualified Data.ByteString.Char8 as BS (ByteString, writeFile)
import Data.Bits ((.|.), (.&.))
import Data.List (find, findIndex)
import Data.Word (Word64)
import Graphics.UI.WX
import Graphics.UI.WXCore
import Graphics.Win32.GDI.Types (HWND)
import System.FilePath.Windows (takeFileName, takeDirectory)
import Text.Printf (printf)

-- project imports

import qualified Constants as CN
import qualified Debug as DG
import qualified EditMenu as EM
import qualified EditorNotebook as EN
import qualified Misc as MI
import qualified Scintilla as SC
import qualified ScintillaConstants as SC
import qualified ScintillaProxyImports as SI
import qualified Session as SS

------------------------------------------------------------    
-- Output pane
------------------------------------------------------------    

createOutputWindow :: Frame () -> IO (AuiNotebook ())
createOutputWindow f = 
    -- create the output note book
    auiNotebookCreate f idAny (Point 0 0) (Size 0 0) 
        (wxCLIP_CHILDREN + wxAUI_NB_TOP + wxAUI_NB_CLOSE_ON_ACTIVE_TAB)

openOutputWindow :: SS.Session -> (String -> IO ()) -> IO ()
openOutputWindow ss fileOpen = do
    mhw <- SS.ssOutput ss
    case mhw of
        Just hw -> do
            auiNotebookSetSelection (SS.ssOutputs ss) 0
            return ()
{-
            case SS.hwGetEditor hw of
                Just scn -> SC.grabFocus scn
                Nothing  -> return () -- shouldn't get here
-}
        Nothing -> addOutputTab ss fileOpen
    
closeOutputWindow :: SS.Session -> IO ()
closeOutputWindow ss = do
    mhw <- SS.ssOutput ss
    case mhw of
        Just hw -> do
            case SS.hwGetEditor hw of
                Just scn -> do
                    SS.ssSetOutput ss Nothing
                    SC.disableEvents scn
                    SC.close scn
                    return ()
                Nothing  -> return () -- shouldn't get here
        Nothing -> return ()

closeOutputTab :: SS.Session -> IO ()
closeOutputTab ss = do
    mhw <- SS.ssOutput ss
    case mhw of
        Just hw -> do
            case SS.hwGetEditor hw of
                Just scn -> do
                    SS.ssSetOutput ss Nothing
                    SC.disableEvents scn
                    SC.close scn
                    auiNotebookDeletePage (SS.ssOutputs ss) 0
                    return ()
                Nothing  -> return () -- shouldn't get here
        Nothing -> return ()

addOutputTab :: SS.Session -> (String -> IO ()) -> IO ()
addOutputTab ss fileOpen = do
    let nb = SS.ssOutputs ss
    -- create output tab
    panel <- panel nb []
    hwndp <- windowGetHandle panel
    scn <- SC.createEditor hwndp
    auiNotebookInsertPage nb 0 panel "Output" False 0
    ta <- auiSimpleTabArtCreate
    auiNotebookSetArtProvider nb ta
    auiNotebookSetSelection nb 0
    
    -- configure editor
    SC.setLexer scn (fromIntegral SC.sCLEX_CONTAINER :: Int)
    SC.setAStyle scn (fromIntegral SC.sTYLE_DEFAULT :: Word64) SC.black SC.white 9 "Courier New"
    SC.styleClearAll scn
    SC.setAStyle scn (fromIntegral SC.sCE_H_DEFAULT :: Word64) SC.black SC.white 9 "Courier New"
    SC.setReadOnly scn True

    SC.setSelectionMode scn SC.sC_SEL_LINES

    -- add text window to project
    hw <- createHideWindow ss scn panel hwndp (SC.getHwnd scn)
    SS.ssSetOutput ss (Just hw)

    -- enable events
    SC.setEventHandler scn $ scnCallback ss hw scn fileOpen
    SC.enableEvents scn
    SC.grabFocus scn

    -- setup the context menu
    SC.usePopup scn SC.sC_POPUP_NEVER
    SC.addPopupMenuItem scn 1000 "Save As ... " (\scn _ -> fileSave ss (SS.hwWindow hw) scn) (\scn _ -> SC.getTextLen scn)
    SC.addPopupMenuItem scn 1001 "Copy"         (\scn _ -> SC.copy scn)                      (\scn _ -> liftM MI.boolToInt $ liftM not $ SC.selectionIsEmpty scn)
    SC.addPopupMenuItem scn 1002 "Select All"   (\scn _ -> SC.selectAll scn)                 (\scn _ -> SC.getTextLen scn)
    SC.addPopupMenuItem scn 1003 "Clear"        (\scn _ -> clearText scn)                    (\scn _ -> SC.getTextLen scn)

menuOption :: SS.Session -> SC.Editor -> Int -> IO ()
menuOption ss scn id = do
    infoDialog (SS.ssFrame ss) CN.programTitle ("menu item " ++ (show id))

createHideWindow :: SS.Session -> SC.Editor -> Panel() -> HWND -> HWND -> IO SS.HideWindow
createHideWindow ss scn panel phwnd hwnd = do
    tw <- SS.createTextWindow (SS.createOutputWindowType scn) panel phwnd hwnd Nothing
    return $ SS.createHideWindow tw (tms tw)

    where  tms tw = SS.createTextMenus
                    [
                        (SS.createMenuFunction CN.menuFileClose         (closeOutputTab ss)             (return True)),
                        (SS.createMenuFunction CN.menuFileSaveAs        (fileSave ss tw scn)            (liftM (>0) $ SC.getTextLen scn)),
                        (SS.createMenuFunction CN.menuEditCopy          (SC.copy scn)                   (liftM not $ SC.selectionIsEmpty scn)),
                        (SS.createMenuFunction CN.menuEditSelectAll     (SC.selectAll scn)              (liftM (>0) $ SC.getTextLen scn)),
                        (SS.createMenuFunction CN.menuEditFind          (EM.editFind ss tw scn)         (return True)),
                        (SS.createMenuFunction CN.menuEditFindForward   (EM.editFindForward ss tw scn)  (return True)),
                        (SS.createMenuFunction CN.menuEditFindBackward  (EM.editFindBackward ss tw scn) (return True)),
                        (SS.createMenuFunction CN.menuEditClear         (clearText scn)                 (liftM (>0) $ SC.getTextLen scn))
                    ]
                    (SC.getFocus scn)
                    (return True)
                    (getStatusInfo scn)

-- File Save As, returns False if user opted to cancel the save 
fileSave :: SS.Session -> SS.TextWindow -> SC.Editor -> IO ()
fileSave ss tw scn = do   
    -- prompt user for name to save to
    mfp <-SS.twFilePath tw
    fd <- fileDialogCreate 
        (SS.ssFrame ss) 
        "Save output as" 
        (maybe "." takeDirectory mfp) 
        (maybe "" id mfp )
        "*.txt" 
        (Point 100 100) 
        (wxSAVE .|. wxOVERWRITE_PROMPT)
    rs <- dialogShowModal fd    
    case rs of
--        wxID_OK -> do
-- ?? don't know how to fix pattern match against a function    
        5100 -> do    
            fp <- fileDialogGetPath fd           
            -- save new name to mutable project data
            SS.twSetFilePath tw fp 
            SC.getAllText scn >>= BS.writeFile fp             
            return () 
 
        --wxID_CANCEL -> do
        5101 -> do
            return ()           
        otherwise  -> do
            return ()

-- jump to source file error location
gotoErrorLine :: SS.Session -> Int -> (String -> IO ()) -> IO ()
gotoErrorLine ss line fileOpen = do
    mce <- SS.crFindError ss line 
    case mce of
        Just ce -> gotoError ss ce fileOpen
        Nothing -> return ()

gotoNextError :: SS.Session -> (String -> IO ()) -> IO ()
gotoNextError ss fileOpen = do
    report <- SS.ssGetCompilerReport ss
    if SS.crErrorCount report > 0 then do
        let merr = SS.crCurrErr report
        case merr of
            Just err -> do
                if err < (SS.crErrorCount report) - 1 then 
                    gotoErrorNo ss (err+1) fileOpen
                else do
                    infoDialog (SS.ssFrame ss) CN.programTitle "No more errors"
                    SS.crUpdateCurrentError ss $ Nothing
            Nothing -> gotoErrorNo ss 0 fileOpen
    else return ()

gotoPreviousError :: SS.Session -> (String -> IO ()) -> IO ()
gotoPreviousError ss fileOpen = do
    report <- SS.ssGetCompilerReport ss
    if SS.crErrorCount report > 0 then do
        let merr = SS.crCurrErr report
        case merr of
            Just err -> do
                if err > 0 then gotoErrorNo ss (err-1) fileOpen
                else do
                    infoDialog (SS.ssFrame ss) CN.programTitle "No more errors"
                    SS.crUpdateCurrentError ss $ Nothing
            Nothing -> gotoErrorNo ss 0 fileOpen
    else return ()
     
gotoErrorNo :: SS.Session -> Int -> (String -> IO ()) -> IO ()
gotoErrorNo ss errno fileOpen = do
    report <- SS.ssGetCompilerReport ss
    let errs = SS.crErrorCount report
    if errno >= 0 && errno < errs then 
        gotoError ss ((SS.crErrors report) !! errno) fileOpen
    else
        return ()

gotoError :: SS.Session -> SS.CompError -> (String -> IO ()) -> IO ()
gotoError ss ce fileOpen = do
    -- goto source file from list of open files
    mhw <- SS.hwFindSourceFileWindow ss $ SS.ceFilePath ce 
    case mhw of          
        Just hw -> do
            b <- EN.enbSelectTab ss $ SS.hwWindow hw
            if b then do
                highlightOutput ss ce
                gotoPos hw ce 
                SS.crUpdateCurrentError ss (Just $ SS.ceErrorNo ce)
            else return ()
        Nothing -> do
            -- source file not open
            fileOpen $ SS.ceFilePath ce
            mhw <- SS.hwFindSourceFileWindow ss $ SS.ceFilePath ce 
            case mhw of
                Just hw -> do
                    highlightOutput ss ce
                    gotoPos hw ce 
                    SS.crUpdateCurrentError ss (Just $ SS.ceErrorNo ce)
                Nothing -> return ()
    
    where   gotoPos hw ce = do
                case SS.hwGetEditor hw of
                    Just scn -> do
                        SC.gotoLineCol scn ((SS.ceSrcLine ce)-1) ((SS.ceSrcCol ce)-1)
                        SC.grabFocus scn
                        SC.selectWord scn
                    Nothing -> return ()
            
            highlightOutput ss ce = do
                mout <- SS.ssOutput ss
                case mout of
                    Just hw ->
                        case SS.hwGetEditor hw of
                            Just scn -> do
                                let ls = SS.ceErrLine ce
                                ps <- SC.getPositionFromLine scn ls
                                let le = ls + (length $ SS.ceErrLines ce) + 1
                                pe <- SC.getPositionFromLine scn le
                                SC.setSelectionRange scn ps pe
                                SC.setFirstVisibleLine scn (ls-1)
                            Nothing -> return ()
                    Nothing -> return ()

scnCallback :: SS.Session -> SS.HideWindow -> SC.Editor -> (String -> IO ()) -> SC.SCNotification -> IO ()
scnCallback ss hw scn fileOpen sn = do 
    -- event from output pane
    case (SI.notifyGetCode sn) of
        2006 -> do -- sCN_DOUBLECLICK
            gotoErrorLine ss (fromIntegral (SI.snLine sn) :: Int) fileOpen
            return ()
        2007 -> do -- sCN_UPDATEUI
            SS.twStatusInfo (SS.hwMenus hw) >>= updateStatus ss 
            if  ( (.&.) (fromIntegral (SI.snUpdated sn) :: Int) 
                        (fromIntegral SC.sC_UPDATE_SELECTION :: Int)) > 0 then
                updateMenus ss hw scn
            else
                return ()
        2028 -> do -- sCN_FOCUSIN
            updateMenus ss hw scn           
        2029 -> do -- sCN_FOCUSOUT
            updateMenus ss hw scn         
        otherwise -> do
            return ()

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

        where   setm :: SS.Session -> SS.TextMenus -> Int -> IO ()
                setm ss tw mid = setm' ss mid (SS.tmGetMenuEnabled tw mid) (SS.tmGetMenuFunction tw mid)
 
                setm' :: SS.Session -> Int -> IO Bool -> IO () -> IO ()
                setm' ss mid me mf = do 
                    e <- me
                    set (SS.ssMenuListGet ss mid) [on command := mf, enabled := e]

                tms = SS.hwMenus hw

-- returns the HWND of the child panel of the currently selected notebook page
getSelectedTabHwnd :: SS.Session -> IO (Maybe HWND)
getSelectedTabHwnd ss = do
    n <- getTabCount ss
    if n > 0 then do
        hp <- auiNotebookGetSelection nb >>= auiNotebookGetPage nb >>= windowGetHandle
        return (Just hp)
    else return Nothing
    where nb = SS.ssOutputs ss
 
-- returns the source file for the currently selected tab
getSelectedGhci :: SS.Session -> IO (Maybe SS.HideWindow)
getSelectedGhci ss = do  
    hws <- SS.hwGetWindows ss
    mhwnd <- getSelectedTabHwnd ss
    case mhwnd of 
        Just hwnd -> return $ find (\hw -> (SS.hwIsGhci hw) && (SS.hwMatchesHwnd hw hwnd)) hws
        Nothing   -> return Nothing

getTabCount :: SS.Session -> IO Int
getTabCount ss = do
    pc <- auiNotebookGetPageCount $ SS.ssOutputs ss
    return pc

getStatusInfo :: SC.Editor -> IO String   
getStatusInfo scn = do
    (l, lp, dp, lc, cc) <- SC.getPositionInfo scn
    return (printf "Line: %d Col: %d, Lines: %d Pos: %d Size: %d" (l+1) (lp+1) lc dp cc)  
    
updateStatus :: SS.Session -> String -> IO ()
updateStatus ss s = do
    let st = SS.ssStatus ss
    set st [text:= s]

clearText :: SC.Editor -> IO ()
clearText scn = do 
    SC.setReadOnly scn False
    SC.clearAll scn
    SC.setReadOnly scn True

clear' :: SS.Session -> SC.Editor -> IO ()
clear' ss scn = do
    SC.setReadOnly scn False
    SC.clearAll scn
    SC.setReadOnly scn True

addText' :: SS.Session -> BS.ByteString -> SC.Editor -> IO ()
addText' ss bs scn = do
    SC.setReadOnly scn False
    SC.appendText scn bs
    SC.setReadOnly scn True
    SC.showLastLine scn
    
addLine' :: SS.Session -> BS.ByteString -> SC.Editor -> IO ()
addLine' ss bs scn = do
    SC.setReadOnly scn False
    SC.appendLine scn bs
    SC.setReadOnly scn True
    SC.showLastLine scn

clear :: SS.Session -> IO ()
clear ss = withEditor ss $ clear' ss 

addText :: SS.Session -> BS.ByteString -> IO ()
addText ss bs = withEditor ss $ addText' ss bs 

addLine :: SS.Session -> BS.ByteString -> IO ()
addLine ss bs = withEditor ss $ addLine' ss bs 

withEditor :: SS.Session -> (SC.Editor -> IO ()) -> IO ()
withEditor ss f = do    
    mhw <- SS.ssOutput ss
    case mhw of
        Just hw -> do
            case SS.hwGetEditor hw of
                Just scn -> f scn
                Nothing  -> return () -- shouldn't get here
        Nothing -> return ()

    
 
     
