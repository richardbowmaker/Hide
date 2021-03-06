
module OutputPane
( 
    addLineBS,
    addLineS,
    addTextBS,
    addTextS,
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
import qualified Data.ByteString.Char8 as BS (ByteString, writeFile, pack)
import Data.Bits ((.|.), (.&.))
import Data.List (find, findIndex)
import qualified Data.Text as TX (Text, unpack, pack)
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
import qualified Menus as MN
import qualified Misc as MI
import qualified Scintilla as SC
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

openOutputWindow :: SS.Session -> IO ()
openOutputWindow ss = do
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
        Nothing -> addOutputTab ss
    
closeOutputWindow :: SS.Session -> IO ()
closeOutputWindow ss = do
    mtw <- SS.ssOutput ss
    case mtw of
        Just tw -> do
            case SS.twGetEditor tw of
                Just scn -> do
                    SS.ssSetOutput ss Nothing
                    SC.disableEvents scn
                    SC.close scn
                    return ()
                Nothing  -> return () -- shouldn't get here
        Nothing -> return ()

closeOutputTab :: SS.Session -> IO ()
closeOutputTab ss = do
    mtw <- SS.ssOutput ss
    case mtw of
        Just tw -> do
            case SS.twGetEditor tw of
                Just scn -> do
                    SS.ssSetOutput ss Nothing
                    SC.disableEvents scn
                    SC.close scn
                    auiNotebookDeletePage (SS.ssOutputs ss) 0
                    return ()
                Nothing  -> return () -- shouldn't get here
        Nothing -> return ()

addOutputTab :: SS.Session -> IO ()
addOutputTab ss = do
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
    SC.setAStyle scn SC.sTYLE_DEFAULT CN.black CN.white 9 "Courier New"
    SC.styleClearAll scn
    SC.setAStyle scn SC.sCE_H_DEFAULT CN.black CN.white 9 "Courier New"
    SC.setReadOnly scn True

    SC.setSelectionMode scn SC.sC_SEL_LINES

    -- add text window to project
    let tw = SS.createOutputTextWindow scn panel hwndp (SC.getHwnd scn)
                (SC.grabFocus scn) (SC.getFocus scn) (return True) (getStatusInfo scn)
    SS.ssSetOutput ss (Just tw)

    let mhs = createMenuHandlers ss scn tw 
    SS.ssSetMenuHandlers ss mhs

    -- enable events
    SC.setEventHandler scn $ scnCallback ss tw
    SC.enableEvents scn
    SC.grabFocus scn

    -- setup the context menu
    SC.usePopup scn SC.sC_POPUP_NEVER
    SC.addPopupMenuItem scn 1000 "Save As ... " (\scn _ -> fileSave ss tw scn) (\scn _ -> SC.getTextLen scn)
    SC.addPopupMenuItem scn 1001 "Copy"         (\scn _ -> SC.copy scn)        (\scn _ -> liftM MI.boolToInt $ liftM not $ SC.selectionIsEmpty scn)
    SC.addPopupMenuItem scn 1002 "Select All"   (\scn _ -> SC.selectAll scn)   (\scn _ -> SC.getTextLen scn)
    SC.addPopupMenuItem scn 1003 "Clear"        (\scn _ -> clearText scn)      (\scn _ -> SC.getTextLen scn)

menuOption :: SS.Session -> SC.Editor -> Int -> IO ()
menuOption ss scn id = do
    infoDialog (SS.ssFrame ss) CN.programTitle ("menu item " ++ (show id))

-- create the menu handlers
createMenuHandlers :: SS.Session -> SC.Editor -> SS.TextWindow -> MN.HideMenuHandlers 
createMenuHandlers ss scn tw = 
    [MN.createMenuHandler MN.menuFileClose         hwnd (closeOutputTab ss)             (return True),
     MN.createMenuHandler MN.menuFileSaveAs        hwnd (fileSave ss tw scn)            (liftM (>0) $ SC.getTextLen scn),
     MN.createMenuHandler MN.menuEditCopy          hwnd (SC.copy scn)                   (liftM not $ SC.selectionIsEmpty scn),
     MN.createMenuHandler MN.menuEditSelectAll     hwnd (SC.selectAll scn)              (liftM (>0) $ SC.getTextLen scn),
     MN.createMenuHandler MN.menuEditFind          hwnd (EM.editFind ss tw scn)         (return True),
     MN.createMenuHandler MN.menuEditFindForward   hwnd (EM.editFindForward ss tw scn)  (return True),
     MN.createMenuHandler MN.menuEditFindBackward  hwnd (EM.editFindBackward ss tw scn) (return True),
     MN.createMenuHandler MN.menuEditClear         hwnd (clearText scn)                 (liftM (>0) $ SC.getTextLen scn)]

    where  hwnd = SS.twHwnd tw

-- File Save As, returns False if user opted to cancel the save 
fileSave :: SS.Session -> SS.TextWindow -> SC.Editor -> IO ()
fileSave ss tw scn = do   
    -- prompt user for name to save to
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
            SS.twFindAndSetFilePath ss tw (Just fp) 
            SC.getAllText scn >>= BS.writeFile fp             
            return () 
 
        --wxID_CANCEL -> do
        5101 -> do
            return ()           
        otherwise  -> do
            return ()

    where mfp = SS.twFilePath tw

-- jump to source file error location
gotoErrorLine :: SS.Session -> Int -> IO ()
gotoErrorLine ss line = do
    mce <- SS.crFindError ss line 
    case mce of
        Just ce -> gotoError ss ce
        Nothing -> return ()

gotoNextError :: SS.Session -> IO ()
gotoNextError ss  = do
    report <- SS.ssGetCompilerReport ss
    if SS.crErrorCount report > 0 then do
        let merr = SS.crCurrErr report
        case merr of
            Just err -> do
                if err < (SS.crErrorCount report) - 1 then 
                    gotoErrorNo ss (err+1)
                else do
                    infoDialog (SS.ssFrame ss) CN.programTitle "No more errors"
                    SS.crUpdateCurrentError ss $ Nothing
            Nothing -> gotoErrorNo ss 0
    else return ()

gotoPreviousError :: SS.Session -> IO ()
gotoPreviousError ss = do
    report <- SS.ssGetCompilerReport ss
    if SS.crErrorCount report > 0 then do
        let merr = SS.crCurrErr report
        case merr of
            Just err -> do
                if err > 0 then gotoErrorNo ss (err-1)
                else do
                    infoDialog (SS.ssFrame ss) CN.programTitle "No more errors"
                    SS.crUpdateCurrentError ss $ Nothing
            Nothing -> gotoErrorNo ss 0
    else return ()
     
gotoErrorNo :: SS.Session -> Int -> IO ()
gotoErrorNo ss errno = do
    report <- SS.ssGetCompilerReport ss
    let errs = SS.crErrorCount report
    if errno >= 0 && errno < errs then 
        gotoError ss ((SS.crErrors report) !! errno)
    else
        return ()

gotoError :: SS.Session -> SS.CompError -> IO ()
gotoError ss ce = do
    -- goto source file from list of open files
    mtw <- SS.twFindSourceFileWindow ss $ SS.ceFilePath ce 
    case mtw of          
        Just tw -> do
            b <- EN.enbSelectTab ss tw
            if b then do
                highlightOutput ss ce
                gotoPos tw ce 
                SS.crUpdateCurrentError ss (Just $ SS.ceErrorNo ce)
            else return ()
        Nothing -> do
            -- source file not open
            (SS.ssFileOpen ss) ss $ SS.ceFilePath ce
            mtw <- SS.twFindSourceFileWindow ss $ SS.ceFilePath ce 
            case mtw of
                Just tw -> do
                    highlightOutput ss ce
                    gotoPos tw ce 
                    SS.crUpdateCurrentError ss (Just $ SS.ceErrorNo ce)
                Nothing -> return ()
    
    where   gotoPos tw ce = do
                case SS.twGetEditor tw of
                    Just scn -> do
                        SC.gotoLineCol scn ((SS.ceSrcLine ce)-1) ((SS.ceSrcCol ce)-1)
                        SC.grabFocus scn
                        SC.selectWord scn
                    Nothing -> return ()
            
            highlightOutput ss ce = do
                mout <- SS.ssOutput ss
                case mout of
                    Just tw ->
                        case SS.twGetEditor tw of
                            Just scn -> do
                                let ls = SS.ceErrLine ce
                                ps <- SC.getPositionFromLine scn ls
                                let le = ls + (length $ SS.ceErrLines ce) + 1
                                pe <- SC.getPositionFromLine scn le
                                SC.setSelectionRange scn ps pe
                                SC.setFirstVisibleLine scn (ls-1)
                            Nothing -> return ()
                    Nothing -> return ()

scnCallback :: SS.Session -> SS.TextWindow -> SC.Editor -> SC.SCNotification -> IO ()
scnCallback ss tw scn sn = do 
    -- event from output pane
    case (SI.notifyGetCode sn) of
        2006 -> do -- sCN_DOUBLECLICK
            gotoErrorLine ss (fromIntegral (SI.snLine sn) :: Int)
            return ()
        2007 -> do -- sCN_UPDATEUI
            SS.twStatusInfo tw >>= updateStatus ss 
            if  ( (.&.) (fromIntegral (SI.snUpdated sn) :: Int) 
                        (fromIntegral SC.sC_UPDATE_SELECTION :: Int)) > 0 then
                updateMenus ss tw scn
            else
                return ()
        2028 -> do -- sCN_FOCUSIN
            updateMenus ss tw scn           
        2029 -> do -- sCN_FOCUSOUT
            updateMenus ss tw scn         
        otherwise -> do
            return ()

updateMenus :: SS.Session -> SS.TextWindow -> SC.Editor -> IO ()
updateMenus ss tw scn = do
    f <- SC.getFocus scn 
    if f then do 
        -- set the menu handlers
        let mhs = createMenuHandlers ss scn tw 
        SS.ssSetMenuHandlers ss mhs
    else do
        SS.ssDisableMenuHandlers ss (SS.twHwnd tw)

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
getSelectedGhci :: SS.Session -> IO (Maybe SS.TextWindow)
getSelectedGhci ss = do  
    tws <- SS.twGetWindows ss
    mhwnd <- getSelectedTabHwnd ss
    case mhwnd of 
        Just hwnd -> return $ find (\tw -> (SS.twIsGhci tw) && (SS.twMatchesHwnd tw hwnd)) tws
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

addTextBS :: SS.Session -> BS.ByteString -> IO ()
addTextBS ss bs = withEditor ss $ addText' ss bs 

addTextS :: SS.Session -> String -> IO ()
addTextS ss s = withEditor ss $ addText' ss $ BS.pack s 

addLineBS :: SS.Session -> BS.ByteString -> IO ()
addLineBS ss bs = withEditor ss $ addLine' ss bs 

addLineS :: SS.Session -> String -> IO ()
addLineS ss s = withEditor ss $ addLine' ss $ BS.pack s 

withEditor :: SS.Session -> (SC.Editor -> IO ()) -> IO ()
withEditor ss f = do    
    mtw <- SS.ssOutput ss
    case mtw of
        Just tw -> do
            case SS.twGetEditor tw of
                Just scn -> f scn
                Nothing  -> return () -- shouldn't get here
        Nothing -> return ()

    
 
     
