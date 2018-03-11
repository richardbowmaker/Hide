
module OutputPane
( 
    createOutputWindow,
    openOutputWindow,
    closeOutputWindow,
    getSelectedGhci
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
import qualified FileMenu as FM
import qualified EditorNotebook as EN
import Output
import qualified Scintilla as SC
import qualified ScintillaConstants as SC
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
            case SS.hwGetEditor hw of
                Just scn -> SC.grabFocus scn
                Nothing  -> return () -- shouldn't get here
        Nothing -> addOutputTab ss
    
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
    SC.setEventHandler scn $ scnCallback ss hw scn
    SC.enableEvents scn
    SC.grabFocus scn

createHideWindow :: SS.Session -> SC.Editor -> Panel() -> HWND -> HWND -> IO SS.HideWindow
createHideWindow ss scn panel phwnd hwnd = do
    tw <- SS.createTextWindow (SS.createOutputWindowType scn) panel phwnd hwnd Nothing
    return $ SS.createHideWindow tw (tms tw)

    where  tms tw = SS.createTextMenus
                    [
                        (SS.createMenuFunction CN.menuFileClose         (closeOutputWindow ss)          (return True)),
                        (SS.createMenuFunction CN.menuFileSaveAs        (fileSave ss tw scn)            (return True)),
                        (SS.createMenuFunction CN.menuEditCopy          (SC.copy scn)                   (liftM not $ SC.selectionIsEmpty scn)),
                        (SS.createMenuFunction CN.menuEditSelectAll     (SC.selectAll scn)              (return True)),
                        (SS.createMenuFunction CN.menuEditFind          (EM.editFind ss tw scn)         (return True)),
                        (SS.createMenuFunction CN.menuEditFindForward   (EM.editFindForward ss tw scn)  (return True)),
                        (SS.createMenuFunction CN.menuEditFindBackward  (EM.editFindBackward ss tw scn) (return True))
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
gotoCompileError :: SS.Session -> Int -> (String -> IO ()) -> IO ()
gotoCompileError ss line fileOpen = do
    -- get compiler errors and lookup the error
    ces <- SS.ssGetCompilerReport ss
    let mce = find (\ce -> (SS.ceErrLine ce) <= line ) (reverse ces)
    case mce of
        Just ce -> do
            -- goto source file from list of open files
            mhw <- SS.hwFindSourceFileWindow ss $ SS.ceFilePath ce 
            case mhw of          
                Just hw -> do
                    b <- EN.enbSelectTab ss $ SS.hwWindow hw
                    if b then gotoPos hw ce
                    else return ()
                Nothing -> do
                    -- source file not open
                    fileOpen $ SS.ceFilePath ce
                    mhw <- SS.hwFindSourceFileWindow ss $ SS.ceFilePath ce 
                    case mhw of
                        Just hw -> gotoPos hw ce
                        Nothing -> return ()
        Nothing -> return ()
     
    where   gotoPos hw ce = do
                case SS.hwGetEditor hw of
                    Just scn -> do
                        SC.gotoLineCol scn ((SS.ceSrcLine ce)-1) ((SS.ceSrcCol ce)-1)
                        SC.grabFocus scn
                        SC.selectWord scn
                    Nothing -> return ()

scnCallback :: SS.Session -> SS.HideWindow -> SC.Editor -> SC.SCNotification -> IO ()
scnCallback ss hw scn sn = do 
    -- event from output pane
    case (SC.notifyGetCode sn) of
        2006 -> do -- sCN_DOUBLECLICK
            gotoCompileError ss (fromIntegral (SC.snLine sn) :: Int) (FM.fileOpen ss)
            return ()
        2007 -> do -- sCN_UPDATEUI
            SS.twStatusInfo (SS.hwMenus hw) >>= updateStatus ss 
            if  ( (.&.) (fromIntegral (SC.snUpdated sn) :: Int) 
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
    
 
     
