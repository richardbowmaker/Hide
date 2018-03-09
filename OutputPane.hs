
module OutputPane
( 
    createOutputPane,
    gotoCompileError,
    clear,
    addText,
    addLine,
    getSelectedGhci
) where 
    
import Control.Concurrent.STM (atomically, readTVar)
import Data.ByteString.Internal (ByteString)
import Data.List (find, findIndex)
import Data.Word (Word64)
import Graphics.UI.WX
import Graphics.UI.WXCore
import Graphics.Win32.GDI.Types (HWND)

-- project imports

import qualified EditorNotebook as EN
import qualified Scintilla as SC
import qualified ScintillaConstants as SC
import qualified Session as SS

------------------------------------------------------------    
-- Output pane
------------------------------------------------------------    

createOutputPane :: Frame () -> IO (AuiNotebook (), SC.ScnEditor)
createOutputPane f = do

    nb <- auiNotebookCreate f idAny (Point 0 0) (Size 0 0) (wxCLIP_CHILDREN + wxAUI_NB_TOP + wxAUI_NB_CLOSE_ON_ACTIVE_TAB)
    set nb [] 
    p <- panel nb []
    hwnd <- windowGetHandle p
    e <- SC.scnCreateEditor hwnd
    auiNotebookAddPage nb p "Output" False 0
    ta <- auiSimpleTabArtCreate
    auiNotebookSetArtProvider nb ta
    
    -- configure editor
    SC.scnSetLexer e (fromIntegral SC.sCLEX_CONTAINER :: Int)
    SC.scnSetAStyle e (fromIntegral SC.sTYLE_DEFAULT :: Word64) SC.scnBlack SC.scnWhite 9 "Courier New"
    SC.scnStyleClearAll e
    SC.scnSetAStyle e (fromIntegral SC.sCE_H_DEFAULT :: Word64) SC.scnBlack SC.scnWhite 9 "Courier New"
    SC.scnSetReadOnly e True

    SC.scnSetSelectionMode e SC.sC_SEL_LINES
 
    return (nb, e)

-- jump to source file error location
gotoCompileError :: SS.Session -> Int -> (String -> IO ()) -> IO ()
gotoCompileError ss line fileOpen = do
    -- get compiler errors and lookup the error
    ces <- atomically $ readTVar $ SS.ssCompilerReport ss
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
                        SC.scnGotoLineCol scn ((SS.ceSrcLine ce)-1) ((SS.ceSrcCol ce)-1)
                        SC.scnGrabFocus scn
                        SC.scnSelectWord scn
                    Nothing -> return ()
          
clear :: SS.Session -> IO ()
clear ss = do
    let e = SS.ssOutput ss
    SC.scnSetReadOnly e False
    SC.scnClearAll e
    SC.scnSetReadOnly e True

addText :: SS.Session -> ByteString -> IO ()
addText ss bs = do
    let e = SS.ssOutput ss
    SC.scnSetReadOnly e False
    SC.scnAppendText e bs
    SC.scnSetReadOnly e True
    SC.scnShowLastLine e
    
addLine :: SS.Session -> ByteString -> IO ()
addLine ss bs = do
    let e = SS.ssOutput ss
    SC.scnSetReadOnly e False
    SC.scnAppendLine e bs
    SC.scnSetReadOnly e True
    SC.scnShowLastLine e


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

 
     
