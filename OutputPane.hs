
module OutputPane
( 
    opCreate,
    opGotoCompileError,
    ghcOpenGhci
) where 
    
import Control.Concurrent 
import Control.Concurrent.STM
import Data.List (find, findIndex)

import Data.Word (Word64)
import Foreign.C.String (CString, withCString)
import Graphics.Win32.GDI.Types (HWND)
import Graphics.UI.WX
import Graphics.UI.WXCore
import System.IO

-- project imports

import EditorNotebook
import Misc
import Scintilla
import ScintillaConstants
import Session


-----------------------
-- Windows API calls --
-----------------------

-- imports from ScintillaProxy.dll
foreign import ccall safe "ScnNewGhci"   c_ScnNewGhci   :: HWND -> CString -> IO HWND 
foreign import ccall safe "ScnCloseGhci" c_ScnCloseGhci :: HWND -> IO ()


ghcOpenGhci :: HWND -> String -> IO HWND
ghcOpenGhci parent file = do
    hwnd <- withCString file (\cs -> c_ScnNewGhci parent cs)
    return hwnd


------------------------------------------------------------    
-- Output pane
------------------------------------------------------------    

opCreate :: Frame () -> IO (AuiNotebook (), ScnEditor)
opCreate f = do

    nb <- auiNotebookCreate f idAny (Point 0 0) (Size 0 0) (wxCLIP_CHILDREN + wxAUI_NB_TOP)
    set nb [] 
    p <- panel nb []
    hwnd <- windowGetHandle p
    e <- scnCreateEditor hwnd
    auiNotebookAddPage nb p "Output" False 0
    ta <- auiSimpleTabArtCreate
    auiNotebookSetArtProvider nb ta
    
    -- configure editor
    scnSetLexer e (fromIntegral sCLEX_CONTAINER :: Int)
    scnSetAStyle e (fromIntegral sTYLE_DEFAULT :: Word64) scnBlack scnWhite 9 "Courier New"
    scnStyleClearAll e
    scnSetAStyle e (fromIntegral sCE_H_DEFAULT :: Word64) scnBlack scnWhite 9 "Courier New"
    scnSetReadOnly e True
 
    return (nb, e)

-- jump to source file error location
opGotoCompileError :: Session -> Int -> (String -> IO ()) -> IO ()
opGotoCompileError ss line fileOpen = do

    -- get compiler errors and lookup the error
    ces <- atomically $ readTVar $ ssCompilerReport ss
    let mce = find (\ce -> (ceErrLine ce) <= line ) (reverse ces)

    case mce of
        Nothing -> return ()
        Just ce -> do

            -- goto source file from list of open files
            msf <- prGetSourceFile ss $ ceFilePath ce 
            case msf of
           
                Just sf -> do
                    b <- enbSelectTab ss sf
                    if b then gotoLine (sfEditor sf) ((ceSrcLine ce)-1) ((ceSrcCol ce)-1)
                    else return ()

                Nothing -> do
                    -- source file not open
                    fileOpen $ ceFilePath ce
                    msf <- sfGetSourceFile ss $ ceFilePath ce
                    case msf of
                        Just sf -> gotoLine (sfEditor sf) ((ceSrcLine ce)-1) ((ceSrcCol ce)-1)
                        Nothing -> return ()
     
    where gotoLine e l c = do
            scnGotoLineCol e l c
            scnGrabFocus e
            


      
