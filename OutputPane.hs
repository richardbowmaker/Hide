
module OutputPane
( 
    createOutputPane,
    gotoCompileError,
    openGhci,
    closeGhci
) where 
    
import Control.Concurrent 
import Control.Concurrent.STM
import Data.List (find, findIndex)

import Data.Word (Word64)
import Foreign.C.String (CString, withCString)
import Graphics.Win32.GDI.Types (HWND)
import Graphics.UI.WX
import Graphics.UI.WXCore
import System.FilePath.Windows (takeFileName)
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
foreign import ccall safe "GhciNew"   c_GhciNew   :: HWND -> CString -> IO HWND 
foreign import ccall safe "GhciClose" c_GhciClose :: HWND -> IO ()

openGhci :: Session -> SourceFile -> IO ()
openGhci ss sf = do

    let nb = ssOutputs ss

    case (sfGhci sf) of

        Just ghci -> do

            -- GHCI already open so select it
            ix <- auiNotebookGetPageIndex nb (ghciPanel ghci)
            auiNotebookSetSelection nb ix
            return ()

        Nothing -> do

            -- GHCI not open so open a new tab
            case (sfFilePath sf) of

                Just fp -> do

                    -- create panel and embed GHCI window
                    p <- panel nb []
                    hp <- windowGetHandle p
                    hwnd <- withCString fp (\cs -> c_GhciNew hp cs)

                    -- add to outputs
                    auiNotebookAddPage nb p (takeFileName fp) False 0

                    -- set focus to new page
                    ix <- auiNotebookGetPageIndex nb p
                    auiNotebookSetSelection nb ix  

                    -- update the project to include the modified source file status
                    sf' <- sfCreate (sfPanel sf) (sfEditor sf) (sfFilePath sf) (Just (sfCreateGhciPanel p hwnd))
                    sfUpdate ss sf'

                    return ()

                Nothing -> return ()
        
closeGhci :: Session -> SourceFile -> IO ()
closeGhci ss sf = do
    let nb = ssOutputs ss
    case (sfGhci sf) of
        Just ghci -> c_GhciClose $ ghciHwnd ghci
        Nothing   -> return ()

------------------------------------------------------------    
-- Output pane
------------------------------------------------------------    

createOutputPane :: Frame () -> IO (AuiNotebook (), ScnEditor)
createOutputPane f = do

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
gotoCompileError :: Session -> Int -> (String -> IO ()) -> IO ()
gotoCompileError ss line fileOpen = do

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
            


      
