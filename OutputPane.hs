
module OutputPane
( 
    createOutputPane,
    gotoCompileError,
    openGhciFile,
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
import System.Win32.Types (nullHANDLE)

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
foreign import ccall safe "GhciNew"   c_GhciNew   :: HWND -> CString -> CString -> IO HWND 
foreign import ccall safe "GhciClose" c_GhciClose :: HWND -> IO ()

openGhciFile :: Session -> SourceFile -> IO ()
openGhciFile ss sf = do

    case (sfGhci sf) of

        Just ghci -> do

            -- GHCI already open so select it
            let nb = ssOutputs ss
            auiNotebookGetPageIndex nb (ghciPanel ghci) >>= auiNotebookSetSelection nb
            return ()

        Nothing -> do

            -- GHCI not open so open a new tab
            case (sfFilePath sf) of
                Just fp -> do
                    m <- openGhciFile' ss fp                  
                    case m of
                        Nothing -> return ()
                        Just (p, hwnd) -> do
                            -- update the project to include GHCI panel handle
                            sfCreate (sfPanel sf) (sfEditor sf) (sfFilePath sf) 
                                (Just (sfCreateGhciPanel p hwnd)) >>= sfUpdate ss
                            return ()
                Nothing -> return ()

openGhci :: Session -> IO ()
openGhci ss = openGhciFile' ss "" >> return ()

openGhciFile' :: Session -> String -> IO (Maybe (Panel (), HWND))
openGhciFile' ss fp = do

    -- create panel and embed GHCI window
    let nb = ssOutputs ss
    p <- panel nb []
    hp <- windowGetHandle p
    hwnd <- withCString fp (\cfp -> 
        withCString "-fasm -L. -lScintillaProxy -threaded" (\cop -> c_GhciNew hp cop cfp))

    case (ptrToWord64 hwnd) of

        0 -> return Nothing
        _ -> do

            -- add to outputs
            auiNotebookAddPage nb p ("GHCI " ++ (takeFileName fp)) False 0

            -- set focus to new page
            ix <- auiNotebookGetPageIndex nb p
            auiNotebookSetSelection nb ix  

            return (Just (p, hp))
                
closeGhci :: Session -> IO ()
closeGhci ss = do
    let nb = ssOutputs ss
    p <- auiNotebookGetSelection nb >>= auiNotebookGetPage nb
    hwnd <- windowGetHandle p 
    c_GhciClose hwnd
    prUpdateSourceFiles ss (\sf -> maybe sf 
        (\ghci -> if (sfMatchesHwnd sf hwnd) then sfSetGhciPanel sf Nothing else sf) $ sfGhci sf)
    return ()

------------------------------------------------------------    
-- Output pane
------------------------------------------------------------    

createOutputPane :: Frame () -> IO (AuiNotebook (), ScnEditor)
createOutputPane f = do

    nb <- auiNotebookCreate f idAny (Point 0 0) (Size 0 0) (wxCLIP_CHILDREN + wxAUI_NB_TOP + wxAUI_NB_CLOSE_ON_ACTIVE_TAB)
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

    scnSetSelectionMode e sC_SEL_LINES
 
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
                    if b then gotoPos (sfEditor sf) ((ceSrcLine ce)-1) ((ceSrcCol ce)-1)
                    else return ()

                Nothing -> do
                    -- source file not open
                    fileOpen $ ceFilePath ce
                    msf <- sfGetSourceFile ss $ ceFilePath ce
                    case msf of
                        Just sf -> gotoPos (sfEditor sf) ((ceSrcLine ce)-1) ((ceSrcCol ce)-1)
                        Nothing -> return ()
     
    where gotoPos e l c = do
            scnGotoLineCol e l c
            scnGrabFocus e
            scnSelectWord e
            


      
