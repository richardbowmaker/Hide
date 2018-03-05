
module Ghci
( 
    openWindow,
    openWindowFile,
    closeWindow,
    paste,
    cut,
    copy,
    selectAll,
    hasFocus,
    sendCommand,
    setEventHandler,
    enableEvents,
    disableEvents,
    eventGotFocus,
    eventLostFocus,
    eventSelectionSet,
    eventSelectionClear,
    eventHandler,
) where 
    
import Control.Concurrent 
import Control.Concurrent.STM
import Data.List (find, findIndex)

import Data.Word (Word64)
import Data.Int (Int32)
import Foreign.C.String (CString, withCString)
import Foreign.Ptr (FunPtr, Ptr, minusPtr, nullPtr)
import Graphics.Win32.GDI.Types (HWND)
import Graphics.UI.WX
import Graphics.UI.WXCore
import System.FilePath.Windows (takeFileName)
import System.IO
import System.Win32.Types (nullHANDLE)

-- project imports

import qualified Constants as CN
import qualified Misc as MI
import qualified Session as SS

-----------------------
-- Windows API calls --
-----------------------

-- imports from ScintillaProxy.dll
foreign import ccall safe "GhciNew"             c_GhciNew               :: HWND -> CString -> CString -> IO HWND 
foreign import ccall safe "GhciSetEventHandler" c_GhciSetEventHandler   :: HWND -> FunPtr (HWND -> Int -> IO ()) -> IO ()
foreign import ccall safe "GhciEnableEvents"    c_GhciEnableEvents      :: HWND -> IO ()
foreign import ccall safe "GhciDisableEvents"   c_GhciDisableEvents     :: HWND -> IO ()
foreign import ccall safe "GhciClose"           c_GhciClose             :: HWND -> IO ()
foreign import ccall safe "GhciPaste"           c_GhciPaste             :: HWND -> IO ()
foreign import ccall safe "GhciCut"             c_GhciCut               :: HWND -> IO () 
foreign import ccall safe "GhciCopy"            c_GhciCopy              :: HWND -> IO ()
foreign import ccall safe "GhciSelectAll"       c_GhciSelectAll         :: HWND -> IO () 
foreign import ccall safe "GhciHasFocus"        c_GhciHasFocus          :: HWND -> IO Int32 
foreign import ccall safe "GhciSendCommand"     c_GhciSendCommand       :: HWND -> CString -> IO HWND 
foreign import ccall safe "GhciIsTextSelected"  c_GhciIsTextSelected    :: HWND -> IO Int32

-- callback wrapper
foreign import ccall safe "wrapper" createCallback ::
    (HWND -> Int -> IO ()) -> IO (FunPtr (HWND -> Int -> IO ()))

--------------------------------------------------------------------------

openWindowFile :: SS.Session -> SS.TextWindow -> (SS.TextWindow -> Int -> IO ()) -> IO ()
openWindowFile ss ftw eh = do
    mtw <- SS.twFindWindow ss (\tw -> (SS.twIsGhci tw) && (SS.twIsSameFile ftw tw)) 
    case mtw of
        Just tw -> do
            -- GHCI already open so select it
            let nb = SS.ssOutputs ss
            auiNotebookGetPageIndex nb (SS.twPanel tw) >>= auiNotebookSetSelection nb
            -- reload the source file
            sendCommand (SS.twPanelHwnd tw) $ ":load " ++ (maybe "" id (SS.twFilePath tw))
        Nothing -> do
            -- GHCI not open so open a new tab
            case (SS.twFilePath ftw) of
                Just fp -> do
                    mw <- open ss fp                 
                    case mw of
                        Just (panel, hwndp, hwnd) -> do
                                let tw = newtw panel hwndp hwnd fp
                                SS.twUpdate ss (\tws -> tw : tws)
                                setEventHandler tw eh
                                enableEvents hwnd
                                return ()
                        Nothing -> return ()
                Nothing -> return ()

    where   newtw panel hwndp hwnd fp = (SS.createTextWindow
                                SS.createGhciWindowType
                                panel
                                hwndp
                                hwnd
                                [   
                                    (SS.createMenuFunction CN.menuEditCut        (cut hwnd)          (return False)),
                                    (SS.createMenuFunction CN.menuEditCopy       (copy hwnd)         (isTextSelected hwnd)),
                                    (SS.createMenuFunction CN.menuEditPaste      (paste hwnd)        (return True)),
                                    (SS.createMenuFunction CN.menuEditSelectAll  (selectAll hwnd)    (return True))

                                ]
                                (hasFocus hwnd)
                                (return True)
                                (Just fp))



openWindow :: SS.Session -> (SS.TextWindow -> Int -> IO ()) -> IO ()
openWindow ss eh = do
    m <- open ss "" 
    case m of
        Just (panel, hwndp, hwnd) -> do
            let tw = newtw panel hwndp hwnd
            SS.twUpdate ss (\tws -> tw : tws)
            setEventHandler tw eh
            enableEvents hwnd
            return ()
        Nothing -> return ()

   where   newtw panel hwndp hwnd = (SS.createTextWindow
                                SS.createGhciWindowType
                                panel
                                hwndp
                                hwnd
                                [   
                                    (SS.createMenuFunction CN.menuEditCut        (cut hwnd)          (return False)),
                                    (SS.createMenuFunction CN.menuEditCopy       (copy hwnd)         (isTextSelected hwnd)),
                                    (SS.createMenuFunction CN.menuEditPaste      (paste hwnd)        (return True)),
                                    (SS.createMenuFunction CN.menuEditSelectAll  (selectAll hwnd)    (return True))

                                ]
                                (hasFocus hwnd)
                                (return True)
                                Nothing)

open :: SS.Session -> String -> IO (Maybe (Panel (), HWND, HWND))
open ss fp = do

    -- create panel and embed GHCI window
    let nb = SS.ssOutputs ss
    p <- panel nb []
    hp <- windowGetHandle p
    hwnd <- withCString fp (\cfp -> 
        withCString "-fasm -L. -lScintillaProxy -threaded" (\cop -> c_GhciNew hp cop cfp))

    case (MI.ptrToWord64 hwnd) of

        0 -> return Nothing
        _ -> do

            -- add to outputs
            auiNotebookAddPage nb p ("GHCI " ++ (takeFileName fp)) False 0

            -- set focus to new page
            ix <- auiNotebookGetPageIndex nb p
            auiNotebookSetSelection nb ix 

            return (Just (p, hp, hwnd))
                
closeWindow :: SS.Session -> SS.TextWindow -> IO ()
closeWindow ss tw = do
    let nb = SS.ssOutputs ss
    p <- auiNotebookGetSelection nb >>= auiNotebookGetPage nb
    windowGetHandle p >>= c_GhciClose
    SS.twRemoveWindow ss tw
    return ()

sendCommand :: HWND -> String -> IO ()
sendCommand hwnd cmd = withCString cmd (\cs -> c_GhciSendCommand hwnd cs) >> return ()

paste :: HWND -> IO ()
paste = c_GhciPaste

cut :: HWND -> IO ()
cut = c_GhciCut

copy :: HWND -> IO ()
copy = c_GhciCopy

selectAll :: HWND -> IO ()
selectAll = c_GhciSelectAll

isTextSelected :: HWND -> IO Bool
isTextSelected hwnd = do
    n <- c_GhciIsTextSelected hwnd
    return (n /= 0)

hasFocus :: HWND -> IO Bool
hasFocus h = do 
        b <- c_GhciHasFocus h
        return (b /= 0)

setEventHandler :: SS.TextWindow -> (SS.TextWindow -> Int -> IO ()) -> IO ()
setEventHandler tw eh = do
    cb <- createCallback (eventHandler $ eh tw)
    c_GhciSetEventHandler (SS.twPanelHwnd tw) cb    
    return ()

eventHandler :: (Int -> IO ()) -> HWND -> Int -> IO ()
eventHandler f h n = f n

enableEvents :: HWND -> IO ()
enableEvents = c_GhciEnableEvents

disableEvents :: HWND -> IO ()
disableEvents = c_GhciDisableEvents


eventGotFocus :: Int
eventGotFocus = 1

eventLostFocus :: Int
eventLostFocus = 2

eventSelectionSet :: Int
eventSelectionSet = 3

eventSelectionClear :: Int
eventSelectionClear = 4

