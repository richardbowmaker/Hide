
module Ghci
( 
    openWindow,
    openWindowFile,
    closeWindow,
    paste,
    cut,
    copy,
    selectAll,
    getFocus,
    sendCommand,
    setEventHandler,
    enableEvents,
    disableEvents,
    eventGotFocus,
    eventLostFocus,
    eventSelectionSet,
    eventSelectionClear,
    GhciPanel
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

import EditorNotebook
import Misc
import Session as SS

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
foreign import ccall safe "GhciHasFocus"        c_GhciHasFocus          :: IO HWND 
foreign import ccall safe "GhciSendCommand"     c_GhciSendCommand       :: HWND -> CString -> IO HWND 
foreign import ccall safe "GhciIsTextSelected"  c_GhciIsTextSelected    :: HWND -> IO Int32

-- callback wrapper
foreign import ccall safe "wrapper" createCallback ::
    (HWND -> Int -> IO ()) -> IO (FunPtr (HWND -> Int -> IO ()))

--------------------------------------------------------------------------

openWindowFile :: SS.Session -> SourceFile -> IO ()
openWindowFile ss sf = do
    mtw <- twFind ss (\tw -> sfPathIs sf $ twFilePath tw) 
    case mtw of
        Just tw -> do
            -- GHCI already open so select it
            let nb = ssOutputs ss
            auiNotebookGetPageIndex nb (twPanel tw) >>= auiNotebookSetSelection nb
            -- reload the source file
            sendCommand (twPanelHwnd tw) $ ":load " ++ (maybe "" id (sfFilePath sf))
            return ()
        Nothing -> do
            -- GHCI not open so open a new tab
            case (sfFilePath sf) of
                Just fp -> do
                    m <- open ss fp                  
                    case m of
                        Just (panel, hwnd) -> do
                                twUpdate ss (\tws -> twCreate ((newTW panel hwnd fp) : txWindows tws))
                                return ()
                        Nothing -> return ()
                Nothing -> return ()

    where newTW panel hwnd fp = (SS.createTextWindow
                                SS.createGhciWindowType
                                panel
                                hwnd
                                (cut hwnd)
                                (copy hwnd)
                                (paste hwnd)
                                (selectAll hwnd)
                                (undo hwnd)
                                (redo hwnd)
                                (isTextSelected hwnd)
                                (isTextSelected hwnd)
                                (return True)
                                (return True)
                                (return False)
                                (return False)
                                (Just fp))

openWindow :: SS.Session -> IO ()
openWindow ss = do
    m <- open ss "" 
    case m of
        Just (panel, hwnd) -> do
            twUpdate ss (\tws -> twCreate ((newTW panel hwnd) : txWindows tws))
            return ()
        Nothing -> return ()

    where newTW panel hwnd = (SS.createTextWindow
                                SS.createGhciWindowType
                                panel
                                hwnd
                                (cut hwnd)
                                (copy hwnd)
                                (paste hwnd)
                                (selectAll hwnd)
                                (undo hwnd)
                                (redo hwnd)
                                (isTextSelected hwnd)
                                (isTextSelected hwnd)
                                (return True)
                                (return True)
                                (return False)
                                (return False)
                                Nothing)

open :: SS.Session -> String -> IO (Maybe (Panel (), HWND))
open ss fp = do

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

            -- enable events
            setEventHandler hwnd $ eventHandler ss
            enableEvents hwnd

            return (Just (p, hp))
                
closeWindow :: Session -> IO ()
closeWindow ss = do
    let nb = ssOutputs ss
    p <- auiNotebookGetSelection nb >>= auiNotebookGetPage nb
    hwnd <- windowGetHandle p 
    c_GhciClose hwnd
    prUpdateSourceFiles ss (\sf -> maybe sf 
        (\ghci -> if (sfMatchesHwnd sf hwnd) then sfSetGhciPanel sf Nothing else sf) $ sfGhci sf)
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

undo :: HWND -> IO ()
undo h = return ()

redo :: HWND -> IO ()
redo h = return ()

isTextSelected :: HWND -> IO Bool
isTextSelected hwnd = do
    n <- c_GhciIsTextSelected hwnd
    return (n /= 0)

getFocus :: IO HWND
getFocus = c_GhciHasFocus

setEventHandler :: HWND -> (HWND -> Int -> IO ()) -> IO ()
setEventHandler hwnd eh = do
    cb <- createCallback eh
    c_GhciSetEventHandler hwnd cb    
    return ()

enableEvents :: HWND -> IO ()
enableEvents = c_GhciEnableEvents

disableEvents :: HWND -> IO ()
disableEvents = c_GhciDisableEvents

eventHandler :: Session -> HWND -> Int -> IO ()
eventHandler ss hwnd code = ssDebugInfo ss $ "GHCI event: " ++ (show code)

eventGotFocus :: Int
eventGotFocus = 1

eventLostFocus :: Int
eventLostFocus = 2

eventSelectionSet :: Int
eventSelectionSet = 3

eventSelectionClear :: Int
eventSelectionClear = 4

