

no longer used

module GhciImports
( 
    c_GhciNew,
    c_GhciSetEventHandler,
    c_GhciEnableEvents,
    c_GhciDisableEvents,
    c_GhciClose,
    c_GhciPaste,
    c_GhciCut,
    c_GhciCopy,
    c_GhciSelectAll,
    c_GhciHasFocus,
    c_GhciSetFocus,
    c_GhciSendCommand,
    c_GhciIsTextSelected,
    c_GhciGetTextLength,
    c_GhciGetText,
    c_GhciClear,
    c_GhciCreateCallback
) where 
  
import Graphics.Win32.GDI.Types (HWND)
import Foreign.C.String (CString, withCString)
import Foreign.Ptr (FunPtr, Ptr, minusPtr, nullPtr)
import Data.Int (Int32)

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
foreign import ccall safe "GhciSetFocus"        c_GhciSetFocus          :: HWND -> IO () 
foreign import ccall safe "GhciSendCommand"     c_GhciSendCommand       :: HWND -> CString -> IO HWND 
foreign import ccall safe "GhciIsTextSelected"  c_GhciIsTextSelected    :: HWND -> IO Int32
foreign import ccall safe "GhciGetTextLength"   c_GhciGetTextLength     :: HWND -> IO Int32 
foreign import ccall safe "GhciGetText"         c_GhciGetText           :: HWND -> CString -> Int32 -> IO Int32 
foreign import ccall safe "GhciClear"           c_GhciClear             :: HWND -> IO ()

-- callback wrapper
foreign import ccall safe "wrapper" c_GhciCreateCallback ::
    (HWND -> Int -> IO ()) -> IO (FunPtr (HWND -> Int -> IO ()))

