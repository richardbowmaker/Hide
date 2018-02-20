
module Ghci
( 
    ghcOpenGhci
) where 
    
import Graphics.Win32.GDI.Types (HWND)

import Foreign.C.String (CString, withCString)

-- project imports

import Misc

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




      
