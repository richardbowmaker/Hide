module Main where
import Graphics.UI.WX
import Graphics.UI.WXCore


import Foreign
import Foreign.C.Types
import Foreign.C.String
import System.Win32.DLL
import System.Win32.Types
import Graphics.Win32.Window
import Graphics.Win32.GDI.Types
import Graphics.Win32.Message

foreign import ccall unsafe "MarshallTypes" c_marshallTypes :: CString -> Int -> Double -> IO ()

sdasd

main = start mainGUI

mainGUI :: IO ()
mainGUI = do 

    f <- frameFixed [] 
    
    set f [ text := "Use Proxy DLL", 
            bgcolor     := white, 
            layout      := space 400 400
          ]
                    
    test <- button f [text := "Click"]  
    set test [on command := cmdTest ]  
    return () 
  
marshallTypes :: String -> Int -> Double -> IO ()
marshallTypes s n d = do 
    cstr <- newCString s 
    c_marshallTypes cstr n d
    return ()
    
cmdTest :: IO ()
cmdTest = do
    marshallTypes "string" 12 45.67
    return ()