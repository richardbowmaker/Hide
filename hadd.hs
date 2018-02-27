module Main where

import Graphics.UI.WX
import Graphics.UI.WXCore
import Documentation.Haddock.Parser


main = start mainGUI

mainGUI :: IO ()
mainGUI = do 
  
    -- main window
    mf <- frame []    
    set mf [ text := "Starter", size := (Size 300 300)]  
     
     -- create statusbar field
    sf <- statusField []

    
    -- set the statusbar and menubar
    set mf [statusBar := [sf]]

    -- create a timer that updates the display
    -- t <- timer mf [interval := 100, on command := onTimer ss] 
       
    return ()


prog :: String
prog = "add :: Int -> Int -> Int\nadd x y = x+y\n"

   