module Main where

import Graphics.UI.WX
import Graphics.UI.WXCore



main = start mainGUI

mainGUI :: IO ()
mainGUI = do 
  
    -- main window
    mf <- frame []    
    set mf [ text := "Starter", size := (Size 1300 800)]  
     
     -- create statusbar field
    sf <- statusField []

    
    -- set the statusbar and menubar
    set mf [statusBar := [sf]]

    -- create a timer that updates the display
    -- t <- timer mf [interval := 100, on command := onTimer ss] 
       
    return ()
   

class Editor 