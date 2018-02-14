
module Main where
import Graphics.UI.WX
import Graphics.UI.WXCore
import System.IO 

main = start mainGUI

mainGUI :: IO ()
mainGUI = do 
  
    -- main window
    f <- frame []    
    
    p <- panel f []

    -- create statusbar field
    sf1 <- statusField [text := "SF1", statusWidth := 20]
    sf2 <- statusField [text := "SF2"]
    set f [statusBar := [sf1,sf2]]

    return ()




