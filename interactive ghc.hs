
module Main where
import Graphics.UI.WX
import Graphics.UI.WXCore
 
import System.IO
import System.Process
import System.Process.Common
import qualified Data.ByteString.Char8 as BS (ByteString, hGetContents, hGetLine, hPutStr, readFile, pack, putStrLn, unpack, writeFile)
import Control.Concurrent 
import Control.Concurrent.STM
import qualified Control.Concurrent.Thread as Thread
import Control.Monad (liftM, when)
import Control.Monad.Loops


import Scintilla

main = start mainGUI

type ChanOut = TChan Char



mainGUI :: IO ()
mainGUI = do 
  
    -- main window
    f <- frame [size := (Size 400 400)]    
    
    p <- panel f [size := (Size 400 400)]

    chan <- atomically $ newTChan 

    -- create statusbar field
    sf1 <- statusField [text := "SF1", statusWidth := 20]
    sf2 <- statusField [text := "SF2"]
    set f [statusBar := [sf1,sf2]]

    scn <- windowGetHandle p >>= scnCreateEditor
    scnConfigureHaskell scn
--    set p [size := (Size 400 400)]

    scnSetEventHandler scn $ scnCallback scn
    scnEnableEvents scn
   

    (hinrd, hinwr) <- createPipe
    (hotrd, hotwr) <- createPipe

    t <- timer f [interval := 100, on command := onTimer scn chan] 
 
    forkIO $ copyToChan hotrd chan

    runGHCI scn hinrd hotwr


    return ()


runGHCI :: ScnEditor -> Handle -> Handle -> IO ()
runGHCI scn hinrd hotwr = do

    scnAppendLine scn $ BS.pack "Start create process"

    (_, _, _, ph) <- createProcess_ "errors" (proc "C:\\Program Files\\Haskell Platform\\8.0.1\\bin\\ghci" [])
        {cwd = Just "D:\\_Rick's\\haskell\\HeyHo", std_in = UseHandle hinrd, std_out = UseHandle hotwr}

    -- stream compiler output to output pane
--    s <- captureOutput ss hr (ssTOutput ss) ""

    scnAppendLine scn $ BS.pack "End create process"




    return ()


copyToChan :: Handle -> ChanOut -> IO ()
copyToChan h chan = do
    whileM_ (liftM not $ hIsEOF h) (do
            c <- hGetChar h
            atomically $ writeTChan chan c)
    return ()


onTimer :: ScnEditor -> ChanOut -> IO ()
onTimer scn chan = do
    whileM_ (liftM not $ atomically $ isEmptyTChan chan)
        (atomically (tryReadTChan chan) >>= maybe (return ()) (\c -> scnAppendText scn $ BS.pack [c]))             
            

scnCallback :: ScnEditor -> SCNotification -> IO ()
scnCallback scn sn = do 

--    case (scnNotifyGetCode sn) of



    return ()

