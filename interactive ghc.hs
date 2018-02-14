
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
import Data.Bits ((.&.))
import Data.Int (Int32)


import Scintilla
import ScintillaConstants

main = start mainGUI

type ChanOut = TChan Char



mainGUI :: IO ()
mainGUI = do 
  
    -- main window
    f <- frame [size := (Size 600 400)]    
    
    p <- panel f []
    p1 <- panel f [size := (Size 600 200)]
    p2 <- panel f [size := (Size 600 200)]

    set f [layout := fill $ (column 5 [fill $ widget p1, fill $ widget p2])]

    set f  [size := (Size 600 400)]    
    set p1 [size := (Size 600 200)]
    set p2 [size := (Size 600 200)]

    repaint f

    chan <- atomically $ newTChan 


    scn1 <- windowGetHandle p1 >>= scnCreateEditor
    scnConfigureHaskell scn1
    scnAppendLineS scn1 "GHCI"

    scn2 <- windowGetHandle p2 >>= scnCreateEditor
    scnConfigureHaskell scn2
    scnAppendLineS scn2 "Debug"

    scnSetEventHandler scn1 $ scnCallback scn1 scn2


    scnEnableEvents scn1
    scnSetModEventMask scn1 $ sC_PERFORMED_USER -- sC_MOD_INSERTTEXT
   

    (hinrd, hinwr) <- createPipe
    (hotrd, hotwr) <- createPipe

    t <- timer f [interval := 100, on command := onTimer scn1 chan] 
 
    forkIO $ copyToChan hotrd chan

    runGHCI scn2 hinrd hotwr

    return ()


runGHCI :: ScnEditor -> Handle -> Handle -> IO ()
runGHCI scn2 hinrd hotwr = do

    scnAppendLineS scn2 "Start create process"

    (_, _, _, ph) <- createProcess_ "errors" (proc "C:\\Program Files\\Haskell Platform\\8.0.1\\bin\\ghci" [])
        {cwd = Just "D:\\_Rick's\\haskell\\HeyHo", std_in = UseHandle hinrd, std_out = UseHandle hotwr} 

    -- stream compiler output to output pane
--    s <- captureOutput ss hr (ssTOutput ss) ""

    scnAppendLineS scn2 "End create process"




    return ()


copyToChan :: Handle -> ChanOut -> IO ()
copyToChan h chan = do
    whileM_ (liftM not $ hIsEOF h) (do
            c <- hGetChar h
            atomically $ writeTChan chan c)
    return ()


onTimer :: ScnEditor -> ChanOut -> IO ()
onTimer scn1 chan = do
    scnSetModEventMask scn1 0
    scnDisableEvents scn1
    whileM_ (liftM not $ atomically $ isEmptyTChan chan)
        (atomically (tryReadTChan chan) >>= maybe (return ()) (\c -> scnAppendText scn1 $ BS.pack [c])) 
    scnEnableEvents scn1
    scnSetModEventMask scn1 $ sC_PERFORMED_USER
  
scnCallback :: ScnEditor -> ScnEditor -> SCNotification -> IO ()
scnCallback scn1 scn2 sn = do 

    case (scnNotifyGetCode sn) of
        2007 -> return ()
        2008 -> do 
--            scnAppendTextS scn2 $ "mod bits " ++ (show $ snModificationType sn) ++ " "
            if (snModificationType sn) .&. (fromIntegral sC_PERFORMED_USER :: Int32) > 0 
                    && (snLinesAdded sn) == 1 then do
                scnAppendLineS scn2 $ "Single line added: " ++ (show $ snPosition sn)
                scnShowLastLine scn2
                len <- scnGetTextLen scn1
                s <- scnGetTextRange scn1 (fromIntegral (snPosition sn) :: Int) len                 
                scnAppendLineS scn2 $ "Single line added: " ++ s
                scnShowLastLine scn2
            else return ()

        2013 -> return ()
        otherwise -> do
            -- scnAppendLineS scn $ "event: " ++ (show $ scnNotifyGetCode sn)
            -- scnShowLastLine scn
            return ()

    return ()

