module Main where

-- library imports
import Control.Concurrent 
import Control.Concurrent.STM
import qualified Control.Concurrent.Thread as Thread
import Control.Exception
import Control.Monad (liftM)
import Control.Monad.Loops
import Data.Bits ((.&.), (.|.), xor)
import qualified Data.ByteString.Char8 as BS (ByteString, hGetContents, hGetLine, hPutStr, hPutStrLn , readFile, pack, putStrLn, unpack, writeFile)
import qualified Data.ByteString as BS (append, empty, hGetNonBlocking )
import Data.List (find, findIndex, isInfixOf)
import Data.Word (Word64)
import Graphics.UI.WX
import Graphics.UI.WXCore
import Numeric (showHex)
import qualified Text.ParserCombinators.Parsec as P
import qualified Text.Parsec.Prim as P (modifyState)
import Text.Printf (printf)
import System.Directory 
import qualified System.FilePath.Windows as Win (dropExtension, takeBaseName, takeDirectory)
import System.IO 
import System.IO.Error (catchIOError)
import System.Process
import System.Process.Common
import System.Timeout (timeout)

type TString = TVar String

main = start mainGUI

mainGUI :: IO ()
mainGUI = do

    ts <- atomically $ newTVar ""
  
    -- main window
    mf <- frame []    
    set mf [text := "Starter"]  
     
     -- create statusbar field
    sf <- statusField []

    -- set the statusbar and menubar
    set mf [statusBar := [sf]]

    tc <- textCtrl mf [size := (Size 800 800)]
    b1 <- button mf [text:= "Click 1", size := (Size 100 50), on command := onB1 tc ts]
    b2 <- button mf [text:= "GHCI", size := (Size 100 50), on command := runGhci ts]

    set mf [layout:= column 10 [(row 10 [fill $ widget b1, fill $ widget b2]), fill $ widget tc]]
    set mf [size := (Size 800 800)]

    -- create a timer that updates the display
    t <- timer mf [interval := 100, on command := onTimer ts tc] 

    set sf [text:= "my status"]
       
    return ()

onTimer :: TString -> TextCtrl () -> IO ()
onTimer ts tc = do
    s <- atomically (do
        s <- readTVar ts
        writeTVar ts ""
        return s)
    if length s > 0 then addText tc s
    else return ()

onB1 :: TextCtrl () -> TString -> IO ()
onB1 tc ts = (forkIO $ addLineTS ts "button 1") >> return ()

addTextTS :: TString -> String -> IO ()
addTextTS ts s = atomically $ modifyTVar' ts (\s' -> (s' ++ s))

addLineTS :: TString -> String -> IO ()
addLineTS ts s = return () -- addTextTS ts (s ++ "\n")

addText :: TextCtrl () -> String -> IO ()
addText tc s = do
    t <- get tc text
    let t' = t ++ s
    set tc [text := t']
    return ()

addLine :: TextCtrl () -> String -> IO ()
addLine tc s = addText tc (s ++ "\n")
       
-- run command and redirect std out to the output pane
-- session -> arguments -> working directory -> stdout TChan -> completion function
runGhci :: TString -> IO ()
runGhci ts = (forkIO $ runGhci1 ts) >> return ()
    
runGhci1 :: TString -> IO ()
runGhci1 ts = do

    addLineTS ts "Start run Ghci"
    
 

    (mih, moh, _, ph) <- createProcess_ "errors" (proc "D:\\_Rick's\\C++\\Cons1\\x64\\Debug\\Cons1.exe" [])
        {cwd = Just "D:\\_Rick's\\haskell\\Hide", std_out = CreatePipe, std_in = CreatePipe}
 
    case mih of
        Just ih ->
            case moh of
                Just oh ->
                    runGhci2 ts ih oh ph
                Nothing -> return ()
        Nothing -> return ()


runGhci2 :: TString -> Handle -> Handle -> ProcessHandle -> IO ()
runGhci2 ts ih oh ph = do

    addLineTS ts "wait for CMD ready"
    ms <- waitForOutput2 ts oh 5000 "enter> "
    addLineTS ts $ maybe "Timedout" id ms
    
    addLineTS ts "************* dir *********************"
    hPutStrLn ih "dir"
    ms <- waitForOutput2 ts oh 5000 "enter> "
    addLineTS ts $ maybe "Timedout" id ms

   

    runGhci2 ts ih oh ph
                   


{-
                    hPutStrLn ih ":browse\n"

                    -- stream compiler output to output pane
                    addLineTS ts "start capture output:"
                    s <- captureOutput ts oh ""
                    addLineTS ts "got output:"
                    addTextTS ts s

                    threadDelay 3000000
                    hPutStrLn ih ":quit"
                    addLineTS ts "quit sent"

                    waitForProcess ph

                    addLineTS ts "process finished"
-}


 


-- captures output from handle, wrtes to the output pane and returns
-- the captured data
captureOutput :: TString -> Handle -> String -> IO String
captureOutput ts h str = do
    threadDelay 200000
    eof <- hIsEOF h
    if eof then do
        addLineTS ts "eof"
        return str
    else do
        addLineTS ts "not eof"
        s <- hGetLine h -- NB hGetLine is appending a CR on the end of the line !!
        -- remove CR and append new line, required by parser
        addLineTS ts s
        captureOutput ts h $ str ++ s

-- waitForOutput h timeout (ms) (end of data marker)
waitForOutput :: Handle -> Int -> String -> IO (Maybe String)
waitForOutput = waitForOutput' ""  

waitForOutput'  :: String -> Handle -> Int -> String -> IO (Maybe String)
waitForOutput' str h timeout eod = do
    b <- hWaitForInput h timeout
    if b then do
        c <- hGetChar h
        let str' = str ++ [c]
        if stringEndsWith str' eod then return (Just str')
        else waitForOutput' str' h timeout eod
    else return Nothing

flushOutput :: TString -> Handle -> IO ()
flushOutput ts h = whileM_ (hWaitForInput h 10) 
    (do c <- hGetChar h
        addLineTS ts $ "flush: " ++ [c])

stringEndsWith :: String -> String -> Bool
stringEndsWith [] _ = False
stringEndsWith _ [] = False
stringEndsWith s e = take (length e) (reverse s) == reverse e

waitForOutput2 :: TString -> Handle -> Int -> String -> IO (Maybe String)
waitForOutput2 ts h t eod = do
    timeout (t * 1000) $ waitForOutput2' "" ts h eod

waitForOutput2' :: String -> TString -> Handle -> String -> IO String
waitForOutput2' str ts h eod = do
    addLineTS ts "waitForOutput2' 1"
    bs <- BS.hGetNonBlocking h 1
    addLineTS ts "waitForOutput2' 2"
    if bs /= BS.empty then do
        let str' = str ++ (BS.unpack bs)
        -- addLineTS ts str'
        if stringEndsWith str' eod then return str'
        else waitForOutput2' str' ts h eod
    else waitForOutput2' str ts h eod
    



   