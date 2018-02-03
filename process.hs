

module Main where
import Graphics.UI.WX
import Graphics.UI.WXCore
import System.Process.Common
import System.Process
import System.IO
import Control.Monad (unless)
import Control.Exception (try, throwIO)
import Control.Concurrent
-- import Control.Concurrent.MVar
import Control.Concurrent.STM
import qualified GHC.IO.Exception as G
import Pipes

main = start mainGUI


stdoutLn :: Consumer String IO ()
stdoutLn = do
    str <- await  -- 'await' a 'String'
    x   <- lift $ try $ putStrLn str
    case x of
        -- Gracefully terminate if we got a broken pipe error
        Left e@(G.IOError { G.ioe_type = t}) ->        
            lift $ unless (t == G.ResourceVanished) $ throwIO e
        -- Otherwise loop
        Right () -> stdoutLn
        
compile :: IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)        
compile = do
    x <- createProcess_ "errors" (proc "D:\\_Rick's\\haskell\\HeyHo\\build.bat" ["heyho"]){ cwd = Just "D:\\_Rick's\\haskell\\HeyHo",
                                        std_out = CreatePipe }
    return (x)
    
compile' :: IO Handle        
compile' = do

    (_, Just hout, _, _) <- createProcess_ "errors" (proc "D:\\_Rick's\\haskell\\HeyHo\\build.bat" ["heyho"]){ cwd = Just "D:\\_Rick's\\haskell\\HeyHo",
                                        std_out = CreatePipe }
    return (hout)
    
type TText = TVar String

mainGUI :: IO ()
mainGUI = do 
  
    -- main window
    f <- frame []    
    
    p <- panel f []
     
    -- create statusbar field
    sf1 <- statusField [text := "SF1", statusWidth := 20]
    sf2 <- statusField [text := "SF2"]
    set f [statusBar := [sf1,sf2]]
    
    tv <- atomically $ newTVar ""
 
    -- create a timer that updates the display
    t <- timer f [interval := 50, on command := updateDisplay tv ]

--   h <- openFile "D:\\_Rick's\\haskell\\HeyHo\\ProxyDllClient.hs" ReadMode 
--    forkIO $ myReadFile h tv
--    hClose h
                                       
{- 
    -- set the statusbar and menubar
    
    h <- compile'
    runEffect $ lift (hGetContents h) >-> stdoutLn

--    runEffect $ lift getLine >~ stdoutLn
-}

    forkIO $ doCompile tv


    return ()
    
doCompile :: TText -> IO ()
doCompile tv = do
    (_, Just hout, _, _) <- createProcess_ "errors" (proc "D:\\_Rick's\\haskell\\HeyHo\\build.bat" ["heyho"]){ cwd = Just "D:\\_Rick's\\haskell\\HeyHo",
                                        std_out = CreatePipe }
    myReadFile hout tv    
    return ()

{-    
    std_out = CreatePipe }
    str <- hGetContents hout
    putMVar tv "compiling"
    putMVar tv str    
    putMVar tv "finished"
-}

 
updateDisplay :: TText -> IO ()
updateDisplay tv = do
    str <- atomically $ swapTVar tv ""
    case (str) of
        "" -> return ()
        str -> putStr str
        
  
myReadFile :: Handle -> TText -> IO ()
myReadFile h tv = do
    eof <- hIsEOF h
    if eof then do 
        hClose h
        return ()
    else do
        s <- hGetLine h
--        putStrLn s
        atomically (do
            s1 <- readTVar tv
            writeTVar tv $ s1 ++ s ++ "\n")
        myReadFile h tv


