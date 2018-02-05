
module Compile
(
    cpBuildProject,
    cpCompileFile
) where


-- library imports
import Control.Concurrent 
import Control.Concurrent.STM
import qualified Control.Concurrent.Thread as Thread
import Control.Monad (liftM)
import Control.Monad.Loops
import qualified Data.ByteString.Char8 as BS (ByteString, hGetLine, readFile, pack, putStrLn, writeFile)
import qualified Data.ByteString as BS (append)
import Data.List (find, findIndex)
import Data.Word (Word64)
import Graphics.UI.WX
import Graphics.UI.WXCore
import Numeric (showHex)
import Text.Printf (printf)
import System.IO
import System.FilePath.Windows (takeFileName)
import System.Process
import System.Process.Common

-- project imports
import Debug
import Misc
import Session
   
    
-- build the project
-- optional final function called in GUI thread on completion  
cpBuildProject :: Session -> Maybe (IO ()) -> IO ()
cpBuildProject ss finally = do

    -- ?? clean up object files

    otClear ss
    otAddLine ss $ BS.pack "Build started ..."

    forkIO $ runExtCmd         
        "D:\\_Rick's\\haskell\\HeyHo\\build.bat" 
        ["heyho"] 
        "D:\\_Rick's\\haskell\\HeyHo" 
        (ssTOutput ss)
        (ssTOutput ss)
        (ssCFunc ss)
        finally
        
    return ()
 
-- compile the specified file
-- optional final function called in GUI thread on completion  
cpCompileFile :: Session -> String -> Maybe (IO ()) -> IO ()
cpCompileFile ss fp finally = do

    otClear ss
    otAddLine ss $ BS.pack "Compile started ..."

    -- ? delete object file 

    forkIO $ runExtCmd 
        "C:\\Program Files\\Haskell Platform\\8.0.1\\bin\\ghc" ["-c", fp] 
        "D:\\_Rick's\\haskell\\HeyHo"
        (ssTOutput ss) -- stdout goes to TOutput
        (ssTOutput ss)
        (ssCFunc ss)
        finally

    return ()
 
               

-- run command and redirect std out to the output pane
-- command -> arguments -> working directory -> stdout TChan -> stderr TChan -> completion function
runExtCmd :: String -> [String] -> String -> TOutput -> TOutput -> FunctionChannel -> Maybe (IO ()) -> IO ()
runExtCmd cmd args dir cout cerr cfn mfinally = do    
    (_, Just hout, Just herr, ph) <- createProcess_ "errors" (proc cmd args)
        {cwd = Just dir, std_out = CreatePipe, std_err = CreatePipe}

--    (_, waits) <- Thread.forkIO $ streamToChan hout cout
    (_, waite) <- Thread.forkIO $ streamToChan herr cerr
    
--    Thread.result =<< waits
    Thread.result =<< waite
    waitForProcess ph
    
    -- schedule finally function to be called in gui thread
    maybe (return ()) (\f -> atomically $ writeTChan cfn f) mfinally
   
    where
    
        streamToChan h tot = whileM_ (liftM not $ hIsEOF h) (BS.hGetLine h >>= (\s -> atomically $ writeTChan tot s)) 
        


