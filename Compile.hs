
module Compile
(
    cpBuildProject,
    cpCompileFile
) where


-- library imports
import Control.Concurrent 
import Control.Concurrent.STM
import qualified Control.Concurrent.Thread as Thread
import Control.Exception
import Control.Monad (liftM)
import Control.Monad.Loops
import qualified Data.ByteString.Char8 as BS (ByteString, hGetLine, readFile, pack, putStrLn, writeFile)
import qualified Data.ByteString as BS (append)
import Data.List (find, findIndex, isInfixOf)
import Data.Word (Word64)
import Graphics.UI.WX
import Graphics.UI.WXCore
import Numeric (showHex)
import qualified Text.ParserCombinators.Parsec as P -- (<|>, anyChar, char, GenParser, getPosition, many, sourceLine, string, try, parse)
import Text.Printf (printf)
import qualified System.FilePath.Windows as Win (dropExtension)
import System.Directory 
import System.IO
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

    --  delete old object file
    result <- try (removeFile $ (Win.dropExtension fp) ++ ".o")  :: IO (Either IOException ())
  
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
        

parseInThread :: TChan String -> IO [CompError]
parseInThread chn = do
    s <- captureChannel chn "" 
    case (P.parse errorFile "" s) of
        Left _ -> return []
        Right es -> return es
    

streamToChan :: Handle -> TChan String -> IO ()
streamToChan h tot = do
    whileM_ (liftM not $ hIsEOF h) (hGetLine h >>= (\s -> atomically $ writeTChan tot (s++"\n")))
    atomically $ writeTChan tot "EOF"
    return ()

chanToStdOut :: TChan String -> IO ()
chanToStdOut chn = do
    b <- atomically $ isEmptyTChan chn 
    if b then chanToStdOut chn
    else do
        s <- atomically $ readTChan chn
        putStrLn s
        if s == "EOF" then return ()
        else chanToStdOut chn

handleToStdOut :: Handle -> String -> IO ()
handleToStdOut h p = do
    b <- hIsEOF h
    if b then do
        putStrLn (p++"-> EOF")
        return ()
    else do
        s <- hGetLine h
        putStrLn (p++s)
        handleToStdOut h p
        
captureChannel :: TChan String -> String -> IO String
captureChannel chn str = do
    b <- atomically $ isEmptyTChan chn 
    if b then captureChannel chn str
    else do
        s <- atomically $ readTChan chn
        if s == "EOF" then return str
        else captureChannel chn (str++s)


chanToString :: TChan String -> IO [String]
chanToString chn = do
    let s = ""
    let s1 = ""
    s <- whileM (return (not $ isInfixOf "EOF" s)) 
            (do 
                    s1 <- atomically $ readTChan chn
                    putStrLn s1
                    let s = s ++ s1
                    return s)
    return (s) 


data CompError = CompError { filename :: String, srcLine :: Int, srcCol :: Int, errIx :: Int, errLines :: [String] } deriving (Show)

compErrorToString :: CompError -> String
compErrorToString c =
    "Filename: " ++ (show $ filename c) ++ " (" ++ (show $ srcLine c) ++ "," ++ (show $ srcCol c) ++ ") errout = " ++ (show $ errIx c) ++ "\n" ++
        (concat $ map (\s -> " " ++ s ++ "\n") (errLines c))
       

parseErrorFile :: String -> IO  (Maybe (Int, [CompError]))
parseErrorFile fn = do
    h <- openFile fn ReadMode 
    s <- hGetContents h
    case (P.parse errorFile "" s) of
        Left _ -> do
            hClose h
            return Nothing
        Right es -> do
            hClose h
            return $  Just (length es, es)

errorFile :: P.GenParser Char () [CompError]
errorFile = do
    errs <- P.many anError
    return errs

anError :: P.GenParser Char () CompError
anError = do
    P.string eol
    pos <- P.getPosition
    (fn, el, ec) <- fileName
    els <- errorDesc
    return (CompError fn el ec (P.sourceLine pos) els)

fileDrive :: P.GenParser Char () String
fileDrive = do
    c <- P.anyChar
    P.char ':'
    return $ c:":"

fileName :: P.GenParser Char () (String, Int, Int)
fileName = do    
    drive <- (P.try fileDrive P.<|> return "")
    path <- P.many (P.noneOf ":")
    P.char ':'
    line <- P.many (P.noneOf ":")
    P.char ':'
    col <- P.many (P.noneOf ":")
    P.string ": error:"
    P.string eol
    return (drive ++ path, read line, read col)

errorDesc :: P.GenParser Char () ([String])
errorDesc = do
    lines <- P.many errorLine
    return (lines)

errorLine :: P.GenParser Char () String
errorLine = do
    P.string "    "
    eline <- P.many (P.noneOf eol)
    P.string eol
    return eline
    
eol :: String
eol = "\n"


