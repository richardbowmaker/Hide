
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
import qualified Data.ByteString.Char8 as BS (ByteString, hGetLine, readFile, pack, putStrLn, unpack, writeFile)
import qualified Data.ByteString as BS (append, empty)
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
import Misc
import Session
      

data CompError = CompError { filename :: String, srcLine :: Int, srcCol :: Int, errIx :: Int, errLines :: [String] } deriving (Show)


-- build the project
-- optional final function called in GUI thread on completion  
cpBuildProject :: Session -> Maybe (IO ()) -> IO ()
cpBuildProject ss finally = do
{-
    -- ?? clean up object files

    otClear ss
    otAddLine ss $ BS.pack "Build started ..."

    forkIO $ runExtCmd ss        
        "D:\\_Rick's\\haskell\\HeyHo\\build.bat" 
        ["heyho"] 
        "D:\\_Rick's\\haskell\\HeyHo" 
        (ssTOutput ss)
        (ssTOutput ss)
        Nothing
        finally
-}        
    return ()
 
-- compile the specified file
-- optional final function called in GUI thread on completion  
cpCompileFile :: Session -> String -> Maybe (IO ()) -> IO ()
cpCompileFile ss fp mfinally = do

    ssDebugInfo ss $ "Start compile: " ++ fp

    otClear ss
    otAddLine ss $ BS.pack "Compile started ..."

    --  delete old object file
    result <- try (removeFile $ (Win.dropExtension fp) ++ ".o")  :: IO (Either IOException ())
  
    forkIO $ runGHC 
        ss
        ["-c", fp] 
        "D:\\_Rick's\\haskell\\HeyHo"
        (ssTOutput ss) -- stdout goes to TOutput
        (Just $ cpCompileFileDone ss mfinally)

    return ()
 
cpCompileFileDone :: Session -> Maybe (IO ()) -> (Int, [CompError]) -> IO ()
cpCompileFileDone ss mfinally (errCount, errors) = do
    
    if errCount == 0 then
        outStr "No errors"
    else 
        outStr $ (show errCount) ++ " errors"

    -- temp debug
    outStr $ concat $ map (\ce -> (compErrorToString ce) ++ "\n" ) errors

    -- schedule GUI finally function
    maybe (return ()) (\f-> atomically $ writeTChan (ssCFunc ss) f) mfinally

    return ()

    where outStr s = atomically $ writeTChan (ssTOutput ss) $ BS.pack s

-- run command and redirect std out to the output pane
-- session -> arguments -> working directory -> stdout TChan -> completion function
runGHC :: Session -> [String] -> String -> TOutput -> Maybe ((Int, [CompError]) -> IO ()) -> IO ()
runGHC ss args dir cerr mfinally = do
    
    ssDebugInfo ss $ "Run GHC: " ++ (concat $ map (\s -> s ++ "|") args) ++ " dir: " ++ dir

    -- duplicate the output channel so that we can capture the compiler 
    -- output for parsing
    cerrs <- atomically $ dupTChan cerr
 
    (_, _, Just herr, ph) <- createProcess_ "errors" (proc "C:\\Program Files\\Haskell Platform\\8.0.1\\bin\\ghc" args)
        {cwd = Just dir, std_err = CreatePipe}

    -- stream the compile results to the output pane
    (_, waite) <- Thread.forkIO $ streamToChan ss herr cerr
    
    -- stream results to string
    (_, waits) <- Thread.forkIO $ captureChannel ss cerrs BS.empty

    -- wait for ghc to complete
    Thread.result =<< waite
    bs <- Thread.result =<< waits
    waitForProcess ph

   -- parse the error results
    case (P.parse errorFile "" $ BS.unpack bs) of
        Left _   -> ssDebugError ss "Parse of compilation errors failed"
        Right es -> do
            ssDebugInfo ss $ "parsed ok"
            maybe (return ()) (\f -> f (length es, es)) mfinally
 
{-
-- run command and redirect std out to the output pane
-- session -> command -> arguments -> working directory -> stdout TChan -> stderr TChan -> completion function
runExtCmd :: Session -> String -> [String] -> String -> TOutput -> TOutput -> FunctionChannel -> Maybe (IO ()) -> IO ()
runExtCmd ss cmd args dir cout cerr mfinally = do
    
    ssDebugInfo ss $ "Run process: " ++ cmd ++ " " ++ (concat $ map (\s -> s ++ "|") args) ++ " dir: " ++ dir
 
    (_, Just hout, Just herr, ph) <- createProcess_ "errors" (proc cmd args)
        {cwd = Just dir, std_out = CreatePipe, std_err = CreatePipe}

--    (_, waits) <- Thread.forkIO $ streamToChan hout cout
    (_, waite) <- Thread.forkIO $ streamToChan herr cerr
    
--    Thread.result =<< waits
    Thread.result =<< waite
    waitForProcess ph
    
    -- run completion function
    maybe (return ()) (\f -> f) mfinally
   
    where
    
        streamToChan h tot = whileM_ (liftM not $ hIsEOF h) (BS.hGetLine h >>= (\s -> atomically $ writeTChan tot s)) 
        
-}

streamToChan :: Session -> Handle -> TChan BS.ByteString -> IO ()
streamToChan ss h tot = do
    whileM_ (liftM not $ hIsEOF h) (BS.hGetLine h >>= (\s -> atomically $ writeTChan tot s))
    atomically $ writeTChan tot $ BS.pack "\nDone"
    return ()
        
captureChannel :: Session -> TChan BS.ByteString -> BS.ByteString -> IO BS.ByteString
captureChannel ss chn str = do
    b <- atomically $ isEmptyTChan chn 
    if b then captureChannel ss chn str
    else do
        s <- atomically $ readTChan chn
        if s == BS.pack "Done" then return str
        else captureChannel ss chn (BS.append str s)

------------------------------------------
-- compiler output parser
------------------------------------------

compErrorToString :: CompError -> String
compErrorToString c =
    "Filename: " ++ (show $ filename c) ++ " (" ++ (show $ srcLine c) ++ "," ++ (show $ srcCol c) ++ ") errout = " ++ (show $ errIx c) ++ "\n" ++
        (concat $ map (\s -> " " ++ s ++ "\n") (errLines c))
       
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


