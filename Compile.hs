{-# LANGUAGE BangPatterns #-}

module Compile
(
    cpBuildProject,
    cpCompileFile,
) where


-- library imports
import Control.Concurrent 
import Control.Concurrent.STM
import qualified Control.Concurrent.Thread as Thread
import Control.Exception
import Control.Monad (liftM)
import Control.Monad.Loops
import qualified Data.ByteString.Char8 as BS (ByteString, hGetContents, hGetLine, hPutStr, readFile, pack, putStrLn, unpack, writeFile)
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
 
cpCompileFileDone :: Session -> Maybe (IO ()) -> [CompError] -> IO ()
cpCompileFileDone ss mfinally ces = do
    
    if length ces == 0 then
        outStr "\nNo errors\n"
    else 
        outStr $ "\n" ++ (show $ length ces) ++ " errors\n"

    -- save compilation results to session
    atomically $ writeTVar (ssCompilerReport ss) ces

    -- schedule GUI finally function
    maybe (return ()) (\f-> atomically $ writeTChan (ssCFunc ss) f) mfinally

    return ()

    where outStr s = atomically $ writeTChan (ssTOutput ss) $ BS.pack s

-- run command and redirect std out to the output pane
-- session -> arguments -> working directory -> stdout TChan -> completion function
runGHC :: Session -> [String] -> String -> TOutput -> Maybe ([CompError] -> IO ()) -> IO ()
runGHC ss args dir cerr mfinally = do
    
    ssDebugInfo ss $ "Run GHC: " ++ (concat $ map (\s -> s ++ "|") args) ++ " dir: " ++ dir

    (_, _, Just herr, ph) <- createProcess_ "errors" (proc "C:\\Program Files\\Haskell Platform\\8.0.1\\bin\\ghc" args)
        {cwd = Just dir, std_err = CreatePipe}

    -- stream compiler output to output pane
    s <- captureOutput ss herr (ssTOutput ss) ""

    case (P.parse errorFile "" s) of
        Left _   -> ssDebugError ss "Parse of compilation errors failed"
        Right es -> do
            ssDebugInfo ss $ "parsed ok"           
            maybe (return ()) (\f -> f es) mfinally

-- captures output from handle, wrtes to the output pane and returns
-- the captured data
captureOutput :: Session -> Handle -> TChan BS.ByteString -> String -> IO String
captureOutput ss h tot str = do
    eof <- hIsEOF h
    if eof then do
        return str
    else do
        bs <- BS.hGetLine h -- NB hGetLine is appending a CR on the end of the line !!
        atomically $ writeTChan tot bs  -- write to output pane
        -- remove CR and append new line, required by parser
        captureOutput ss h tot $ str ++ (init (BS.unpack bs)) ++ "\n"

------------------------------------------
-- compiler output parser
------------------------------------------

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
    return $ ceCompError fn el ec (P.sourceLine pos) els

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


