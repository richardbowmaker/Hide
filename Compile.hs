
----------------------
-- Compile module
----------------------

module Compile
(
    cpBuildProject,
    cpCompileFile,
    cpDebugRun,
    onBuildCompile,
    onBuildBuild
) where

-- library imports
import Control.Concurrent 
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import qualified Control.Concurrent.Thread as Thread
import Control.Exception
import Control.Monad (liftM)
import Control.Monad.Loops
import Data.Bits ((.&.), (.|.), xor)
--import qualified Data.ByteString.Char8 as BS (ByteString, hGetContents, hGetLine, hPutStr, readFile, pack, putStrLn, unpack, writeFile)
--import qualified Data.ByteString as BS (append, empty)
import Data.List (find, findIndex, isInfixOf)
import qualified Data.Text as TX (append, init, pack, Text, unpack)
import qualified Data.Text.IO as TX (hGetChunk, hGetLine)
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

-- project imports
import qualified Constants as CN
import qualified Ghci as GH
import qualified Menus as MN
import qualified Misc as MI
import qualified OutputPane as OT
import qualified Parsers as PR
import qualified Scintilla as SC
import qualified Session as SS

------------------------------------------------------------    
-- Build Menu handlers
------------------------------------------------------------    
    
onBuildBuild :: SS.Session -> SS.TextWindow -> SC.Editor -> IO ()
onBuildBuild ss tw scn = do

    -- save file first
    ans <- (SS.ssFileSave ss) ss tw scn
    if ans then do
        -- get again in case filename changed
        mtw <- SS.twFindWindow ss (\tw' -> SS.twMatchesHwnd tw' (SS.twPanelHwnd tw))
        case mtw of
            Just tw' -> do
                case SS.twFilePath tw' of
                    Just fp -> do
                        SS.ssSetStateBit ss SS.ssStateCompile
                        SS.ssSetMenus ss
                        OT.openOutputWindow ss
                        cpBuildProject ss fp (Just $ compileComplete ss)
                        SS.twSetFocus tw
                    Nothing -> do
                        SS.ssDebugError ss "onBuildBuild:: no file name set"
                        return ()
            Nothing -> return ()
    else return ()
    
onBuildCompile :: SS.Session -> SS.TextWindow -> SC.Editor -> IO ()
onBuildCompile ss tw scn = do

    -- save file first
    ans <- (SS.ssFileSave ss) ss tw scn
    if ans then do
        -- get again in case filename changed
        mtw <- SS.twFindWindow ss (\tw' -> SS.twMatchesHwnd tw' (SS.twPanelHwnd tw))
        case mtw of
            Just tw' -> do
                case SS.twFilePath tw' of
                    Just fp -> do
                        SS.ssSetStateBit ss SS.ssStateCompile
                        SS.ssSetMenus ss
                        OT.openOutputWindow ss
                        cpCompileFile ss fp (Just $ compileComplete ss)
                        SS.twSetFocus tw
                    Nothing -> do
                        SS.ssDebugError ss "onBuildBuild:: no file name set"
                        return ()
            Nothing -> return ()
    else return ()
               
compileComplete :: SS.Session -> IO ()
compileComplete ss = do

    SS.ssClearStateBit ss SS.ssStateCompile
    SS.ssSetMenus ss

    OT.addTextS ss $ "Compile complete\n"
    return ()

-- | Build the project
cpBuildProject ::   SS.Session          -- ^ The HIDE session
                    -> String           -- ^ Filename of project to build
                    -> Maybe (IO ())    -- ^ Optional function called on completion in GUI thread
                    -> IO ()            
cpBuildProject ss fp mfinally = do  
   
    SS.ssDebugInfo ss $ "Start build: " ++ fp

    OT.clear ss
    OT.addLineS ss "Build started ..."

    --  delete old object file
    result <- try (removeFile $ (Win.dropExtension fp) ++ ".o")  :: IO (Either IOException ())
  
    forkIO $ runGHC 
        ss
        ["-fasm", "-L.", "-lScintillaProxy", "-threaded", "-o", Win.dropExtension fp, fp] 
        (Win.takeDirectory fp)
        (Just $ cpCompileFileDone ss mfinally)

    return ()
  
-- | compile the specified file
-- optional final function called in GUI thread on completion  
cpCompileFile :: SS.Session -> String -> Maybe (IO ()) -> IO ()
cpCompileFile ss fp mfinally = do

    SS.ssDebugInfo ss $ "Start compile: " ++ fp
    OT.clear ss
    OT.addLineS ss "Compile started ..."

    --  delete old object file
    result <- try (removeFile $ (Win.dropExtension fp) ++ ".o")  :: IO (Either IOException ())
  
    forkIO $ runGHC 
        ss
        ["-c", fp] 
        (Win.takeDirectory fp)
        (Just $ cpCompileFileDone ss mfinally)

    return ()
 

cpCompileFileDone :: SS.Session -> Maybe (IO ()) -> SS.CompReport -> IO ()
cpCompileFileDone ss mfinally ces = do
    
    if SS.crErrorCount ces == 0 then do
        SS.ssQueueFunction ss $ OT.addTextS ss "\nNo errors\n"
    else do
        SS.ssQueueFunction ss  $OT.addTextS ss ("\n" ++ (show $ SS.crErrorCount ces) ++ " errors\n")

    -- save compilation results to session
    SS.ssSetCompilerReport ss ces
    SS.ssClearStateBit ss SS.ssStateCompile

    -- schedule GUI finally function
    maybe (return ()) (SS.ssQueueFunction ss) mfinally

    return ()


cpDebugRun :: SS.Session -> SS.TextWindow -> IO ()
cpDebugRun ss tw = do 
    case SS.twFilePath tw of
        Just fp -> do
            SS.ssDebugInfo ss $ "Start run: " ++ fp
            --  check .exe file exists
            let exe = (Win.dropExtension fp) ++ ".exe"
            b <- doesFileExist exe
            if b then do
                catchIOError
                    (createProcess_ "errors" (proc exe []) {cwd = (Just $ Win.takeDirectory fp)} >> return ())
                    (\err -> SS.ssDebugError ss $ show err)
                return ()
            else infoDialog (SS.ssFrame ss) CN.programTitle $ "File: " ++ exe ++ " does not exist"
        Nothing -> return ()

-- run command and redirect std out to the output pane
-- session -> arguments -> working directory -> completion function
runGHC :: SS.Session -> [String] -> String -> Maybe (SS.CompReport -> IO ()) -> IO ()
runGHC ss args dir mfinally = do
    
    SS.ssDebugInfo ss $ "Run GHC: " ++ (concat $ map (\s -> s ++ "|") args) ++ " dir: " ++ dir

    (hor, how) <- createPipe
    (_, _, _, ph) <- createProcess (proc "C:\\Program Files\\Haskell Platform\\8.0.1\\bin\\ghc.exe" args)
        {cwd = Just dir, std_out = UseHandle how, std_err = UseHandle how}

    -- capture compiler output and stream it to the output pane
    tx <- captureOutput ss hor ph 
    SS.ssDebugInfo ss $ "compiler report: " ++ TX.unpack tx

    case P.runParser PR.errorFile 0 "" $ TX.unpack tx of
        Left _       -> SS.ssDebugError ss "Parse of compilation errors failed"
        Right report -> do
            SS.ssDebugInfo ss $ "parsed ok\n" ++ (SS.compErrorsToString $ SS.crErrors report)
            maybe (return ()) (\f -> f report) mfinally
   
-- the first parameter is a delay in ms, which is doubled each time on EOF, this is
-- because the linking phase can take several minutes with no output, and the stack might overflow
captureOutput :: SS.Session -> Handle -> ProcessHandle -> IO TX.Text
captureOutput = captureOutput' 10 $ TX.pack ""

captureOutput' :: Int -> TX.Text -> SS.Session -> Handle -> ProcessHandle -> IO TX.Text
captureOutput' delay output ss h ph = do
    mcode <- getProcessExitCode ph
    case mcode of
        Just _ -> return output
        Nothing -> do
            eof <- hIsEOF h         
            if eof then do
                threadDelay (1000 * delay)
                captureOutput' (min 1000 (delay * 2)) output ss h ph             
            else do
                tx <- TX.hGetLine h -- NB hGetLine is appending a CR on the end of the line !!
                SS.ssQueueFunction ss $ OT.addTextS ss $ TX.unpack tx
                -- remove CR and append new line, required by parser
                captureOutput' 10 (output `TX.append` TX.init tx `TX.append` TX.pack "\n") ss h ph

