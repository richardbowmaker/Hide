
----------------------
-- Compile module
----------------------

module Compile
(
    cpBuildProject,
    cpCompileFile,
    cpDebugRun,
    onBuildCompile,
    onBuildBuild,
    onBuildGhci
) where

-- library imports
import Control.Concurrent 
import Control.Concurrent.STM
import qualified Control.Concurrent.Thread as Thread
import Control.Exception
import Control.Monad (liftM)
import Control.Monad.Loops
import Data.Bits ((.&.), (.|.), xor)
import qualified Data.ByteString.Char8 as BS (ByteString, hGetContents, hGetLine, hPutStr, readFile, pack, putStrLn, unpack, writeFile)
import qualified Data.ByteString as BS (append, empty)
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

-- project imports
import qualified Constants as CN
import qualified Ghci as GH
import qualified Misc as MI
import qualified OutputPane as OT
import qualified Parsers as PR
import qualified Scintilla as SC
import qualified Session as SS

------------------------------------------------------------    
-- Build Menu handlers
------------------------------------------------------------    
    
onBuildBuild :: SS.Session -> SS.TextWindow -> SC.Editor -> IO Bool -> (String -> IO ()) -> IO ()
onBuildBuild ss tw scn fileSave fileOpen = do

    SS.ssSetStateBit ss SS.ssStateCompile

    set (SS.ssMenuListGet ss CN.menuDebugNextError)     [enabled := False]
    set (SS.ssMenuListGet ss CN.menuDebugPreviousError) [enabled := False]
    set (SS.ssMenuListGet ss CN.menuBuildBuild)         [enabled := False]        
    set (SS.ssMenuListGet ss CN.menuBuildCompile)       [enabled := False]
    set (SS.ssMenuListGet ss CN.menuBuildGhci)          [enabled := False]
    set (SS.ssMenuListGet ss CN.menuDebugRun)           [enabled := False]

    OT.openOutputWindow ss fileOpen

    -- save file first
    ans <- fileSave
    if ans then do
        -- get again in case filename changed
        mhw <- SS.hwFindWindow ss (\hw -> SS.hwMatchesHwnd hw (SS.twPanelHwnd tw))
        case mhw of
            Just hw -> do
                mfp <- SS.hwFilePath hw
                case mfp of
                    Just fp -> cpBuildProject ss fp (Just $ compileComplete ss)
                    Nothing -> return ()
            Nothing -> do
                    SS.ssDebugError ss "onBuildBuild:: no file name set"
    else return ()
    
onBuildCompile :: SS.Session -> SS.TextWindow -> SC.Editor -> IO Bool -> (String -> IO ()) -> IO ()
onBuildCompile ss tw scn fileSave fileOpen = do

    set (SS.ssMenuListGet ss CN.menuDebugNextError)     [enabled := False]
    set (SS.ssMenuListGet ss CN.menuDebugPreviousError) [enabled := False]
    set (SS.ssMenuListGet ss CN.menuBuildBuild)         [enabled := False]        
    set (SS.ssMenuListGet ss CN.menuBuildCompile)       [enabled := False]
    set (SS.ssMenuListGet ss CN.menuBuildGhci)          [enabled := False]
    set (SS.ssMenuListGet ss CN.menuDebugRun)           [enabled := False]

    OT.openOutputWindow ss fileOpen

    -- save file first
    ans <- fileSave
    if ans then do
        -- get again in case filename changed
        mhw <- SS.hwFindWindow ss (\hw -> SS.hwMatchesHwnd hw (SS.twPanelHwnd tw))
        case mhw of
            Just hw -> do
                mfp <- SS.hwFilePath hw
                case mfp of
                    Just fp -> do
                        SS.ssSetStateBit ss SS.ssStateCompile
                        cpCompileFile ss fp (Just $ compileComplete ss)
                    Nothing -> return ()
            Nothing -> do
                    SS.ssDebugError ss "onBuildCompile:: no file name set"
    else return ()
               
compileComplete :: SS.Session -> IO ()
compileComplete ss = do
    set (SS.ssMenuListGet ss CN.menuBuildBuild)   [enabled := True]        
    set (SS.ssMenuListGet ss CN.menuBuildCompile) [enabled := True]
    set (SS.ssMenuListGet ss CN.menuBuildGhci)    [enabled := True]
    set (SS.ssMenuListGet ss CN.menuDebugRun)     [enabled := True]
    SS.ssClearStateBit ss SS.ssStateCompile
    OT.addText ss $ BS.pack "Compile complete\n"
    return ()

onBuildGhci :: SS.Session -> SS.TextWindow -> SC.Editor -> IO Bool -> (String -> IO ()) -> IO ()
onBuildGhci ss tw scn fileSave fileOpen = do

    set (SS.ssMenuListGet ss CN.menuBuildBuild)   [enabled := False]        
    set (SS.ssMenuListGet ss CN.menuBuildCompile) [enabled := False]
    set (SS.ssMenuListGet ss CN.menuBuildGhci)    [enabled := False]

   -- save file first
    ans <- fileSave
    if ans then do
        -- get again in case filename changed
        mhw <- SS.hwFindWindow ss (\hw -> SS.hwMatchesHwnd hw (SS.twPanelHwnd tw))
        case mhw of
            Just hw -> do
                mfp <- SS.hwFilePath hw
                case mfp of
                    Just fp -> do
                        OT.openOutputWindow ss fileOpen
                        SS.ssSetStateBit ss SS.ssStateCompile
                        cpCompileFile ss fp (Just $ ghciComplete ss hw)
                    Nothing -> return ()
            Nothing -> do
                    SS.ssDebugError ss "onBuildGhci:: no file name set"
    else return ()

ghciComplete :: SS.Session -> SS.HideWindow -> IO ()
ghciComplete ss hw = do

    set (SS.ssMenuListGet ss CN.menuBuildBuild)   [enabled := True]        
    set (SS.ssMenuListGet ss CN.menuBuildCompile) [enabled := True]
    set (SS.ssMenuListGet ss CN.menuBuildGhci)    [enabled := True]
    OT.addText ss $ BS.pack "Compile complete\n"

    SS.ssClearStateBit ss SS.ssStateCompile

    --  delete object file, force GHCI to load .hs file
    mfp <- SS.hwFilePath hw
    case mfp of
        Just fp -> do
                try (removeFile $ (Win.dropExtension fp) ++ ".o")  :: IO (Either IOException ())
                return ()
        Nothing -> return ()

    nerrs <- SS.crGetNoOfErrors ss
    if nerrs == 0 then do
        SS.ssWriteToOutputChan ss "\nNo errors\n"
        set (SS.ssMenuListGet ss CN.menuDebugNextError)     [enabled := False]
        set (SS.ssMenuListGet ss CN.menuDebugPreviousError) [enabled := False]
        GH.openWindowFile ss $ SS.hwWindow hw 
    else do
        SS.ssWriteToOutputChan ss $ "\n" ++ (show nerrs) ++ " errors\n"
        set (SS.ssMenuListGet ss CN.menuDebugNextError)     [enabled := True]
        set (SS.ssMenuListGet ss CN.menuDebugPreviousError) [enabled := True]
        ans <- proceedDialog (SS.ssFrame ss) CN.programTitle "There were compilation errors, continue ?"
        case ans of
            True -> GH.openWindowFile ss $ SS.hwWindow hw 
            False -> return ()

-- | Build the project
cpBuildProject ::   SS.Session             -- ^ The HIDE session
                    -> String           -- ^ Filename of project to build
                    -> Maybe (IO ())    -- ^ Optional function called on completion in GUI thread
                    -> IO ()            
cpBuildProject ss fp mfinally = do  
   
-- ghc -fasm -L. -lScintillaProxy -threaded -o %1 %1.hs

    SS.ssDebugInfo ss $ "Start build: " ++ fp

    OT.clear ss
    OT.addLine ss $ BS.pack "Build started ..."

    --  delete old object file
    result <- try (removeFile $ (Win.dropExtension fp) ++ ".o")  :: IO (Either IOException ())
  
    forkIO $ runGHC 
        ss
        ["-fasm", "-L.", "-lScintillaProxy", "-threaded", "-o", Win.dropExtension fp, fp] 
        "D:\\_Rick's\\haskell\\Hide"
        (SS.ssTOutput ss) -- stdout goes to TOutput
        (Just $ cpCompileFileDone ss mfinally)

    return ()
  
-- | compile the specified file
-- optional final function called in GUI thread on completion  
cpCompileFile :: SS.Session -> String -> Maybe (IO ()) -> IO ()
cpCompileFile ss fp mfinally = do

    SS.ssDebugInfo ss $ "Start compile: " ++ fp

    OT.clear ss
    OT.addLine ss $ BS.pack "Compile started ..."

    --  delete old object file
    result <- try (removeFile $ (Win.dropExtension fp) ++ ".o")  :: IO (Either IOException ())
  
    forkIO $ runGHC 
        ss
        ["-c", fp] 
        "D:\\_Rick's\\haskell\\Hide"
        (SS.ssTOutput ss) -- stdout goes to TOutput
        (Just $ cpCompileFileDone ss mfinally)

    return ()
 
cpCompileFileDone :: SS.Session -> Maybe (IO ()) -> SS.CompReport -> IO ()
cpCompileFileDone ss mfinally ces = do
    
    if SS.crErrorCount ces == 0 then do
        SS.ssWriteToOutputChan ss "\nNo errors\n"
        set (SS.ssMenuListGet ss CN.menuDebugNextError)     [enabled := False]
        set (SS.ssMenuListGet ss CN.menuDebugPreviousError) [enabled := False]
    else do
        SS.ssWriteToOutputChan ss $ "\n" ++ (show $ SS.crErrorCount ces) ++ " errors\n"
        set (SS.ssMenuListGet ss CN.menuDebugNextError)     [enabled := True]
        set (SS.ssMenuListGet ss CN.menuDebugPreviousError) [enabled := True]

    -- save compilation results to session
    SS.ssSetCompilerReport ss ces
    SS.ssClearStateBit ss SS.ssStateCompile

    -- schedule GUI finally function
    maybe (return ()) (\f-> atomically $ writeTChan (SS.ssCFunc ss) f) mfinally

    return ()

cpDebugRun :: SS.Session -> SS.TextWindow -> IO ()
cpDebugRun ss tw = do
    mfp <- SS.twFilePath tw
    case mfp of
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
-- session -> arguments -> working directory -> stdout TChan -> completion function
runGHC :: SS.Session -> [String] -> String -> SS.TOutput -> Maybe (SS.CompReport -> IO ()) -> IO ()
runGHC ss args dir cerr mfinally = do
    
    SS.ssDebugInfo ss $ "Run GHC: " ++ (concat $ map (\s -> s ++ "|") args) ++ " dir: " ++ dir

    (hr, hw) <- createPipe

    (_, _, _, ph) <- createProcess_ "errors" (proc "C:\\Program Files\\Haskell Platform\\8.0.1\\bin\\ghc.exe" args)
        {cwd = Just dir, std_out = UseHandle hw, std_err = UseHandle hw}

    -- stream compiler output to output pane
    s <- captureOutput ss hr (SS.ssTOutput ss) ""

    waitForProcess ph

    case (P.runParser PR.errorFile 0 "" s) of
        Left _   -> SS.ssDebugError ss "Parse of compilation errors failed"
        Right report -> do
            SS.ssDebugInfo ss $ "parsed ok\n" ++ (SS.compErrorsToString $ SS.crErrors report)
            maybe (return ()) (\f -> f report) mfinally

-- captures output from handle, wrtes to the output pane and returns
-- the captured data
captureOutput :: SS.Session -> Handle -> TChan BS.ByteString -> String -> IO String
captureOutput ss h tot str = do
    eof <- hIsEOF h
    if eof then do
        return str
    else do
        bs <- BS.hGetLine h -- NB hGetLine is appending a CR on the end of the line !!
        atomically $ writeTChan tot bs  -- write to output pane
        -- remove CR and append new line, required by parser
        captureOutput ss h tot $ str ++ (init (BS.unpack bs)) ++ "\n"

