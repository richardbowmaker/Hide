
module Debugger
( 
    onDebugDebug,
    onDebugStop,
    onDebugContinue,
    onDebugStep,
    toggleBreakPoint

) where 
 
-- library imports
 
import Control.Concurrent (threadDelay)
import Control.Exception
import Control.Monad (foldM)
import Data.Bits (testBit)
import Data.List (lines, lookup, words)
import Graphics.Win32.GDI.Types (HWND)
import Graphics.UI.WXCore.Dialogs (infoDialog)
import System.Directory (removeFile)
import System.FilePath.Windows (takeFileName)
import qualified System.FilePath.Windows as Win (dropExtension)

-- project imports

import qualified Constants as CN
import qualified Ghci as GH
import qualified Misc as MI
import qualified Parsers as PR
import qualified Scintilla as SC
import qualified ScintillaProxyImports as SI
import qualified Session as SS

onDebugDebug :: SS.Session -> SS.TextWindow -> IO ()
onDebugDebug ss tw = do
    mfp <- SS.twFilePath tw
    case mfp of
        Just fp -> do
            id <- SI.ghciNew "" ""
            ms <- SI.ghciWaitForResponse id "Prelude> " 10000
            case ms of 
                Just s -> do
                    SS.ssDebugInfo ss s
                    SS.dsSetSessionId ss id
                    startDebug ss id fp
                    return ()
                Nothing -> SS.ssDebugError ss "GHCI didn't start"
        Nothing -> return ()

onDebugStop :: SS.Session -> SS.TextWindow -> IO ()
onDebugStop ss tw = stopDebug ss

onDebugContinue :: SS.Session -> SS.TextWindow -> IO ()
onDebugContinue ss tw = continue ss >> return ()

onDebugStep :: SS.Session -> SS.TextWindow -> IO ()
onDebugStep ss tw = step ss >> return ()

toggleBreakPoint :: SS.Session -> SS.HideWindow -> SC.Editor -> SC.SCNotification -> IO ()
toggleBreakPoint ss hw scn sn = do
    l <- SC.getLineFromPosition scn (fromIntegral (SI.snPosition sn) :: Int)
    m <- SC.markerGet scn l
    if testBit m CN.breakPointMarker then do
        bps <- SS.dsGetBreakPoints ss
        -- remove breakpoint from session
        bps' <- MI.findAndRemoveIO (\bp -> do
            l' <- SC.markerLineFromHandle scn (SS.dsHandle bp)
            return $ l' == l) bps
        SS.dsUpdateBreakPoints ss (\_ -> bps')
        SC.markerDelete scn l CN.breakPointMarker
        s <- SS.dsBreakPointsToString ss
        SS.ssDebugInfo ss s 
        return ()
    else do
        h <- SC.markerAdd scn l CN.breakPointMarker
        mfp <- SS.hwFilePath hw
        let bp = SS.createBreakPoint scn (maybe "" id mfp) h 0 
        SS.dsAddBreakPoint ss bp
        s <- SS.dsBreakPointsToString ss
        SS.ssDebugInfo ss s 
        return ()
    
startDebug :: SS.Session -> Int -> String -> IO Bool
startDebug ss id fp = do
    -- delete object file, to force GHCi to run in interpretative mode
    -- result <- try (removeFile $ (Win.dropExtension fp) ++ ".o")  :: IO (Either IOException ())
    SI.ghciSetEventHandler id $ eventHandler ss
    let seq = [(load ss fp), (getModulesLookup ss >>= setBreakPoints ss), (run ss)]
    ok <- MI.doUntilFalseIO seq
    if ok then do
        SS.ssSetStateBit ss SS.ssStateDebugging
        return True
    else do
        SS.ssDebugError ss $ "Debug startup failed" 
        return False

stopDebug :: SS.Session -> IO ()
stopDebug ss = do
    sendCommand ss ":quit\n"
    SS.ssClearStateBit ss SS.ssStateDebugging
    SS.ssClearStateBit ss SS.ssStateRunning
    id <- SS.dsGetSessionId ss
    SI.ghciClose id
    SS.dsSetSessionId ss 0

load :: SS.Session -> String -> IO Bool
load ss fp = do
    ms <- sendCommandSynch ss (":load *" ++ fp) "Main> " 30000
    case ms of
        Just _  -> return True
        Nothing -> return False

run :: SS.Session -> IO Bool
run ss = do
    SS.ssSetStateBit ss SS.ssStateRunning
    sendCommandAsynch ss "main\n" "Main> "
    return True

continue :: SS.Session -> IO Bool
continue ss = do
    SS.ssSetStateBit ss SS.ssStateRunning    
    sendCommandAsynch ss ":continue\n" "Main> "
    return True

step :: SS.Session -> IO Bool
step ss = do
    SS.ssSetStateBit ss SS.ssStateRunning
    sendCommandAsynch ss ":step\n" "Main> "
    return True

deleteBreakPoints :: SS.Session -> IO Bool
deleteBreakPoints ss = do
    ms <- sendCommandSynch ss ":delete *" "Main> " 1000
    case ms of
        Just _  -> return True
        Nothing -> return False

getModulesLookup :: SS.Session -> IO ([(String, String)])
getModulesLookup ss = do
    ms <- sendCommandSynch ss ":show modules" "Main> " 1000 
    case ms of
        Just s -> return $ map getModuleLookup (lines s) 
        Nothing -> return [] 

getModuleLookup :: String -> (String, String)
getModuleLookup s = 
    let ts = words s in
    if (length ts > 2) then
        ((init . takeFileName) $ ts !! 2, ts !! 0)
    else
        ("", "")

setBreakPoints :: SS.Session -> [(String, String)] -> IO Bool
setBreakPoints ss modules = do
    bps <- SS.dsGetBreakPoints ss
    bps' <- foldM (\bps' bp -> do
        mno <- setBreakPoint ss bp modules
        case mno of
            Just no -> return $ (SS.dsSetBreakPointNo bp no) : bps'
            Nothing -> return $ bp : bps') [] bps
    if length bps == length bps' then do
        SS.dsSetBreakPoints ss bps'
        return True
    else
        return False
  
setBreakPoint :: SS.Session -> SS.BreakPoint -> [(String, String)] -> IO (Maybe Int)
setBreakPoint ss bp modules = do
    l <- SC.markerLineFromHandle (SS.dsEditor bp) (SS.dsHandle bp)
    let mod = maybe "" id $ lookup (takeFileName $ SS.dsFilePath bp) modules
    ms <- sendCommandSynch ss  (":break " ++ mod ++ " " ++ (show (l+1)))  "Main> " 1000
    case ms of
        Just s -> do
            let ws = words s
            if ws !! 0 == "Breakpoint" then
                return $ Just (read (ws !! 1) :: Int)
            else 
                return Nothing
        Nothing -> return Nothing 

sendCommand :: SS.Session -> String -> IO ()
sendCommand ss cmd = do
    id <- SS.dsGetSessionId ss
    SI.ghciSendCommand id cmd 

sendCommandAsynch :: SS.Session -> String -> String -> IO ()
sendCommandAsynch ss cmd eod = do
    id <- SS.dsGetSessionId ss
    SI.ghciSendCommandAsynch id cmd eod
    
sendCommandSynch :: SS.Session -> String -> String -> Int -> IO (Maybe String)
sendCommandSynch ss cmd eod timeout = do
    id <- SS.dsGetSessionId ss
    ms <- SI.ghciSendCommandSynch id cmd eod timeout
    case ms of 
        Just s -> do
            SS.ssDebugInfo ss $ "response to command: " ++ cmd
            SS.ssDebugInfo ss s
            return ms
        Nothing -> do
            SS.ssDebugError ss $ "bad response to command: " ++ cmd
            return Nothing
 
eventHandler :: SS.Session -> Int -> String -> IO ()
eventHandler ss id str = do
    SS.ssDebugInfo ss $ "event handler: " ++ str
    b <- SS.ssTestState ss SS.ssStateRunning
    if b then do
        case PR.parseDebuggerOutput str of
            Just dg -> SS.ssDebugInfo ss $ "Debugger output parsed OK\n" ++ show dg
            Nothing -> SS.ssDebugError ss $ "Failed to parse debugger output:\n" ++ str
        SS.ssClearStateBit ss SS.ssStateRunning
    else return () -- should send this to the output pane

    
    


   



