
module Debugger
( 
    onDebugDebug,
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
                    startDebug ss fp
                Nothing -> SS.ssDebugError ss "GHCI didn't start"
        Nothing -> return ()

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
    
startDebug :: SS.Session -> String -> IO ()
startDebug ss fp = do
    --  delete object file, to force GHCi to run in interpretative mode
    result <- try (removeFile $ (Win.dropExtension fp) ++ ".o")  :: IO (Either IOException ())
    let seq = [(load ss fp), (getModulesLookup ss >>= setBreakPoints ss)]
    ok <- MI.doUntilFalseIO seq
    if ok then SS.ssSetStateBit ss SS.ssStateDebugging
    else infoDialog (SS.ssFrame ss) CN.programTitle "Debug startup failed" 

load :: SS.Session -> String -> IO Bool
load ss fp = do
    ms <- sendCommand ss (":load " ++ fp) "Main> " 30000
    case ms of
        Just _  -> return True
        Nothing -> return False

deleteBreakPoints :: SS.Session -> IO Bool
deleteBreakPoints ss = do
    ms <- sendCommand ss ":delete *" "Main> " 1000
    case ms of
        Just _  -> return True
        Nothing -> return False

getModulesLookup :: SS.Session -> IO ([(String, String)])
getModulesLookup ss = do
    ms <- sendCommand ss ":show modules" "Main> " 1000 
    case ms of
        Just s -> return $ map getModuleLookup (lines s)           
        Nothing -> return [] 

getModuleLookup :: String -> (String, String)
getModuleLookup s = 
    let ts = words s in
    if (length ts > 2) then
        (takeFileName $ ts !! 2, ts !! 0)
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
    ms <- sendCommand ss  (":break " ++ mod ++ " " ++ (show l))  "Main> " 1000
    case ms of
        Just s -> do
            let ws = words s
            if ws !! 0 == "Breakpoint" then
                return $ Just (read (ws !! 1) :: Int)
            else 
                return Nothing
        Nothing -> return Nothing 

sendCommand :: SS.Session -> String -> String -> Int -> IO (Maybe String)
sendCommand ss cmd eod timeout= do
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

    
   



