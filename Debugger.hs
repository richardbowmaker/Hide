
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
            mtw <- GH.openDebugWindow ss
            case mtw of
                Just tw -> startDebug ss tw fp
                Nothing -> return ()
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
    
startDebug :: SS.Session -> SS.TextWindow -> String -> IO ()
startDebug ss tw fp = do
    --  delete object file, to force GHCi to run in interpretative mode
    result <- try (removeFile $ (Win.dropExtension fp) ++ ".o")  :: IO (Either IOException ())
    let seq = [(load ss tw fp), (getModulesLookup ss tw >>= setBreakPoints ss tw)]
    ok <- MI.doUntilFalseIO seq
    if ok then SS.ssSetStateBit ss SS.ssStateDebugging
    else infoDialog (SS.ssFrame ss) CN.programTitle "Debug startup failed" 

load :: SS.Session -> SS.TextWindow -> String -> IO Bool
load ss tw fp = do
    ms <- sendCommand ss tw $ ":load " ++ fp
    case ms of
        Just _  -> return True
        Nothing -> return False

deleteBreakPoints :: SS.Session -> SS.TextWindow -> IO Bool
deleteBreakPoints ss tw = do
    ms <- sendCommand ss tw ":delete *" 
    case ms of
        Just _  -> return True
        Nothing -> return False

getModulesLookup :: SS.Session -> SS.TextWindow -> IO ([(String, String)])
getModulesLookup ss tw = do
    ms <- sendCommand ss tw $ ":show modules" 
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

setBreakPoints :: SS.Session -> SS.TextWindow -> [(String, String)] -> IO Bool
setBreakPoints ss tw modules = do
    bps <- SS.dsGetBreakPoints ss
    bps' <- foldM (\bps' bp -> do
        mno <- setBreakPoint ss tw bp modules
        case mno of
            Just no -> return $ (SS.dsSetBreakPointNo bp no) : bps'
            Nothing -> return $ bp : bps') [] bps
    if length bps == length bps' then do
        SS.dsSetBreakPoints ss bps'
        return True
    else
        return False
  
setBreakPoint :: SS.Session -> SS.TextWindow -> SS.BreakPoint -> [(String, String)] -> IO (Maybe Int)
setBreakPoint ss tw bp modules = do
    l <- SC.markerLineFromHandle (SS.dsEditor bp) (SS.dsHandle bp)
    let mod = maybe "" id $ lookup (takeFileName $ SS.dsFilePath bp) modules
    ms <- sendCommand ss tw $ ":break " ++ mod ++ " " ++ (show l)
    case ms of
        Just s -> do
            if MI.stringStartsWith s "Breakpoint" then
                return $ MI.scanInt s
            else 
                return Nothing
        Nothing -> return Nothing 

sendCommand :: SS.Session -> SS.TextWindow -> String -> IO (Maybe String)
sendCommand ss tw cmd = do
    let hwnd = SS.twHwnd tw
    SS.dsClearDebugOutput ss
    GH.sendCommand hwnd cmd
    waitForResponse ss 100 100

-- wait for response from GHCI, try n times with delay ms inbetween
waitForResponse :: SS.Session -> Int -> Int -> IO (Maybe String)
waitForResponse _ 0 _  = return Nothing
waitForResponse ss n delay = do
    s <- SS.dsGetDebugOutput ss
    if MI.stringEndsWith s "Main> " then return $ Just s
    else threadDelay (delay * 1000) >> waitForResponse ss (n-1) delay



