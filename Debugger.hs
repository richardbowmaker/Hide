
module Debugger
( 
    onDebugDebug,
    toggleBreakPoint

) where 
 
-- library imports
 
import Control.Exception
import Data.Bits (testBit)
import Graphics.Win32.GDI.Types (HWND)
import System.Directory (removeFile)
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
            mtw <- GH.openWindow ss
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
startDebug ss fp = do
    --  delete object file, to force GHCi to run in interpretative mode
    result <- try (removeFile $ (Win.dropExtension fp) ++ ".o")  :: IO (Either IOException ())
    GH.sendCommand hwnd $ ":load " ++ fp
    return ()

        
        


{-

check if breakpoint already set

-}


