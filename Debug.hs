
module Debug
(
    debugError,
    debugWarn,
    debugInfo,
    debugOut,
    debugPause
) where

import qualified Data.ByteString.Char8 as BS (pack)
import Foreign.Ptr (FunPtr, Ptr, minusPtr, nullPtr)
import Numeric (showHex)
import Graphics.UI.WXCore
import Data.Word (Word64)

import Scintilla

debugError :: ScnEditor -> String -> IO ()
debugError e s = debugOut e ("Error: " ++ s)

debugWarn :: ScnEditor -> String -> IO ()
debugWarn e s = debugOut e ("Warning: " ++ s)

debugInfo :: ScnEditor -> String -> IO ()
debugInfo e s = debugOut e ("Info: " ++ s)

debugOut :: ScnEditor -> String -> IO ()
debugOut e s = do
    let bs = BS.pack s
    scnSetReadOnly e False
    scnAppendLine e bs
    scnSetReadOnly e True
    scnShowLastLine e
    return ()

debugPause :: Frame () -> String -> IO ()
debugPause f s = infoDialog f "Debug" s
