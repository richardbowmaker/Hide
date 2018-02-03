
module Debug
(
    debugOut
) where

import qualified Data.ByteString.Char8 as BS (pack)
import Foreign.Ptr (FunPtr, Ptr, minusPtr, nullPtr)
import Numeric (showHex)
import Graphics.UI.WXCore
import Data.Word (Word64)

import Scintilla
import Session

debugOut :: Session -> String -> IO ()
debugOut ss s = do
    let bs = BS.pack s
    let e = ssDebug ss
    scnSetReadOnly e False
    scnAppendLine e bs
    scnSetReadOnly e True
    return ()
{-
debugOut :: Session -> String -> [String] -> IO ()
debugOut ss str strs = do
    let bs = BS.pack str
    let e = ssDebug ss
    scnSetReadOnly e False
    scnAppendLine e bs
    scnSetReadOnly e True
    return ()
 -}   