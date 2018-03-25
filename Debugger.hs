
module Debugger
( 
    onDebugDebug

) where 
 
-- library imports
 
import Control.Concurrent 
import Control.Concurrent.STM
import Control.Exception
import Control.Monad (mapM_, liftM, liftM2) 
import Data.Bits ((.|.))
import qualified Data.ByteString as BS (init, replicate)
import qualified Data.ByteString.Char8 as BS (unpack, take, writeFile)
import qualified Data.ByteString.Internal as BS (ByteString)
import qualified Data.ByteString.Unsafe as BS (unsafeUseAsCString)
import Data.Int (Int32)
import Data.List (find, findIndex)
import Data.Maybe (isJust)
import Data.Word (Word64)
import Foreign.C.String (CString, withCString, peekCString)
import Foreign.Ptr (FunPtr, Ptr, minusPtr, nullPtr)
import Graphics.UI.WX
import Graphics.UI.WXCore
import Graphics.Win32.GDI.Types (HWND)
import System.Directory
import System.FilePath.Windows (takeFileName, takeDirectory)
import System.IO
import System.Win32.Types (nullHANDLE)
import qualified System.FilePath.Windows as Win (dropExtension, takeBaseName, takeDirectory)

-- project imports

import qualified Constants as CN
import qualified Ghci as GH
import qualified Misc as MI
import qualified Scintilla as SC
import qualified ScintillaProxyImports as SI
import qualified Session as SS

onDebugDebug :: SS.Session -> SS.TextWindow -> IO ()
onDebugDebug ss tw = do
    mhwnd <- GH.openWindow ss
    case mhwnd of
        Just hwnd -> do
            mfp <- SS.twFilePath tw
            case mfp of
                Just fp -> do
                    --  delete object file, to force GHCi to run in interpretative mode
                    result <- try (removeFile $ (Win.dropExtension fp) ++ ".o")  :: IO (Either IOException ())
                    GH.sendCommand hwnd $ ":load " ++ fp
                    return ()
                Nothing -> return ()
        Nothing -> return ()


