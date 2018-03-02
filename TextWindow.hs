module TextWindow 
(
)
where

import Graphics.UI.WX
import Graphics.UI.WXCore
import Control.Concurrent (myThreadId, ThreadId)
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Data.ByteString.Internal (ByteString)
import Data.String.Combinators (punctuate)
import Data.List (find)
import qualified Data.ByteString.Char8 as BS (pack)
import Data.Char (toLower)
import Data.Word (Word64)
import Graphics.Win32.GDI.Types (HWND)
import Numeric (showHex)

import Debug
import Misc
import Scintilla
import Ghci



data TextWindowType = Scintilla ScnEditor | Ghci GhciPanel | Debug ScnEditor | Output ScnEditor

-- | Source File Data
data TextWindow 
    = TextWindow {  twType      :: TextWindowType,
                    twPanel     :: Panel (),            -- ^ The parent panel of text window
                    twPanelHwnd :: HWND,                -- ^ HWND of panel
                    twCut       :: IO (),               -- ^ Function to perform cut operation
                    twCopy      :: IO (),
                    twPaste     :: IO (),
                    twCanCut    :: IO Bool,             -- ^ Whether the cut is currently valid, i.e. text is selected
                    twCanCopy   :: IO Bool,              
                    twCanPaste  :: IO Bool,
                    twFilePath  :: Maybe String }       -- ^ File name associated with window
