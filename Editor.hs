module Session 
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

-- | Source File Data
data SourceFile 
    = SourceFile {  sfPanel     :: Panel (),            -- ^ The panel added to the AuiNotebookManager
                    sfPanelHwnd :: HWND,                -- ^ HWND of panel
                    sfEditor    :: ScnEditor,           -- ^ The Scintilla editor, child window of panel
                    sfFilePath  :: Maybe String,        -- ^ Source file path, Nothing = file name not set yet
                    sfGhci      :: Maybe GhciPanel }    -- ^ Parent panel of GHCI window in the output pane