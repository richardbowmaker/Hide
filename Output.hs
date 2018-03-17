
module Output
( 
    clear,
    addText,
    addLine,
    withEditor
) where 
    

import qualified Data.ByteString.Char8 as BS (ByteString, writeFile)


import qualified Scintilla as SC
import qualified ScintillaConstants as SC
import qualified Session as SS


clear' :: SS.Session -> SC.Editor -> IO ()
clear' ss scn = do
    SC.setReadOnly scn False
    SC.clearAll scn
    SC.setReadOnly scn True

addText' :: SS.Session -> BS.ByteString -> SC.Editor -> IO ()
addText' ss bs scn = do
    SC.setReadOnly scn False
    SC.appendText scn bs
    SC.setReadOnly scn True
    SC.showLastLine scn
    
addLine' :: SS.Session -> BS.ByteString -> SC.Editor -> IO ()
addLine' ss bs scn = do
    SC.setReadOnly scn False
    SC.appendLine scn bs
    SC.setReadOnly scn True
    SC.showLastLine scn

clear :: SS.Session -> IO ()
clear ss = withEditor ss $ clear' ss 

addText :: SS.Session -> BS.ByteString -> IO ()
addText ss bs = withEditor ss $ addText' ss bs 

addLine :: SS.Session -> BS.ByteString -> IO ()
addLine ss bs = withEditor ss $ addLine' ss bs 

withEditor :: SS.Session -> (SC.Editor -> IO ()) -> IO ()
withEditor ss f = do    
    mhw <- SS.ssOutput ss
    case mhw of
        Just hw -> do
            case SS.hwGetEditor hw of
                Just scn -> f scn
                Nothing  -> return () -- shouldn't get here
        Nothing -> return ()