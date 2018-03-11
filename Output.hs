
module Output
( 
    clear,
    addText,
    addLine,
    withScnEditor
) where 
    

import qualified Data.ByteString.Char8 as BS (ByteString, writeFile)

import qualified Scintilla as SC
import qualified ScintillaConstants as SC
import qualified Session as SS


clear' :: SS.Session -> SC.ScnEditor -> IO ()
clear' ss scn = do
    SC.scnSetReadOnly scn False
    SC.scnClearAll scn
    SC.scnSetReadOnly scn True

addText' :: SS.Session -> BS.ByteString -> SC.ScnEditor -> IO ()
addText' ss bs scn = do
    SC.scnSetReadOnly scn False
    SC.scnAppendText scn bs
    SC.scnSetReadOnly scn True
    SC.scnShowLastLine scn
    
addLine' :: SS.Session -> BS.ByteString -> SC.ScnEditor -> IO ()
addLine' ss bs scn = do
    SC.scnSetReadOnly scn False
    SC.scnAppendLine scn bs
    SC.scnSetReadOnly scn True
    SC.scnShowLastLine scn

clear :: SS.Session -> IO ()
clear ss = withScnEditor ss $ clear' ss 

addText :: SS.Session -> BS.ByteString -> IO ()
addText ss bs = withScnEditor ss $ addText' ss bs 

addLine :: SS.Session -> BS.ByteString -> IO ()
addLine ss bs = withScnEditor ss $ addLine' ss bs 

withScnEditor :: SS.Session -> (SC.ScnEditor -> IO ()) -> IO ()
withScnEditor ss f = do    
    mhw <- SS.ssOutput ss
    case mhw of
        Just hw -> do
            case SS.hwGetEditor hw of
                Just scn -> f scn
                Nothing  -> return () -- shouldn't get here
        Nothing -> return ()