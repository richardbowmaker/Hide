
module OutputPane
( 
    createOutputPane,
    gotoCompileError,
 
) where 
    

import Control.Concurrent.STM (atomically, readTVar)
import Data.List (find, findIndex)
import Data.Word (Word64)
import Graphics.UI.WX
import Graphics.UI.WXCore

-- project imports

import EditorNotebook
import Scintilla
import ScintillaConstants
import Session

------------------------------------------------------------    
-- Output pane
------------------------------------------------------------    

createOutputPane :: Frame () -> IO (AuiNotebook (), ScnEditor)
createOutputPane f = do

    nb <- auiNotebookCreate f idAny (Point 0 0) (Size 0 0) (wxCLIP_CHILDREN + wxAUI_NB_TOP + wxAUI_NB_CLOSE_ON_ACTIVE_TAB)
    set nb [] 
    p <- panel nb []
    hwnd <- windowGetHandle p
    e <- scnCreateEditor hwnd
    auiNotebookAddPage nb p "Output" False 0
    ta <- auiSimpleTabArtCreate
    auiNotebookSetArtProvider nb ta
    
    -- configure editor
    scnSetLexer e (fromIntegral sCLEX_CONTAINER :: Int)
    scnSetAStyle e (fromIntegral sTYLE_DEFAULT :: Word64) scnBlack scnWhite 9 "Courier New"
    scnStyleClearAll e
    scnSetAStyle e (fromIntegral sCE_H_DEFAULT :: Word64) scnBlack scnWhite 9 "Courier New"
    scnSetReadOnly e True

    scnSetSelectionMode e sC_SEL_LINES
 
    return (nb, e)

-- jump to source file error location
gotoCompileError :: Session -> Int -> (String -> IO ()) -> IO ()
gotoCompileError ss line fileOpen = do

    -- get compiler errors and lookup the error
    ces <- atomically $ readTVar $ ssCompilerReport ss
    let mce = find (\ce -> (ceErrLine ce) <= line ) (reverse ces)

    case mce of
        Nothing -> return ()
        Just ce -> do

            -- goto source file from list of open files
            msf <- prGetSourceFile ss $ ceFilePath ce 
            case msf of
           
                Just sf -> do
                    b <- enbSelectTab ss sf
                    if b then gotoPos (sfEditor sf) ((ceSrcLine ce)-1) ((ceSrcCol ce)-1)
                    else return ()

                Nothing -> do
                    -- source file not open
                    fileOpen $ ceFilePath ce
                    msf <- sfGetSourceFile ss $ ceFilePath ce
                    case msf of
                        Just sf -> gotoPos (sfEditor sf) ((ceSrcLine ce)-1) ((ceSrcCol ce)-1)
                        Nothing -> return ()
     
    where gotoPos e l c = do
            scnGotoLineCol e l c
            scnGrabFocus e
            scnSelectWord e
            


      
