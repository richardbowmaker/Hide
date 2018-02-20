module EditorNotebook
( 
    enbCreate,
    enbAddNewFile,
    enbGetSelectedTabHwnd,
    enbGetSelectedSourceFile,
    enbSelectedSourceFileIsClean,
    enbSelectTab,
    enbCloseTab,
    enbGetTabIndex,
    enbSetTabText,
    enbGetTabCount
) where 
    
import Control.Concurrent 
import Control.Concurrent.STM
import Data.List (find, findIndex)

import Data.Word (Word64)
import Foreign.C.String (CString, withCString)
import Graphics.Win32.GDI.Types (HWND)
import Graphics.UI.WX
import Graphics.UI.WXCore
import System.FilePath.Windows (takeFileName)
import System.IO


-- project imports

import Misc
import Scintilla
import ScintillaConstants
import Session

------------------------------------------------------------    
-- Create the source file editor notebook
------------------------------------------------------------    

enbCreate :: Frame mf -> IO (AuiNotebook ())
enbCreate mf = do

    -- create the notebook
    nb <- auiNotebookCreate mf idAny (Point 0 0) (Size 0 0) (wxCLIP_CHILDREN + wxAUI_NB_TOP + wxAUI_NB_CLOSE_ON_ACTIVE_TAB)
    return (nb)
  
enbAddNewFile :: Session -> (SCNotification -> IO ()) -> IO (SourceFile)  
enbAddNewFile ss callback = do
    
    let nb = ssEditors ss

    -- create panel with scintilla editor inside
    p <- panel nb []
    scn <- windowGetHandle p >>= scnCreateEditor
    scnConfigureHaskell scn

    -- add panel to notebook
    auiNotebookAddPage nb p "..." False 0
    ta <- auiSimpleTabArtCreate
    auiNotebookSetArtProvider nb ta

    sf <- sfCreate p scn Nothing

    -- enable events
    scn' <- scnSetEventHandler scn callback
    scnEnableEvents scn'

    -- update mutable project
    prUpdate ss (\pr -> prSetFiles pr (sf:(prFiles pr)))
    
    scnSetSavePoint scn'
    
    return (sf)
    
------------------------------------------------------------    
-- Editor notebook helpers
------------------------------------------------------------    
    
-- returns the HWND of the child panel of the currently selected notebook page
enbGetSelectedTabHwnd :: Session -> IO Word64
enbGetSelectedTabHwnd ss = do
    let nb = ssEditors ss
    hp <- auiNotebookGetSelection nb >>= auiNotebookGetPage nb >>= windowGetHandle
    return (ptrToWord64 hp)
 
-- returns the source file for the currently selected tab
enbGetSelectedSourceFile :: Session -> IO SourceFile
enbGetSelectedSourceFile ss = do  
    fs <- ssReadSourceFiles ss
    hp <- enbGetSelectedTabHwnd ss
   
    case (find (\sf -> sfMatchesHwnd sf hp) fs) of
        Just sf -> 
            return (sf)
        Nothing -> do 
            -- should not occur, like this to simplfy calling code
            ssDebugError ss "enbGetSelectedSourceFile no source file for current tab"
            return (head fs) 
                  
-- returns true if the source file of the currently selected tab is clean 
enbSelectedSourceFileIsClean :: Session -> IO Bool
enbSelectedSourceFileIsClean ss = do
    sf <- enbGetSelectedSourceFile ss
    ic <- scnIsClean $ sfEditor sf
    return (ic)
    
enbSelectTab :: Session -> SourceFile -> IO (Bool)
enbSelectTab ss sf = do
    let nb = ssEditors ss
    mix <- enbGetTabIndex ss sf
    case mix of
        Just ix -> do
            auiNotebookSetSelection nb ix
            return (True)            
        Nothing -> return (False)

enbCloseTab :: Session -> SourceFile -> IO ()
enbCloseTab ss sf = do
    let nb = ssEditors ss
    mix <- enbGetTabIndex ss sf
    case (mix) of
        Just ix -> do
            auiNotebookSetSelection nb ix
            auiNotebookRemovePage nb ix
            return ()
        Nothing -> do
            ssDebugError ss "enbCloseTab, source file not in tabs"
            return ()

enbGetTabIndex :: Session -> SourceFile -> IO (Maybe Int)
enbGetTabIndex ss sf = do

    let nb = ssEditors ss
    pc <- auiNotebookGetPageCount nb

    -- get list of window handles as ints
    hs <- mapM (getHwnd nb) [0..(pc-1)]

    -- find tab with hwnd that matches the source file
    return (findIndex (\h -> sfMatchesHwnd sf h) hs)
    
    where getHwnd nb i = do
            w <- auiNotebookGetPage nb i
            h <- windowGetHandle w
            return (ptrToWord64 h)
 
enbSetTabText :: Session -> SourceFile -> IO ()
enbSetTabText ss sf = do
    mix <- enbGetTabIndex ss sf
    case mix of
        Just ix -> do
            case (sfFilePath sf) of
                Just fp -> do
                    auiNotebookSetPageText (ssEditors ss) ix $ takeFileName fp
                    return ()
                Nothing -> return ()                   
        Nothing -> return ()
    return ()
 
enbGetTabCount :: Session -> IO Int
enbGetTabCount ss = do
    pc <- auiNotebookGetPageCount $ ssEditors ss
    return (pc)



