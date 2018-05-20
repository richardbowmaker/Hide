module EditorNotebook
( 
    enbCreate,
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

import qualified Session as SS

------------------------------------------------------------    
-- Create the source file editor notebook
------------------------------------------------------------    

enbCreate :: Frame mf -> IO (AuiNotebook ())
enbCreate mf = do

    -- create the notebook
    nb <- auiNotebookCreate mf idAny (Point 0 0) (Size 0 0) (wxCLIP_CHILDREN + wxAUI_NB_TOP + wxAUI_NB_CLOSE_ON_ACTIVE_TAB)
    ta <- auiSimpleTabArtCreate
    auiNotebookSetArtProvider nb ta
    return nb
  
------------------------------------------------------------    
-- Editor notebook helpers
------------------------------------------------------------    
    
-- returns the HWND of the child panel of the currently selected notebook page
enbGetSelectedTabHwnd :: SS.Session -> IO (Maybe HWND)
enbGetSelectedTabHwnd ss = do
    n <- enbGetTabCount ss
    if n > 0 then do
        hp <- auiNotebookGetSelection nb >>= auiNotebookGetPage nb >>= windowGetHandle
        return (Just hp)
    else return Nothing

    where   nb = SS.ssEditors ss
 
-- returns the source file for the currently selected tab
enbGetSelectedSourceFile :: SS.Session -> IO (Maybe SS.TextWindow)
enbGetSelectedSourceFile ss = do  
    tws <- SS.twGetWindows ss
    mhwnd <- enbGetSelectedTabHwnd ss
    case mhwnd of 
        Just hwnd -> return $ find (\tw -> (SS.twIsSourceFile tw) && (SS.twMatchesHwnd tw hwnd)) tws
        Nothing   -> return Nothing

-- returns true if the source file of the currently selected tab is clean 
enbSelectedSourceFileIsClean :: SS.Session -> IO Bool
enbSelectedSourceFileIsClean ss = 
    enbGetSelectedSourceFile ss >>= maybe (return True) (\tw -> SS.twIsClean tw) 
 
enbSelectTab :: SS.Session -> SS.TextWindow -> IO (Bool)
enbSelectTab ss tw = do
    let nb = SS.ssEditors ss
    mix <- enbGetTabIndex ss tw
    case mix of
        Just ix -> do
            auiNotebookSetSelection nb ix
            return True            
        Nothing -> return False

enbCloseTab :: SS.Session -> SS.TextWindow -> IO ()
enbCloseTab ss tw = do
    let nb = SS.ssEditors ss
    mix <- enbGetTabIndex ss tw
    case mix of
        Just ix -> do
            auiNotebookSetSelection nb ix
            auiNotebookRemovePage nb ix
            return ()
        Nothing -> do
            SS.ssDebugError ss "enbCloseTab, source file not in tabs"
            return ()

enbGetTabIndex :: SS.Session -> SS.TextWindow -> IO (Maybe Int)
enbGetTabIndex ss tw = do

    let nb = SS.ssEditors ss
    pc <- auiNotebookGetPageCount nb

    -- get list of window handles as ints
    hs <- mapM (getHwnd nb) [0..(pc-1)]

    -- find tab with hwnd that matches the source file
    return (findIndex (\h -> SS.twMatchesHwnd tw h) hs)
    
    where getHwnd nb i = auiNotebookGetPage nb i >>= windowGetHandle
 
enbSetTabText :: SS.Session -> SS.TextWindow -> IO ()
enbSetTabText ss tw = do
    mix <- enbGetTabIndex ss tw
    case mix of
        Just ix -> do 
            case SS.twFilePath tw of
                Just fp -> do
                    auiNotebookSetPageText (SS.ssEditors ss) ix $ takeFileName fp
                    return ()
                Nothing -> return ()                   
        Nothing -> return ()
    return ()
 
enbGetTabCount :: SS.Session -> IO Int
enbGetTabCount ss = do
    pc <- auiNotebookGetPageCount $ SS.ssEditors ss
    return (pc)



