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
    ta <- auiSimpleTabArtCreate
    auiNotebookSetArtProvider nb ta
    return nb
  
enbAddNewFile :: Session -> (TextWindow -> SCNotification -> IO ()) -> IO (SourceFile)  
enbAddNewFile ss callback = do
    
    let nb = ssEditors ss

    -- create panel with scintilla editor inside
    p <- panel nb []
    scn <- windowGetHandle p >>= scnCreateEditor
    scnConfigureHaskell scn

    -- add panel to notebook
    auiNotebookAddPage nb p "..." False 0

    sf <- sfCreate p scn Nothing Nothing


    let tw = newtw scn p hwnd (SC.scnGetHwnd scn) fp
    SS.twUpdate ss (\tws -> SS.twCreate (tw : (SS.txWindows tws)))

    -- enable events
    scn' <- scnSetEventHandler scn callback tw
    scnEnableEvents scn'

    -- update mutable project
    prUpdate ss (\pr -> prSetFiles pr (sf:(prFiles pr)))
    
    scnSetSavePoint scn'
    
    return (sf)
    
    where   newtw scn panel hwndp hwnd fp = (SS.createTextWindow
                                (SS.createSourceWindowType scn)
                                panel
                                hwndp
                                hwnd
                                [   
                                    (SS.createMenuFunction CN.menuEditUndo          (SC.scnUndo scn)                (SC.scnCanUndo scn)),
                                    (SS.createMenuFunction CN.menuEditRedo          (SC.scnRedo scn)                (SC.scnCanRedo scn)),
                                    (SS.createMenuFunction CN.menuEditCut           (SC.scnCut scn)                 (liftM not $ SC.scnSelectionIsEmpty scn)),
                                    (SS.createMenuFunction CN.menuEditCopy          (SC.scnCopy scn)                (liftM not $ SC.scnSelectionIsEmpty scn)),
                                    (SS.createMenuFunction CN.menuEditPaste         (SC.scnPaste scn)               (SC.scnCanPaste scn)),
                                    (SS.createMenuFunction CN.menuEditSelectAll     (SC.scnSelectAll scn)           (return True)),
                                    (SS.createMenuFunction CN.menuEditFind          (EM.editFind ss scn)            (return True)),
                                    (SS.createMenuFunction CN.menuEditFindForward   (EM.editFindForward ss scn)     (return True)),
                                    (SS.createMenuFunction CN.menuEditFindBackward  (EM.editFindBackward ss scn)    (return True))
                                ]
                                (SC.scnGetFocus scn)
                                Nothing)

------------------------------------------------------------    
-- Editor notebook helpers
------------------------------------------------------------    
    
-- returns the HWND of the child panel of the currently selected notebook page
enbGetSelectedTabHwnd :: Session -> IO HWND
enbGetSelectedTabHwnd ss = do
    let nb = ssEditors ss
    hp <- auiNotebookGetSelection nb >>= auiNotebookGetPage nb >>= windowGetHandle
    return hp
 
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
    
    where getHwnd nb i = auiNotebookGetPage nb i >>= windowGetHandle
 
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



