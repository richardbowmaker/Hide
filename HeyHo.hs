
-- main IDE for Haskell --
--------------------------

module Main where

-- library imports
import Control.Concurrent 
import Control.Concurrent.STM
import qualified Control.Concurrent.Thread as Thread
import Control.Monad (liftM, when)
import Control.Monad.Loops
import qualified Data.ByteString.Char8 as BS (ByteString, hGetLine, readFile, pack, putStrLn, writeFile)
import qualified Data.ByteString as BS (append)
import Data.List (find, findIndex)
import Data.Word (Word64)
import Graphics.UI.WX
import Graphics.UI.WXCore
import Graphics.UI.WXCore.Events
import Numeric (showHex)
import System.FilePath.Windows (takeFileName)
import System.IO
import System.Process
import System.Process.Common

-- project imports
import qualified Compile as CP
import qualified Constants as CN
import qualified EditMenu as EM
import qualified EditorNotebook as EN
import qualified FileMenu as FM
import qualified Ghci as GH
import qualified Misc as MI
import qualified OutputPane as OT
import qualified Scintilla as SC
import qualified ScintillaConstants as SC
import qualified Session as SS
   
--------------------------------------
-- Main
--------------------------------------   

main = start mainGUI

mainGUI :: IO ()
mainGUI = do 
  
    -- main window
    mf <- frame []    
    set mf [ text := CN.programTitle, size := (Size 1300 800)]  
     
     -- create statusbar field
    sf <- statusField []

    -- AUI manager and child windows
    ss <- setUpMainWindow mf sf
    
    -- set the statusbar and menubar
    set mf [statusBar := [sf]]

    set mf [on closing := onClosing ss]
    
    -- create a timer that updates the display
    t <- timer mf [interval := 100, on command := onTimer ss] 
       
    return ()
   
------------------------------------------------------------    
-- Setup main window, AUI manager its child windows and the menus
------------------------------------------------------------    
   
setUpMainWindow :: Frame () -> StatusField -> IO SS.Session
setUpMainWindow mf sf = do 

    am <- auiManagerCreate mf wxAUI_MGR_DEFAULT
      
    -- add dockable tree
    tree <- MI.createTree mf
    api <- auiPaneInfoCreateDefault
    auiPaneInfoCaption api "Tree Control"
    auiPaneInfoLeft api
    auiPaneInfoCloseButton api True
    
    auiManagerAddPaneByPaneInfo am tree api
    
{-
    -- add dockable grid
    grid <- createGrid mf
    api <- auiPaneInfoCreateDefault
    auiPaneInfoCaption api "Grid Control"
    auiPaneInfoBottom api
    auiPaneInfoCloseButton api True
   
    auiManagerAddPaneByPaneInfo am grid api
-}    
    -- add editor notebook
    enb <- EN.enbCreate mf
   
    api <- auiPaneInfoCreateDefault
    auiPaneInfoCaption api "Editor"
    auiPaneInfoCentre api
    auiPaneInfoCloseButton api True
    auiPaneInfoMaximizeButton api True

    auiManagerAddPaneByPaneInfo am enb api
   
    -- add output pane
    onb <- OT.createOutputWindow mf
    
    api <- auiPaneInfoCreateDefault
    auiPaneInfoCaption api "Output"
    auiPaneInfoBottom api
    auiPaneInfoCloseButton api True

    auiManagerAddPaneByPaneInfo am onb api
    
    -- add floating debug window
    dp <- panel mf [size := (Size 400 400)]
    hwnd <- windowGetHandle dp
    scn <- SC.scnCreateEditor hwnd
    SC.scnConfigureHaskell scn
    SC.scnSetReadOnly scn True
  
    api <- auiPaneInfoCreateDefault
    auiPaneInfoCaption api "Debug"
    auiPaneInfoBottom api
    auiPaneInfoCloseButton api True

    auiManagerAddPaneByPaneInfo am dp api   

    -- update the manager display
    auiManagerUpdate am
    
    -- setup the menus
    menus <- setupMenus mf 

    -- create the session data
    ss <- SS.ssCreate mf am enb menus sf onb scn 
    
    -- setup static menu handlers
    set (SS.ssMenuListGet ss CN.menuFileOpen)  [on command := FM.onFileOpen ss]
    set (SS.ssMenuListGet ss CN.menuFileNew)   [on command := FM.onFileNew  ss]
    set (SS.ssMenuListGet ss CN.menuDebugGhci) [on command := GH.openWindow ss]
    
    set enb [on auiNotebookOnPageCloseEvent   := onTabClose         ss]
    set enb [on auiNotebookOnPageChangedEvent := onTabChanged       ss]
    set onb [on auiNotebookOnPageCloseEvent   := onOutputTabClose   ss]
    set onb [on auiNotebookOnPageChangedEvent := onOutputTabChanged ss]
   
    return (ss)
  
------------------------------------------------------------    
-- Setup menus
------------------------------------------------------------    

setupMenus :: Frame () -> IO SS.SsMenuList
setupMenus mf  = do

    -- file menu  
    menuFile            <- menuPane             [text := "&File"]
    menuFileOpen        <- menuItem menuFile    [text := (CN.menuText' CN.menuFileOpen),        help := (CN.menuHelp' CN.menuFileOpen)]
    menuFileNew         <- menuItem menuFile    [text := (CN.menuText' CN.menuFileNew),         help := (CN.menuHelp' CN.menuFileNew)]
    menuFileClose       <- menuItem menuFile    [text := (CN.menuText' CN.menuFileClose),       help := (CN.menuHelp' CN.menuFileClose)]
    menuFileCloseAll    <- menuItem menuFile    [text := (CN.menuText' CN.menuFileCloseAll),    help := (CN.menuHelp' CN.menuFileCloseAll)]                                             
    menuFileSave        <- menuItem menuFile    [text := (CN.menuText' CN.menuFileSave),        help := (CN.menuHelp' CN.menuFileSave),     enabled := False]
    menuFileSaveAs      <- menuItem menuFile    [text := (CN.menuText' CN.menuFileSaveAs),      help := (CN.menuHelp' CN.menuFileSaveAs)]
    menuFileSaveAll     <- menuItem menuFile    [text := (CN.menuText' CN.menuFileSaveAll),     help := (CN.menuHelp' CN.menuFileSaveAll),  enabled := False]
                                             
    menuAppendSeparator menuFile
                             
    menuQuit  <- menuQuit menuFile [help := "Quit", on command := close mf]

    menuEdit                <- menuPane            [text := "&Edit"]
    menuEditUndo            <- menuItem menuEdit   [text := (CN.menuText' CN.menuEditUndo),             help := (CN.menuHelp' CN.menuEditUndo)]
    menuEditRedo            <- menuItem menuEdit   [text := (CN.menuText' CN.menuEditRedo),             help := (CN.menuHelp' CN.menuEditRedo)]
    menuAppendSeparator menuEdit
    menuEditCut             <- menuItem menuEdit   [text := (CN.menuText' CN.menuEditCut),              help := (CN.menuHelp' CN.menuEditCut)]
    menuEditCopy            <- menuItem menuEdit   [text := (CN.menuText' CN.menuEditCopy),             help := (CN.menuHelp' CN.menuEditCopy)]
    menuEditPaste           <- menuItem menuEdit   [text := (CN.menuText' CN.menuEditPaste),            help := (CN.menuHelp' CN.menuEditPaste)]
    menuEditAll             <- menuItem menuEdit   [text := (CN.menuText' CN.menuEditSelectAll),        help := (CN.menuHelp' CN.menuEditSelectAll)]
    menuEditFind            <- menuItem menuEdit   [text := (CN.menuText' CN.menuEditFind),             help := (CN.menuHelp' CN.menuEditFind)]
    menuEditFindForward     <- menuItem menuEdit   [text := (CN.menuText' CN.menuEditFindForward),      help := (CN.menuHelp' CN.menuEditFindForward)]
    menuEditFindBackward    <- menuItem menuEdit   [text := (CN.menuText' CN.menuEditFindBackward),     help := (CN.menuHelp' CN.menuEditFindBackward)]
    
    menuBuild        <- menuPane            [text := "&Build"]
    menuBuildCompile <- menuItem menuBuild  [text := (CN.menuText' CN.menuBuildCompile),    help := (CN.menuHelp' CN.menuBuildCompile)]
    menuBuildBuild   <- menuItem menuBuild  [text := (CN.menuText' CN.menuBuildBuild),      help := (CN.menuHelp' CN.menuBuildBuild)]
    menuBuildReBuild <- menuItem menuBuild  [text := (CN.menuText' CN.menuBuildRebuild),    help := (CN.menuHelp' CN.menuBuildRebuild)]
    menuBuildClean   <- menuItem menuBuild  [text := (CN.menuText' CN.menuBuildClean),      help := (CN.menuHelp' CN.menuBuildClean)]
    menuBuildGhci    <- menuItem menuBuild  [text := (CN.menuText' CN.menuBuildGhci),       help := (CN.menuHelp' CN.menuBuildGhci)]
          
    menuDebug        <- menuPane            [text := "Debug"]
    menuDebugRun     <- menuItem menuDebug  [text := (CN.menuText' CN.menuDebugRun),    help := (CN.menuHelp' CN.menuDebugRun)]
    menuDebugGhci    <- menuItem menuDebug  [text := (CN.menuText' CN.menuDebugGhci),   help := (CN.menuHelp' CN.menuDebugGhci)]

    menuTest         <- menuPane            [text := "Test"]
    menuTestTest     <- menuItem menuTest   [text := (CN.menuText' CN.menuTestTest),    help := (CN.menuHelp' CN.menuTestTest)]

    -- create Help menu
    menuHelp'        <- menuHelp []
    menuHelpAbout    <- menuAbout menuHelp' [help := (CN.menuHelp' CN.menuHelpAbout), on command := infoDialog mf "About HeyHo" "mmmmm !"]
      
    set mf [ menuBar := [menuFile, menuEdit, menuBuild, menuDebug, menuTest, menuHelp']]

    -- create lookup list of menus for session data   
    ml <- SS.ssMenuListCreate [     (CN.menuFileOpen,            menuFileOpen), 
                                    (CN.menuFileSave,            menuFileSave), 
                                    (CN.menuFileNew,             menuFileNew), 
                                    (CN.menuFileClose,           menuFileClose), 
                                    (CN.menuFileCloseAll,        menuFileCloseAll), 
                                    (CN.menuFileSaveAs,          menuFileSaveAs), 
                                    (CN.menuFileSaveAll,         menuFileSaveAll),
                                    (CN.menuEditUndo,            menuEditUndo),
                                    (CN.menuEditRedo,            menuEditRedo),
                                    (CN.menuEditCut,             menuEditCut),
                                    (CN.menuEditCopy,            menuEditCopy),
                                    (CN.menuEditPaste,           menuEditPaste),
                                    (CN.menuEditSelectAll,       menuEditAll),
                                    (CN.menuEditFind,            menuEditFind),
                                    (CN.menuEditFindForward,     menuEditFindForward),
                                    (CN.menuEditFindBackward,    menuEditFindBackward),
                                    (CN.menuBuildBuild,          menuBuildBuild),
                                    (CN.menuBuildCompile,        menuBuildCompile),
                                    (CN.menuBuildGhci,           menuBuildGhci),
                                    (CN.menuDebugRun,            menuDebugRun),
                                    (CN.menuDebugGhci,           menuDebugGhci),
                                    (CN.menuTestTest,            menuTestTest)]

    
    -- create Toolbar
    tbar   <- toolBar mf []
    _      <- toolMenu tbar menuFileSave    "" "save.png"    []
    _      <- toolMenu tbar menuFileSaveAll "" "saveall.png" []
  
    return (ml)
    
------------------------------------------------------------    
-- File Menu handlers
------------------------------------------------------------    

onClosing :: SS.Session -> IO ()
onClosing ss = do
    GH.closeAll ss
    FM.fileCloseAll ss
    (auiManagerUnInit . SS.ssAuiMgr) ss
    (windowDestroy . SS.ssFrame) ss
    return ()

onTabChanged :: SS.Session -> EventAuiNotebook -> IO ()
onTabChanged ss ev@(AuiNotebookPageChanged _ _) = do   
    mhw <- EN.enbGetSelectedSourceFile ss 
    case mhw of
        Just hw -> do 
            case SS.hwGetEditor hw of
                Just scn -> SC.scnGrabFocus scn
                Nothing  -> return ()
        Nothing -> return ()

onTabClose :: SS.Session -> EventAuiNotebook -> IO ()
onTabClose ss enb = do
    mhw <- EN.enbGetSelectedSourceFile ss 
    case mhw of
        Just hw -> do 
            case SS.hwGetEditor hw of
                Just scn -> do
                    FM.closeEditor ss (SS.hwWindow hw) scn
                    return ()
                Nothing  -> return ()
        Nothing -> return ()
    return ()

{-
got EventAuiNotebook
expected Object (CWxObject (CEvtHandler (CAuiManagerEvent a0)))

auiManagerEventVeto :: AuiManagerEvent  a -> Bool ->  IO ()

data EventAuiNotebook = AuiNotebookAllowDnd { newSel ::  WindowSelection, oldSel ::  WindowSelection }
                      | AuiNotebookBeginDrag  { newSel ::  WindowSelection, oldSel ::  WindowSelection }
                      | AuiNotebookBgDclick  { newSel ::  WindowSelection, oldSel ::  WindowSelection }
                      | AuiNotebookButton  { newSel ::  WindowSelection, oldSel ::  WindowSelection }
                      | AuiNotebookDragDone  { newSel ::  WindowSelection, oldSel ::  WindowSelection }
                      | AuiNotebookDragMotion  { newSel ::  WindowSelection, oldSel ::  WindowSelection }
                      | AuiNotebookEndDrag  { newSel ::  WindowSelection, oldSel ::  WindowSelection }
                      | AuiNotebookPageChanged  { newSel ::  WindowSelection, oldSel ::  WindowSelection }
                      | AuiNotebookPageChanging  { newSel ::  WindowSelection, oldSel ::  WindowSelection }
                      | AuiNotebookPageClose  { newSel ::  WindowSelection, oldSel ::  WindowSelection }
                      | AuiNotebookPageClosed  { newSel ::  WindowSelection, oldSel ::  WindowSelection }
                      | AuiNotebookTabMiddleDown  { newSel ::  WindowSelection, oldSel ::  WindowSelection }
                      | AuiNotebookTabMiddleUp  { newSel ::  WindowSelection, oldSel ::  WindowSelection }
                      | AuiNotebookTabRightDown  { newSel ::  WindowSelection, oldSel ::  WindowSelection }
                      | AuiNotebookTabRightUp  { newSel ::  WindowSelection, oldSel ::  WindowSelection }
                      | AuiNotebookUnknown
                      | AuiTabCtrlPageChanging  { newSel ::  WindowSelection, oldSel ::  WindowSelection }
                      | AuiTabCtrlUnknown
                      deriving (Show, Eq)

eventGetEventObject :: Event a -> IO (WxObject ())
withCurrentEvent :: (Event () -> IO ()) -> IO ()

    withCurrentEvent (\ev -> do
                            x <- (eventGetEventObject (objectCast ev))                          
                            auiManagerEventVeto x  True)
-}


onOutputTabClose :: SS.Session -> EventAuiNotebook -> IO ()
onOutputTabClose ss _ = do
    mhw <- OT.getSelectedGhci ss 
    case mhw of
        Just hw -> GH.closeWindow ss $ SS.hwWindow hw
        Nothing -> return ()

onOutputTabChanged :: SS.Session -> EventAuiNotebook -> IO ()
onOutputTabChanged ss _ = do
    mhw <- OT.getSelectedGhci ss 
    case mhw of
        Just hw -> GH.setFocus $ SS.hwPanelHwnd hw
        Nothing -> return ()

------------------------------------------------------------    
-- Test Menu handlers
------------------------------------------------------------    
 
onTestTest :: SS.Session -> IO ()
onTestTest ss = do 
    return ()

------------------------------------------------------------    
-- Debug menu handlers
------------------------------------------------------------    
 
{-   
onDebugRun :: Session -> IO ()
onDebugRun ss = do
    sf <- enbGetSelectedSourceFile ss
    case (sfFilePath sf) of
        Just fp -> cpDebugRun ss fp 
        Nothing -> warningDialog (ssFrame ss) CN.programTitle "Source file has not been given a name" 
    return ()

onDebugGhci :: Session -> IO ()
onDebugGhci ss = GH.openWindow ss (ghciCallback ss)
-}

------------------------------------------------------------    
-- Timer handler
------------------------------------------------------------    
    
onTimer :: SS.Session -> IO ()
onTimer ss = do

    -- update output pane
    withTChan (SS.ssTOutput ss) (OT.addText ss) 
    
    -- run any scheduled functions
    withTChan (SS.ssCFunc ss) (\f -> f)
   
    where

        -- calls supplied function whilst there is still data in the channel
        withTChan :: TChan a -> (a -> IO ()) -> IO ()
        withTChan chan f =  
            whileM_ (liftM not $ atomically $ isEmptyTChan chan)
                (atomically (tryReadTChan chan) >>= maybe (return ()) (\a -> f a))             
            

