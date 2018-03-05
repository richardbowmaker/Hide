
-- main IDE for Haskell --
--------------------------

module Main where

-- library imports
import Control.Concurrent 
import Control.Concurrent.STM
import qualified Control.Concurrent.Thread as Thread
import Control.Monad (liftM, when)
import Control.Monad.Loops
import Data.Bits ((.&.))
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
import Compile
import qualified Constants as CN
import qualified EditMenu as EM
import EditorNotebook
import qualified FileMenu as FM
import qualified Ghci as GH
import Misc
import qualified OutputPane as OP
import Scintilla
import ScintillaConstants
import Session
   
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
   
setUpMainWindow :: Frame () -> StatusField -> IO (Session)    
setUpMainWindow mf sf = do 

    am <- auiManagerCreate mf wxAUI_MGR_DEFAULT
      
    -- add dockable tree
    tree <- createTree mf
    api <- auiPaneInfoCreateDefault
    auiPaneInfoCaption api "Tree Control"
    auiPaneInfoLeft api
    auiPaneInfoCloseButton api True
    
    auiManagerAddPaneByPaneInfo am tree api
    
    -- add dockable grid
    grid <- createGrid mf
    api <- auiPaneInfoCreateDefault
    auiPaneInfoCaption api "Grid Control"
    auiPaneInfoBottom api
    auiPaneInfoCloseButton api True
    
    auiManagerAddPaneByPaneInfo am grid api
    
    -- add editor notebook
    enb <- enbCreate mf
   
    api <- auiPaneInfoCreateDefault
    auiPaneInfoCaption api "Editor"
    auiPaneInfoCentre api
    auiPaneInfoCloseButton api True
    auiPaneInfoMaximizeButton api True

    auiManagerAddPaneByPaneInfo am enb api
   
    -- add output pane
    (onb, oe) <- OP.createOutputPane mf
    
    api <- auiPaneInfoCreateDefault
    auiPaneInfoCaption api "Output"
    auiPaneInfoBottom api
    auiPaneInfoCloseButton api True

    auiManagerAddPaneByPaneInfo am onb api
    
    -- add floating debug window
    dp <- panel mf [size := (Size 400 400)]
    hwnd <- windowGetHandle dp
    scn <- scnCreateEditor hwnd
    scnConfigureHaskell scn
    scnSetReadOnly scn True
  
    api <- auiPaneInfoCreateDefault
    auiPaneInfoCaption api "Debug"
    auiPaneInfoFloat api
    auiPaneInfoCloseButton api True

    auiManagerAddPaneByPaneInfo am dp api   

    -- update the manager display
    auiManagerUpdate am
    
    -- setup the menus
    menus <- setupMenus mf 

    -- create the session data
    ss <- ssCreate mf am enb (prCreate []) menus sf onb oe scn 
    
    -- add blank file to editor
    FM.newFile ss (scnCallback ss)
    
    -- setup menu handlers
    set (ssMenuListGet ss CN.menuFileOpen)           [on command := onFileOpen           ss]
    set (ssMenuListGet ss CN.menuFileNew)            [on command := onFileNew            ss]
    set (ssMenuListGet ss CN.menuFileSave)           [on command := onFileSave           ss]
    set (ssMenuListGet ss CN.menuFileSaveAs)         [on command := onFileSaveAs         ss]
    set (ssMenuListGet ss CN.menuFileSaveAll)        [on command := onFileSaveAll        ss]
    set (ssMenuListGet ss CN.menuFileClose)          [on command := onFileClose          ss]
    set (ssMenuListGet ss CN.menuFileCloseAll)       [on command := onFileCloseAll       ss]
    set (ssMenuListGet ss CN.menuBuildBuild)         [on command := onBuildBuild         ss]
    set (ssMenuListGet ss CN.menuBuildCompile)       [on command := onBuildCompile       ss]
    set (ssMenuListGet ss CN.menuBuildGhci)          [on command := onBuildGhci          ss]
    set (ssMenuListGet ss CN.menuDebugRun)           [on command := onDebugRun           ss]
    set (ssMenuListGet ss CN.menuDebugGhci)          [on command := onDebugGhci          ss]
    set (ssMenuListGet ss CN.menuTestTest)           [on command := onTestTest           ss]
     
    set enb [on auiNotebookOnPageCloseEvent   := onTabClose   ss]
    set enb [on auiNotebookOnPageChangedEvent := onTabChanged ss]

    set onb [on auiNotebookOnPageCloseEvent   := onOutputTabClose ss]

   -- enable events for output pane, dbl click = goto error
--    scnSetEventHandler oe $ scnCallback ss
--    scnEnableEvents oe
   
    return (ss)
  
------------------------------------------------------------    
-- Setup menus
------------------------------------------------------------    

setupMenus :: Frame () -> IO (SsMenuList)
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
    ml <- ssMenuListCreate [    (CN.menuFileOpen,            menuFileOpen), 
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

onClosing :: Session -> IO ()
onClosing ss = do
    FM.fileCloseAll ss
    (auiManagerUnInit . ssAuiMgr) ss
    (windowDestroy . ssFrame) ss
    return ()

onTabChanged :: Session -> EventAuiNotebook -> IO ()
onTabChanged ss ev@(AuiNotebookPageChanged _ _) = do   
    set (ssMenuListGet ss CN.menuBuildCompile) [text := (CN.menuText' CN.menuBuildCompile)]        
    set (ssMenuListGet ss CN.menuBuildGhci)    [text := (CN.menuText' CN.menuBuildGhci)] 
    c <- enbGetTabCount ss
    if c > 0 then do
        sf <- enbGetSelectedSourceFile ss 
        (scnGrabFocus . sfEditor) sf
        FM.updateSaveMenus ss
        FM.updateStatus ss
        case (sfFilePath sf) of
            Just fp -> do
                set (ssMenuListGet ss CN.menuBuildCompile) 
                    [text := ((CN.menuTitle' CN.menuBuildCompile) ++ (takeFileName fp) ++ (CN.menuKey' CN.menuBuildCompile))]        
                set (ssMenuListGet ss CN.menuBuildGhci)    
                    [text := ((CN.menuTitle' CN.menuBuildGhci) ++ (takeFileName fp) ++ (CN.menuKey' CN.menuBuildGhci) )] 
            Nothing -> return ()
    else return ()

onTabClose :: Session -> EventAuiNotebook -> IO ()
onTabClose ss enb = do
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
    enbGetSelectedSourceFile ss >>= FM.closeEditor ss    
    FM.updateSaveMenus ss      
    return ()

onOutputTabClose :: Session -> EventAuiNotebook -> IO ()
onOutputTabClose ss _ = GH.closeWindow ss
  
onFileClose :: Session -> IO ()
onFileClose ss = do
    enbGetSelectedSourceFile ss >>= FM.fileClose ss 
    FM.updateSaveMenus ss    
    return ()
  
onFileCloseAll :: Session -> IO ()
onFileCloseAll = FM.fileCloseAll

onFileSave :: Session -> IO ()
onFileSave ss = do
    enbGetSelectedSourceFile ss >>= FM.fileSave ss 
    FM.updateSaveMenus ss    
    return ()

onFileSaveAs :: Session -> IO ()
onFileSaveAs ss = do
    enbGetSelectedSourceFile ss >>= FM.fileSaveAs ss
    FM.updateSaveMenus ss    
    return ()
    
onFileSaveAll :: Session -> IO ()    
onFileSaveAll ss = do   
    FM.fileSaveAll ss
    FM.updateSaveMenus ss    
    return ()
   
-- File Open
onFileOpen :: Session -> IO ()
onFileOpen ss = do
    let mf = ssFrame ss                     -- wxFD_OPEN wxFD_FILE_MUST_EXIST
    fd <- fileDialogCreate mf "Open file" "." "" "*.hs" (Point 100 100) 0x11
    ans <- dialogShowModal fd
    if ans == wxID_OK
    then do
        fp <- fileDialogGetPath fd 
        FM.fileOpen ss (scnCallback ss) fp
        FM.updateSaveMenus ss    
        set (ssMenuListGet ss CN.menuBuildCompile) 
                [text := ((CN.menuTitle' CN.menuBuildCompile) ++ (takeFileName fp) ++ (CN.menuKey' CN.menuBuildCompile))]        
        set (ssMenuListGet ss CN.menuBuildGhci)    
                [text := ((CN.menuTitle' CN.menuBuildGhci) ++ (takeFileName fp) ++ (CN.menuKey' CN.menuBuildGhci) )] 
        return ()
    else
        return ()

onFileNew :: Session -> IO ()
onFileNew ss = do
    FM.newFile ss (scnCallback ss) >>= enbSelectTab ss 
    FM.updateSaveMenus ss      
    return ()
 
------------------------------------------------------------    
-- Edit Menu handlers
------------------------------------------------------------    
 
ifEditorHasFocus :: Session -> (SourceFile -> IO ()) -> IO ()
ifEditorHasFocus ss action = do
    sf <- enbGetSelectedSourceFile ss
    f <- (scnGetFocus . sfEditor ) sf
    if f then action sf else return ()
 
onTestTest :: Session -> IO ()
onTestTest ss = do 
    return ()

------------------------------------------------------------    
-- Build Menu handlers
------------------------------------------------------------    
    
onBuildBuild :: Session -> IO ()
onBuildBuild ss = do

    set (ssMenuListGet ss CN.menuBuildBuild)   [enabled := False]        
    set (ssMenuListGet ss CN.menuBuildCompile) [enabled := False]
    set (ssMenuListGet ss CN.menuBuildGhci)    [enabled := False]
    set (ssMenuListGet ss CN.menuDebugRun)     [enabled := False]

    -- save file first
    sf <- enbGetSelectedSourceFile ss
    ans <- FM.fileSave ss sf 
    if ans then do
        -- get again in case filename changed
        sf <- enbGetSelectedSourceFile ss
        case (sfFilePath sf) of
            Just fp -> cpBuildProject ss fp (Just $ compileComplete ss)
            Nothing -> return () 
    else return ()
    
onBuildCompile :: Session -> IO ()
onBuildCompile ss = do

    set (ssMenuListGet ss CN.menuBuildBuild)   [enabled := False]        
    set (ssMenuListGet ss CN.menuBuildCompile) [enabled := False]
    set (ssMenuListGet ss CN.menuBuildGhci)    [enabled := False]

    -- save file first
    sf <- enbGetSelectedSourceFile ss
    ans <- FM.fileSave ss sf 
    if ans then do
        -- get again in case filename changed
        sf <- enbGetSelectedSourceFile ss
        case (sfFilePath sf) of
            Just fp -> cpCompileFile ss fp (Just $ compileComplete ss)
            Nothing -> return () 
    else return ()
               
compileComplete :: Session -> IO ()
compileComplete ss = do
    set (ssMenuListGet ss CN.menuBuildBuild)   [enabled := True]        
    set (ssMenuListGet ss CN.menuBuildCompile) [enabled := True]
    set (ssMenuListGet ss CN.menuBuildGhci)    [enabled := True]
    set (ssMenuListGet ss CN.menuDebugRun)     [enabled := True]
    otAddText ss $ BS.pack "Compile complete\n"
    return ()

onBuildGhci :: Session -> IO ()
onBuildGhci ss = do

    set (ssMenuListGet ss CN.menuBuildBuild)   [enabled := False]        
    set (ssMenuListGet ss CN.menuBuildCompile) [enabled := False]
    set (ssMenuListGet ss CN.menuBuildGhci)    [enabled := False]

    -- save file first
    sf <- enbGetSelectedSourceFile ss
    ans <- FM.fileSave ss sf 
    if ans then do
        -- get again in case filename changed
        sf <- enbGetSelectedSourceFile ss
        case (sfFilePath sf) of
            Just fp -> cpCompileFile ss fp (Just $ ghciComplete ss sf)
            Nothing -> return () 
    else return ()

ghciComplete :: Session -> SourceFile -> IO ()
ghciComplete ss sf = do
    set (ssMenuListGet ss CN.menuBuildBuild)   [enabled := True]        
    set (ssMenuListGet ss CN.menuBuildCompile) [enabled := True]
    set (ssMenuListGet ss CN.menuBuildGhci)    [enabled := True]
    otAddText ss $ BS.pack "Compile complete\n"

    ces <- atomically $ readTVar $ ssCompilerReport ss
    case ces of
        [] -> GH.openWindowFile ss sf (ghciCallback ss)
        _  -> do
            ans <- proceedDialog (ssFrame ss) CN.programTitle "There were compilation errors, continue ?"
            case ans of
                True -> GH.openWindowFile ss sf (ghciCallback ss)
                False -> return ()


ghciCallback :: Session -> TextWindow -> Int -> IO ()
ghciCallback ss tw n = EM.updateEditMenus ss tw

------------------------------------------------------------    
-- Debug menu handlers
------------------------------------------------------------    
    
onDebugRun :: Session -> IO ()
onDebugRun ss = do
    sf <- enbGetSelectedSourceFile ss
    case (sfFilePath sf) of
        Just fp -> cpDebugRun ss fp 
        Nothing -> warningDialog (ssFrame ss) CN.programTitle "Source file has not been given a name" 
    return ()

onDebugGhci :: Session -> IO ()
onDebugGhci ss = GH.openWindow ss (ghciCallback ss)

------------------------------------------------------------    
-- Timer handler
------------------------------------------------------------    
    
onTimer :: Session -> IO ()
onTimer ss = do

    -- update output pane
    withTChan (ssTOutput ss) (otAddText ss) 
    
    -- run any scheduled functions
    withTChan (ssCFunc ss) (\f -> f)
   
    where

        -- calls supplied function whilst there is still data in the channel
        withTChan :: TChan a -> (a -> IO ()) -> IO ()
        withTChan chan f =  
            whileM_ (liftM not $ atomically $ isEmptyTChan chan)
                (atomically (tryReadTChan chan) >>= maybe (return ()) (\a -> f a))             
            

-----------------------------------------------------------------
-- Scintilla callback
-----------------------------------------------------------------

scnCallback :: Session -> TextWindow -> SCNotification -> IO ()
scnCallback ss tw sn = do 

    let hw1 = scnNotifyGetHwnd sn -- event source HWND
    let hw2 = ptrToWord64 (scnGetHwnd (ssOutput ss)) -- output pane HWND

    if hw1 == hw2 then do
        -- event from output pane
        case (scnNotifyGetCode sn) of
            
            2006 -> do -- sCN_DOUBLECLICK
                OP.gotoCompileError ss (fromIntegral (snLine sn) :: Int) (FM.fileOpen ss $ scnCallback ss)
                ssDebugInfo ss "Output pane double click"
                return ()

            otherwise -> do
                -- ssDebugInfo ss $ "Event: " ++ (show $ scnNotifyGetCode sn)
                return ()
    else do
        case (scnNotifyGetCode sn) of                   
            2002 -> do -- sCN_SAVEPOINTREACHED
                FM.updateSaveMenus ss
                return ()
            2003 -> do -- sCN_SAVEPOINTLEFT
                FM.updateSaveMenus ss
                return ()                
            2007 -> do -- sCN_UPDATEUI
                FM.updateStatus ss
                if  ( (.&.) (fromIntegral (snUpdated sn) :: Int) 
                            (fromIntegral sC_UPDATE_SELECTION :: Int)) > 0 then
                    EM.updateEditMenus ss tw
                else
                    return ()
            2028 -> do -- sCN_FOCUSIN
                EM.updateEditMenus ss tw
                return ()               
            2029 -> do -- sCN_FOCUSOUT
                EM.updateEditMenus ss tw
                return ()           
            2013 -> return () -- sCN_PAINTED
              
            otherwise -> do
                -- ssDebugInfo ss $ "Event: " ++ (show $ scnNotifyGetCode sn)
                return ()
         
