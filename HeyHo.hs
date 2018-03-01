
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
import Compile
import qualified EditMenu as EM
import EditorNotebook
import qualified FileMenu as FM
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
    set mf [ text := ssProgramTitle, size := (Size 1300 800)]  
     
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
    enbAddNewFile ss (scnCallback ss)
    
    -- setup menu handlers
    set (ssMenuListGet ss "FileOpen")           [on command := onFileOpen           ss]
    set (ssMenuListGet ss "FileNew")            [on command := onFileNew            ss]
    set (ssMenuListGet ss "FileSave")           [on command := onFileSave           ss]
    set (ssMenuListGet ss "FileSaveAs")         [on command := onFileSaveAs         ss]
    set (ssMenuListGet ss "FileSaveAll")        [on command := onFileSaveAll        ss]
    set (ssMenuListGet ss "FileClose")          [on command := onFileClose          ss]
    set (ssMenuListGet ss "FileCloseAll")       [on command := onFileCloseAll       ss]
    set (ssMenuListGet ss "EditUndo")           [on command := onEditUndo           ss]
    set (ssMenuListGet ss "EditRedo")           [on command := onEditRedo           ss]
    set (ssMenuListGet ss "EditCut")            [on command := onEditCut            ss]
    set (ssMenuListGet ss "EditCopy")           [on command := onEditCopy           ss]
    set (ssMenuListGet ss "EditPaste")          [on command := onEditPaste          ss]
    set (ssMenuListGet ss "EditAll")            [on command := onEditSelectAll      ss]
    set (ssMenuListGet ss "EditFind")           [on command := onEditFind           ss]
    set (ssMenuListGet ss "EditFindForward")    [on command := onEditFindForward    ss]
    set (ssMenuListGet ss "EditFindBackward")   [on command := onEditFindBackward   ss]
    set (ssMenuListGet ss "BuildBuild")         [on command := onBuildBuild         ss]
    set (ssMenuListGet ss "BuildCompile")       [on command := onBuildCompile       ss]
    set (ssMenuListGet ss "BuildGhci")          [on command := onBuildGhci          ss]
    set (ssMenuListGet ss "DebugRun")           [on command := onDebugRun           ss]
    set (ssMenuListGet ss "DebugGhci")          [on command := onDebugGhci          ss]
    set (ssMenuListGet ss "TestTest")           [on command := onTestTest           ss]
     
    set enb [on auiNotebookOnPageCloseEvent   := onTabClose   ss]
    set enb [on auiNotebookOnPageChangedEvent := onTabChanged ss]

    set onb [on auiNotebookOnPageCloseEvent   := onOutputTabClose ss]

   -- enable events for output pane, dbl click = goto error
    scnSetEventHandler oe $ scnCallback ss
    scnEnableEvents oe
   
    return (ss)
  
------------------------------------------------------------    
-- Setup menus
------------------------------------------------------------    

setupMenus :: Frame () -> IO (SsMenuList)
setupMenus mf  = do

    -- file menu  
    menuFile            <- menuPane             [text := "&File"]
    menuFileOpen        <- menuItem menuFile    [text := "Open ...\tCtrl-O",        help := "Opens a file"]
    menuFileNew         <- menuItem menuFile    [text := "New\tCtrl-N",             help := "Starts a new file"]
    menuFileClose       <- menuItem menuFile    [text := "Close",                   help := "Closes the current file"]
    menuFileCloseAll    <- menuItem menuFile    [text := "Close All",               help := "Closes all files"]                                             
    menuFileSave        <- menuItem menuFile    [text := "Save\tCtrl-S",            help := "Saves a file", enabled := False]
    menuFileSaveAs      <- menuItem menuFile    [text := "Save As ...",             help := "Saves a file"]
    menuFileSaveAll     <- menuItem menuFile    [text := "Save All\tCtrl-Shift-S",  help := "Saves all files", enabled := False]
                                             
    menuAppendSeparator menuFile
                             
    menuQuit  <- menuQuit menuFile [help := "Quit the demo", on command := close mf]

    menuEdit                <- menuPane            [text := "&Edit"]
    menuEditUndo            <- menuItem menuEdit   [text := "Undo\tCtrl-Z"]
    menuEditRedo            <- menuItem menuEdit   [text := "Redo\tCtrl-Y"]
    menuAppendSeparator menuEdit
    menuEditCut             <- menuItem menuEdit   [text := "Cut\tCtrl-X"]
    menuEditCopy            <- menuItem menuEdit   [text := "Copy\tCtrl-C"]
    menuEditPaste           <- menuItem menuEdit   [text := "Paste\tCtrl-V"]
    menuEditAll             <- menuItem menuEdit   [text := "Select All\tCtrl-A"]
    menuEditFind            <- menuItem menuEdit   [text := "Find\tCtrl-F"]
    menuEditFindForward     <- menuItem menuEdit   [text := "Find Forward\tF3"]
    menuEditFindBackward    <- menuItem menuEdit   [text := "Find Backward\tShift-F3"]
    
    menuBuild        <- menuPane            [text := "Build"]
    menuBuildCompile <- menuItem menuBuild  [text := "Compile\tCtrl-F7",        help := "Compiles current source file"]
    menuBuildBuild   <- menuItem menuBuild  [text := "Build\tF7",               help := "Build the project"]
    menuBuildReBuild <- menuItem menuBuild  [text := "Rebuild\tCtrl-Alt-F7",    help := "Rebuild the project"]
    menuBuildClean   <- menuItem menuBuild  [text := "Clean",                   help := "Clean the project"]
    menuBuildGhci    <- menuItem menuBuild  [text := "Open GHCI\tAlt-F11",      help := "Compile and load file into GHCI"]
          
    menuDebug        <- menuPane            [text := "Debug"]
    menuDebugRun     <- menuItem menuDebug  [text := "Run\tF5",   help := "Compiles current source file"]
    menuDebugGhci    <- menuItem menuDebug  [text := "GHCI\tF11", help := "Compiles current source file"]

    menuTest         <- menuPane            [text := "Test"]
    menuTestTest     <- menuItem menuTest   [text := "Test\tCtrl-T"]

    -- create Help menu
    menuHelp'        <- menuHelp []
    menuHelpAbout    <- menuAbout menuHelp' [help := "About HeyHo", on command := infoDialog mf "About HeyHo" "mmmmm !"]
      
    set mf [ menuBar := [menuFile, menuEdit, menuBuild, menuDebug, menuTest, menuHelp']]

    -- create lookup list of menus for session data   
    ml <- ssMenuListCreate [    ("FileOpen",            menuFileOpen), 
                                ("FileSave",            menuFileSave), 
                                ("FileNew",             menuFileNew), 
                                ("FileClose",           menuFileClose), 
                                ("FileCloseAll",        menuFileCloseAll), 
                                ("FileSaveAs",          menuFileSaveAs), 
                                ("FileSaveAll",         menuFileSaveAll),
                                ("EditUndo",            menuEditUndo),
                                ("EditRedo",            menuEditRedo),
                                ("EditCut",             menuEditCut),
                                ("EditCopy",            menuEditCopy),
                                ("EditPaste",           menuEditPaste),
                                ("EditAll",             menuEditAll),
                                ("EditFind",            menuEditFind),
                                ("EditFindForward",     menuEditFindForward),
                                ("EditFindBackward",    menuEditFindBackward),
                                ("BuildBuild",          menuBuildBuild),
                                ("BuildCompile",        menuBuildCompile),
                                ("BuildGhci",           menuBuildGhci),
                                ("DebugRun",            menuDebugRun),
                                ("DebugGhci",           menuDebugGhci),
                                ("TestTest",            menuTestTest)]

    
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
    set (ssMenuListGet ss "BuildCompile") [text := "Compile\tCtrl-F7"]        
    set (ssMenuListGet ss "BuildGhci")    [text := "Open GHCI\tAlt-F11"] 
    c <- enbGetTabCount ss
    if c > 0 then do
        sf <- enbGetSelectedSourceFile ss 
        (scnGrabFocus . sfEditor) sf
        FM.updateSaveMenus ss
        EM.updateEditMenus ss
        FM.updateStatus ss
        case (sfFilePath sf) of
            Just fp -> do
                set (ssMenuListGet ss "BuildCompile") [text := "Compile " ++ (takeFileName fp) ++ "\tCtrl-F7"]        
                set (ssMenuListGet ss "BuildGhci")    [text := "Open GHCI " ++ (takeFileName fp) ++ "\tAlt-F11"] 
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
    EM.updateEditMenus ss
    return ()

onOutputTabClose :: Session -> EventAuiNotebook -> IO ()
onOutputTabClose ss _ = OP.closeGhci ss
  
onFileClose :: Session -> IO ()
onFileClose ss = do
    enbGetSelectedSourceFile ss >>= FM.fileClose ss 
    FM.updateSaveMenus ss    
    EM.updateEditMenus ss
    return ()
  
onFileCloseAll :: Session -> IO ()
onFileCloseAll = FM.fileCloseAll

onFileSave :: Session -> IO ()
onFileSave ss = do
    enbGetSelectedSourceFile ss >>= FM.fileSave ss 
    FM.updateSaveMenus ss    
    EM.updateEditMenus ss
    return ()

onFileSaveAs :: Session -> IO ()
onFileSaveAs ss = do
    enbGetSelectedSourceFile ss >>= FM.fileSaveAs ss
    FM.updateSaveMenus ss    
    EM.updateEditMenus ss
    return ()
    
onFileSaveAll :: Session -> IO ()    
onFileSaveAll ss = do   
    FM.fileSaveAll ss
    FM.updateSaveMenus ss    
    EM.updateEditMenus ss
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
        EM.updateEditMenus ss   
        set (ssMenuListGet ss "BuildCompile") [text := "Compile " ++ (takeFileName fp) ++ "\tCtrl-F7"]        
        set (ssMenuListGet ss "BuildGhci")    [text := "Open GHCI " ++ (takeFileName fp) ++ "\tAlt-F11"]      
        return ()
    else
        return ()

onFileNew :: Session -> IO ()
onFileNew ss = do
    enbAddNewFile ss (scnCallback ss) >>= enbSelectTab ss 
    FM.updateSaveMenus ss    
    EM.updateEditMenus ss    
    return ()
 
------------------------------------------------------------    
-- Edit Menu handlers
------------------------------------------------------------    

onEdit :: Session -> IO ()
onEdit ss = infoDialog (ssFrame ss) "edit" "edit"

onEditUndo :: Session -> IO ()
onEditUndo ss = ifEditorHasFocus ss (scnUndo . sfEditor)

onEditRedo :: Session -> IO ()
onEditRedo ss = ifEditorHasFocus ss (scnRedo . sfEditor)

onEditCut :: Session -> IO ()
onEditCut ss = ifEditorHasFocus ss (scnCut . sfEditor) -- >> ifGhciHasFocus OP.ghciCut

onEditCopy :: Session -> IO ()
onEditCopy ss = ifEditorHasFocus ss (scnCopy . sfEditor) -- >> ifGhciHasFocus OP.ghciCopy
    
onEditPaste :: Session -> IO ()
onEditPaste ss = ifEditorHasFocus ss (scnPaste . sfEditor) -- >> ifGhciHasFocus OP.ghciPaste

onEditSelectAll :: Session -> IO ()
onEditSelectAll ss = ifEditorHasFocus ss (scnSelectAll . sfEditor) -- >> ifGhciHasFocus OP.ghciSelectAll
 
ifEditorHasFocus :: Session -> (SourceFile -> IO ()) -> IO ()
ifEditorHasFocus ss action = do
    sf <- enbGetSelectedSourceFile ss
    f <- (scnGetFocus . sfEditor ) sf
    if f then action sf else return ()
 
ifGhciHasFocus :: (IO Bool) -> IO ()
ifGhciHasFocus action = do
    h <- OP.ghciHasFocus
    if ((ptrToWord64 h) /= 0) then action >> return () else return ()

onEditFind :: Session -> IO ()
onEditFind = EM.editFind

onEditFindForward :: Session -> IO ()
onEditFindForward = EM.editFindForward

onEditFindBackward :: Session -> IO ()
onEditFindBackward = EM.editFindBackward

onTestTest :: Session -> IO ()
onTestTest ss = do 
    return ()


------------------------------------------------------------    
-- Build Menu handlers
------------------------------------------------------------    
    
onBuildBuild :: Session -> IO ()
onBuildBuild ss = do

    set (ssMenuListGet ss "BuildBuild")   [enabled := False]        
    set (ssMenuListGet ss "BuildCompile") [enabled := False]
    set (ssMenuListGet ss "BuildGhci")    [enabled := False]

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

    set (ssMenuListGet ss "BuildBuild")   [enabled := False]        
    set (ssMenuListGet ss "BuildCompile") [enabled := False]
    set (ssMenuListGet ss "BuildGhci")    [enabled := False]

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
    set (ssMenuListGet ss "BuildBuild")   [enabled := True]        
    set (ssMenuListGet ss "BuildCompile") [enabled := True]
    set (ssMenuListGet ss "BuildGhci")    [enabled := True]
    otAddText ss $ BS.pack "Compile complete\n"
    return ()

onBuildGhci :: Session -> IO ()
onBuildGhci ss = do

    set (ssMenuListGet ss "BuildBuild")   [enabled := False]        
    set (ssMenuListGet ss "BuildCompile") [enabled := False]
    set (ssMenuListGet ss "BuildGhci")    [enabled := False]

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
    set (ssMenuListGet ss "BuildBuild")   [enabled := True]        
    set (ssMenuListGet ss "BuildCompile") [enabled := True]
    set (ssMenuListGet ss "BuildGhci")    [enabled := True]
    otAddText ss $ BS.pack "Compile complete\n"

    ces <- atomically $ readTVar $ ssCompilerReport ss
    case ces of
        [] -> OP.openGhciFile ss sf -- no errors, open GHCI
        _  -> do
            ans <- proceedDialog (ssFrame ss) ssProgramTitle "There were compilation errors, continue ?"
            case ans of
                True -> OP.openGhciFile ss sf
                False -> return ()

------------------------------------------------------------    
-- Debug menu handlers
------------------------------------------------------------    
    
onDebugRun :: Session -> IO ()
onDebugRun ss = do
    sf <- enbGetSelectedSourceFile ss
    case (sfFilePath sf) of
        Just fp -> cpDebugRun ss fp 
        Nothing -> warningDialog (ssFrame ss) ssProgramTitle "Source file has not been given a name" 
    return ()

onDebugGhci :: Session -> IO ()
onDebugGhci = OP.openGhci

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

scnCallback :: Session -> SCNotification -> IO ()
scnCallback ss sn = do 

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
            
        -- If the constants are used rather than the real values the compiler
        -- gives some nonsense about overlapping cases !! compiler bug
        
            2002 -> do -- sCN_SAVEPOINTREACHED
                FM.updateSaveMenus ss
                EM.updateEditMenus ss
                return ()

            2003 -> do -- sCN_SAVEPOINTLEFT
                FM.updateSaveMenus ss
                EM.updateEditMenus ss
                return ()
                
            2007 -> do -- sCN_UPDATEUI
                FM.updateStatus ss
                EM.updateEditMenus ss
                return ()
            
            2013 -> return () -- sCN_PAINTED
              
            otherwise -> do
                -- ssDebugInfo ss $ "Event: " ++ (show $ scnNotifyGetCode sn)
                return ()
         


