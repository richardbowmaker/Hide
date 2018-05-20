
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
import Data.Bits ((.&.), (.|.)) 
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
import qualified Constants as CN
import qualified EditorNotebook as EN
import qualified FileMenu as FM
import qualified Ghci as GH
import qualified Menus as MN
import qualified Misc as MI
import qualified OutputPane as OT
import qualified Scintilla as SC
import qualified ScintillaConstants as SC
import qualified ScintillaProxyImports as SI
import qualified Session as SS
  
--------------------------------------
-- Main
--------------------------------------   

main = start mainGUI

mainGUI :: IO ()
mainGUI = do 

    ok <- SI.initialise 
    
    if ok then do
     
        -- main window
        mf <- frame []    
        set mf [ text := CN.programTitle, size := (Size 1300 800)]  
         
         -- create statusbar field
        sf <- statusField []

        -- AUI manager and child windows
        ss <- setUpMainWindow mf sf
        
        -- set the statusbar and menubar
        set mf [statusBar := [sf]]

        set mf [on closing :~ onClosing ss]
        
        -- create a timer that updates the display
        t <- timer mf [interval := 100, on command := SS.ssRunFunctionQueue ss] 
           
        return ()
        
    else return ()
   
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
    auiPaneInfoIsFloatable api
    auiPaneInfoIsDockable api
   
    auiManagerAddPaneByPaneInfo am tree api
    
    -- add dockable grid
    grid <- GH.createGrid mf
    api <- auiPaneInfoCreateDefault
    auiPaneInfoCaption api "Grid Control"
    auiPaneInfoBottom api
    auiPaneInfoIsFloatable api
    auiPaneInfoIsDockable api
   
    auiManagerAddPaneByPaneInfo am grid api
    
    -- add editor notebook
    enb <- EN.enbCreate mf
   
    api <- auiPaneInfoCreateDefault
    auiPaneInfoCaption api "Editor"
    auiPaneInfoCentre api
    auiPaneInfoCloseButton api True
    auiPaneInfoMaximizeButton api True
    auiPaneInfoIsFloatable api
    auiPaneInfoIsDockable api

    auiManagerAddPaneByPaneInfo am enb api
   
    -- add output pane
    onb <- OT.createOutputWindow mf
    
    api <- auiPaneInfoCreateDefault
    auiPaneInfoCaption api "Output"
    auiPaneInfoBottom api
    auiPaneInfoCloseButton api True
    auiPaneInfoIsFloatable api
    auiPaneInfoIsDockable api

    auiManagerAddPaneByPaneInfo am onb api
    
    -- add debug window
    dp <- panel mf [size := (Size 400 400)]
    hwnd <- windowGetHandle dp
    scn <- SC.createEditor hwnd
    SC.configureHaskell scn
    SC.setReadOnly scn True
  
    api <- auiPaneInfoCreateDefault
    auiPaneInfoCaption api "Debug"
    auiPaneInfoBottom api
    auiPaneInfoCloseButton api True
    auiPaneInfoIsFloatable api
    auiPaneInfoIsDockable api

    auiManagerAddPaneByPaneInfo am dp api   

    -- update the manager display
    auiManagerUpdate am
    
    -- setup the menus
    menus <- setupMenus mf

    -- create the session data
    ss <- SS.ssCreate mf am enb menus sf onb scn grid FM.fileOpen FM.fileSave
    
    -- setup static menu handlers
    let mhs = [ MN.createMenuHandler MN.menuFileOpen           hwnd (FM.onFileOpen ss)                 (return True),
                MN.createMenuHandler MN.menuFileNew            hwnd (FM.onFileNew ss)                  (return True),
                MN.createMenuHandler MN.menuWindowGhci         hwnd (GH.openWindow ss >> return ())    (return True),
                MN.createMenuHandler MN.menuWindowOutput       hwnd (OT.openOutputWindow ss)           (return True),
                MN.createMenuHandler MN.menuTestTest           hwnd (onTestTest ss)                    (return True),
                MN.createMenuHandler MN.menuBuildNextError     hwnd (OT.gotoNextError ss)              (return False),
                MN.createMenuHandler MN.menuBuildPreviousError hwnd (OT.gotoPreviousError ss)          (return False)]
    SS.ssSetMenuHandlers ss mhs
    SS.ssSetMenus ss

    set enb [on auiNotebookOnPageCloseEvent   := onTabClose         ss]
    set enb [on auiNotebookOnPageChangedEvent := onTabChanged       ss]
    set onb [on auiNotebookOnPageCloseEvent   := onOutputTabClose   ss]
    set onb [on auiNotebookOnPageChangedEvent := onOutputTabChanged ss]

    OT.openOutputWindow ss 

    return ss

    where hwnd = windowGetHandle mf

------------------------------------------------------------    
-- Setup menus
------------------------------------------------------------    

setupMenus :: Frame () -> IO MN.HideMenus
setupMenus mf  = do

    -- file menu  
    menuFile    <- menuPane [text := "&File"]
    menuEdit    <- menuPane [text := "&Edit"]
    menuBuild   <- menuPane [text := "&Build"]
    menuDebug   <- menuPane [text := "Debug"]
    menuWindow  <- menuPane [text := "Window"]
    menuTest    <- menuPane [text := "Test"]
    menuHelp'   <- menuHelp []

    ms <- sequence 
        [
            (MN.createMenu menuFile MN.menuFileOpen), 
            (MN.createMenu menuFile MN.menuFileNew),
            (MN.createMenu menuFile MN.menuFileClose),
            (MN.createMenu menuFile MN.menuFileCloseAll),
            (MN.createMenu menuFile MN.menuFileSave),
            (MN.createMenu menuFile MN.menuFileSaveAs),
            (MN.createMenu menuFile MN.menuFileSaveAll), menuAppendSeparator menuFile >>
            (MN.createMenu menuFile MN.menuFileQuit),

            (MN.createMenu menuEdit MN.menuEditUndo),
            (MN.createMenu menuEdit MN.menuEditRedo), menuAppendSeparator menuEdit >>
            (MN.createMenu menuEdit MN.menuEditCut),
            (MN.createMenu menuEdit MN.menuEditCopy),
            (MN.createMenu menuEdit MN.menuEditPaste),
            (MN.createMenu menuEdit MN.menuEditSelectAll), menuAppendSeparator menuEdit >>
            (MN.createMenu menuEdit MN.menuEditFind),
            (MN.createMenu menuEdit MN.menuEditFindForward),
            (MN.createMenu menuEdit MN.menuEditFindBackward), menuAppendSeparator menuEdit >>
            (MN.createMenu menuEdit MN.menuEditSort),
            (MN.createMenu menuEdit MN.menuEditClear),

            (MN.createMenu menuBuild MN.menuBuildCompile),
            (MN.createMenu menuBuild MN.menuBuildBuild),
            (MN.createMenu menuBuild MN.menuBuildRebuild),
            (MN.createMenu menuBuild MN.menuBuildClean), menuAppendSeparator menuBuild >>
            (MN.createMenu menuBuild MN.menuBuildNextError),
            (MN.createMenu menuBuild MN.menuBuildPreviousError),

            (MN.createMenu menuDebug MN.menuDebugRun),
            (MN.createMenu menuDebug MN.menuDebugGhci),
            (MN.createMenu menuDebug MN.menuDebugDebug),
            (MN.createMenu menuDebug MN.menuDebugStop),
            (MN.createMenu menuDebug MN.menuDebugContinue),
            (MN.createMenu menuDebug MN.menuDebugStep),
            (MN.createMenu menuDebug MN.menuDebugStepLocal),
            (MN.createMenu menuDebug MN.menuDebugStepModule),

            (MN.createMenu menuWindow MN.menuWindowGhci),
            (MN.createMenu menuWindow MN.menuWindowOutput),

            (MN.createMenu menuTest MN.menuTestTest),

            (MN.createMenu menuHelp' MN.menuHelpAbout)
        ]

    set mf [ menuBar := [menuFile, menuEdit, menuBuild, menuDebug, menuWindow, menuTest, menuHelp']]




{-
    -- create Toolbar
    tbar   <- toolBar mf []
    _      <- toolMenu tbar menuFileSave    "" "save.png"    []
    _      <- toolMenu tbar menuFileSaveAll "" "saveall.png" []
-}

   
    return ms
    
------------------------------------------------------------    
-- Event handlers
------------------------------------------------------------    

onClosing :: SS.Session -> (IO ()) -> IO ()
onClosing ss previous = do
    OT.closeOutputWindow ss
    GH.closeAll ss
    FM.fileCloseAll ss
    (auiManagerUnInit . SS.ssAuiMgr) ss
    (windowDestroy . SS.ssFrame) ss
    SI.uninitialise
    propagateEvent
    previous
    return ()

onTabChanged :: SS.Session -> EventAuiNotebook -> IO ()
onTabChanged ss ev@(AuiNotebookPageChanged _ _) = do   
    mtw <- EN.enbGetSelectedSourceFile ss 
    case mtw of
        Just tw -> do 
            case SS.twGetEditor tw of
                Just scn -> SC.grabFocus scn
                Nothing  -> return ()
        Nothing -> return ()

onTabClose :: SS.Session -> EventAuiNotebook -> IO ()
onTabClose ss enb = do
    mtw <- EN.enbGetSelectedSourceFile ss 
    case mtw of
        Just tw -> do 
            case SS.twGetEditor tw of
                Just scn -> do
                    FM.closeEditor ss tw scn
                    return ()
                Nothing  -> return ()
        Nothing -> return ()
    return ()

onOutputTabClose :: SS.Session -> EventAuiNotebook -> IO ()
onOutputTabClose ss _ = do
    mtw <- OT.getSelectedGhci ss 
    case mtw of
        Just tw -> GH.closeWindow ss tw
        Nothing -> OT.closeOutputWindow ss

onOutputTabChanged :: SS.Session -> EventAuiNotebook -> IO ()
onOutputTabChanged ss _ = do
    mtw <- OT.getSelectedGhci ss 
    case mtw of
        Just tw -> GH.setFocus $ SS.twPanelHwnd tw
        Nothing -> return ()

------------------------------------------------------------    
-- Test Menu handlers
------------------------------------------------------------    
 
onTestTest :: SS.Session -> IO ()
onTestTest ss = do 
--    mfn <- SI.winOpenFileDialog (SS.ssFrame ss) "Open a file" "." "*.txt" "text files" 0x1000 -- file must exist
    mfn <- SI.winSaveFileDialog (SS.ssFrame ss) "Save file" "." "*.txt" "text files" "test.txt" 0x02 -- overwrite prompt
    case mfn of 
        Just fn -> infoDialog (SS.ssFrame ss) "file name picked" fn
        Nothing -> infoDialog (SS.ssFrame ss) "no filename" ""
        



