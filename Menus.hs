
module Menus 
(
    HideMenu,
    HideMenuHandler,
    HideMenus,
    HideMenuHandlers,
    createMenu,
    createMenuHandler,
    disableMenus,
    menuBuildBuild,
    menuBuildClean,
    menuBuildCompile,
    menuBuildNextError,
    menuBuildPreviousError,
    menuBuildRebuild,
    menuDebugContinue,
    menuDebugDebug,
    menuDebugGhci,
    menuDebugRun,
    menuDebugStep,
    menuDebugStepLocal,
    menuDebugStepModule,
    menuDebugStop,
    menuEditClear,
    menuEditCopy,
    menuEditCut,
    menuEditFind,
    menuEditFindBackward,
    menuEditFindForward,
    menuEditPaste,
    menuEditRedo,
    menuEditSelectAll,
    menuEditSort,
    menuEditUndo,
    menuFileClose,
    menuFileCloseAll,
    menuFileNew,
    menuFileOpen,
    menuFileQuit,
    menuFileSave,
    menuFileSaveAll,
    menuFileSaveAs,
    menuHelpAbout,
    menuTestTest,
    menuWindowGhci,
    menuWindowOutput,
    mergeMenus,
    mnAction,
    mnEnabled,
    mnHelp,
    mnHwnd,  
    mnId,
    mnItem,
    mnTitle
)
where

import Control.Applicative (liftA)
import Data.List (find)
import Graphics.UI.WX
import Graphics.UI.WXCore
import Graphics.Win32.GDI.Types (HWND)

import qualified Constants as CN
import qualified Misc as MI

type HideMenus = [HideMenu]

data HideMenu = HideMenu
    {
        mnId            :: Int, -- menu ident
        mnItem          :: MenuItem (),
        mnTitle         :: String,
        mnHelp          :: String,
        mnAction        :: IO (),
        mnEnabled       :: IO Bool,
        mnHwnd          :: Maybe HWND        
    }

type HideMenuHandlers = [HideMenuHandler]

data HideMenuHandler = HideMenuHandler
    {
        mhId            :: Int, -- menu ident
        mhAction        :: IO (),
        mhEnabled       :: IO Bool,
        mhHwnd          :: HWND
    }

-- creates a menu item and attaches it to the parent
-- createMenu parent menuid
createMenu :: Menu () -> Int -> IO HideMenu
createMenu parent mid = do
    item <- menuItem parent [text := (title ++ "\t" ++ key), help := help', enabled := False]  
    return $ (HideMenu mid item title help' (return ()) (return False) Nothing)
    where 
        title = maybe "" mcTitle $ find (\m -> mcId m == mid) menusConfig
        key   = maybe "" mcKey   $ find (\m -> mcId m == mid) menusConfig
        help' = maybe "" mcHelp  $ find (\m -> mcId m == mid) menusConfig

-- creates a hide window with just the handlers set, is a precursor to
-- merging a windows menus into the current active set of menus
createMenuHandler :: Int -> HWND -> IO () -> IO Bool -> HideMenuHandler
createMenuHandler mid hwnd action enabled = (HideMenuHandler mid action enabled hwnd)

-- merge a set of menu handlers into the active set
-- mergeMenus active new, each menu in new is used to set the handlers (action and enabled) in
-- the corresponding menu in the active set.
mergeMenus :: HideMenus -> HideMenuHandlers -> HideMenus
mergeMenus menus handlers = 
    flip map menus $ \m -> 
        case find (\h -> mnId m == mhId h) handlers of
            Just h  -> m { mnAction = mhAction h, mnEnabled = mhEnabled h, mnHwnd = Just $ mhHwnd h } 
            Nothing -> m

-- disables all menu handlers owned by the window with hwnd
disableMenus :: HideMenus -> HWND -> HideMenus
disableMenus menus hwnd = 
    flip map menus $ \m ->
        case mnHwnd m of
            Just hwnd' ->   
                if hwnd == hwnd' 
                then m { mnAction = return (), mnEnabled = return False } 
                else m
            Nothing -> m

data MenuCfg = MenuCfg { mcId :: Int, mcTitle :: String, mcKey :: String, mcHelp :: String }

menusConfig :: [MenuCfg]
menusConfig = [ (MenuCfg menuFileOpen           "Open"               "Ctrl-O"        "Opens a file"),
                (MenuCfg menuFileNew            "New"                "Ctrl-N"        "Starts a new file"),
                (MenuCfg menuFileClose          "Close"              ""              "Closes the current file"),
                (MenuCfg menuFileCloseAll       "Close All"          ""              "Closes all files"),
                (MenuCfg menuFileSave           "Save"               "Ctrl-S"        "Saves a file"),
                (MenuCfg menuFileSaveAs         "Save As ..."        ""              "Saves a file"),
                (MenuCfg menuFileSaveAll        "Save All"           "Ctrl-Shift-S"  "Saves all files"),
                (MenuCfg menuFileQuit           "Quit"               "Ctrl-Q"        "Quits HIDE"),
                (MenuCfg menuEditUndo           "Undo"               "Ctrl-Z"        "Undoes last edit"),
                (MenuCfg menuEditRedo           "Redo"               "Ctrl-Y"        "Redoes an edit"),
                (MenuCfg menuEditCut            "Cut"                "Ctrl-X"        "Cut selected text"),
                (MenuCfg menuEditCopy           "Copy"               "Ctrl-C"        "Copy select text"),
                (MenuCfg menuEditPaste          "Paste"              "Ctrl-V"        "Paste contents of clipboard"),
                (MenuCfg menuEditSelectAll      "Select All"         "Ctrl-A"        "Selects all text"),
                (MenuCfg menuEditFind           "Find"               "Ctrl-F"        "Find text"),
                (MenuCfg menuEditFindForward    "Find Forward"       "F3"            "Find next occurrence"),
                (MenuCfg menuEditFindBackward   "Find Backward"      "Shift-F3"      "Find previous occurrence"),
                (MenuCfg menuEditSort           "Sort"               ""              "Sort selected text"),
                (MenuCfg menuEditClear          "Clear"              ""              "Clear all text"),
                (MenuCfg menuBuildCompile       "Compile"            "Ctrl-F7"       "Compiles current file"),
                (MenuCfg menuBuildBuild         "Build"              "Ctrl-Shift-B"  "Builds the project"),
                (MenuCfg menuBuildRebuild       "Rebuild"            ""              "Rebuilds the project"),
                (MenuCfg menuBuildClean         "Clean"              ""              "Cleans the project files"),
                (MenuCfg menuBuildNextError     "Next error"         "F4"            "Goto next compile error"),
                (MenuCfg menuBuildPreviousError "Previous error"     "Shift-F4"      "Goto previous compile error"),
                (MenuCfg menuDebugRun           "Run"                "F5"            "Run program"),
                (MenuCfg menuDebugGhci          "Open GHCI"          "F9"            "Open file in GHCI"),
                (MenuCfg menuDebugDebug         "Debug"              "F6"            "Run program in GHCI"),
                (MenuCfg menuDebugStop          "Debug Stop"         "Shift-F6"      "Stop debugging"),
                (MenuCfg menuDebugContinue      "Debug Continue"     "Shift-F11"     "Continue in debugger"),
                (MenuCfg menuDebugStep          "Single step"        "F11"           "Single step debugger"),
                (MenuCfg menuDebugStepModule    "Single step local"  "F10"           "Single step debugger within current binding"),
                (MenuCfg menuDebugStepLocal     "Single step module" "F12"           "Single step debugger within module"),
                (MenuCfg menuWindowGhci         "GHCI"               "Alt-F9"        "Start GHCI"),
                (MenuCfg menuWindowOutput       "Output"             "F8"            "Open output window"),
                (MenuCfg menuTestTest           "Test"               "Ctrl-T"        ""),
                (MenuCfg menuHelpAbout          "About"              ""              "About HIDE")]

-------------------------------------
-- menu ids
-------------------------------------

menuFileOpen :: Int
menuFileOpen = 1

menuFileNew :: Int
menuFileNew = 2

menuFileClose :: Int
menuFileClose = 3

menuFileCloseAll :: Int
menuFileCloseAll = 4

menuFileSave :: Int
menuFileSave = 5

menuFileSaveAs :: Int
menuFileSaveAs = 6

menuFileSaveAll :: Int
menuFileSaveAll = 7

menuFileQuit :: Int
menuFileQuit = 8

menuEditUndo :: Int
menuEditUndo = 9

menuEditRedo :: Int
menuEditRedo = 10

menuEditCut :: Int
menuEditCut = 11

menuEditCopy :: Int
menuEditCopy = 12

menuEditPaste :: Int
menuEditPaste = 13

menuEditSelectAll :: Int
menuEditSelectAll = 14

menuEditFind :: Int
menuEditFind = 15

menuEditFindForward :: Int
menuEditFindForward = 16

menuEditFindBackward :: Int
menuEditFindBackward = 17

menuEditSort :: Int
menuEditSort = 18

menuEditClear :: Int
menuEditClear = 19

menuBuildCompile :: Int
menuBuildCompile = 20

menuBuildBuild :: Int
menuBuildBuild = 21

menuBuildRebuild :: Int
menuBuildRebuild = 22

menuBuildClean :: Int
menuBuildClean = 23

menuBuildNextError :: Int
menuBuildNextError = 24

menuBuildPreviousError :: Int
menuBuildPreviousError = 25

menuDebugRun :: Int
menuDebugRun = 26

menuDebugGhci :: Int
menuDebugGhci = 27

menuDebugDebug :: Int
menuDebugDebug = 28

menuDebugStop :: Int
menuDebugStop = 29

menuDebugContinue :: Int
menuDebugContinue = 30

menuDebugStep :: Int
menuDebugStep = 31

menuDebugStepLocal:: Int
menuDebugStepLocal = 32

menuDebugStepModule :: Int
menuDebugStepModule = 33

menuWindowGhci :: Int
menuWindowGhci = 34

menuWindowOutput :: Int
menuWindowOutput = 35

menuTestTest :: Int
menuTestTest = 36

menuHelpAbout :: Int
menuHelpAbout = 37


