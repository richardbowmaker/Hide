module Constants 
(
    debug,
    programTitle,
    menuFileOpen,
    menuFileNew,
    menuFileClose,
    menuFileCloseAll,
    menuFileSave,
    menuFileSaveAs,
    menuFileSaveAll,
    menuFileQuit,
    menuEditUndo,
    menuEditRedo,
    menuEditCut,
    menuEditCopy,
    menuEditPaste,
    menuEditSelectAll,
    menuEditFind,
    menuEditFindForward,
    menuEditFindBackward,
    menuEditSort,
    menuEditClear,
    menuBuildCompile,
    menuBuildBuild,
    menuBuildRebuild,
    menuBuildClean,
    menuBuildGhci,
    menuDebugRun,
    menuWindowGhci,
    menuWindowOutput,
    menuTestTest,
    menuHelpAbout,
    menuId,
    menuTitle,
    menuKey,
    menuHelp,
    menuHelp',
    menuText',
    menuTitle',
    menuKey'
)
where

import Data.List (find)
import Data.Maybe (maybe)

debug = True

programTitle :: String
programTitle = "HIDE Dev"

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

menuBuildGhci :: Int
menuBuildGhci = 24

menuDebugRun :: Int
menuDebugRun = 25

menuWindowGhci :: Int
menuWindowGhci = 26

menuWindowOutput :: Int
menuWindowOutput = 27

menuTestTest :: Int
menuTestTest = 28

menuHelpAbout :: Int
menuHelpAbout = 29

-----------------------------
-- menu names
-----------------------------

data Menu = Menu { menuId :: Int, menuTitle :: String, menuKey :: String, menuHelp :: String }

menuNames :: [Menu]
menuNames = [   (Menu menuFileOpen           "Open"             "Ctrl-O"        "Opens a file"),
                (Menu menuFileNew            "New"              "Ctrl-N"        "Starts a new file"),
                (Menu menuFileClose          "Close"            ""              "Closes the current file"),
                (Menu menuFileCloseAll       "Close All"        ""              "Closes all files"),
                (Menu menuFileSave           "Save"             "Ctrl-S"        "Saves a file"),
                (Menu menuFileSaveAs         "Save As ..."      ""              "Saves a file"),
                (Menu menuFileSaveAll        "Save All"         "Ctrl-Shift-S"  "Saves all files"),
                (Menu menuFileQuit           "Quit"             "Ctrl-Q"        "Quits HIDE"),
                (Menu menuEditUndo           "Undo"             "Ctrl-Z"        "Undoes last edit"),
                (Menu menuEditRedo           "Redo"             "Ctrl-Y"        "Redoes an edit"),
                (Menu menuEditCut            "Cut"              "Ctrl-X"        "Cut selected text"),
                (Menu menuEditCopy           "Copy"             "Ctrl-C"        "Copy select text"),
                (Menu menuEditPaste          "Paste"            "Ctrl-V"        "Paste contents of clipboard"),
                (Menu menuEditSelectAll      "Select All"       "Ctrl-A"        "Selects all text"),
                (Menu menuEditFind           "Find"             "Ctrl-F"        "Find text"),
                (Menu menuEditFindForward    "Find Forward"     "F3"            "Find next occurrence"),
                (Menu menuEditFindBackward   "Find Backward"    "Shift-F3"      "Find previous occurrence"),
                (Menu menuEditSort           "Sort"             ""              "Sort selected text"),
                (Menu menuEditClear          "Clear"            ""              "Clear all text"),
                (Menu menuBuildCompile       "Compile"          "Ctrl-F7"       "Compiles current file"),
                (Menu menuBuildBuild         "Build"            "Ctrl-Shift-B"  "Builds the project"),
                (Menu menuBuildRebuild       "Rebuild"          ""              "Rebuilds the project"),
                (Menu menuBuildClean         "Clean"            ""              "Cleans the project files"),
                (Menu menuBuildGhci          "Open GHCI"        "Alt-F11"       "Open file in GHCI"),
                (Menu menuDebugRun           "Run"              "F5"            "Run program"),
                (Menu menuWindowGhci         "GHCI"             "F11"           "Start GHCI"),
                (Menu menuWindowOutput       "Output"           "F12"           "Open output window"),
                (Menu menuTestTest           "Test"             "Ctrl-T"        ""),
                (Menu menuHelpAbout          "About"            ""              "About HIDE")]
                            
menuText' :: Int -> String
menuText' id = maybe "" (\m -> (menuTitle m) ++ "\t" ++ (menuKey m)) 
                $ find (\(Menu mid _ _ _) -> id == mid) menuNames

menuTitle' :: Int -> String
menuTitle' id = maybe "" (\m -> (menuTitle m)) $ find (\(Menu mid _ _ _) -> id == mid) menuNames

menuHelp' :: Int -> String
menuHelp' id = maybe "" (\m -> (menuHelp m)) $ find (\(Menu mid _ _ _) -> id == mid) menuNames

menuKey' :: Int -> String
menuKey' id = maybe "" (\m -> (menuKey m)) $ find (\(Menu mid _ _ _) -> id == mid) menuNames
 