module Main where
import Graphics.UI.WX
import Graphics.UI.WXCore
import Graphics.Win32.Misc



main = start mainGUI

mainGUI :: IO ()
mainGUI = do 

    f      <- frame         [text := "Hello world!", clientSize := sz 300 200]                               

    -- create file menu  
    menuFile        <- menuPane             [text := "&File"]
    menuFileOpen    <- menuItem menuFile    [text := "Open ...\tCtrl-O", help := "Opens a file"]
    set menuFileOpen [on command := fileOpen menuFileOpen f]
                                             
    menuFileSave    <- menuItem menuFile    [text := "Save\tCtrl-S", help := "Saves a file"]
    menuFileSaveAs  <- menuItem menuFile    [text := "Save As ...\tCtrl-Shift-S", help := "Saves a file"]
    set menuFileSaveAs [on command := fileSaveAs menuFileSaveAs f]
                                             
    menuAppendSeparator menuFile
                             
    _quit  <- menuQuit menuFile [help := "Quit the demo", on command := close f]

    menuEdit         <- menuPane            [text := "&Edit"]
    menuEditUndo     <- menuItem menuEdit   [text := "Undo\tCtrl-Z"]
    
    menuBuild        <- menuPane            [text := "Build"]
    menuBuildCompile <- menuItem menuBuild  [text := "Compile\tCtrl-F7",       help := "Compiles current source file"]
    menuBuildBuild   <- menuItem menuBuild  [text := "Build\tF7",              help := "Build the project"]
    menuBuildReBuild <- menuItem menuBuild  [text := "Rebuild\tCtrl-Alt-F7",   help := "Rebuild the project"]
    menuBuildClean   <- menuItem menuBuild  [text := "Clean",                  help := "Clean the project"]
    
      
    -- create Help menu
    menuHelp'        <- menuHelp []
    menuHelpAbout    <- menuAbout menuHelp' [help := "About HeyHo"]

    -- create statusbar field
    statusBar' <- statusField   [text := "Welcome to wxHaskell"]

    -- set the statusbar and menubar
    set f [ statusBar := [statusBar']
         , menuBar   := [menuFile, menuEdit, menuBuild, menuHelp']
         -- as an example, put the menu event handler for an about box on the frame.
         ,on (menu menuHelpAbout) := infoDialog f "About wxHaskell" "This is a wxHaskell demo"
         ]

    return ()
    
fileOpen :: MenuItem () -> Frame () -> IO ()
fileOpen m f = do
    h <- windowGetHandle f
    messageBox h "open" "open" mB_OK
    return ()

fileSaveAs :: MenuItem () -> Frame () -> IO ()
fileSaveAs m f = do
    h <- windowGetHandle f
    messageBox h "save as" "save as" mB_OK
    return ()
    