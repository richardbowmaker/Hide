
module Main where
import Graphics.UI.WX
import Graphics.UI.WXCore
import Graphics.UI.WX.Events



main = start mainGUI

mainGUI :: IO ()
mainGUI = do 

    f <- frame [] 
    
    set f [ text := "Aui Test", 
            size := (Size 500 500)]
                       
    auiMgr <- auiManagerCreate f wxAUI_MGR_DEFAULT
      
    -- add dockable tree
    tree <- createTree f   
    api <- auiPaneInfoCreateDefault
    auiPaneInfoCaption api "Tree Control"
    auiPaneInfoLeft api
    auiPaneInfoLayer api 1
    auiPaneInfoPosition api 1
    auiPaneInfoCloseButton api True
    auiPaneInfoMaximizeButton api True
    
    auiManagerAddPaneByPaneInfo auiMgr tree api
    
    -- add dockable grid
    grid <- createGrid f
    api <- auiPaneInfoCreateDefault
    auiPaneInfoCaption api "Grid Control"
    auiPaneInfoRight api
    auiPaneInfoLayer api 1
    auiPaneInfoPosition api 1
    auiPaneInfoCloseButton api True
    auiPaneInfoMaximizeButton api True
    
    auiManagerAddPaneByPaneInfo auiMgr grid api
 {-   
    -- add scintilla editor
    p <- panel f []
    hwnd <- windowGetHandle p
    scn <- scnCreateEditor hwnd
    
    set p [ bgcolor := red ]
    api <- auiPaneInfoCreateDefault
    auiPaneInfoCaption api "Scintilla"
    auiPaneInfoCentre api
    auiPaneInfoLayer api 1
    auiPaneInfoPosition api 1
    auiPaneInfoCloseButton api True
    auiPaneInfoMaximizeButton api True

    auiManagerAddPaneByPaneInfo auiMgr p api
 -}   
 
    nb <- auiNotebookCreate f idAny (Point 0 0) (Size 0 0) (wxCLIP_CHILDREN + wxAUI_NB_TOP + wxAUI_NB_CLOSE_ON_ACTIVE_TAB)
    set nb []


    api <- auiPaneInfoCreateDefault
    auiPaneInfoCaption api "Notebook"
    auiPaneInfoCentre api
    auiPaneInfoLayer api 1
    auiPaneInfoPosition api 1
    auiPaneInfoCloseButton api True
    auiPaneInfoMaximizeButton api True

    auiManagerAddPaneByPaneInfo auiMgr nb api
    
{-    
    scn <- scnCreateEditor hwnd
    scnConfigureHaskell scn
    scnEnableEvents scn scnCallback 
-}
    -- create panel with scintilla editor inside

    -- add page to notebook
    p <- panel nb []
    auiNotebookAddPage nb p "1" False 0
    ta <- auiSimpleTabArtCreate
    auiNotebookSetArtProvider nb ta
    
    p <- panel nb []
    auiNotebookAddPage nb p "2" False 0
    ta <- auiSimpleTabArtCreate
    auiNotebookSetArtProvider nb ta
    
    p <- panel nb []
    auiNotebookAddPage nb p "3" False 0
    ta <- auiSimpleTabArtCreate
    auiNotebookSetArtProvider nb ta
    
    p <- panel nb []
    auiNotebookAddPage nb p "4" False 0
    ta <- auiSimpleTabArtCreate
    auiNotebookSetArtProvider nb ta
    
    set nb [ on auiNotebookOnPageCloseEvent := pageClose f]
--    auiNotebookOnPageCloseEvent nb pageClose
    
--    menuFileOpen <- menuItemCreate
--    menuItemSetItemLabel menuFileOpen "Open"
    
    menuFile <- menuCreate "Open" 0
 --   menuAppendItem menuFile menuFileOpen
    
    mbar <- menuBarCreate 0
    menuBarAppend mbar menuFile "&File"
    
    frameSetMenuBar f mbar
    
   
    tb <- auiToolBarCreate f idAny (Point (-1) (-1)) (Size (-1) (-1)) $ wxAUI_TB_DEFAULT_STYLE + wxAUI_TB_OVERFLOW + wxAUI_TB_HORIZONTAL
--    auiToolBarSetToolBitmapSize tb (Size 16 16)

    let save = bitmap  "save.png"
    let burning = bitmap  "burning.ico"
    
--    set save [size := (Size 16 16)]
    tbi <- auiToolBarAddTool tb 101 "Save" save burning wxITEM_NORMAL "help short" "help long" f
    auiToolBarEnableTool tb 101 True
    auiToolBarRealize tb
    
    set tbi [on command := cmdToolbar]
    
 
    api <- auiPaneInfoCreateDefault
    auiPaneInfoCaption api "Toolbar  1"
    auiPaneInfoToolbarPane api
    auiPaneInfoTop api 
    auiPaneInfoLayer api 1
    auiPaneInfoPosition api 1

    auiManagerAddPaneByPaneInfo auiMgr tb api
    
    
 
    auiManagerUpdate auiMgr

    set f [on closing := onClosing f auiMgr]
     
    return ()

cmdToolbar :: IO ()
cmdToolbar = do
    return ()

cmdClose :: Button () -> IO ()
cmdClose b = do
    set b [ text := "Clicked! "]
    return ()
    
onClosing :: Frame() -> AuiManager() -> IO ()
onClosing f aui = do   
    auiManagerUnInit aui
    windowDestroy f
    return ()
    
pageClose :: Frame () -> EventAuiNotebook -> IO ()
pageClose f ev@(AuiNotebookPageClose _ _) = do
--    auiManagerEventVeto ev True
    set f [ text := "Page Closed" ]
    return ()    

------------------------------------------------------------    
-- Tree Control
------------------------------------------------------------    
    
createTree :: Frame () ->  IO (TreeCtrl ())
createTree f = do      
    tree <- treeCtrl f []
    root <- treeCtrlAddRoot tree "root" (-1) (-1) objectNull     
    _    <- treeCtrlAppendItem tree root "item1" (-1) (-1) objectNull
    _    <- treeCtrlAppendItem tree root "item2" (-1) (-1) objectNull
    _    <- treeCtrlAppendItem tree root "item3" (-1) (-1) objectNull
    treeCtrlExpand tree root
    cs <- treeCtrlGetChildren tree root
    return (tree)
    
------------------------------------------------------------    
-- Grid Control
------------------------------------------------------------    

createGrid :: Frame () -> IO (Grid ())
createGrid f = do
    -- grids
    g <- gridCtrl f []
    gridSetGridLineColour g (colorSystem Color3DFace)
    gridSetCellHighlightColour g black
    appendColumns g (head names)
    appendRows    g (map show [1..length (tail names)])
    mapM_ (setRow g) (zip [0..] (tail names))
    gridAutoSize g  
    return (g)
    
gridCtrl :: Window a -> [Prop (Grid ())] -> IO (Grid ())
gridCtrl parent_ props_
  = feed2 props_ 0 $
    initialWindow $ \id_ rect' -> \props' flags ->
    do g <- gridCreate parent_ id_ rect' flags
       gridCreateGrid g 0 0 0
       set g props'
       return g

appendColumns :: Grid a -> [String] -> IO ()
appendColumns _g []
  = return ()
appendColumns g labels
  = do n <- gridGetNumberCols g
       _ <- gridAppendCols g (length labels) True
       mapM_ (\(i, label_) -> gridSetColLabelValue g i label_) (zip [n..] labels)

appendRows :: Grid a -> [String] -> IO ()
appendRows _g []
  = return ()
appendRows g labels
  = do n <- gridGetNumberRows g
       _ <- gridAppendRows g (length labels) True
       mapM_ (\(i, label_) -> gridSetRowLabelValue g i label_) (zip [n..] labels)

setRow :: Grid a -> (Int, [String]) -> IO ()
setRow g (row_, values)
  = mapM_ (\(col,value_) -> gridSetCellValue g row_ col value_) (zip [0..] values)

names :: [[String]]
names
  = [["First Name", "Last Name"]
    ,["Daan","Leijen"],["Arjan","van IJzendoorn"]
    ,["Martijn","Schrage"],["Andres","Loh"]]
