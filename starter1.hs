module Main where

import Graphics.UI.WX
import Graphics.UI.WXCore
import qualified Scintilla as SC

main = start mainGUI

mainGUI :: IO ()
mainGUI = do 
  
    -- main window
    mf <- frame []    
    set mf [text := "Starter", size := (Size 1300 800)] 

    p1 <- panel mf [bgcolor := red] 
    p2 <- panel mf [bgcolor := blue] 

    
    set mf [layout:= row 5 [fill $ widget p1, fill $ widget p2]]
    set mf [size := (Size 1300 800)]
         
     -- create statusbar field
    sf <- statusField []

    -- set the statusbar and menubar
    set mf [statusBar := [sf]]

    hwnd <- windowGetHandle p1
    scn <- SC.createEditor hwnd
{-
    -- AUI in pane 2
    auiMgr <- auiManagerCreate p2 wxAUI_MGR_DEFAULT

    nb <- auiNotebookCreate mf idAny (Point 0 0) (Size 0 0) (wxCLIP_CHILDREN + wxAUI_NB_TOP + wxAUI_NB_CLOSE_ON_ACTIVE_TAB)

    api <- auiPaneInfoCreateDefault
    auiPaneInfoCaption api "Notebook"
    auiPaneInfoCentre api
    auiPaneInfoLayer api 1
    auiPaneInfoPosition api 1
    auiPaneInfoCloseButton api True
    auiPaneInfoMaximizeButton api True

    auiManagerAddPaneByPaneInfo auiMgr nb api

    auiManagerUpdate auiMgr
-}

    -- create a timer that updates the display
    t <- timer mf [interval := 100, on command := onTimer] 

    set sf [text:= "my status"]
       
    return ()

onTimer = return ()
   
