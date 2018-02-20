
-- main IDE for Haskell --

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
import Numeric (showHex)
import Text.Printf (printf)
import System.IO
import System.FilePath.Windows (takeFileName)
import System.Process
import System.Process.Common

-- project imports
import Compile
import EditorNotebook
import Misc
import OutputPane
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
    (onb, oe) <- opCreate mf
    
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
    ss <- ssCreate mf am enb (prCreate []) menus sf oe scn
    
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
    set (ssMenuListGet ss "DebugRun")           [on command := onDebugRun           ss]
    set (ssMenuListGet ss "TestTest")           [on command := onTestTest           ss]
    
    set enb [on auiNotebookOnPageCloseEvent   := onTabClose   ss]
    set enb [on auiNotebookOnPageChangedEvent := onTabChanged ss]

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
          
    menuDebug        <- menuPane            [text := "Debug"]
    menuDebugRun     <- menuItem menuDebug  [text := "Run\tF5",                 help := "Compiles current source file"]

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
                                ("DebugRun",            menuDebugRun),
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
    fileCloseAll ss
    (auiManagerUnInit . ssAuiMgr) ss
    (windowDestroy . ssFrame) ss
    return ()

onTabChanged :: Session -> EventAuiNotebook -> IO ()
onTabChanged ss ev@(AuiNotebookPageChanged _ _) = do   
    c <- enbGetTabCount ss
    if c > 0 then do
        enbGetSelectedSourceFile ss >>= (scnGrabFocus . sfEditor)
        updateSaveMenus ss
        updateEditMenus ss
        updateStatus ss
        return ()
    else return ()

onTabClose :: Session -> EventAuiNotebook -> IO ()
onTabClose ss _ = do
    enbGetSelectedSourceFile ss >>= closeEditor ss    
    updateSaveMenus ss      
    updateEditMenus ss
    return ()
    
onFileClose :: Session -> IO ()
onFileClose ss = do
    enbGetSelectedSourceFile ss >>= fileClose ss 
    updateSaveMenus ss    
    updateEditMenus ss
    return ()
  
onFileCloseAll :: Session -> IO ()
onFileCloseAll = fileCloseAll

onFileSave :: Session -> IO ()
onFileSave ss = do
    enbGetSelectedSourceFile ss >>= fileSave ss 
    updateSaveMenus ss    
    updateEditMenus ss
    return ()

onFileSaveAs :: Session -> IO ()
onFileSaveAs ss = do
    enbGetSelectedSourceFile ss >>= fileSaveAs ss
    updateSaveMenus ss    
    updateEditMenus ss
    return ()
    
onFileSaveAll :: Session -> IO ()    
onFileSaveAll ss = do   
    fileSaveAll ss
    updateSaveMenus ss    
    updateEditMenus ss
    return ()
   
-- File Open
onFileOpen :: Session -> IO ()
onFileOpen ss = do
    let mf = ssFrame ss                     -- wxFD_OPEN wxFD_FILE_MUST_EXIST
    fd <- fileDialogCreate mf "Open file" "." "" "*.hs" (Point 100 100) 0x11
    ans <- dialogShowModal fd
    if ans == wxID_OK
    then do
        fileDialogGetPath fd >>= fileOpen ss 
        updateSaveMenus ss    
        updateEditMenus ss        
        return ()
    else
        return ()

onFileNew :: Session -> IO ()
onFileNew ss = do
    enbAddNewFile ss (scnCallback ss) >>= enbSelectTab ss 
    updateSaveMenus ss    
    updateEditMenus ss    
    return ()
 
------------------------------------------------------------    
-- Edit Menu handlers
------------------------------------------------------------    

onEditUndo :: Session -> IO ()
onEditUndo ss = enbGetSelectedSourceFile ss >>= (scnUndo . sfEditor)

onEditRedo :: Session -> IO ()
onEditRedo ss = enbGetSelectedSourceFile ss >>= (scnRedo . sfEditor)

onEditCut :: Session -> IO ()
onEditCut ss = enbGetSelectedSourceFile ss >>= (scnCut . sfEditor)

onEditCopy :: Session -> IO ()
onEditCopy ss = enbGetSelectedSourceFile ss >>= (scnCopy . sfEditor)
    
onEditPaste :: Session -> IO ()
onEditPaste ss = enbGetSelectedSourceFile ss >>= (scnPaste . sfEditor)
  
onEditSelectAll :: Session -> IO ()
onEditSelectAll ss = enbGetSelectedSourceFile ss >>= (scnSelectAll . sfEditor)
  
onTestTest :: Session -> IO ()
onTestTest ss = do 
    p <- panel (ssFrame ss) [size := (Size 300 300)]
    h <- windowGetHandle p
    ghcOpenGhci h "parsermain.hs"
    return ()

onEditFind :: Session -> IO ()
onEditFind ss = do

    sf <- enbGetSelectedSourceFile ss
    let e = sfEditor sf
    sel <- scnGetSelText e
    s <- textDialog (ssFrame ss) "Find:" "HeyHo" sel

    if s /= "" then do

        pos <- scnGetCurrentPos e
        len <- scnGetTextLen e

        p <- findTextInRange e s pos (pos, len) (0, (pos + (length s) -1))

        if p >= 0 then do

            ssDebugInfo ss $ "Found at: " ++ (show p)

            -- save find string for next and prev
            atomically $ writeTVar (ssFindText ss) (ftFindText s (filepath sf) p (filepath sf) pos)
            return ()

        else do

            infoDialog (ssFrame ss) ssProgramTitle "Not found" 
            return ()

    else return ()

    where filepath sf =  maybe ("") id (sfFilePath sf)

onEditFindForward :: Session -> IO ()
onEditFindForward ss = do

    ft <- atomically $ readTVar (ssFindText ss)
    let s = (ftText ft)

    if (s /= "") then do

        sf <- enbGetSelectedSourceFile ss
        let e = sfEditor sf

        -- set search range from current pos to end of doc
        pos <- scnGetCurrentPos e
        len <- scnGetTextLen e

        p <- findTextInRange e s pos ((ftStartPos ft), len) (0, ((ftStartPos ft) + (length s) -1))

        if p >= 0 then do

            ssDebugInfo ss $ "Found at: " ++ (show p)

            -- save find string for next and prev
            atomically $ writeTVar (ssFindText ss) (ftFindText s (filepath sf) p (ftStartFile ft) (ftStartPos ft))
            return ()

        else do

            infoDialog (ssFrame ss) ssProgramTitle "No more ocurrences found" 
            atomically $ writeTVar (ssFindText ss) (ftFindText s (ftStartFile ft) (pos+1) (ftStartFile ft) (pos+1))
            return ()

    else return ()

    where filepath sf =  maybe ("") id (sfFilePath sf)

onEditFindBackward :: Session -> IO ()
onEditFindBackward ss = do

    ft <- atomically $ readTVar (ssFindText ss)
    let s = (ftText ft)

    if (s /= "") then do

        sf <- enbGetSelectedSourceFile ss
        let e = sfEditor sf

        -- set search range from current pos to end of doc
        pos <- scnGetCurrentPos e
        len <- scnGetTextLen e

        p <- findTextInRange e s pos (((ftStartPos ft) + (length s) -1), 0) ((len, ftStartPos ft))

        if p >= 0 then do

            ssDebugInfo ss $ "Found at: " ++ (show p)

            -- save find string for next and prev
            atomically $ writeTVar (ssFindText ss) (ftFindText s (filepath sf) p (ftStartFile ft) (ftStartPos ft))
            return ()

        else do

            infoDialog (ssFrame ss) ssProgramTitle "No more ocurrences found" 
            atomically $ writeTVar (ssFindText ss) (ftFindText s (ftStartFile ft) (pos+1) (ftStartFile ft) (pos+1))
            return ()

    else return ()

    where filepath sf =  maybe ("") id (sfFilePath sf)

-- editor -> find text -> last found position -> Range 1 -> Range 2
findTextInRange :: ScnEditor -> String -> Int -> (Int, Int) -> (Int, Int) -> IO Int
findTextInRange e s pos r1 r2 = do 

    if (inRange pos r1) then do

        scnSetTargetRange e (pos+1) (snd r1)
        p <- scnSearchInTarget e s

        if p >= 0 then gotoPos e p          
        else do

            scnSetTargetRange e (fst r2) (snd r2)
            p <- scnSearchInTarget e s

            if p >= 0 then gotoPos e p    
            else return (-1)

    else if (inRange pos r2) then do

        scnSetTargetRange e (pos+1) (snd r2)
        p <- scnSearchInTarget e s

        if p >= 0 then gotoPos e p          
        else return (-1)

    else return (-1)
    
    where 
        inRange a (b,c) = (a >= (min b c)) && (a <= (max b c))
        gotoPos e p = scnGotoPosWithScroll e p >> scnGrabFocus e >> return p


------------------------------------------------------------    
-- Build Menu handlers
------------------------------------------------------------    
    
onBuildBuild :: Session -> IO ()
onBuildBuild ss = do

    set (ssMenuListGet ss "BuildBuild")   [enabled := False]        
    set (ssMenuListGet ss "BuildCompile") [enabled := False]

    -- save file first
    sf <- enbGetSelectedSourceFile ss
    ans <- fileSave ss sf 
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

    -- save file first
    sf <- enbGetSelectedSourceFile ss
    ans <- fileSave ss sf 
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
    otAddText ss $ BS.pack "Compile complete\n"
    return ()


------------------------------------------------------------    
-- Build Menu handlers
------------------------------------------------------------    
    
onDebugRun :: Session -> IO ()
onDebugRun ss = do
    sf <- enbGetSelectedSourceFile ss
    case (sfFilePath sf) of
        Just fp -> cpDebugRun ss fp 
        Nothing -> warningDialog (ssFrame ss) ssProgramTitle "Source file has not been given a name" 
    return ()

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
            
------------------------------------------------------------    
-- 
------------------------------------------------------------    

closeEditor :: Session -> SourceFile -> IO Bool
closeEditor ss sf = do

    let e = sfEditor sf      
    ic <- scnIsClean e
    
    if ic then do
        
       -- save file
        b <- fileSave ss sf
        
        if b then do
        
            closeTab ss sf         
            return (True)

        -- veto close, don't know how to do this yet ??
        else return (False)         
        
    else do
    
        -- file is dirty so prompt the user if they want to save it
        enbSelectTab ss sf
        b <- confirmDialog (ssFrame ss) 
                "Heyho" 
                ("Do you wish to save the file: ?\n" ++ (show $ sfFilePathString sf))
                True
                
        if b then do
            
           -- save file
            b <- fileSave ss sf
            
            if b then do
            
                closeTab ss sf         
                return (True)

            -- veto close, don't know how to do this yet ??
            else return (False) 
    
        else do
            closeTab ss sf
            return (True)
          
closeTab :: Session -> SourceFile -> IO ()
closeTab ss sf = do
    
    -- close down scintilla editor
    let e = sfEditor sf
    scnDisableEvents e
    scnClose e
    
    -- remove source file from project
    prUpdate ss (\pr -> prCreate (findAndRemove (sfIsSame sf) (prFiles pr)))
    
    -- update status bar
    updateStatus ss
    
    return ()
    
fileCloseAll :: Session -> IO ()
fileCloseAll ss = do 
    ssReadSourceFiles ss >>= doWhileTrueIO (fileClose ss)
    updateSaveMenus ss    
    updateEditMenus ss
    return ()
       
fileClose :: Session -> SourceFile -> IO Bool
fileClose ss sf = do

    b <- closeEditor ss sf
    
    if b then do
    
        -- remove page from notebook
        mix <- enbGetTabIndex ss sf 
        case mix of
            Just ix -> do
                let nb = ssEditors ss        
                auiNotebookDeletePage nb ix  
                return (True)
            Nothing -> do
                ssDebugError ss "fileClose, no tab for source file"
                return (True)
        
    else return (False)
    
fileSaveAll :: Session -> IO (Bool)
fileSaveAll ss = do
        b <- ssReadSourceFiles ss >>= doWhileTrueIO (fileSave ss)
        return (b)

-- if file is dirty then writes it to file
-- if no filename has been set then file save as is called
-- returns false if user cancelled        
fileSave :: Session -> SourceFile -> IO Bool
fileSave ss sf = do

    let e = sfEditor sf      
    ic <- scnIsClean e
    
    if (ic) then do
        return (True)
    else       
        case (sfFilePath sf) of
            Just fp -> do
                writeSourceFile sf
                return (True)
            
            -- source file has no name, so prompt user for one
            Nothing -> do
                b <- fileSaveAs ss sf
                return (b)
                   
-- File Save As, returns False if user opted to cancel the save 
fileSaveAs :: Session -> SourceFile -> IO Bool
fileSaveAs ss sf = do
   
    -- ensure source file is displayed
    enbSelectTab ss sf
    
    -- prompt user for name to save to
    let mf = ssFrame ss                   -- wxFD_SAVE wxFD_OVERWRITE_PROMPT
    fd <- fileDialogCreate mf "Save file as" "." "" "*.hs" (Point 100 100) 0x6
    rs <- dialogShowModal fd 
    
    case rs of
--        wxID_OK -> do
-- ?? don't know how to fix pattern match against a function    
        5100 -> do    
            fp <- fileDialogGetPath fd
            
            -- save new name to mutable project data
            let sf' = sfSetFilePath sf fp
            sfUpdate ss sf'
            writeSourceFile sf'
            
            -- update tab name
            enbSetTabText ss sf'
            return (True)
            
        --wxID_CANCEL -> do
        5101 -> do
            return (False)
            
        otherwise  -> do
            return (True)
           
-- writes file to disk and sets editor to clean
writeSourceFile :: SourceFile -> IO ()
writeSourceFile sf = do
    let e = sfEditor sf
    case (sfFilePath sf) of
        Just fp -> do
            scnGetAllText e >>= BS.writeFile fp
            scnSetSavePoint e
            return ()
        
        Nothing -> do
            -- bug, shouldn't end up here
            return ()
            
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
                opGotoCompileError ss (fromIntegral (snLine sn) :: Int) (fileOpen ss)
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
                updateSaveMenus ss
                updateEditMenus ss
                return ()

            2003 -> do -- sCN_SAVEPOINTLEFT
                updateSaveMenus ss
                updateEditMenus ss
                return ()
                
            2007 -> do -- sCN_UPDATEUI
                updateStatus ss
                updateEditMenus ss
                return ()
            
            2013 -> return () -- sCN_PAINTED
              
            otherwise -> do
                -- ssDebugInfo ss $ "Event: " ++ (show $ scnNotifyGetCode sn)
                return ()
         

-----------------------------------------------------------------
-- Update menu status
-----------------------------------------------------------------
         
-- updates the enabled state of the Save, SaveAs and SaveAll menus                                
updateSaveMenus :: Session -> IO ()   
updateSaveMenus ss = do
 
    fs <- ssReadSourceFiles ss
    
    if (length fs > 0) then do
    
        ic <- enbSelectedSourceFileIsClean ss       
        set (ssMenuListGet ss "FileSave")      [enabled := not ic]        
        set (ssMenuListGet ss "FileSaveAs")    [enabled := True]       
        b <- allFilesClean fs
        set (ssMenuListGet ss "FileSaveAll")   [enabled := not b]
        set (ssMenuListGet ss "FileClose")     [enabled := True]
        set (ssMenuListGet ss "FileCloseAll")  [enabled := True]    
        return ()
        
    else do
    
        set (ssMenuListGet ss "FileSave")      [enabled := False]
        set (ssMenuListGet ss "FileSaveAs")    [enabled := False]
        set (ssMenuListGet ss "FileClose")     [enabled := False]
        set (ssMenuListGet ss "FileCloseAll")  [enabled := False]
        set (ssMenuListGet ss "FileSaveAll")   [enabled := False]
        return ()
       
    where allFilesClean fs = do
            b <- doWhileTrueIO (\sf -> scnIsClean $ sfEditor sf) fs
            return (b)
    
-- updates the enabled state of the Save, SaveAs and SaveAll menus                                
updateEditMenus :: Session -> IO ()   
updateEditMenus ss = do
 
    fs <- ssReadSourceFiles ss
        
    if (length fs > 0) then do
    
        sf <- enbGetSelectedSourceFile ss
        b <- scnSelectionIsEmpty $ sfEditor sf    
        -- ssDebugInfo ss $ "updateEditMenus: " ++ (show b) ++ " " ++ (sfToString sf)
        
        b <- scnCanUndo $ sfEditor sf
        set (ssMenuListGet ss "EditUndo")           [enabled := b]
        b <- scnCanRedo $ sfEditor sf
        set (ssMenuListGet ss "EditRedo")           [enabled := b]
        
        b <- scnSelectionIsEmpty $ sfEditor sf        
        set (ssMenuListGet ss "EditCut")            [enabled := not b]
        set (ssMenuListGet ss "EditCopy")           [enabled := not b]        
        b <- scnCanPaste $ sfEditor sf        
        set (ssMenuListGet ss "EditPaste")          [enabled := b]
        set (ssMenuListGet ss "EditAll")            [enabled := True]
        set (ssMenuListGet ss "EditFind")           [enabled := True]
        set (ssMenuListGet ss "EditFindForward")    [enabled := True]
        set (ssMenuListGet ss "EditFindBackward")   [enabled := True]
        return ()
        
    else do
    
        ssDebugError ss "updateEditMenus: no file"
        set (ssMenuListGet ss "EditUndo")           [enabled := False]
        set (ssMenuListGet ss "EditRedo")           [enabled := False]
        set (ssMenuListGet ss "EditCut")            [enabled := False]
        set (ssMenuListGet ss "EditCopy")           [enabled := False]
        set (ssMenuListGet ss "EditPaste")          [enabled := False]
        set (ssMenuListGet ss "EditAll")            [enabled := False]
        set (ssMenuListGet ss "EditFind")           [enabled := False]
        set (ssMenuListGet ss "EditFindForward")    [enabled := False]
        set (ssMenuListGet ss "EditFindBackward")   [enabled := False]
        return ()
 
-- display line count, cursor position etc. 
updateStatus :: Session -> IO ()   
updateStatus ss = do
    let st = ssStatus ss
    fs <- ssReadSourceFiles ss
    if (length fs > 0) then do
        sf <- enbGetSelectedSourceFile ss   
        (l, lp, dp, lc, cc) <- scnGetPositionInfo $ sfEditor sf
        set st [text:= (printf "Line: %d Col: %d, Lines: %d Pos: %d Size: %d" (l+1) (lp+1) lc dp cc)]
        return ()
    else do
        set st [text:= ""]
        return ()           
    
-----------------------------------------------------------------
-- Session management
-----------------------------------------------------------------

fileOpen :: Session -> String -> IO ()
fileOpen ss fp = do

    fs <- ssReadSourceFiles ss
    b <- ssIsOpeningState fs
   
    if b then do
    
        -- set 1st slot
        let sf' = sfSetFilePath (head fs) fp
        let nb = ssEditors ss
        writeSourceFileEditor sf'
        auiNotebookSetPageText nb 0 (takeFileName fp)
        prUpdate ss (\_ -> prCreate [sf'])
        return ()          
    
    else do
    
        if (sfIsInList fp fs) then do
            
            -- if already in file list then just switch focus to editor
            setSourceFileFocus ss fp
            return ()
            
         else do
         
            -- existing file so add to list, create window and set focus
            sf' <- openSourceFileEditor ss fp
            writeSourceFileEditor sf'
            return ()          

   
setSourceFileFocus :: Session -> String -> IO ()
setSourceFileFocus ss fp = do
    msf <- sfGetSourceFile ss fp
    case (msf) of
        Just sf -> do
            let nb = ssEditors ss
            ix <- auiNotebookGetPageIndex nb $ sfPanel sf
            auiNotebookSetSelection nb ix 
            return ()
        Nothing -> return ()

writeSourceFileEditor :: SourceFile -> IO ()
writeSourceFileEditor sf = do
    let mfp = sfFilePath sf
    case mfp of
        Just fp -> do
            text <- BS.readFile fp
            let e = sfEditor sf
            scnSetText e text
            scnSetSavePoint e
            return ()
        Nothing -> return () -- error

openSourceFileEditor :: Session -> String -> IO (SourceFile)
openSourceFileEditor ss fp = do

    let nb = ssEditors ss

    -- create panel with scintilla editor inside
    p <- panel nb []
    hwnd <- windowGetHandle p
    scn <- scnCreateEditor hwnd
    scnConfigureHaskell scn
    scn' <- scnSetEventHandler scn (scnCallback ss)
    scnEnableEvents scn'
    
    -- add panel to notebook
    auiNotebookAddPage nb p (takeFileName fp) False 0
    ta <- auiSimpleTabArtCreate
    auiNotebookSetArtProvider nb ta
      
    -- add source file to project
    sf <- sfCreate p scn' (Just fp)
    prUpdate ss (\pr -> prSetFiles pr (sf:(prFiles pr)))
          
    -- set focus to new page
    ix <- auiNotebookGetPageIndex nb p
    auiNotebookSetSelection nb ix  

    return (sf) 
  
