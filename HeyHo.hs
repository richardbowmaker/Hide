
module Main where

-- library imports
import Control.Concurrent 
import Control.Concurrent.STM
import qualified Control.Concurrent.Thread as Thread
import Control.Monad (liftM)
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
import Debug
import Misc
import Scintilla
import ScintillaConstants
import Session
   

   
main = start mainGUI

mainGUI :: IO ()
mainGUI = do 
  
    -- main window
    mf <- frame []    
    set mf [ text := "HeyHo", size := (Size 1300 800)]  
     
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
    enb <- createEditorNoteBook mf
   
    api <- auiPaneInfoCreateDefault
    auiPaneInfoCaption api "Editor"
    auiPaneInfoCentre api
    auiPaneInfoCloseButton api True
    auiPaneInfoMaximizeButton api True

    auiManagerAddPaneByPaneInfo am enb api
   
    -- add output pane
    (onb, oe) <- createNoteBook mf
    
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
    editorAddNewFile ss
    
    -- setup menu handlers
    set (ssMenuListGet ss "FileOpen")     [on command := onFileOpen      ss]
    set (ssMenuListGet ss "FileNew")      [on command := onFileNew       ss]
    set (ssMenuListGet ss "FileSave")     [on command := onFileSave      ss]
    set (ssMenuListGet ss "FileSaveAs")   [on command := onFileSaveAs    ss]
    set (ssMenuListGet ss "FileSaveAll")  [on command := onFileSaveAll   ss]
    set (ssMenuListGet ss "FileClose")    [on command := onFileClose     ss]
    set (ssMenuListGet ss "FileCloseAll") [on command := onFileCloseAll  ss]
    set (ssMenuListGet ss "EditUndo")     [on command := onEditUndo      ss]
    set (ssMenuListGet ss "EditRedo")     [on command := onEditRedo      ss]
    set (ssMenuListGet ss "EditCut")      [on command := onEditCut       ss]
    set (ssMenuListGet ss "EditCopy")     [on command := onEditCopy      ss]
    set (ssMenuListGet ss "EditPaste")    [on command := onEditPaste     ss]
    set (ssMenuListGet ss "EditAll")      [on command := onEditSelectAll ss]
    set (ssMenuListGet ss "BuildBuild")   [on command := onBuildBuild    ss]
    set (ssMenuListGet ss "BuildCompile") [on command := onBuildCompile  ss]
    set (ssMenuListGet ss "TestTest")     [on command := onTestTest      ss]
    
    set enb [on auiNotebookOnPageCloseEvent   := onTabClose   ss]
    set enb [on auiNotebookOnPageChangedEvent := onTabChanged ss]
   
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

    menuEdit         <- menuPane            [text := "&Edit"]
    menuEditUndo     <- menuItem menuEdit   [text := "Undo\tCtrl-Z"]
    menuEditRedo     <- menuItem menuEdit   [text := "Redo\tCtrl-Y"]
    menuAppendSeparator menuEdit
    menuEditCut      <- menuItem menuEdit   [text := "Cut\tCtrl-X"]
    menuEditCopy     <- menuItem menuEdit   [text := "Copy\tCtrl-C"]
    menuEditPaste    <- menuItem menuEdit   [text := "Paste\tCtrl-V"]
    menuEditAll      <- menuItem menuEdit   [text := "Select All\tCtrl-A"]
    
    menuBuild        <- menuPane            [text := "Build"]
    menuBuildCompile <- menuItem menuBuild  [text := "Compile\tCtrl-F7",       help := "Compiles current source file"]
    menuBuildBuild   <- menuItem menuBuild  [text := "Build\tF7",              help := "Build the project"]
    menuBuildReBuild <- menuItem menuBuild  [text := "Rebuild\tCtrl-Alt-F7",   help := "Rebuild the project"]
    menuBuildClean   <- menuItem menuBuild  [text := "Clean",                  help := "Clean the project"]
          
    menuTest         <- menuPane            [text := "Test"]
    menuTestTest     <- menuItem menuTest   [text := "Test\tCtrl-T"]

    -- create Help menu
    menuHelp'        <- menuHelp []
    menuHelpAbout    <- menuAbout menuHelp' [help := "About HeyHo", on command := infoDialog mf "About HeyHo" "mmmmm !"]
      
    set mf [ menuBar := [menuFile, menuEdit, menuBuild, menuTest, menuHelp']]

    -- create lookup list of menus for session data   
    ml <- ssMenuListCreate [    ("FileOpen",        menuFileOpen), 
                                ("FileSave",        menuFileSave), 
                                ("FileNew",         menuFileNew), 
                                ("FileClose",       menuFileClose), 
                                ("FileCloseAll",    menuFileCloseAll), 
                                ("FileSaveAs",      menuFileSaveAs), 
                                ("FileSaveAll",     menuFileSaveAll),
                                ("EditUndo",        menuEditUndo),
                                ("EditRedo",        menuEditRedo),
                                ("EditCut",         menuEditCut),
                                ("EditCopy",        menuEditCopy),
                                ("EditPaste",       menuEditPaste),
                                ("EditAll",         menuEditAll),
                                ("BuildBuild",      menuBuildBuild),
                                ("BuildCompile",    menuBuildCompile),
                                ("TestTest",        menuTestTest)]

    
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
    let mf = ssFrame ss
    let am = ssAuiMgr ss
    fileCloseAll ss
    auiManagerUnInit am
    windowDestroy mf
    return ()

onTabChanged :: Session -> EventAuiNotebook -> IO ()
onTabChanged ss ev@(AuiNotebookPageChanged _ _) = do
    updateSaveMenus ss
    updateEditMenus ss
    updateStatus ss
    return ()    

onTabClose :: Session -> EventAuiNotebook -> IO ()
onTabClose ss _ = do
    sf <- enbGetSelectedSourceFile ss
    closeEditor ss sf   
    updateSaveMenus ss      
    updateEditMenus ss
    return ()
    
onFileClose :: Session -> IO ()
onFileClose ss = do
    sf <- enbGetSelectedSourceFile ss
    fileClose ss sf
    updateSaveMenus ss    
    updateEditMenus ss
    return ()
  
onFileCloseAll :: Session -> IO ()
onFileCloseAll = fileCloseAll

onFileSave :: Session -> IO ()
onFileSave ss = do
    sf <- enbGetSelectedSourceFile ss
    fileSave ss sf
    updateSaveMenus ss    
    updateEditMenus ss
    return ()

onFileSaveAs :: Session -> IO ()
onFileSaveAs ss = do
    sf <- enbGetSelectedSourceFile ss
    fileSaveAs ss sf
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
        fp <- fileDialogGetPath fd
        fileOpen ss fp
        updateSaveMenus ss    
        updateEditMenus ss        
        return ()
    else
        return ()

onFileNew :: Session -> IO ()
onFileNew ss = do
    sf <- editorAddNewFile ss
    enbSelectTab ss sf
    updateSaveMenus ss    
    updateEditMenus ss    
    return ()
 
------------------------------------------------------------    
-- Edit Menu handlers
------------------------------------------------------------    

onEditUndo :: Session -> IO ()
onEditUndo ss = do
    sf <- enbGetSelectedSourceFile ss
    scnUndo $ sfEditor sf
    return ()

onEditRedo :: Session -> IO ()
onEditRedo ss = do
    sf <- enbGetSelectedSourceFile ss
    scnRedo $ sfEditor sf
    return ()
 
onEditCut :: Session -> IO ()
onEditCut ss = do
    sf <- enbGetSelectedSourceFile ss
    scnCut $ sfEditor sf
    return ()

onEditCopy :: Session -> IO ()
onEditCopy ss = do
    sf <- enbGetSelectedSourceFile ss
    scnCopy $ sfEditor sf
    return ()
    
onEditPaste :: Session -> IO ()
onEditPaste ss = do
    sf <- enbGetSelectedSourceFile ss
    scnPaste $ sfEditor sf
    return ()
  
onEditSelectAll :: Session -> IO ()
onEditSelectAll ss = do
    sf <- enbGetSelectedSourceFile ss
    scnSelectAll $ sfEditor sf
    return ()
  
onTestTest :: Session -> IO ()
onTestTest ss = do
    otClear ss
    otAddLine ss $ BS.pack "line 1"
    otAddLine ss $ BS.pack "line 2"
    otAddLine ss $ BS.pack "line 3"
    return ()

------------------------------------------------------------    
-- Build Menu handlers
------------------------------------------------------------    
    
onBuildBuild :: Session -> IO ()
onBuildBuild ss = do

    otClear ss
    otAddLine ss $ BS.pack "Compile started ..."
    set (ssMenuListGet ss "BuildBuild")   [enabled := False]        
    set (ssMenuListGet ss "BuildCompile") [enabled := False]

    forkIO $ runExtCmd         
        "D:\\_Rick's\\haskell\\HeyHo\\build.bat" 
        ["heyho"] 
        "D:\\_Rick's\\haskell\\HeyHo" 
        (ssTOutput ss)
        (ssTOutput ss)
        (ssCFunc ss)
        (Just $ compileComplete ss)
        
    return ()
    
onBuildCompile :: Session -> IO ()
onBuildCompile ss = do

    otClear ss
    otAddLine ss $ BS.pack "Compile started ..."
    set (ssMenuListGet ss "BuildBuild")   [enabled := False]        
    set (ssMenuListGet ss "BuildCompile") [enabled := False]

    -- save file first
    sf <- enbGetSelectedSourceFile ss
    ans <- fileSave ss sf
    if ans then do
            -- get again in case filename changed
            sf <- enbGetSelectedSourceFile ss
            case (sfFilePath sf) of
                Just fp -> do
                    forkIO $ runExtCmd 
                        "C:\\Program Files\\Haskell Platform\\8.0.1\\bin\\ghc" ["-c", fp] 
                        "D:\\_Rick's\\haskell\\HeyHo"
                        (ssTOutput ss) -- stdout goes to TOutput
                        (ssTOutput ss)
                        (ssCFunc ss)
                        (Just $ compileComplete ss)
                    return ()
                Nothing -> return ()
                
    else return ()
               
compileComplete :: Session -> IO ()
compileComplete ss = do
    set (ssMenuListGet ss "BuildBuild")   [enabled := True]        
    set (ssMenuListGet ss "BuildCompile") [enabled := True]
    otAddText ss $ BS.pack "\nCompile complete\n"
    return ()

-- run command and redirect std out to the output pane
-- command -> arguments -> working directory -> stdout TChan -> stderr TChan -> completion function
runExtCmd :: String -> [String] -> String -> TOutput -> TOutput -> FunctionChannel -> Maybe (IO ()) -> IO ()
runExtCmd cmd args dir cout cerr cfn mfinally = do    
    (_, Just hout, Just herr, ph) <- createProcess_ "errors" (proc cmd args)
        {cwd = Just dir, std_out = CreatePipe, std_err = CreatePipe}

    (_, waits) <- Thread.forkIO $ streamToChan hout cout
    (_, waite) <- Thread.forkIO $ streamToChan herr cerr
    
    Thread.result =<< waits
    Thread.result =<< waite
    waitForProcess ph
    
    -- schedule finally function to be called in gui thread
    maybe (return ()) (\f -> atomically $ writeTChan cfn f) mfinally
   
    where
    
        streamToChan h tot = whileM_ (liftM not $ hIsEOF h) (BS.hGetLine h >>= (\s -> atomically $ writeTChan tot s)) 
        
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
            whileM_ 
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
    sfs <- ssReadSourceFiles ss  
    doWhileTrueIO (fileClose ss) sfs
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
                debugOut ss "*** error: fileClose, no tab for source file"
                return (True)
        
    else return (False)
    
fileSaveAll :: Session -> IO (Bool)
fileSaveAll ss = do

        sfs <- ssReadSourceFiles ss
        b <- doWhileTrueIO (fileSave ss) sfs
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
            bs <- scnGetAllText e
            BS.writeFile fp bs
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
            debugOut ss $ "Event: " ++ (show $ scnNotifyGetCode sn)
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
--        debugOut ss $ "updateEditMenus: " ++ (show b) ++ " " ++ (sfToString sf)
        
        b <- scnCanUndo $ sfEditor sf
        set (ssMenuListGet ss "EditUndo")    [enabled := b]
        b <- scnCanRedo $ sfEditor sf
        set (ssMenuListGet ss "EditRedo")    [enabled := b]
        
        b <- scnSelectionIsEmpty $ sfEditor sf        
        set (ssMenuListGet ss "EditCut")     [enabled := not b]
        set (ssMenuListGet ss "EditCopy")    [enabled := not b]        
        b <- scnCanPaste $ sfEditor sf        
        set (ssMenuListGet ss "EditPaste")   [enabled := b]
        set (ssMenuListGet ss "EditAll")     [enabled := True]
        return ()
        
    else do
    
        debugOut ss "updateEditMenus: no file"
        set (ssMenuListGet ss "EditUndo")    [enabled := False]
        set (ssMenuListGet ss "EditRedo")    [enabled := False]
        set (ssMenuListGet ss "EditCut")     [enabled := False]
        set (ssMenuListGet ss "EditCopy")    [enabled := False]
        set (ssMenuListGet ss "EditPaste")   [enabled := False]
        set (ssMenuListGet ss "EditAll")     [enabled := False]
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
    msf <- getSourceFile ss fp
    case (msf) of
        Just sf -> do
            let nb = ssEditors ss
            ix <- auiNotebookGetPageIndex nb $ sfPanel sf
            auiNotebookSetSelection nb ix 
            return ()
        Nothing -> return ()

getSourceFile :: Session -> String -> IO (Maybe SourceFile)
getSourceFile ss fp = do 
    fs <- ssReadSourceFiles ss
    let sf = find (\sf -> sfPathIs sf (Just fp)) fs
    return (sf)

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
  
------------------------------------------------------------    
-- Create the source file editor notebook
------------------------------------------------------------    

createEditorNoteBook :: Frame mf -> IO (AuiNotebook ())
createEditorNoteBook mf = do

    -- create the notebook
    nb <- auiNotebookCreate mf idAny (Point 0 0) (Size 0 0) (wxCLIP_CHILDREN + wxAUI_NB_TOP + wxAUI_NB_CLOSE_ON_ACTIVE_TAB)
    return (nb)
  
editorAddNewFile :: Session -> IO (SourceFile)  
editorAddNewFile ss = do
    
    let nb = ssEditors ss

    -- create panel with scintilla editor inside
    p <- panel nb []
    hwnd <- windowGetHandle p
    scn <- scnCreateEditor hwnd
    scnConfigureHaskell scn

    -- add panel to notebook
    auiNotebookAddPage nb p "..." False 0
    ta <- auiSimpleTabArtCreate
    auiNotebookSetArtProvider nb ta

    sf <- sfCreate p scn Nothing

    -- enable events
    scn' <- scnSetEventHandler scn (scnCallback ss)
    scnEnableEvents scn'

    -- update mutable project
    prUpdate ss (\pr -> prSetFiles pr (sf:(prFiles pr)))
    
    scnSetSavePoint scn'
    
    return (sf)
    
------------------------------------------------------------    
-- Editor notebook helpers
------------------------------------------------------------    
    
-- returns the HWND of the child panel of the currently selected notebook page
enbGetSelectedTabHwnd :: Session -> IO Word64
enbGetSelectedTabHwnd ss = do
    let nb = ssEditors ss
    ix <- auiNotebookGetSelection nb
    p <- auiNotebookGetPage nb ix
    hp <- windowGetHandle p
    return (ptrToWord64 hp)
 
-- returns the source file for the currently selected tab
enbGetSelectedSourceFile :: Session -> IO SourceFile
enbGetSelectedSourceFile ss = do  
    fs <- ssReadSourceFiles ss
    hp <- enbGetSelectedTabHwnd ss
   
    case (find (\sf -> sfMatchesHwnd sf hp) fs) of
        Just sf -> 
            return (sf)
        Nothing -> do 
            -- should not occur, like this to simplfy calling code
            debugOut ss "*** error: enbGetSelectedSourceFile no source file for current tab"
            return (head fs) 
                  
-- returns true if the source file of the currently selected tab is clean 
enbSelectedSourceFileIsClean :: Session -> IO Bool
enbSelectedSourceFileIsClean ss = do
    sf <- enbGetSelectedSourceFile ss
    ic <- scnIsClean $ sfEditor sf
    return (ic)
    
enbSelectTab :: Session -> SourceFile -> IO ()
enbSelectTab ss sf = do
    let nb = ssEditors ss
    mix <- enbGetTabIndex ss sf
    case mix of
        Just ix -> do
            auiNotebookSetSelection nb ix
            return ()            
        Nothing -> return ()
    return ()

enbCloseTab :: Session -> SourceFile -> IO ()
enbCloseTab ss sf = do
    let nb = ssEditors ss
    mix <- enbGetTabIndex ss sf
    case (mix) of
        Just ix -> do
            auiNotebookSetSelection nb ix
            auiNotebookRemovePage nb ix
            return ()
        Nothing -> do
            debugOut ss "*** error: enbCloseTab, source file not in tabs"
            return ()

enbGetTabIndex :: Session -> SourceFile -> IO (Maybe Int)
enbGetTabIndex ss sf = do

    let nb = ssEditors ss
    pc <- auiNotebookGetPageCount nb

    -- get list of window handles as ints
    hs <- mapM (getHwnd nb) [0..(pc-1)]

    -- find tab with hwnd that matches the source file
    return (findIndex (\h -> sfMatchesHwnd sf h) hs)
    
    where getHwnd nb i = do
            w <- auiNotebookGetPage nb i
            h <- windowGetHandle w
            return (ptrToWord64 h)
 
enbSetTabText :: Session -> SourceFile -> IO ()
enbSetTabText ss sf = do
    mix <- enbGetTabIndex ss sf
    case mix of
        Just ix -> do
            case (sfFilePath sf) of
                Just fp -> do
                    auiNotebookSetPageText (ssEditors ss) ix $ takeFileName fp
                    return ()
                Nothing -> return ()                   
        Nothing -> return ()
    return ()
 
enbGetTabCount :: Session -> IO Int
enbGetTabCount ss = do
    pc <- auiNotebookGetPageCount $ ssEditors ss
    return (pc)

------------------------------------------------------------    
-- Create output pane
------------------------------------------------------------    

createNoteBook :: Frame () -> IO (AuiNotebook (), ScnEditor)
createNoteBook f = do

    nb <- auiNotebookCreate f idAny (Point 0 0) (Size 0 0) (wxCLIP_CHILDREN + wxAUI_NB_TOP)
    set nb [] 
    p <- panel nb []
    hwnd <- windowGetHandle p
    e <- scnCreateEditor hwnd
    auiNotebookAddPage nb p "Output" False 0
    ta <- auiSimpleTabArtCreate
    auiNotebookSetArtProvider nb ta
    
    -- configure editor
    scnSetLexer e (fromIntegral sCLEX_CONTAINER :: Int)
    scnSetAStyle e (fromIntegral sTYLE_DEFAULT :: Word64) scnBlack scnWhite 9 "Courier New"
    scnStyleClearAll e
    scnSetAStyle e (fromIntegral sCE_H_DEFAULT :: Word64) scnBlack scnWhite 9 "Courier New"
    scnSetReadOnly e True
 
    return (nb, e)

------------------------------------------------------------    
-- Tree Control
------------------------------------------------------------    
    
createTree :: Frame () ->  IO (TreeCtrl ())
createTree f = do      
    tree <- treeCtrl f [size := (Size 100 100)] 
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
    
    

