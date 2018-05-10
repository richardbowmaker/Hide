
module Ghci
( 
    closeAll,
    closeWindow,
    copy,
    createGrid,
    cut,
    disableEvents,
    enableEvents,
    getAllText,
    getTextLength,
    hasFocus,
    onDebugGhci,
    openDebugWindow,
    openWindow,
    openWindowFile,
    paste,
    selectAll,
    sendCommand,
    setEventHandler,
    setFocus,
    toggleBreakPoint
) where 

import qualified Data.ByteString as BS (init, replicate)
import qualified Data.ByteString.Char8 as BS (unpack, take, writeFile)
import qualified Data.ByteString.Internal as BS (ByteString)
import Control.Monad (foldM, liftM, liftM2) 
import Data.Bits ((.&.), (.|.), testBit)
import Data.Int (Int32)
import Data.List (findIndex, intercalate)
import Graphics.UI.WX
import Graphics.UI.WXCore
import Graphics.Win32.GDI.Types (HWND)
import System.FilePath.Windows ((</>), takeFileName, takeDirectory)


-- project imports

import qualified Constants as CN
import qualified Misc as MI
import qualified Parsers as PR
import qualified Scintilla as SC
import qualified ScintillaProxyImports as SI
import qualified Session as SS

onDebugGhci :: SS.Session -> SS.TextWindow -> SC.Editor -> IO ()
onDebugGhci ss tw scn = do
   -- save file first
    ans <- (SS.ssFileSave ss) ss tw scn
    if ans then do
        -- get again in case filename changed
        mhw <- SS.hwFindWindow ss (\hw -> SS.hwMatchesHwnd hw (SS.twPanelHwnd tw))
        case mhw of
            Just hw -> do
                case SS.hwFilePath hw of
                    Just fp -> do
                        mtw' <- openWindowFile ss $ SS.hwWindow hw 
                        case mtw' of 
                            Just tw' -> do
                                SS.ssQueueFunction ss $ startDebug ss tw (SS.twHwnd tw')
                                return() 
                            Nothing -> return ()
                    Nothing -> return ()
            Nothing -> do
                SS.ssDebugError ss "onBuildGhci:: no file name set"
    else return ()

openWindowFile :: SS.Session -> SS.TextWindow -> IO (Maybe SS.TextWindow)
openWindowFile ss ftw = do
    -- see if a GHCI window is already open with the same file as in ftw (scintilla editor)
    mtw <- SS.twFindWindow ss (\tw -> liftM2 (&&) (return $ SS.twIsGhci tw) (return $ SS.twIsSameFile ftw tw)) 
    case mtw of
        Just tw -> do
            -- GHCI already open so select it
            let nb = SS.ssOutputs ss
            auiNotebookGetPageIndex nb (SS.twPanel tw) >>= auiNotebookSetSelection nb
            -- reload the source file
            return (Just tw)
        Nothing -> do
            -- GHCI not open so open a new tab
            case SS.twFilePath ftw of
                Just fp -> do
                    mw <- open ss fp                
                    case mw of
                        Just (panel, hwndp, hwnd) -> do
                                hw <- createHideWindow ss panel hwndp hwnd (Just fp)
                                SS.hwUpdate ss (\hws -> hw : hws)
                                setEventHandler ss hw hwnd SI.ghciTerminalEventMaskDebug
                                enableEvents hwnd
                                setFocus hwnd
                                return (Just $ SS.hwWindow hw)
                        Nothing -> return Nothing
                Nothing -> return Nothing

startDebug :: SS.Session -> SS.TextWindow -> HWND -> IO ()
startDebug ss tw hwnd = do
    case SS.twFilePath tw of
        Just fp -> do
            SS.dsUpdateDebugSession ss (\ds -> 
                SS.createDebugSession (Just tw) (takeDirectory fp) (SS.dsBreakPoints ds) Nothing)
            load ss hwnd fp
            -- modules <- getModulesLookup ss hwnd
            -- mapM_ (addModule ss) modules
            -- setBreakPoints ss modules hwnd
            SS.ssSetStateBit ss SS.ssStateDebugging
        Nothing -> do
            SS.ssDebugError ss "Ghci.startDebug: no filename set"

openWindow :: SS.Session -> IO (Maybe SS.TextWindow)
openWindow ss = do
    m <- open ss "" 
    case m of
        Just (panel, hwndp, hwnd) -> do
            hw <- createHideWindow ss panel hwndp hwnd Nothing
            SS.hwUpdate ss (\hws -> hw : hws)
            setEventHandler ss hw hwnd SI.ghciTerminalEventMaskDebug
            enableEvents hwnd
            setFocus hwnd
            return (Just $ SS.hwWindow hw)
        Nothing -> return Nothing

openDebugWindow :: SS.Session -> IO (Maybe SS.TextWindow)
openDebugWindow ss = do
    m <- open ss "" 
    case m of
        Just (panel, hwndp, hwnd) -> do
            hw <- createHideWindow ss panel hwndp hwnd Nothing
            SS.hwUpdate ss (\hws -> hw : hws)
            setEventHandler ss hw hwnd SI.ghciTerminalEventMaskDebug
            enableEvents hwnd
            setFocus hwnd
            return (Just $ SS.hwWindow hw)
        Nothing -> return Nothing

open :: SS.Session -> String -> IO (Maybe (Panel (), HWND, HWND))
open ss fp = do

    -- create panel and embed GHCI window
    let nb = SS.ssOutputs ss
    p <- panel nb []
    hp <- windowGetHandle p 
    hwnd <- SI.ghciTerminalNew hp "-fasm -threaded" "" (takeDirectory fp) -- "-fasm -L. -lScintillaProxy -threaded"

    case (MI.ptrToWord64 hwnd) of

        0 -> return Nothing
        _ -> do

            -- add to outputs
            auiNotebookAddPage nb p ("GHCI " ++ (takeFileName fp)) False 0

            -- set focus to new page
            ix <- auiNotebookGetPageIndex nb p
            auiNotebookSetSelection nb ix 

            return (Just (p, hp, hwnd))
                
closeWindow :: SS.Session -> SS.TextWindow -> IO ()
closeWindow ss tw = do
    let nb = SS.ssOutputs ss
    p <- auiNotebookGetSelection nb >>= auiNotebookGetPage nb
    windowGetHandle p >>= SI.ghciTerminalClose
    SS.twRemoveWindow ss tw
    return ()

closeAll :: SS.Session -> IO ()
closeAll ss = SS.hwFindWindows ss SS.hwIsGhci >>= mapM_ (\hw -> closeWindow ss (SS.hwWindow hw))

createHideWindow :: SS.Session -> Panel() -> HWND -> HWND -> Maybe String -> IO SS.HideWindow
createHideWindow ss panel phwnd hwnd mfp = do
    let tw = SS.createTextWindow SS.createGhciWindowType panel phwnd hwnd mfp
    return $ SS.createHideWindow tw (tms tw)

    where  
        debugging = SS.ssTestState ss SS.ssStateDebugging
        debuggerPaused = liftM2 (&&) debugging (liftM (not) $ SS.ssTestState ss SS.ssStateRunning)
        tms tw = SS.createTextMenus 
                    [ 
                        (SS.createMenuFunction CN.menuFileClose         (closeWindow ss tw)     (return True)),
                        (SS.createMenuFunction CN.menuFileCloseAll      (closeAll ss)           (return True)),
                        (SS.createMenuFunction CN.menuFileSaveAs        (fileSaveAs ss tw)      (return True)),
                        (SS.createMenuFunction CN.menuEditCut           (cut hwnd)              (isTextSelected hwnd)),
                        (SS.createMenuFunction CN.menuEditCopy          (copy hwnd)             (isTextSelected hwnd)),
                        (SS.createMenuFunction CN.menuEditPaste         (paste hwnd)            (return True)),
                        (SS.createMenuFunction CN.menuEditSelectAll     (selectAll hwnd)        (return True)),
                        (SS.createMenuFunction CN.menuEditClear         (clear hwnd)            (return True)),
                        (SS.createMenuFunction CN.menuDebugStop         (stopDebug ss hwnd)     (debugging)),
                        (SS.createMenuFunction CN.menuDebugContinue     (continue ss hwnd)      (debuggerPaused)),
                        (SS.createMenuFunction CN.menuDebugStep         (step ss hwnd)          (debuggerPaused)),
                        (SS.createMenuFunction CN.menuDebugStepLocal    (stepLocal ss hwnd)     (debuggerPaused)),
                        (SS.createMenuFunction CN.menuDebugStepModule   (stepModule ss hwnd)    (debuggerPaused))
                    ]
                    (hasFocus hwnd)
                    (return True)
                    (return "")

paste :: HWND -> IO ()
paste = SI.ghciTerminalPaste

cut :: HWND -> IO ()
cut = SI.ghciTerminalCut

copy :: HWND -> IO ()
copy = SI.ghciTerminalCopy

selectAll :: HWND -> IO ()
selectAll = SI.ghciTerminalSelectAll

isTextSelected :: HWND -> IO Bool
isTextSelected = SI.ghciTerminalIsTextSelected

hasFocus :: HWND -> IO Bool
hasFocus = SI.ghciTerminalHasFocus

setFocus :: HWND -> IO ()
setFocus = SI.ghciTerminalSetFocus

getTextLength :: HWND -> IO Int
getTextLength = SI.ghciTerminalGetTextLength 

getAllText :: HWND -> IO BS.ByteString
getAllText = SI.ghciTerminalGetText           

clear :: HWND -> IO ()
clear = SI.ghciTerminalClear

-- File Save As, returns False if user opted to cancel the save 
fileSaveAs :: SS.Session -> SS.TextWindow -> IO ()
fileSaveAs ss tw = do 
    -- prompt user for name to save to
    fd <- fileDialogCreate 
        (SS.ssFrame ss)
        "Save GHCI as" 
        (maybe "." takeDirectory $ SS.twFilePath tw)
        (maybe "" id $ SS.twFilePath tw) 
        "*.txt" 
        (Point 100 100) 
        (wxSAVE .|. wxOVERWRITE_PROMPT)
    rs <- dialogShowModal fd  
    case rs of
--        wxID_OK -> do
        5100 -> do    
            fp <- fileDialogGetPath fd
            getAllText (SS.twPanelHwnd tw) >>= BS.writeFile fp
            -- save filename used
            SS.twFindAndSetFilePath ss tw (Just fp) 
            return ()  
        otherwise -> return ()
   
setEventHandler :: SS.Session -> SS.HideWindow -> HWND -> Int -> IO ()
setEventHandler ss hw hwnd mask = SI.ghciTerminalSetEventHandler hwnd (eventHandler ss hw mask)

enableEvents :: HWND -> IO ()
enableEvents = SI.ghciTerminalEnableEvents

disableEvents :: HWND -> IO ()
disableEvents = SI.ghciTerminalDisableEvents
    
toggleBreakPoint :: SS.Session -> SS.HideWindow -> SC.Editor -> SC.SCNotification -> IO ()
toggleBreakPoint ss hw scn sn = do
    l <- SC.getLineFromPosition scn (fromIntegral (SI.snPosition sn) :: Int)
    markers <- SC.markerGet scn l
    if testBit markers CN.breakPointMarker then do
        bps <- SS.dsGetBreakPoints ss
        -- remove breakpoint from session
        bps' <- MI.findAndRemoveIO (\bp -> do
            l' <- SC.markerLineFromHandle scn (SS.dsHandle bp)
            return $ l' == l) bps
        SS.dsUpdateBreakPoints ss (\_ -> bps')
        SC.markerDelete scn l CN.breakPointMarker
        traceBPs
    else do
        h <- SC.markerAdd scn l CN.breakPointMarker
        let bp = SS.createBreakPoint scn (maybe "" id $ SS.hwFilePath hw) h 0 
        SS.dsAddBreakPoint ss bp
        traceBPs

    where
        traceBPs = do
            bps <- SS.dsGetBreakPoints ss
            SS.ssDebugInfo ss $ "Ghci.toggleBreakPoint" ++ intercalate "\n" (map show bps)

stopDebug :: SS.Session -> HWND -> IO ()
stopDebug ss hwnd = do
    sendCommand hwnd ":quit\n"
    clearDebugStoppedMarker ss
    SS.ssClearStateBit ss SS.ssStateDebugging
    SS.ssClearStateBit ss SS.ssStateRunning
    SI.ghciTerminalClose hwnd
    SS.dsClearDebugSession ss

load :: SS.Session -> HWND -> String -> IO Bool
load ss hwnd fp = do
    ms <- sendCommandSynch ss hwnd (":load *" ++ fp) "Main> " 30000
    case ms of
        Just _  -> return True
        Nothing -> return False

printVar :: SS.Session -> HWND -> String -> IO String
printVar ss hwnd var = do
    ms <- sendCommandSynch ss hwnd (":print " ++ var) "Main> " 30000
    case ms of
        Just s  -> do
            let is = maybe 0 id (findIndex (== '=') s)
            let ie = maybe (length s) id (findIndex (== '\n') s)
            return $ take (ie - is) $ drop is s
        Nothing -> return "<variable not found>"

addModule :: SS.Session -> HWND -> (String, String) -> IO Bool
addModule ss hwnd (_, mod) = do
    ms <- sendCommandSynch ss hwnd (":add *" ++ mod) "Main> " 30000
    case ms of
        Just _  -> return True
        Nothing -> return False

runMain :: SS.Session -> HWND -> IO ()
runMain ss hwnd = do
    SS.ssSetStateBit ss SS.ssStateRunning
    sendCommandAsynch hwnd "main\n" "Main> "

continue :: SS.Session -> HWND -> IO ()
continue ss hwnd = do
    clearDebugStoppedMarker ss
    SS.ssSetStateBit ss SS.ssStateRunning    
    sendCommandAsynch hwnd ":continue\n" "Main> "

step :: SS.Session -> HWND -> IO ()
step ss hwnd = do
    clearDebugStoppedMarker ss
    SS.ssSetStateBit ss SS.ssStateRunning
    sendCommandAsynch hwnd ":step\n" "Main> "

stepLocal :: SS.Session -> HWND -> IO ()
stepLocal ss hwnd = do
    clearDebugStoppedMarker ss
    SS.ssSetStateBit ss SS.ssStateRunning
    sendCommandAsynch hwnd ":steplocal\n" "Main> "

stepModule :: SS.Session -> HWND -> IO ()
stepModule ss hwnd = do
    clearDebugStoppedMarker ss
    SS.ssSetStateBit ss SS.ssStateRunning
    sendCommandAsynch hwnd ":stepmodule\n" "Main> "

deleteBreakPoints :: SS.Session -> HWND -> IO Bool
deleteBreakPoints ss hwnd = do
    ms <- sendCommandSynch ss hwnd ":delete *" "Main> " 1000
    case ms of
        Just _  -> return True
        Nothing -> return False

getModulesLookup :: SS.Session -> HWND -> IO ([(String, String)])
getModulesLookup ss hwnd = do
    ms <- sendCommandSynch ss hwnd ":show modules" "Main> " 1000 
    case ms of
        Just s -> do 
            SS.ssDebugInfo ss $ "Modules: " ++ (intercalate "\n" (map tuptostr $ modules s))
            return $ modules s
        Nothing -> return []
    where modules s = map getModuleLookup (lines s)
          tuptostr (x,y) = show x ++ " = " ++ show y

getModuleLookup :: String -> (String, String)
getModuleLookup s = 
    let ts = words s in
    if (length ts > 2) then
        ((init . takeFileName) $ ts !! 2, ts !! 0)
    else
        ("", "")

setBreakPoints :: SS.Session -> [(String, String)] -> HWND -> IO Bool
setBreakPoints ss modules hwnd = do
    bps <- SS.dsGetBreakPoints ss
    bps' <- foldM (\bps' bp -> do
        mno <- setBreakPoint ss bp modules hwnd
        case mno of
            Just no -> return $ (SS.dsSetBreakPointNo bp no) : bps'
            Nothing -> return $ bp : bps') [] bps
    if length bps == length bps' then do
        SS.dsSetBreakPoints ss bps'
        return True
    else
        return False
  
setBreakPoint :: SS.Session -> SS.DebugBreakPoint -> [(String, String)] -> HWND -> IO (Maybe Int)
setBreakPoint ss bp modules hwnd = do
    l <- SC.markerLineFromHandle (SS.dsEditor bp) (SS.dsHandle bp)
    let mod = maybe "" id $ lookup (takeFileName $ SS.dsFilePath bp) modules
    ms <- sendCommandSynch ss  hwnd (":break " ++ mod ++ " " ++ (show (l+1)))  "Main> " 1000
    case ms of
        Just s -> do
            let ws = words s
            if ws !! 0 == "Breakpoint" then
                return $ Just (read (ws !! 1) :: Int)
            else 
                return Nothing
        Nothing -> return Nothing 

sendCommand :: HWND -> String -> IO ()
sendCommand hwnd cmd = SI.ghciTerminalSendCommand hwnd cmd 

sendCommandAsynch :: HWND -> String -> String -> IO ()
sendCommandAsynch hwnd cmd eod = SI.ghciTerminalSendCommandAsynch hwnd cmd "" eod
   
sendCommandSynch :: SS.Session -> HWND -> String -> String -> Int -> IO (Maybe String)
sendCommandSynch ss hwnd cmd eod timeout = do
    ms <- SI.ghciTerminalSendCommandSynch hwnd cmd eod timeout
    case ms of 
        Just s -> do
            SS.ssDebugInfo ss $ "response to command: " ++ cmd
            SS.ssDebugInfo ss s
            return ms
        Nothing -> do
            SS.ssDebugError ss $ "bad response to command: " ++ cmd
            return Nothing
    
clearDebugStoppedMarker :: SS.Session -> IO ()
clearDebugStoppedMarker ss = do
    ds <- SS.dsGetDebugSession ss
    case SS.dsOutput ds of
        Just dout -> do
            let filePath = (SS.dsDirectory ds) </> (takeFileName $ SS.doFilePath dout)
            mhw <- SS.hwFindSourceFileWindow ss filePath
            case mhw of 
                Just hw -> do
                    case SS.hwGetEditor hw of
                        Just scn -> do                            
                            SC.markerDeleteAll scn CN.debugMarker
                        Nothing -> return ()
                Nothing -> return ()
        Nothing -> return ()

setDebugStoppedMarker :: SS.Session -> IO ()
setDebugStoppedMarker ss = do
    ds <- SS.dsGetDebugSession ss
    case SS.dsOutput ds of
        Just dout -> do
            let (ls, le, cs, ce) = SS.doGetDebugRange dout
            let filePath = (SS.dsDirectory ds) </> (takeFileName $ SS.doFilePath dout)
            mhw <- SS.hwFindSourceFileWindow ss filePath
            case mhw of 
                Just hw -> do
                    case SS.hwGetEditor hw of
                        Just scn -> do
                            SC.markerAdd scn (ls-1) CN.debugMarker
                            SC.selectLinesCols scn (ls-1) (cs-1) (le-1) ce
                            SC.grabFocus scn
                        Nothing  -> return ()
                Nothing -> return ()
        Nothing -> return ()

------------------------------------------------------------    
-- Event Handling
------------------------------------------------------------  

eventHandler :: SS.Session -> SS.HideWindow -> Int -> HWND -> Int -> Maybe String -> IO ()
eventHandler ss hw mask hwnd evt mstr
    | evt' == SI.ghciTerminalEventLostFocus = do
        setm' ss CN.menuFileClose         (return False) (return ())
        setm' ss CN.menuFileCloseAll      (return False) (return ())
        setm' ss CN.menuFileSave          (return False) (return ())
        setm' ss CN.menuFileSaveAs        (return False) (return ())
        setm' ss CN.menuFileSaveAll       (return False) (return ())
        setm' ss CN.menuEditUndo          (return False) (return ())
        setm' ss CN.menuEditCut           (return False) (return ())
        setm' ss CN.menuEditCopy          (return False) (return ())
        setm' ss CN.menuEditPaste         (return False) (return ())
        setm' ss CN.menuEditSelectAll     (return False) (return ())
        setm' ss CN.menuEditFind          (return False) (return ())
        setm' ss CN.menuEditFindForward   (return False) (return ())
        setm' ss CN.menuEditFindBackward  (return False) (return ())
        setm' ss CN.menuEditClear         (return False) (return ())
        setm' ss CN.menuDebugStop         (return False) (return ())
        setm' ss CN.menuDebugContinue     (return False) (return ())
        setm' ss CN.menuDebugStep         (return False) (return ())
        setm' ss CN.menuDebugStepLocal    (return False) (return ())
        setm' ss CN.menuDebugStepModule   (return False) (return ())
    | evt' == SI.ghciTerminalEventGotFocus = do
        setm ss tms CN.menuFileClose        
        setm ss tms CN.menuFileCloseAll        
        setm ss tms CN.menuFileSave        
        setm ss tms CN.menuFileSaveAs        
        setm ss tms CN.menuFileSaveAll        
        setm ss tms CN.menuEditUndo          
        setm ss tms CN.menuEditRedo          
        setm ss tms CN.menuEditCut           
        setm ss tms CN.menuEditCopy          
        setm ss tms CN.menuEditPaste         
        setm ss tms CN.menuEditSelectAll     
        setm ss tms CN.menuEditFind          
        setm ss tms CN.menuEditFindForward   
        setm ss tms CN.menuEditFindBackward            
        setm ss tms CN.menuEditClear          
        setm ss tms CN.menuDebugStop   
        setm ss tms CN.menuDebugContinue   
        setm ss tms CN.menuDebugStep   
        setm ss tms CN.menuDebugStepLocal   
        setm ss tms CN.menuDebugStepModule   
    | evt' == SI.ghciTerminalEventSelectionSet || evt == SI.ghciTerminalEventSelectionClear = do
        setm ss tms CN.menuEditCut           
        setm ss tms CN.menuEditCopy          
        setm ss tms CN.menuEditPaste
    | evt' == SI.ghciTerminalEventAsynchOutput = do
        b <- SS.ssTestState ss SS.ssStateRunning
        if b then do
            case PR.parseDebuggerOutput str of
                Just dout -> do
                    SS.dsSetDebugOutput ss dout
                    SS.ssQueueFunction ss $ handleDebuggerOutput ss hwnd
                Nothing   -> SS.ssDebugError ss $ "Failed to parse debugger output:\n" ++ str
            SS.ssClearStateBit ss SS.ssStateRunning            
        else return () -- should send this to the output pane
    | otherwise = return ()

    where   setm :: SS.Session -> SS.TextMenus -> Int -> IO ()
            setm ss tw mid = setm' ss mid (SS.tmGetMenuEnabled tw mid) (SS.tmGetMenuFunction tw mid) 
            setm' :: SS.Session -> Int -> IO Bool -> IO () -> IO ()
            setm' ss mid me mf = do 
                e <- me
                set (SS.ssMenuListGet ss mid) [on command := mf, enabled := e]
            tms = SS.hwMenus hw
            evt' = evt .&. mask
            str = (maybe "" id mstr)

handleDebuggerOutput :: SS.Session -> HWND -> IO ()
handleDebuggerOutput ss hwnd = do
    ds <- SS.dsGetDebugSession ss
    case SS.dsOutput ds of
        Just dout -> do
            let filePath = (SS.dsDirectory ds) </> (takeFileName $ SS.doFilePath dout)
            (SS.ssFileOpen ss) ss filePath
        Nothing -> return ()
    setDebugStoppedMarker ss
    displayVariablesGrid ss hwnd

displayVariablesGrid :: SS.Session -> HWND -> IO ()
displayVariablesGrid ss hwnd = do
    mdout <- SS.dsGetDebugOutput ss
    case mdout of
        Just dout -> do
            -- display free variables in grid 
            nr <- gridGetNumberRows grid
            if nr > 0 then gridDeleteRows grid 0 nr True
            else return False
            appendRows grid $ replicate (length $ SS.doVariables dout) ""
            prints <- mapM (\var -> printVar ss hwnd (SS.doVariable var)) (SS.doVariables dout)
            mapM_ (\(row, var, print) -> 
                setRow grid (row, [(SS.doVariable var), (SS.doType var), print])) (zip3 [0..] (SS.doVariables dout) prints)
            return ()
        Nothing -> return ()

    where       
        grid = SS.ssDebugGrid ss

                    
------------------------------------------------------------    
-- Grid Control
------------------------------------------------------------    

createGrid :: Frame () -> IO (Grid ())
createGrid f = do
    -- grids
    g <- gridCtrl f []
    gridSetGridLineColour g (colorSystem Color3DFace)
    gridSetCellHighlightColour g black
    appendColumns g ["Variable", "Type", "Value"]
    gridSetRowLabelSize g 0
    gridSetColSize g 0 100
    gridSetColSize g 1 100
    gridSetColSize g 2 300     
    return g
    
gridCtrl :: Window a -> [Prop (Grid ())] -> IO (Grid ())
gridCtrl parent_ props_
  = feed2 props_ 0 $
    initialWindow $ \id_ rect' -> \props' flags ->
    do g <- gridCreate parent_ id_ rect' flags
       gridCreateGrid g 0 0 0
       set g props'
       return g

--wxTC_FIXEDWIDTH

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


    


