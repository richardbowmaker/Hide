
module Ghci
( 
    closeAll,
    tabClosing,
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
import qualified Menus as MN
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
        mftw <- SS.twFindWindow ss (\tw' -> SS.twMatchesHwnd tw' (SS.twPanelHwnd tw))
        case mftw of
            Just ftw -> do
                case SS.twFilePath ftw of
                    Just fp -> do
                        -- see if a GHCI window is already open with the same file as in ftw (scintilla editor)
                        mgtw <- SS.twFindWindow ss (\gtw -> (SS.twIsGhci gtw) && (SS.twIsSameFile ftw gtw)) 
                        case mgtw of
                            Just gtw -> do
                                -- GHCI already open so select it
                                let nb = SS.ssOutputs ss
                                auiNotebookGetPageIndex nb (SS.twPanel gtw) >>= auiNotebookSetSelection nb
                                -- reload the source file
                                load ss (SS.twHwnd gtw) fp
                                SS.twSetFocus ftw
                            Nothing -> do
                                mgtw <- openWindowFile ss ftw 
                                case mgtw of 
                                    Just gtw -> do
                                        initDebugger ss gtw
                                        SS.twSetFocus ftw 
                                    Nothing  -> SS.ssDebugError ss "Ghci.onDebugGhci:: GHCI window failed to open"
                    Nothing -> SS.ssDebugError ss "Ghci.onDebugGhci:: no file name set #1"
            Nothing -> SS.ssDebugError ss "Ghci.onDebugGhci:: no file name set #2"
    else return ()

openWindowFile :: SS.Session -> SS.TextWindow -> IO (Maybe SS.TextWindow)
openWindowFile ss ftw = do
    case SS.twFilePath ftw of
        Just fp -> do
            mw <- open ss fp                
            case mw of
                Just (panel, hwndp, hwnd) -> do
                    let tw = SS.createGhciTextWindow panel hwndp hwnd 
                            (Just fp) (setFocus hwnd) (hasFocus hwnd) (return True) (return "")
                    SS.twUpdate ss (\tws -> tw : tws)
                    let menus = createMenuHandlers ss tw (Just fp)
                    SS.ssSetMenuHandlers ss menus
                    setEventHandler ss tw hwnd SI.ghciTerminalEventMaskDebug
                    enableEvents hwnd
                    setFocus hwnd
                    return $ Just tw
                Nothing -> return Nothing
        Nothing -> return Nothing

initDebugger :: SS.Session -> SS.TextWindow -> IO ()
initDebugger ss tw = do
    case SS.twFilePath tw of
        Just fp -> do
            SS.dsUpdateDebugSession ss (\ds -> 
                SS.createDebugSession (Just tw) (takeDirectory fp) (SS.dsBreakPoints ds) Nothing)
            load ss (SS.twHwnd tw) fp
            startDebugger ss tw
        Nothing -> do
            SS.ssDebugError ss "Ghci.startDebug: no filename set"

startDebugger :: SS.Session -> SS.TextWindow -> IO ()
startDebugger ss tw = do
    SS.ssSetStateBit ss SS.ssStateDebugging
    SS.ssSetMenus ss
    SS.dsDebugOutputClear ss
    runDebugger ss tw (SS.twHwnd tw) $ SS.createDebugRecord SS.DbInitialising 0

stopDebugger :: SS.Session -> SS.TextWindow -> IO ()
stopDebugger ss tw = do
    clearDebugStoppedMarker ss
    SS.ssClearStateBit ss SS.ssStateDebugging
    SS.ssClearStateBit ss SS.ssStateRunning
    SS.dsClearDebugSession ss

-- on timer handler
runDebugger :: SS.Session -> SS.TextWindow -> HWND -> SS.DebugRecord -> IO ()
runDebugger ss tw hwnd dbr = do
    b <- SS.ssTestState ss SS.ssStateDebugging
    if b then do
        when (elapsed `mod` 1000 == 0) $ SS.ssDebugInfo ss $ "Debugger: " ++ show state
        -- dispatch to handler
        state' <- case state of 
            SS.DbInitialising -> 
                runDebuggerInitialising ss tw hwnd elapsed
            SS.DbBreakpoints -> 
                runDebuggerBreakpoints ss tw hwnd elapsed
            SS.DbPaused -> 
                runDebuggerPaused ss tw hwnd elapsed
            otherwise -> do
                SS.dsDebugOutputClear ss
                return state
        -- reset timer if state changed
        let tics' = if state == state' then (tics+1) else 0
        -- reschedule debugger
        case state' of 
            SS.DbFinished -> return ()
            otherwise -> SS.ssQueueFunction ss $ 
                runDebugger ss tw hwnd $ SS.createDebugRecord state' tics'
    else return () -- debugger stopped

    where   state = SS.dbState dbr
            tics = SS.dbTics dbr
            elapsed = CN.timerToMs tics

runDebuggerInitialising :: SS.Session -> SS.TextWindow -> HWND -> Int -> IO (SS.DebugState)
runDebuggerInitialising ss tw hwnd elapsed = do
    -- have we timedout waiting for GHCI to start
    if elapsed > 5000 then do
        infoDialog (SS.ssFrame ss) CN.programTitle "Debugger did not start"
        stopDebugger ss tw
        return SS.DbFinished
    else do
        -- get the GHCI output
        s <- SS.dsDebugOutputGet ss
        case MI.lastN (lines s) 2 of
            Just [l, _] -> do
                if MI.stringStartsWith l "Ok, modules loaded:" then do
                    SS.dsDebugOutputClear ss
                    return SS.DbBreakpoints                
                else return SS.DbInitialising
            Nothing -> return SS.DbInitialising

runDebuggerBreakpoints :: SS.Session -> SS.TextWindow -> HWND -> Int -> IO (SS.DebugState)
runDebuggerBreakpoints ss tw hwnd elapsed = do
    modules <- getModulesLookup ss hwnd
    -- mapM_ (addModule ss) modules
    setBreakPoints ss modules hwnd
    SS.dsDebugOutputClear ss
    return SS.DbPaused
    
runDebuggerPaused :: SS.Session -> SS.TextWindow -> HWND -> Int -> IO (SS.DebugState)
runDebuggerPaused ss tw hwnd elapsed = do
    s <- SS.dsDebugOutputGet ss
    case PR.parseDebuggerOutput s of
        Just dout -> do
            SS.dsSetDebugOutput ss dout
            handleDebuggerOutput ss hwnd
            -- SS.ssDebugInfo ss $ show dout
            SS.dsDebugOutputClear ss
            return ()
        Nothing -> return ()
    return SS.DbPaused

openWindow :: SS.Session -> IO (Maybe SS.TextWindow)
openWindow ss = do
    m <- open ss "" 
    case m of
        Just (panel, hwndp, hwnd) -> do
            let tw = SS.createGhciTextWindow panel hwndp hwnd 
                    Nothing (setFocus hwnd) (hasFocus hwnd) (return False) (return "")
            SS.twUpdate ss (\tws -> tw : tws)
            let menus = createMenuHandlers ss tw Nothing
            SS.ssSetMenuHandlers ss menus
            setEventHandler ss tw hwnd SI.ghciTerminalEventMaskDebug
            enableEvents hwnd
            setFocus hwnd
            return $ Just tw
        Nothing -> return Nothing

openDebugWindow :: SS.Session -> IO (Maybe SS.TextWindow)
openDebugWindow ss = do
    m <- open ss "" 
    case m of
        Just (panel, hwndp, hwnd) -> do
            let tw = SS.createGhciTextWindow panel hwndp hwnd 
                    Nothing (setFocus hwnd) (hasFocus hwnd) (return False) (return "")
            SS.twUpdate ss (\tws -> tw : tws)
            let menus = createMenuHandlers ss tw Nothing
            SS.ssSetMenuHandlers ss menus 
            setEventHandler ss tw hwnd SI.ghciTerminalEventMaskDebug
            enableEvents hwnd
            setFocus hwnd
            return $ Just tw
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

closeAll :: SS.Session -> IO ()
closeAll ss = SS.twFindWindows ss SS.twIsGhci >>= mapM_ (\tw -> closeWindow ss tw)

tabClosing :: SS.Session -> SS.TextWindow -> IO ()
tabClosing ss tw = do 
    stopDebugger ss tw
    SS.ssDisableMenuHandlers ss (SS.twHwnd tw)
    SI.ghciTerminalClose (SS.twHwnd tw)
    SS.twRemoveWindow ss tw
    return ()

closeWindow :: SS.Session -> SS.TextWindow -> IO ()
closeWindow ss tw = do

   -- remove page from notebook
    mix <- getTabIndex ss tw 
    case mix of
        Just ix -> do
            stopDebugger ss tw
            SS.ssDisableMenuHandlers ss (SS.twHwnd tw)
            SI.ghciTerminalClose (SS.twHwnd tw)
            let nb = SS.ssOutputs ss        
            auiNotebookDeletePage nb ix
            return ()
        Nothing -> do
            SS.ssDebugError ss "fileClose, no tab for source file"    

    SS.twRemoveWindow ss tw
    return ()

getTabIndex :: SS.Session -> SS.TextWindow -> IO (Maybe Int)
getTabIndex ss tw = do

    let nb = SS.ssOutputs ss
    pc <- auiNotebookGetPageCount nb

    -- get list of window handles as ints
    hs <- mapM (getHwnd nb) [0..(pc-1)]

    -- find tab with hwnd that matches the source file
    return (findIndex (\h -> SS.twMatchesHwnd tw h) hs)
    
    where getHwnd nb i = auiNotebookGetPage nb i >>= windowGetHandle

-- create the menu handlers
createMenuHandlers :: SS.Session -> SS.TextWindow -> Maybe String -> MN.HideMenuHandlers 
createMenuHandlers ss tw mfp = 
    [MN.createMenuHandler MN.menuFileClose         hwnd (closeWindow ss tw)     (return True),
     MN.createMenuHandler MN.menuFileSaveAs        hwnd (fileSaveAs ss tw)      (return True),
     MN.createMenuHandler MN.menuEditCut           hwnd (cut hwnd)              (isTextSelected hwnd),
     MN.createMenuHandler MN.menuEditCopy          hwnd (copy hwnd)             (isTextSelected hwnd),
     MN.createMenuHandler MN.menuEditPaste         hwnd (paste hwnd)            (return True),
     MN.createMenuHandler MN.menuEditSelectAll     hwnd (selectAll hwnd)        (return True),
     MN.createMenuHandler MN.menuEditClear         hwnd (clear hwnd)            (return True),
     MN.createMenuHandler MN.menuDebugDebug        hwnd (startDebugger ss tw)   (liftM not debugging),
     MN.createMenuHandler MN.menuDebugStop         hwnd (stopDebugger ss tw)    (debugging),
     MN.createMenuHandler MN.menuDebugContinue     hwnd (continue ss hwnd)      (return True),
     MN.createMenuHandler MN.menuDebugStep         hwnd (step ss hwnd)          (return True),
     MN.createMenuHandler MN.menuDebugStepLocal    hwnd (stepLocal ss hwnd)     (return True),
     MN.createMenuHandler MN.menuDebugStepModule   hwnd (stepModule ss hwnd)    (return True)]

    where 
        hwnd = SS.twHwnd tw
        debugging = SS.ssTestState ss SS.ssStateDebugging
        debuggerPaused = liftM2 (&&) debugging (liftM (not) $ SS.ssTestState ss SS.ssStateRunning)

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
    mfn <- SI.winSaveFileDialog 
            (SS.ssFrame ss) 
            "Save GHCI as" 
            (maybe "." takeDirectory $ SS.twFilePath tw) 
            "*.txt" 
            "Text file" 
            (maybe "" id $ SS.twFilePath tw)
            0x02 -- overwrite prompt
    case mfn of 
        Just fp -> do
            getAllText (SS.twPanelHwnd tw) >>= BS.writeFile fp
            -- save filename used
            SS.twFindAndSetFilePath ss tw (Just fp) 
            return ()
        Nothing -> return ()
          
setEventHandler :: SS.Session -> SS.TextWindow -> HWND -> Int -> IO ()
setEventHandler ss tw hwnd mask = SI.ghciTerminalSetEventHandler hwnd (eventHandler ss tw mask)

enableEvents :: HWND -> IO ()
enableEvents = SI.ghciTerminalEnableEvents

disableEvents :: HWND -> IO ()
disableEvents = SI.ghciTerminalDisableEvents
    
toggleBreakPoint :: SS.Session -> SS.TextWindow -> SC.Editor -> SC.SCNotification -> IO ()
toggleBreakPoint ss tw scn sn = do
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
        let bp = SS.createBreakPoint scn (maybe "" id $ SS.twFilePath tw) h 0 
        SS.dsAddBreakPoint ss bp
        traceBPs

    where
        traceBPs = do
            bps <- SS.dsGetBreakPoints ss
            SS.ssDebugInfo ss $ "Ghci.toggleBreakPoint" ++ intercalate "\n" (map show bps)

load :: SS.Session -> HWND -> String -> IO ()
load ss hwnd fp = do
    SS.ssSetStateBit ss SS.ssStateRunning
    sendCommand hwnd (":load *" ++ fp) 

printVar :: SS.Session -> HWND -> String -> IO String
printVar ss hwnd var = do
    ms <- sendCommandSynch ss hwnd (":print " ++ var) "*Main>" 30000
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
    sendCommandAsynch hwnd ":continue\n" ">"

step :: SS.Session -> HWND -> IO ()
step ss hwnd = do
    clearDebugStoppedMarker ss
    SS.ssSetStateBit ss SS.ssStateRunning
    sendCommandAsynch hwnd ":step\n" ">"

stepLocal :: SS.Session -> HWND -> IO ()
stepLocal ss hwnd = do
    clearDebugStoppedMarker ss
    SS.ssSetStateBit ss SS.ssStateRunning
    sendCommandAsynch hwnd ":steplocal\n" ">"

stepModule :: SS.Session -> HWND -> IO ()
stepModule ss hwnd = do
    clearDebugStoppedMarker ss
    SS.ssSetStateBit ss SS.ssStateRunning
    sendCommandAsynch hwnd ":stepmodule\n" ">"

deleteBreakPoints :: SS.Session -> HWND -> IO Bool
deleteBreakPoints ss hwnd = do
    ms <- sendCommandSynch ss hwnd ":delete *" "> " 1000
    case ms of
        Just _  -> return True
        Nothing -> return False

getModulesLookup :: SS.Session -> HWND -> IO ([(String, String)])
getModulesLookup ss hwnd = do
    ms <- sendCommandSynch ss hwnd ":show modules" "> " 1000 
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
    ms <- sendCommandSynch ss  hwnd (":break " ++ mod ++ " " ++ (show (l+1)))  ">" 1000
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
            mtw <- SS.twFindSourceFileWindow ss filePath
            case mtw of 
                Just tw -> do
                    case SS.twGetEditor tw of
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
            mtw <- SS.twFindSourceFileWindow ss filePath
            case mtw of 
                Just tw -> do
                    case SS.twGetEditor tw of
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

eventHandler :: SS.Session -> SS.TextWindow -> Int -> HWND -> Int -> Maybe String -> IO ()
eventHandler ss tw mask hwnd evt mstr
    | evt' == SI.ghciTerminalEventLostFocus = do
        SS.ssDisableMenuHandlers ss (SS.twHwnd tw)
    | evt' == SI.ghciTerminalEventGotFocus = do
        let mhs = createMenuHandlers ss tw Nothing
        SS.ssSetMenuHandlers ss mhs
    | evt' == SI.ghciTerminalEventSelectionSet || evt == SI.ghciTerminalEventSelectionClear = do
        SS.ssSetMenus ss        
    | evt' == SI.ghciTerminalEventOutput = do
        case mstr of
            Just str -> (SS.dsDebugOutputAppend ss str) >> return ()
            Nothing -> return ()
    | otherwise = return ()

    where   evt' = evt .&. mask
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
    -- displayVariablesGrid ss hwnd

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


    


