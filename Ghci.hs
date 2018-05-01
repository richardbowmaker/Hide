
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
    onDebugContinue,
    onDebugStep,
    onDebugStepLocal,
    onDebugStepModule,
    onDebugStop,
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
import qualified Data.ByteString.Unsafe as BS (unsafeUseAsCString)
import Control.Concurrent 
import Control.Concurrent.STM
import Control.Monad (foldM, mapM_, liftM, liftM2) 
import Data.Bits ((.&.), (.|.), testBit)
import Data.Int (Int32)
import Data.List (find, findIndex, intercalate)
import Data.Maybe (isJust)
import Data.Word (Word64)
import Foreign.C.String (CString, withCString, peekCString)
import Foreign.Ptr (FunPtr, Ptr, minusPtr, nullPtr)
import Graphics.UI.WX
import Graphics.UI.WXCore
import Graphics.Win32.GDI.Types (HWND)
import System.FilePath.Windows ((</>), takeFileName, takeDirectory)
import System.IO
import System.Win32.Types (nullHANDLE)

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
                            Just tw' -> startDebug ss tw' >> return() 
                            Nothing -> return ()
                    Nothing -> return ()
            Nothing -> do
                SS.ssDebugError ss "onBuildGhci:: no file name set"
    else return ()

onDebugStop :: SS.Session -> SS.TextWindow -> IO ()
onDebugStop ss tw = stopDebug ss

onDebugContinue :: SS.Session -> SS.TextWindow -> IO ()
onDebugContinue ss tw = continue ss >> return ()

onDebugStep :: SS.Session -> SS.TextWindow -> IO ()
onDebugStep ss tw = step ss >> return ()

onDebugStepLocal :: SS.Session -> SS.TextWindow -> IO ()
onDebugStepLocal ss tw = stepLocal ss >> return ()

onDebugStepModule :: SS.Session -> SS.TextWindow -> IO ()
onDebugStepModule ss tw = stepModule ss >> return ()

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
            sendCommand ss $ ":load *" ++ (maybe "" id (SS.twFilePath tw))
            return (Just tw)
        Nothing -> do
            -- GHCI not open so open a new tab
            case SS.twFilePath ftw of
                Just fp -> do
                    mw <- open ss fp                
                    case mw of
                        Just (panel, hwndp, hwnd) -> do
                                sendCommand ss $ ":load *" ++ fp
                                hw <- createHideWindow ss panel hwndp hwnd (Just fp)
                                SS.hwUpdate ss (\hws -> hw : hws)
                                setEventHandler ss hw hwnd SI.ghciTerminalEventMaskDebug
                                enableEvents hwnd
                                setFocus hwnd
                                return (Just $ SS.hwWindow hw)
                        Nothing -> return Nothing
                Nothing -> return Nothing

startDebug :: SS.Session -> SS.TextWindow -> IO Bool
startDebug ss tw = do
    case SS.twFilePath tw of
        Just fp -> do
            SS.dsUpdateDebugSession ss (\ds -> 
                SS.createDebugSession (Just tw) (takeDirectory fp) (SS.dsBreakPoints ds) Nothing)
            modules <- getModulesLookup ss
            -- mapM_ (addModule ss) modules
            setBreakPoints ss modules
            SS.ssSetStateBit ss SS.ssStateDebugging
            return True
        Nothing -> do
            SS.ssDebugError ss "Ghci.startDebug: no filename set"
            return False

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
                        (SS.createMenuFunction CN.menuFileClose         (closeWindow ss tw)                 (return True)),
                        (SS.createMenuFunction CN.menuFileCloseAll      (closeAll ss)                       (return True)),
                        (SS.createMenuFunction CN.menuFileSaveAs        (fileSaveAs ss tw)                  (return True)),
                        (SS.createMenuFunction CN.menuEditCut           (cut hwnd)                          (isTextSelected hwnd)),
                        (SS.createMenuFunction CN.menuEditCopy          (copy hwnd)                         (isTextSelected hwnd)),
                        (SS.createMenuFunction CN.menuEditPaste         (paste hwnd)                        (return True)),
                        (SS.createMenuFunction CN.menuEditSelectAll     (selectAll hwnd)                    (return True)),
                        (SS.createMenuFunction CN.menuEditClear         (clear hwnd)                        (return True)),
                        (SS.createMenuFunction CN.menuDebugStop         (onDebugStop ss tw)                 (debugging)),
                        (SS.createMenuFunction CN.menuDebugContinue     (onDebugContinue ss tw)             (debuggerPaused)),
                        (SS.createMenuFunction CN.menuDebugStep         (onDebugStep ss tw)                 (debuggerPaused)),
                        (SS.createMenuFunction CN.menuDebugStepLocal    (onDebugStepLocal ss tw)            (debuggerPaused)),
                        (SS.createMenuFunction CN.menuDebugStepModule   (onDebugStepModule ss tw)           (debuggerPaused))
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

stopDebug :: SS.Session -> IO ()
stopDebug ss = do
    sendCommand ss ":quit\n"
    clearDebugStoppedMarker ss
    SS.ssClearStateBit ss SS.ssStateDebugging
    SS.ssClearStateBit ss SS.ssStateRunning
    mhwnd <- SS.dsGetSessionHwnd ss
    case mhwnd of
        Just hwnd -> do
            SI.ghciTerminalClose hwnd
            SS.dsClearDebugSession ss
        Nothing -> SS.ssDebugError ss "Ghci.stopDebug: No GHCI session"

load :: SS.Session -> String -> IO Bool
load ss fp = do
    ms <- sendCommandSynch ss (":load *" ++ fp) "Main> " 30000
    case ms of
        Just _  -> return True
        Nothing -> return False

printVar :: SS.Session -> String -> IO String
printVar ss var = do
    ms <- sendCommandSynch ss (":print " ++ var) "Main> " 30000
    case ms of
        Just s  -> do
            let is = maybe 0 id (findIndex (== '=') s)
            let ie = maybe (length s) id (findIndex (== '\n') s)
            return $ take (ie - is) $ drop is s
        Nothing -> return "<variable not found>"

addModule :: SS.Session -> (String, String) -> IO Bool
addModule ss (_, mod) = do
    ms <- sendCommandSynch ss (":add *" ++ mod) "Main> " 30000
    case ms of
        Just _  -> return True
        Nothing -> return False

runMain :: SS.Session -> IO Bool
runMain ss = do
    SS.ssSetStateBit ss SS.ssStateRunning
    sendCommandAsynch ss "main\n" "Main> "
    return True

continue :: SS.Session -> IO Bool
continue ss = do
    clearDebugStoppedMarker ss
    SS.ssSetStateBit ss SS.ssStateRunning    
    sendCommandAsynch ss ":continue\n" "Main> "
    return True

step :: SS.Session -> IO Bool
step ss = do
    clearDebugStoppedMarker ss
    SS.ssSetStateBit ss SS.ssStateRunning
    sendCommandAsynch ss ":step\n" "Main> "
    return True

stepLocal :: SS.Session -> IO Bool
stepLocal ss = do
    clearDebugStoppedMarker ss
    SS.ssSetStateBit ss SS.ssStateRunning
    sendCommandAsynch ss ":steplocal\n" "Main> "
    return True

stepModule :: SS.Session -> IO Bool
stepModule ss = do
    clearDebugStoppedMarker ss
    SS.ssSetStateBit ss SS.ssStateRunning
    sendCommandAsynch ss ":stepmodule\n" "Main> "
    return True

deleteBreakPoints :: SS.Session -> IO Bool
deleteBreakPoints ss = do
    ms <- sendCommandSynch ss ":delete *" "Main> " 1000
    case ms of
        Just _  -> return True
        Nothing -> return False

getModulesLookup :: SS.Session -> IO ([(String, String)])
getModulesLookup ss = do
    ms <- sendCommandSynch ss ":show modules" "Main> " 1000 
    case ms of
        Just s -> return $ map getModuleLookup (lines s) 
        Nothing -> return [] 

getModuleLookup :: String -> (String, String)
getModuleLookup s = 
    let ts = words s in
    if (length ts > 2) then
        ((init . takeFileName) $ ts !! 2, ts !! 0)
    else
        ("", "")

setBreakPoints :: SS.Session -> [(String, String)] -> IO Bool
setBreakPoints ss modules = do
    bps <- SS.dsGetBreakPoints ss
    bps' <- foldM (\bps' bp -> do
        mno <- setBreakPoint ss bp modules
        case mno of
            Just no -> return $ (SS.dsSetBreakPointNo bp no) : bps'
            Nothing -> return $ bp : bps') [] bps
    if length bps == length bps' then do
        SS.dsSetBreakPoints ss bps'
        return True
    else
        return False
  
setBreakPoint :: SS.Session -> SS.DebugBreakPoint -> [(String, String)] -> IO (Maybe Int)
setBreakPoint ss bp modules = do
    l <- SC.markerLineFromHandle (SS.dsEditor bp) (SS.dsHandle bp)
    let mod = maybe "" id $ lookup (takeFileName $ SS.dsFilePath bp) modules
    ms <- sendCommandSynch ss  (":break " ++ mod ++ " " ++ (show (l+1)))  "Main> " 1000
    case ms of
        Just s -> do
            let ws = words s
            if ws !! 0 == "Breakpoint" then
                return $ Just (read (ws !! 1) :: Int)
            else 
                return Nothing
        Nothing -> return Nothing 

sendCommand :: SS.Session -> String -> IO ()
sendCommand ss cmd = do
    mhwnd <- SS.dsGetSessionHwnd ss
    case mhwnd of
        Just hwnd -> SI.ghciTerminalSendCommand hwnd cmd 
        Nothing -> SS.ssDebugError ss "Debugger.sendCommand: no GHCI session"

sendCommandAsynch :: SS.Session -> String -> String -> IO ()
sendCommandAsynch ss cmd eod = do
    mhwnd <- SS.dsGetSessionHwnd ss
    case mhwnd of
        Just hwnd -> SI.ghciTerminalSendCommandAsynch hwnd cmd "" eod
        Nothing -> SS.ssDebugError ss "Debugger.sendCommandAsynch: no GHCI session"
   
sendCommandSynch :: SS.Session -> String -> String -> Int -> IO (Maybe String)
sendCommandSynch ss cmd eod timeout = do
    mhwnd <- SS.dsGetSessionHwnd ss
    case mhwnd of
        Just hwnd -> do
            ms <- SI.ghciTerminalSendCommandSynch hwnd cmd eod timeout
            case ms of 
                Just s -> do
                    SS.ssDebugInfo ss $ "response to command: " ++ cmd
                    SS.ssDebugInfo ss s
                    return ms
                Nothing -> do
                    SS.ssDebugError ss $ "bad response to command: " ++ cmd
                    return Nothing
        Nothing -> do
            SS.ssDebugError ss "Debugger.sendCommandAsynch: no GHCI session"
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
                    SS.ssQueueFunction ss $ handleDebuggerOutput ss
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

handleDebuggerOutput :: SS.Session -> IO ()
handleDebuggerOutput ss = do
    ds <- SS.dsGetDebugSession ss
    case SS.dsOutput ds of
        Just dout -> do
            let filePath = (SS.dsDirectory ds) </> (takeFileName $ SS.doFilePath dout)
            (SS.ssFileOpen ss) ss filePath
        Nothing -> return ()
    setDebugStoppedMarker ss
    displayVariablesGrid ss

displayVariablesGrid :: SS.Session -> IO ()
displayVariablesGrid ss = do
    mdout <- SS.dsGetDebugOutput ss
    case mdout of
        Just dout -> do
            -- display free variables in grid 
            nr <- gridGetNumberRows grid
            if nr > 0 then gridDeleteRows grid 0 nr True
            else return False
            appendRows grid $ replicate (length $ SS.doVariables dout) ""
            prints <- mapM (\var -> printVar ss (SS.doVariable var)) (SS.doVariables dout)
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


    


