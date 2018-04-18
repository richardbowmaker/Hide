
module Debugger
( 
    appendColumns,
    appendRows,
    createGrid, 
    onDebugContinue,
    onDebugDebug,
    onDebugStep,
    onDebugStepLocal,
    onDebugStepModule,
    onDebugStop,
    toggleBreakPoint
) where 
 
-- library imports 
import Control.Concurrent (threadDelay)
import Control.Exception
import Control.Monad (foldM)
import Data.Bits (testBit)
import Data.List (findIndex, intercalate, lines, lookup, words)
import Graphics.Win32.GDI.Types (HWND)
import Graphics.UI.WX 
import Graphics.UI.WXCore
import Graphics.UI.WXCore.Dialogs (infoDialog)
import System.Directory (removeFile)
import System.FilePath.Windows ((</>), takeFileName, takeDirectory)
import qualified System.FilePath.Windows as Win (dropExtension)

-- project imports
import qualified Constants as CN
import qualified Ghci as GH
import qualified Misc as MI
import qualified Parsers as PR
import qualified Scintilla as SC
import qualified ScintillaProxyImports as SI
import qualified Session as SS

onDebugDebug :: SS.Session -> SS.TextWindow -> (String -> IO ()) -> IO ()
onDebugDebug ss tw fileopen = do
    mfp <- SS.twFilePath tw
    case mfp of
        Just fp -> do
            id <- SI.ghciNew "-fasm -L. -lScintillaProxy -threaded" "" (takeDirectory fp)
            ms <- SI.ghciWaitForResponse id "Prelude> " 10000
            case ms of 
                Just s -> do
                    SS.ssDebugInfo ss s
                    SS.dsUpdateDebugSession ss (\ds -> 
                        SS.createDebugSession id (takeDirectory fp) (SS.dsBreakPoints ds) Nothing)
                    startDebug ss id fp fileopen
                    return ()
                Nothing -> SS.ssDebugError ss "GHCI didn't start"
        Nothing -> return ()

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
        mfp <- SS.hwFilePath hw
        let bp = SS.createBreakPoint scn (maybe "" id mfp) h 0 
        SS.dsAddBreakPoint ss bp
        traceBPs

    where
        traceBPs = do
            bps <- SS.dsGetBreakPoints ss
            SS.ssDebugInfo ss $ intercalate "\n" (map show bps)
       
startDebug :: SS.Session -> Int -> String -> (String -> IO ()) -> IO Bool
startDebug ss id fp fileopen = do
    -- delete object file, to force GHCi to run in interpretative mode
    -- result <- try (removeFile $ (Win.dropExtension fp) ++ ".o")  :: IO (Either IOException ())
    SI.ghciSetEventHandler id $ eventHandler ss fileopen
    load ss fp
    modules <- getModulesLookup ss
    -- mapM_ (addModule ss) modules
    setBreakPoints ss modules
    runMain ss
    SS.ssSetStateBit ss SS.ssStateDebugging
    return True

stopDebug :: SS.Session -> IO ()
stopDebug ss = do
    sendCommand ss ":quit\n"
    clearDebugStoppedMarker ss
    SS.ssClearStateBit ss SS.ssStateDebugging
    SS.ssClearStateBit ss SS.ssStateRunning
    id <- SS.dsGetSessionId ss
    SI.ghciClose id
    SS.dsSetSessionId ss 0

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
    id <- SS.dsGetSessionId ss
    SI.ghciSendCommand id cmd 

sendCommandAsynch :: SS.Session -> String -> String -> IO ()
sendCommandAsynch ss cmd eod = do
    id <- SS.dsGetSessionId ss
    SI.ghciSendCommandAsynch id cmd eod
    
sendCommandSynch :: SS.Session -> String -> String -> Int -> IO (Maybe String)
sendCommandSynch ss cmd eod timeout = do
    id <- SS.dsGetSessionId ss
    ms <- SI.ghciSendCommandSynch id cmd eod timeout
    case ms of 
        Just s -> do
            SS.ssDebugInfo ss $ "response to command: " ++ cmd
            SS.ssDebugInfo ss s
            return ms
        Nothing -> do
            SS.ssDebugError ss $ "bad response to command: " ++ cmd
            return Nothing
 
eventHandler :: SS.Session -> (String -> IO ()) -> Int -> String -> IO ()
eventHandler ss fileopen id str = do
    SS.ssDebugInfo ss $ "event handler: " ++ str
    b <- SS.ssTestState ss SS.ssStateRunning
    if b then do
        case PR.parseDebuggerOutput str of
            Just dout -> do
                SS.dsSetDebugOutput ss dout
                SS.ssQueueFunction ss $ handleDebuggerOutput ss fileopen
            Nothing   -> SS.ssDebugError ss $ "Failed to parse debugger output:\n" ++ str
        SS.ssClearStateBit ss SS.ssStateRunning
    else return () -- should send this to the output pane

handleDebuggerOutput :: SS.Session -> (String -> IO ()) -> IO ()
handleDebuggerOutput ss fileopen = do
    ds <- SS.dsGetDebugSession ss
    case SS.dsOutput ds of
        Just dout -> do
            let filePath = (SS.dsDirectory ds) </> (takeFileName $ SS.doFilePath dout)
            fileopen filePath
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


    


   



