
module Session 
(
    CompError (..),
    CompReport (..),
    DebugBreakPoint (..),
    DebugOutput (..),
    DebugRange (..),
    DebugRecord (..),
    DebugSession (..),
    DebugState (..),
    DebugVariable (..),
    FindText (..),
    FunctionChannel,
    Session (..),
    TErrors,
    TextWindow (..),
    TextWindows,
    compErrorToString,
    compErrorsToString,
    crCreateCompError,
    crCreateCompReport,
    crFindError,
    crGetNoOfErrors,
    crUpdateCurrentError,
    crUpdateReport,
    createBreakPoint,
    createDebugOutput,
    createDebugRange,
    createDebugRecord,
    createDebugSession,
    createDebugTextWindow,
    createDebugVariable,
    createGhciTextWindow,
    createMenuFunction,
    createOutputTextWindow,
    createSourceTextWindow,
    createTextWindow,
    doGetDebugRange,
    dsAddBreakPoint,
    dsBreakPointSet,
    dsClearDebugOutput,
    dsClearDebugSession,
    dsDebugOutputAppend,
    dsDebugOutputClear,
    dsDebugOutputGet,
    dsDebugOutputSet,
    dsDeleteBreakPoint,
    dsEqualBreakPoint,
    dsGetBreakPoints,
    dsGetDebugOutput,
    dsGetDebugSession,
    dsGetSessionHwnd,
    dsSetBreakPointNo,
    dsSetBreakPoints,
    dsSetDebugOutput,
    dsUpdateBreakPoints,
    dsUpdateDebugSession,
    ftFindText,
    dsIncDebugRecord,
    ssClearStateBit,
    ssCreate,
    ssDisableMenuHandlers,
    ssGetCompilerReport,
    ssOutput,
    ssQueueFunction,
    ssRunFunctionQueue,
    ssSetCompilerReport,
    ssSetMenuHandlers,
    ssSetMenus,
    ssSetOutput,
    ssSetStateBit,
    ssStateCompile,
    ssStateDebugging,
    ssStateRunning,
    ssTestState,
    ssToString,
    twFilePathToString,
    twFindAndSetFilePath,
    twFindSourceFileWindow,
    twFindWindow,
    twFindWindows,
    twGetEditor,
    twGetWindows,
    twIsDebug,
    twIsGhci,
    twIsOutput,
    twIsSameFile,
    twIsSameWindow,
    twIsSourceFile,
    twMatchesHwnd,
    twRemoveWindow,
    twSetFilePath,
    twUpdate
) where

import Control.Concurrent (myThreadId, ThreadId)
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Monad (forM_, liftM, liftM2)
import Control.Monad.Loops (whileM_)
import Data.Bits ((.&.), setBit, clearBit, testBit)
import Data.ByteString.Internal (ByteString)
import Data.String.Combinators (punctuate)
import Data.List (find, intercalate)
import qualified Data.ByteString.Char8 as BS (pack)
import Data.Char (toLower)
import Data.Word (Word64)
import Graphics.UI.WX
import Graphics.UI.WXCore
import Graphics.Win32.GDI.Types (HWND)
import Numeric (showHex)
import System.FilePath.Windows (takeFileName)

import qualified Constants as CN
import Debug as DG
import qualified Menus as MN
import qualified Misc as MI
import qualified Scintilla as SC
                
----------------------------------------------------------
-- Session data and ToString functions
----------------------------------------------------------

data Session = Session {    ssFrame             :: Frame (),            -- Main window
                            ssAuiMgr            :: AuiManager (),       -- Application level AUI manager
                            ssEditors           :: AuiNotebook (),      -- Notebook of source file editors
                            ssMenus             :: TMenus,
                            ssStatus            :: StatusField,
                            ssCFunc             :: FunctionChannel,     -- TChan for scheduling functions to be called in main GUI thread (see timer)
                            ssOutputs           :: AuiNotebook (),      -- The output panes notebook, includes ssOutput pane below
                            ssTMOutput          :: TMTextWindow,        -- The output pane, maybe
                            ssDebugError        :: String -> IO (),
                            ssDebugWarn         :: String -> IO (),
                            ssDebugInfo         :: String -> IO (),
                            ssMainThreadId      :: ThreadId,
                            ssCompilerReport    :: TErrors,
                            ssFindText          :: TFindText,
                            ssTextWindows       :: TTextWindows,
                            ssState             :: TState,
                            ssDebugSession      :: TDebugSession,
                            ssDebugGrid         :: Grid (),
                            ssDebugOut          :: TDebugOut,
                            ssFileOpen          :: (Session -> String -> IO ()), -- File Open and Save are used in many places in the program
                                                                                 -- however the strict modular hierarchy prevents import of FileMenu.hs where needed
                            ssFileSave          :: (Session -> TextWindow -> SC.Editor -> IO Bool)}
   
data FindText = FindText { ftText :: String, ftCurrPos :: Int, ftStartPos :: Int }

-- compiler errors
type TErrors = TVar CompReport
type TFindText = TVar FindText

-- debugger output capture
type TDebugOut = TVar String 

-- scheduled functions for timer event to run
type FunctionChannel = TChan (IO ())

data MenuFunction = MenuFunction { mfId :: Int, mfFunction :: IO (), mfEnabled :: IO Bool  }

type TState = TVar Int

-- menus
type TMenus = TVar MN.HideMenus

----------------------------------------------------------------
-- Session  helpers
----------------------------------------------------------------

-- please call this on the main thread
ssCreate :: 
    Frame () -> 
    AuiManager () -> 
    AuiNotebook () -> 
    MN.HideMenus -> 
    StatusField -> 
    AuiNotebook () -> 
    SC.Editor -> 
    Grid () ->
    (Session -> String -> IO ()) ->
    (Session -> TextWindow -> SC.Editor -> IO Bool) ->
    IO Session
ssCreate mf am nb ms sf ots db dbgr fileopen filesave = do
    mtid <- myThreadId
    cfn  <- atomically $ newTChan
    terr <- atomically $ newTVar (crCreateCompReport Nothing [])
    tfnd <- atomically $ newTVar (FindText "" 0 0)
    tout <- atomically $ newTVar Nothing
    tms  <- atomically $ newTVar ms
    let dbe = if CN.debug then (\s -> ssInvokeInGuiThread mtid cfn $ DG.debugError db s) else (\s -> return ())
    let dbw = if CN.debug then (\s -> ssInvokeInGuiThread mtid cfn $ DG.debugWarn  db s) else (\s -> return ())
    let dbi = if CN.debug then (\s -> ssInvokeInGuiThread mtid cfn $ DG.debugInfo  db s) else (\s -> return ())
    hws  <- atomically $ newTVar $ createTextWindows [] 
    state <- atomically $ newTVar 0 
    debug <- atomically $ newTVar $ createDebugSession Nothing "" [] Nothing
    debot <- atomically $ newTVar "" 
    return (Session mf am nb tms sf cfn ots tout dbe dbw dbi mtid terr tfnd hws state debug dbgr debot fileopen filesave)

----------------------------------------------------------------
-- Menu helpers
----------------------------------------------------------------

ssSetMenuHandlers :: Session -> MN.HideMenuHandlers -> IO ()
ssSetMenuHandlers ss handlers = do
    -- update the session with the new handlers
    menus <- atomically $ do
        menus <- readTVar $ ssMenus ss
        writeTVar (ssMenus ss) $ MN.mergeMenus menus handlers
        return menus
    -- update the menus
    ssSetMenus ss

ssSetMenus :: Session -> IO ()
ssSetMenus ss = do
    menus <- atomically $ do readTVar $ ssMenus ss
    forM_ menus $ \menu -> do
        e <- MN.mnEnabled menu
        set (MN.mnItem menu) [
            text := MN.mnTitle menu,
            help := MN.mnHelp menu, 
            on command := (MN.mnAction menu >> ssSetMenus ss), 
            enabled := e]

ssDisableMenuHandlers :: Session -> HWND -> IO ()
ssDisableMenuHandlers ss handlers = do
    -- update the session with the new handlers
    menus <- atomically $ do
        menus <- readTVar $ ssMenus ss
        writeTVar (ssMenus ss) $ MN.disableMenus menus handlers
        return menus
    -- update the menus
    ssSetMenus ss

----------------------------------------------------------------
-- 
----------------------------------------------------------------

ssToString :: Session -> IO String
ssToString ss = do
    fs <- MI.frameToString $ ssFrame ss
    return ("{Session} Main: " ++ fs)

ssInvokeInGuiThread :: ThreadId -> FunctionChannel -> (IO ()) -> IO ()
ssInvokeInGuiThread mtid chan f = do
    tid <- myThreadId
    if mtid == tid then f
    else atomically $ writeTChan chan f
   
ssOutput :: Session -> IO (Maybe TextWindow)
ssOutput ss = atomically $ readTVar $ ssTMOutput ss
 
ssSetOutput :: Session -> Maybe TextWindow -> IO ()
ssSetOutput ss mhw = atomically $ writeTVar (ssTMOutput ss) mhw

ssGetCompilerReport :: Session -> IO CompReport
ssGetCompilerReport ss = atomically $ readTVar $ ssCompilerReport ss
         
ssSetCompilerReport :: Session -> CompReport -> IO ()
ssSetCompilerReport ss cr = atomically $ writeTVar (ssCompilerReport ss) cr

----------------------------------------------------------------
-- state management
----------------------------------------------------------------

ssTestState :: Session -> Int -> IO Bool
ssTestState ss mask = atomically (do 
    s <- readTVar (ssState ss) 
    return $ testBit s mask)

ssSetStateBit :: Session -> Int -> IO ()
ssSetStateBit ss n = atomically $ modifyTVar (ssState ss) (\s -> setBit s n) 

ssClearStateBit :: Session -> Int -> IO ()
ssClearStateBit ss n = atomically $ modifyTVar (ssState ss) (\s -> clearBit s n) 

-- use bit no's for states
ssStateCompile :: Int
ssStateCompile = 1

ssStateDebugging :: Int
ssStateDebugging = 2

ssStateRunning :: Int
ssStateRunning = 3

----------------------------------------------------------------
-- Comp error
----------------------------------------------------------------

-- compilation report
data CompReport = CompReport {  crCurrErr       :: Maybe Int,  -- the last error jumped to
                                crErrorCount    :: Int, 
                                crErrors        :: [CompError] }

data CompError = CompError {    ceErrorNo   :: Int,    
                                ceFilePath  :: String, 
                                ceSrcLine   :: Int, 
                                ceSrcCol    :: Int, 
                                ceErrLine   :: Int, -- line in compiler output 
                                ceErrLines  :: [String] } deriving (Show)

crCreateCompReport :: Maybe Int -> [CompError] -> CompReport
crCreateCompReport mcerr errs = (CompReport mcerr (length errs) errs)

crUpdateReport :: Session -> (CompReport -> CompReport) -> IO ()
crUpdateReport ss f = atomically $ modifyTVar (ssCompilerReport ss) f

crUpdateCurrentError :: Session -> Maybe Int -> IO ()
crUpdateCurrentError ss mcerr = crUpdateReport ss (\cr -> crCreateCompReport mcerr $ crErrors cr)

crCreateCompError :: Int -> String -> Int -> Int -> Int -> [String] -> CompError
crCreateCompError errn fp sl sc el els = (CompError errn fp sl sc el els)

crFindError :: Session -> Int -> IO (Maybe CompError)
crFindError ss line = do
    ces <- ssGetCompilerReport ss
    return $ find (\ce -> 
        let ls = ceErrLine ce
            le = (ceErrLine ce) + (length $ ceErrLines ce)
        in  line >= ls && line <= le) (crErrors ces)

crGetNoOfErrors :: Session -> IO Int
crGetNoOfErrors ss = ssGetCompilerReport ss >>= (return . crErrorCount)

compErrorsToString :: [CompError] -> String
compErrorsToString ces = "Errors = " ++ (show $ length ces) ++ (concat $ map (\ce -> (compErrorToString ce) ++ "\n" ) ces)

compErrorToString :: CompError -> String
compErrorToString c =
    "Filename: " ++ (show $ ceFilePath c) ++ " (" ++ (show $ ceSrcLine c) ++ "," ++ 
        (show $ ceSrcCol c) ++ ") errout = " ++ 
        (show $ ceErrorNo c) ++ ", " ++ 
        (show $ ceErrLine c) ++ "\n" ++
        (concat $ map (\s -> " " ++ s ++ "\n") (ceErrLines c))
     
----------------------------------------------------------------
-- Find Text
----------------------------------------------------------------

ftFindText :: String -> Int -> Int -> FindText
ftFindText text currPos startPos = (FindText text currPos startPos)

----------------------------------------------------------------
-- Text windows
----------------------------------------------------------------

data TextWindowType = SourceFile SC.Editor | GhciFile | DebugFile SC.Editor | OutputFile SC.Editor

-- | Text Window
-- things to add return string for updating status bar, line col pos etc.
-- close, save, save as
data TextWindow 
    = TextWindow {  twType              :: TextWindowType,
                    twPanel             :: Panel (),            -- ^ The parent panel of text window
                    twPanelHwnd         :: HWND,                -- ^ HWND of panel
                    twHwnd              :: HWND,
                    twFilePath          :: Maybe String,        -- ^ File name associated with window
                    twSetFocus          :: IO (),
                    twHasFocus          :: IO Bool,
                    twIsClean           :: IO Bool,
                    twStatusInfo        :: IO String}

type TMTextWindow = TVar (Maybe TextWindow)
type TTextWindows = TVar TextWindows
data TextWindows = TextWindows { twWindows :: [TextWindow] }

---------------------------------------------------------------

instance Show TextWindowType where
    show (SourceFile scn) = show scn
    show GhciFile = "GHCI window"
    show (DebugFile scn) = show scn
    show (OutputFile scn) = show scn

instance Show TextWindow where
    show (TextWindow typ _ phwnd hwnd mfp _ _ _ _) = 
        "TextWindow: " ++ show typ ++ 
        ", panel hwnd = " ++ MI.hwndToString phwnd ++
        ", child hwnd = " ++ MI.hwndToString hwnd  ++
        ", file path = " ++ maybe "" show mfp

---------------------------------------------------------------

createTextWindow :: TextWindowType -> Panel () -> HWND -> HWND -> Maybe String -> IO () -> IO Bool -> IO Bool -> IO String -> TextWindow
createTextWindow wtype panel hwndp hwnd mfp setfocus hasfocus isclean status = (TextWindow wtype panel hwndp hwnd mfp setfocus hasfocus isclean status)

createSourceTextWindow :: SC.Editor -> Panel () -> HWND -> HWND -> Maybe String -> IO () -> IO Bool -> IO Bool -> IO String -> TextWindow
createSourceTextWindow scn = createTextWindow $ SourceFile scn

createGhciTextWindow :: Panel () -> HWND -> HWND -> Maybe String -> IO () -> IO Bool -> IO Bool -> IO String -> TextWindow
createGhciTextWindow  = createTextWindow GhciFile

createDebugTextWindow :: SC.Editor -> Panel () -> HWND -> HWND -> Maybe String -> IO () -> IO Bool -> IO Bool -> IO String -> TextWindow
createDebugTextWindow scn = createTextWindow $ DebugFile scn

createOutputTextWindow :: SC.Editor -> Panel () -> HWND -> HWND -> IO () -> IO Bool -> IO Bool -> IO String -> TextWindow
createOutputTextWindow scn panel phwnd hwnd = createTextWindow (OutputFile scn) panel phwnd hwnd Nothing 

createMenuFunction :: Int -> IO () -> IO Bool -> MenuFunction
createMenuFunction id mf me = (MenuFunction id mf me)

createTextWindows :: [TextWindow] -> TextWindows
createTextWindows tws = (TextWindows tws)


twFilePathToString :: TextWindow -> String                        
twFilePathToString tw = maybe "" id $ twFilePath tw

twGetEditor :: TextWindow -> Maybe SC.Editor
twGetEditor tw = 
    case twType tw of
        (SourceFile scn) -> Just scn
        (OutputFile scn) -> Just scn
        (DebugFile scn)  -> Just scn
        _                -> Nothing

twFindAndSetFilePath :: Session -> TextWindow -> Maybe String -> IO (Maybe TextWindow)
twFindAndSetFilePath ss tw mfp = do 
    twUpdateWindow ss (\tw -> 
        if twMatchesHwnd tw (twHwnd tw) then
            Just $ twSetFilePath tw mfp
        else Nothing)
    twFindWindow ss (\tw' -> twMatchesHwnd tw' (twHwnd tw))

twSetFilePath :: TextWindow -> Maybe String -> TextWindow
twSetFilePath tw mfp = tw { twFilePath = mfp }

twIsGhci :: TextWindow -> Bool
twIsGhci tw = case twType tw of
                GhciFile  -> True
                otherwise -> False

twIsSourceFile :: TextWindow -> Bool
twIsSourceFile tw = case twType tw of
                (SourceFile _) -> True
                otherwise      -> False

twIsOutput :: TextWindow -> Bool
twIsOutput tw = case twType tw of
                (OutputFile _) -> True
                otherwise      -> False

twIsDebug :: TextWindow -> Bool
twIsDebug tw = case twType tw of
                (DebugFile _) -> True
                otherwise     -> False

twMatchesHwnd :: TextWindow -> HWND -> Bool
twMatchesHwnd tw h = isMatch (twPanelHwnd tw) || isMatch (twHwnd tw)
    where isMatch = MI.comparePtrs h

twIsSameFile :: TextWindow -> TextWindow -> Bool
twIsSameFile tw1 tw2 = 
    fmap (map toLower) (twFilePath tw1) == fmap (map toLower) (twFilePath tw2)

twIsSameWindow :: TextWindow -> TextWindow -> Bool
twIsSameWindow tw1 tw2 = twPanelHwnd tw1 == twPanelHwnd tw2

twGetWindows :: Session -> IO [TextWindow]
twGetWindows ss = do
    tws <- atomically (readTVar $ ssTextWindows ss) 
    return $ twWindows tws

twFindWindow :: Session -> (TextWindow -> Bool) -> IO (Maybe TextWindow)
twFindWindow ss p = do
    tws <- twGetWindows ss
    return $ find p tws

twRemoveWindow :: Session -> TextWindow -> IO TextWindows
twRemoveWindow ss tw = twUpdate ss (\tws -> MI.findAndRemove (\tw' -> twIsSameWindow tw tw') tws)

twUpdate :: Session -> ([TextWindow] -> [TextWindow]) -> IO TextWindows
twUpdate ss f = atomically (modifyTVar ttws (\tws -> (createTextWindows $ f $ twWindows tws)) >> readTVar ttws) 
    where ttws = ssTextWindows ss

-- finds and modifies a hide window, and returns the modified window
twUpdateWindow :: Session -> (TextWindow -> Maybe TextWindow) -> IO TextWindows
twUpdateWindow ss p = atomically (modifyTVar ttws (\tws -> createTextWindows $ update (twWindows tws) p) >> readTVar ttws)
    where   update [] _ = []
            update (tw:tws) p = case p tw of
                Just tw' -> tw':tws
                Nothing  -> tw:(update tws p)
            ttws = ssTextWindows ss

twFindWindows :: Session -> (TextWindow -> Bool) -> IO [TextWindow]
twFindWindows ss p = do
    tws <- atomically (readTVar $ ssTextWindows ss) 
    return $ filter p $ twWindows tws

twFindSourceFileWindow :: Session -> String -> IO (Maybe TextWindow)
twFindSourceFileWindow ss fp = do
    tws <- twGetWindows ss
    MI.findIO (\tw -> do 
        case twFilePath tw of
            Just fp' -> do
                return $ (twIsSourceFile tw) && (namelc fp == namelc fp')
            Nothing  -> return False) tws
    where namelc f = (map toLower) (takeFileName f)

-----------------------------------------------------------------------
-- Debug session
-----------------------------------------------------------------------

type TDebugOutput = TVar String -- output from debugger
type TDebugSession = TVar DebugSession

data DebugState = DbInitialising | DbPaused | DbFinished deriving (Eq)
data DebugRecord = DebugRecord 
    { 
        dbTics :: Int, 
        dbState :: DebugState 
    }

instance Show DebugState where
    show dbs = case dbs of 
        DbInitialising -> "Initialising"
        DbPaused       -> "Paused"
        DbFinished     -> "Finished"

instance Show DebugRecord where
    show (DebugRecord tics state) = "DbState: Tics = " ++ show tics ++ ", State = " ++ show state

data DebugSession = DebugSession 
    { 
        dsTw            :: Maybe TextWindow,        -- GHCI session
        dsDirectory     :: String,                  -- working directory for GHCI
        dsBreakPoints   :: [DebugBreakPoint],             
        dsOutput        :: Maybe DebugOutput        -- after single step the free variables are saved
    }

data DebugBreakPoint = DebugBreakPoint 
    { 
        dsEditor    :: SC.Editor, 
        dsFilePath  :: String, 
        dsHandle    :: Int,     -- the scintilla marker handle
        dsNo        :: Int      -- the GHCI breakpoint no.
    }

-- the parsed output from GHCI after single
-- step has occurred.
data DebugOutput = DebugOutput 
    { 
        doModule    :: String, 
        doFunction  :: String, 
        doFilePath  :: String,
        doRange     :: DebugRange,
        doVariables :: [DebugVariable]
    }

-- the row and column range provided by GHCI
-- after single step has completed.
data DebugRange = DebugRange
    {
        doLineS :: Int, -- line start
        doLineE :: Int, -- line end
        doColS  :: Int,
        doColE  :: Int
    }

-- the value of a free variable as returned by GHCI after single step
data DebugVariable = DebugVariable 
    {
        doVariable  :: String,
        doType      :: String,
        doValue     :: String 
    }

----------------------------------------
-- show functions for debugger types
----------------------------------------

instance Show DebugSession where
    show (DebugSession tw dir bps mdout) = 
        "DebugSession: tw = " ++ show tw ++ 
        ", directory = " ++ dir ++
        (intercalate "\n" (map show bps)) ++ 
        maybe "" (\dout -> "\n" ++ show dout) mdout

instance Show DebugBreakPoint where
    show (DebugBreakPoint scn fp h no) = 
        "DebugBreakPoint: editor = " ++ show scn ++ 
        ", filepath = " ++ fp ++
        ", editor handle = " ++ show h ++
        ", GHCI no. = " ++ show no 

instance Show DebugOutput where
    show (DebugOutput mod fn fp dr drs) = 
        "DebugOutput: module = " ++ mod ++ 
        ", Function = " ++ fn ++
        ", Filepath = " ++ fp ++
        "\n  Range = " ++ show dr ++ 
        (concat $ map (\dr -> "\n" ++ show dr) drs)

instance Show DebugVariable where
    show (DebugVariable var ty val) = 
        "DebugValue: Variable = " ++ var ++ 
        ", Type = " ++ ty ++
        ", Value = " ++ val 

instance Show DebugRange where
    show (DebugRange ls le cs ce) = 
        "DebugRange: Lines = " ++ show ls ++ " - " ++ show le ++
        ", Columns = " ++ show cs ++ " - " ++ show ce 

createDebugSession :: Maybe TextWindow -> String -> [DebugBreakPoint] -> Maybe DebugOutput -> DebugSession 
createDebugSession mtw dir bps mdout = (DebugSession mtw dir bps mdout)

createDebugRecord :: DebugState -> Int -> DebugRecord
createDebugRecord state tics = (DebugRecord tics state)

dsIncDebugRecord :: DebugRecord -> DebugRecord
dsIncDebugRecord dbr =  dbr { dbTics = dbTics dbr + 1 }

dsGetDebugSession :: Session -> IO DebugSession
dsGetDebugSession ss = atomically $ readTVar (ssDebugSession ss)

dsUpdateDebugSession :: Session -> (DebugSession -> DebugSession) -> IO ()
dsUpdateDebugSession ss f = atomically $ 
    modifyTVar (ssDebugSession ss) (\ds -> f ds)

dsClearDebugSession :: Session -> IO ()
dsClearDebugSession ss = dsUpdateDebugSession ss (\ds -> createDebugSession Nothing "" (dsBreakPoints ds) Nothing)

dsSetSessionTextWindow :: Session -> Maybe TextWindow -> IO ()
dsSetSessionTextWindow ss mtw = dsUpdateDebugSession ss (\ds -> 
        createDebugSession mtw (dsDirectory ds) (dsBreakPoints ds) (dsOutput ds))

dsGetSessionHwnd :: Session -> IO (Maybe HWND)
dsGetSessionHwnd ss = do
    ds <- atomically $ readTVar (ssDebugSession ss)
    case dsTw ds of
        Just tw -> return $ Just $ twHwnd tw
        Nothing -> return Nothing

createBreakPoint :: SC.Editor -> String -> Int -> Int -> DebugBreakPoint
createBreakPoint scn fp handle no = (DebugBreakPoint scn fp handle no)

dsAddBreakPoint :: Session -> DebugBreakPoint -> IO ()
dsAddBreakPoint ss bp = 
    atomically $ modifyTVar (ssDebugSession ss) (\ds -> 
        let bps = dsBreakPoints ds 
            mtw  = dsTw ds
            dout = dsOutput ds in
        if dsBreakPointSet bp bps then 
            createDebugSession mtw (dsDirectory ds) bps dout
        else 
            createDebugSession mtw (dsDirectory ds) (bp:bps) dout)

dsDeleteBreakPoint :: Session -> String -> Int -> IO ()
dsDeleteBreakPoint ss fp handle = 
    atomically $ modifyTVar (ssDebugSession ss) (\ds ->
        let bps = dsBreakPoints ds 
            mtw  = dsTw ds
            dout = dsOutput ds in
        createDebugSession mtw (dsDirectory ds) (
            MI.findAndRemove (\bp -> (dsFilePath bp == fp) && (dsHandle bp == handle)) bps ) dout)
       
dsBreakPointSet :: DebugBreakPoint -> [DebugBreakPoint] -> Bool
dsBreakPointSet bp bps = 
    case find (\bp' -> dsEqualBreakPoint bp bp') bps of
        Just _ ->  True
        Nothing -> False

dsEqualBreakPoint :: DebugBreakPoint -> DebugBreakPoint -> Bool
dsEqualBreakPoint bp1 bp2 = 
    (dsFilePath bp1 == dsFilePath bp2) &&
    (dsHandle   bp1 == dsHandle   bp2)

dsUpdateBreakPoints :: Session -> ([DebugBreakPoint] -> [DebugBreakPoint]) -> IO ()
dsUpdateBreakPoints ss f = do
    atomically ( 
        modifyTVar (ssDebugSession ss) (\ds -> 
                let bps = dsBreakPoints ds 
                    mtw = dsTw ds 
                    dout = dsOutput ds in
                createDebugSession mtw (dsDirectory ds) (f bps) dout))
    return ()

dsGetBreakPoints :: Session -> IO [DebugBreakPoint]
dsGetBreakPoints ss = do
    ds <- atomically $ readTVar (ssDebugSession ss)
    return $ dsBreakPoints ds

dsSetBreakPoints :: Session -> [DebugBreakPoint] -> IO ()
dsSetBreakPoints ss bps = dsUpdateBreakPoints ss (\_ -> bps)

dsSetBreakPointNo :: DebugBreakPoint -> Int -> DebugBreakPoint
dsSetBreakPointNo bp no = createBreakPoint (dsEditor bp) (dsFilePath bp) (dsHandle bp) no

createDebugOutput :: String -> String -> String -> DebugRange -> [DebugVariable] -> DebugOutput
createDebugOutput mod fn fp dr dvs = (DebugOutput mod fn fp dr dvs)

createDebugVariable :: String -> String -> String -> DebugVariable
createDebugVariable var ty val = (DebugVariable var ty val)
  
createDebugRange :: Int -> Int -> Int -> Int -> DebugRange
createDebugRange ls le cs ce = (DebugRange ls le cs ce)

doGetDebugRange :: DebugOutput -> (Int, Int, Int, Int)
doGetDebugRange dout = let r = doRange dout in ((doLineS r), (doLineE r), (doColS r), (doColE r))

dsGetDebugOutput :: Session -> IO (Maybe DebugOutput)
dsGetDebugOutput ss = do
    ds <- atomically (readTVar $ ssDebugSession ss)
    return $ dsOutput ds

dsSetDebugOutput :: Session -> DebugOutput -> IO ()
dsSetDebugOutput ss dout =
    dsUpdateDebugSession ss (\ds ->
        createDebugSession (dsTw ds) (dsDirectory ds) (dsBreakPoints ds) (Just dout))

dsClearDebugOutput :: Session -> IO ()
dsClearDebugOutput ss =
    dsUpdateDebugSession ss (\ds ->
        createDebugSession (dsTw ds) (dsDirectory ds) (dsBreakPoints ds) Nothing)

dsDebugOutputClear :: Session -> IO ()
dsDebugOutputClear ss = dsDebugOutputSet ss ""

dsDebugOutputSet :: Session -> String -> IO ()
dsDebugOutputSet ss tx = atomically (writeTVar (ssDebugOut ss) tx)

dsDebugOutputAppend :: Session -> String -> IO (String)
dsDebugOutputAppend ss txb = atomically $ do
    txa <- readTVar (ssDebugOut ss) 
    writeTVar (ssDebugOut ss) $ txa ++ txb
    return $ txa ++ txb

dsDebugOutputGet :: Session -> IO String
dsDebugOutputGet ss = atomically $ readTVar (ssDebugOut ss)

--------------------------------------------
-- Function queue
--------------------------------------------

ssQueueFunction :: Session -> IO () -> IO ()
ssQueueFunction ss f = atomically $ writeTChan (ssCFunc ss) f

-- the queue is read before it is sequenced so that any of the queued functions
-- can schedule another function for the next timer tic
ssRunFunctionQueue :: Session -> IO ()
ssRunFunctionQueue ss = ssReadQueue ss [] >>= sequence_ . reverse

ssReadQueue :: Session -> [IO ()] -> IO [IO ()]
ssReadQueue ss fns = do
    mfn <- atomically (tryReadTChan $ ssCFunc ss)
    case mfn of
        Just fn -> ssReadQueue ss $ fn:fns
        Nothing -> return fns


 
