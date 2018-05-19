
module Session 
(
    CompError,
    CompReport,
    DebugBreakPoint,
    DebugOutput,
    DebugRange,
    DebugSession,
    DebugVariable,
    FindText,
    FunctionChannel,
    HideWindow,
    HideWindows,
    Session,
    SsMenuList,
    SsNameMenuPair,
    TErrors,
    THideWindows,
    TextMenus,
    TextWindow,
    ceErrLine,
    ceErrLines,
    ceErrorNo,
    ceFilePath,
    ceSrcCol,
    ceSrcLine,
    compErrorToString,
    compErrorsToString,
    crCreateCompError,
    crCreateCompReport,
    crCurrErr,
    crErrorCount,
    crErrors,
    crFindError,
    crGetNoOfErrors,
    crUpdateCurrentError,
    crUpdateReport,
    createBreakPoint,
    createDebugOutput,
    createDebugRange,
    createDebugSession,
    createDebugVariable,
    createDebugWindowType,
    createGhciWindowType,
    createHideWindow,
    createHideWindows,
    createMenuFunction,
    createOutputWindowType,
    createSourceWindowType,
    createTextMenus,
    createTextWindow,
    ssDisableMenuHandlers,
    doColE,
    doColS,
    doFilePath,
    doFunction,
    doGetDebugRange,
    doLineE,
    doLineS,
    doModule,
    doRange,
    doType,
    doValue,
    doVariable,
    doVariables,
    dsAddBreakPoint,
    dsBreakPointSet,
    dsBreakPoints,
    dsClearDebugOutput,
    dsClearDebugSession,
    dsDeleteBreakPoint,
    dsDirectory,
    dsEditor,
    dsEqualBreakPoint,
    dsFilePath,
    dsGetBreakPoints,
    dsGetDebugOutput,
    dsGetDebugSession,
    dsGetSessionHwnd,
    dsHandle,
    dsNo,
    dsOutput,
    dsSetBreakPointNo,
    dsSetBreakPoints,
    dsSetDebugOutput,
    dsUpdateBreakPoints,
    dsUpdateDebugSession,
    ftCurrPos,
    ftFindText,
    ftStartPos, 
    ftText,
    hwFilePath,
    hwFindAndSetFilePath,
    hwFindFocusedWindow,
    hwFindSourceFileWindow,
    hwFindWindow,
    hwFindWindows,
    hwGetEditor,
    hwGetWindows,
    hwHasFocus,
    hwHwnd,
    hwIsClean,
    hwIsDebug,
    hwIsGhci,
    hwIsOutput,
    hwIsSameFile,
    hwIsSameWindow,
    hwIsSourceFile,
    hwMatchesHwnd,
    hwMenus,
    hwPanelHwnd,
    hwRemoveWindow,
    hwUpdate,
    hwUpdateHideWindows,
    hwUpdateWindow,
    hwWindow,
    hwWindows,
    ssAuiMgr,
    ssClearStateBit,
    ssCompilerReport,
    ssCreate,
    ssDebugError,
    ssDebugGrid,
    ssDebugInfo,
    ssDebugSession,
    ssDebugWarn,
    ssEditors,
    ssFileOpen,
    ssFileSave,
    ssFindText,
    ssFrame,
    ssGetCompilerReport,
    ssHideWindows,
    ssOutput,
    ssOutputs,
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
    ssStatus,
    ssTestState,
    ssToString,
    tmGetMenuEnabled, 
    tmGetMenuFunction,
    twFilePath,
    twFilePathToString,
    twFindAndSetFilePath,
    twFindWindow,
    twGetEditor,
    twHasFocus,
    twHwnd,
    twIsClean,
    twIsDebug,
    twIsGhci,
    twIsOutput,
    twIsSameFile,
    twIsSameWindow,
    twIsSourceFile,
    twMatchesHwnd,
    twMenuFunctions,
    twPanel,
    twPanelHwnd,
    twRemoveWindow,
    twSetFilePath,
    twStatusInfo,
    twType
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
                            ssTMOutput          :: TMHideWindow,        -- The output pane, maybe
                            ssDebugError        :: String -> IO (),
                            ssDebugWarn         :: String -> IO (),
                            ssDebugInfo         :: String -> IO (),
                            ssMainThreadId      :: ThreadId,
                            ssCompilerReport    :: TErrors,
                            ssFindText          :: TFindText,
                            ssHideWindows       :: THideWindows,
                            ssState             :: TState,
                            ssDebugSession      :: TDebugSession,
                            ssDebugGrid         :: Grid (),
                            ssFileOpen          :: (Session -> String -> IO ()), -- File Open and Save are used in many places in the program
                                                                                 -- however the strict modular hierarchy prevents import of FileMenu.hs where needed
                            ssFileSave          :: (Session -> TextWindow -> SC.Editor -> IO Bool)}
   
data FindText = FindText { ftText :: String, ftCurrPos :: Int, ftStartPos :: Int }

-- compiler errors
type TErrors = TVar CompReport
type TFindText = TVar FindText

-- scheduled functions for timer event to run
type FunctionChannel = TChan (IO ())

data MenuFunction = MenuFunction { mfId :: Int, mfFunction :: IO (), mfEnabled :: IO Bool  }

type SsNameMenuPair = (Int, MenuItem ())                               
type SsMenuList = [SsNameMenuPair]

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
    hws  <- atomically $ newTVar $ createHideWindows [] 
    state <- atomically $ newTVar 0 
    debug <- atomically $ newTVar $ createDebugSession Nothing "" [] Nothing
    return (Session mf am nb tms sf cfn ots tout dbe dbw dbi mtid terr tfnd hws state debug dbgr fileopen filesave)

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
        set (MN.mnItem menu) [text := MN.mnTitle menu, help := MN.mnHelp menu, on command := MN.mnAction menu, enabled := e]

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
   
ssOutput :: Session -> IO (Maybe HideWindow)
ssOutput ss = atomically $ readTVar $ ssTMOutput ss
 
ssSetOutput :: Session -> Maybe HideWindow -> IO ()
ssSetOutput ss mhw = atomically $ writeTVar (ssTMOutput ss) mhw

ssGetCompilerReport :: Session -> IO CompReport
ssGetCompilerReport ss = atomically $ readTVar $ ssCompilerReport ss
         
ssSetCompilerReport :: Session -> CompReport -> IO ()
ssSetCompilerReport ss cr = atomically $ writeTVar (ssCompilerReport ss) cr


-- state management

ssTestState :: Session -> Int -> IO Bool
ssTestState ss mask = atomically (do 
    s <- readTVar (ssState ss) 
    return $ testBit s mask)

ssSetStateBit :: Session -> Int -> IO ()
ssSetStateBit ss n = atomically $ modifyTVar (ssState ss) (\s -> setBit s n) 

ssClearStateBit :: Session -> Int -> IO ()
ssClearStateBit ss n = atomically $ modifyTVar (ssState ss) (\s -> clearBit s n) 

-- use bit no.s for states
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
            le = (ceErrLine ce) + (length $ ceErrLines ce) - 1
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

data TextWindowType = SourceFile SC.Editor | Ghci | Debug SC.Editor | Output SC.Editor

-- | Text Window
-- things to add return string for updating status bar, line col pos etc.
-- close, save, save as
data TextWindow 
    = TextWindow {  twType              :: TextWindowType,
                    twPanel             :: Panel (),            -- ^ The parent panel of text window
                    twPanelHwnd         :: HWND,                -- ^ HWND of panel
                    twHwnd              :: HWND,
                    twFilePath          :: Maybe String }       -- ^ File name associated with window

data TextMenus 
    = TextMenus {   twMenuFunctions     :: [MenuFunction],
                    twHasFocus          :: IO Bool,
                    twIsClean           :: IO Bool,
                    twStatusInfo        :: IO String}

data HideWindow = HideWindow { hwWindow :: TextWindow, hwMenus :: TextMenus }
data HideWindows = HideWindows { hwWindows :: [HideWindow] }
type THideWindows = TVar HideWindows
type TMHideWindow = TVar (Maybe HideWindow)

---------------------------------------------------------------

instance Show TextWindowType where
    show (SourceFile scn) = show scn
    show Ghci = "GHCI window"
    show (Debug scn) = show scn
    show (Output scn) = show scn

instance Show TextWindow where
    show (TextWindow typ _ phwnd hwnd mfp) = 
        "TextWindow: " ++ show typ ++ 
        ", panel hwnd = " ++ MI.hwndToString phwnd ++
        ", child hwnd = " ++ MI.hwndToString hwnd  ++
        ", file path = " ++ maybe "" show mfp

---------------------------------------------------------------

createGhciWindowType :: TextWindowType
createGhciWindowType = (Ghci)

createSourceWindowType :: SC.Editor -> TextWindowType
createSourceWindowType scn = (SourceFile scn)

createDebugWindowType :: SC.Editor -> TextWindowType
createDebugWindowType scn = (Debug scn)

createOutputWindowType :: SC.Editor -> TextWindowType
createOutputWindowType scn = (Output scn)

createTextWindow :: TextWindowType -> Panel () -> HWND -> HWND -> Maybe String -> TextWindow
createTextWindow wtype panel hwndp hwnd mfp = (TextWindow wtype panel hwndp hwnd mfp)

createTextMenus :: [MenuFunction] -> IO Bool -> IO Bool -> IO String -> TextMenus
createTextMenus mfs focus clean status = (TextMenus mfs focus clean status)

createHideWindow :: TextWindow -> TextMenus -> HideWindow
createHideWindow tw tms = (HideWindow tw tms) 

createHideWindows :: [HideWindow] -> HideWindows
createHideWindows hws = (HideWindows hws)

createMenuFunction :: Int -> IO () -> IO Bool -> MenuFunction
createMenuFunction id mf me = (MenuFunction id mf me)

--------------------------------------------------------------

hwPanelHwnd :: HideWindow -> HWND
hwPanelHwnd = twPanelHwnd . hwWindow

hwHwnd :: HideWindow -> HWND
hwHwnd = twHwnd . hwWindow

hwFindWindow :: Session -> (HideWindow -> Bool) -> IO (Maybe HideWindow)
hwFindWindow ss p = do
    hws <- atomically (readTVar $ ssHideWindows ss)
    return $ find p $ hwWindows hws

hwUpdate :: Session -> ([HideWindow] -> [HideWindow]) -> IO HideWindows
hwUpdate ss f = atomically (modifyTVar thws (\hws -> (createHideWindows $ f $ hwWindows hws)) >> readTVar thws) 
    where thws = ssHideWindows ss

hwFindFocusedWindow :: Session -> IO (Maybe HideWindow)
hwFindFocusedWindow ss = do
    hws <- atomically (readTVar $ ssHideWindows ss)
    MI.findIO (\hw -> twHasFocus $ hwMenus hw ) $ hwWindows hws

hwHasFocus :: HideWindow -> IO Bool
hwHasFocus = twHasFocus . hwMenus 

hwGetWindows :: Session -> IO [HideWindow]
hwGetWindows ss = do
    hws <- atomically (readTVar $ ssHideWindows ss) 
    return $ hwWindows hws

hwIsGhci :: HideWindow -> Bool
hwIsGhci = twIsGhci . hwWindow 

hwIsSourceFile :: HideWindow -> Bool
hwIsSourceFile = twIsSourceFile . hwWindow 

hwIsOutput :: HideWindow -> Bool
hwIsOutput = twIsOutput . hwWindow 

hwIsDebug :: HideWindow -> Bool
hwIsDebug = twIsDebug . hwWindow 

hwFilePath :: HideWindow -> Maybe String
hwFilePath = twFilePath . hwWindow 

hwIsSameWindow :: HideWindow -> HideWindow -> Bool
hwIsSameWindow hw1 hw2 = (twPanelHwnd $ hwWindow hw1) == (twPanelHwnd $ hwWindow hw2)

hwIsSameFile :: HideWindow -> HideWindow -> Bool
hwIsSameFile hw1 hw2 = twIsSameFile (hwWindow hw1) (hwWindow hw2)

hwFindSourceFileWindow :: Session -> String -> IO (Maybe HideWindow)
hwFindSourceFileWindow ss fp = do
    hws <- hwGetWindows ss
    MI.findIO (\hw -> do 
        case hwFilePath hw of
            Just fp' -> do
                return $ (hwIsSourceFile hw) && (namelc fp == namelc fp')
            Nothing  -> return False) hws
    where namelc f = (map toLower) (takeFileName f)

hwRemoveWindow :: Session -> HideWindow -> IO HideWindows
hwRemoveWindow ss hw = hwUpdate ss (\hws -> MI.findAndRemove (\hw' -> hwIsSameWindow hw hw') hws)

hwUpdateHideWindows :: Session -> ([HideWindow] -> [HideWindow]) -> IO HideWindows
hwUpdateHideWindows ss f = atomically (modifyTVar thws (\hws -> createHideWindows $ hwWindows hws) >> readTVar thws)
    where thws = ssHideWindows ss

hwGetEditor :: HideWindow -> Maybe SC.Editor
hwGetEditor = twGetEditor . hwWindow

hwMatchesHwnd :: HideWindow -> HWND -> Bool
hwMatchesHwnd = twMatchesHwnd . hwWindow

hwIsClean :: HideWindow -> IO Bool
hwIsClean = twIsClean . hwMenus

hwFindWindows :: Session -> (HideWindow -> Bool) -> IO [HideWindow]
hwFindWindows ss p = do
    hws <- atomically (readTVar $ ssHideWindows ss) 
    return $ filter p $ hwWindows hws

-- finds and modifies a hide window, and returns the modified window
hwUpdateWindow :: Session -> (HideWindow -> Maybe HideWindow) -> IO HideWindows
hwUpdateWindow ss p = atomically (modifyTVar thws (\hws -> createHideWindows $ update (hwWindows hws) p) >> readTVar thws)
    where   update [] _ = []
            update (hw:hws) p = case p hw of
                Just hw' -> hw':hws
                Nothing  -> hw:(update hws p)
            thws = ssHideWindows ss

hwFindAndSetFilePath :: Session -> HideWindow -> Maybe String -> IO (Maybe TextWindow)
hwFindAndSetFilePath ss hw  = twFindAndSetFilePath ss (hwWindow hw) 

----------------------------------------------------------------

twFilePathToString :: TextWindow -> String                        
twFilePathToString tw = maybe "" id $ twFilePath tw

twGetEditor :: TextWindow -> Maybe SC.Editor
twGetEditor tw = 
    case twType tw of
        (SourceFile scn) -> Just scn
        (Output scn)     -> Just scn
        (Debug scn)      -> Just scn
        _                -> Nothing

twFindAndSetFilePath :: Session -> TextWindow -> Maybe String -> IO (Maybe TextWindow)
twFindAndSetFilePath ss tw mfp = do 
    hwUpdateWindow ss (\hw -> 
        if hwMatchesHwnd hw (twHwnd tw) then
            Just $ createHideWindow (twSetFilePath tw mfp) (hwMenus hw) 
        else Nothing)
    twFindWindow ss (\tw' -> return $ twMatchesHwnd tw' (twHwnd tw))

twSetFilePath :: TextWindow -> Maybe String -> TextWindow
twSetFilePath tw mfp = createTextWindow (twType tw) (twPanel tw) (twPanelHwnd tw) (twHwnd tw) mfp

twIsGhci :: TextWindow -> Bool
twIsGhci tw = case twType tw of
                Ghci      -> True
                otherwise -> False

twIsSourceFile :: TextWindow -> Bool
twIsSourceFile tw = case twType tw of
                (SourceFile _) -> True
                otherwise      -> False

twIsOutput :: TextWindow -> Bool
twIsOutput tw = case twType tw of
                (Output _) -> True
                otherwise  -> False

twIsDebug :: TextWindow -> Bool
twIsDebug tw = case twType tw of
                (Debug _) -> True
                otherwise -> False

twMatchesHwnd :: TextWindow -> HWND -> Bool
twMatchesHwnd tw h = isMatch (twPanelHwnd tw) || isMatch (twHwnd tw)
    where isMatch = MI.comparePtrs h

twIsSameFile :: TextWindow -> TextWindow -> Bool
twIsSameFile tw1 tw2 = 
    fmap (map toLower) (twFilePath tw1) == fmap (map toLower) (twFilePath tw2)

twIsSameWindow :: TextWindow -> TextWindow -> Bool
twIsSameWindow tw1 tw2 = twPanelHwnd tw1 == twPanelHwnd tw2

twFindWindow :: Session -> (TextWindow -> IO Bool) -> IO (Maybe TextWindow)
twFindWindow ss p = do
    hws <- hwGetWindows ss
    mhw <- MI.findIO (\hw -> p $ hwWindow hw) hws
    return $ liftM hwWindow mhw

twRemoveWindow :: Session -> TextWindow -> IO HideWindows
twRemoveWindow ss tw = hwUpdate ss (\hws -> MI.findAndRemove (\hw' -> twIsSameWindow tw $ hwWindow hw') hws)

----------------------------------------------------------------

tmGetMenuFunction :: TextMenus -> Int -> IO ()
tmGetMenuFunction tw id = maybe (return ()) (\(MenuFunction _ mf _) -> mf)  $ find (\(MenuFunction mid _ _) -> mid == id) (twMenuFunctions tw)

tmGetMenuEnabled :: TextMenus -> Int -> IO Bool
tmGetMenuEnabled tw id = maybe (return False) (\(MenuFunction _ _ me) -> me)  $ find (\(MenuFunction mid _ _) -> mid == id) (twMenuFunctions tw)

-----------------------------------------------------------------------
-- Debug session
-----------------------------------------------------------------------

type TDebugOutput = TVar String -- output from debugger
type TDebugSession = TVar DebugSession

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


 
