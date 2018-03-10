module Session 
(
    Session,
    TOutput,
    FunctionChannel,
    TErrors,
--
    ssCreate,
    ssFrame,
    ssAuiMgr,
    ssEditors,
    ssMenus,
    ssStatus,
    ssTOutput,
    ssCFunc,
    ssOutputs,
    ssOutput,
    ssDebugError,
    ssDebugWarn,
    ssDebugInfo,
    ssHideWindows,
    ssMenuListNew,
    ssMenuListCreate,
    ssMenuListAdd,
    ssMenuListGet,
    ssCompilerReport,
    ssFindText,
    ssToString,
    SsMenuList,
    SsNameMenuPair,
--
    CompError,
    ceFilePath,
    ceSrcLine,
    ceSrcCol,
    ceErrLine,
    ceErrLines,
    ceCompError,
    compErrorToString,
    compErrorsToString,
--
    FindText,
    ftFindText,
    ftText,
    ftCurrPos,
    ftStartPos, 
--
    THideWindows,
    HideWindows,
    HideWindow,
    TextWindow,
    TextMenus,
    hwWindows,
    hwWindow,
    hwMenus,
    createGhciWindowType,
    createSourceWindowType,
    createDebugWindowType,
    createOutputWindowType,
    createTextWindow,
    createTextMenus,
    createHideWindow,
    createHideWindows,
    createMenuFunction,
    hwPanelHwnd,
    hwFindWindow,
    hwUpdate,
    hwFindFocusedWindow,
    hwHasFocus,
    hwGetWindows,
    hwIsGhci,
    hwIsSourceFile,
    hwIsOutput,
    hwIsDebug,
    hwFilePath,
    hwIsSameWindow,
    hwIsSameFile,
    hwFindSourceFileWindow,
    hwRemoveWindow,
    hwUpdateHideWindows,
    hwGetEditor,
    hwMatchesHwnd,
    hwIsClean,
    hwFindWindows,
    hwSetFilePath,
    hwUpdateWindow,
--    
    twType,
    twPanel,
    twPanelHwnd,
    twHwnd,
    twFilePath,
    twMenuFunctions,
    twHasFocus,
    twIsClean,
    twStatusInfo,
    twFilePathToString,
    twGetEditor,
    twSetFilePath,
    twIsGhci,
    twIsSourceFile,
    twIsOutput,
    twIsDebug,
    twMatchesHwnd,
    twIsSameFile,
    twIsSameWindow,
    twFindWindow,
    twRemoveWindow,
--
    tmGetMenuFunction,
    tmGetMenuEnabled 
) where


import Graphics.UI.WX
import Graphics.UI.WXCore
import Control.Concurrent (myThreadId, ThreadId)
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Monad (liftM)
import Data.ByteString.Internal (ByteString)
import Data.String.Combinators (punctuate)
import Data.List (find)
import qualified Data.ByteString.Char8 as BS (pack)
import Data.Char (toLower)
import Data.Word (Word64)
import Graphics.Win32.GDI.Types (HWND)
import Numeric (showHex)

import qualified Constants as CN
import Debug
import qualified Misc as MI
import qualified Scintilla as SC
                
----------------------------------------------------------
-- Session data and ToString functions
----------------------------------------------------------

data Session = Session {    ssFrame             :: Frame (),            -- Main window
                            ssAuiMgr            :: AuiManager (),       -- Application level AUI manager
                            ssEditors           :: AuiNotebook (),      -- Notebook of source file editors
                            ssMenus             :: SsMenuList,
                            ssStatus            :: StatusField,
                            ssTOutput           :: TOutput,             -- TCHan for output pane, e.g. compiler output
                            ssCFunc             :: FunctionChannel,     -- TChan for scheduling functions to be called in main GUI thread (see timer)
                            ssOutputs           :: AuiNotebook (),      -- The output panes notebook, includes ssOutput pane below
                            ssOutput            :: SC.ScnEditor,        -- The output pane
                            ssDebugError        :: String -> IO (),
                            ssDebugWarn         :: String -> IO (),
                            ssDebugInfo         :: String -> IO (),
                            ssMainThreadId      :: ThreadId,
                            ssCompilerReport    :: TErrors,
                            ssFindText          :: TFindText,
                            ssHideWindows       :: THideWindows}
   
data FindText = FindText { ftText :: String, ftCurrPos :: Int, ftStartPos :: Int }

-- compiler output channel
type TOutput = TChan ByteString

-- compiler errors
type TErrors = TVar [CompError]

type TFindText = TVar FindText

-- scheduled functions for timer event to run
type FunctionChannel = TChan (IO ())

data TextWindowType = SourceFile SC.ScnEditor | Ghci | Debug SC.ScnEditor | Output SC.ScnEditor
data MenuFunction = MenuFunction { mfId :: Int, mfFunction :: IO (), mfEnabled :: IO Bool  }

type SsNameMenuPair = (Int, MenuItem ())                               
type SsMenuList = [SsNameMenuPair]

----------------------------------------------------------------
-- Session  helpers
----------------------------------------------------------------

-- please call this on the main thread
ssCreate :: Frame () -> AuiManager () -> AuiNotebook () -> SsMenuList -> StatusField -> AuiNotebook () -> SC.ScnEditor -> SC.ScnEditor -> IO Session
ssCreate mf am nb ms sf ots ot db = do 
    mtid <- myThreadId
    tot  <- atomically $ newTChan
    cfn  <- atomically $ newTChan
    terr <- atomically $ newTVar []
    tfnd <- atomically $ newTVar (FindText "" 0 0)
    let dbe = if CN.debug then (\s -> ssInvokeInGuiThread mtid cfn $ debugError db s) else (\s -> return ())
    let dbw = if CN.debug then (\s -> ssInvokeInGuiThread mtid cfn $ debugWarn  db s) else (\s -> return ())
    let dbi = if CN.debug then (\s -> ssInvokeInGuiThread mtid cfn $ debugInfo  db s) else (\s -> return ())
    hws  <- atomically $ newTVar $ createHideWindows [] 
    return (Session mf am nb ms sf tot cfn ots ot dbe dbw dbi mtid terr tfnd hws)

-- creates a new menu item lookup list
-- a dummy entry is provided for failed lookups to simplfy client calls to menuListGet 
ssMenuListNew :: IO SsMenuList 
ssMenuListNew = do
    mi <- menuItemCreate
    return ([(0, mi)])

ssMenuListCreate :: [SsNameMenuPair] -> IO SsMenuList
ssMenuListCreate nmps = do
    ml <- ssMenuListNew
    return (ssMenuListAdd nmps ml)

ssMenuListAdd :: [SsNameMenuPair] -> SsMenuList -> SsMenuList
ssMenuListAdd nmps ml = ml ++ nmps

ssMenuListGet :: Session -> Int -> MenuItem ()
ssMenuListGet ss n = 
    case (lookup n ml) of
        Just mi -> mi
        Nothing -> snd $ last ml
    where ml = ssMenus ss
    
ssToString :: Session -> IO String
ssToString ss = do
    fs <- MI.frameToString $ ssFrame ss
    return ("{Session} Main: " ++ fs)

ssInvokeInGuiThread :: ThreadId -> FunctionChannel -> (IO ()) -> IO ()
ssInvokeInGuiThread mtid chan f = do
    tid <- myThreadId
    if mtid == tid then f
    else atomically $ writeTChan chan f
             
----------------------------------------------------------------
-- Comp error
----------------------------------------------------------------

-- compilation error
data CompError = CompError {    ceFilePath  :: String, 
                                ceSrcLine   :: Int, 
                                ceSrcCol    :: Int, 
                                ceErrLine   :: Int, -- line in compiler output
                                ceErrLines  :: [String] } deriving (Show)

ceCompError :: String -> Int -> Int -> Int -> [String] -> CompError
ceCompError fp sl sc el els = (CompError fp sl sc el els)

compErrorsToString :: [CompError] -> String
compErrorsToString ces = "Errors = " ++ (show $ length ces) ++ (concat $ map (\ce -> (compErrorToString ce) ++ "\n" ) ces)

compErrorToString :: CompError -> String
compErrorToString c =
    "Filename: " ++ (show $ ceFilePath c) ++ " (" ++ (show $ ceSrcLine c) ++ "," ++ (show $ ceSrcCol c) ++ ") errout = " ++ (show $ ceErrLine c) ++ "\n" ++
        (concat $ map (\s -> " " ++ s ++ "\n") (ceErrLines c))
     
----------------------------------------------------------------
-- Find Text
----------------------------------------------------------------

ftFindText :: String -> Int -> Int -> FindText
ftFindText text currPos startPos = (FindText text currPos startPos)

----------------------------------------------------------------
-- Text windows
----------------------------------------------------------------

-- | Text Window
-- things to add return string for updating status bar, line col pos etc.
-- close, save, save as
data TextWindow 
    = TextWindow {  twType              :: TextWindowType,
                    twPanel             :: Panel (),            -- ^ The parent panel of text window
                    twPanelHwnd         :: HWND,                -- ^ HWND of panel
                    twHwnd              :: HWND,
                    twTFilePath         :: TFilePath }          -- ^ File name associated with window

data TextMenus 
    = TextMenus {   twMenuFunctions     :: [MenuFunction],
                    twHasFocus          :: IO Bool,
                    twIsClean           :: IO Bool,
                    twStatusInfo        :: IO String}

data HideWindow = HideWindow { hwWindow :: TextWindow, hwMenus :: TextMenus }
data HideWindows = HideWindows { hwWindows :: [HideWindow] }
type THideWindows = TVar HideWindows
type TFilePath = TVar (Maybe String)

---------------------------------------------------------------

createGhciWindowType :: TextWindowType
createGhciWindowType = (Ghci)

createSourceWindowType :: SC.ScnEditor -> TextWindowType
createSourceWindowType scn = (SourceFile scn)

createDebugWindowType :: SC.ScnEditor -> TextWindowType
createDebugWindowType scn = (Debug scn)

createOutputWindowType :: SC.ScnEditor -> TextWindowType
createOutputWindowType scn = (Output scn)

createTextWindow :: TextWindowType -> Panel () -> HWND -> HWND -> Maybe String -> IO TextWindow
createTextWindow wtype panel hwndp hwnd file =  do
    tfile <- (atomically $ newTVar file)
    return (TextWindow wtype panel hwndp hwnd tfile)

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
hwFindSourceFileWindow ss fp = 
    hwFindWindow ss (\hw -> (hwIsSourceFile hw) && (fmap (map toLower) (hwFilePath hw) == Just ((map toLower) fp)))

hwRemoveWindow :: Session -> HideWindow -> IO HideWindows
hwRemoveWindow ss hw = hwUpdate ss (\hws -> MI.findAndRemove (\hw' -> hwIsSameWindow hw hw') hws)

hwUpdateHideWindows :: Session -> ([HideWindow] -> [HideWindow]) -> IO HideWindows
hwUpdateHideWindows ss f = atomically (modifyTVar thws (\hws -> createHideWindows $ hwWindows hws) >> readTVar thws)
    where thws = ssHideWindows ss

hwGetEditor :: HideWindow -> Maybe SC.ScnEditor
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

hwSetFilePath :: HideWindow -> String -> HideWindow
hwSetFilePath hw fp = createHideWindow (twSetFilePath (hwWindow hw) fp) (hwMenus hw)



----------------------------------------------------------------

twFilePath :: TextWindow -> IO (Maybe String)
twFilePath tw = atomically $ readTVar twTFilePath tw

twFilePathToString :: TextWindow -> String                        
twFilePathToString tw = maybe "" id (twFilePath tw)

twGetEditor :: TextWindow -> Maybe SC.ScnEditor
twGetEditor tw = 
    case twType tw of
        (SourceFile scn) -> Just scn
        _                -> Nothing

twSetFilePath :: TextWindow -> String -> TextWindow
twSetFilePath (TextWindow wtype p phwnd hwnd _) fp = 
    createTextWindow wtype p phwnd hwnd $ atomically $ newTVar (Just fp)

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
twMatchesHwnd tw h = MI.comparePtrs h (twPanelHwnd tw)

twIsSameFile :: TextWindow -> TextWindow -> Bool
twIsSameFile tw1 tw2 = fmap (map toLower) (twFilePath tw1) == fmap (map toLower) (twFilePath tw2)

twIsSameWindow :: TextWindow -> TextWindow -> Bool
twIsSameWindow tw1 tw2 = twPanelHwnd tw1 == twPanelHwnd tw2

twFindWindow :: Session -> (TextWindow -> Bool) -> IO (Maybe TextWindow)
twFindWindow ss p = do
    hws <- atomically (readTVar $ ssHideWindows ss)
    return $ liftM hwWindow $ find (\hw -> p $ hwWindow hw) $ hwWindows hws

twRemoveWindow :: Session -> TextWindow -> IO HideWindows
twRemoveWindow ss tw = hwUpdate ss (\hws -> MI.findAndRemove (\hw' -> twIsSameWindow tw $ hwWindow hw') hws)

----------------------------------------------------------------

tmGetMenuFunction :: TextMenus -> Int -> IO ()
tmGetMenuFunction tw id = maybe (return ()) (\(MenuFunction _ mf _) -> mf)  $ find (\(MenuFunction mid _ _) -> mid == id) (twMenuFunctions tw)

tmGetMenuEnabled :: TextMenus -> Int -> IO Bool
tmGetMenuEnabled tw id = maybe (return False) (\(MenuFunction _ _ me) -> me)  $ find (\(MenuFunction mid _ _) -> mid == id) (twMenuFunctions tw)
