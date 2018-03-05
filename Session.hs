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
    ssTextWindows,
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
    ftCurrFile,
    ftCurrPos,
    ftStartFile,
    ftStartPos, 
--
    createTextWindow,
    createGhciWindowType,
    createSourceWindowType,
    createDebugWindowType,
    createOutputWindowType,
    twCreate,
    twFindWindow,
    TextWindow,
    TextWindowType,
    twType,
    twPanel,
    twPanelHwnd,
    twHwnd,
    twMenuFunctions,
    twFilePath,
    twHasFocus,
    twIsClean,
    twUpdateTextWindows,
    twUpdate,
    txWindows,
    createMenuFunction,
    MenuFunction,
    twGetMenuFunction,
    twGetMenuEnabled,
    twGetFocusedWindow,
    twMatchesHwnd,
    twGetWindows,
    twIsGhci,
    twIsSourceFile,
    twIsDebug,
    twIsOutput,
    twIsSameWindow,
    twIsSameFile,
    twRemoveWindow,
    twFilePathToString,
    twGetSourceFileWindow,
    twGetEditor
) where


import Graphics.UI.WX
import Graphics.UI.WXCore
import Control.Concurrent (myThreadId, ThreadId)
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
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
                            ssOutput            :: SC.ScnEditor,           -- The output pane
                            ssDebugError        :: String -> IO (),
                            ssDebugWarn         :: String -> IO (),
                            ssDebugInfo         :: String -> IO (),
                            ssMainThreadId      :: ThreadId,
                            ssCompilerReport    :: TErrors,
                            ssFindText          :: TFindText,
                            ssTextWindows       :: TvTextWindows}
   
data FindText = FindText { ftText :: String, ftCurrFile :: String, ftCurrPos :: Int, ftStartFile :: String, ftStartPos :: Int }

-- compiler output channel
type TOutput = TChan ByteString

-- compiler errors
type TErrors = TVar [CompError]

type TFindText = TVar FindText

-- scheduled functions for timer event to run
type FunctionChannel = TChan (IO ())

type TvTextWindows = TVar TextWindows
data TextWindows = TextWindows { txWindows :: [TextWindow] }

data TextWindowType = SourceFile SC.ScnEditor | Ghci | Debug SC.ScnEditor | Output SC.ScnEditor
data MenuFunction = MenuFunction { mfId :: Int, mfFunction :: IO (), mfEnabled :: IO Bool  }

-- | Text Window
data TextWindow 
    = TextWindow {  twType              :: TextWindowType,
                    twPanel             :: Panel (),            -- ^ The parent panel of text window
                    twPanelHwnd         :: HWND,                -- ^ HWND of panel
                    twHwnd              :: HWND,
                    twMenuFunctions     :: [MenuFunction],
                    twHasFocus          :: IO Bool,
                    twIsClean           :: IO Bool,
                    twFilePath          :: Maybe String }       -- ^ File name associated with window

type SsNameMenuPair = (Int, MenuItem ())                               
type SsMenuList = [SsNameMenuPair]

-- compilation error
data CompError = CompError {    ceFilePath  :: String, 
                                ceSrcLine   :: Int, 
                                ceSrcCol    :: Int, 
                                ceErrLine   :: Int, -- line in compiler output
                                ceErrLines  :: [String] } deriving (Show)

----------------------------------------------------------------
-- Session  helpers
----------------------------------------------------------------

-- please call this on the main thread
ssCreate :: Frame () -> AuiManager () -> AuiNotebook () -> SsMenuList -> StatusField -> AuiNotebook () -> SC.ScnEditor -> SC.ScnEditor -> IO (Session)
ssCreate mf am nb ms sf ots ot db = do 
    mtid <- myThreadId
    tot  <- atomically $ newTChan
    cfn  <- atomically $ newTChan
    terr <- atomically $ newTVar []
    tfnd <- atomically $ newTVar (FindText "" "" 0 "" 0)
    let dbe = if CN.debug then (\s -> ssInvokeInGuiThread mtid cfn $ debugError db s) else (\s -> return ())
    let dbw = if CN.debug then (\s -> ssInvokeInGuiThread mtid cfn $ debugWarn  db s) else (\s -> return ())
    let dbi = if CN.debug then (\s -> ssInvokeInGuiThread mtid cfn $ debugInfo  db s) else (\s -> return ())
    tws  <- atomically $ newTVar $ twCreate [] 
    return (Session mf am nb ms sf tot cfn ots ot dbe dbw dbi mtid terr tfnd tws)

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
-- Source file helpers
----------------------------------------------------------------
  
{-                     
sfEditorHwnd :: SourceFile -> Word64                        
sfEditorHwnd (SourceFile _ _ e _ _) = MI.ptrToWord64 $ scnGetHwnd e                        

sfFilePathString :: SourceFile -> String                        
sfFilePathString sf = maybe "" id (sfFilePath sf)

sfSetFilePath :: SourceFile -> String -> SourceFile
sfSetFilePath (SourceFile p hp e _ ghci) fp = (SourceFile p hp e (Just fp) ghci)

sfMatchesHwnd :: SourceFile -> HWND -> Bool
sfMatchesHwnd sf h = MI.comparePtrs h (sfPanelHwnd sf)

sfCreate :: Panel() -> ScnEditor -> Maybe String -> Maybe GhciPanel -> IO SourceFile
sfCreate p e mfp ghci = do
    hp <- windowGetHandle p
    return (SourceFile p hp e mfp ghci)

sfSetGhciPanel :: SourceFile -> Maybe GhciPanel -> SourceFile
sfSetGhciPanel (SourceFile p hp e scn _) ghci = (SourceFile p hp e scn ghci)
 
sfIsInList :: String -> [SourceFile] -> Bool
sfIsInList fp fs = 
    case find (\sf -> sfPathIs sf (Just fp)) fs of
        Just _ -> True
        Nothing -> False
 
sfPathIs :: SourceFile -> Maybe String -> Bool
sfPathIs (SourceFile _ _ _ mfp1 _) mfp2 = fmap (map toLower) mfp1 == fmap (map toLower) mfp2

sfIsSame :: SourceFile -> SourceFile -> Bool
sfIsSame sf1 sf2 = (sfPanelHwnd sf1) == (sfPanelHwnd sf2)

-- updates the mutable project data to include the modified source file                        
sfUpdate :: Session -> SourceFile -> IO Project                        
sfUpdate ss sf' = do
    pr' <- prUpdate ss (\pr -> updateFile (prFiles pr) h)
    return (pr')
    
    where
        h = sfPanelHwnd sf'
        updateFile sfs h = prCreate (map (\sf -> if (sfMatchesHwnd sf h) then sf' else sf) sfs)

sfToString :: SourceFile -> String
sfToString (SourceFile _ hp e mfp _) = 
        "{SourceFile} Panel: 0x" ++ (showHex (MI.ptrToWord64 hp) "" ) ++
        ", (" ++ show (e) ++ "), " ++ 
        ", File: " ++ show (mfp)
       
sfGetSourceFile :: Session -> String -> IO (Maybe SourceFile)
sfGetSourceFile ss fp = do 
    fs <- ssReadSourceFiles ss
    let sf = find (\sf -> sfPathIs sf (Just fp)) fs
    return (sf)

sfCreateGhciPanel :: Panel () -> HWND -> GhciPanel
sfCreateGhciPanel p h = (GhciPanel p h)
-}

----------------------------------------------------------------
-- Project helpers
----------------------------------------------------------------
{-
prSetFiles :: Project -> [SourceFile] -> Project
prSetFiles (Project _) x = (Project x)

prCreate :: [SourceFile] -> Project
prCreate sfs = (Project sfs)

prToString :: TProject -> IO String
prToString tpr = do
    (Project fs) <- atomically $ readTVar tpr
    let ss = map sfToString fs
    let s = concat $ punctuate ", " ss
    return ("Files : " ++ s)
    
prUpdate :: Session -> (Project -> Project) -> IO Project
prUpdate ss f = atomically (modifyTVar tpr (\pr -> f pr) >> readTVar tpr) 
    where tpr = ssProject ss

prRead :: Session -> IO Project
prRead ss = atomically $ readTVar $ ssProject ss

prGetSourceFile :: Session -> String -> IO (Maybe SourceFile)
prGetSourceFile ss fp = do
    (Project fs) <- atomically $ readTVar $ ssProject ss
    return $ find (\sf -> sfPathIs sf (Just fp)) fs

prUpdateSourceFiles :: Session -> (SourceFile -> SourceFile) -> IO Project
prUpdateSourceFiles ss f = atomically (modifyTVar tpr (\pr -> prCreate $ map f (prFiles pr)) >> readTVar tpr)
    where tpr = ssProject ss
-}

----------------------------------------------------------------
-- Comp error
----------------------------------------------------------------

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

-- search string -> current file -> start file -> start pos
ftFindText :: String -> String -> Int -> String -> Int -> FindText
ftFindText text currFile currPos startFile startPos = (FindText text currFile currPos startFile startPos)

----------------------------------------------------------------
-- Text windows
----------------------------------------------------------------

twCreate :: [TextWindow] -> TextWindows
twCreate tws = (TextWindows tws)

createGhciWindowType :: TextWindowType
createGhciWindowType = (Ghci)

createSourceWindowType :: SC.ScnEditor -> TextWindowType
createSourceWindowType scn = (SourceFile scn)

createDebugWindowType :: SC.ScnEditor -> TextWindowType
createDebugWindowType scn = (Debug scn)

createOutputWindowType :: SC.ScnEditor -> TextWindowType
createOutputWindowType scn = (Output scn)

createTextWindow :: TextWindowType
                    -> Panel ()
                    -> HWND
                    -> HWND
                    -> [MenuFunction]
                    -> IO Bool      -- ^ Has focus
                    -> IO Bool      -- ^ Is clean
                    -> Maybe String -- ^ Filname
                    -> TextWindow
createTextWindow wtype panel hwndp hwnd mfs focus clean file =
    (TextWindow wtype panel hwndp hwnd mfs focus clean file)

twFindWindow :: Session -> (TextWindow -> Bool) -> IO (Maybe TextWindow)
twFindWindow ss p = do
    tws <- atomically (readTVar $ ssTextWindows ss)
    return $ find p $ txWindows tws

twUpdate :: Session -> ([TextWindow] -> [TextWindow]) -> IO TextWindows
twUpdate ss f = atomically (modifyTVar ttws (\tws -> (twCreate $ f $ txWindows tws)) >> readTVar ttws) 
    where ttws = ssTextWindows ss

createMenuFunction :: Int -> IO () -> IO Bool -> MenuFunction
createMenuFunction id mf me = (MenuFunction id mf me)

twGetMenuFunction :: TextWindow -> Int -> IO ()
twGetMenuFunction tw id = maybe (return ()) (\(MenuFunction _ mf _) -> mf)  $ find (\(MenuFunction mid _ _) -> mid == id) (twMenuFunctions tw)

twGetMenuEnabled :: TextWindow -> Int -> IO Bool
twGetMenuEnabled tw id = maybe (return False) (\(MenuFunction _ _ me) -> me)  $ find (\(MenuFunction mid _ _) -> mid == id) (twMenuFunctions tw)

twGetFocusedWindow :: Session -> IO (Maybe TextWindow)
twGetFocusedWindow ss = do
    tws <- atomically (readTVar $ ssTextWindows ss)
    MI.findIO (\tw -> twHasFocus tw) $ txWindows tws
  
twMatchesHwnd :: TextWindow -> HWND -> Bool
twMatchesHwnd tw h = MI.comparePtrs h (twPanelHwnd tw)

twGetWindows :: Session -> IO [TextWindow]
twGetWindows ss = do
    tws <- atomically (readTVar $ ssTextWindows ss) 
    return $ txWindows tws

twIsGhci :: TextWindow -> Bool
twIsGhci tw = case (twType tw) of
                Ghci      -> True
                otherwise -> False

twIsSourceFile :: TextWindow -> Bool
twIsSourceFile tw = case (twType tw) of
                SourceFile _ -> True
                otherwise    -> False

twIsDebug :: TextWindow -> Bool
twIsDebug tw = case (twType tw) of
                Debug _    -> True
                otherwise -> False

twIsOutput :: TextWindow -> Bool
twIsOutput tw = case (twType tw) of
                Output _  -> True
                otherwise -> False

twIsSameWindow :: TextWindow -> TextWindow -> Bool
twIsSameWindow tw1 tw2 = (twPanelHwnd tw1) == (twPanelHwnd tw2)

twIsSameFile :: TextWindow -> TextWindow -> Bool
twIsSameFile tw1 tw2 = fmap (map toLower) (twFilePath tw1) == fmap (map toLower) (twFilePath tw2)

twGetSourceFileWindow :: Session -> String -> IO (Maybe TextWindow)
twGetSourceFileWindow ss fp = 
    twFindWindow ss (\tw -> (twIsSourceFile tw) && (fmap (map toLower) (twFilePath tw) == Just ((map toLower) fp)))

twRemoveWindow :: Session -> TextWindow -> IO TextWindows
twRemoveWindow ss tw = twUpdate ss (\tws -> MI.findAndRemove (\tw' -> twIsSameWindow tw tw') tws)

twUpdateTextWindows :: Session -> (TextWindow -> TextWindow) -> IO TextWindows
twUpdateTextWindows ss f = atomically (modifyTVar tws (\ws -> twCreate $ map f (txWindows ws)) >> readTVar tws)
    where tws = ssTextWindows ss

twFilePathToString :: TextWindow -> String                        
twFilePathToString tw = maybe "" id (twFilePath tw)

twGetEditor :: TextWindow -> Maybe SC.ScnEditor
twGetEditor tw = 
    case twType tw of
        (SourceFile scn) -> Just scn
        (Debug scn)      -> Just scn
        (Output scn)     -> Just scn
        _                -> Nothing

