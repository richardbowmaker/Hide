module Session 
(
    Session,
    Project,
    SourceFile,
    TOutput,
    FunctionChannel,
    TErrors,
    ssProgramTitle,
--
    ssCreate,
    ssFrame,
    ssAuiMgr,
    ssEditors,
    ssProject,
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
    ssReadSourceFiles,
    ssIsOpeningState,
    ssCompilerReport,
    ssFindText,
    ssToString,
    SsMenuList,
    SsNameMenuPair,
--
    sfCreate,
    sfSetFilePath,
    sfPanel,
    sfPanelHwnd,
    sfEditor,
    sfFilePath,
    sfGhci,
    sfFilePathString,
    sfIsInList,
    sfPathIs,
    sfUpdate,
    sfIsSame,    
    sfMatchesHwnd,
    sfToString, 
    sfGetSourceFile,
    sfSetGhciPanel,
--
    prFiles,
    prSetFiles,
    prCreate,
    prToString,
    prUpdate,
    prUpdateSourceFiles,
    prRead,
    prGetSourceFile,
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
    otClear,
    otAddText,
    otAddLine,
--
    FindText,
    ftFindText,
    ftText,
    ftCurrFile,
    ftCurrPos,
    ftStartFile,
    ftStartPos, 
--
    GhciPanel,
    ghciPanel,
    ghciHwnd,
    sfCreateGhciPanel,
--
    createGhciWindowType,
    createTextWindow,
    twCreate,
    twFind,
    TextWindow,
    TextWindowType,
    twType,
    twPanel,
    twPanelHwnd,
    twCut,
    twCopy,
    twPaste,
    twSelectAll,
    twUndo,
    twRedo,
    twCanCut,
    twCanCopy,      
    twCanPaste,
    twCanSelectAll,
    twCanUndo,
    twCanRedo,
    twFilePath,
    twUpdateTextWindows,
    twUpdate,
    txWindows
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

import Debug
import Misc
import Scintilla

----------------------------------------------------------------
-- Globals
----------------------------------------------------------------

debug = True

ssProgramTitle :: String
ssProgramTitle = "HIDE"
                     
----------------------------------------------------------
-- Session data and ToString functions
----------------------------------------------------------

data Session = Session {    ssFrame             :: Frame (),            -- Main window
                            ssAuiMgr            :: AuiManager (),       -- Application level AUI manager
                            ssEditors           :: AuiNotebook (),      -- Notebook of source file editors
                            ssProject           :: TProject,            -- Project data (mutable)
                            ssMenus             :: SsMenuList,
                            ssStatus            :: StatusField,
                            ssTOutput           :: TOutput,             -- TCHan for output pane, e.g. compiler output
                            ssCFunc             :: FunctionChannel,     -- TChan for scheduling functions to be called in main GUI thread (see timer)
                            ssOutputs           :: AuiNotebook (),      -- The output panes notebook, includes ssOutput pane below
                            ssOutput            :: ScnEditor,           -- The output pane
                            ssDebugError        :: String -> IO (),
                            ssDebugWarn         :: String -> IO (),
                            ssDebugInfo         :: String -> IO (),
                            ssMainThreadId      :: ThreadId,
                            ssCompilerReport    :: TErrors,
                            ssFindText          :: TFindText,
                            ssTextWindows       :: TvTextWindows}
   
data FindText = FindText { ftText :: String, ftCurrFile :: String, ftCurrPos :: Int, ftStartFile :: String, ftStartPos :: Int }

 -- project data is mutable
type TProject = TVar Project

-- compiler output channel
type TOutput = TChan ByteString

-- compiler errors
type TErrors = TVar [CompError]

type TFindText = TVar FindText

-- scheduled functions for timer event to run
type FunctionChannel = TChan (IO ())

type TvTextWindows = TVar TextWindows
data TextWindows = TextWindows { txWindows :: [TextWindow] }
data Project = Project { prFiles :: [SourceFile] }

-- | Source File Data
data SourceFile 
    = SourceFile {  sfPanel     :: Panel (),            -- ^ The panel added to the AuiNotebookManager
                    sfPanelHwnd :: HWND,                -- ^ HWND of panel
                    sfEditor    :: ScnEditor,           -- ^ The Scintilla editor, child window of panel
                    sfFilePath  :: Maybe String,        -- ^ Source file path, Nothing = file name not set yet
                    sfGhci      :: Maybe GhciPanel }    -- ^ Parent panel of GHCI window in the output pane

data GhciPanel = GhciPanel { ghciPanel :: Panel (), ghciHwnd :: HWND } 

data TextWindowType = Scintilla ScnEditor | Ghci | Debug ScnEditor | Output ScnEditor

-- | Text Window
data TextWindow 
    = TextWindow {  twType          :: TextWindowType,
                    twPanel         :: Panel (),            -- ^ The parent panel of text window
                    twPanelHwnd     :: HWND,                -- ^ HWND of panel
                    twCut           :: IO (),               -- ^ Function to perform cut operation
                    twCopy          :: IO (),
                    twPaste         :: IO (),
                    twSelectAll     :: IO (),
                    twUndo          :: IO (),
                    twRedo          :: IO (),
                    twCanCut        :: IO Bool,             -- ^ Whether the cut is currently valid, i.e. text is selected
                    twCanCopy       :: IO Bool,              
                    twCanPaste      :: IO Bool,
                    twCanSelectAll  :: IO Bool,
                    twCanUndo       :: IO Bool,
                    twCanRedo       :: IO Bool,
                    twFilePath      :: Maybe String }       -- ^ File name associated with window

type SsNameMenuPair = (String, MenuItem ())                               
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
ssCreate :: Frame () -> AuiManager () -> AuiNotebook () -> Project -> SsMenuList -> StatusField -> AuiNotebook () -> ScnEditor -> ScnEditor -> IO (Session)
ssCreate mf am nb pr ms sf ots ot db = do 
    mtid <- myThreadId
    tpr  <- atomically $ newTVar $ prCreate []
    tot  <- atomically $ newTChan
    cfn  <- atomically $ newTChan
    terr <- atomically $ newTVar []
    tfnd <- atomically $ newTVar (FindText "" "" 0 "" 0)
    let dbe = if debug then (\s -> ssInvokeInGuiThread mtid cfn $ debugError db s) else (\s -> return ())
    let dbw = if debug then (\s -> ssInvokeInGuiThread mtid cfn $ debugWarn  db s) else (\s -> return ())
    let dbi = if debug then (\s -> ssInvokeInGuiThread mtid cfn $ debugInfo  db s) else (\s -> return ())
    tws  <- atomically $ newTVar $ twCreate [] 
    return (Session mf am nb tpr ms sf tot cfn ots ot dbe dbw dbi mtid terr tfnd tws)

-- creates a new menu item lookup list
-- a dummy entry is provided for failed lookups to simplfy client calls to menuListGet 
ssMenuListNew :: IO SsMenuList 
ssMenuListNew = do
    mi <- menuItemCreate
    return ([("", mi)])

ssMenuListCreate :: [SsNameMenuPair] -> IO SsMenuList
ssMenuListCreate nmps = do
    ml <- ssMenuListNew
    return (ssMenuListAdd nmps ml)

ssMenuListAdd :: [SsNameMenuPair] -> SsMenuList -> SsMenuList
ssMenuListAdd nmps ml = ml ++ nmps

ssMenuListGet :: Session -> String -> MenuItem ()
ssMenuListGet ss s = 
    case (lookup s ml) of
        Just mi -> mi
        Nothing -> snd $ last ml
    where ml = ssMenus ss
    
ssReadSourceFiles :: Session -> IO [SourceFile]
ssReadSourceFiles ss = do 
    pr <- prRead ss 
    return (prFiles pr)

ssToString :: Session -> IO String
ssToString ss = do
    fs <- frameToString $ ssFrame ss
    prs <- prToString $ ssProject ss
    return ("{Session} Main: " ++ fs ++ prs)

ssIsOpeningState :: [SourceFile] -> IO Bool
ssIsOpeningState [sf@(SourceFile _ _ e Nothing _)] = do
    b <- scnIsClean e
    return (b)
ssIsOpeningState _ = return (False)

ssInvokeInGuiThread :: ThreadId -> FunctionChannel -> (IO ()) -> IO ()
ssInvokeInGuiThread mtid chan f = do
    tid <- myThreadId
    if mtid == tid then f
    else atomically $ writeTChan chan f
             
----------------------------------------------------------------
-- Source file helpers
----------------------------------------------------------------
                       
sfEditorHwnd :: SourceFile -> Word64                        
sfEditorHwnd (SourceFile _ _ e _ _) = ptrToWord64 $ scnGetHwnd e                        

sfFilePathString :: SourceFile -> String                        
sfFilePathString sf = maybe "" id (sfFilePath sf)

sfSetFilePath :: SourceFile -> String -> SourceFile
sfSetFilePath (SourceFile p hp e _ ghci) fp = (SourceFile p hp e (Just fp) ghci)

sfMatchesHwnd :: SourceFile -> HWND -> Bool
sfMatchesHwnd sf h = comparePtrs h (sfPanelHwnd sf)

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
        "{SourceFile} Panel: 0x" ++ (showHex (ptrToWord64 hp) "" ) ++
        ", (" ++ show (e) ++ "), " ++ 
        ", File: " ++ show (mfp)
       
sfGetSourceFile :: Session -> String -> IO (Maybe SourceFile)
sfGetSourceFile ss fp = do 
    fs <- ssReadSourceFiles ss
    let sf = find (\sf -> sfPathIs sf (Just fp)) fs
    return (sf)

sfCreateGhciPanel :: Panel () -> HWND -> GhciPanel
sfCreateGhciPanel p h = (GhciPanel p h)

----------------------------------------------------------------
-- Project helpers
----------------------------------------------------------------

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
----------------------------------------------------------------
-- Output helpers
----------------------------------------------------------------

otClear :: Session -> IO ()
otClear ss = do
    let e = ssOutput ss
    scnSetReadOnly e False
    scnClearAll e
    scnSetReadOnly e True

otAddText :: Session -> ByteString -> IO ()
otAddText ss bs = do
    let e = ssOutput ss
    scnSetReadOnly e False
    scnAppendText e bs
    scnSetReadOnly e True
    scnShowLastLine e
    
otAddLine :: Session -> ByteString -> IO ()
otAddLine ss bs = do
    let e = ssOutput ss
    scnSetReadOnly e False
    scnAppendLine e bs
    scnSetReadOnly e True
    scnShowLastLine e

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

createTextWindow :: TextWindowType
                    -> Panel ()
                    -> HWND
                    -> IO ()        -- ^ Cut function
                    -> IO ()        -- ^ Copy
                    -> IO ()        -- ^ Paste
                    -> IO ()        -- ^ Select all
                    -> IO ()        -- ^ Undo
                    -> IO ()        -- ^ Redo
                    -> IO Bool      -- ^ Cut valid
                    -> IO Bool      -- ^ Copy
                    -> IO Bool      -- ^ Paste
                    -> IO Bool      -- ^ Select all
                    -> IO Bool      -- ^ Undo
                    -> IO Bool      -- ^ Redo
                    -> Maybe String -- ^ Filname
                    -> TextWindow
createTextWindow wtype p hp cuf cof paf saf unf ref cuv cov pav sav unv rev mfp =
    (TextWindow wtype p hp cuf cof paf saf unf ref cuv cov pav sav unv rev mfp)

twFind :: Session -> (TextWindow -> Bool) -> IO (Maybe TextWindow)
twFind ss p = do
    tws <- atomically (readTVar $ ssTextWindows ss)
    return $ find p $ txWindows tws

twUpdateTextWindows :: Session -> (TextWindow -> TextWindow) -> IO TextWindows
twUpdateTextWindows ss f = atomically (modifyTVar tws (\ws -> twCreate $ map f (txWindows ws)) >> readTVar tws)
    where tws = ssTextWindows ss

twUpdate :: Session -> (TextWindows -> TextWindows) -> IO TextWindows
twUpdate ss f = atomically (modifyTVar tws (\ws -> f ws) >> readTVar tws) 
    where tws = ssTextWindows ss


