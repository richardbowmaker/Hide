

module Session 
(
    Session,
    Project,
    SourceFile,
    TOutput,
    FunctionChannel,
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
    ssOutput,
    ssDebug,
    ssMenuListNew,
    ssMenuListCreate,
    ssMenuListAdd,
    ssMenuListGet,
    ssReadSourceFiles,
    ssIsOpeningState,
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
    sfFilePathString,
    sfIsInList,
    sfPathIs,
    sfUpdate,
    sfIsSame,    
    sfMatchesHwnd,
    sfToString,    
--
    prFiles,
    prSetFiles,
    prCreate,
    prToString,
    prUpdate,
    prRead,
--
    otClear,
    otAddText,
    otAddLine    
) where


import Graphics.UI.WX
import Graphics.UI.WXCore
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Data.ByteString.Internal (ByteString)
import Data.String.Combinators (punctuate)
import Data.List (find)
import qualified Data.ByteString.Char8 as BS (pack)
import Data.Char (toLower)
import Data.Word (Word64)
import Numeric (showHex)

import Scintilla
import Misc

----------------------------------------------------------
-- Session data and ToString functions
----------------------------------------------------------

data Session = Session {    ssFrame   :: Frame (),            -- Main window
                            ssAuiMgr  :: AuiManager (),       -- Application level AUI manager
                            ssEditors :: AuiNotebook (),      -- Notebook of source file editors
                            ssProject :: TProject,            -- Project data (mutable)
                            ssMenus   :: SsMenuList,
                            ssStatus  :: StatusField,
                            ssTOutput :: TOutput,
                            ssCFunc   :: FunctionChannel,
                            ssOutput  :: ScnEditor,
                            ssDebug   :: ScnEditor}
                                                        
 -- project data is mutable
type TProject = TVar Project

-- compiler output channel
type TOutput = TChan ByteString

type FunctionChannel = TChan (IO ())

data Project = Project { prFiles :: [SourceFile] }

-- Session data
data SourceFile = SourceFile {  sfPanel     :: Panel (),        -- The panel added to the AuiNotebookManager
                                sfPanelHwnd :: Word64,          -- HWND of panel
                                sfEditor    :: ScnEditor,       -- The Scintilla editor, child window of panel
                                sfFilePath  :: Maybe String}    -- Source file path, Nothing = file name not set yet

type SsNameMenuPair = (String, MenuItem ())                               
type SsMenuList = [SsNameMenuPair]
                       
----------------------------------------------------------------
-- Session  helpers
----------------------------------------------------------------

ssCreate :: Frame () -> AuiManager () -> AuiNotebook () -> Project -> SsMenuList -> StatusField -> ScnEditor -> ScnEditor -> IO (Session)
ssCreate mf am nb pr ms sf ot db = do 
    tpr <- atomically $ newTVar $ prCreate []
    tot <- atomically $ newTChan
    cfn <- atomically $ newTChan
    return (Session mf am nb tpr ms sf tot cfn ot db)

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
ssIsOpeningState [sf@(SourceFile _ _ e Nothing)] = do
    b <- scnIsClean e
    return (b)
ssIsOpeningState _ = return (False)
               
----------------------------------------------------------------
-- Source file helpers
----------------------------------------------------------------
                       
sfEditorHwnd :: SourceFile -> Word64                        
sfEditorHwnd (SourceFile _ _ e _) = ptrToWord64 $ scnGetHwnd e                        

sfFilePathString :: SourceFile -> String                        
sfFilePathString sf = maybe "" id (sfFilePath sf)

sfSetFilePath :: SourceFile -> String -> SourceFile
sfSetFilePath (SourceFile p hp e _) fp = (SourceFile p hp e (Just fp))

sfMatchesHwnd :: SourceFile -> Word64 -> Bool
sfMatchesHwnd sf h = h == (sfPanelHwnd sf)

sfCreate :: Panel() -> ScnEditor -> Maybe String -> IO SourceFile
sfCreate p e mfp = do
    hp <- windowGetHandle p
    return (SourceFile p (ptrToWord64 hp) e mfp)
 
sfIsInList :: String -> [SourceFile] -> Bool
sfIsInList fp fs = 
    case find (\sf -> sfPathIs sf (Just fp)) fs of
        Just _ -> True
        Nothing -> False
 
sfPathIs :: SourceFile -> Maybe String -> Bool
sfPathIs (SourceFile _ _ _ mfp1) mfp2 = fmap (map toLower) mfp1 == fmap (map toLower) mfp2

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
sfToString (SourceFile _ hp e mfp) = 
        "{SourceFile} Panel: 0x" ++ (showHex hp "" ) ++
        ", (" ++ show (e) ++ "), " ++ 
        ", File: " ++ show (mfp)
        
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
prUpdate ss f = atomically (do
                        let tpr = ssProject ss
                        pr <- readTVar $ tpr
                        let pr' = f pr
                        writeTVar tpr pr'
                        return (pr))
                           
prRead :: Session -> IO Project
prRead ss = atomically $ readTVar $ ssProject ss

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
    
otAddLine :: Session -> ByteString -> IO ()
otAddLine ss bs = do
    let e = ssOutput ss
    scnSetReadOnly e False
    scnAppendLine e bs
    scnSetReadOnly e True


