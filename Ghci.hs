
module Ghci
( 
    openWindow,
    openWindowFile,
    closeWindow,
    closeAll,
    paste,
    cut,
    copy,
    selectAll,
    hasFocus,
    setFocus,
    getTextLength,
    getAllText,
    sendCommand,
    setEventHandler,
    enableEvents,
    disableEvents,
    eventGotFocus,
    eventLostFocus,
    eventSelectionSet,
    eventSelectionClear
) where 
  

import Control.Concurrent 
import Control.Concurrent.STM
import Control.Monad (mapM_) 
import qualified Data.ByteString as BS (init, replicate)
import qualified Data.ByteString.Internal as BS (ByteString)
import qualified Data.ByteString.Unsafe as BS (unsafeUseAsCString)
import qualified Data.ByteString.Char8 as BS (unpack, take, writeFile)
import Data.Bits ((.|.))
import Data.List (find, findIndex)
import Data.Word (Word64)
import Data.Int (Int32)
import Foreign.C.String (CString, withCString)
import Foreign.Ptr (FunPtr, Ptr, minusPtr, nullPtr)
import Graphics.Win32.GDI.Types (HWND)
import Graphics.UI.WX
import Graphics.UI.WXCore
import System.FilePath.Windows (takeFileName, takeDirectory)
import System.IO
import System.Win32.Types (nullHANDLE)

-- project imports

import qualified Constants as CN
import qualified Misc as MI
import qualified Session as SS

-----------------------
-- Windows API calls --
-----------------------

-- imports from ScintillaProxy.dll
foreign import ccall safe "GhciNew"             c_GhciNew               :: HWND -> CString -> CString -> IO HWND 
foreign import ccall safe "GhciSetEventHandler" c_GhciSetEventHandler   :: HWND -> FunPtr (HWND -> Int -> IO ()) -> IO ()
foreign import ccall safe "GhciEnableEvents"    c_GhciEnableEvents      :: HWND -> IO ()
foreign import ccall safe "GhciDisableEvents"   c_GhciDisableEvents     :: HWND -> IO ()
foreign import ccall safe "GhciClose"           c_GhciClose             :: HWND -> IO ()
foreign import ccall safe "GhciPaste"           c_GhciPaste             :: HWND -> IO ()
foreign import ccall safe "GhciCut"             c_GhciCut               :: HWND -> IO () 
foreign import ccall safe "GhciCopy"            c_GhciCopy              :: HWND -> IO ()
foreign import ccall safe "GhciSelectAll"       c_GhciSelectAll         :: HWND -> IO () 
foreign import ccall safe "GhciHasFocus"        c_GhciHasFocus          :: HWND -> IO Int32 
foreign import ccall safe "GhciSetFocus"        c_GhciSetFocus          :: HWND -> IO () 
foreign import ccall safe "GhciSendCommand"     c_GhciSendCommand       :: HWND -> CString -> IO HWND 
foreign import ccall safe "GhciIsTextSelected"  c_GhciIsTextSelected    :: HWND -> IO Int32
foreign import ccall safe "GhciGetTextLength"   c_GhciGetTextLength     :: HWND -> IO Int32 
foreign import ccall safe "GhciGetText"         c_GhciGetText           :: HWND -> CString -> Int32 -> IO Int32 

-- callback wrapper
foreign import ccall safe "wrapper" createCallback ::
    (HWND -> Int -> IO ()) -> IO (FunPtr (HWND -> Int -> IO ()))

--------------------------------------------------------------------------

openWindowFile :: SS.Session -> SS.TextWindow -> IO ()
openWindowFile ss ftw = do
    mtw <- SS.twFindWindow ss (\tw -> (SS.twIsGhci tw) && (SS.twIsSameFile ftw tw)) 
    case mtw of
        Just tw -> do
            -- GHCI already open so select it
            let nb = SS.ssOutputs ss
            auiNotebookGetPageIndex nb (SS.twPanel tw) >>= auiNotebookSetSelection nb
            -- reload the source file
            sendCommand (SS.twPanelHwnd tw) $ ":load " ++ (maybe "" id (SS.twFilePath tw))
        Nothing -> do
            -- GHCI not open so open a new tab
            case (SS.twFilePath ftw) of
                Just fp -> do
                    mw <- open ss fp                 
                    case mw of
                        Just (panel, hwndp, hwnd) -> do
                                let hw = createHideWindow ss panel hwndp hwnd (Just fp)
                                SS.hwUpdate ss (\hws -> hw : hws)
                                setEventHandler ss hw
                                enableEvents hwnd
                                setFocus hwnd
                                return ()
                        Nothing -> return ()
                Nothing -> return ()

openWindow :: SS.Session -> IO ()
openWindow ss = do
    m <- open ss "" 
    case m of
        Just (panel, hwndp, hwnd) -> do
            let hw = createHideWindow ss panel hwndp hwnd Nothing
            SS.hwUpdate ss (\hws -> hw : hws)
            setEventHandler ss hw
            enableEvents hwnd
            setFocus hwnd
            return ()
        Nothing -> return ()

open :: SS.Session -> String -> IO (Maybe (Panel (), HWND, HWND))
open ss fp = do

    -- create panel and embed GHCI window
    let nb = SS.ssOutputs ss
    p <- panel nb []
    hp <- windowGetHandle p
    hwnd <- withCString fp (\cfp -> 
        withCString "-fasm -L. -lScintillaProxy -threaded" (\cop -> c_GhciNew hp cop cfp))

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
    windowGetHandle p >>= c_GhciClose
    SS.twRemoveWindow ss tw
    return ()

closeAll :: SS.Session -> IO ()
closeAll ss = SS.hwFindWindows ss SS.hwIsGhci >>= mapM_ (\hw -> closeWindow ss (SS.hwWindow hw))

createHideWindow :: SS.Session -> Panel() -> HWND -> HWND -> Maybe String -> SS.HideWindow
createHideWindow ss panel phwnd hwnd mfp = SS.createHideWindow tw tms

    where   tw = SS.createTextWindow SS.createGhciWindowType panel phwnd hwnd mfp
            tms = SS.createTextMenus 
                    [ 
                        (SS.createMenuFunction CN.menuFileClose      (closeWindow ss tw) (return True)),
                        (SS.createMenuFunction CN.menuFileCloseAll   (closeAll ss)       (return True)),
                        (SS.createMenuFunction CN.menuFileSave       (fileSaveAs ss tw)  (return True)),
                        (SS.createMenuFunction CN.menuEditCut        (cut hwnd)          (return False)),
                        (SS.createMenuFunction CN.menuEditCopy       (copy hwnd)         (isTextSelected hwnd)),
                        (SS.createMenuFunction CN.menuEditPaste      (paste hwnd)        (return True)),
                        (SS.createMenuFunction CN.menuEditSelectAll  (selectAll hwnd)    (return True))
                    ]
                    (hasFocus hwnd)
                    (return True)
                    (return "")

sendCommand :: HWND -> String -> IO ()
sendCommand hwnd cmd = withCString cmd (\cs -> c_GhciSendCommand hwnd cs) >> return ()

paste :: HWND -> IO ()
paste = c_GhciPaste

cut :: HWND -> IO ()
cut = c_GhciCut

copy :: HWND -> IO ()
copy = c_GhciCopy

selectAll :: HWND -> IO ()
selectAll = c_GhciSelectAll

isTextSelected :: HWND -> IO Bool
isTextSelected hwnd = do
    n <- c_GhciIsTextSelected hwnd
    return (n /= 0)

hasFocus :: HWND -> IO Bool
hasFocus h = do 
        b <- c_GhciHasFocus h
        return (b /= 0)

setFocus :: HWND -> IO ()
setFocus = c_GhciSetFocus

getTextLength :: HWND -> IO Int
getTextLength hwnd = do 
    n <- c_GhciGetTextLength hwnd
    return (fromIntegral n :: Int)

getAllText :: HWND -> IO BS.ByteString
getAllText hwnd = do            
    len <- getTextLength hwnd
    let bs = (BS.replicate len 0)   -- allocate buffer
    len' <- BS.unsafeUseAsCString bs (\cs -> do c_GhciGetText hwnd cs (fromIntegral len :: Int32))
    if len == (fromIntegral len' :: Int) then return bs
    else return $ BS.take (fromIntegral len' :: Int) bs

-- File Save As, returns False if user opted to cancel the save 
fileSaveAs :: SS.Session -> SS.TextWindow -> IO ()
fileSaveAs ss tw = do 
    SS.ssDebugInfo ss $ "fileSaveAs :: filename " ++ (maybe "" id (SS.twFilePath tw)) 
    -- prompt user for name to save to                   
    fd <- fileDialogCreate 
        (SS.ssFrame ss)
        "Save GHCI as" 
        (maybe "." takeDirectory (SS.twFilePath tw))
        (maybe "" id (SS.twFilePath tw)) 
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
            let tw' = SS.twSetFilePath tw fp 
            SS.hwUpdateWindow ss 
                (\hw -> if SS.twIsSameWindow tw' (SS.hwWindow hw) 
                        then Just $ SS.createHideWindow tw' (SS.hwMenus hw) 
                        else Nothing)
            return ()  
        otherwise -> return ()
    return ()
   
setEventHandler :: SS.Session -> SS.HideWindow -> IO ()
setEventHandler ss hw = do
    cb <- createCallback (callback ss hw)
    c_GhciSetEventHandler (SS.hwPanelHwnd hw) cb    
    return ()

enableEvents :: HWND -> IO ()
enableEvents = c_GhciEnableEvents

disableEvents :: HWND -> IO ()
disableEvents = c_GhciDisableEvents
    
callback :: SS.Session -> SS.HideWindow -> HWND -> Int -> IO ()
callback ss hw hwnd evt 
    | evt == eventLostFocus = do
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
    | evt == eventGotFocus  = do
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
    | evt == eventSelectionSet || evt == eventSelectionClear = do
            setm ss tms CN.menuEditCut           
            setm ss tms CN.menuEditCopy          
            setm ss tms CN.menuEditPaste
    | otherwise = return ()

        where   setm :: SS.Session -> SS.TextMenus -> Int -> IO ()
                setm ss tw mid = setm' ss mid (SS.tmGetMenuEnabled tw mid) (SS.tmGetMenuFunction tw mid)
 
                setm' :: SS.Session -> Int -> IO Bool -> IO () -> IO ()
                setm' ss mid me mf = do 
                    e <- me
                    set (SS.ssMenuListGet ss mid) [on command := mf, enabled := e]

                tms = SS.hwMenus hw

eventGotFocus :: Int
eventGotFocus = 1

eventLostFocus :: Int
eventLostFocus = 2

eventSelectionSet :: Int
eventSelectionSet = 3

eventSelectionClear :: Int
eventSelectionClear = 4

eventClosed :: Int
eventClosed = 5

  
