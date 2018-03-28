
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

import qualified Data.ByteString as BS (init, replicate)
import qualified Data.ByteString.Char8 as BS (unpack, take, writeFile)
import qualified Data.ByteString.Internal as BS (ByteString)
import qualified Data.ByteString.Unsafe as BS (unsafeUseAsCString)
import Control.Concurrent 
import Control.Concurrent.STM
import Control.Monad (mapM_, liftM, liftM2) 
import Data.Bits ((.|.))
import Data.Int (Int32)
import Data.List (find, findIndex)
import Data.Maybe (isJust)
import Data.Word (Word64)
import Foreign.C.String (CString, withCString, peekCString)
import Foreign.Ptr (FunPtr, Ptr, minusPtr, nullPtr)
import Graphics.UI.WX
import Graphics.UI.WXCore
import Graphics.Win32.GDI.Types (HWND)
import System.FilePath.Windows (takeFileName, takeDirectory)
import System.IO
import System.Win32.Types (nullHANDLE)

-- project imports

import qualified Constants as CN
import qualified ScintillaProxyImports as SI
import qualified Misc as MI
import qualified Scintilla as SC
import qualified Session as SS

openWindowFile :: SS.Session -> SS.TextWindow -> IO (Maybe SS.TextWindow)
openWindowFile ss ftw = do
    mtw <- SS.twFindWindow ss (\tw -> liftM2 (&&) (return $ SS.twIsGhci tw) (SS.twIsSameFile ftw tw)) 
    case mtw of
        Just tw -> do
            -- GHCI already open so select it
            let nb = SS.ssOutputs ss
            auiNotebookGetPageIndex nb (SS.twPanel tw) >>= auiNotebookSetSelection nb
            -- reload the source file
            mfp <- SS.twFilePath tw
            sendCommand (SS.twPanelHwnd tw) $ ":load " ++ (maybe "" id mfp)
            return (Just tw)
        Nothing -> do
            -- GHCI not open so open a new tab
            mfp <- SS.twFilePath ftw
            case mfp of
                Just fp -> do
                    mw <- open ss fp                 
                    case mw of
                        Just (panel, hwndp, hwnd) -> do
                                hw <- createHideWindow ss panel hwndp hwnd (Just fp)
                                SS.hwUpdate ss (\hws -> hw : hws)
                                setEventHandler ss hw
                                enableEvents hwnd
                                setFocus hwnd
                                return (Just $ SS.hwWindow hw)
                        Nothing -> return Nothing
                Nothing -> return Nothing

openWindow :: SS.Session -> IO (Maybe SS.TextWindow)
openWindow ss = do
    m <- open ss "" 
    case m of
        Just (panel, hwndp, hwnd) -> do
            hw <- createHideWindow ss panel hwndp hwnd Nothing
            SS.hwUpdate ss (\hws -> hw : hws)
            setEventHandler ss hw
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
    hwnd <- withCString fp (\cfp -> 
        withCString "-fasm -L. -lScintillaProxy -threaded" (\cop -> SI.c_GhciNew hp cop cfp))

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
    windowGetHandle p >>= SI.c_GhciClose
    SS.twRemoveWindow ss tw
    return ()

closeAll :: SS.Session -> IO ()
closeAll ss = SS.hwFindWindows ss SS.hwIsGhci >>= mapM_ (\hw -> closeWindow ss (SS.hwWindow hw))

createHideWindow :: SS.Session -> Panel() -> HWND -> HWND -> Maybe String -> IO SS.HideWindow
createHideWindow ss panel phwnd hwnd mfp = do
    tw <- SS.createTextWindow SS.createGhciWindowType panel phwnd hwnd mfp
    return $ SS.createHideWindow tw (tms tw)

    where  tms tw = SS.createTextMenus 
                    [ 
                        (SS.createMenuFunction CN.menuFileClose      (closeWindow ss tw)                    (return True)),
                        (SS.createMenuFunction CN.menuFileCloseAll   (closeAll ss)                          (return True)),
                        (SS.createMenuFunction CN.menuFileSaveAs     (fileSaveAs ss tw)                     (return True)),
                        (SS.createMenuFunction CN.menuEditCut        (cut hwnd)                             (isTextSelected hwnd)),
                        (SS.createMenuFunction CN.menuEditCopy       (copy hwnd)                            (isTextSelected hwnd)),
                        (SS.createMenuFunction CN.menuEditPaste      (paste hwnd)                           (return True)),
                        (SS.createMenuFunction CN.menuEditSelectAll  (selectAll hwnd)                       (return True)),
                        (SS.createMenuFunction CN.menuEditClear      (clear hwnd)                           (return True)),
                        (SS.createMenuFunction CN.menuBuildGhci      (openWindowFile ss tw >> return ())    (liftM (isJust) (SS.twFilePath tw)))
                    ]
                    (hasFocus hwnd)
                    (return True)
                    (return "")

sendCommand :: HWND -> String -> IO ()
sendCommand hwnd cmd = withCString cmd (\cs -> SI.c_GhciSendCommand hwnd cs) >> return ()

paste :: HWND -> IO ()
paste = SI.c_GhciPaste

cut :: HWND -> IO ()
cut = SI.c_GhciCut

copy :: HWND -> IO ()
copy = SI.c_GhciCopy

selectAll :: HWND -> IO ()
selectAll = SI.c_GhciSelectAll

isTextSelected :: HWND -> IO Bool
isTextSelected hwnd = do
    n <- SI.c_GhciIsTextSelected hwnd
    return (n /= 0)

hasFocus :: HWND -> IO Bool
hasFocus h = do 
        b <- SI.c_GhciHasFocus h
        return (b /= 0)

setFocus :: HWND -> IO ()
setFocus = SI.c_GhciSetFocus

getTextLength :: HWND -> IO Int
getTextLength hwnd = do 
    n <- SI.c_GhciGetTextLength hwnd
    return (fromIntegral n :: Int)

getAllText :: HWND -> IO BS.ByteString
getAllText hwnd = do            
    len <- getTextLength hwnd
    let bs = (BS.replicate len 0)   -- allocate buffer
    len' <- BS.unsafeUseAsCString bs (\cs -> do SI.c_GhciGetText hwnd cs (fromIntegral len :: Int32))
    if len == (fromIntegral len' :: Int) then return bs
    else return $ BS.take (fromIntegral len' :: Int) bs

clear :: HWND -> IO ()
clear = SI.c_GhciClear

-- File Save As, returns False if user opted to cancel the save 
fileSaveAs :: SS.Session -> SS.TextWindow -> IO ()
fileSaveAs ss tw = do 
    -- prompt user for name to save to
    mfp <- SS.twFilePath tw
    fd <- fileDialogCreate 
        (SS.ssFrame ss)
        "Save GHCI as" 
        (maybe "." takeDirectory mfp)
        (maybe "" id mfp) 
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
            SS.twSetFilePath tw fp 
            return ()  
        otherwise -> return ()
   
setEventHandler :: SS.Session -> SS.HideWindow -> IO ()
setEventHandler ss hw = do
    cb <- SI.c_GhciCreateCallback (callback ss hw)
    SI.c_GhciSetEventHandler (SS.hwPanelHwnd hw) cb    
    return ()

enableEvents :: HWND -> IO ()
enableEvents = SI.c_GhciEnableEvents

disableEvents :: HWND -> IO ()
disableEvents = SI.c_GhciDisableEvents
    
callback :: SS.Session -> SS.HideWindow -> HWND -> Int -> CString -> IO ()
callback ss hw hwnd evt str
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
            setm' ss CN.menuEditClear         (return False) (return ())
            setm' ss CN.menuBuildGhci         (return False) (return ())
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
            setm ss tms CN.menuEditClear          
            setm ss tms CN.menuBuildGhci         
    | evt == eventSelectionSet || evt == eventSelectionClear = do
            setm ss tms CN.menuEditCut           
            setm ss tms CN.menuEditCopy          
            setm ss tms CN.menuEditPaste
    | evt == eventOutput = do
            s <- peekCString str
            SS.ssDebugInfo ss $ "output = " ++ s    
    | evt == eventInput = do
            s <- peekCString str
            SS.ssDebugInfo ss $ "input = " ++ s
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

eventOutput :: Int
eventOutput = 6
  
eventInput :: Int
eventInput = 7
  
