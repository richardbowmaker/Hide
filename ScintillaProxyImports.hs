
module ScintillaProxyImports
(
    SCNotification,
    c_ScnNewEditor,
    c_ScnDestroyEditor,
    c_ScnSetEventHandler,
    c_ScnEnableEvents,
    c_ScnDisableEvents,
    c_ScnAddPopupMenuItem,
    c_ScnCreateCallback,
    c_ScnCreateHandlerCallback,
    c_ScnCreateEnabledCallback,
    c_GhciTerminalNew,
    c_GhciTerminalSetEventHandler,
    c_GhciTerminalEnableEvents,
    c_GhciTerminalDisableEvents,
    c_GhciTerminalClose,
    c_GhciTerminalPaste,
    c_GhciTerminalCut,
    c_GhciTerminalCopy,
    c_GhciTerminalSelectAll,
    c_GhciTerminalHasFocus,
    c_GhciTerminalSetFocus,
    c_GhciTerminalSendCommand,
    c_GhciTerminalIsTextSelected,
    c_GhciTerminalGetTextLength,
    c_GhciTerminalGetText,
    c_GhciTerminalClear,
    c_GhciTerminalCreateCallback,
    ghciNew,
    ghciClose,
    ghciSetEventHandler,
    ghciSendCommand,
    ghciSendCommandAsynch,
    ghciSendCommandSynch,
    ghciWaitForResponse,
    initialise,
    uninitialise,
    notifyGetHwnd,
    notifyGetCode,
    notifyGetPosition,
    notifyGetWParam,
    notifyGetListCompletionMethod,
    snLine,
    snLinesAdded,
    snModificationType,
    snPosition,
    snUpdated,
    sciSendEditorII,
    sciSendEditorIO,
    sciSendEditorSI,
    sciSendEditorSO,
    sciSendEditorIB
) where 
  
import Control.Monad (liftM)
import Data.Int (Int32, Int64)
import Data.Word (Word32, Word64)
import Foreign.C.String (CString, peekCString, withCString, withCStringLen)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (fromBool)
import Foreign.Ptr (FunPtr, Ptr, minusPtr, nullPtr)
import Foreign.Storable (Storable, alignment, sizeOf, peek, poke, pokeByteOff, peekByteOff)
import Graphics.Win32.GDI.Types (HWND)

foreign import ccall safe "ScintillaProxyInitialise"   c_ScintillaProxyInitialise :: IO Bool     
foreign import ccall safe "ScintillaProxyUninitialise" c_ScintillaProxyUninitialise :: IO ()   

-- imports from ScintillaProxy.dll
foreign import ccall safe "ScnNewEditor"        c_ScnNewEditor          :: HWND -> IO (HWND)      
foreign import ccall safe "ScnDestroyEditor"    c_ScnDestroyEditor      :: HWND -> IO ()      
foreign import ccall safe "ScnSetEventHandler"  c_ScnSetEventHandler    :: HWND -> FunPtr (Ptr (SCNotification) -> IO ()) -> IO ()
foreign import ccall safe "ScnEnableEvents"     c_ScnEnableEvents       :: HWND -> IO ()
foreign import ccall safe "ScnDisableEvents"    c_ScnDisableEvents      :: HWND -> IO ()      
foreign import ccall safe "ScnAddPopupMenuItem" c_ScnAddPopupMenuItem   :: 
    HWND -> Int32 -> CString -> FunPtr (Int -> IO ()) -> FunPtr (Int -> IO Int) -> IO ()      

-- direct call to Scintilla, different aliases simplify conversion to WPARAM and LPARAM types 
foreign import ccall safe "ScnSendEditor" c_ScnSendEditorI :: HWND -> Word32 -> Word64 -> Int64 -> IO Int64
foreign import ccall safe "ScnSendEditor" c_ScnSendEditorS :: HWND -> Word32 -> Word64 -> CString -> IO Int64

-- callback wrappers
foreign import ccall safe "wrapper" c_ScnCreateCallback ::
    (Ptr (SCNotification) -> IO ()) -> IO (FunPtr (Ptr (SCNotification) -> IO ()))

foreign import ccall safe "wrapper" c_ScnCreateHandlerCallback ::
    (Int -> IO ()) -> IO (FunPtr (Int -> IO ()))

foreign import ccall safe "wrapper" c_ScnCreateEnabledCallback ::
    (Int -> IO Int) -> IO (FunPtr (Int -> IO Int))
  
foreign import ccall safe "GhciTerminalNew"             c_GhciTerminalNew               :: HWND -> CString -> CString -> IO HWND 
foreign import ccall safe "GhciTerminalSetEventHandler" c_GhciTerminalSetEventHandler   :: HWND -> FunPtr (HWND -> Int -> CString -> IO ()) -> IO ()
foreign import ccall safe "GhciTerminalEnableEvents"    c_GhciTerminalEnableEvents      :: HWND -> IO ()
foreign import ccall safe "GhciTerminalDisableEvents"   c_GhciTerminalDisableEvents     :: HWND -> IO ()
foreign import ccall safe "GhciTerminalClose"           c_GhciTerminalClose             :: HWND -> IO ()
foreign import ccall safe "GhciTerminalPaste"           c_GhciTerminalPaste             :: HWND -> IO ()
foreign import ccall safe "GhciTerminalCut"             c_GhciTerminalCut               :: HWND -> IO () 
foreign import ccall safe "GhciTerminalCopy"            c_GhciTerminalCopy              :: HWND -> IO ()
foreign import ccall safe "GhciTerminalSelectAll"       c_GhciTerminalSelectAll         :: HWND -> IO () 
foreign import ccall safe "GhciTerminalHasFocus"        c_GhciTerminalHasFocus          :: HWND -> IO Int32 
foreign import ccall safe "GhciTerminalSetFocus"        c_GhciTerminalSetFocus          :: HWND -> IO () 
foreign import ccall safe "GhciTerminalSendCommand"     c_GhciTerminalSendCommand       :: HWND -> CString -> IO HWND 
foreign import ccall safe "GhciTerminalIsTextSelected"  c_GhciTerminalIsTextSelected    :: HWND -> IO Int32
foreign import ccall safe "GhciTerminalGetTextLength"   c_GhciTerminalGetTextLength     :: HWND -> IO Int32 
foreign import ccall safe "GhciTerminalGetText"         c_GhciTerminalGetText           :: HWND -> CString -> Int32 -> IO Int32 
foreign import ccall safe "GhciTerminalClear"           c_GhciTerminalClear             :: HWND -> IO ()

-- callback wrapper
foreign import ccall safe "wrapper" c_GhciTerminalCreateCallback ::
    (HWND -> Int -> CString -> IO ()) -> IO (FunPtr (HWND -> Int -> CString -> IO ()))
    
foreign import ccall safe "GhciNew"                 c_GhciNew               :: CString -> CString -> IO Int32
foreign import ccall safe "GhciClose"               c_GhciClose             :: Int32 -> IO ()
foreign import ccall safe "GhciSetEventHandler"     c_GhciSetEventHandler   :: Int32 -> FunPtr (Int32 -> CString -> Word64 -> IO ()) -> Word64 -> IO ()
foreign import ccall safe "GhciSendCommand"         c_GhciSendCommand       :: Int32 -> CString -> IO () 
foreign import ccall safe "GhciSendCommandAsynch"   c_GhciSendCommandAsynch :: Int32 -> CString -> CString -> IO ()
foreign import ccall safe "GhciSendCommandSynch"    c_GhciSendCommandSynch  :: Int32 -> CString -> CString -> Word64 -> Ptr CString -> IO Int32
foreign import ccall safe "GhciWaitForResponse"     c_GhciWaitForResponse   :: Int32 -> CString -> Word64 -> Ptr CString -> IO Int32
  
-- callback wrapper
foreign import ccall safe "wrapper" c_GhciCreateCallback ::
    (Int32 -> CString -> Word64 -> IO ()) -> IO (FunPtr (Int32 -> CString -> Word64 -> IO ()))

-----------------------------------------------

initialise :: IO Bool
initialise = do
    b <- c_ScintillaProxyInitialise
    return b -- $ b /= 0

uninitialise :: IO ()
uninitialise = c_ScintillaProxyUninitialise

-----------------------------------------------

sciSendEditorII :: HWND -> Int -> Int -> Int -> IO Int
sciSendEditorII h code wp lp = do
    res <- c_ScnSendEditorI h (fromIntegral code :: Word32) (fromIntegral wp :: Word64) (fromIntegral lp :: Int64)
    return (fromIntegral res :: Int)

sciSendEditorIO :: HWND -> Int -> Int -> Int -> IO ()
sciSendEditorIO h code wp lp = do
    c_ScnSendEditorI h (fromIntegral code :: Word32) (fromIntegral wp :: Word64) (fromIntegral lp :: Int64)
    return ()

sciSendEditorSI :: HWND -> Int -> Int -> CString -> IO Int
sciSendEditorSI h code wp lp = do
    res <- c_ScnSendEditorS h (fromIntegral code :: Word32) (fromIntegral wp :: Word64) lp
    return (fromIntegral res :: Int)

sciSendEditorSO :: HWND -> Int -> Int -> CString -> IO ()
sciSendEditorSO h code wp lp = do
    res <- c_ScnSendEditorS h (fromIntegral code :: Word32) (fromIntegral wp :: Word64) lp
    return ()

sciSendEditorIB :: HWND -> Int -> Int -> Int -> IO Bool
sciSendEditorIB h code wp lp = do
    res <- c_ScnSendEditorI h (fromIntegral code :: Word32) (fromIntegral wp :: Word64) (fromIntegral lp :: Int64)
    return (res /= 0)

-----------------------------------------------

ghciNew :: String -> String -> IO Int
ghciNew options file = do
    id <- withCString options (\o ->
            withCString file (\f -> do
                c_GhciNew o f))
    return (fromIntegral id :: Int)

ghciClose :: Int -> IO ()
ghciClose id = c_GhciClose (fromIntegral id :: Int32)

ghciSetEventHandler :: Int -> (Int -> String -> IO()) -> IO ()
ghciSetEventHandler id fn = do
    cb <- c_GhciCreateCallback $ ghciEventHandler fn
    c_GhciSetEventHandler (fromIntegral id :: Int32) cb 0

ghciEventHandler :: (Int -> String -> IO()) -> Int32 -> CString -> Word64 -> IO ()
ghciEventHandler handler id cstr _ = 
    peekCString cstr >>= handler (fromIntegral id :: Int)

ghciSendCommand :: Int -> String -> IO ()
ghciSendCommand id cmd = 
    withCString cmd (\cs -> c_GhciSendCommand (fromIntegral id :: Int32) cs)

ghciSendCommandAsynch :: Int -> String -> String -> IO ()
ghciSendCommandAsynch id cmd eod = 
    withCString cmd (\ccmd -> 
        withCString eod (\ceod -> do
            c_GhciSendCommandAsynch (fromIntegral id :: Int32) ccmd ceod))
               
ghciSendCommandSynch :: Int -> String -> String -> Int -> IO (Maybe String)
ghciSendCommandSynch id cmd eod timeout = 
    alloca (\pr ->
        withCString cmd (\ccmd -> 
            withCString eod (\ceod -> do
                res <- c_GhciSendCommandSynch 
                        (fromIntegral id :: Int32) 
                        ccmd 
                        ceod
                        (fromIntegral timeout :: Word64)
                        pr            
                if (res /= 0) then 
                    liftM Just $ peekCString =<< peek pr
                else 
                    return Nothing)))
               
ghciWaitForResponse :: Int -> String -> Int -> IO (Maybe String)
ghciWaitForResponse id eod timeout = 
    alloca (\pr ->
        withCString eod (\ceod -> do
            res <- c_GhciWaitForResponse 
                    (fromIntegral id :: Int32) 
                    ceod
                    (fromIntegral timeout :: Word64)
                    pr            
            if (res /= 0) then 
                liftM Just $ peekCString =<< peek pr
            else 
                return Nothing))

    -- Structure for Scintilla Notification (64 bit version)
-- See Scintilla.h SCNotification for original       
data  SCNotification = SCNotification {
                snPtrHwndFrom     :: Word64,
                scIdFrom          :: Word64,
                snCode            :: Word32,
                
                snPosition        :: Int64,
                -- SCN_STYLENEEDED, SCN_DOUBLECLICK, SCN_MODIFIED, SCN_MARGINCLICK, 
                -- SCN_NEEDSHOWN, SCN_DWELLSTART, SCN_DWELLEND, SCN_CALLTIPCLICK, 
                -- SCN_HOTSPOTCLICK, SCN_HOTSPOTDOUBLECLICK, SCN_HOTSPOTRELEASECLICK, 
                -- SCN_INDICATORCLICK, SCN_INDICATORRELEASE, 
                -- SCN_USERLISTSELECTION, SCN_AUTOCSELECTION            
                snCh              :: Int32,
                -- SCN_CHARADDED, SCN_KEY, SCN_AUTOCCOMPLETED, SCN_AUTOCSELECTION, 
                -- SCN_USERLISTSELECTION 
                snModifiers       :: Int32,
                -- SCN_KEY, SCN_DOUBLECLICK, SCN_HOTSPOTCLICK, SCN_HOTSPOTDOUBLECLICK, 
                -- SCN_HOTSPOTRELEASECLICK, SCN_INDICATORCLICK, SCN_INDICATORRELEASE, 

                snModificationType :: Int32, -- SCN_MODIFIED 
                snPtrText          :: Word64,
                -- SCN_MODIFIED, SCN_USERLISTSELECTION, SCN_AUTOCSELECTION, SCN_URIDROPPED 

                snLength          :: Int64, 
                snLinesAdded      :: Int64,  -- SCN_MODIFIED 
                snMessage         :: Int32,  -- SCN_MACRORECORD 
                snWParam          :: Word64, -- SCN_MACRORECORD
                snLParam          :: Int64,  -- SCN_MACRORECORD
                snLine            :: Int64,  -- SCN_MODIFIED
                snFoldLevelNow    :: Int32,  -- SCN_MODIFIED
                snFoldLevelPrev   :: Int32,  -- SCN_MODIFIED 
                snMargin          :: Int32,  -- SCN_MARGINCLICK 
                snListType        :: Int32,  -- SCN_USERLISTSELECTION 
                snX               :: Int32,  -- SCN_DWELLSTART, SCN_DWELLEND 
                snY               :: Int32,  -- SCN_DWELLSTART, SCN_DWELLEND 
                snToken           :: Int32,  -- SCN_MODIFIED with SC_MOD_CONTAINER 
                snAnnotationLinesAdded :: Int64, -- SCN_MODIFIED with SC_MOD_CHANGEANNOTATION 
                snUpdated         :: Int32,  -- SCN_UPDATEUI 
                snListCompletionMethod :: Int32
                -- SCN_AUTOCSELECTION, SCN_AUTOCCOMPLETED, SCN_USERLISTSELECTION,    
            }

-- 0 8 16 24 32 36 40 48 56 64 72 80 88 96 104 108 112 116 120 124 128 136 144 148             
instance Storable SCNotification where
        alignment _ = 8
        sizeOf _    = 152
        peek ptr    = SCNotification
            <$> peekByteOff ptr 0
            <*> peekByteOff ptr 8
            <*> peekByteOff ptr 16 
            <*> peekByteOff ptr 24
            <*> peekByteOff ptr 32
            <*> peekByteOff ptr 36
            <*> peekByteOff ptr 40  
            <*> peekByteOff ptr 48
            <*> peekByteOff ptr 56
            <*> peekByteOff ptr 64
            <*> peekByteOff ptr 72 
            <*> peekByteOff ptr 80
            <*> peekByteOff ptr 88
            <*> peekByteOff ptr 96
            <*> peekByteOff ptr 104 
            <*> peekByteOff ptr 108
            <*> peekByteOff ptr 112 
            <*> peekByteOff ptr 116
            <*> peekByteOff ptr 120
            <*> peekByteOff ptr 124
            <*> peekByteOff ptr 128
            <*> peekByteOff ptr 136
            <*> peekByteOff ptr 144
            <*> peekByteOff ptr 148
        poke ptr (SCNotification 
                snPtrHwndFrom
                scIdFrom
                snCode                
                snPosition           
                snCh
                snModifiers
                snModificationType
                snPtrText
                snLength
                snLinesAdded
                snMessage
                snWParam
                snLParam
                snLine
                snFoldLevelNow
                snFoldLevelPrev
                snMargin
                snListType
                snX
                snY
                snToken
                snAnnotationLinesAdded 
                snUpdated
                snListCompletionMethod) = do             
-- 0 8 16 24 32 36 40 48 56 64 72 80 88 96 104 108 112 116 120 124 128 136 144 148            
            pokeByteOff ptr 0     snPtrHwndFrom
            pokeByteOff ptr 8     scIdFrom
            pokeByteOff ptr 16    snCode                
            pokeByteOff ptr 24    snPosition           
            pokeByteOff ptr 32    snCh
            pokeByteOff ptr 36    snModifiers
            pokeByteOff ptr 40    snModificationType
            pokeByteOff ptr 48    snPtrText
            pokeByteOff ptr 56    snLength
            pokeByteOff ptr 64    snLinesAdded
            pokeByteOff ptr 72    snMessage
            pokeByteOff ptr 80    snWParam
            pokeByteOff ptr 88    snLParam
            pokeByteOff ptr 96    snLine
            pokeByteOff ptr 104   snFoldLevelNow
            pokeByteOff ptr 108   snFoldLevelPrev
            pokeByteOff ptr 112   snMargin
            pokeByteOff ptr 116   snListType
            pokeByteOff ptr 120   snX
            pokeByteOff ptr 124   snY
            pokeByteOff ptr 128   snToken
            pokeByteOff ptr 136   snAnnotationLinesAdded 
            pokeByteOff ptr 144   snUpdated
            pokeByteOff ptr 148   snListCompletionMethod                 
           
notifyGetHwnd :: SCNotification -> Word64
notifyGetHwnd (SCNotification x _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = x           

notifyGetCode :: SCNotification -> Word32
notifyGetCode (SCNotification _ _ x _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = x           

notifyGetPosition :: SCNotification -> Int64
notifyGetPosition (SCNotification _ _ _ x _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = x           

notifyGetWParam :: SCNotification -> Word64
notifyGetWParam (SCNotification _ _ _ _ _ _ _ _ _ _ _ x _ _ _ _ _ _ _ _ _ _ _ _) = x           

notifyGetListCompletionMethod :: SCNotification -> Int32
notifyGetListCompletionMethod (SCNotification _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ x) = x 
         


