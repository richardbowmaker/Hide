
module ScintillaProxyImports
(
    SCNotification,
    c_ScintillaProxyInitialise,
    c_ScintillaProxyUninitialise,
    c_ScnNewEditor,
    c_ScnDestroyEditor,
    c_ScnSetEventHandler,
    c_ScnEnableEvents,
    c_ScnDisableEvents,
    c_ScnAddPopupMenuItem,
    c_ScnSendEditorII,
    c_ScnSendEditorIS,
    c_ScnCreateCallback,
    c_ScnCreateHandlerCallback,
    c_ScnCreateEnabledCallback,
    c_GhciNew,
    c_GhciSetEventHandler,
    c_GhciEnableEvents,
    c_GhciDisableEvents,
    c_GhciClose,
    c_GhciPaste,
    c_GhciCut,
    c_GhciCopy,
    c_GhciSelectAll,
    c_GhciHasFocus,
    c_GhciSetFocus,
    c_GhciSendCommand,
    c_GhciIsTextSelected,
    c_GhciGetTextLength,
    c_GhciGetText,
    c_GhciClear,
    c_GhciCreateCallback,
    notifyGetHwnd,
    notifyGetCode,
    notifyGetPosition,
    notifyGetWParam,
    notifyGetListCompletionMethod,
    snLine,
    snLinesAdded,
    snModificationType,
    snPosition,
    snUpdated
) where 
  
import Graphics.Win32.GDI.Types (HWND)
import Foreign.C.String (CString, peekCString, withCString, withCStringLen)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (fromBool)
import Foreign.Ptr (FunPtr, Ptr, minusPtr, nullPtr)
import Foreign.Storable (Storable, alignment, sizeOf, peek, poke, pokeByteOff, peekByteOff)

import Data.Int (Int32, Int64)
import Data.Word (Word32, Word64)


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
foreign import ccall safe "ScnSendEditor" c_ScnSendEditorII :: HWND -> Word32 -> Word64 -> Int64 -> IO (Int64)
foreign import ccall safe "ScnSendEditor" c_ScnSendEditorIS :: HWND -> Word32 -> Word64 -> CString -> IO (Int64)

-- callback wrappers
foreign import ccall safe "wrapper" c_ScnCreateCallback ::
    (Ptr (SCNotification) -> IO ()) -> IO (FunPtr (Ptr (SCNotification) -> IO ()))

foreign import ccall safe "wrapper" c_ScnCreateHandlerCallback ::
    (Int -> IO ()) -> IO (FunPtr (Int -> IO ()))

foreign import ccall safe "wrapper" c_ScnCreateEnabledCallback ::
    (Int -> IO Int) -> IO (FunPtr (Int -> IO Int))
  

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
foreign import ccall safe "GhciClear"           c_GhciClear             :: HWND -> IO ()

-- callback wrapper
foreign import ccall safe "wrapper" c_GhciCreateCallback ::
    (HWND -> Int -> IO ()) -> IO (FunPtr (HWND -> Int -> IO ()))

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
         


