
{-# LANGUAGE ScopedTypeVariables #-}

module Scintilla
(   SCNotification,
    ScnEditor,
    scnCreateEditor,
    scnNotifyGetHwnd,
    scnNotifyGetCode,
    scnNotifyGetPosition,
    scnNotifyGetWParam,
    scnNotifyGetListCompletionMethod,
    scnGetHwnd,
    scnSetEventHandler,
    scnEnableEvents,
    scnDisableEvents,
    scnSetModEventMask,
    scnClearAll,
    scnSetText,
    scnGetAllText,
    scnGetTextLen,
    scnGetTextRange,
    scnConfigureHaskell,
    scnSetSavePoint,
    scnCompareHwnd,
    scnSetReadOnly,
    scnAppendText,
    scnAppendLine,
    scnAppendTextS,
    scnAppendLineS,
    scnIsClean,
    scnClose,
    scnUndo, 
    scnRedo,
    scnCanUndo,
    scnCanRedo,
    scnBeginUndoAction, 
    scnEndUndoAction,
    scnSetUndoCollection,
    scnGetUndoCollection,
    scnCut,
    scnCopy,
    scnPaste,
    scnClear,
    scnCanPaste,
    scnSelectionIsEmpty,
    scnSelectAll,
    scnBraceHighlight,
    scnBraceBadLight,
    scnBraceMatch,
    scnGetCharAt,
    scnGetLineCount,
    scnGetLinesOnScreen,
    scnGetCurrentPos,
    scnGetPositionFromLine,
    scnGetLineFromPosition,
    scnGetPositionInfo,
    scnSetLexer,
    scnSetAStyle,
    scnStyleClearAll,
    scnBlack,
    scnWhite,
    scnSetTabWidth,
    scnSetUseTabs,
    scnSetIndentationGuides,
    scnGotoLine,
    scnShowLastLine,
    scnSetFocus,
    scnGrabFocus,
-- notification gets
    snPosition,
    snLine,
    snLinesAdded,
    snModificationType,
--
    scnGotoLineCol,
    scnGotoPos,
    scnSetFirstVisibleLine,
    scnGetFirstVisibleLine,
    scnSetSelectionMode,
    scnLinesOnScreen,
    scnFindText,
    scnSearchNext,
    scnSearchPrev,
    scnSetTargetStart,
    scnSetTargetEnd,
    scnSetTargetWholeDocument,
    scnSearchInTarget,
    scnSetTargetRange,
    scnSetSearchFlags,
    scnGotoPosWithScroll,
    scnGetSelText
) where 
    
import Control.Applicative ((<$>), (<*>))

import Data.Word (Word32, Word64)
import Data.Int (Int32, Int64)
import qualified Data.ByteString as BS (append, init, replicate)
import qualified Data.ByteString.Char8 as BS (pack, unpack)
import Data.ByteString.Internal (ByteString)
import Data.ByteString.Unsafe (unsafeUseAsCString, unsafeUseAsCStringLen)
import Data.String.Combinators (punctuate)
import Data.Strings (strNull)

import Graphics.Win32.GDI.Types (COLORREF, HWND, rgb)
import Graphics.Win32.Message (wM_SETFOCUS)
import Graphics.Win32.Window.PostMessage (postMessage)
import Graphics.UI.WXCore (varCreate, varGet)

import Foreign.C.String (CString, peekCString, withCString, withCStringLen)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (fromBool)
import Foreign.Ptr (FunPtr, Ptr, minusPtr, nullPtr)
import Foreign.Storable (Storable, alignment, sizeOf, peek, poke, pokeByteOff, peekByteOff)

import Numeric (showHex)

-- project imports
import ScintillaConstants
import Misc

-----------------------
-- Windows API calls --
-----------------------

type HHOOK = Word64

-- imports from ScintillaProxy.dll
foreign import ccall safe "ScnNewEditor"        c_ScnNewEditor       :: HWND -> IO (HWND)      
foreign import ccall safe "ScnDestroyEditor"    c_ScnDestroyEditor   :: HWND -> IO ()      
foreign import ccall safe "ScnSetEventHandler"  c_ScnSetEventHandler :: HWND -> FunPtr (Ptr (SCNotification) -> IO ()) -> IO ()
foreign import ccall safe "ScnEnableEvents"     c_ScnEnableEvents    :: HWND -> IO (Int32)
foreign import ccall safe "ScnDisableEvents"    c_ScnDisableEvents   :: HWND -> IO ()      

-- direct call to Scintilla, different aliases simplify conversion to WPARAM and LPARAM types 
foreign import ccall safe "ScnSendEditor"    c_ScnSendEditorII :: HWND -> Word32 -> Word64 -> Int64 -> IO (Int64)
foreign import ccall safe "ScnSendEditor"    c_ScnSendEditorIS :: HWND -> Word32 -> Word64 -> CString -> IO (Int64)

-- callback wrapper
foreign import ccall safe "wrapper" createCallback ::
    (Ptr (SCNotification) -> IO ()) -> IO (FunPtr (Ptr (SCNotification) -> IO ()))

--------------------------------------------------------------
-- data types
--------------------------------------------------------------
 
data SciTextToFind = SciTextToFind {
                scnTtFChrgCpMin        :: Int32,
                scnTtFChrgCpMax        :: Int32,
                scnTtFLpstrText        :: Word64,
                scnTtFChrgTextCpMin    :: Int32,
                scnTtFChrgTextCpMax    :: Int32}

instance Storable SciTextToFind where
        alignment _ = 8
        sizeOf _    = 24
        peek ptr    = SciTextToFind
            <$> peekByteOff ptr 0
            <*> peekByteOff ptr 4
            <*> peekByteOff ptr 8 
            <*> peekByteOff ptr 16
            <*> peekByteOff ptr 20
        poke ptr (SciTextToFind
                scnTtFChrgCpMin
                scnTtFChrgCpMax
                scnTtFLpstrText
                scnTtFChrgTextCpMin
                scnTtFChrgTextCpMax) = do                        
            pokeByteOff ptr 0     scnTtFChrgCpMin
            pokeByteOff ptr 4     scnTtFChrgCpMax
            pokeByteOff ptr 8     scnTtFLpstrText                
            pokeByteOff ptr 16    scnTtFChrgTextCpMin           
            pokeByteOff ptr 20    scnTtFChrgTextCpMax
          
data SciTextRange = SciTextRange {
                scnTRChrgCpMin    :: Int32,
                scnTRChrgCpMax    :: Int32,
                scnTRLpstrText    :: CString}

instance Storable SciTextRange where
        alignment _ = 8
        sizeOf _    = 16
        peek ptr    = SciTextRange
            <$> peekByteOff ptr 0
            <*> peekByteOff ptr 4
            <*> peekByteOff ptr 8 
        poke ptr (SciTextRange
                scnTRChrgCpMin
                scnTRChrgCpMax
                scnTRLpstrText) = do                        
            pokeByteOff ptr 0     scnTRChrgCpMin
            pokeByteOff ptr 4     scnTRChrgCpMax
            pokeByteOff ptr 8     scnTRLpstrText                

-- Structure for Scintilla Notification (64 bit version)
-- See Scintilla.h SCNotification for original       
data  SCNotification = SCNotification {
                ptrHwndFrom     :: Word64,
                idFrom          :: Word64,
                code            :: Word32,
                
                snPosition      :: Int64,
                -- SCN_STYLENEEDED, SCN_DOUBLECLICK, SCN_MODIFIED, SCN_MARGINCLICK, 
                -- SCN_NEEDSHOWN, SCN_DWELLSTART, SCN_DWELLEND, SCN_CALLTIPCLICK, 
                -- SCN_HOTSPOTCLICK, SCN_HOTSPOTDOUBLECLICK, SCN_HOTSPOTRELEASECLICK, 
                -- SCN_INDICATORCLICK, SCN_INDICATORRELEASE, 
                -- SCN_USERLISTSELECTION, SCN_AUTOCSELECTION            
                ch              :: Int32,
                -- SCN_CHARADDED, SCN_KEY, SCN_AUTOCCOMPLETED, SCN_AUTOCSELECTION, 
                -- SCN_USERLISTSELECTION 
                modifiers       :: Int32,
                -- SCN_KEY, SCN_DOUBLECLICK, SCN_HOTSPOTCLICK, SCN_HOTSPOTDOUBLECLICK, 
                -- SCN_HOTSPOTRELEASECLICK, SCN_INDICATORCLICK, SCN_INDICATORRELEASE, 

                snModificationType :: Int32, -- SCN_MODIFIED 
                ptrText         :: Word64,
                -- SCN_MODIFIED, SCN_USERLISTSELECTION, SCN_AUTOCSELECTION, SCN_URIDROPPED 

                length          :: Int64, 
                snLinesAdded    :: Int64,  -- SCN_MODIFIED 
                message         :: Int32,  -- SCN_MACRORECORD 
                wParam          :: Word64, -- SCN_MACRORECORD
                lParam          :: Int64,  -- SCN_MACRORECORD
                snLine          :: Int64,  -- SCN_MODIFIED
                foldLevelNow    :: Int32,  -- SCN_MODIFIED
                foldLevelPrev   :: Int32,  -- SCN_MODIFIED 
                margin          :: Int32,  -- SCN_MARGINCLICK 
                listType        :: Int32,  -- SCN_USERLISTSELECTION 
                x               :: Int32,  -- SCN_DWELLSTART, SCN_DWELLEND 
                y               :: Int32,  -- SCN_DWELLSTART, SCN_DWELLEND 
                token           :: Int32,  -- SCN_MODIFIED with SC_MOD_CONTAINER 
                annotationLinesAdded :: Int64, -- SCN_MODIFIED with SC_MOD_CHANGEANNOTATION 
                updated         :: Int32,  -- SCN_UPDATEUI 
                listCompletionMethod :: Int32
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
                ptrHwndFrom
                idFrom
                code                
                snPosition           
                ch
                modifiers
                snModificationType
                ptrText
                length
                snLinesAdded
                message
                wParam
                lParam
                snLine
                foldLevelNow
                foldLevelPrev
                margin
                listType
                x
                y
                token
                annotationLinesAdded 
                updated
                listCompletionMethod) = do             
-- 0 8 16 24 32 36 40 48 56 64 72 80 88 96 104 108 112 116 120 124 128 136 144 148            
            pokeByteOff ptr 0     ptrHwndFrom
            pokeByteOff ptr 8     idFrom
            pokeByteOff ptr 16    code                
            pokeByteOff ptr 24    snPosition           
            pokeByteOff ptr 32    ch
            pokeByteOff ptr 36    modifiers
            pokeByteOff ptr 40    snModificationType
            pokeByteOff ptr 48    ptrText
            pokeByteOff ptr 56    length
            pokeByteOff ptr 64    snLinesAdded
            pokeByteOff ptr 72    message
            pokeByteOff ptr 80    wParam
            pokeByteOff ptr 88    lParam
            pokeByteOff ptr 96    snLine
            pokeByteOff ptr 104   foldLevelNow
            pokeByteOff ptr 108   foldLevelPrev
            pokeByteOff ptr 112   margin
            pokeByteOff ptr 116   listType
            pokeByteOff ptr 120   x
            pokeByteOff ptr 124   y
            pokeByteOff ptr 128   token
            pokeByteOff ptr 136   annotationLinesAdded 
            pokeByteOff ptr 144   updated
            pokeByteOff ptr 148   listCompletionMethod                 
           
data ScnEditor = ScnEditor 
    { 
        hParent     :: HWND,
        hScnWnd     :: HWND, 
        events      :: Maybe (SCNotification -> IO ())
    }  
 
-----------------------------------------------------------
-- helpers

instance Show ScnEditor where
    show (ScnEditor p e me) = 
        "{ScnEditor} Parent HWND: " ++ (ptrToString p) ++ 
        ", Editor HWND: " ++ (ptrToString e) ++ 
        ", Event Handler: " ++ (case me of 
                                    Nothing -> "Not set" 
                                    (Just _) -> "Set" )

ioNull :: IO ()
ioNull = return ()

ioBool :: Int64 -> IO Bool
ioBool i = return (i /= 0)

ioInt :: Int64 -> IO Int
ioInt i = return (fromIntegral i :: Int)

-----------------------------------------------------------

-- Create the Scintilla editor window
-- parent = HWND of parent window
scnCreateEditor :: HWND -> IO (ScnEditor)
scnCreateEditor parent = do
    hwnd <- c_ScnNewEditor parent
    return (ScnEditor parent hwnd Nothing)
    
---------------------------------------------    
-- Callback from ScintillaProxy dll    
---------------------------------------------    

scnSetEventHandler :: ScnEditor -> (SCNotification -> IO ()) -> IO (ScnEditor)
scnSetEventHandler (ScnEditor p c _) eh = do
    let s = (ScnEditor p c (Just eh))
    cb <- createCallback $ scnCallback s
    c_ScnSetEventHandler c cb    
    return (s)

scnEnableEvents :: ScnEditor -> IO ()
scnEnableEvents (ScnEditor _ c _) = c_ScnEnableEvents c >> ioNull 

scnDisableEvents :: ScnEditor -> IO ()
scnDisableEvents (ScnEditor _ c _) = c_ScnDisableEvents c >> ioNull 
   
{-
Available event masks
sC_MOD_INSERTTEXT
sC_MOD_DELETETEXT
sC_MOD_CHANGESTYLE
sC_MOD_CHANGEFOLD
sC_PERFORMED_USER
sC_PERFORMED_UNDO
sC_PERFORMED_REDO
sC_MULTISTEPUNDOREDO
sC_LASTSTEPINUNDOREDO
sC_MOD_CHANGEMARKER
sC_MOD_BEFOREINSERT
sC_MOD_BEFOREDELETE
sC_MULTILINEUNDOREDO
sC_MODEVENTMASKALL.
-}
scnSetModEventMask :: ScnEditor -> Word32 -> IO ()
scnSetModEventMask e m = c_ScnSendEditorII (scnGetHwnd e) sCI_SETMODEVENTMASK (fromIntegral m :: Word64) 0 >> ioNull 


-- callback from scintilla
scnCallback :: ScnEditor -> Ptr (SCNotification) -> IO ()
scnCallback (ScnEditor _ _ Nothing) _ = return ()
scnCallback e@(ScnEditor _ _ (Just f)) p = do

    sn <- peek p
    
    case (scnNotifyGetCode sn) of
                    
        2007 -> do -- sCN_UPDATEUI
            f sn            
            scnUpdateBraces e
            return ()
        
        otherwise -> do
--            debugOut ss $ "Event: " ++ (show $ scnNotifyGetCode sn)
            f sn            
            return ()    

------------------------------------------------------------    
-- Accessors
------------------------------------------------------------    
    
scnGetHwnd :: ScnEditor -> HWND
scnGetHwnd (ScnEditor _ h _) = h

scnNotifyGetHwnd :: SCNotification -> Word64
scnNotifyGetHwnd (SCNotification x _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = x           

scnNotifyGetCode :: SCNotification -> Word32
scnNotifyGetCode (SCNotification _ _ x _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = x           

scnNotifyGetPosition :: SCNotification -> Int64
scnNotifyGetPosition (SCNotification _ _ _ x _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = x           

scnNotifyGetWParam :: SCNotification -> Word64
scnNotifyGetWParam (SCNotification _ _ _ _ _ _ _ _ _ _ _ x _ _ _ _ _ _ _ _ _ _ _ _) = x           

scnNotifyGetListCompletionMethod :: SCNotification -> Int32
scnNotifyGetListCompletionMethod (SCNotification _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ x) = x           

scnCompareHwnd :: ScnEditor -> SCNotification -> Bool
scnCompareHwnd scn sn = ptrToWord64 (scnGetHwnd scn) == (scnNotifyGetHwnd sn)

----------------------------------------------
-- Text Get and Set 
----------------------------------------------

scnClearAll :: ScnEditor -> IO ()
scnClearAll e = c_ScnSendEditorII (scnGetHwnd e) sCI_CLEARALL 0 0 >> ioNull

-- set the entire content of the editor    
scnSetText :: ScnEditor -> ByteString -> IO ()
scnSetText e bs = do
    let bs0 = BS.append bs (BS.replicate 1 0) -- add terminating null 
    unsafeUseAsCString bs0 (\cs -> do c_ScnSendEditorIS (scnGetHwnd e) sCI_SETTEXT 0 cs)
    return ()

-- get all text from editor    
scnGetAllText :: ScnEditor -> IO ByteString
scnGetAllText e = do            
    len <- scnGetTextLen e
    let bs = (BS.replicate (len+1) 0)   -- allocate buffer
    unsafeUseAsCString bs (\cs -> do c_ScnSendEditorIS (scnGetHwnd e) sCI_GETTEXT (fromIntegral (len+1) :: Word64) cs)   
    return (BS.init bs) -- drop the zero byte at the end
    
scnGetTextLen :: ScnEditor -> IO Int
scnGetTextLen e = c_ScnSendEditorII (scnGetHwnd e) sCI_GETLENGTH 0 0 >>= ioInt
  
scnAppendText :: ScnEditor -> ByteString -> IO ()
scnAppendText e bs = do
    let bs0 = BS.append bs (BS.replicate 1 0) -- add terminating null 
    unsafeUseAsCStringLen bs0 (\(cs, l) -> do c_ScnSendEditorIS (scnGetHwnd e) sCI_APPENDTEXT (fromIntegral (l-1) :: Word64) cs)
    return ()
    
scnAppendLine :: ScnEditor -> ByteString -> IO ()
scnAppendLine scn bs = scnAppendText scn $ BS.append bs $ BS.pack "\n"
    
scnGetCharAt :: ScnEditor -> Int -> IO Char
scnGetCharAt e p = do
    c <- c_ScnSendEditorII (scnGetHwnd e) sCI_GETCHARAT (fromIntegral p :: Word64) 0
    return (toEnum (fromIntegral c :: Int) :: Char)
   
scnAppendTextS :: ScnEditor -> String -> IO ()
scnAppendTextS e s = withCStringLen s (\(cs, l) -> c_ScnSendEditorIS (scnGetHwnd e) sCI_APPENDTEXT (fromIntegral (l) :: Word64) cs) >> ioNull
    
scnAppendLineS :: ScnEditor -> String -> IO ()
scnAppendLineS scn s = scnAppendTextS scn s >>  scnAppendTextS scn "\n"

scnGetTextRange :: ScnEditor -> Int -> Int -> IO String
scnGetTextRange e start end = do
    p <- alloca (\(ptr :: Ptr SciTextRange) -> do
        let range = (SciTextRange 
                (fromIntegral start :: Int32)
                (fromIntegral end   :: Int32)
                nullPtr)

        poke ptr range
        c_ScnSendEditorII (scnGetHwnd e) sCI_GETTEXTRANGE 0 (ptrToInt64 ptr)
        (SciTextRange _ _ ps) <- peek ptr
        peekCString ps)
    return ""

------------------------------------------------------------    
-- Scintilla commands
------------------------------------------------------------    

scnBlack :: COLORREF
scnBlack = (rgb 0 0 0)

scnWhite :: COLORREF
scnWhite = (rgb 0xff 0xff 0xff)

scnDarkGreen :: COLORREF
scnDarkGreen = (rgb 0 0x80 0)

scnKeyBlue :: COLORREF
scnKeyBlue = (rgb 0 0 230)

scnBraceGood :: COLORREF
scnBraceGood = (rgb 255 0 0)

scnBraceBad :: COLORREF
scnBraceBad = (rgb 150 0 150)

scnStringBrown :: COLORREF
scnStringBrown = (rgb 0xA0 0x10 0x20)

scnIndents :: COLORREF
scnIndents = (rgb 200 200 200)

-- configure lexer for haskell
scnConfigureHaskell :: ScnEditor -> IO ()
scnConfigureHaskell e = do
    scnSetLexer e (fromIntegral sCLEX_HASKELL :: Int)
    scnSetKeywords e 0 ["do", "if", "then", "else", "case", "qualified", "case", "module", "of", "instance", 
                        "ccall", "safe", "unsafe", "import", "data", "deriving", "where", "as", "let",
                        "newtype", "type"]
    scnSetAStyle e (fromIntegral sTYLE_DEFAULT :: Word64) scnBlack scnWhite 9 "Courier New"
    scnStyleClearAll e
    scnSetAStyle e (fromIntegral sCE_H_DEFAULT :: Word64) scnBlack scnWhite 9 "Courier New"
    scnSetStyleColour e sCE_HA_KEYWORD       scnKeyBlue     scnWhite
    scnSetStyleColour e sCE_HA_STRING        scnStringBrown scnWhite
    scnSetStyleColour e sCE_HA_IMPORT        scnKeyBlue     scnWhite
    scnSetStyleColour e sCE_HA_COMMENTLINE   scnDarkGreen   scnWhite
    scnSetStyleColour e sCE_HA_COMMENTBLOCK  scnDarkGreen   scnWhite    
    scnSetStyleColour e sCE_HA_COMMENTBLOCK2 scnDarkGreen   scnWhite
    scnSetStyleColour e sCE_HA_COMMENTBLOCK3 scnDarkGreen   scnWhite
    scnSetStyleColour e (fromIntegral sTYLE_BRACELIGHT :: Word64) scnBraceGood scnWhite
    scnSetStyleColour e (fromIntegral sTYLE_BRACEBAD :: Word64)   scnBraceBad  scnWhite

    -- tabs and indents
    scnSetTabWidth e 4
    scnSetUseTabs e False
    scnSetIndentationGuides e sC_IV_LOOKBOTH    
    scnSetStyleColour e (fromIntegral sTYLE_INDENTGUIDE :: Word64) scnIndents  scnWhite
    
    return ()
    
scnSetLexer :: ScnEditor -> Int -> IO ()
scnSetLexer e s = do               
    c_ScnSendEditorII (scnGetHwnd e) sCI_SETLEXER (fromIntegral s :: Word64) 0
    return ()

scnSetKeywords :: ScnEditor -> Int -> [String] -> IO ()
scnSetKeywords e set ks = do
    withCString 
        (concat $ punctuate " " ks) 
        (\cs -> c_ScnSendEditorIS (scnGetHwnd e) sCI_SETKEYWORDS 0 cs)
    return ()

scnSetAStyle :: ScnEditor -> Word64 -> COLORREF -> COLORREF -> Int -> String -> IO ()
scnSetAStyle e st fc bc sz fnt = do
    let h = scnGetHwnd e
    c_ScnSendEditorII h sCI_STYLESETFORE st (fromIntegral fc :: Int64)
    c_ScnSendEditorII h sCI_STYLESETBACK st (fromIntegral bc :: Int64)
    c_ScnSendEditorII h sCI_STYLESETSIZE st (fromIntegral sz :: Int64)
    withCString fnt (\cs -> c_ScnSendEditorIS h sCI_STYLESETFONT st cs)
    return ()
    
scnSetStyleColour :: ScnEditor -> Word64 -> COLORREF -> COLORREF -> IO ()
scnSetStyleColour e st fc bc = do
    let h = scnGetHwnd e
    c_ScnSendEditorII h sCI_STYLESETFORE st (fromIntegral fc :: Int64)
    c_ScnSendEditorII h sCI_STYLESETBACK st (fromIntegral bc :: Int64)
    return ()

scnStyleClearAll :: ScnEditor -> IO ()
scnStyleClearAll e = do
    c_ScnSendEditorII (scnGetHwnd e) sCI_STYLECLEARALL 0 0
    return ()
    
scnSetSavePoint :: ScnEditor -> IO ()
scnSetSavePoint e = do
    c_ScnSendEditorII (scnGetHwnd e) sCI_SETSAVEPOINT 0 0
    return ()

scnSetReadOnly :: ScnEditor -> Bool -> IO ()
scnSetReadOnly e b = do
    c_ScnSendEditorII (scnGetHwnd e) sCI_SETREADONLY (fromBool b :: Word64) 0
    return ()
      
scnIsClean :: ScnEditor -> IO Bool
scnIsClean e = do
    x <- c_ScnSendEditorII (scnGetHwnd e) sCI_GETMODIFY  0 0
    return (x == 0)
  
scnClose :: ScnEditor -> IO ()
scnClose e = c_ScnDestroyEditor (scnGetHwnd e)
 
 
----------------------------------------------
-- Undo and Redo 
----------------------------------------------

scnUndo :: ScnEditor -> IO ()
scnUndo e = c_ScnSendEditorII (scnGetHwnd e) sCI_UNDO 0 0 >> ioNull 
    
scnRedo :: ScnEditor -> IO ()
scnRedo e = c_ScnSendEditorII (scnGetHwnd e) sCI_REDO 0 0 >> ioNull 

scnCanUndo :: ScnEditor -> IO Bool
scnCanUndo e = c_ScnSendEditorII (scnGetHwnd e) sCI_CANUNDO 0 0 >>= ioBool

scnCanRedo :: ScnEditor -> IO Bool
scnCanRedo e = c_ScnSendEditorII (scnGetHwnd e) sCI_CANREDO 0 0 >>= ioBool

scnBeginUndoAction :: ScnEditor -> IO ()
scnBeginUndoAction e = c_ScnSendEditorII (scnGetHwnd e) sCI_BEGINUNDOACTION 0 0 >> ioNull 

scnEndUndoAction :: ScnEditor -> IO ()
scnEndUndoAction e = c_ScnSendEditorII (scnGetHwnd e) sCI_ENDUNDOACTION 0 0 >> ioNull 

scnSetUndoCollection :: ScnEditor -> Bool -> IO ()
scnSetUndoCollection e b = c_ScnSendEditorII (scnGetHwnd e) sCI_SETUNDOCOLLECTION (fromBool b :: Word64) 0 >> ioNull 

scnGetUndoCollection :: ScnEditor -> IO Bool
scnGetUndoCollection e = c_ScnSendEditorII (scnGetHwnd e) sCI_GETUNDOCOLLECTION 0 0 >>= ioBool
    
----------------------------------------------
-- Cut and Paste 
----------------------------------------------

scnCut :: ScnEditor -> IO ()
scnCut e = c_ScnSendEditorII (scnGetHwnd e) sCI_CUT 0 0 >> ioNull 

scnCopy :: ScnEditor -> IO ()
scnCopy e = c_ScnSendEditorII (scnGetHwnd e) sCI_COPY 0 0 >> ioNull 
  
scnPaste :: ScnEditor -> IO ()
scnPaste e = c_ScnSendEditorII (scnGetHwnd e) sCI_PASTE 0 0 >> ioNull 

scnClear :: ScnEditor -> IO ()
scnClear e = c_ScnSendEditorII (scnGetHwnd e) sCI_CLEAR 0 0 >> ioNull 

scnCanPaste :: ScnEditor -> IO Bool
scnCanPaste e =  c_ScnSendEditorII (scnGetHwnd e) sCI_CANPASTE 0 0 >>= ioBool
    
----------------------------------------------
-- Selection 
----------------------------------------------
    
scnSelectionIsEmpty :: ScnEditor -> IO Bool
scnSelectionIsEmpty e = do
    b <- c_ScnSendEditorII (scnGetHwnd e) sCI_GETSELTEXT  0 0
    return (b == 1)
  
scnSelectAll :: ScnEditor -> IO ()
scnSelectAll e = c_ScnSendEditorII (scnGetHwnd e) sCI_SELECTALL 0 0 >> ioNull 

{-
    sC_SEL_STREAM
    sC_SEL_RECTANGLE
    sC_SEL_LINES,
    sC_SEL_THIN,
-}      
scnSetSelectionMode :: ScnEditor -> Word32 -> IO ()
scnSetSelectionMode e m = c_ScnSendEditorII (scnGetHwnd e) sCI_SETSELECTIONMODE (fromIntegral m :: Word64) 0 >> ioNull 

scnGetSelText :: ScnEditor -> IO String
scnGetSelText e = do
    len <- c_ScnSendEditorII (scnGetHwnd e) sCI_GETSELTEXT 0 0
    if len > 0 then do
        let bs = (BS.replicate ((fromIntegral len :: Int)+1) 0)   -- allocate buffer
        unsafeUseAsCString bs (\cs -> do c_ScnSendEditorIS (scnGetHwnd e) sCI_GETSELTEXT 0 cs)   
        return $ BS.unpack $ BS.init bs
    else return ""

----------------------------------------------
-- Brace Highlighting 
----------------------------------------------
  
scnBraceHighlight :: ScnEditor -> Int -> Int -> IO ()
scnBraceHighlight e pa pb = c_ScnSendEditorII (scnGetHwnd e) sCI_BRACEHIGHLIGHT (fromIntegral pa :: Word64) (fromIntegral pb :: Int64) >> ioNull 

scnBraceBadLight :: ScnEditor -> Int -> IO ()
scnBraceBadLight e p = c_ScnSendEditorII (scnGetHwnd e) sCI_BRACEBADLIGHT (fromIntegral p :: Word64) 0 >> ioNull 

scnBraceMatch :: ScnEditor -> Int -> IO Int
scnBraceMatch e p = c_ScnSendEditorII (scnGetHwnd e) sCI_BRACEMATCH  (fromIntegral p :: Word64) 0 >>= ioInt
    
scnUpdateBraces :: ScnEditor -> IO ()
scnUpdateBraces e = do
    
    -- look at position ahead and behind the cursor
    -- (scintilla seems to tolerate positions out of range)
    pa <- scnGetCurrentPos e
    let pss = [pa-1, pa]

    -- find the matching braces for both positions
    pes <- mapM (scnBraceMatch e) pss
    
    -- (de)highlight in preference the brackets behind the cursor
    -- otherwise the brackets ahead of the cursor
    highlight pss pes
          
    where 
        highlight _ [-1, -1] = scnBraceHighlight e (-1) (-1) -- dehighlight    
        highlight [psb, psa] [peb, pea] 
            | peb /= -1 = scnBraceHighlight e psb peb
            | otherwise = scnBraceHighlight e psa pea 
        
----------------------------------------------
-- Position and size 
----------------------------------------------

scnGetLineCount :: ScnEditor -> IO Int
scnGetLineCount e = c_ScnSendEditorII (scnGetHwnd e) sCI_GETLINECOUNT  0 0 >>= ioInt
   
scnGetLinesOnScreen :: ScnEditor -> IO Int
scnGetLinesOnScreen e = c_ScnSendEditorII (scnGetHwnd e) sCI_LINESONSCREEN  0 0 >>= ioInt
   
scnGetCurrentPos :: ScnEditor -> IO Int
scnGetCurrentPos e = c_ScnSendEditorII (scnGetHwnd e) sCI_GETCURRENTPOS  0 0 >>= ioInt

   
scnGetPositionFromLine :: ScnEditor -> Int -> IO Int
scnGetPositionFromLine e l = c_ScnSendEditorII (scnGetHwnd e) sCI_POSITIONFROMLINE  (fromIntegral l :: Word64) 0 >>= ioInt

scnGetLineFromPosition :: ScnEditor -> Int -> IO Int
scnGetLineFromPosition e p = c_ScnSendEditorII (scnGetHwnd e) sCI_LINEFROMPOSITION  (fromIntegral p :: Word64) 0 >>= ioInt

-- returns line, line position, doc position, total lines, total size
scnGetPositionInfo :: ScnEditor -> IO (Int, Int, Int, Int, Int) 
scnGetPositionInfo e = do
    p <- scnGetCurrentPos e
    l <- scnGetLineFromPosition e p
    ls <- scnGetPositionFromLine e l
    lc <- scnGetLineCount e
    cc <- scnGetTextLen e
    return (l, (p-ls), p, lc, cc)
    
scnGotoLine :: ScnEditor -> Int -> IO ()
scnGotoLine e l = do
    c_ScnSendEditorII (scnGetHwnd e) sCI_GOTOLINE  (fromIntegral l :: Word64) 0
    return ()

scnGotoPos :: ScnEditor -> Int -> IO ()
scnGotoPos e p = do
    c_ScnSendEditorII (scnGetHwnd e) sCI_GOTOPOS  (fromIntegral p :: Word64) 0
    return ()

scnShowLastLine :: ScnEditor -> IO ()
scnShowLastLine e = do
    sl <- scnGetLinesOnScreen e
    tl <- scnGetLineCount e
    if (sl < tl) then (scnGotoLine e (tl-1)) 
    else return ()

scnGotoLineCol :: ScnEditor -> Int -> Int -> IO ()
scnGotoLineCol e l c = do
    fl <- scnGetFirstVisibleLine e
    sl <- scnLinesOnScreen e
    p <- scnGetPositionFromLine e l

    if (l < fl || l >= (fl+sl)) then do
        scnSetFirstVisibleLine e (l-(sl `div` 2))
        scnGotoPos e (p+c)
        return ()
    else do 
        scnGotoPos e (p+c)
        return ()

scnGotoPosWithScroll :: ScnEditor -> Int -> IO ()
scnGotoPosWithScroll e pos = do
    l <- scnGetLineFromPosition e pos
    fl <- scnGetFirstVisibleLine e
    sl <- scnLinesOnScreen e

    if (l < fl || l >= (fl+sl)) then do
        scnSetFirstVisibleLine e (l-(sl `div` 4))
        scnGotoPos e pos
        return ()
    else do 
        scnGotoPos e pos
        return ()

scnSetFirstVisibleLine :: ScnEditor -> Int -> IO ()
scnSetFirstVisibleLine e l = c_ScnSendEditorII (scnGetHwnd e) sCI_SETFIRSTVISIBLELINE  (fromIntegral l :: Word64) 0 >> ioNull


scnGetFirstVisibleLine :: ScnEditor -> IO Int
scnGetFirstVisibleLine e = c_ScnSendEditorII (scnGetHwnd e) sCI_GETFIRSTVISIBLELINE  0 0 >>= ioInt

scnLinesOnScreen :: ScnEditor -> IO Int
scnLinesOnScreen e = c_ScnSendEditorII (scnGetHwnd e) sCI_LINESONSCREEN  0 0 >>= ioInt

----------------------------------------------
-- Tabs 
----------------------------------------------

scnSetTabWidth :: ScnEditor -> Int -> IO ()
scnSetTabWidth e w = c_ScnSendEditorII (scnGetHwnd e) sCI_SETTABWIDTH  (fromIntegral w :: Word64) 0 >> ioNull


scnSetUseTabs :: ScnEditor -> Bool -> IO ()
scnSetUseTabs e t = c_ScnSendEditorII (scnGetHwnd e) sCI_SETUSETABS  (fromBool t :: Word64) 0 >> ioNull


{-
    sC_IV_NONE
    sC_IV_REAL
    sC_IV_LOOKFORWARD,
    sC_IV_LOOKBOTH,
-}      
scnSetIndentationGuides :: ScnEditor -> Word32 -> IO ()
scnSetIndentationGuides e w = c_ScnSendEditorII (scnGetHwnd e) sCI_SETINDENTATIONGUIDES  (fromIntegral w :: Word64) 0 >> ioNull
  
----------------------------------------------
-- Focus 
----------------------------------------------

scnSetFocus :: ScnEditor -> Bool -> IO ()
scnSetFocus e b = do
    c_ScnSendEditorII (scnGetHwnd e) sCI_SETFOCUS (fromBool b :: Word64) 0
    scnGrabFocus e
    return ()

scnGrabFocus :: ScnEditor -> IO ()
scnGrabFocus e = c_ScnSendEditorII (scnGetHwnd e) sCI_GRABFOCUS 0 0 >> ioNull

----------------------------------------------
-- Search and replace
----------------------------------------------

scnSetTargetStart :: ScnEditor -> Int -> IO ()
scnSetTargetStart e t = c_ScnSendEditorII (scnGetHwnd e) sCI_SETTARGETSTART (fromIntegral t :: Word64) 0 >> ioNull

scnSetTargetEnd :: ScnEditor -> Int -> IO ()
scnSetTargetEnd e t = c_ScnSendEditorII (scnGetHwnd e) sCI_SETTARGETEND  (fromIntegral t :: Word64) 0  >> ioNull

scnSetTargetRange :: ScnEditor -> Int -> Int -> IO ()
scnSetTargetRange e s f = c_ScnSendEditorII (scnGetHwnd e) sCI_SETTARGETRANGE  (fromIntegral s :: Word64) (fromIntegral f :: Int64) >> ioNull


{-
    Second argument is the seqrch options:-

    sCFIND_WHOLEWORD,
    sCFIND_MATCHCASE,
    sCFIND_WORDSTART,
    sCFIND_REGEXP,
    sCFIND_POSIX,
    sCFIND_CXX11REGEX,
-}
scnSetSearchFlags :: ScnEditor -> Int -> IO ()
scnSetSearchFlags e f = c_ScnSendEditorII (scnGetHwnd e) sCI_SETSEARCHFLAGS  (fromIntegral f :: Word64) 0 >> ioNull

scnSetTargetWholeDocument :: ScnEditor -> IO ()
scnSetTargetWholeDocument e = c_ScnSendEditorII (scnGetHwnd e) sCI_TARGETWHOLEDOCUMENT 0 0 >> ioNull


scnSearchInTarget :: ScnEditor -> String -> IO Int
scnSearchInTarget e s = do
    p <- withCStringLen s (\(cs, l) -> c_ScnSendEditorII (scnGetHwnd e) sCI_SEARCHINTARGET (fromIntegral l :: Word64) (ptrToInt64 cs)) 
    return (fromIntegral p :: Int)

scnFindText :: ScnEditor -> String -> Word32 -> Int -> Int -> IO Int
scnFindText e text ops start end = do
    p <- alloca (\(ptr :: Ptr SciTextToFind) -> do
        pos <- withCString text 
                (\ps -> do
                    let sci = (SciTextToFind 
                            (fromIntegral start :: Int32)
                            (fromIntegral end   :: Int32)
                            (ptrToWord64 ps)
                            0 0)    
                    poke ptr sci
                    c_ScnSendEditorII (scnGetHwnd e) sCI_FINDTEXT (fromIntegral ops :: Word64) (ptrToInt64 ptr)
                )
        return (fromIntegral pos :: Int))
    return p

scnSearchNext :: ScnEditor -> String -> Int -> IO Int
scnSearchNext e text ops = 
    withCString text 
        (\ps -> c_ScnSendEditorII (scnGetHwnd e) sCI_SEARCHNEXT (fromIntegral ops :: Word64) (ptrToInt64 ps)) 
            >>= ioInt


scnSearchPrev :: ScnEditor -> String -> Int -> IO Int
scnSearchPrev e text ops = 
    withCString text 
        (\ps -> c_ScnSendEditorII (scnGetHwnd e) sCI_SEARCHPREV (fromIntegral ops :: Word64) (ptrToInt64 ps))
            >>= ioInt


