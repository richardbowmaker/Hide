
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
    scnClearAll,
    scnSetText,
    scnGetAllText,
    scnGetTextLen,
    scnConfigureHaskell,
    scnSetSavePoint,
    scnCompareHwnd,
    scnSetReadOnly,
    scnAppendText,
    scnAppendLine,
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
    snLine,
    scnGotoLineCol,
    scnGotoPos,
    scnSetFirstVisibleLine,
    scnSetSelectionMode
) where 
    
import Control.Applicative ((<$>), (<*>))

import Data.Word (Word32, Word64)
import Data.Int (Int32, Int64)
import qualified Data.ByteString as BS (append, init, replicate)
import qualified Data.ByteString.Char8 as BS (pack)
import Data.ByteString.Internal (ByteString)
import Data.ByteString.Unsafe (unsafeUseAsCString, unsafeUseAsCStringLen)
import Data.String.Combinators (punctuate)
import Data.Strings (strNull)

import Graphics.Win32.GDI.Types (COLORREF, HWND, rgb)
import Graphics.Win32.Message (wM_SETFOCUS)
import Graphics.Win32.Window.PostMessage (postMessage)
import Graphics.UI.WXCore (varCreate, varGet)

import Foreign.C.String (CString, withCString, withCStringLen)
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
      
-- Structure for Scintilla Notification (64 bit version)
-- See Scintilla.h SCNotification for original       
data  SCNotification = SCNotification
            {
                ptrHwndFrom     :: Word64,
                idFrom          :: Word64,
                code            :: Word32,
                
                position        :: Int64,
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

                modificationType :: Int32, -- SCN_MODIFIED 
                ptrText         :: Word64,
                -- SCN_MODIFIED, SCN_USERLISTSELECTION, SCN_AUTOCSELECTION, SCN_URIDROPPED 

                length          :: Int64, 
                linesAdded      :: Int64,  -- SCN_MODIFIED 
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
                position           
                ch
                modifiers
                modificationType
                ptrText
                length
                linesAdded
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
            pokeByteOff ptr 24    position           
            pokeByteOff ptr 32    ch
            pokeByteOff ptr 36    modifiers
            pokeByteOff ptr 40    modificationType
            pokeByteOff ptr 48    ptrText
            pokeByteOff ptr 56    length
            pokeByteOff ptr 64    linesAdded
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
-- show

instance Show ScnEditor where
    show (ScnEditor p e me) = 
        "{ScnEditor} Parent HWND: " ++ (ptrToString p) ++ 
        ", Editor HWND: " ++ (ptrToString e) ++ 
        ", Event Handler: " ++ (case me of 
                                    Nothing -> "Not set" 
                                    (Just _) -> "Set" )

-----------------------------------------------------------

-- Create the Scintilla editor window
-- parent = HWND of parent window
scnCreateEditor :: HWND -> IO (ScnEditor)
scnCreateEditor parent = do
    hwnd <- c_ScnNewEditor parent
    return (ScnEditor parent hwnd Nothing)
    
scnSetEventHandler :: ScnEditor -> (SCNotification -> IO ()) -> IO (ScnEditor)
scnSetEventHandler (ScnEditor p c _) eh = do
    let s = (ScnEditor p c (Just eh))
    cb <- createCallback $ scnCallback s
    c_ScnSetEventHandler c cb    
    return (s)

scnEnableEvents :: ScnEditor -> IO ()
scnEnableEvents (ScnEditor _ c _) = do
    c_ScnEnableEvents c    
    return ()

scnDisableEvents :: ScnEditor -> IO ()
scnDisableEvents s@(ScnEditor _ c _) = do
    c_ScnDisableEvents c    
    return ()
   
---------------------------------------------    
-- Callback from ScintillaProxy dll    
---------------------------------------------    

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
scnUndo e = do
    c_ScnSendEditorII (scnGetHwnd e) sCI_UNDO 0 0
    return ()
    
scnRedo :: ScnEditor -> IO ()
scnRedo e = do
    c_ScnSendEditorII (scnGetHwnd e) sCI_REDO 0 0
    return ()

scnCanUndo :: ScnEditor -> IO Bool
scnCanUndo e = do
    b <- c_ScnSendEditorII (scnGetHwnd e) sCI_CANUNDO 0 0
    return (b /= 0)

scnCanRedo :: ScnEditor -> IO Bool
scnCanRedo e = do
    b <- c_ScnSendEditorII (scnGetHwnd e) sCI_CANREDO 0 0
    return (b /= 0)

scnBeginUndoAction :: ScnEditor -> IO ()
scnBeginUndoAction e = do
    c_ScnSendEditorII (scnGetHwnd e) sCI_BEGINUNDOACTION 0 0
    return ()

scnEndUndoAction :: ScnEditor -> IO ()
scnEndUndoAction e = do
    c_ScnSendEditorII (scnGetHwnd e) sCI_ENDUNDOACTION 0 0
    return ()

scnSetUndoCollection :: ScnEditor -> Bool -> IO ()
scnSetUndoCollection e b = do
    c_ScnSendEditorII (scnGetHwnd e) sCI_SETUNDOCOLLECTION (fromBool b :: Word64) 0
    return ()

scnGetUndoCollection :: ScnEditor -> IO Bool
scnGetUndoCollection e = do
    b <- c_ScnSendEditorII (scnGetHwnd e) sCI_GETUNDOCOLLECTION 0 0
    return (b /= 0)
    
----------------------------------------------
-- Cut and Paste 
----------------------------------------------

scnCut :: ScnEditor -> IO ()
scnCut e = do
    c_ScnSendEditorII (scnGetHwnd e) sCI_CUT 0 0
    return ()

scnCopy :: ScnEditor -> IO ()
scnCopy e = do
    c_ScnSendEditorII (scnGetHwnd e) sCI_COPY 0 0
    return ()
    
scnPaste :: ScnEditor -> IO ()
scnPaste e = do
    c_ScnSendEditorII (scnGetHwnd e) sCI_PASTE 0 0
    return ()

scnClear :: ScnEditor -> IO ()
scnClear e = do
    c_ScnSendEditorII (scnGetHwnd e) sCI_CLEAR 0 0
    return ()

scnCanPaste :: ScnEditor -> IO Bool
scnCanPaste e = do
    b <- c_ScnSendEditorII (scnGetHwnd e) sCI_CANPASTE 0 0
    return (b /= 0)
    
----------------------------------------------
-- Selection 
----------------------------------------------
    
scnSelectionIsEmpty :: ScnEditor -> IO Bool
scnSelectionIsEmpty e = do
    b <- c_ScnSendEditorII (scnGetHwnd e) sCI_GETSELTEXT  0 0
    return (b == 1)
  
scnSelectAll :: ScnEditor -> IO ()
scnSelectAll e = do
    c_ScnSendEditorII (scnGetHwnd e) sCI_SELECTALL 0 0
    return ()

{-
    sC_SEL_STREAM
    sC_SEL_RECTANGLE
    sC_SEL_LINES,
    sC_SEL_THIN,
-}      
scnSetSelectionMode :: ScnEditor -> Word32 -> IO ()
scnSetSelectionMode e m = do
    c_ScnSendEditorII (scnGetHwnd e) sCI_SETSELECTIONMODE  (fromIntegral m :: Word64) 0
    return ()
  
----------------------------------------------
-- Brace Highlighting 
----------------------------------------------
  
scnBraceHighlight :: ScnEditor -> Int -> Int -> IO ()
scnBraceHighlight e pa pb = do
    c_ScnSendEditorII (scnGetHwnd e) sCI_BRACEHIGHLIGHT (fromIntegral pa :: Word64) (fromIntegral pb :: Int64)
    return ()

scnBraceBadLight :: ScnEditor -> Int -> IO ()
scnBraceBadLight e p = do
    c_ScnSendEditorII (scnGetHwnd e) sCI_BRACEBADLIGHT (fromIntegral p :: Word64) 0
    return ()

scnBraceMatch :: ScnEditor -> Int -> IO Int
scnBraceMatch e p = do
    p' <- c_ScnSendEditorII (scnGetHwnd e) sCI_BRACEMATCH  (fromIntegral p :: Word64) 0
    return (fromIntegral p' :: Int)
    
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
-- Text Get and Set 
----------------------------------------------

scnClearAll :: ScnEditor -> IO ()
scnClearAll e = do
    c_ScnSendEditorII (scnGetHwnd e) sCI_CLEARALL 0 0
    return ()

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
scnGetTextLen e = do
    len <- c_ScnSendEditorII (scnGetHwnd e) sCI_GETLENGTH 0 0
    return (fromIntegral len :: Int)
   
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
   
----------------------------------------------
-- Position and size 
----------------------------------------------

scnGetLineCount :: ScnEditor -> IO Int
scnGetLineCount e = do
    c <- c_ScnSendEditorII (scnGetHwnd e) sCI_GETLINECOUNT  0 0
    return (fromIntegral c :: Int)
    
scnGetLinesOnScreen :: ScnEditor -> IO Int
scnGetLinesOnScreen e = do
    c <- c_ScnSendEditorII (scnGetHwnd e) sCI_LINESONSCREEN  0 0
    return (fromIntegral c :: Int)
    
scnGetCurrentPos :: ScnEditor -> IO Int
scnGetCurrentPos e = do
    p <- c_ScnSendEditorII (scnGetHwnd e) sCI_GETCURRENTPOS  0 0
    return (fromIntegral p :: Int)
   
scnGetPositionFromLine :: ScnEditor -> Int -> IO Int
scnGetPositionFromLine e l = do
    p <- c_ScnSendEditorII (scnGetHwnd e) sCI_POSITIONFROMLINE  (fromIntegral l :: Word64) 0
    return (fromIntegral p :: Int)

scnGetLineFromPosition :: ScnEditor -> Int -> IO Int
scnGetLineFromPosition e p = do
    l <- c_ScnSendEditorII (scnGetHwnd e) sCI_LINEFROMPOSITION  (fromIntegral p :: Word64) 0
    return (fromIntegral l :: Int)

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
    p <- scnGetPositionFromLine e l
    scnGotoPos e (p+c)
    scnSetFirstVisibleLine e (l-5)
    return ()

scnSetFirstVisibleLine :: ScnEditor -> Int -> IO ()
scnSetFirstVisibleLine e l = do
    c_ScnSendEditorII (scnGetHwnd e) sCI_SETFIRSTVISIBLELINE  (fromIntegral l :: Word64) 0
    return ()

----------------------------------------------
-- Tabs 
----------------------------------------------

scnSetTabWidth :: ScnEditor -> Int -> IO ()
scnSetTabWidth e w = do
    c_ScnSendEditorII (scnGetHwnd e) sCI_SETTABWIDTH  (fromIntegral w :: Word64) 0
    return ()

scnSetUseTabs :: ScnEditor -> Bool -> IO ()
scnSetUseTabs e t = do
    c_ScnSendEditorII (scnGetHwnd e) sCI_SETUSETABS  (fromBool t :: Word64) 0
    return ()

{-
    sC_IV_NONE
    sC_IV_REAL
    sC_IV_LOOKFORWARD,
    sC_IV_LOOKBOTH,
-}      
scnSetIndentationGuides :: ScnEditor -> Word32 -> IO ()
scnSetIndentationGuides e w = do
    c_ScnSendEditorII (scnGetHwnd e) sCI_SETINDENTATIONGUIDES  (fromIntegral w :: Word64) 0
    return ()
    
----------------------------------------------
-- Focus 
----------------------------------------------

scnSetFocus :: ScnEditor -> Bool -> IO ()
scnSetFocus e b = do
    c_ScnSendEditorII (scnGetHwnd e) sCI_SETFOCUS (fromBool b :: Word64) 0
    scnGrabFocus e
    return ()

scnGrabFocus :: ScnEditor -> IO ()
scnGrabFocus e = do
    c_ScnSendEditorII (scnGetHwnd e) sCI_GRABFOCUS 0 0
    return ()


