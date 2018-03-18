
{-# LANGUAGE ScopedTypeVariables #-}

module Scintilla
(   
    Editor,
    SCNotification,
    appendLine,
    appendLineS,
    appendText,
    appendTextS,
    beginUndoAction, 
    black,
    braceBadLight,
    braceHighlight,
    braceMatch,
    canPaste,
    canRedo,
    canUndo,
    clear,
    clearAll,
    close,
    compareHwnd,
    configureHaskell,
    copy,
    createEditor,
    cut,
    disableEvents,
    enableEvents,
    endUndoAction,
    findText,
    getAllText,
    getCharAt,
    getCurrentPos,
    getFirstVisibleLine,
    getFocus,
    getHwnd,
    getLineCount,
    getLineFromPosition,
    getLinesOnScreen,
    getPositionFromLine,
    getPositionInfo,
    getSelText,
    getTextLen,
    getTextRange,
    getUndoCollection,
    gotoLine,
    gotoLineCol,
    gotoPos,
    gotoPosWithScroll,
    grabFocus,
    isClean,
    linesOnScreen,
    notifyGetCode,
    notifyGetHwnd,
    notifyGetListCompletionMethod,
    notifyGetPosition,
    notifyGetWParam,
    paste,
    redo,
    searchInTarget,
    searchNext,
    searchPrev,
    selectAll,
    selectWord,
    selectionIsEmpty,
    setAStyle,
    setEventHandler,
    setFirstVisibleLine,
    setFocus,
    setIndentationGuides,
    setLexer,
    setModEventMask,
    setReadOnly,
    setSavePoint,
    setSearchFlags,
    setSelectionEnd,
    setSelectionMode,
    setSelectionRange,
    setSelectionStart,
    setTabWidth,
    setTargetEnd,
    setTargetRange,
    setTargetStart,
    setTargetWholeDocument,
    setText,
    setUndoCollection,
    setUseTabs,
    showLastLine,
    snLine,
    snLinesAdded,
    snModificationType,
    snPosition,
    snUpdated,
    sortSelectedText,
    styleClearAll,
    undo, 
    white
) where 
    
import Control.Applicative ((<$>), (<*>))

import Data.Word (Word32, Word64)
import Data.Int (Int32, Int64)
import Data.List (sort)
import qualified Data.ByteString as BS (append, ByteString, init, replicate, useAsCString)
import qualified Data.ByteString.Char8 as BS (pack, unpack, lines, unlines)
import qualified Data.ByteString.Internal as BS (ByteString)
import qualified Data.ByteString.Unsafe  as BS (unsafeUseAsCString, unsafeUseAsCStringLen)
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
foreign import ccall safe "ScnEnableEvents"     c_ScnEnableEvents    :: HWND -> IO ()
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
                ttFChrgCpMin        :: Int32,
                ttFChrgCpMax        :: Int32,
                ttFLpstrText        :: Word64,
                ttFChrgTextCpMin    :: Int32,
                ttFChrgTextCpMax    :: Int32}

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
                ttFChrgCpMin
                ttFChrgCpMax
                ttFLpstrText
                ttFChrgTextCpMin
                ttFChrgTextCpMax) = do                        
            pokeByteOff ptr 0     ttFChrgCpMin
            pokeByteOff ptr 4     ttFChrgCpMax
            pokeByteOff ptr 8     ttFLpstrText                
            pokeByteOff ptr 16    ttFChrgTextCpMin           
            pokeByteOff ptr 20    ttFChrgTextCpMax
          
data SciTextRange = SciTextRange {
                trChrgCpMin    :: Int32,
                trChrgCpMax    :: Int32,
                trLpstrText    :: CString}

instance Storable SciTextRange where
        alignment _ = 8
        sizeOf _    = 16
        peek ptr    = SciTextRange
            <$> peekByteOff ptr 0
            <*> peekByteOff ptr 4
            <*> peekByteOff ptr 8 
        poke ptr (SciTextRange
                trChrgCpMin
                trChrgCpMax
                trLpstrText) = do                        
            pokeByteOff ptr 0 trChrgCpMin
            pokeByteOff ptr 4 trChrgCpMax
            pokeByteOff ptr 8 trLpstrText                

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
           
data Editor = Editor 
    { 
        hParent     :: HWND,
        hScnWnd     :: HWND 
    }  
 
-----------------------------------------------------------
-- helpers

instance Show Editor where
    show (Editor p e) = 
        "{Editor} Parent HWND: " ++ (ptrToString p) ++ 
        ", Editor HWND: " ++ (ptrToString e)

ioNull :: IO ()
ioNull = return ()

ioBool :: Int64 -> IO Bool
ioBool i = return (i /= 0)

ioInt :: Int64 -> IO Int
ioInt i = return (fromIntegral i :: Int)

-----------------------------------------------------------

-- Create the Scintilla editor window
-- parent = HWND of parent window
createEditor :: HWND -> IO (Editor)
createEditor parent = do
    hwnd <- c_ScnNewEditor parent
    return (Editor parent hwnd)
    
---------------------------------------------    
-- Callback from ScintillaProxy dll    
---------------------------------------------    

setEventHandler :: Editor -> (SCNotification -> IO ()) -> IO ()
setEventHandler scn@(Editor p c) eh = (createCallback $ callback scn eh) >>= c_ScnSetEventHandler c

enableEvents :: Editor -> IO ()
enableEvents (Editor _ c) = c_ScnEnableEvents c >> ioNull 

disableEvents :: Editor -> IO ()
disableEvents (Editor _ c) = c_ScnDisableEvents c >> ioNull 
   
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
setModEventMask :: Editor -> Word32 -> IO ()
setModEventMask e m = c_ScnSendEditorII (getHwnd e) sCI_SETMODEVENTMASK (fromIntegral m :: Word64) 0 >> ioNull 

-- callback from scintilla
callback :: Editor -> (SCNotification -> IO ()) -> Ptr (SCNotification) -> IO ()
callback scn eh p = do
    sn <- peek p 
    eh sn -- call client event handler
    case (notifyGetCode sn) of                  
        2007 -> updateBraces scn -- sCN_UPDATEUI                          
        otherwise -> return ()    

------------------------------------------------------------    
-- Accessors
------------------------------------------------------------    
    
getHwnd :: Editor -> HWND
getHwnd (Editor _ h) = h

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

compareHwnd :: Editor -> SCNotification -> Bool
compareHwnd scn sn = ptrToWord64 (getHwnd scn) == (notifyGetHwnd sn)

----------------------------------------------
-- Text Get and Set 
----------------------------------------------

clearAll :: Editor -> IO ()
clearAll e = c_ScnSendEditorII (getHwnd e) sCI_CLEARALL 0 0 >> ioNull

-- set the entire content of the editor    
setText :: Editor -> BS.ByteString -> IO ()
setText e bs = do
    let bs0 = BS.append bs (BS.replicate 1 0) -- add terminating null 
    BS.unsafeUseAsCString bs0 (\cs -> do c_ScnSendEditorIS (getHwnd e) sCI_SETTEXT 0 cs)
    return ()

-- get all text from editor    
getAllText :: Editor -> IO BS.ByteString
getAllText e = do            
    len <- getTextLen e
    let bs = (BS.replicate (len+1) 0)   -- allocate buffer
    BS.unsafeUseAsCString bs (\cs -> do c_ScnSendEditorIS (getHwnd e) sCI_GETTEXT (fromIntegral (len+1) :: Word64) cs)   
    return (BS.init bs) -- drop the zero byte at the end
    
getTextLen :: Editor -> IO Int
getTextLen e = c_ScnSendEditorII (getHwnd e) sCI_GETLENGTH 0 0 >>= ioInt
  
appendText :: Editor -> BS.ByteString -> IO ()
appendText e bs = do
    let bs0 = BS.append bs (BS.replicate 1 0) -- add terminating null 
    BS.unsafeUseAsCStringLen bs0 (\(cs, l) -> do c_ScnSendEditorIS (getHwnd e) sCI_APPENDTEXT (fromIntegral (l-1) :: Word64) cs)
    return ()
    
appendLine :: Editor -> BS.ByteString -> IO ()
appendLine scn bs = appendText scn $ BS.append bs $ BS.pack "\n"
    
getCharAt :: Editor -> Int -> IO Char
getCharAt e p = do
    c <- c_ScnSendEditorII (getHwnd e) sCI_GETCHARAT (fromIntegral p :: Word64) 0
    return (toEnum (fromIntegral c :: Int) :: Char)
   
appendTextS :: Editor -> String -> IO ()
appendTextS e s = withCStringLen s (\(cs, l) -> c_ScnSendEditorIS (getHwnd e) sCI_APPENDTEXT (fromIntegral (l) :: Word64) cs) >> ioNull
    
appendLineS :: Editor -> String -> IO ()
appendLineS scn s = appendTextS scn s >>  appendTextS scn "\n"

getTextRange :: Editor -> Int -> Int -> IO String
getTextRange e start end = do

    let bs = (BS.replicate (end-start+2) 0)   -- allocate for return string
    s <- BS.unsafeUseAsCString bs (\cs -> do 
        s <- alloca (\(ptr :: Ptr SciTextRange) -> do
            let range = (SciTextRange 
                    (fromIntegral start :: Int32)
                    (fromIntegral end   :: Int32)
                    cs)
            poke ptr range
            c_ScnSendEditorII (getHwnd e) sCI_GETTEXTRANGE 0 (ptrToInt64 ptr)
            (SciTextRange _ _ ps) <- peek ptr
            s <- peekCString ps
            return s)
        return s)
    return s

------------------------------------------------------------    
-- Scintilla commands
------------------------------------------------------------    

black :: COLORREF
black = (rgb 0 0 0)

white :: COLORREF
white = (rgb 0xff 0xff 0xff)

darkGreen :: COLORREF
darkGreen = (rgb 0 0x80 0)

keyBlue :: COLORREF
keyBlue = (rgb 0 0 230)

braceGood :: COLORREF
braceGood = (rgb 255 0 0)

braceBad :: COLORREF
braceBad = (rgb 150 0 150)

stringBrown :: COLORREF
stringBrown = (rgb 0xA0 0x10 0x20)

indents :: COLORREF
indents = (rgb 200 200 200)

-- configure lexer for haskell
configureHaskell :: Editor -> IO ()
configureHaskell e = do
    setLexer e (fromIntegral sCLEX_HASKELL :: Int)
    setKeywords e 0 ["do", "if", "then", "else", "case", "qualified", "case", "module", "of", "instance", 
                        "ccall", "safe", "unsafe", "import", "data", "deriving", "where", "as", "let",
                        "newtype", "type", "class", "in"]
    setAStyle e (fromIntegral sTYLE_DEFAULT :: Word64) black white 9 "Courier New"
    styleClearAll e
    setAStyle e (fromIntegral sCE_H_DEFAULT :: Word64) black white 9 "Courier New"
    setStyleColour e sCE_HA_KEYWORD       keyBlue     white
    setStyleColour e sCE_HA_STRING        stringBrown white
    setStyleColour e sCE_HA_IMPORT        keyBlue     white
    setStyleColour e sCE_HA_COMMENTLINE   darkGreen   white
    setStyleColour e sCE_HA_COMMENTBLOCK  darkGreen   white    
    setStyleColour e sCE_HA_COMMENTBLOCK2 darkGreen   white
    setStyleColour e sCE_HA_COMMENTBLOCK3 darkGreen   white
    setStyleColour e (fromIntegral sTYLE_BRACELIGHT :: Word64) braceGood white
    setStyleColour e (fromIntegral sTYLE_BRACEBAD :: Word64)   braceBad  white

    -- tabs and indents
    setTabWidth e 4
    setUseTabs e False
    setIndentationGuides e sC_IV_LOOKBOTH    
    setStyleColour e (fromIntegral sTYLE_INDENTGUIDE :: Word64) indents  white

    -- inhibit keys
    disbleKey e 16
    
    return ()
    
setLexer :: Editor -> Int -> IO ()
setLexer e s = do               
    c_ScnSendEditorII (getHwnd e) sCI_SETLEXER (fromIntegral s :: Word64) 0
    return ()

setKeywords :: Editor -> Int -> [String] -> IO ()
setKeywords e set ks = do
    withCString 
        (concat $ punctuate " " ks) 
        (\cs -> c_ScnSendEditorIS (getHwnd e) sCI_SETKEYWORDS 0 cs)
    return ()

setAStyle :: Editor -> Word64 -> COLORREF -> COLORREF -> Int -> String -> IO ()
setAStyle e st fc bc sz fnt = do
    let h = getHwnd e
    c_ScnSendEditorII h sCI_STYLESETFORE st (fromIntegral fc :: Int64)
    c_ScnSendEditorII h sCI_STYLESETBACK st (fromIntegral bc :: Int64)
    c_ScnSendEditorII h sCI_STYLESETSIZE st (fromIntegral sz :: Int64)
    withCString fnt (\cs -> c_ScnSendEditorIS h sCI_STYLESETFONT st cs)
    return ()
    
setStyleColour :: Editor -> Word64 -> COLORREF -> COLORREF -> IO ()
setStyleColour e st fc bc = do
    let h = getHwnd e
    c_ScnSendEditorII h sCI_STYLESETFORE st (fromIntegral fc :: Int64)
    c_ScnSendEditorII h sCI_STYLESETBACK st (fromIntegral bc :: Int64)
    return ()

styleClearAll :: Editor -> IO ()
styleClearAll e = do
    c_ScnSendEditorII (getHwnd e) sCI_STYLECLEARALL 0 0
    return ()
    
setSavePoint :: Editor -> IO ()
setSavePoint e = do
    c_ScnSendEditorII (getHwnd e) sCI_SETSAVEPOINT 0 0
    return ()

setReadOnly :: Editor -> Bool -> IO ()
setReadOnly e b = do
    c_ScnSendEditorII (getHwnd e) sCI_SETREADONLY (fromBool b :: Word64) 0
    return ()
      
isClean :: Editor -> IO Bool
isClean e = do
    x <- c_ScnSendEditorII (getHwnd e) sCI_GETMODIFY  0 0
    return (x == 0)
  
close :: Editor -> IO ()
close e = c_ScnDestroyEditor (getHwnd e)
 
----------------------------------------------
-- Undo and Redo 
----------------------------------------------

undo :: Editor -> IO ()
undo e = c_ScnSendEditorII (getHwnd e) sCI_UNDO 0 0 >> ioNull 
    
redo :: Editor -> IO ()
redo e = c_ScnSendEditorII (getHwnd e) sCI_REDO 0 0 >> ioNull 

canUndo :: Editor -> IO Bool
canUndo e = c_ScnSendEditorII (getHwnd e) sCI_CANUNDO 0 0 >>= ioBool

canRedo :: Editor -> IO Bool
canRedo e = c_ScnSendEditorII (getHwnd e) sCI_CANREDO 0 0 >>= ioBool

beginUndoAction :: Editor -> IO ()
beginUndoAction e = c_ScnSendEditorII (getHwnd e) sCI_BEGINUNDOACTION 0 0 >> ioNull 

endUndoAction :: Editor -> IO ()
endUndoAction e = c_ScnSendEditorII (getHwnd e) sCI_ENDUNDOACTION 0 0 >> ioNull 

setUndoCollection :: Editor -> Bool -> IO ()
setUndoCollection e b = c_ScnSendEditorII (getHwnd e) sCI_SETUNDOCOLLECTION (fromBool b :: Word64) 0 >> ioNull 

getUndoCollection :: Editor -> IO Bool
getUndoCollection e = c_ScnSendEditorII (getHwnd e) sCI_GETUNDOCOLLECTION 0 0 >>= ioBool
    
----------------------------------------------
-- Cut and Paste 
----------------------------------------------

cut :: Editor -> IO ()
cut e = c_ScnSendEditorII (getHwnd e) sCI_CUT 0 0 >> ioNull 

copy :: Editor -> IO ()
copy e = c_ScnSendEditorII (getHwnd e) sCI_COPY 0 0 >> ioNull 
  
paste :: Editor -> IO ()
paste e = c_ScnSendEditorII (getHwnd e) sCI_PASTE 0 0 >> ioNull 

clear :: Editor -> IO ()
clear e = c_ScnSendEditorII (getHwnd e) sCI_CLEAR 0 0 >> ioNull 

canPaste :: Editor -> IO Bool
canPaste e =  c_ScnSendEditorII (getHwnd e) sCI_CANPASTE 0 0 >>= ioBool
    
----------------------------------------------
-- Selection 
----------------------------------------------
    
selectionIsEmpty :: Editor -> IO Bool
selectionIsEmpty e = do
    b <- c_ScnSendEditorII (getHwnd e) sCI_GETSELTEXT  0 0
    return (b == 1)
  
selectAll :: Editor -> IO ()
selectAll e = c_ScnSendEditorII (getHwnd e) sCI_SELECTALL 0 0 >> ioNull 

{-
    sC_SEL_STREAM
    sC_SEL_RECTANGLE
    sC_SEL_LINES,
    sC_SEL_THIN,
-}      
setSelectionMode :: Editor -> Word32 -> IO ()
setSelectionMode e m = c_ScnSendEditorII (getHwnd e) sCI_SETSELECTIONMODE (fromIntegral m :: Word64) 0 >> ioNull 

getSelText :: Editor -> IO String
getSelText e = do
    len <- c_ScnSendEditorII (getHwnd e) sCI_GETSELTEXT 0 0
    if len > 1 then do
        let bs = (BS.replicate (fromIntegral len :: Int) 0)   -- allocate buffer
        BS.unsafeUseAsCString bs (\cs -> do c_ScnSendEditorIS (getHwnd e) sCI_GETSELTEXT 0 cs)   
        return $ BS.unpack bs
    else return ""

selectWord :: Editor -> IO ()
selectWord e = do
    p1 <- getCurrentPos e
    l  <- getTextLen e
    p2 <- findText e " " 0 p1 l
    p3 <- findText e "\r" 0 p1 l
    if (p1 < p2) then setSelectionRange e p1 (min p2 p3)
    else return ()

setSelectionStart :: Editor -> Int -> IO ()
setSelectionStart e p = c_ScnSendEditorII (getHwnd e) sCI_SETSELECTIONSTART (fromIntegral p :: Word64) 0 >> ioNull 

setSelectionEnd :: Editor -> Int -> IO ()
setSelectionEnd e p = c_ScnSendEditorII (getHwnd e) sCI_SETSELECTIONEND (fromIntegral p :: Word64) 0 >> ioNull 

setSelectionRange :: Editor -> Int -> Int -> IO ()
setSelectionRange e p1 p2 = setSelectionStart e p1 >> setSelectionEnd e p2

getSelectionStart :: Editor -> IO Int
getSelectionStart e = do
    p <- c_ScnSendEditorII (getHwnd e) sCI_GETSELECTIONSTART 0 0
    return (fromIntegral p :: Int)

getSelectionEnd :: Editor -> IO Int
getSelectionEnd e = do
    p <- c_ScnSendEditorII (getHwnd e) sCI_GETSELECTIONEND 0 0
    return (fromIntegral p :: Int)

sortSelectedText :: Editor -> IO ()
sortSelectedText e = do
    -- returns length of string + 1 for terminating null
    len <- c_ScnSendEditorII (getHwnd e) sCI_GETSELTEXT 0 0
    if len > 1 then do
        -- modify selection range to include whole lines only 
        ps <- getSelectionStart e 
        pe <- getSelectionEnd e
        let lp = sort [ps,pe]
        let ps' = lp !! 0       
        let pe' = lp !! 1        
        p1 <- findText e "\n" 0 ps' 0
        setSelectionStart e (p1+1)
        len <- getTextLen e
        p2 <- findText e "\n" 0 (pe'-1) len
        if p2 == -1 then setSelectionEnd e len
        else setSelectionEnd e (p2+1)
        -- selection size may have changed
        len <- c_ScnSendEditorII (getHwnd e) sCI_GETSELTEXT  0 0 
        beginUndoAction e
        let bs = (BS.replicate (fromIntegral len :: Int) 0)   -- allocate buffer
        BS.unsafeUseAsCString bs (\cs -> do c_ScnSendEditorIS (getHwnd e) sCI_GETSELTEXT 0 cs)         
        let bs' = (BS.unlines . sort . BS.lines) (BS.init bs)
        BS.useAsCString bs' (\cs -> c_ScnSendEditorIS (getHwnd e) sCI_REPLACESEL 0 cs) 
        endUndoAction e
        setSelectionStart e (p1+1)
        setSelectionEnd e (p2+1)
        return ()
    else return ()

----------------------------------------------
-- Brace Highlighting 
----------------------------------------------
  
braceHighlight :: Editor -> Int -> Int -> IO ()
braceHighlight e pa pb = c_ScnSendEditorII (getHwnd e) sCI_BRACEHIGHLIGHT (fromIntegral pa :: Word64) (fromIntegral pb :: Int64) >> ioNull 

braceBadLight :: Editor -> Int -> IO ()
braceBadLight e p = c_ScnSendEditorII (getHwnd e) sCI_BRACEBADLIGHT (fromIntegral p :: Word64) 0 >> ioNull 

braceMatch :: Editor -> Int -> IO Int
braceMatch e p = c_ScnSendEditorII (getHwnd e) sCI_BRACEMATCH  (fromIntegral p :: Word64) 0 >>= ioInt
    
updateBraces :: Editor -> IO ()
updateBraces e = do
    
    -- look at position ahead and behind the cursor
    -- (scintilla seems to tolerate positions out of range)
    pa <- getCurrentPos e
    let pss = [pa-1, pa]

    -- find the matching braces for both positions
    pes <- mapM (braceMatch e) pss
    
    -- (de)highlight in preference the brackets behind the cursor
    -- otherwise the brackets ahead of the cursor
    highlight pss pes
          
    where 
        highlight _ [-1, -1] = braceHighlight e (-1) (-1) -- dehighlight    
        highlight [psb, psa] [peb, pea] 
            | peb /= -1 = braceHighlight e psb peb
            | otherwise = braceHighlight e psa pea 
        
----------------------------------------------
-- Position and size 
----------------------------------------------

getLineCount :: Editor -> IO Int
getLineCount e = c_ScnSendEditorII (getHwnd e) sCI_GETLINECOUNT  0 0 >>= ioInt
   
getLinesOnScreen :: Editor -> IO Int
getLinesOnScreen e = c_ScnSendEditorII (getHwnd e) sCI_LINESONSCREEN  0 0 >>= ioInt
   
getCurrentPos :: Editor -> IO Int
getCurrentPos e = c_ScnSendEditorII (getHwnd e) sCI_GETCURRENTPOS  0 0 >>= ioInt

   
getPositionFromLine :: Editor -> Int -> IO Int
getPositionFromLine e l = c_ScnSendEditorII (getHwnd e) sCI_POSITIONFROMLINE  (fromIntegral l :: Word64) 0 >>= ioInt

getLineFromPosition :: Editor -> Int -> IO Int
getLineFromPosition e p = c_ScnSendEditorII (getHwnd e) sCI_LINEFROMPOSITION  (fromIntegral p :: Word64) 0 >>= ioInt

-- returns line, line position, doc position, total lines, total size
getPositionInfo :: Editor -> IO (Int, Int, Int, Int, Int) 
getPositionInfo e = do
    p <- getCurrentPos e
    l <- getLineFromPosition e p
    ls <- getPositionFromLine e l
    lc <- getLineCount e
    cc <- getTextLen e
    return (l, (p-ls), p, lc, cc)
    
gotoLine :: Editor -> Int -> IO ()
gotoLine e l = do
    c_ScnSendEditorII (getHwnd e) sCI_GOTOLINE  (fromIntegral l :: Word64) 0
    return ()

gotoPos :: Editor -> Int -> IO ()
gotoPos e p = do
    c_ScnSendEditorII (getHwnd e) sCI_GOTOPOS  (fromIntegral p :: Word64) 0
    return ()

showLastLine :: Editor -> IO ()
showLastLine e = do
    sl <- getLinesOnScreen e
    tl <- getLineCount e
    if (sl < tl) then (gotoLine e (tl-1)) 
    else return ()

gotoLineCol :: Editor -> Int -> Int -> IO ()
gotoLineCol e l c = do
    fl <- getFirstVisibleLine e
    sl <- linesOnScreen e
    p <- getPositionFromLine e l

    if (l < fl || l >= (fl+sl)) then do
        setFirstVisibleLine e (l-(sl `div` 2))
        gotoPos e (p+c)
        return ()
    else do 
        gotoPos e (p+c)
        return ()

gotoPosWithScroll :: Editor -> Int -> IO ()
gotoPosWithScroll e pos = do
    l <- getLineFromPosition e pos
    fl <- getFirstVisibleLine e
    sl <- linesOnScreen e

    if (l < fl || l >= (fl+sl)) then do
        setFirstVisibleLine e (l-(sl `div` 4))
        gotoPos e pos
        return ()
    else do 
        gotoPos e pos
        return ()

setFirstVisibleLine :: Editor -> Int -> IO ()
setFirstVisibleLine e l = c_ScnSendEditorII (getHwnd e) sCI_SETFIRSTVISIBLELINE  (fromIntegral l :: Word64) 0 >> ioNull

getFirstVisibleLine :: Editor -> IO Int
getFirstVisibleLine e = c_ScnSendEditorII (getHwnd e) sCI_GETFIRSTVISIBLELINE  0 0 >>= ioInt

linesOnScreen :: Editor -> IO Int
linesOnScreen e = c_ScnSendEditorII (getHwnd e) sCI_LINESONSCREEN  0 0 >>= ioInt

----------------------------------------------
-- Tabs 
----------------------------------------------

setTabWidth :: Editor -> Int -> IO ()
setTabWidth e w = c_ScnSendEditorII (getHwnd e) sCI_SETTABWIDTH  (fromIntegral w :: Word64) 0 >> ioNull


setUseTabs :: Editor -> Bool -> IO ()
setUseTabs e t = c_ScnSendEditorII (getHwnd e) sCI_SETUSETABS  (fromBool t :: Word64) 0 >> ioNull


{-
    sC_IV_NONE
    sC_IV_REAL
    sC_IV_LOOKFORWARD,
    sC_IV_LOOKBOTH,
-}      
setIndentationGuides :: Editor -> Word32 -> IO ()
setIndentationGuides e w = c_ScnSendEditorII (getHwnd e) sCI_SETINDENTATIONGUIDES  (fromIntegral w :: Word64) 0 >> ioNull
  
----------------------------------------------
-- Focus 
----------------------------------------------

setFocus :: Editor -> Bool -> IO ()
setFocus e b = do
    c_ScnSendEditorII (getHwnd e) sCI_SETFOCUS (fromBool b :: Word64) 0
    grabFocus e
    return ()

grabFocus :: Editor -> IO ()
grabFocus e = c_ScnSendEditorII (getHwnd e) sCI_GRABFOCUS 0 0 >> ioNull

getFocus :: Editor -> IO Bool
getFocus e = c_ScnSendEditorII (getHwnd e) sCI_GETFOCUS 0 0 >>= ioBool

----------------------------------------------
-- Search and replace
----------------------------------------------

setTargetStart :: Editor -> Int -> IO ()
setTargetStart e t = c_ScnSendEditorII (getHwnd e) sCI_SETTARGETSTART (fromIntegral t :: Word64) 0 >> ioNull

setTargetEnd :: Editor -> Int -> IO ()
setTargetEnd e t = c_ScnSendEditorII (getHwnd e) sCI_SETTARGETEND  (fromIntegral t :: Word64) 0  >> ioNull

setTargetRange :: Editor -> Int -> Int -> IO ()
setTargetRange e s f = c_ScnSendEditorII (getHwnd e) sCI_SETTARGETRANGE  (fromIntegral s :: Word64) (fromIntegral f :: Int64) >> ioNull


{-
    Second argument is the seqrch options:-

    sCFIND_WHOLEWORD,
    sCFIND_MATCHCASE,
    sCFIND_WORDSTART,
    sCFIND_REGEXP,
    sCFIND_POSIX,
    sCFIND_CXX11REGEX,
-}
setSearchFlags :: Editor -> Int -> IO ()
setSearchFlags e f = c_ScnSendEditorII (getHwnd e) sCI_SETSEARCHFLAGS  (fromIntegral f :: Word64) 0 >> ioNull

setTargetWholeDocument :: Editor -> IO ()
setTargetWholeDocument e = c_ScnSendEditorII (getHwnd e) sCI_TARGETWHOLEDOCUMENT 0 0 >> ioNull

searchInTarget :: Editor -> String -> IO Int
searchInTarget e s = do
    p <- withCStringLen s (\(cs, l) -> c_ScnSendEditorII (getHwnd e) sCI_SEARCHINTARGET (fromIntegral l :: Word64) (ptrToInt64 cs)) 
    return (fromIntegral p :: Int)

findText :: Editor -> String -> Word32 -> Int -> Int -> IO Int
findText e text ops start end = do
    p <- alloca (\(ptr :: Ptr SciTextToFind) -> do
        pos <- withCString text 
                (\ps -> do
                    let sci = (SciTextToFind 
                            (fromIntegral start :: Int32)
                            (fromIntegral end   :: Int32)
                            (ptrToWord64 ps)
                            0 0)    
                    poke ptr sci
                    c_ScnSendEditorII (getHwnd e) sCI_FINDTEXT (fromIntegral ops :: Word64) (ptrToInt64 ptr)
                )
        return (fromIntegral pos :: Int))
    return p

searchNext :: Editor -> String -> Int -> IO Int
searchNext e text ops = 
    withCString text 
        (\ps -> c_ScnSendEditorII (getHwnd e) sCI_SEARCHNEXT (fromIntegral ops :: Word64) (ptrToInt64 ps)) 
            >>= ioInt


searchPrev :: Editor -> String -> Int -> IO Int
searchPrev e text ops = 
    withCString text 
        (\ps -> c_ScnSendEditorII (getHwnd e) sCI_SEARCHPREV (fromIntegral ops :: Word64) (ptrToInt64 ps))
            >>= ioInt


----------------------------------------------
-- Keyboard mapping
----------------------------------------------

-- see scintilla help online for key codes
disbleKey :: Editor -> Int -> IO ()
disbleKey e kc = c_ScnSendEditorII (getHwnd e) sCI_ASSIGNCMDKEY (fromIntegral kc :: Word64) (fromIntegral sCI_NULL :: Int64) >> ioNull

