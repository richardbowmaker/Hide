
{-# LANGUAGE ScopedTypeVariables #-}

module Scintilla
(   
    Editor,
    SI.SCNotification,
    addPopupMenuItem,
    appendLine,
    appendLineS,
    appendText,
    appendTextS,
    beginUndoAction, 
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
    emptyUndoBuffer,
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
    getMarginMask,
    getMarginSensitive,
    getMarginType,
    getMarginWidth,
    getMargins,
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
    marginSetText,
    markerAdd,
    markerDefine,
    markerDelete,
    markerDeleteHandle,
    markerGet,
    markerLineFromHandle,
    markerSetBack,
    markerSetFore,
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
    setMarginLeft,
    setMarginMask,
    setMarginRight,
    setMarginSensitive,
    setMarginType,
    setMarginWidth,
    setMargins,
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
    sortSelectedText,
    styleClearAll,
    undo,
    usePopup,
) where 
    
import Control.Applicative ((<$>), (<*>))

import Data.Bits ((.|.), bit)
import Data.Int (Int32, Int64)
import Data.List (sort)
import Data.String.Combinators (punctuate)
import Data.Strings (strNull)
import Data.Word (Word32, Word64)
import qualified Data.ByteString as BS (append, ByteString, init, replicate, useAsCString)
import qualified Data.ByteString.Char8 as BS (pack, unpack, lines, unlines)
import qualified Data.ByteString.Internal as BS (ByteString)
import qualified Data.ByteString.Unsafe  as BS (unsafeUseAsCString, unsafeUseAsCStringLen)
import Foreign.C.String (CString, peekCString, withCString, withCStringLen)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (fromBool)
import Foreign.Ptr (FunPtr, Ptr, minusPtr, nullPtr)
import Foreign.Storable (Storable, alignment, sizeOf, peek, poke, pokeByteOff, peekByteOff)
import Graphics.Win32.GDI.Types (COLORREF, HWND)

-- project imports
import ScintillaConstants
import qualified Constants as CN
import Misc as MI
import qualified ScintillaProxyImports as SI

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
    hwnd <- SI.c_ScnNewEditor parent
    let e = (Editor parent hwnd) 
    setBufferedDraw e False
    return e
    
---------------------------------------------    
-- Callback from ScintillaProxy dll    
---------------------------------------------    

setEventHandler :: Editor -> (SI.SCNotification -> IO ()) -> IO ()
setEventHandler scn@(Editor p c) eh = (SI.c_ScnCreateCallback $ callback scn eh) >>= SI.c_ScnSetEventHandler c

enableEvents :: Editor -> IO ()
enableEvents (Editor _ c) = SI.c_ScnEnableEvents c >> ioNull 

disableEvents :: Editor -> IO ()
disableEvents (Editor _ c) = SI.c_ScnDisableEvents c >> ioNull 
   
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
setModEventMask e m = SI.c_ScnSendEditorII (getHwnd e) sCI_SETMODEVENTMASK (fromIntegral m :: Word64) 0 >> ioNull 

-- callback from scintilla
callback :: Editor -> (SI.SCNotification -> IO ()) -> Ptr (SI.SCNotification) -> IO ()
callback scn eh p = do
    sn <- peek p 
    eh sn -- call client event handler
    case (SI.notifyGetCode sn) of                  
        2007 -> updateBraces scn -- sCN_UPDATEUI                          
        otherwise -> return ()    

------------------------------------------------------------    
-- Accessors
------------------------------------------------------------    
    
getHwnd :: Editor -> HWND
getHwnd (Editor _ h) = h

compareHwnd :: Editor -> SI.SCNotification -> Bool
compareHwnd scn sn = ptrToWord64 (getHwnd scn) == (SI.notifyGetHwnd sn)

----------------------------------------------
-- Text Get and Set 
----------------------------------------------

clearAll :: Editor -> IO ()
clearAll e = SI.c_ScnSendEditorII (getHwnd e) sCI_CLEARALL 0 0 >> ioNull

-- set the entire content of the editor    
setText :: Editor -> BS.ByteString -> IO ()
setText e bs = do
    let bs0 = BS.append bs (BS.replicate 1 0) -- add terminating null 
    BS.unsafeUseAsCString bs0 (\cs -> do SI.c_ScnSendEditorIS (getHwnd e) sCI_SETTEXT 0 cs)
    emptyUndoBuffer e
    return ()

-- get all text from editor    
getAllText :: Editor -> IO BS.ByteString
getAllText e = do            
    len <- getTextLen e
    let bs = (BS.replicate (len+1) 0)   -- allocate buffer
    BS.unsafeUseAsCString bs (\cs -> do SI.c_ScnSendEditorIS (getHwnd e) sCI_GETTEXT (fromIntegral (len+1) :: Word64) cs)   
    return (BS.init bs) -- drop the zero byte at the end
    
getTextLen :: Editor -> IO Int
getTextLen e = SI.c_ScnSendEditorII (getHwnd e) sCI_GETLENGTH 0 0 >>= ioInt
  
appendText :: Editor -> BS.ByteString -> IO ()
appendText e bs = do
    let bs0 = BS.append bs (BS.replicate 1 0) -- add terminating null 
    BS.unsafeUseAsCStringLen bs0 (\(cs, l) -> do SI.c_ScnSendEditorIS (getHwnd e) sCI_APPENDTEXT (fromIntegral (l-1) :: Word64) cs)
    return ()
    
appendLine :: Editor -> BS.ByteString -> IO ()
appendLine scn bs = appendText scn $ BS.append bs $ BS.pack "\n"
    
getCharAt :: Editor -> Int -> IO Char
getCharAt e p = do
    c <- SI.c_ScnSendEditorII (getHwnd e) sCI_GETCHARAT (fromIntegral p :: Word64) 0
    return (toEnum (fromIntegral c :: Int) :: Char)
   
appendTextS :: Editor -> String -> IO ()
appendTextS e s = withCStringLen s (\(cs, l) -> SI.c_ScnSendEditorIS (getHwnd e) sCI_APPENDTEXT (fromIntegral (l) :: Word64) cs) >> ioNull
    
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
            SI.c_ScnSendEditorII (getHwnd e) sCI_GETTEXTRANGE 0 (ptrToInt64 ptr)
            (SciTextRange _ _ ps) <- peek ptr
            s <- peekCString ps
            return s)
        return s)
    return s

------------------------------------------------------------    
-- Scintilla commands
------------------------------------------------------------    

-- configure lexer for haskell
configureHaskell :: Editor -> IO ()
configureHaskell e = do
    setLexer e (fromIntegral sCLEX_HASKELL :: Int)
    setKeywords e 0 ["do", "if", "then", "else", "case", "qualified", "case", "module", "of", "instance", 
                        "ccall", "safe", "unsafe", "import", "data", "deriving", "where", "as", "let",
                        "newtype", "type", "class", "in"]
    setAStyle e (fromIntegral sTYLE_DEFAULT :: Word64) CN.black CN.white 9 "Courier New"
    styleClearAll e
    setAStyle e (fromIntegral sCE_H_DEFAULT :: Word64) CN.black CN.white 9 "Courier New"
    setStyleColour e sCE_HA_KEYWORD       CN.keyBlue     CN.white
    setStyleColour e sCE_HA_STRING        CN.stringBrown CN.white
    setStyleColour e sCE_HA_IMPORT        CN.keyBlue     CN.white
    setStyleColour e sCE_HA_COMMENTLINE   CN.darkGreen   CN.white
    setStyleColour e sCE_HA_COMMENTBLOCK  CN.darkGreen   CN.white    
    setStyleColour e sCE_HA_COMMENTBLOCK2 CN.darkGreen   CN.white
    setStyleColour e sCE_HA_COMMENTBLOCK3 CN.darkGreen   CN.white
    setStyleColour e (fromIntegral sTYLE_BRACELIGHT :: Word64) CN.braceGood CN.white
    setStyleColour e (fromIntegral sTYLE_BRACEBAD :: Word64)   CN.braceBad  CN.white

    -- tabs and indents
    setTabWidth e 4
    setUseTabs e False
    setIndentationGuides e sC_IV_LOOKBOTH    
    setStyleColour e (fromIntegral sTYLE_INDENTGUIDE :: Word64) CN.indents  CN.white

    -- popup menu handled by scintilla proxy dll
    usePopup e sC_POPUP_NEVER

    -- margins
    setMarginLeft e 5
    setMargins e 2
    setMarginType  e CN.lineMargin sC_MARGIN_NUMBER
    setMarginWidth e CN.lineMargin 40
    setMarginType  e CN.symbolMargin sC_MARGIN_SYMBOL
    setMarginWidth e CN.symbolMargin 20

    -- margin markers
    markerDefine e CN.breakPointMarker sC_MARK_CIRCLE
    markerDefine e CN.bookMarkMarker sC_MARK_BOOKMARK
    setMarginMask e 1 $ (fromIntegral sC_MASK_FOLDERS :: Int) 
        .|. (bit CN.breakPointMarker)
        .|. (bit CN.bookMarkMarker)

    markerSetFore e CN.breakPointMarker CN.red
    markerSetBack e CN.breakPointMarker CN.red
    markerSetFore e CN.bookMarkMarker   CN.blue
    markerSetBack e CN.bookMarkMarker   CN.blue

    return ()
    
setLexer :: Editor -> Int -> IO ()
setLexer e s = SI.c_ScnSendEditorII (getHwnd e) sCI_SETLEXER (fromIntegral s :: Word64) 0 >> ioNull

setKeywords :: Editor -> Int -> [String] -> IO ()
setKeywords e set ks = do
    withCString 
        (concat $ punctuate " " ks) 
        (\cs -> SI.c_ScnSendEditorIS (getHwnd e) sCI_SETKEYWORDS 0 cs)
    return ()

setAStyle :: Editor -> Word64 -> COLORREF -> COLORREF -> Int -> String -> IO ()
setAStyle e st fc bc sz fnt = do
    let h = getHwnd e
    SI.c_ScnSendEditorII h sCI_STYLESETFORE st (fromIntegral fc :: Int64)
    SI.c_ScnSendEditorII h sCI_STYLESETBACK st (fromIntegral bc :: Int64)
    SI.c_ScnSendEditorII h sCI_STYLESETSIZE st (fromIntegral sz :: Int64)
    withCString fnt (\cs -> SI.c_ScnSendEditorIS h sCI_STYLESETFONT st cs)
    return ()
    
setStyleColour :: Editor -> Word64 -> COLORREF -> COLORREF -> IO ()
setStyleColour e st fc bc = do
    let h = getHwnd e
    SI.c_ScnSendEditorII h sCI_STYLESETFORE st (fromIntegral fc :: Int64)
    SI.c_ScnSendEditorII h sCI_STYLESETBACK st (fromIntegral bc :: Int64)
    return ()

styleClearAll :: Editor -> IO ()
styleClearAll e = SI.c_ScnSendEditorII (getHwnd e) sCI_STYLECLEARALL 0 0 >> ioNull
    
setSavePoint :: Editor -> IO ()
setSavePoint e = SI.c_ScnSendEditorII (getHwnd e) sCI_SETSAVEPOINT 0 0 >> ioNull

setReadOnly :: Editor -> Bool -> IO ()
setReadOnly e b = SI.c_ScnSendEditorII (getHwnd e) sCI_SETREADONLY (fromBool b :: Word64) 0 >> ioNull
     
isClean :: Editor -> IO Bool
isClean e = do
    x <- SI.c_ScnSendEditorII (getHwnd e) sCI_GETMODIFY  0 0
    return (x == 0)
  
close :: Editor -> IO ()
close e = SI.c_ScnDestroyEditor (getHwnd e)
 
----------------------------------------------
-- Undo and Redo 
----------------------------------------------

undo :: Editor -> IO ()
undo e = SI.c_ScnSendEditorII (getHwnd e) sCI_UNDO 0 0 >> ioNull 
    
redo :: Editor -> IO ()
redo e = SI.c_ScnSendEditorII (getHwnd e) sCI_REDO 0 0 >> ioNull 

canUndo :: Editor -> IO Bool
canUndo e = SI.c_ScnSendEditorII (getHwnd e) sCI_CANUNDO 0 0 >>= ioBool

canRedo :: Editor -> IO Bool
canRedo e = SI.c_ScnSendEditorII (getHwnd e) sCI_CANREDO 0 0 >>= ioBool

beginUndoAction :: Editor -> IO ()
beginUndoAction e = SI.c_ScnSendEditorII (getHwnd e) sCI_BEGINUNDOACTION 0 0 >> ioNull 

endUndoAction :: Editor -> IO ()
endUndoAction e = SI.c_ScnSendEditorII (getHwnd e) sCI_ENDUNDOACTION 0 0 >> ioNull 

setUndoCollection :: Editor -> Bool -> IO ()
setUndoCollection e b = SI.c_ScnSendEditorII (getHwnd e) sCI_SETUNDOCOLLECTION (fromBool b :: Word64) 0 >> ioNull 

getUndoCollection :: Editor -> IO Bool
getUndoCollection e = SI.c_ScnSendEditorII (getHwnd e) sCI_GETUNDOCOLLECTION 0 0 >>= ioBool
    
emptyUndoBuffer :: Editor -> IO ()
emptyUndoBuffer e = SI.c_ScnSendEditorII (getHwnd e) sCI_EMPTYUNDOBUFFER 0 0 >> ioNull 

----------------------------------------------
-- Cut and Paste 
----------------------------------------------

cut :: Editor -> IO ()
cut e = SI.c_ScnSendEditorII (getHwnd e) sCI_CUT 0 0 >> ioNull 

copy :: Editor -> IO ()
copy e = SI.c_ScnSendEditorII (getHwnd e) sCI_COPY 0 0 >> ioNull 
  
paste :: Editor -> IO ()
paste e = SI.c_ScnSendEditorII (getHwnd e) sCI_PASTE 0 0 >> ioNull 

clear :: Editor -> IO ()
clear e = SI.c_ScnSendEditorII (getHwnd e) sCI_CLEAR 0 0 >> ioNull 

canPaste :: Editor -> IO Bool
canPaste e =  SI.c_ScnSendEditorII (getHwnd e) sCI_CANPASTE 0 0 >>= ioBool
    
----------------------------------------------
-- Selection 
----------------------------------------------
    
selectionIsEmpty :: Editor -> IO Bool
selectionIsEmpty e = do
    b <- SI.c_ScnSendEditorII (getHwnd e) sCI_GETSELTEXT  0 0
    return (b == 1)
  
selectAll :: Editor -> IO ()
selectAll e = SI.c_ScnSendEditorII (getHwnd e) sCI_SELECTALL 0 0 >> ioNull 

{-
    sC_SEL_STREAM
    sC_SEL_RECTANGLE
    sC_SEL_LINES,
    sC_SEL_THIN,
-}      
setSelectionMode :: Editor -> Word32 -> IO ()
setSelectionMode e m = SI.c_ScnSendEditorII (getHwnd e) sCI_SETSELECTIONMODE (fromIntegral m :: Word64) 0 >> ioNull 

getSelText :: Editor -> IO String
getSelText e = do
    len <- SI.c_ScnSendEditorII (getHwnd e) sCI_GETSELTEXT 0 0
    if len > 1 then do
        let bs = (BS.replicate (fromIntegral len :: Int) 0)   -- allocate buffer
        BS.unsafeUseAsCString bs (\cs -> do SI.c_ScnSendEditorIS (getHwnd e) sCI_GETSELTEXT 0 cs)   
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
setSelectionStart e p = SI.c_ScnSendEditorII (getHwnd e) sCI_SETSELECTIONSTART (fromIntegral p :: Word64) 0 >> ioNull 

setSelectionEnd :: Editor -> Int -> IO ()
setSelectionEnd e p = SI.c_ScnSendEditorII (getHwnd e) sCI_SETSELECTIONEND (fromIntegral p :: Word64) 0 >> ioNull 

setSelectionRange :: Editor -> Int -> Int -> IO ()
setSelectionRange e p1 p2 = setSelectionStart e p1 >> setSelectionEnd e p2

getSelectionStart :: Editor -> IO Int
getSelectionStart e = do
    p <- SI.c_ScnSendEditorII (getHwnd e) sCI_GETSELECTIONSTART 0 0
    return (fromIntegral p :: Int)

getSelectionEnd :: Editor -> IO Int
getSelectionEnd e = do
    p <- SI.c_ScnSendEditorII (getHwnd e) sCI_GETSELECTIONEND 0 0
    return (fromIntegral p :: Int)

sortSelectedText :: Editor -> IO ()
sortSelectedText e = do
    -- returns length of string + 1 for terminating null
    len <- SI.c_ScnSendEditorII (getHwnd e) sCI_GETSELTEXT 0 0
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
        len <- SI.c_ScnSendEditorII (getHwnd e) sCI_GETSELTEXT  0 0 
        beginUndoAction e
        let bs = (BS.replicate (fromIntegral len :: Int) 0)   -- allocate buffer
        BS.unsafeUseAsCString bs (\cs -> do SI.c_ScnSendEditorIS (getHwnd e) sCI_GETSELTEXT 0 cs)         
        let bs' = (BS.unlines . sort . BS.lines) (BS.init bs)
        BS.useAsCString bs' (\cs -> SI.c_ScnSendEditorIS (getHwnd e) sCI_REPLACESEL 0 cs) 
        endUndoAction e
        setSelectionStart e (p1+1)
        setSelectionEnd e (p2+1)
        return ()
    else return ()

----------------------------------------------
-- Brace Highlighting 
----------------------------------------------
  
braceHighlight :: Editor -> Int -> Int -> IO ()
braceHighlight e pa pb = SI.c_ScnSendEditorII (getHwnd e) sCI_BRACEHIGHLIGHT (fromIntegral pa :: Word64) (fromIntegral pb :: Int64) >> ioNull 

braceBadLight :: Editor -> Int -> IO ()
braceBadLight e p = SI.c_ScnSendEditorII (getHwnd e) sCI_BRACEBADLIGHT (fromIntegral p :: Word64) 0 >> ioNull 

braceMatch :: Editor -> Int -> IO Int
braceMatch e p = SI.c_ScnSendEditorII (getHwnd e) sCI_BRACEMATCH  (fromIntegral p :: Word64) 0 >>= ioInt
    
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
getLineCount e = SI.c_ScnSendEditorII (getHwnd e) sCI_GETLINECOUNT  0 0 >>= ioInt
   
getLinesOnScreen :: Editor -> IO Int
getLinesOnScreen e = SI.c_ScnSendEditorII (getHwnd e) sCI_LINESONSCREEN  0 0 >>= ioInt
   
getCurrentPos :: Editor -> IO Int
getCurrentPos e = SI.c_ScnSendEditorII (getHwnd e) sCI_GETCURRENTPOS  0 0 >>= ioInt

   
getPositionFromLine :: Editor -> Int -> IO Int
getPositionFromLine e l = SI.c_ScnSendEditorII (getHwnd e) sCI_POSITIONFROMLINE  (fromIntegral l :: Word64) 0 >>= ioInt

getLineFromPosition :: Editor -> Int -> IO Int
getLineFromPosition e p = SI.c_ScnSendEditorII (getHwnd e) sCI_LINEFROMPOSITION  (fromIntegral p :: Word64) 0 >>= ioInt

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
    SI.c_ScnSendEditorII (getHwnd e) sCI_GOTOLINE  (fromIntegral l :: Word64) 0
    return ()

gotoPos :: Editor -> Int -> IO ()
gotoPos e p = do
    SI.c_ScnSendEditorII (getHwnd e) sCI_GOTOPOS  (fromIntegral p :: Word64) 0
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
setFirstVisibleLine e l = SI.c_ScnSendEditorII (getHwnd e) sCI_SETFIRSTVISIBLELINE  (fromIntegral l :: Word64) 0 >> ioNull

getFirstVisibleLine :: Editor -> IO Int
getFirstVisibleLine e = SI.c_ScnSendEditorII (getHwnd e) sCI_GETFIRSTVISIBLELINE  0 0 >>= ioInt

linesOnScreen :: Editor -> IO Int
linesOnScreen e = SI.c_ScnSendEditorII (getHwnd e) sCI_LINESONSCREEN  0 0 >>= ioInt

----------------------------------------------
-- Tabs 
----------------------------------------------

setTabWidth :: Editor -> Int -> IO ()
setTabWidth e w = SI.c_ScnSendEditorII (getHwnd e) sCI_SETTABWIDTH  (fromIntegral w :: Word64) 0 >> ioNull


setUseTabs :: Editor -> Bool -> IO ()
setUseTabs e t = SI.c_ScnSendEditorII (getHwnd e) sCI_SETUSETABS  (fromBool t :: Word64) 0 >> ioNull


{-
    sC_IV_NONE
    sC_IV_REAL
    sC_IV_LOOKFORWARD,
    sC_IV_LOOKBOTH,
-}      
setIndentationGuides :: Editor -> Word32 -> IO ()
setIndentationGuides e w = SI.c_ScnSendEditorII (getHwnd e) sCI_SETINDENTATIONGUIDES  (fromIntegral w :: Word64) 0 >> ioNull
  
----------------------------------------------
-- Focus 
----------------------------------------------

setFocus :: Editor -> Bool -> IO ()
setFocus e b = do
    SI.c_ScnSendEditorII (getHwnd e) sCI_SETFOCUS (fromBool b :: Word64) 0
    grabFocus e
    return ()

grabFocus :: Editor -> IO ()
grabFocus e = SI.c_ScnSendEditorII (getHwnd e) sCI_GRABFOCUS 0 0 >> ioNull

getFocus :: Editor -> IO Bool
getFocus e = SI.c_ScnSendEditorII (getHwnd e) sCI_GETFOCUS 0 0 >>= ioBool

----------------------------------------------
-- Search and replace
----------------------------------------------

setTargetStart :: Editor -> Int -> IO ()
setTargetStart e t = SI.c_ScnSendEditorII (getHwnd e) sCI_SETTARGETSTART (fromIntegral t :: Word64) 0 >> ioNull

setTargetEnd :: Editor -> Int -> IO ()
setTargetEnd e t = SI.c_ScnSendEditorII (getHwnd e) sCI_SETTARGETEND  (fromIntegral t :: Word64) 0  >> ioNull

setTargetRange :: Editor -> Int -> Int -> IO ()
setTargetRange e s f = SI.c_ScnSendEditorII (getHwnd e) sCI_SETTARGETRANGE  (fromIntegral s :: Word64) (fromIntegral f :: Int64) >> ioNull


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
setSearchFlags e f = SI.c_ScnSendEditorII (getHwnd e) sCI_SETSEARCHFLAGS  (fromIntegral f :: Word64) 0 >> ioNull

setTargetWholeDocument :: Editor -> IO ()
setTargetWholeDocument e = SI.c_ScnSendEditorII (getHwnd e) sCI_TARGETWHOLEDOCUMENT 0 0 >> ioNull

searchInTarget :: Editor -> String -> IO Int
searchInTarget e s = do
    p <- withCStringLen s (\(cs, l) -> SI.c_ScnSendEditorII (getHwnd e) sCI_SEARCHINTARGET (fromIntegral l :: Word64) (ptrToInt64 cs)) 
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
                    SI.c_ScnSendEditorII (getHwnd e) sCI_FINDTEXT (fromIntegral ops :: Word64) (ptrToInt64 ptr)
                )
        return (fromIntegral pos :: Int))
    return p

searchNext :: Editor -> String -> Int -> IO Int
searchNext e text ops = 
    withCString text 
        (\ps -> SI.c_ScnSendEditorII (getHwnd e) sCI_SEARCHNEXT (fromIntegral ops :: Word64) (ptrToInt64 ps)) 
            >>= ioInt

searchPrev :: Editor -> String -> Int -> IO Int
searchPrev e text ops = 
    withCString text 
        (\ps -> SI.c_ScnSendEditorII (getHwnd e) sCI_SEARCHPREV (fromIntegral ops :: Word64) (ptrToInt64 ps))
            >>= ioInt

----------------------------------------------
-- Keyboard mapping
----------------------------------------------

-- see scintilla help online for key codes
clearCmdKey :: Editor -> Int -> IO ()
clearCmdKey e kc = SI.c_ScnSendEditorII (getHwnd e) sCI_CLEARCMDKEY (fromIntegral kc :: Word64) 0 >> ioNull

----------------------------------------------
-- Popup Menu
----------------------------------------------

{-
    Second argument is the popup selection:-

    sC_POPUP_NEVER,
    sC_POPUP_ALL,
    sC_POPUP_TEXT,
-}
usePopup :: Editor -> Word32 -> IO ()
usePopup e p = SI.c_ScnSendEditorII (getHwnd e) sCI_USEPOPUP  (fromIntegral p :: Word64) 0 >> ioNull

-- addPopupMenuItem scn 1000 "Option 1" menufunction isenabled :-
--  appends a menu item to the editors context menu with resource id 100, title = Option 1, 
--  a function that is executed when the option is selected
--  a function that is called before the popup is displayed and returns 0-disabled, 1-enabled
addPopupMenuItem :: Editor -> Int -> String -> (Editor -> Int -> IO ()) -> (Editor -> Int -> IO Int) -> IO ()
addPopupMenuItem e id title handler enabled = do
    mf <- SI.c_ScnCreateHandlerCallback $ handler e
    eh <- SI.c_ScnCreateEnabledCallback $ enabled e
    withCString title (\cs -> SI.c_ScnAddPopupMenuItem (getHwnd e) (fromIntegral id :: Int32) cs mf eh)
  

----------------------------------------------
-- Margins
-- margin numbers are 0..4
----------------------------------------------
  
setMargins :: Editor -> Int -> IO ()
setMargins e m = SI.c_ScnSendEditorII (getHwnd e) sCI_SETMARGINS (fromIntegral m :: Word64) 0 >> ioNull

getMargins :: Editor -> IO Int
getMargins e = SI.c_ScnSendEditorII (getHwnd e) sCI_GETMARGINS  0 0 >>= ioInt

{-
Valid types are 
SC_MARGIN_SYMBOL
SC_MARGIN_NUMBER
SC_MARGIN_TEXT
SC_MARGIN_RTEXT
SC_MARGIN_BACK 
SC_MARGIN_FORE
SC_MARGIN_COLOUR   
-} 

setMarginType :: Editor -> Int -> Word32 -> IO ()
setMarginType e m t = SI.c_ScnSendEditorII (getHwnd e) sCI_SETMARGINTYPEN (fromIntegral m :: Word64) (fromIntegral t :: Int64) >> ioNull

getMarginType :: Editor -> Int -> IO Int
getMarginType e m = SI.c_ScnSendEditorII (getHwnd e) sCI_GETMARGINTYPEN (fromIntegral m :: Word64) 0 >>= ioInt

setMarginWidth :: Editor -> Int -> Word32 -> IO ()
setMarginWidth e m t = SI.c_ScnSendEditorII (getHwnd e) sCI_SETMARGINWIDTHN (fromIntegral m :: Word64) (fromIntegral t :: Int64) >> ioNull

getMarginWidth :: Editor -> Int -> IO Int
getMarginWidth e m = SI.c_ScnSendEditorII (getHwnd e) sCI_GETMARGINWIDTHN (fromIntegral m :: Word64) 0 >>= ioInt

setMarginMask :: Editor -> Int -> Int -> IO ()
setMarginMask e m b = SI.c_ScnSendEditorII (getHwnd e) sCI_SETMARGINMASKN (fromIntegral m :: Word64) (fromIntegral b :: Int64) >> ioNull

getMarginMask :: Editor -> Int -> IO Int
getMarginMask e m = SI.c_ScnSendEditorII (getHwnd e) sCI_GETMARGINMASKN (fromIntegral m :: Word64) 0 >>= ioInt

setMarginSensitive :: Editor -> Int -> Bool -> IO ()
setMarginSensitive e m b = SI.c_ScnSendEditorII (getHwnd e) sCI_SETMARGINSENSITIVEN (fromIntegral m :: Word64) (fromBool b :: Int64) >> ioNull

getMarginSensitive :: Editor -> Int -> IO Bool
getMarginSensitive e m = SI.c_ScnSendEditorII (getHwnd e) sCI_GETMARGINSENSITIVEN (fromIntegral m :: Word64) 0 >>= ioBool

marginSetText :: Editor -> Int -> String -> IO ()
marginSetText e l s = withCString s (\cs -> SI.c_ScnSendEditorIS (getHwnd e) sCI_MARGINSETTEXT (fromIntegral l :: Word64) cs) >> ioNull

marginSetStyle :: Editor -> Int -> Int -> IO ()
marginSetStyle e l s = SI.c_ScnSendEditorII (getHwnd e) sCI_MARGINSETSTYLE (fromIntegral l :: Word64) (fromIntegral s :: Int64) >> ioNull

setMarginLeft :: Editor -> Int -> IO Int
setMarginLeft e m = SI.c_ScnSendEditorII (getHwnd e) sCI_SETMARGINLEFT 0 (fromIntegral m :: Int64) >>= ioInt

setMarginRight :: Editor -> Int -> IO Int
setMarginRight e m = SI.c_ScnSendEditorII (getHwnd e) sCI_SETMARGINRIGHT 0 (fromIntegral m :: Int64) >>= ioInt

----------------------------------------------
-- Markers
----------------------------------------------

markerDefine :: Editor -> Int -> Word32 -> IO ()
markerDefine e m b = SI.c_ScnSendEditorII (getHwnd e) sCI_MARKERDEFINE (fromIntegral m :: Word64) (fromIntegral b :: Int64) >> ioNull

markerSetFore :: Editor -> Int -> COLORREF -> IO ()
markerSetFore e m c = SI.c_ScnSendEditorII (getHwnd e) sCI_MARKERSETFORE (fromIntegral m :: Word64) (fromIntegral c :: Int64) >> ioNull

markerSetBack :: Editor -> Int -> COLORREF -> IO ()
markerSetBack e m c = SI.c_ScnSendEditorII (getHwnd e) sCI_MARKERSETBACK (fromIntegral m :: Word64) (fromIntegral c :: Int64) >> ioNull

markerAdd :: Editor -> Int -> Int -> IO Int
markerAdd e m n = SI.c_ScnSendEditorII (getHwnd e) sCI_MARKERADD (fromIntegral m :: Word64) (fromIntegral n :: Int64) >>= ioInt

markerGet :: Editor -> Int -> IO Int
markerGet e l = SI.c_ScnSendEditorII (getHwnd e) sCI_MARKERGET (fromIntegral l :: Word64) 0 >>= ioInt
 
markerDeleteHandle :: Editor -> Int -> IO ()
markerDeleteHandle e h = SI.c_ScnSendEditorII (getHwnd e) sCI_MARKERDELETEHANDLE (fromIntegral h :: Word64) 0 >> ioNull

markerDelete :: Editor -> Int -> Int -> IO ()
markerDelete e l m = SI.c_ScnSendEditorII (getHwnd e) sCI_MARKERDELETE (fromIntegral l :: Word64) (fromIntegral m :: Int64) >> ioNull

markerLineFromHandle :: Editor -> Int -> IO Int
markerLineFromHandle e h = SI.c_ScnSendEditorII (getHwnd e) sCI_MARKERLINEFROMHANDLE (fromIntegral h :: Word64) 0 >>= ioInt

----------------------------------------------
-- Other settings
----------------------------------------------

setBufferedDraw :: Editor -> Bool -> IO ()
setBufferedDraw e b = SI.c_ScnSendEditorII (getHwnd e) sCI_SETBUFFEREDDRAW (fromBool b :: Word64) 0 >> ioNull

getBufferedDraw :: Editor -> IO Bool
getBufferedDraw e = SI.c_ScnSendEditorII (getHwnd e) sCI_GETBUFFEREDDRAW 0 0 >>= ioBool



