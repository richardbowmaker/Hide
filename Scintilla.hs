
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
    isLineVisible,
    linesOnScreen,
    marginSetText,
    markerAdd,
    markerDefine,
    markerDelete,
    markerDeleteAll,
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
    selectLinesCols,
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
    usePopup
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
enableEvents (Editor _ c) = SI.c_ScnEnableEvents c 

disableEvents :: Editor -> IO ()
disableEvents (Editor _ c) = SI.c_ScnDisableEvents c 
   
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
setModEventMask :: Editor -> Int -> IO ()
setModEventMask e m = SI.sciSendEditorIO (getHwnd e) sCI_SETMODEVENTMASK m 0 

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
clearAll e = SI.sciSendEditorIO (getHwnd e) sCI_CLEARALL 0 0

-- set the entire content of the editor    
setText :: Editor -> BS.ByteString -> IO ()
setText e bs = do
    let bs0 = BS.append bs (BS.replicate 1 0) -- add terminating null 
    BS.unsafeUseAsCString bs0 (\cs -> do SI.sciSendEditorSO (getHwnd e) sCI_SETTEXT 0 cs)
    emptyUndoBuffer e
    return ()

-- get all text from editor    
getAllText :: Editor -> IO BS.ByteString
getAllText e = do            
    len <- getTextLen e
    let bs = (BS.replicate (len+1) 0)   -- allocate buffer
    BS.unsafeUseAsCString bs (\cs -> do SI.sciSendEditorSO (getHwnd e) sCI_GETTEXT (len+1) cs)   
    return (BS.init bs) -- drop the zero byte at the end
    
getTextLen :: Editor -> IO Int
getTextLen e = SI.sciSendEditorII (getHwnd e) sCI_GETLENGTH 0 0
  
appendText :: Editor -> BS.ByteString -> IO ()
appendText e bs = do
    let bs0 = BS.append bs (BS.replicate 1 0) -- add terminating null 
    BS.unsafeUseAsCStringLen bs0 (\(cs, l) -> do SI.sciSendEditorSO (getHwnd e) sCI_APPENDTEXT (l-1) cs)
    return ()
    
appendLine :: Editor -> BS.ByteString -> IO ()
appendLine scn bs = appendText scn $ BS.append bs $ BS.pack "\n"
    
getCharAt :: Editor -> Int -> IO Char
getCharAt e p = do
    c <- SI.sciSendEditorII (getHwnd e) sCI_GETCHARAT p 0
    return (toEnum (fromIntegral c :: Int) :: Char)
   
appendTextS :: Editor -> String -> IO ()
appendTextS e s = withCStringLen s (\(cs, l) -> SI.sciSendEditorSO (getHwnd e) sCI_APPENDTEXT l cs)
    
appendLineS :: Editor -> String -> IO ()
appendLineS scn s = appendTextS scn s >>  appendTextS scn "\n"

getTextRange :: Editor -> Int -> Int -> IO String
getTextRange e start end = do
    let bs = (BS.replicate (end-start+2) 0)   -- allocate for return string
    s <- BS.unsafeUseAsCString bs (\cs -> do 
        s <- alloca (\(ptr :: Ptr SciTextRange) -> do
            let range = (SciTextRange (fromIntegral start :: Int32) (fromIntegral end :: Int32) cs)
            poke ptr range
            SI.sciSendEditorIO (getHwnd e) sCI_GETTEXTRANGE 0 (ptrToInt ptr)
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
    setLexer e sCLEX_HASKELL
    setKeywords e 0 ["do", "if", "then", "else", "case", "qualified", "case", "module", "of", "instance", 
                        "ccall", "safe", "unsafe", "import", "data", "deriving", "where", "as", "let",
                        "newtype", "type", "class", "in"]
    setAStyle e sTYLE_DEFAULT CN.black CN.white 9 "Courier New"
    styleClearAll e
    setAStyle e sCE_H_DEFAULT CN.black CN.white 9 "Courier New"
    setStyleColour e sCE_HA_KEYWORD       CN.keyBlue     CN.white
    setStyleColour e sCE_HA_STRING        CN.stringBrown CN.white
    setStyleColour e sCE_HA_IMPORT        CN.keyBlue     CN.white
    setStyleColour e sCE_HA_COMMENTLINE   CN.darkGreen   CN.white
    setStyleColour e sCE_HA_COMMENTBLOCK  CN.darkGreen   CN.white    
    setStyleColour e sCE_HA_COMMENTBLOCK2 CN.darkGreen   CN.white
    setStyleColour e sCE_HA_COMMENTBLOCK3 CN.darkGreen   CN.white
    setStyleColour e sTYLE_BRACELIGHT     CN.braceGood CN.white
    setStyleColour e sTYLE_BRACEBAD       CN.braceBad  CN.white

    -- tabs and indents
    setTabWidth e 4
    setUseTabs e False
    setIndentationGuides e sC_IV_LOOKBOTH    
    setStyleColour e sTYLE_INDENTGUIDE CN.indents  CN.white

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
    markerSetFore e CN.breakPointMarker CN.red
    markerSetBack e CN.breakPointMarker CN.red
    markerDefine e CN.bookMarkMarker sC_MARK_BOOKMARK
    markerSetFore e CN.bookMarkMarker CN.blue
    markerSetBack e CN.bookMarkMarker CN.blue

    setMarginMask e 1 $ sC_MASK_FOLDERS
        .|. (bit CN.breakPointMarker)
        .|. (bit CN.bookMarkMarker)

    -- marker with no associated marging
    -- scintilla will set background colour of whole line, used for debugging
    markerDefine e CN.debugMarker sC_MARK_ARROW
    markerSetFore e CN.debugMarker CN.yellow
    markerSetBack e CN.debugMarker CN.yellow
    
    return ()
    
setLexer :: Editor -> Int -> IO ()
setLexer e s = SI.sciSendEditorIO (getHwnd e) sCI_SETLEXER s 0

setKeywords :: Editor -> Int -> [String] -> IO ()
setKeywords e set ks = do
    withCString 
        (concat $ punctuate " " ks) 
        (\cs -> SI.sciSendEditorSO (getHwnd e) sCI_SETKEYWORDS 0 cs)
    return ()

setAStyle :: Editor -> Int -> COLORREF -> COLORREF -> Int -> String -> IO ()
setAStyle e st fc bc sz fnt = do
    let h = getHwnd e
    SI.sciSendEditorIO h sCI_STYLESETFORE st (fromIntegral fc :: Int)
    SI.sciSendEditorIO h sCI_STYLESETBACK st (fromIntegral bc :: Int)
    SI.sciSendEditorIO h sCI_STYLESETSIZE st (fromIntegral sz :: Int)
    withCString fnt (\cs -> SI.sciSendEditorSO h sCI_STYLESETFONT st cs)
    return ()
    
setStyleColour :: Editor -> Int -> COLORREF -> COLORREF -> IO ()
setStyleColour e st fc bc = do
    let h = getHwnd e
    SI.sciSendEditorIO h sCI_STYLESETFORE st (fromIntegral fc :: Int)
    SI.sciSendEditorIO h sCI_STYLESETBACK st (fromIntegral bc :: Int)
    return ()

styleClearAll :: Editor -> IO ()
styleClearAll e = SI.sciSendEditorIO (getHwnd e) sCI_STYLECLEARALL 0 0
    
setSavePoint :: Editor -> IO ()
setSavePoint e = SI.sciSendEditorIO (getHwnd e) sCI_SETSAVEPOINT 0 0

setReadOnly :: Editor -> Bool -> IO ()
setReadOnly e b = SI.sciSendEditorIO (getHwnd e) sCI_SETREADONLY (fromBool b :: Int) 0
     
isClean :: Editor -> IO Bool
isClean e = do
    x <- SI.sciSendEditorII (getHwnd e) sCI_GETMODIFY  0 0
    return (x == 0)
  
close :: Editor -> IO ()
close e = SI.c_ScnDestroyEditor (getHwnd e)
 
----------------------------------------------
-- Undo and Redo 
----------------------------------------------

undo :: Editor -> IO ()
undo e = SI.sciSendEditorIO (getHwnd e) sCI_UNDO 0 0 
    
redo :: Editor -> IO ()
redo e = SI.sciSendEditorIO (getHwnd e) sCI_REDO 0 0 

canUndo :: Editor -> IO Bool
canUndo e = SI.sciSendEditorIB (getHwnd e) sCI_CANUNDO 0 0

canRedo :: Editor -> IO Bool
canRedo e = SI.sciSendEditorIB (getHwnd e) sCI_CANREDO 0 0

beginUndoAction :: Editor -> IO ()
beginUndoAction e = SI.sciSendEditorIO (getHwnd e) sCI_BEGINUNDOACTION 0 0 

endUndoAction :: Editor -> IO ()
endUndoAction e = SI.sciSendEditorIO (getHwnd e) sCI_ENDUNDOACTION 0 0 

setUndoCollection :: Editor -> Bool -> IO ()
setUndoCollection e b = SI.sciSendEditorIO (getHwnd e) sCI_SETUNDOCOLLECTION (fromBool b :: Int) 0 

getUndoCollection :: Editor -> IO Bool
getUndoCollection e = SI.sciSendEditorIB (getHwnd e) sCI_GETUNDOCOLLECTION 0 0
    
emptyUndoBuffer :: Editor -> IO ()
emptyUndoBuffer e = SI.sciSendEditorIO (getHwnd e) sCI_EMPTYUNDOBUFFER 0 0 

----------------------------------------------
-- Cut and Paste 
----------------------------------------------

cut :: Editor -> IO ()
cut e = SI.sciSendEditorIO (getHwnd e) sCI_CUT 0 0 

copy :: Editor -> IO ()
copy e = SI.sciSendEditorIO (getHwnd e) sCI_COPY 0 0 
  
paste :: Editor -> IO ()
paste e = SI.sciSendEditorIO (getHwnd e) sCI_PASTE 0 0 

clear :: Editor -> IO ()
clear e = SI.sciSendEditorIO (getHwnd e) sCI_CLEAR 0 0 

canPaste :: Editor -> IO Bool
canPaste e =  SI.sciSendEditorIB (getHwnd e) sCI_CANPASTE 0 0
    
----------------------------------------------
-- Selection 
----------------------------------------------
    
selectionIsEmpty :: Editor -> IO Bool
selectionIsEmpty e = do
    b <- SI.sciSendEditorII (getHwnd e) sCI_GETSELTEXT  0 0
    return (b == 1)
  
selectAll :: Editor -> IO ()
selectAll e = SI.sciSendEditorIO (getHwnd e) sCI_SELECTALL 0 0 

{-
    sC_SEL_STREAM
    sC_SEL_RECTANGLE
    sC_SEL_LINES,
    sC_SEL_THIN,
-}      
setSelectionMode :: Editor -> Int -> IO ()
setSelectionMode e m = SI.sciSendEditorIO (getHwnd e) sCI_SETSELECTIONMODE m 0 

getSelText :: Editor -> IO String
getSelText e = do
    len <- SI.sciSendEditorII (getHwnd e) sCI_GETSELTEXT 0 0
    if len > 1 then do
        let bs = (BS.replicate len 0)   -- allocate buffer
        BS.unsafeUseAsCString bs (\cs -> do SI.sciSendEditorSO (getHwnd e) sCI_GETSELTEXT 0 cs)   
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
setSelectionStart e p = SI.sciSendEditorIO (getHwnd e) sCI_SETSELECTIONSTART p 0 

setSelectionEnd :: Editor -> Int -> IO ()
setSelectionEnd e p = SI.sciSendEditorIO (getHwnd e) sCI_SETSELECTIONEND p 0 

setSelectionRange :: Editor -> Int -> Int -> IO ()
setSelectionRange e p1 p2 = setSelectionStart e p1 >> setSelectionEnd e p2

getSelectionStart :: Editor -> IO Int
getSelectionStart e = SI.sciSendEditorII (getHwnd e) sCI_GETSELECTIONSTART 0 0

getSelectionEnd :: Editor -> IO Int
getSelectionEnd e = SI.sciSendEditorII (getHwnd e) sCI_GETSELECTIONEND 0 0

sortSelectedText :: Editor -> IO ()
sortSelectedText e = do
    -- returns length of string + 1 for terminating null
    len <- SI.sciSendEditorII (getHwnd e) sCI_GETSELTEXT 0 0
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
        len <- SI.sciSendEditorII (getHwnd e) sCI_GETSELTEXT  0 0 
        beginUndoAction e
        let bs = (BS.replicate len 0)   -- allocate buffer
        BS.unsafeUseAsCString bs (\cs -> do SI.sciSendEditorSO (getHwnd e) sCI_GETSELTEXT 0 cs)         
        let bs' = (BS.unlines . sort . BS.lines) (BS.init bs)
        BS.useAsCString bs' (\cs -> SI.sciSendEditorSO (getHwnd e) sCI_REPLACESEL 0 cs) 
        endUndoAction e
        setSelectionStart e (p1+1)
        setSelectionEnd e (p2+1)
        return ()
    else return ()

----------------------------------------------
-- Brace Highlighting 
----------------------------------------------
  
braceHighlight :: Editor -> Int -> Int -> IO ()
braceHighlight e pa pb = SI.sciSendEditorIO (getHwnd e) sCI_BRACEHIGHLIGHT pa pb 

braceBadLight :: Editor -> Int -> IO ()
braceBadLight e p = SI.sciSendEditorIO (getHwnd e) sCI_BRACEBADLIGHT p 0 

braceMatch :: Editor -> Int -> IO Int
braceMatch e p = SI.sciSendEditorII (getHwnd e) sCI_BRACEMATCH  p 0
    
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
getLineCount e = SI.sciSendEditorII (getHwnd e) sCI_GETLINECOUNT  0 0
   
getLinesOnScreen :: Editor -> IO Int
getLinesOnScreen e = SI.sciSendEditorII (getHwnd e) sCI_LINESONSCREEN  0 0
   
getCurrentPos :: Editor -> IO Int
getCurrentPos e = SI.sciSendEditorII (getHwnd e) sCI_GETCURRENTPOS  0 0

   
getPositionFromLine :: Editor -> Int -> IO Int
getPositionFromLine e l = SI.sciSendEditorII (getHwnd e) sCI_POSITIONFROMLINE  l 0

getLineFromPosition :: Editor -> Int -> IO Int
getLineFromPosition e p = SI.sciSendEditorII (getHwnd e) sCI_LINEFROMPOSITION  p 0

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
    SI.sciSendEditorIO (getHwnd e) sCI_GOTOLINE  l 0
    return ()

gotoPos :: Editor -> Int -> IO ()
gotoPos e p = do
    SI.sciSendEditorIO (getHwnd e) sCI_GOTOPOS  p 0
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
setFirstVisibleLine e l = SI.sciSendEditorIO (getHwnd e) sCI_SETFIRSTVISIBLELINE  l 0

getFirstVisibleLine :: Editor -> IO Int
getFirstVisibleLine e = SI.sciSendEditorII (getHwnd e) sCI_GETFIRSTVISIBLELINE  0 0

linesOnScreen :: Editor -> IO Int
linesOnScreen e = SI.sciSendEditorII (getHwnd e) sCI_LINESONSCREEN  0 0

-- selectLinesCols e 10 1 12 5, selects text from line 10 col 1 to line 12 col 5
selectLinesCols :: Editor -> Int -> Int -> Int -> Int -> IO ()
selectLinesCols e ls cs le ce = do
    ps <- getPositionFromLine e ls
    pe <- getPositionFromLine e le
    nl <- linesOnScreen e
    setSelectionRange e (ps+cs) (pe+ce)
    b <- isLineVisible e ls
    if b then return ()
    else setFirstVisibleLine e (ls-(nl `div` 2))

isLineVisible :: Editor -> Int -> IO Bool
isLineVisible e l = do
    fl <- getFirstVisibleLine e
    nl <- linesOnScreen e
    return (l >= fl && l <= (fl+nl-1))

----------------------------------------------
-- Tabs 
----------------------------------------------

setTabWidth :: Editor -> Int -> IO ()
setTabWidth e w = SI.sciSendEditorIO (getHwnd e) sCI_SETTABWIDTH  w 0

setUseTabs :: Editor -> Bool -> IO ()
setUseTabs e t = SI.sciSendEditorIO (getHwnd e) sCI_SETUSETABS  (fromBool t :: Int) 0

{-
    sC_IV_NONE
    sC_IV_REAL
    sC_IV_LOOKFORWARD,
    sC_IV_LOOKBOTH,
-}      
setIndentationGuides :: Editor -> Int -> IO ()
setIndentationGuides e w = SI.sciSendEditorIO (getHwnd e) sCI_SETINDENTATIONGUIDES  w 0
  
----------------------------------------------
-- Focus 
----------------------------------------------

setFocus :: Editor -> Bool -> IO ()
setFocus e b = do
    SI.sciSendEditorIO (getHwnd e) sCI_SETFOCUS (fromBool b :: Int) 0
    grabFocus e
    return ()

grabFocus :: Editor -> IO ()
grabFocus e = SI.sciSendEditorIO (getHwnd e) sCI_GRABFOCUS 0 0

getFocus :: Editor -> IO Bool
getFocus e = SI.sciSendEditorIB (getHwnd e) sCI_GETFOCUS 0 0

----------------------------------------------
-- Search and replace
----------------------------------------------

setTargetStart :: Editor -> Int -> IO ()
setTargetStart e t = SI.sciSendEditorIO (getHwnd e) sCI_SETTARGETSTART t 0

setTargetEnd :: Editor -> Int -> IO ()
setTargetEnd e t = SI.sciSendEditorIO (getHwnd e) sCI_SETTARGETEND  t 0 

setTargetRange :: Editor -> Int -> Int -> IO ()
setTargetRange e s f = SI.sciSendEditorIO (getHwnd e) sCI_SETTARGETRANGE  s f


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
setSearchFlags e f = SI.sciSendEditorIO (getHwnd e) sCI_SETSEARCHFLAGS  f 0

setTargetWholeDocument :: Editor -> IO ()
setTargetWholeDocument e = SI.sciSendEditorIO (getHwnd e) sCI_TARGETWHOLEDOCUMENT 0 0

searchInTarget :: Editor -> String -> IO Int
searchInTarget e s = withCStringLen s (\(cs, l) -> SI.sciSendEditorII (getHwnd e) sCI_SEARCHINTARGET l (ptrToInt cs)) 

findText :: Editor -> String -> Int -> Int -> Int -> IO Int
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
                    SI.sciSendEditorII (getHwnd e) sCI_FINDTEXT ops (ptrToInt ptr)
                )
        return pos)
    return p

searchNext :: Editor -> String -> Int -> IO Int
searchNext e text ops = 
    withCString text 
        (\ps -> SI.sciSendEditorII (getHwnd e) sCI_SEARCHNEXT ops (ptrToInt ps)) 
           

searchPrev :: Editor -> String -> Int -> IO Int
searchPrev e text ops = 
    withCString text 
        (\ps -> SI.sciSendEditorII (getHwnd e) sCI_SEARCHPREV ops (ptrToInt ps))
           

----------------------------------------------
-- Keyboard mapping
----------------------------------------------

-- see scintilla help online for key codes
clearCmdKey :: Editor -> Int -> IO ()
clearCmdKey e kc = SI.sciSendEditorIO (getHwnd e) sCI_CLEARCMDKEY kc 0

----------------------------------------------
-- Popup Menu
----------------------------------------------

{-
    Second argument is the popup selection:-

    sC_POPUP_NEVER,
    sC_POPUP_ALL,
    sC_POPUP_TEXT,
-}
usePopup :: Editor -> Int -> IO ()
usePopup e p = SI.sciSendEditorIO (getHwnd e) sCI_USEPOPUP  p 0

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
setMargins e m = SI.sciSendEditorIO (getHwnd e) sCI_SETMARGINS m 0

getMargins :: Editor -> IO Int
getMargins e = SI.sciSendEditorII (getHwnd e) sCI_GETMARGINS  0 0

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

setMarginType :: Editor -> Int -> Int -> IO ()
setMarginType e m t = SI.sciSendEditorIO (getHwnd e) sCI_SETMARGINTYPEN m t

getMarginType :: Editor -> Int -> IO Int
getMarginType e m = SI.sciSendEditorII (getHwnd e) sCI_GETMARGINTYPEN m 0

setMarginWidth :: Editor -> Int -> Int -> IO ()
setMarginWidth e m t = SI.sciSendEditorIO (getHwnd e) sCI_SETMARGINWIDTHN m t

getMarginWidth :: Editor -> Int -> IO Int
getMarginWidth e m = SI.sciSendEditorII (getHwnd e) sCI_GETMARGINWIDTHN m 0

setMarginMask :: Editor -> Int -> Int -> IO ()
setMarginMask e m b = SI.sciSendEditorIO (getHwnd e) sCI_SETMARGINMASKN m b

getMarginMask :: Editor -> Int -> IO Int
getMarginMask e m = SI.sciSendEditorII (getHwnd e) sCI_GETMARGINMASKN m 0

setMarginSensitive :: Editor -> Int -> Bool -> IO ()
setMarginSensitive e m b = SI.sciSendEditorIO (getHwnd e) sCI_SETMARGINSENSITIVEN m (fromBool b :: Int)

getMarginSensitive :: Editor -> Int -> IO Bool
getMarginSensitive e m = SI.sciSendEditorIB (getHwnd e) sCI_GETMARGINSENSITIVEN m 0

marginSetText :: Editor -> Int -> String -> IO ()
marginSetText e l s = withCString s (\cs -> SI.sciSendEditorSO (getHwnd e) sCI_MARGINSETTEXT l cs)

marginSetStyle :: Editor -> Int -> Int -> IO ()
marginSetStyle e l s = SI.sciSendEditorIO (getHwnd e) sCI_MARGINSETSTYLE l s

setMarginLeft :: Editor -> Int -> IO Int
setMarginLeft e m = SI.sciSendEditorII (getHwnd e) sCI_SETMARGINLEFT 0 m

setMarginRight :: Editor -> Int -> IO Int
setMarginRight e m = SI.sciSendEditorII (getHwnd e) sCI_SETMARGINRIGHT 0 m

----------------------------------------------
-- Markers
----------------------------------------------

markerDefine :: Editor -> Int -> Int -> IO ()
markerDefine e m b = SI.sciSendEditorIO (getHwnd e) sCI_MARKERDEFINE m b

markerSetFore :: Editor -> Int -> COLORREF -> IO ()
markerSetFore e m c = SI.sciSendEditorIO (getHwnd e) sCI_MARKERSETFORE m (fromIntegral c :: Int)

markerSetBack :: Editor -> Int -> COLORREF -> IO ()
markerSetBack e m c = SI.sciSendEditorIO (getHwnd e) sCI_MARKERSETBACK m (fromIntegral c :: Int)

markerAdd :: Editor -> Int -> Int -> IO Int
markerAdd e m n = SI.sciSendEditorII (getHwnd e) sCI_MARKERADD m n

markerGet :: Editor -> Int -> IO Int
markerGet e l = SI.sciSendEditorII (getHwnd e) sCI_MARKERGET l 0
 
markerDeleteHandle :: Editor -> Int -> IO ()
markerDeleteHandle e h = SI.sciSendEditorIO (getHwnd e) sCI_MARKERDELETEHANDLE h 0

markerDelete :: Editor -> Int -> Int -> IO ()
markerDelete e l m = SI.sciSendEditorIO (getHwnd e) sCI_MARKERDELETE l m

markerDeleteAll :: Editor -> Int -> IO ()
markerDeleteAll e m = SI.sciSendEditorIO (getHwnd e) sCI_MARKERDELETEALL m 0

markerLineFromHandle :: Editor -> Int -> IO Int
markerLineFromHandle e h = SI.sciSendEditorII (getHwnd e) sCI_MARKERLINEFROMHANDLE h 0

----------------------------------------------
-- Other settings
----------------------------------------------

setBufferedDraw :: Editor -> Bool -> IO ()
setBufferedDraw e b = SI.sciSendEditorIO (getHwnd e) sCI_SETBUFFEREDDRAW (fromBool b :: Int) 0

getBufferedDraw :: Editor -> IO Bool
getBufferedDraw e = SI.sciSendEditorIB (getHwnd e) sCI_GETBUFFEREDDRAW 0 0



