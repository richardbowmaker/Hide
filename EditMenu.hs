module EditMenu
( 
    findTextInRange,
    editFindBackward,
    editFindForward,
    editFind
) where 
    
import Control.Concurrent 
import Control.Concurrent.STM
import qualified Data.ByteString.Char8 as BS (ByteString, hGetLine, readFile, pack, putStrLn, writeFile)
import qualified Data.ByteString as BS (append)
import Data.List (find, findIndex)
import Data.Word (Word64)
import Foreign.C.String (CString, withCString)
import Graphics.Win32.GDI.Types (HWND)
import Graphics.UI.WX
import Graphics.UI.WXCore
import System.FilePath.Windows (takeFileName)
import System.IO

-- project imports

import qualified Constants as CN
import qualified EditorNotebook as EN
import qualified Scintilla as SC
import qualified Session as SS

editFind :: SS.Session -> SS.TextWindow -> SC.Editor -> IO ()
editFind ss tw scn = do
    sel <- SC.getSelText scn
    s <- textDialog (SS.ssFrame ss) "Find:" CN.programTitle sel
    if s /= "" then do
        pos <- SC.getCurrentPos scn
        len <- SC.getTextLen scn
        p <- findTextInRange scn s pos (pos, len) (0, (pos + (length s) -1))
        if p >= 0 then do
            SS.ssDebugInfo ss $ "Found at: " ++ (show p)
            -- save find string for next and prev
            atomically $ writeTVar (SS.ssFindText ss) (SS.ftFindText s p pos)
            return ()
        else do
            infoDialog (SS.ssFrame ss) CN.programTitle "Not found" 
            return ()
    else return ()

editFindForward :: SS.Session -> SS.TextWindow -> SC.Editor -> IO ()
editFindForward ss tw scn = do
    ft <- atomically $ readTVar (SS.ssFindText ss)
    let s = (SS.ftText ft)
    if (s /= "") then do
        -- set search range from current pos to end of doc
        pos <- SC.getCurrentPos scn
        len <- SC.getTextLen scn
        p <- findTextInRange scn s pos ((SS.ftStartPos ft), len) (0, ((SS.ftStartPos ft) + (length s) -1))
        if p >= 0 then do
            SS.ssDebugInfo ss $ "Found at: " ++ (show p)
            -- save find string for next and prev
            atomically $ writeTVar (SS.ssFindText ss) (SS.ftFindText s p (SS.ftStartPos ft))
            return ()
        else do
            infoDialog (SS.ssFrame ss) CN.programTitle "No more ocurrences found" 
            atomically $ writeTVar (SS.ssFindText ss) (SS.ftFindText s (pos+1) (pos+1))
            return ()
    else return ()

editFindBackward :: SS.Session -> SS.TextWindow -> SC.Editor -> IO ()
editFindBackward ss tw scn = do
    ft <- atomically $ readTVar (SS.ssFindText ss)
    let s = (SS.ftText ft)
    if (s /= "") then do
        -- set search range from current pos to end of doc
        pos <- SC.getCurrentPos scn
        len <- SC.getTextLen scn
        p <- findTextInRange scn s pos (((SS.ftStartPos ft) + (length s) -1), 0) ((len, SS.ftStartPos ft))
        if p >= 0 then do
            SS.ssDebugInfo ss $ "Found at: " ++ (show p)
            -- save find string for next and prev
            atomically $ writeTVar (SS.ssFindText ss) (SS.ftFindText s p (SS.ftStartPos ft))
            return ()
        else do
            infoDialog (SS.ssFrame ss) CN.programTitle "No more ocurrences found" 
            atomically $ writeTVar (SS.ssFindText ss) (SS.ftFindText s (pos+1) (pos+1))
            return ()
    else return () 
            
-- editor -> find text -> last found position -> Range 1 -> Range 2
findTextInRange :: SC.Editor -> String -> Int -> (Int, Int) -> (Int, Int) -> IO Int
findTextInRange e s pos r1 r2 = do 
    if (inRange pos r1) then do
        SC.setTargetRange e (pos+1) (snd r1)
        p <- SC.searchInTarget e s
        if p >= 0 then gotoPos e p          
        else do
            SC.setTargetRange e (fst r2) (snd r2)
            p <- SC.searchInTarget e s
            if p >= 0 then gotoPos e p    
            else return (-1)
    else if (inRange pos r2) then do
        SC.setTargetRange e (pos+1) (snd r2)
        p <- SC.searchInTarget e s
        if p >= 0 then gotoPos e p          
        else return (-1)
    else return (-1)
    
    where 
        inRange a (b,c) = (a >= (min b c)) && (a <= (max b c))
        gotoPos e p = SC.gotoPosWithScroll e p >> SC.grabFocus e 
                        >> SC.setSelectionRange e p (p+(length s)) >> return p
