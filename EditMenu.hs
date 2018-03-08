module EditMenu
( 
    findTextInRange,
    editFindBackward,
    editFindForward,
    editFind,
    updateEditMenus
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
import qualified ScintillaConstants as SC
import qualified Session as SS

updateEditMenus :: SS.Session -> SS.TextMenus -> IO ()
updateEditMenus ss tw = do

        f <- SS.twHasFocus tw 

        if (f) then do
            setm ss tw CN.menuEditUndo          
            setm ss tw CN.menuEditRedo          
            setm ss tw CN.menuEditCut           
            setm ss tw CN.menuEditCopy          
            setm ss tw CN.menuEditPaste         
            setm ss tw CN.menuEditSelectAll     
            setm ss tw CN.menuEditFind          
            setm ss tw CN.menuEditFindForward   
            setm ss tw CN.menuEditFindBackward  
        else do
            setm' ss CN.menuEditUndo          (return False) (return ())
            setm' ss CN.menuEditCut           (return False) (return ())
            setm' ss CN.menuEditCopy          (return False) (return ())
            setm' ss CN.menuEditPaste         (return False) (return ())
            setm' ss CN.menuEditSelectAll     (return False) (return ())
            setm' ss CN.menuEditFind          (return False) (return ())
            setm' ss CN.menuEditFindForward   (return False) (return ())
            setm' ss CN.menuEditFindBackward  (return False) (return ())

        where   setm :: SS.Session -> SS.TextMenus -> Int -> IO ()
                setm ss tw mid = setm' ss mid (SS.tmGetMenuEnabled tw mid) (SS.tmGetMenuFunction tw mid)
 
                setm' :: SS.Session -> Int -> IO Bool -> IO () -> IO ()
                setm' ss mid me mf = do 
                    e <- me
                    set (SS.ssMenuListGet ss mid) [on command := mf, enabled := e]

editFind :: SS.Session -> SS.TextWindow -> SC.ScnEditor -> IO ()
editFind ss tw scn = do
    sel <- SC.scnGetSelText scn
    s <- textDialog (SS.ssFrame ss) "Find:" CN.programTitle sel
    if s /= "" then do
        pos <- SC.scnGetCurrentPos scn
        len <- SC.scnGetTextLen scn
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

editFindForward :: SS.Session -> SS.TextWindow -> SC.ScnEditor -> IO ()
editFindForward ss tw scn = do
    ft <- atomically $ readTVar (SS.ssFindText ss)
    let s = (SS.ftText ft)
    if (s /= "") then do
        -- set search range from current pos to end of doc
        pos <- SC.scnGetCurrentPos scn
        len <- SC.scnGetTextLen scn
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

editFindBackward :: SS.Session -> SS.TextWindow -> SC.ScnEditor -> IO ()
editFindBackward ss tw scn = do
    ft <- atomically $ readTVar (SS.ssFindText ss)
    let s = (SS.ftText ft)
    if (s /= "") then do
        -- set search range from current pos to end of doc
        pos <- SC.scnGetCurrentPos scn
        len <- SC.scnGetTextLen scn
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
findTextInRange :: SC.ScnEditor -> String -> Int -> (Int, Int) -> (Int, Int) -> IO Int
findTextInRange e s pos r1 r2 = do 
    if (inRange pos r1) then do
        SC.scnSetTargetRange e (pos+1) (snd r1)
        p <- SC.scnSearchInTarget e s
        if p >= 0 then gotoPos e p          
        else do
            SC.scnSetTargetRange e (fst r2) (snd r2)
            p <- SC.scnSearchInTarget e s
            if p >= 0 then gotoPos e p    
            else return (-1)
    else if (inRange pos r2) then do
        SC.scnSetTargetRange e (pos+1) (snd r2)
        p <- SC.scnSearchInTarget e s
        if p >= 0 then gotoPos e p          
        else return (-1)
    else return (-1)
    
    where 
        inRange a (b,c) = (a >= (min b c)) && (a <= (max b c))
        gotoPos e p = SC.scnGotoPosWithScroll e p >> SC.scnGrabFocus e 
                        >> SC.scnSetSelectionRange e p (p+(length s)) >> return p
