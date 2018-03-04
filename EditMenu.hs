module EditMenu
( 
    findTextInRange,
    editFindBackward,
    editFindForward,
    editFind,
    updateEditMenus,
    updateEditMenu
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
import EditorNotebook
import Misc
import Scintilla
import ScintillaConstants
import Session

-- updates the enabled state of the Save, SaveAs and SaveAll menus                                
updateEditMenus :: Session -> IO ()   
updateEditMenus ss = do
 
    fs <- ssReadSourceFiles ss
        
    if (length fs > 0) then do
    
        sf <- enbGetSelectedSourceFile ss
        b <- scnSelectionIsEmpty $ sfEditor sf    
        -- ssDebugInfo ss $ "updateEditMenus: " ++ (show b) ++ " " ++ (sfToString sf)
        
        b <- scnCanUndo $ sfEditor sf
        set (ssMenuListGet ss CN.menuEditUndo)           [enabled := b]
        b <- scnCanRedo $ sfEditor sf
        set (ssMenuListGet ss CN.menuEditRedo)           [enabled := b]
        
        b <- scnSelectionIsEmpty $ sfEditor sf        
        set (ssMenuListGet ss CN.menuEditCut)            [enabled := not b]
        set (ssMenuListGet ss CN.menuEditCopy)           [enabled := not b]        
        b <- scnCanPaste $ sfEditor sf        
        set (ssMenuListGet ss CN.menuEditPaste)          [enabled := b]
        set (ssMenuListGet ss CN.menuEditSelectAll)      [enabled := True]
        set (ssMenuListGet ss CN.menuEditFind)           [enabled := True]
        set (ssMenuListGet ss CN.menuEditFindForward)    [enabled := True]
        set (ssMenuListGet ss CN.menuEditFindBackward)   [enabled := True]
        return ()
        
    else do
    
        ssDebugError ss "updateEditMenus: no file"
        set (ssMenuListGet ss CN.menuEditUndo)           [enabled := False]
        set (ssMenuListGet ss CN.menuEditRedo)           [enabled := False]
        set (ssMenuListGet ss CN.menuEditCut)            [enabled := False]
        set (ssMenuListGet ss CN.menuEditCopy)           [enabled := False]
        set (ssMenuListGet ss CN.menuEditPaste)          [enabled := False]
        set (ssMenuListGet ss CN.menuEditSelectAll)      [enabled := False]
        set (ssMenuListGet ss CN.menuEditFind)           [enabled := False]
        set (ssMenuListGet ss CN.menuEditFindForward)    [enabled := False]
        set (ssMenuListGet ss CN.menuEditFindBackward)   [enabled := False]
        return ()

updateEditMenu :: Session -> TextWindow -> IO ()
updateEditMenu ss tw = do

        f <- twHasFocus tw 

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

        where   setm :: Session -> TextWindow -> Int -> IO ()
                setm ss tw mid = setm' ss mid (twGetMenuEnabled tw mid) (twGetMenuFunction tw mid)
 
                setm' :: Session -> Int -> IO Bool -> IO () -> IO ()
                setm' ss mid me mf = do 
                    e <- me
                    set (ssMenuListGet ss mid) [on command := mf, enabled := e]


editFind :: Session -> IO ()
editFind ss = do

    sf <- enbGetSelectedSourceFile ss
    let e = sfEditor sf
    sel <- scnGetSelText e
    s <- textDialog (ssFrame ss) "Find:" CN.programTitle sel

    if s /= "" then do

        pos <- scnGetCurrentPos e
        len <- scnGetTextLen e

        p <- findTextInRange e s pos (pos, len) (0, (pos + (length s) -1))

        if p >= 0 then do

            ssDebugInfo ss $ "Found at: " ++ (show p)

            -- save find string for next and prev
            atomically $ writeTVar (ssFindText ss) (ftFindText s (filepath sf) p (filepath sf) pos)
            return ()

        else do

            infoDialog (ssFrame ss) CN.programTitle "Not found" 
            return ()

    else return ()

    where filepath sf =  maybe ("") id (sfFilePath sf)

editFindForward :: Session -> IO ()
editFindForward ss = do

    ft <- atomically $ readTVar (ssFindText ss)
    let s = (ftText ft)

    if (s /= "") then do

        sf <- enbGetSelectedSourceFile ss
        let e = sfEditor sf

        -- set search range from current pos to end of doc
        pos <- scnGetCurrentPos e
        len <- scnGetTextLen e

        p <- findTextInRange e s pos ((ftStartPos ft), len) (0, ((ftStartPos ft) + (length s) -1))

        if p >= 0 then do

            ssDebugInfo ss $ "Found at: " ++ (show p)

            -- save find string for next and prev
            atomically $ writeTVar (ssFindText ss) (ftFindText s (filepath sf) p (ftStartFile ft) (ftStartPos ft))
            return ()

        else do

            infoDialog (ssFrame ss) CN.programTitle "No more ocurrences found" 
            atomically $ writeTVar (ssFindText ss) (ftFindText s (ftStartFile ft) (pos+1) (ftStartFile ft) (pos+1))
            return ()

    else return ()

    where filepath sf =  maybe ("") id (sfFilePath sf)

editFindBackward :: Session -> IO ()
editFindBackward ss = do

    ft <- atomically $ readTVar (ssFindText ss)
    let s = (ftText ft)

    if (s /= "") then do

        sf <- enbGetSelectedSourceFile ss
        let e = sfEditor sf

        -- set search range from current pos to end of doc
        pos <- scnGetCurrentPos e
        len <- scnGetTextLen e

        p <- findTextInRange e s pos (((ftStartPos ft) + (length s) -1), 0) ((len, ftStartPos ft))

        if p >= 0 then do

            ssDebugInfo ss $ "Found at: " ++ (show p)

            -- save find string for next and prev
            atomically $ writeTVar (ssFindText ss) (ftFindText s (filepath sf) p (ftStartFile ft) (ftStartPos ft))
            return ()

        else do

            infoDialog (ssFrame ss) CN.programTitle "No more ocurrences found" 
            atomically $ writeTVar (ssFindText ss) (ftFindText s (ftStartFile ft) (pos+1) (ftStartFile ft) (pos+1))
            return ()

    else return ()

    where filepath sf =  maybe ("") id (sfFilePath sf)

-- editor -> find text -> last found position -> Range 1 -> Range 2
findTextInRange :: ScnEditor -> String -> Int -> (Int, Int) -> (Int, Int) -> IO Int
findTextInRange e s pos r1 r2 = do 

    if (inRange pos r1) then do

        scnSetTargetRange e (pos+1) (snd r1)
        p <- scnSearchInTarget e s

        if p >= 0 then gotoPos e p          
        else do

            scnSetTargetRange e (fst r2) (snd r2)
            p <- scnSearchInTarget e s

            if p >= 0 then gotoPos e p    
            else return (-1)

    else if (inRange pos r2) then do

        scnSetTargetRange e (pos+1) (snd r2)
        p <- scnSearchInTarget e s

        if p >= 0 then gotoPos e p          
        else return (-1)

    else return (-1)
    
    where 
        inRange a (b,c) = (a >= (min b c)) && (a <= (max b c))
        gotoPos e p = scnGotoPosWithScroll e p >> scnGrabFocus e >> scnSetSelectionRange e p (p+(length s)) >> return p
