module Main where

import Control.Concurrent
import Data.Char
import Data.List
import Graphics.UI.WX
import Graphics.UI.WXCore
import qualified Scintilla as SC

main = start mainGUI

mainGUI :: IO ()
mainGUI = do 
  
    -- main window
    mf <- frame []    
    set mf [text := "Starter", size := (Size 1300 800)] 

    p1 <- panel mf [bgcolor := red] 
    p2 <- panel mf [bgcolor := blue] 

    
    set mf [layout:= row 5 [fill $ widget p1, fill $ widget p2]]
    set mf [size := (Size 1300 800)]
         
     -- create statusbar field
    sf <- statusField []

    -- set the statusbar and menubar
    set mf [statusBar := [sf]]

    hwnd <- windowGetHandle p1
    scn <- SC.createEditor hwnd
{-
    -- AUI in pane 2
    auiMgr <- auiManagerCreate p2 wxAUI_MGR_DEFAULT

    nb <- auiNotebookCreate mf idAny (Point 0 0) (Size 0 0) (wxCLIP_CHILDREN + wxAUI_NB_TOP + wxAUI_NB_CLOSE_ON_ACTIVE_TAB)

    api <- auiPaneInfoCreateDefault
    auiPaneInfoCaption api "Notebook"
    auiPaneInfoCentre api
    auiPaneInfoLayer api 1
    auiPaneInfoPosition api 1
    auiPaneInfoCloseButton api True
    auiPaneInfoMaximizeButton api True

    auiManagerAddPaneByPaneInfo auiMgr nb api

    auiManagerUpdate auiMgr
-}

    -- create a timer that updates the display
    t <- timer mf [interval := 100, on command := onTimer] 

    set sf [text:= "my status"]
       
    return ()

onTimer = return ()


f1 = Just "123"
eos s = True
eos1 s = False

-- doTimeout attempts delay gets eos, returns nothing if timedout
accumulateStringTimed :: Int -> Int -> (Maybe String) -> (String -> Bool) -> IO (Maybe String)
accumulateStringTimed n delay gets eos = accumulateStringTimed' "" n (delay * 1000) gets eos

accumulateStringTimed' :: String -> Int -> Int -> (Maybe String) -> (String -> Bool) -> IO (Maybe String)
accumulateStringTimed' _ 0 _ _ _ = return Nothing
accumulateStringTimed' str n delay gets eos = do
    case gets of
        Just s  -> do 
            let str' = str ++ s
            if eos str' then return $ Just str'
            else repeat str' 
        Nothing -> repeat str
            
    where repeat s = threadDelay delay >> accumulateStringTimed' s (n-1) delay gets eos

   
-- stringEndsWith str end returns true if str ends with end
stringEndsWith :: String -> String -> Bool
stringEndsWith [] _ = False
stringEndsWith _ [] = False
stringEndsWith s e = take (length e) (reverse s) == reverse e

scanInt :: String -> Maybe Int
scanInt s = 
    case findIndex isDigit s of
        Just ix -> Just $ readInt s ix 0
        Nothing -> Nothing
        
readInt :: String -> Int -> Int -> Int
readInt s ix n 
    | ix >= length s = n
    | (not . isDigit) c = n
    | otherwise = readInt s (ix+1) (n*10 + read [c])
    where c = s !! ix



