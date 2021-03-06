
module Misc
(
    boolToInt,
    comparePtrs,
    createTree,
    doUntilFalseIO,
    doWhileTrueIO,
    findAndRemove,
    findAndRemoveIO,
    findAndUpdate1,
    findAndUpdate2,
    findIO,
    frameToString,
    hwndToString,
    isSameWindow,
    lastN,
    panelToString,
    ptrToInt,
    ptrToInt64,
    ptrToString,
    ptrToWord64,
    readInt,
    scanInt,
    stringEndsWith,
    stringStartsWith,
    windowToString,
    withCStrings
) where

import Control.Applicative (liftA2)
import Control.Monad (liftM2)
import Data.Char (isDigit)
import Data.Int (Int64)
import Data.List (findIndex)
import Data.Word (Word64)
import Foreign.C.String (CString, withCString)
import Foreign.Ptr (FunPtr, Ptr, minusPtr, nullPtr)
import Graphics.UI.WX 
import Graphics.UI.WXCore
import Graphics.Win32.GDI.Types (HWND)
import Numeric (showHex)

ptrToString :: Ptr a -> String
ptrToString p = "0x0" ++ (showHex (minusPtr p nullPtr) "")
              
ptrToWord64 :: Ptr a -> Word64
ptrToWord64 p = fromIntegral  (minusPtr p nullPtr) :: Word64

ptrToInt64 :: Ptr a -> Int64
ptrToInt64 p = fromIntegral  (minusPtr p nullPtr) :: Int64

ptrToInt :: Ptr a -> Int
ptrToInt p = fromIntegral  (minusPtr p nullPtr) :: Int

panelToString :: Panel () -> IO String
panelToString p = do
    h <- windowGetHandle p
    return $ "Window HWND: " ++ hwndToString h

hwndToString :: HWND -> String
hwndToString hwnd = showHex (minusPtr hwnd nullPtr) ""

frameToString :: Frame () -> IO String
frameToString f = do
    h <- windowGetHandle f
    t <- frameGetTitle f
    return ("Frame HWND: " ++ (showHex (minusPtr h nullPtr) "") ++ " Title: " ++ t)

windowToString :: Window () -> IO String
windowToString w = do
    h <- windowGetHandle w
    return ("Window HWND: " ++ (showHex (minusPtr h nullPtr) ""))
    
findAndUpdate1 :: (a -> Bool) -> [a] -> a -> [a]
findAndUpdate1 _ [] _ = []
findAndUpdate1 f (x:xs) x' = (if f x then x' else x) : findAndUpdate1 f xs x'

findAndUpdate2 :: (a -> Maybe a) -> [a] -> [a]
findAndUpdate2 _ [] = []
findAndUpdate2 f (x:xs) = 
    (case f x of
        Just x' -> x' 
        Nothing -> x) : findAndUpdate2 f xs
     
comparePtrs :: Ptr a -> Ptr a -> Bool
comparePtrs p1 p2 = (ptrToWord64 p1) == (ptrToWord64 p2)

isSameWindow :: Window () -> Window () -> IO Bool
isSameWindow w1 w2 = do
    h1 <- windowGetHandle w1
    h2 <- windowGetHandle w2
    return (comparePtrs h1 h2)


findAndRemove :: (a -> Bool) -> [a] -> [a]
findAndRemove _ [] = []
findAndRemove f (x:xs) = if f x then rest else x : rest
    where rest = findAndRemove f xs

findAndRemoveIO :: (a -> IO Bool) -> [a] -> IO [a]
findAndRemoveIO _ [] =  return []
findAndRemoveIO f (x:xs) = do
    b <- f x
    if b then rest else liftM2 (:) (return x) rest
    where rest = findAndRemoveIO f xs

doWhileTrueIO :: (a -> IO Bool) -> [a] -> IO Bool    
doWhileTrueIO _ [] = return (True)
doWhileTrueIO p (x:xs) = do
        b <- p x
        if b then doWhileTrueIO p xs
        else return False
 
findIO :: (a -> IO Bool) -> [a] -> IO (Maybe a)
findIO _ [] = return Nothing
findIO p (x:xs) = do
    b <- p x
    if b then return (Just x) else findIO p xs
     
boolToInt :: Bool -> Int
boolToInt b = if b then 1 else 0

-- stringStartsWith str end returns true if str ends with end
stringStartsWith :: String -> String -> Bool
stringStartsWith [] _ = False
stringStartsWith _ [] = False
stringStartsWith s e = take (length e) s == e

stringEndsWith :: String -> String -> Bool
stringEndsWith [] _ = False
stringEndsWith _ [] = False
stringEndsWith s e = take (length e) (reverse s) == reverse e

doUntilFalseIO :: [(IO Bool)] -> IO Bool
doUntilFalseIO [] = return True
doUntilFalseIO (f:fs) = f >>= (\b -> if b then doUntilFalseIO fs else return False)

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

withCStrings :: [String] -> ([CString] -> IO a) -> IO a
withCStrings = withCStrings' []
    where
        withCStrings' :: [CString] -> [String] -> ([CString] -> IO a) -> IO a
        withCStrings' cstrs [] fn = (fn . reverse) cstrs
        withCStrings' cstrs (str:strs) fn = withCString str (\cstr -> withCStrings' (cstr:cstrs) strs fn)

lastN :: [a] -> Int -> Maybe [a]
lastN _ 0 = Just []
lastN [] _ = Nothing
lastN xs n = liftA2 (++) (lastN (init xs) (n-1)) (Just $ [last xs])

------------------------------------------------------------    
-- Tree Control
------------------------------------------------------------    
    
createTree :: Frame () ->  IO (TreeCtrl ())
createTree f = do      
    tree <- treeCtrl f [size := (Size 100 100)] 
    root <- treeCtrlAddRoot tree "root" (-1) (-1) objectNull     
    _    <- treeCtrlAppendItem tree root "item1" (-1) (-1) objectNull
    _    <- treeCtrlAppendItem tree root "item2" (-1) (-1) objectNull
    _    <- treeCtrlAppendItem tree root "item3" (-1) (-1) objectNull
    treeCtrlExpand tree root
    cs <- treeCtrlGetChildren tree root
    return (tree)
    
    


