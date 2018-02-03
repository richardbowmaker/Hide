
module Misc
(
    ptrToString,
    frameToString,
    windowToString,
    panelToString,
    ptrToWord64,
    findAndUpdate1,
    findAndUpdate2,
    comparePtrs,
    isSameWindow,
    findAndRemove,
    doWhileTrueIO
) where

import Foreign.Ptr (FunPtr, Ptr, minusPtr, nullPtr)
import Numeric (showHex)
import Graphics.UI.WXCore
import Data.Word (Word64)

ptrToString :: Ptr a -> String
ptrToString p = "0x0" ++ (showHex (minusPtr p nullPtr) "")
              
ptrToWord64 :: Ptr a -> Word64
ptrToWord64 p = fromIntegral  (minusPtr p nullPtr) :: Word64

panelToString :: Panel () -> IO String
panelToString p = do
    h <- windowGetHandle p
    return ("Window HWND: " ++ (showHex (minusPtr h nullPtr) ""))

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

doWhileTrueIO :: (a -> IO Bool) -> [a] -> IO Bool    
doWhileTrueIO _ [] = return (True)
doWhileTrueIO p (x:xs) = do
        b <- p x
        if b then doWhileTrueIO p xs
        else return (False)
        
        
        
