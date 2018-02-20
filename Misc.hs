
module Misc
(
    ptrToString,
    frameToString,
    windowToString,
    panelToString,
    ptrToWord64,
    ptrToInt64,
    findAndUpdate1,
    findAndUpdate2,
    comparePtrs,
    isSameWindow,
    findAndRemove,
    doWhileTrueIO,
    createGrid, 
    createTree   
) where

import Foreign.Ptr (FunPtr, Ptr, minusPtr, nullPtr)
import Numeric (showHex)
import Graphics.UI.WX
import Graphics.UI.WXCore
import Data.Word (Word64)
import Data.Int (Int64)

ptrToString :: Ptr a -> String
ptrToString p = "0x0" ++ (showHex (minusPtr p nullPtr) "")
              
ptrToWord64 :: Ptr a -> Word64
ptrToWord64 p = fromIntegral  (minusPtr p nullPtr) :: Word64

ptrToInt64 :: Ptr a -> Int64
ptrToInt64 p = fromIntegral  (minusPtr p nullPtr) :: Int64

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
    
------------------------------------------------------------    
-- Grid Control
------------------------------------------------------------    

createGrid :: Frame () -> IO (Grid ())
createGrid f = do
    -- grids
    g <- gridCtrl f []
    gridSetGridLineColour g (colorSystem Color3DFace)
    gridSetCellHighlightColour g black
    appendColumns g (head names)
    appendRows    g (map show [1..length (tail names)])
    mapM_ (setRow g) (zip [0..] (tail names))
    gridAutoSize g  
    return (g)
    
gridCtrl :: Window a -> [Prop (Grid ())] -> IO (Grid ())
gridCtrl parent_ props_
  = feed2 props_ 0 $
    initialWindow $ \id_ rect' -> \props' flags ->
    do g <- gridCreate parent_ id_ rect' flags
       gridCreateGrid g 0 0 0
       set g props'
       return g

appendColumns :: Grid a -> [String] -> IO ()
appendColumns _g []
  = return ()
appendColumns g labels
  = do n <- gridGetNumberCols g
       _ <- gridAppendCols g (length labels) True
       mapM_ (\(i, label_) -> gridSetColLabelValue g i label_) (zip [n..] labels)

appendRows :: Grid a -> [String] -> IO ()
appendRows _g []
  = return ()
appendRows g labels
  = do n <- gridGetNumberRows g
       _ <- gridAppendRows g (length labels) True
       mapM_ (\(i, label_) -> gridSetRowLabelValue g i label_) (zip [n..] labels)

setRow :: Grid a -> (Int, [String]) -> IO ()
setRow g (row_, values)
  = mapM_ (\(col,value_) -> gridSetCellValue g row_ col value_) (zip [0..] values)

names :: [[String]]
names
  = [["First Name", "Last Name"]
    ,["Daan","Leijen"],["Arjan","van IJzendoorn"]
    ,["Martijn","Schrage"],["Andres","Loh"]]
    
    


