module Constants 
(
    lineMargin,
    symbolMargin,
    breakPointMarker,
    bookMarkMarker,
    debug,
    debugMarker,
    programTitle,
    rgb,
    black,
    red,
    blue,
    green,
    white,
    darkGreen,
    keyBlue,
    braceGood,
    braceBad,
    stringBrown,
    indents,
    yellow
)
where

import Data.Bits (shift, (.|.))
import Data.List (find)
import Data.Maybe (maybe)
import Graphics.Win32.GDI.Types (COLORREF)

debug = True

programTitle :: String
programTitle = "HIDE V.24-05-2018-2"

------------------------------------------------------------    
-- colors
------------------------------------------------------------    

rgb :: Int -> Int -> Int -> COLORREF
rgb r g b = fromIntegral ((shift b 16) .|. (shift g 8) .|. r) :: COLORREF

black :: COLORREF
black = (rgb 0 0 0)

red :: COLORREF
red = (rgb 255 0 0)

blue :: COLORREF
blue = (rgb 0 0 255)

green :: COLORREF
green = (rgb 0 255 0)

yellow :: COLORREF
yellow = (rgb 255 255 0)

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

------------------------------------------------------------    
-- scintilla margin constants
------------------------------------------------------------    

lineMargin :: Int
lineMargin = 0

symbolMargin :: Int 
symbolMargin = 1

breakPointMarker :: Int
breakPointMarker = 0

bookMarkMarker :: Int
bookMarkMarker = 1

debugMarker :: Int
debugMarker = 2
 