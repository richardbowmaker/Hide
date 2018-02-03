
module Main where
import Graphics.UI.WX
import Graphics.UI.WXCore
import Text.ParserCombinators.Parsec
import qualified Text.Parsec.Prim as P 

main = start mainGUI

mainGUI :: IO ()
mainGUI =
  
    -- main window
    f <- frame []    
    
    p <- panel f []
     
    -- create statusbar field
    sf1 <- statusField [text := "SF1", statusWidth := 20]
    sf2 <- statusField [text := "SF2"]
    set f [statusBar := [sf1,sf2]]
    
    

    return ()
    
-- parseCells :: String -> Either ParseError [String]
-- parseCells s = parse cells "(unknown)" s

parseCells1 :: String -> Either ParseError ([String], Int)
parseCells1 s = P.runParser cells 0 "(unknown)" s

cells :: GenParser Char Int ([String], Int)
cells = do
    first <- cellContent
    P.modifyState (+1)
    (next, _) <- remainingCells
    count <- getState
    return ((first : next), count)

remainingCells :: GenParser Char Int ([String], Int)
remainingCells = 
    (char ',' >> cells)
    <|> (return ([], 0))

cellContent :: GenParser Char Int String
cellContent = many (noneOf ",\n")


{-

first parser with state

parseCells1 :: String -> Either ParseError ([String], Int)
parseCells1 s = P.runParser cells 0 "(unknown)" s

cells :: GenParser Char Int ([String], Int)
cells = do
    first <- cellContent
    P.modifyState (+1)
    (next, _) <- remainingCells
    count <- getState
    return ((first : next), count)

remainingCells :: GenParser Char Int ([String], Int)
remainingCells = 
    (char ',' >> cells)
    <|> (return ([], 0))

cellContent :: GenParser Char Int String
cellContent = many (noneOf ",\n")


-}




