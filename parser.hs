
module Main where
import Graphics.UI.WX
import Graphics.UI.WXCore
import Text.ParserCombinators.Parsec
import qualified Text.Parsec.Prim as P 

main = start mainGUI

mainGUI :: IO ()
mainGUI = do 
  
    -- main window
    f <- frame []    
    
    p <- panel f []
     
    -- create statusbar field
    sf1 <- statusField [text := "SF1", statusWidth := 20]
    sf2 <- statusField [text := "SF2"]
    set f [statusBar := [sf1,sf2]]
    
    

    return ()


-- D:\_Rick's\haskell\HeyHo\Errors.hs:233:23: error:

data CompError = CompError { filename :: String, line :: Int, col :: Int, err1 :: String, errls :: [String] } deriving (Show)

parseErrors :: String -> Either ParseError [CompError]
parseErrors s = parse errorFile "" s

errorFile :: GenParser Char () [CompError]
errorFile = do
    errs <- many anError
    return errs

anError :: GenParser Char () CompError
anError = do
    (fn, el, ec) <- fileName
    (e1, els) <- errorBlock
    return (CompError fn el ec e1 els)

fileName :: GenParser Char () (String, Int, Int)
fileName = do
    drive <- many (noneOf ":")
    char ':'
    path <- many (noneOf ":")
    char ':'
    line <- many (noneOf ":")
    char ':'
    col <- many (noneOf ":")
    string ": error:\n"
    return (drive ++ ":" ++ path, read line, read col)

errorBlock :: GenParser Char () (String, [String])
errorBlock = do
    header <- errorStart
    lines <- many errorLine
    char ('\n')
    return (header, lines)

errorLine :: GenParser Char () String
errorLine = do
    string "    "
    eline <- many (noneOf "\n")
    char '\n'
    return eline
    
errorStart :: GenParser Char () String
errorStart = do
    string "    * "
    line <- many (noneOf "\n")
    char '\n'
    return line

{-

D:\_Rick's\haskell\HeyHo\Errors.hs:233:23: error:
    * Couldn't match type `Object
                             (CWxObject (CEvtHandler (CWindow (CTopLevelWindow (CFrame ())))))'
                     with `Session'
      Expected type: Session
        Actual type: Frame ()
    * In the first argument of `ssAuiMgr', namely `mf'
      In the expression: ssAuiMgr mf
      In an equation for `am': am = ssAuiMgr mf


-}



{-
parseErrors :: String -> Either ParseError String
parseErrors s = parse fileName "" s

fileName :: GenParser Char () String
fileName = do
    drive <- pDrive
    char ':'
    path <- pPath
    return (drive ++ " # " ++ path)

pDrive :: GenParser Char () String
pDrive = many (noneOf ":")

pPath :: GenParser Char () String
pPath = many (noneOf "\n")

-}

{-
parseErrors :: String -> Either ParseError [String]
parseErrors s = parse errorFile "" s

    
errorFile = endBy eline eol
eline = many (noneOf "\n")
eol = char '\n'

-}

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




