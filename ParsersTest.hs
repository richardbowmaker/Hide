
module ParsersTest
{-
(
    parseModuleHeader
) 
-}
where


import Text.ParserCombinators.Parsec
import Text.Parsec.Prim (modifyState)
-- import Text.ParserCombinators.ReadP (skipSpaces)

-- import qualified Session as SS

------------------------------------------
-- source file module header parser
------------------------------------------

{-

module Parsers
(
    errorFile
) where 

module Main where

-}

parseModuleHeader :: String -> (Maybe (Int, Bool))
parseModuleHeader s = 
    case parse moduleHeader "" s of
        Left _ -> Nothing
        Right r -> (Just r)

moduleHeader :: GenParser Char () (Int, Bool)
moduleHeader = do
--    many1 (noneOf "{")
--    string "module"
--    many commentLine
--    endBy (many (noneOf "m")) (string "module")
    commentBlock
    pos <- getPosition
    return (sourceLine pos,False)

commentLine :: GenParser Char () ()
commentLine = do
    many (char ' ')
    string "--"
    many (noneOf "\n")
    return ()

commentBlock :: GenParser Char () ()
commentBlock = do

    between (string "{-") (string "-}") (many anyChar)
    return ()

{-
------------------------------------------
-- compiler output parser
------------------------------------------

errorFile :: GenParser Char Int SS.CompReport
errorFile = do
    errs <- many (try fileError)
    optional linkLine
    return $ SS.crCreateCompReport Nothing errs

fileError :: GenParser Char Int SS.CompError
fileError = do
    many fileTitle
    string eol
    pos <- getPosition
    (fn, el, ec) <- fileName
    els <- errorDesc
    errn <- getState
    modifyState (+1)
    return $ SS.crCreateCompError errn fn el ec (sourceLine pos) els

fileDrive :: GenParser Char Int String
fileDrive = do
    c <- anyChar
    char ':'
    return $ c:":"

fileName :: GenParser Char Int (String, Int, Int)
fileName = do    
    drive <- (try fileDrive <|> return "")
    path <- many (noneOf ":")
    char ':'
    line <- many (noneOf ":")
    char ':'
    col <- many (noneOf ":")
    string ": error:"
    string eol
    return (drive ++ path, read line, read col)

fileTitle :: GenParser Char Int ()
fileTitle = do
    char '['
    many (noneOf eol)
    string eol
    return ()

errorDesc :: GenParser Char Int ([String])
errorDesc = do
    lines <- many errorLine
    return (lines)

errorLine :: GenParser Char Int String
errorLine = do
    string "    "
    eline <- many (noneOf eol)
    string eol
    return eline

linkLine :: GenParser Char Int ()
linkLine = do
    string "Linking"
    many (noneOf eol)
    return ()
    
eol :: String
eol = "\n"


-}

