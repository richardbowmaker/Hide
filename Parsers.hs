
module Parsers
(
    errorFile,
    parseDebuggerOutput,
    parseModuleHeader
) where

import Data.Maybe (catMaybes)
import Text.ParserCombinators.Parsec
import Text.Parsec.Prim (modifyState)



import qualified Session as SS

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
    (fn, el, ec) <- fileNamePos
    els <- errorDesc
    errn <- getState
    modifyState (+1)
    return $ SS.crCreateCompError errn fn el ec (sourceLine pos) els

fileDrive :: GenParser Char Int String
fileDrive = do
    c <- anyChar
    char ':'
    return $ c:":"

fileNamePos :: GenParser Char Int (String, Int, Int)
fileNamePos = do    
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

------------------------------------------
-- debugger output parser
------------------------------------------

parseDebuggerOutput :: String -> Maybe SS.DebuggerOutput
parseDebuggerOutput s = 
    case parse debuggerOutput "" s of
        Left _ -> Nothing
        Right r -> (Just r)

debuggerOutput :: GenParser Char () SS.DebuggerOutput
debuggerOutput = do
    manyTill anyChar $ string "Stopped in "
    mod <- many1 (noneOf ".")
    char '.'
    fn  <- many1 (noneOf ",")
    string ", "
    fp <- try filePath <|> fileName
    char ':'
    dr <- try lineRange <|> columnRange
    mdrs <- many1 debuggerValue
    return $ SS.createDebuggerOutput mod fn fp dr (catMaybes mdrs)

filePath :: GenParser Char () String
filePath = do
    d <- letter
    char ':'
    fp <- many1 (noneOf ":")
    return $ [d] ++ ":" ++ fp

fileName :: GenParser Char () String
fileName = many1 (noneOf ":")

-- (9,11)-(36,13)
lineRange :: GenParser Char () SS.DebuggerRange
lineRange = do
    char '('
    ls <- many1 digit
    char ','
    le <- many1 digit
    string ")-("
    cs <- many1 digit
    char ','
    ce <- many1 digit
    char ')'
    return $ SS.createDebuggerRange (read ls) (read le) (read cs) (read ce)

-- 22:11-38  
columnRange :: GenParser Char () SS.DebuggerRange
columnRange = do
    ls <- many1 digit
    char ':'
    cs <- many1 digit
    char '-'
    ce <- many1 digit
    return $ SS.createDebuggerRange (read ls) (read ls) (read cs) (read ce)
    
-- _result :: ([Integer], [Integer]) = _
debuggerValue :: GenParser Char () (Maybe SS.DebuggerValue)
debuggerValue = do
    try ( do
            string "\r\n"
            var <- many1 (noneOf " ")
            string " :: "
            ty <- many1 (noneOf "=")
            string "= "
            val <- many1 (noneOf "\r")
            return $ Just $ SS.createDebuggerValue var (init ty) val)
        <|> 
            (many1 anyChar >> return Nothing)
    
------------------------------------------
-- source file module header parser
------------------------------------------

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





