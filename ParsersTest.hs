
module ParsersTest
{-
(
    parseModuleHeader
) 
-}
where

import Text.ParserCombinators.Parsec
import Text.Parsec.Prim (modifyState)
import Text.ParserCombinators.ReadP (get)
-- import Language.Haskell.Exts.Parser
import System.IO
import Data.List
import Data.Maybe

-- import qualified Session as SS

------------------------------------------
-- source file module header parser
------------------------------------------

myReadFile :: IO String
myReadFile = do
    h <- openFile "D:\\_Rick's\\haskell\\starter.hs" ReadMode
    s <- hGetContents h
--    hClose h
    return s
   

noGo :: String
noGo = "hello"

{-

module Parsers
(
  errorFile
) where 

module Main where

offset :: String -> SourcePos -> Maybe Location
offset source pos = elemIndex pos positions
  where positions = scanl updatePosChar firstPos source
        firstPos = initialPos (sourceName pos)

https://stackoverflow.com/questions/10473857/parsec-positions-as-offsets


-}
{-
-----------------------------------------
-- parsing debugger output

Stopped in Main.mainGUI, D:\_Rick's\haskell\simple.hs:22:11-38
_result :: IO (Button ()) = _
mf :: Frame () = Graphics.UI.WXCore.WxcObject.Object
                   0x000000000836e700
[D:\_Rick's\haskell\simple.hs:22:11-38] *Main>

Stopped in Main.qsort.(...), D:\_Rick's\haskell\sort.hs:5:24-56
_result :: ([Integer], [Integer]) = _
a :: Integer = 8
as :: [Integer] = [4,0,3,1,23,....]
[D:\_Rick's\haskell\sort.hs:5:24-56] *Main>

Stopped in Main.mainGUI, D:\_Rick's\haskell\simple.hs:(9,11)-(36,13)
_result :: IO () = _
[D:\_Rick's\haskell\simple.hs:(9,11)-(36,13)] *Main> 

Stopped in Main.mainGUI, simple.hs:22:11-38
_result :: IO (Button ()) = _
mf :: Frame () = Graphics.UI.WXCore.WxcObject.Object
                   0x0000000015eded30
[simple.hs:22:11-38] *Main>


-----------------------------------------
-}

data DebuggerOutput = DebuggerOutput 
    { 
        doModule    :: String, 
        doFunction  :: String, 
        doFilePath  :: String,
        doRange     :: DebuggerRange,
        doResults   :: [DebuggerValue]
    }

data DebuggerRange = DebuggerRange
    {
        doLineS :: Int,
        doLineE :: Int,
        doColS  :: Int,
        doColE  :: Int
    }

data DebuggerValue = DebuggerValue 
    {
        doVariable  :: String,
        doType      :: String,
        doValue     :: String 
    }

instance Show DebuggerOutput where
    show (DebuggerOutput mod fn fp dr drs) = 
        "DebuggerOutput: module = " ++ mod ++ 
        ", Function = " ++ fn ++
        ", Filepath = " ++ fp ++
        "\n  Range = " ++ show dr ++ 
        (concat $ map (\dr -> "\n" ++ show dr) drs)

instance Show DebuggerValue where
    show (DebuggerValue var ty val) = 
        "DebuggerValue: Variable = " ++ var ++ 
        ", Type = " ++ ty ++
        ", Value = " ++ val 

instance Show DebuggerRange where
    show (DebuggerRange ls le cs ce) = 
        "DebuggerRange: Lines = " ++ show ls ++ " - " ++ show le ++
        ", Columns = " ++ show cs ++ " - " ++ show ce 

parseDebuggerOutput :: String -> Maybe DebuggerOutput
parseDebuggerOutput s = 
    case parse debuggerOutput "" s of
        Left _ -> Nothing
        Right r -> (Just r)

parseDebuggerValue :: String -> Maybe DebuggerValue
parseDebuggerValue s = 
    case parse debuggerValue "" s of
        Left _ -> Nothing
        Right r -> r

debuggerOutput :: GenParser Char () DebuggerOutput
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
    return (DebuggerOutput mod fn fp dr (catMaybes mdrs))

filePath :: GenParser Char () String
filePath = do
    d <- letter
    char ':'
    fp <- many1 (noneOf ":")
    return $ [d] ++ ":" ++ fp

fileName :: GenParser Char () String
fileName = many1 (noneOf ":")

-- (9,11)-(36,13)
lineRange :: GenParser Char () DebuggerRange
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
    return (DebuggerRange (read ls) (read le) (read cs) (read ce))

-- 22:11-38  
columnRange :: GenParser Char () DebuggerRange
columnRange = do
    ls <- many1 digit
    char ':'
    cs <- many1 digit
    char '-'
    ce <- many1 digit
    return (DebuggerRange (read ls) (read ls) (read cs) (read ce))
    
-- _result :: ([Integer], [Integer]) = _
debuggerValue :: GenParser Char () (Maybe DebuggerValue)
debuggerValue = do
    try ( do
            char '\n'
            var <- many1 (noneOf " ")
            string " :: "
            ty <- many1 (noneOf "=")
            string "= "
            val <- many1 (noneOf "\n")
            return $ Just (DebuggerValue var (init ty) val))
        <|> 
            (many1 anyChar >> return Nothing)
    
-----------------------------------------
-- parsing compiler output
-----------------------------------------

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
    commentLine
    pos <- getPosition
    return (sourceLine pos,False)

commentLine :: GenParser Char () (Int, Int)
commentLine = do
    manyTill anyChar (try $ string "--")
    p1 <- getPosition
    many (noneOf "\n")
    p2 <- getPosition
    return (0, 0)

commentBlock :: GenParser Char () ()
commentBlock = do
    many (char ' ')
    string "{-"
    manyTill anyChar (try $ string "-}")
    return ()
    where 




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

