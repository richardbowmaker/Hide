

module Parser
(
    doParse,
    CompError,
    compErrorToString,
    createCompError
) where


import Text.ParserCombinators.Parsec
import qualified Text.Parsec.Prim as P
import System.IO 
import System.Directory  
import Control.Concurrent 
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import qualified Control.Concurrent.Thread as Thread
import Control.Monad (liftM)
import Control.Monad.Loops
import qualified Data.ByteString.Char8 as BS (ByteString, hGetLine, readFile, pack, putStrLn, writeFile)
import qualified Data.ByteString as BS (append)
import Data.List
import GHC.IO.Handle


doParse :: String -> [CompError]
doParse s =
    case (parse errorFile "" s) of
        Left _ ->   []
        Right es -> es

parseInThread :: TChan String -> IO [CompError]
parseInThread chn = do
    s <- captureChannel chn "" 
    case (parse errorFile "" s) of
        Left _ -> return []
        Right es -> return es
    

streamToChan :: Handle -> TChan String -> IO ()
streamToChan h tot = do
    whileM_ (liftM not $ hIsEOF h) (hGetLine h >>= (\s -> atomically $ writeTChan tot (s++"\n")))
    atomically $ writeTChan tot "EOF"
    return ()

chanToStdOut :: TChan String -> IO ()
chanToStdOut chn = do
    b <- atomically $ isEmptyTChan chn 
    if b then chanToStdOut chn
    else do
        s <- atomically $ readTChan chn
        putStrLn s
        if s == "EOF" then return ()
        else chanToStdOut chn

handleToStdOut :: Handle -> String -> IO ()
handleToStdOut h p = do
    b <- hIsEOF h
    if b then do
        putStrLn (p++"-> EOF")
        return ()
    else do
        s <- hGetLine h
        putStrLn (p++s)
        handleToStdOut h p
        
captureChannel :: TChan String -> String -> IO String
captureChannel chn str = do
    b <- atomically $ isEmptyTChan chn 
    if b then captureChannel chn str
    else do
        s <- atomically $ readTChan chn
        if s == "EOF" then return str
        else captureChannel chn (str++s)


chanToString :: TChan String -> IO [String]
chanToString chn = do
    let s = ""
    let s1 = ""
    s <- whileM (return (not $ isInfixOf "EOF" s)) 
            (do 
                    s1 <- atomically $ readTChan chn
                    putStrLn s1
                    let s = s ++ s1
                    return s)
    return (s) 

data CompError = CompError { filename :: String, srcLine :: Int, srcCol :: Int, errIx :: Int, errLines :: [String] } deriving (Show)

createCompError fn el ec ln els = (CompError fn el ec ln els)

compErrorToString :: CompError -> String
compErrorToString c =
    "Filename: " ++ (show $ filename c) ++ " (" ++ (show $ srcLine c) ++ "," ++ (show $ srcCol c) ++ ") errout = " ++ (show $ errIx c) ++ "\n" ++
        (concat $ map (\s -> " " ++ s ++ "\n") (errLines c))
       

parseErrorFile :: String -> IO  (Maybe (Int, [CompError]))
parseErrorFile fn = do
    h <- openFile fn ReadMode 
    s <- hGetContents h
    case (parse errorFile "" s) of
        Left _ -> do
            hClose h
            return Nothing
        Right es -> do
            hClose h
            return $  Just (length es, es)

errorFile :: GenParser Char () [CompError]
errorFile = do
    errs <- many anError
    return errs

anError :: GenParser Char () CompError
anError = do
    string eol
    pos <- getPosition
    (fn, el, ec) <- fileName
    els <- errorDesc
    return (CompError fn el ec (sourceLine pos) els)

fileDrive :: GenParser Char () String
fileDrive = do
    c <- anyChar
    char ':'
    return $ c:":"

fileName :: GenParser Char () (String, Int, Int)
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

errorDesc :: GenParser Char () ([String])
errorDesc = do
    lines <- many errorLine
    return (lines)

errorLine :: GenParser Char () String
errorLine = do
    string "    "
    eline <- many (noneOf eol)
    string eol
    return eline
    
eol :: String
eol = "\n"

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




