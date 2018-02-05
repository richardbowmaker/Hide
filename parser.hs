
module Main where
import Graphics.UI.WX
import Graphics.UI.WXCore
import Text.ParserCombinators.Parsec
import qualified Text.Parsec.Prim as P
import System.IO 
import Control.Concurrent.STM.TChan
import Control.Concurrent 
import Control.Concurrent.STM
import qualified Control.Concurrent.Thread as Thread
import Control.Monad (liftM)
import Control.Monad.Loops
import qualified Data.ByteString.Char8 as BS (ByteString, hGetLine, readFile, pack, putStrLn, writeFile)
import qualified Data.ByteString as BS (append)
import Data.List
import GHC.IO.Handle





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
    
    h <- openFile "Errors1.txt" ReadMode

    chn <- atomically $ newTChan
    chn' <- atomically $ dupTChan chn

    -- stream file to channels in separate thread
    (_, wait) <- Thread.forkIO $ streamToChan h chn 
  
    -- parse same input on both channels
    (_, wait1) <- Thread.forkIO $ parseInThread chn
    (_, wait2) <- Thread.forkIO $ parseInThread chn'

    -- wait for threads to complete
    es1 <- Thread.result =<< wait1
    es2 <- Thread.result =<< wait2
    Thread.result =<< wait

    -- close file
    putStrLn "*** Close ***"
    hClose h

    -- display results
    putStrLn $ concat $ map (\ce -> (compErrorToString ce) ++ "\n") es1      
    putStrLn $ concat $ map (\ce -> (compErrorToString ce) ++ "\n") es2      
 
    return ()

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


data CompError = CompError { filename :: String, srcLine :: Int, srcCol :: Int, errLine :: Int, errTop :: String, errLines :: [String] } deriving (Show)

compErrorToString :: CompError -> String
compErrorToString c =
    "Filename: " ++ (show $ filename c) ++ " (" ++ (show $ srcLine c) ++ "," ++ (show $ srcCol c) ++ ") errout = " ++ (show $ errLine c) ++ "\n" ++
        " " ++ (errTop c) ++ "\n" ++
        (concat $ map (\s -> " " ++ s ++ "\n") (errLines c))
    
    

parseErrorFile :: String -> IO [CompError]
parseErrorFile fn = do
    h <- openFile fn ReadMode 
    s <- hGetContents h
    case (parse errorFile "" s) of
        Left _ -> do
            hClose h
            return [(CompError "" 0 0 0 "" [])]
        Right es -> do
            hClose h
            return es

errorFile :: GenParser Char () [CompError]
errorFile = do
    string "Compile started ...\n\n"
    errs <- many anErrorOrEof
    return (init errs)

anErrorOrEof :: GenParser Char () CompError
anErrorOrEof = do
        err <- try (anError) 
        return err
    <|> do 
        many1 anyChar
        return (CompError "" 0 0 0 "" [])

anError :: GenParser Char () CompError
anError = do
    pos <- getPosition
    (fn, el, ec) <- fileName
    (e1, els) <- errorBlock
    return (CompError fn el ec (sourceLine pos)e1 els)

fileName :: GenParser Char () (String, Int, Int)
fileName = do
    drive <- many (noneOf ":")
    char ':'
    path <- many (noneOf ":")
    char ':'
    line <- many (noneOf ":")
    char ':'
    col <- many (noneOf ":")
    string ": error:"
    string eol
    return (drive ++ ":" ++ path, read line, read col)

errorBlock :: GenParser Char () (String, [String])
errorBlock = do
    header <- errorStart
    lines <- many errorLine
    string eol
    return (header, lines)

errorLine :: GenParser Char () String
errorLine = do
    string "    "
    eline <- many (noneOf eol)
    string eol
    return eline
    
errorStart :: GenParser Char () String
errorStart = do
    string "    * "
    line <- many (noneOf eol)
    string eol
    return line

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




