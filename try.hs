import Control.Exception
import System.Directory  

main1 = catch (print $ 5 `div` 0) handler
  where
    handler :: SomeException -> IO ()
    handler ex = putStrLn $ "Caught exception: " ++ show ex


main2 = do
    result <- try (evaluate (5 `div` 0)) :: IO (Either SomeException Int)
    case result of
        Left ex  -> putStrLn $ "Caught exception: " ++ show ex
        Right val -> putStrLn $ "The answer was: " ++ show val

{-
main3 = do
    result <- try (removeFile "textcccc.txt") -- :: IO (Either SomeException Int)
    case result of
        Left ex  -> putStrLn $ "No such file: " ++ show ex
        Right val -> putStrLn $ "File deleted: " ++ show val

-}

main4 = do
    result <- try (removeFile "f4.hs")  :: IO (Either IOException ())
    case result of
        Left ex  -> putStrLn $ "No such file: " ++ show ex
        Right val -> putStrLn $ "File deleted: " ++ show val
    return ()
