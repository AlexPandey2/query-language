module Main (main) where

import RQLEval
import System.Environment 
import Control.Exception
import System.IO

main :: IO ()
main = catch main' noParse

main' :: IO ()
main' = do
    args <- getArgs
    case args of 
        [fileName] -> do
            contents <- readFile fileName
            let res = rqlMain contents
            result <- res
            putStrLn result
        _ -> do
            putStrLn "Invalid filename"

noParse :: ErrorCall -> IO ()
noParse e = do 
    let err = show e
    hPutStr stderr err
    return ()
             
