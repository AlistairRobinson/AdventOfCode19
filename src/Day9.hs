module Day9 where

import System.Environment
import System.IO
import Data.List
import Data.List.Split

import IntComp

run :: IO ()
run = do
    args <- getArgs
    file <- openFile (head args) ReadMode
    contents <- hGetContents file
    putStr (p1 contents ++ "\n")
    putStr (p2 contents ++ "\n")
    hClose file

p1 :: String -> String
p1 x = show $ run_ic 0 0 prog [1]
        where prog = map read $ splitOn "," x

p2 :: String -> String
p2 x = show $ run_ic 0 0 prog [2]
        where prog = map read $ splitOn "," x