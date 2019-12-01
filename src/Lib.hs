module Lib where

import System.Environment
import System.IO
import Data.List.Split

run :: IO ()
run = do
    args <- getArgs
    file <- openFile (head args) ReadMode 
    contents <- hGetContents file  
    putStr (d1p1 contents ++ "\n")
    hClose file 

d1p1 :: [Char] -> [Char]
d1p1 x = show $ foldl d1p1_process 0 $ splitOn "\n" x

d1p1_process :: Int -> [Char] -> Int
d1p1_process x y = x + (floor ((read y) / 3)) - 2