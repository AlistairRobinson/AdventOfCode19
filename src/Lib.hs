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
    putStr (d1p2 contents ++ "\n")
    hClose file

d1p1 :: String -> String
d1p1 x = show $ foldl d1p1_process 0 $ splitOn "\n" x

d1p1_process :: Int -> String -> Int
d1p1_process x y = x + floor (read y / 3) - 2

d1p2 :: String -> String
d1p2 x = show $ foldl d1p2_process 0 $ splitOn "\n" x

d1p2_process :: Int -> String -> Int
d1p2_process x y = x + d1p2_fuel (read y)

d1p2_fuel :: Int -> Int
d1p2_fuel x | x > 0 = max (f x) 0 + d1p2_fuel (f x)
            | x < 1 = 0
              where f x = floor (fromIntegral x / 3) - 2