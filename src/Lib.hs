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
d1p1 x = show $ foldl (\x y -> x + d1p1_fuel y) 0 $ map read $ splitOn "\n" x

d1p1_fuel :: Int -> Int
d1p1_fuel x = (x `div` 3) - 2

d1p2 :: String -> String
d1p2 x = show $ foldl (\x y -> x + d1p2_fuel y) 0 $ map read $ splitOn "\n" x

d1p2_fuel :: Int -> Int
d1p2_fuel x | d1p1_fuel x > 0 = d1p1_fuel x + d1p2_fuel (d1p1_fuel x)
            | otherwise       = 0