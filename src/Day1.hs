module Day1 where

import System.Environment
import System.IO
import Data.List.Split

run :: IO ()
run = do
    args <- getArgs
    file <- openFile (head args) ReadMode
    contents <- hGetContents file
    putStr (p1 contents ++ "\n")
    putStr (p2 contents ++ "\n")
    hClose file

p1 :: String -> String
p1 x = show $ foldl (\x y -> x + p1_fuel y) 0 $ map read $ splitOn "\n" x

p1_fuel :: Int -> Int
p1_fuel x = (x `div` 3) - 2

p2 :: String -> String
p2 x = show $ foldl (\x y -> x + p2_fuel y) 0 $ map read $ splitOn "\n" x

p2_fuel :: Int -> Int
p2_fuel x | p1_fuel x > 0 = p1_fuel x + p2_fuel (p1_fuel x)
          | otherwise       = 0