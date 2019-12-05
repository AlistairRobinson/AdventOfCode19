module Day4 where

import System.Environment
import System.IO
import Data.List.Split
import Data.List

run :: IO ()
run = do
    args <- getArgs
    file <- openFile (head args) ReadMode
    contents <- hGetContents file
    putStr (p1 contents ++ "\n")
    putStr (p2 contents ++ "\n")
    hClose file

p1 :: String -> String
p1 x = show $ length $ p1_passwords (head input) (last input)
       where input = map read $ splitOn "-" x

p1_passwords :: Int -> Int -> [Int]
p1_passwords min max = [x | x <- [min..max], p1_valid x]

p1_valid :: Int -> Bool
p1_valid x = show x == sort (show x) &&
             (maximum (map length (group (show x))) >= 2)

p2 :: String -> String
p2 x = show $ length $ p2_passwords (head input) (last input)
       where input = map read $ splitOn "-" x

p2_passwords :: Int -> Int -> [Int]
p2_passwords min max = [x | x <- [min..max], p2_valid x]

p2_valid :: Int -> Bool
p2_valid x = show x == sort (show x) && elem 2 (map length (group (show x)))
