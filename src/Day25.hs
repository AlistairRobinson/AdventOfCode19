module Day25 where

import System.Environment
import System.IO
import Data.Char
import Data.List
import Data.List.Split

import IntComp

run :: IO ()
run = do
    args  <- getArgs
    file  <- openFile (head args) ReadMode
    file' <- openFile (args !! 1) ReadMode
    contents  <- hGetContents file
    contents' <- hGetContents file'
    putStrLn (p1 contents contents')
    --putStrLn (p2 contents contents')
    hClose file
    hClose file'

p1_items :: [String]
p1_items = ["mug\n", "ornament\n", "hypercube\n", "astronaut ice cream\n", "wreath\n", "mouse\n", "prime number\n", "easter egg\n"]

p1_drops :: [[String]]
p1_drops = subsequences (map (\x -> "drop " ++ x) p1_items)

p1_done :: [[String]]
p1_done = map (\x -> x ++ ["north\n"] ++ (map (\x -> "take " ++ x) p1_items)) p1_drops

p1 :: String -> String -> String
p1 x y = map (chr . fromIntegral) r
         where prog    = conv $ map read $ splitOn "," x
               args    = map (\s -> (filter (/= '\r') s) ++ "\n") (lines y)
               instr   = concat $ map (map (fromIntegral . ord)) args
               (p', r) = iter_prog (Program 0 0 prog instr) []
