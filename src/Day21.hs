module Day21 where

import System.Environment
import System.IO
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Char

import IntComp

run :: IO ()
run = do
    args <- getArgs
    file <- openFile (head args) ReadMode
    contents <- hGetContents file
    putStrLn (p1 contents)
    putStrLn (p2 contents)
    hClose file

p1_instr :: String
p1_instr = "NOT A J\nOR B T\nAND C T\nNOT T T\nAND D T\nOR T J\nWALK\n"

p1 :: String -> String
p1 x = show (map fromIntegral list)
       where prog  = conv $ map read $ splitOn "," x
             list  = run_prog (Program 0 0 prog (map fromIntegral args))
             args  = (map ord p1_instr)

p2_instr :: String
p2_instr = "OR B T\nAND C T\nNOT T T\nAND D T\nOR E J\nOR H J\nAND T J\nNOT A T\nOR T J\nRUN\n"

p2 :: String -> String
p2 x = show (map fromIntegral list)
       where prog  = conv $ map read $ splitOn "," x
             list  = run_prog (Program 0 0 prog (map fromIntegral args))
             args  = (map ord p2_instr)
