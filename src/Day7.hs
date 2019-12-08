module Day7 where

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
p1 x = show $ maximum $ p1_amp prog 5 0 []
       where prog = map read $ splitOn "," x

p1_amp :: [Int] -> Int -> Int -> [Int] -> [Int]
p1_amp prog 0 i b = [i]
p1_amp prog x i b = concat [p1_amp prog (x - 1)
                              (head (run_ic 0 prog [a, i])) (a:b)
                              | a <- [0..4], a `notElem` b]

p2 :: String -> String
p2 x = show $ maximum [p2_amp (map (\i -> Program 0 prog [i]) perms) 0
                      | perms <- permutations [5..9]]
       where prog = map read $ splitOn "," x

p2_amp :: [Program] -> Int -> Int
p2_amp []     inp = inp
p2_amp (q:qs) inp = case res of
    (p@(Program n [] as), r) -> p2_amp qs inp
    (p@(Program n m  as), r) -> p2_amp (qs ++ [p]) r
    where res = yield_prog (p2_load q inp)

p2_load :: Program -> Int -> Program
p2_load (Program n m a) inp = Program n m (a ++ [inp])