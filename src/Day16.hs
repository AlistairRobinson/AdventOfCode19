module Day16 where

import System.Environment
import System.IO
import Data.List.Split
import Data.List
import Data.Char

run :: IO ()
run = do
    args <- getArgs
    file <- openFile (head args) ReadMode
    contents <- hGetContents file
    putStrLn (p1 contents)
    putStrLn (p2 contents)
    hClose file

p1 :: String -> String
p1 x = show $ p1_process 100 input
       where input = map digitToInt x

p1_base :: [Int]
p1_base = [0, 1, 0, -1]

p1_process :: Int -> [Int] -> [Int]
p1_process 0 xs = xs
p1_process i xs = [((abs . sum) (zipWith (*) (p1_process (i - 1) xs)
                   (tail (cycle (concatMap (replicate n) p1_base))))) `mod` 10
                  | n <- [1..(length xs)]]

p2 :: String -> String
p2 x = show $ take 8 $ reverse $ (p2_process 100 local)
        where input = map digitToInt (concat (replicate 10000 x))
              ndrop = read (take 7 x)
              local = reverse (drop ndrop input)

p2_process :: Int -> [Int] -> [Int]
p2_process 0 xs = xs
p2_process i xs = scanl' (\a b -> (a + b) `mod` 10) 0 (p2_process (i - 1) xs)