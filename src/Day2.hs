module Day2 where

import System.Environment
import System.IO
import Data.List.Split
import Data.Sequence

run :: IO ()
run = do
    args <- getArgs
    file <- openFile (head args) ReadMode
    contents <- hGetContents file
    putStr (p1 contents ++ "\n")
    putStr (p2 contents ++ "\n")
    hClose file

p1 :: String -> String
p1 x = show $ p1_execute input input
       where input = p1_process (fromList $ map read $ splitOn "," x) 12 2

p1_process :: Seq Int -> Int -> Int -> Seq Int
p1_process xs n v = update 2 v (update 1 n xs)

p1_execute :: Seq Int -> Seq Int -> Seq Int
p1_execute xs ys | index xs 0 == 1 = p1_execute (next 4 xs) $ update c (a + b) ys
                 | index xs 0 == 2 = p1_execute (next 4 xs) $ update c (a * b) ys
                 | otherwise       = ys
                   where a    = index ys (index xs 1)
                         b    = index ys (index xs 2)
                         c    = index xs 3
                         next = Data.Sequence.drop

p2 :: String -> String
p2 x = show $ [(n, v) | n <- [0..99], v <- [0..99],
               index (p1_execute (input n v) (input n v)) 0 == 19690720]
       where input = p1_process $ fromList $ map read $ splitOn "," x