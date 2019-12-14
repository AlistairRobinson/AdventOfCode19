module Day13 where

import System.Environment
import System.IO
import Data.List
import Data.List.Split

import qualified Data.Map as Map

import IntComp

type Point = (Integer, Integer)
type Grid  = Map.Map Point String

run :: IO ()
run = do
    args <- getArgs
    file <- openFile (head args) ReadMode
    contents <- hGetContents file
    putStrLn (p1 contents)
    putStrLn (p2 contents)
    hClose file

p1 :: String -> String
p1 x = p1_display $ foldl p1_render (Map.empty) m
       where prog = conv $ map read $ splitOn "," x
             m    = chunksOf 3 $ run_prog (Program 0 0 prog [])

p1_render :: Grid -> [Integer] -> Grid
p1_render g []         = g
p1_render g (x:y:t:zs) = case t of
    0 -> Map.insert (x, y) " " g
    1 -> Map.insert (x, y) "█" g
    2 -> Map.insert (x, y) "#" g
    3 -> Map.insert (x, y) "=" g
    4 -> Map.insert (x, y) "0" g
    _ -> Map.insert (x, y) (show t) g

p1_lookup :: Grid -> Point -> String
p1_lookup g p = case Map.lookup p g of
                    Just c -> c
                    _      -> " "

p1_display :: Grid -> String
p1_display g = score ++ (intercalate "\n" $ chunksOf 43 $
               concat [p1_lookup g (x, y) | y <- [0..25], x <- [0..42]])
               where score = "\n" ++ p1_lookup g (-1, 0) ++ "\n"

p2 :: String -> String
p2 x = p1_display grid
       where prog = conv $ 2: tail(map read $ (splitOn "," x))
             grid = p2_run (Program 0 0 prog []) Map.empty

p2_run :: Program -> Grid -> Grid
p2_run p g | Map.null m = grid
           | x < x'     = p2_run (Program n q m [-1]) grid
           | x > x'     = p2_run (Program n q m [1])  grid
           | otherwise  = p2_run (Program n q m [0])  grid
           where (Program n q m a, r) = iter_prog p []
                 grid = Map.union (foldl p1_render Map.empty (chunksOf 3 r)) g
                 (x, _) = p2_find "0" grid
                 (x',_) = p2_find "=" grid

p2_find :: String -> Grid -> Point
p2_find s g | null l    = (0, 0)
            | otherwise = head l
            where l = [p | p <- Map.keys g, Map.lookup p g == (Just s)]