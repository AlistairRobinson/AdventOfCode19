module Day17 where

import System.Environment
import System.IO
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Char

import qualified Data.Map as Map

import IntComp

type Point = (Int, Int)
type Space = Map.Map Point Char

run :: IO ()
run = do
    args <- getArgs
    file <- openFile (head args) ReadMode
    contents <- hGetContents file
    putStrLn (p1 contents)
    putStrLn (p2 contents)
    hClose file

p1 :: String -> String
p1 x = show $ foldl (p1_intersections space) 0 (Map.keys space)
       where prog  = conv $ map read $ splitOn "," x
             list  = run_prog (Program 0 0 prog [])
             space = p1_construct list 0 0 Map.empty

p1_construct :: [Integer] -> Int -> Int -> Space -> Space
p1_construct [] x y s = s
p1_construct (10:is) x y s = p1_construct is 0 (y + 1) s
p1_construct ( i:is) x y s = p1_construct is (x + 1) y m
                   where m = (Map.insert (x, y) (chr (fromIntegral i)) s)

p1_adj :: Point -> [Point]
p1_adj (x, y) = [(x, y), (x + 1, y), (x, y + 1), (x - 1, y), (x, y - 1)]

p1_intersections :: Space -> Int -> Point -> Int
p1_intersections s r (x, y) | iscross   = r + (x * y)
                            | otherwise = r
    where adj     = [p1_lookup p s | p <- p1_adj (x, y)]
          iscross = all ((==) '#') adj

p1_lookup :: Point -> Space -> Char
p1_lookup p s = case Map.lookup p s of
    Just '.' -> ' '
    Nothing  -> ' '
    Just c   -> c

p1_display :: Space -> String
p1_display s = intercalate "\n" $ chunksOf 51 $
               [p1_lookup (x, y) s | y <- [0..50], x <- [0..50]] ++ "\n"

p2 :: String -> String
p2 x = show $ list
        where prog  = conv $ 2:(tail $ map read $ splitOn "," x)
              list  = run_prog (Program 0 0 prog (map fromIntegral args))
              instr = (map ord "A,B,A,C,C,A,B,C,B,B")  ++ [10]
              funca = (map ord "L,8,R,10,L,8,R,8")     ++ [10]
              funcb = (map ord "L,12,R,8,R,8")         ++ [10]
              funcc = (map ord "L,8,R,6,R,6,R,10,L,8") ++ [10]
              args  = instr ++ funca ++ funcb ++ funcc ++ [110, 10]