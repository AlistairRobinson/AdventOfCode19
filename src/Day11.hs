module Day11 where

import System.Environment
import System.IO
import Data.List
import Data.List.Split

import qualified Data.Map as Map

import IntComp

type Point = (Integer, Integer)
type Space  = Map.Map Point Char

run :: IO ()
run = do
    args <- getArgs
    file <- openFile (head args) ReadMode
    contents <- hGetContents file
    putStrLn (p1 contents)
    putStrLn (p2 contents)
    hClose file

p1 :: String -> String
p1 x = show $ length (Map.toList m)
       where prog = conv $ map read $ splitOn "," x
             m    = p1_run (Map.empty) (0, 0) (Program 0 0 prog []) 0

p1_run :: Space -> Point -> Program -> Integer -> Space
p1_run s p (Program n q m a) d
    | Map.null m = s
    | otherwise  = p1_trn (p1_paint s p (snd res)) p (fst res) d
       where res = yield_prog (Program n q m ((p1_sense s p):a))

p1_trn :: Space -> Point -> Program -> Integer -> Space
p1_trn s p (Program n q m a) d = p1_run s (p1_move p e) (fst res) e
                     where res = yield_prog (Program n q m a)
                           e   = p1_rotate d (snd res)

p1_move :: Point -> Integer -> Point
p1_move (x, y) 0 = (x, y - 1)
p1_move (x, y) 1 = (x + 1, y)
p1_move (x, y) 2 = (x, y + 1)
p1_move (x, y) 3 = (x - 1, y)

p1_rotate :: Integer -> Integer -> Integer
p1_rotate d 0 = (d + 3) `mod` 4
p1_rotate d 1 = (d + 1) `mod` 4

p1_sense :: Space -> Point -> Integer
p1_sense s p = case Map.lookup p s of
                    Just '█' -> 1
                    _        -> 0

p1_paint :: Space -> Point -> Integer -> Space
p1_paint s p 0 = Map.insert p ' ' s
p1_paint s p 1 = Map.insert p '█' s

p2 :: String -> String
p2 x = p2_display m
       where prog = conv $ map read $ splitOn "," x
             s    = Map.insert (0, 0) '█' (Map.empty)
             m    = p1_run s (0, 0) (Program 0 0 prog []) 0

p2_lookup :: Space -> Point -> Char
p2_lookup s p = case Map.lookup p s of
                    Just c -> c
                    _      -> ' '

p2_display :: Space -> String
p2_display s = intercalate "\n" $ chunksOf 50 [p2_lookup s (x, y)
                                              | y <- [0..5], x <- [0..49]]