module Day10 where

import System.Environment
import System.IO
import Data.List
import Data.List.Split

import qualified Data.Set as Set
import qualified Data.Map as Map

type Vector = (Int, Int)
type Space  = Map.Map Vector Char

compare_v :: Vector -> Vector -> Ordering
compare_v a@(ax, ay) b@(bx, by)
    | ax == 0 && ay < 0 = LT
    | bx == 0 && by < 0 = GT
    | otherwise         = compare ang1 ang2
        where ang1 = atan2 (fromIntegral (-ax)) (fromIntegral (ay))
              ang2 = atan2 (fromIntegral (-bx)) (fromIntegral (by))

run :: IO ()
run = do
    args <- getArgs
    file <- openFile (head args) ReadMode
    contents <- hGetContents file
    putStr (p1 contents ++ "\n")
    putStr (p2 contents ++ "\n")
    hClose file

p1 :: String -> String
p1 inp = show $ (p1_process s)
         where s = Map.fromList $ zip [(x, y) | y <- [0..19], x <- [0..19]] inp

p1_process :: Space -> (Int, Vector)
p1_process s = maximum $ [(length (p1_visible s a) - 1, a)
                         | a <- Map.keys s, p1_is_ast s a]

p1_visible :: Space -> Vector -> Set.Set Vector
p1_visible s a = Set.fromList $ [p1_vec a k
                                | k <- Map.keys s, p1_is_ast s k]

p1_is_ast :: Space -> Vector -> Bool
p1_is_ast s v = case Map.lookup v s of
                  Just '#' -> True
                  _        -> False

p1_vec :: Vector -> Vector -> Vector
p1_vec (x1, y1) (x2, y2) = (x `div` d, y `div` d)
                           where x = x2 - x1
                                 y = y2 - y1
                                 d = max (gcd x y) 1

p2 :: String -> String
p2 inp = show $ (p2_destroy 200 b s (p2_visible s b))
         where i = Map.fromList $ zip [(x, y) | y <- [0..30], x <- [0..30]] inp
               b = snd (p1_process i)
               s = Map.insert b 'X' i

p2_visible :: Space -> Vector -> [Vector]
p2_visible s a = sortBy compare_v (unique $ [p1_vec a k
                                            | k <- Map.keys s, p1_is_ast s k])
                 where unique = (Set.toList . Set.fromList)

p2_destroy :: Int -> Vector -> Space -> [Vector] -> Vector
p2_destroy 1 b s (v:vs) = (fst b + fst v, snd b + snd v)
p2_destroy i b s (v:vs) = p2_destroy (i - 1) b (Map.insert
                          (fst b + fst v, snd b + snd v) '.' s) vs
p2_destroy i b s []     = p2_destroy i b s (p2_visible s b)
