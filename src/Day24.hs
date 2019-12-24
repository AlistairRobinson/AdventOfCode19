module Day24 where

import System.Environment
import System.IO
import Data.List
import Data.List.Split

import qualified Data.Set as Set
import qualified Data.Map as Map

type Point = (Int, Int)
type Space = Map.Map Point Char
type Level = (Int, Int, Int)
type HyperSpace = Map.Map Level Char

data Direction = N | E | S | W

run :: IO ()
run = do
    args <- getArgs
    file <- openFile (head args) ReadMode
    contents <- hGetContents file
    putStrLn (p1 contents)
    putStrLn (p2 contents)
    hClose file

p1 :: String -> String
p1 i = show $ p1_run s Set.empty
       where s   = Map.fromList $ zip [(x, y) | y <- [0..yl], x <- [0..xl]]
                  (concat (lines inp))
             xl  = length (head (lines inp)) - 1
             yl  = length (lines inp) - 1
             inp = filter (\c -> c /= '\r') i

p1_run :: Space -> Set.Set Int -> Int
p1_run s d | p1_div s' `Set.member` d = p1_div s'
           | otherwise                = p1_run s' (Set.insert (p1_div s') d)
             where s' = p1_process s

p1_div :: Space -> Int
p1_div s = sum [2^(x + (y * (5))) | (x, y) <- Map.keys s, p1_lookup (x, y) s == '#']

p1_process :: Space -> Space
p1_process s = Map.fromList [(p, p1_state s p) | p <- Map.keys s]

p1_state :: Space -> Point -> Char
p1_state s p | b == '#' && adj /= 1 = '.'
             | b /= '#' && adj == 1 = '#'
             | b /= '#' && adj == 2 = '#'
             | otherwise            = b
               where b   = p1_lookup p s
                     adj = length $ filter ((==) '#') [p1_lookup (p1_step p d) s
                                                      | d <- [N, E, S, W]]

p1_step :: Point -> Direction -> Point
p1_step (x, y) N = (x, y - 1)
p1_step (x, y) E = (x + 1, y)
p1_step (x, y) S = (x, y + 1)
p1_step (x, y) W = (x - 1, y)

p1_lookup :: Point -> Space -> Char
p1_lookup p s = case Map.lookup p s of
    Just i  -> i
    Nothing -> ' '

p1_display :: Space -> String
p1_display s = intercalate "\n" $ chunksOf 6 $
               [p1_lookup (x, y) s | y <- [0..5], x <- [0..5]] ++ "\n"

p2 :: String -> String
p2 i = show $ map p2_pop $ take 201 $ iterate p2_process h
        where s   = Map.fromList $ zip [(0, x, y) | y <- [0..yl], x <- [0..xl]]
                    (concat (lines inp))
              p   = foldl (\z a -> Map.insert (fst a) (snd a) z) s
                    [((d, x, y), '.') | d <- [-200..200], y <- [0..yl], x <- [0..xl], d /= 0]
              h   = foldl (\z a -> Map.insert (fst a) (snd a) z) p
                    [((d, 2, 2), '?') | d <- [-200, 200]]
              xl  = length (head (lines inp)) - 1
              yl  = length (lines inp) - 1
              inp = filter (\c -> c /= '\r') i

p2_pop :: HyperSpace -> Int
p2_pop h = Map.size $ Map.filter ((==) '#') h

p2_process :: HyperSpace -> HyperSpace
p2_process h = foldl (\z a -> Map.insert (fst a) (snd a) z)
                     (Map.mapWithKey (p2_state h) h) [((d, 2, 2), '?') | d <- [-200, 200]]

p2_state :: HyperSpace -> Level -> Char -> Char
p2_state h p b | b == '#' && adj /= 1 = '.'
               | b /= '#' && adj == 1 = '#'
               | b /= '#' && adj == 2 = '#'
               | otherwise            = b
                 where adj = sum [p2_count (p2_step p d) d h | d <- [N, E, S, W]]

p2_count :: Level -> Direction -> HyperSpace -> Int
p2_count (l, -1,  y) d h = p2_count (l - 1, 1, 2) d h
p2_count (l,  5,  y) d h = p2_count (l - 1, 3, 2) d h
p2_count (l,  x, -1) d h = p2_count (l - 1, 2, 1) d h
p2_count (l,  x,  5) d h = p2_count (l - 1, 2, 3) d h
p2_count (l,  2,  2) N h = sum [p2_count (l + 1, x, 4) N h | x <- [0..4]]
p2_count (l,  2,  2) S h = sum [p2_count (l + 1, x, 0) S h | x <- [0..4]]
p2_count (l,  2,  2) E h = sum [p2_count (l + 1, 0, y) E h | y <- [0..4]]
p2_count (l,  2,  2) W h = sum [p2_count (l + 1, 4, y) W h | y <- [0..4]]
p2_count p _ h = case Map.lookup p h of
    Just '#' -> 1
    _        -> 0

p2_step :: Level -> Direction -> Level
p2_step (d, x, y) N = (d, x, y - 1)
p2_step (d, x, y) E = (d, x + 1, y)
p2_step (d, x, y) S = (d, x, y + 1)
p2_step (d, x, y) W = (d, x - 1, y)