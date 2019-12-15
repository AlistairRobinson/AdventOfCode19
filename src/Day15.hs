module Day15 where

import System.Environment
import System.IO
import Data.List
import Data.List.Split
import Data.Maybe

import qualified Data.Map as Map

import IntComp

type Point = (Integer, Integer)
type Space = Map.Map Point Item

data Direction = N | E | S | W            deriving (Eq, Show)
data Item = Wall | Path | Dead | New | O2 deriving (Eq)

instance Show Item where
    show Wall = "█"
    show Path = "."
    show Dead = "x"
    show New  = "█"
    show O2   = "~"

run :: IO ()
run = do
    args <- getArgs
    file <- openFile (head args) ReadMode
    contents <- hGetContents file
    putStrLn (p1 contents)
    putStrLn (p2 contents)
    hClose file

p1 :: String -> String
p1 x = (p1_display s) ++ (p1_result s)
       where prog   = conv $ map read $ splitOn "," x
             (s, p) = p1_run (Program 0 0 prog []) (0, 0) (Map.empty)

p1_run :: Program -> Point -> Space -> (Space, Point)
p1_run p x s = case next of
    Nothing -> p1_move p x (Map.insert x Dead s) (fromJust back)
    Just d  -> p1_move p x (Map.insert x Path s) d
    where local = p1_search x s
          walls = length $ filter ((==) Wall) local
          next  = (elemIndex New  local) >>= (p1_dir)
          back  = (elemIndex Path local) >>= (p1_dir)

p1_move :: Program -> Point -> Space -> Direction -> (Space, Point)
p1_move (Program n q m a) p s d = case ret of
    0 -> p1_run prog p (Map.insert (p1_step p d) Wall s)
    1 -> p1_run prog (p1_step p d) s
    2 -> (s, p1_step p d)
    where (prog, ret) = yield_prog (Program n q m ((p1_read d):a))

p1_lookup :: Point -> Space -> Item
p1_lookup p s = case Map.lookup p s of
    Just i  -> i
    Nothing -> New

p1_search :: Point -> Space -> [Item]
p1_search p s = [p1_lookup (p1_step p d) s | d <- [N, S, W, E]]

p1_dir :: Int -> Maybe Direction
p1_dir 0 = Just N
p1_dir 1 = Just S
p1_dir 2 = Just W
p1_dir 3 = Just E
p1_dir _ = Nothing

p1_read :: Direction -> Integer
p1_read N = 1
p1_read S = 2
p1_read W = 3
p1_read E = 4

p1_step :: Point -> Direction -> Point
p1_step (x, y) N = (x, y - 1)
p1_step (x, y) E = (x + 1, y)
p1_step (x, y) S = (x, y + 1)
p1_step (x, y) W = (x - 1, y)

p1_display :: Space -> String
p1_display s = intercalate "\n" $ chunksOf 51 $
               concat [show (p1_lookup (x, y) s)
                      | y <- [-25..25], x <- [-25..25]] ++ "\n"

p1_result :: Space -> String
p1_result s = show (length (p1_path s))

p1_path :: Space -> [Point]
p1_path s = [p | p <- Map.keys s, Map.lookup p s == Just Path]

p2 :: String -> String
p2 x = show $ p2_flood s [p] 0
       where prog   = conv $ map read $ splitOn "," x
             (s, p) = p1_run (Program 0 0 prog []) (0, 0) (Map.empty)

p2_flood :: Space -> [Point] -> Int -> Int
p2_flood s [] i = i - 1
p2_flood s p  i = p2_flood (foldl p2_insert s p) p' (i + 1)
                  where p' = [p1_step q d | q <- p, d <- [N, S, W, E],
                              p1_lookup (p1_step q d) s `elem` [Dead, Path]]

p2_insert :: Space -> Point -> Space
p2_insert s p = Map.insert p O2 s