module Day20 where

import System.Environment
import System.IO
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Char

import qualified Data.Map as Map

type Point = (Int, Int)
type Space = Map.Map Point String

data Direction = N | E | S | W deriving (Eq, Show)

data Walk = Walk {
    dist :: Int,
    obst :: [String]
} deriving (Show, Eq, Ord)

run :: IO ()
run = do
    args <- getArgs
    file <- openFile (head args) ReadMode
    contents <- hGetContents file
    putStrLn (p1 contents)
    -- putStrLn (p2 contents)
    hClose file

p1 :: String -> String
p1 x = p1_display (p1_explore maze (head (p1_find maze "AA")) 0)
       where maze = Map.fromList (zip [(x, y) | y <- [0..129], x <- [0..129]]
                    (map (:[]) $ filter (/= '\n') x))

p1_find :: Space -> String -> [Point]
p1_find s t = [(x, y) | y <- [0..129], x <- [0..129], d <- [N, E, S, W],
                        p1_lookup (x, y)                         s == '.',
                        p1_lookup (p1_step (x, y) d)             s `elem` t,
                        p1_lookup (p1_step (p1_step (x, y) d) d) s `elem` t]

p1_explore :: Space -> Point -> Int -> Space
p1_explore s p n = case c of
    '#' -> s
    '.' -> run
    _    | isDigit c -> if (read (fromJust (Map.lookup p s)) > n) then run else s
         | t == "AA" -> s
         | t == "ZZ" -> s
         | isUpper c -> jmp
    where c   = p1_lookup p s
          t   = p1_portal s p
          run = foldl (Map.unionWith (max)) (Map.insert p (show n) s)
                [p1_explore (Map.insert p (show n) s) (p1_step p d) (n + 1)
                | d <- [N, E, S, W]]
          loc = filter ((/=) p) (p1_find s t)
          jmp = if (null loc) then s else p1_explore s (head loc) (n + 1)

p1_portal :: Space -> Point -> String
p1_portal s p = (p1_lookup p s):(filter (isLetter)
                [p1_lookup (p1_step p d) s | d <- [N, E, S, W]])

p1_step :: Point -> Direction -> Point
p1_step (x, y) N = (x, y - 1)
p1_step (x, y) E = (x + 1, y)
p1_step (x, y) S = (x, y + 1)
p1_step (x, y) W = (x - 1, y)

p1_display :: Space -> String
p1_display s = intercalate "\n" $ chunksOf 130 $
               [p1_lookup (x, y) s | y <- [0..129], x <- [0..129]] ++ "\n"

p1_lookup :: Point -> Space -> Char
p1_lookup p s = case Map.lookup p s of
    Just i  -> head i
    Nothing -> ' '