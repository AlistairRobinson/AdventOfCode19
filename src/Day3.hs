module Day3 where

import System.Environment
import System.IO
import Data.List.Split
import Data.List
import Data.Tuple

run :: IO ()
run = do
    args <- getArgs
    file <- openFile (head args) ReadMode
    contents <- hGetContents file
    putStr (p1 contents ++ "\n")
    -- putStr (p2 contents ++ "\n")
    hClose file

data Instr = In Char Int deriving (Show, Eq)
data Point = P  Int Int Int deriving (Show, Eq)

{- Part one
instance Ord Point where
    compare (P a b d) (P x y e) = if (abs a + abs b) >= (abs x + abs y) then GT
                                  else LT
-}

instance Ord Point where
    compare (P a b d) (P x y e) = if d >= e then GT else LT

p1 :: String -> String
p1 x = show $ p1_closest (head (p1_input x)) (last (p1_input x))

p1_input :: String -> [[(Point, Point)]]
p1_input x = map (p1_build . (splitOn ",")) (splitOn "\n" x)

p1_build :: [String] -> [(Point, Point)]
p1_build x = p1_line (map (\y -> In (head y) (read (tail y))) x) (P 0 0 0)

p1_line :: [Instr] -> Point -> [(Point, Point)]
p1_line []              (P x y d) = []
p1_line ((In 'U' b):as) (P x y d) = ((P x y d), (P x (y + b) (d + b))):
                                    (p1_line as (P x (y + b) (d + b)))
p1_line ((In 'D' b):as) (P x y d) = ((P x y d), (P x (y - b) (d + b))):
                                    (p1_line as (P x (y - b) (d + b)))
p1_line ((In 'L' b):as) (P x y d) = ((P x y d), (P (x - b) y (d + b))):
                                    (p1_line as (P (x - b) y (d + b)))
p1_line ((In 'R' b):as) (P x y d) = ((P x y d), (P (x + b) y (d + b))):
                                    (p1_line as (P (x + b) y (d + b)))

p1_closest :: [(Point, Point)] -> [(Point, Point)] -> Maybe Point
p1_closest x y = foldr p1_min Nothing [p1_intersect p1 p2 | p1 <- x, p2 <- y]

p1_min :: Maybe Point -> Maybe Point -> Maybe Point
p1_min (Just p1) (Just p2) = if p1 < p2 then Just p1 else Just p2
p1_min (Nothing) (Just p2) = Just p2
p1_min p1        (Nothing) = p1

p1_intersect :: (Point, Point) -> (Point, Point) -> Maybe Point
p1_intersect ((P x1 y1 d1), (P x2 y2 d2)) ((P a1 b1 c1), (P a2 b2 c2))
    = if min y1 y2 <= min b1 b2 && max y1 y2 >= max b1 b2
      && min a1 a2 <= min x1 x2 && max a1 a2 >= max x1 x2
      && not (x1 == 0 && b1 == 0)
        then Just (P x1 b1 (d1 + abs (y1 - b1) + c1 + abs (a1 - x1))) else
      if min x1 x2 <= min a1 a2 && max x1 x2 >= max a1 a2
      && min b1 b2 <= min y1 y2 && max b1 b2 >= max y1 y2
      && not (a1 == 0 && y1 == 0)
        then Just (P a1 y1 (d1 + abs (x1 - a1) + c1 + abs (b1 - y1))) else
      Nothing