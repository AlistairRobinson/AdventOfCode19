module Day12 where

import System.Environment
import System.IO
import Data.List
import Data.List.Split

import qualified Data.Map as Map

type Vector = (Integer, Integer, Integer)
type State = (Integer, Integer)
data Moon   = Moon {
    pos :: Vector,
    vel :: Vector
} deriving Show

run :: IO ()
run = do
    args <- getArgs
    file <- openFile (head args) ReadMode
    contents <- hGetContents file
    putStrLn (p1 contents)
    putStrLn (p2 contents)
    hClose file

p1 :: String -> String
p1 x = show $ last $ map sum (map (map p1_energy) (take 1000 (p1_run [a, b, c, d])))
       where a = Moon ( -9, 10, -1) (0, 0, 0)
             b = Moon (-14, -8, 14) (0, 0, 0)
             c = Moon (  1,  5,  6) (0, 0, 0)
             d = Moon (-19,  7,  8) (0, 0, 0)

p1_run :: [Moon] -> [[Moon]]
p1_run m = move:(p1_run move)
           where grav = map (\r -> foldl (\z a -> p1_grav z a) r m) m
                 move = map (p1_move) grav

p1_grav :: Moon -> Moon -> Moon
p1_grav (Moon (x1, y1, z1) (vx1, vy1, vz1)) (Moon (x2, y2, z2) v2)
    = Moon (x1, y1, z1) (vx3, vy3, vz3)
    where vx3 = if x1 < x2 then vx1 + 1 else if x1 > x2 then vx1 - 1 else vx1
          vy3 = if y1 < y2 then vy1 + 1 else if y1 > y2 then vy1 - 1 else vy1
          vz3 = if z1 < z2 then vz1 + 1 else if z1 > z2 then vz1 - 1 else vz1

p1_move :: Moon -> Moon
p1_move(Moon (x, y, z) (a, b, c)) = Moon (x + a, y + b, z + c) (a, b, c)

p1_energy :: Moon -> Integer
p1_energy (Moon (x, y, z) (a, b, c)) = p * k
            where p = (sum $ map abs [x, y, z])
                  k = (sum $ map abs [a, b, c])

p2 :: String -> String
p2 x = show $ foldl lcm 1 [length (p2_period [a, b, c, d] axis) + 1
                          | axis <- [p2_x, p2_y, p2_z]]
    where a = Moon ( -9, 10, -1) (0, 0, 0)
          b = Moon (-14, -8, 14) (0, 0, 0)
          c = Moon (  1,  5,  6) (0, 0, 0)
          d = Moon (-19,  7,  8) (0, 0, 0)

p2_run :: [Moon] -> [Moon]
p2_run m = map (p1_move) grav
    where grav = map (\r -> foldl p1_grav r m) m

p2_period :: [Moon] -> ([Moon] -> [State]) -> [[State]]
p2_period m a = takeWhile (/= (a m)) (map a (iterate (p2_run) (p2_run m)))

p2_x :: [Moon] -> [State]
p2_x m = map (\(Moon (x, _, _) (a, _, _)) -> (x, a)) m

p2_y :: [Moon] -> [State]
p2_y m = map (\(Moon (_, y, _) (_, b, _)) -> (y, b)) m

p2_z :: [Moon] -> [State]
p2_z m = map (\(Moon (_, _, z) (_, _, c)) -> (z, c)) m