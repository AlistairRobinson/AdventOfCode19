module Day19 where

import System.Environment
import System.IO
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Char

import qualified Data.Map as Map

import IntComp

type Point = (Integer, Integer)
type Space = Map.Map Point Integer

run :: IO ()
run = do
    args <- getArgs
    file <- openFile (head args) ReadMode
    contents <- hGetContents file
    putStrLn (p1 contents)
    putStrLn (p2 contents)
    hClose file

p1 :: String -> String
p1 x = p1_display (foldl p1_run Map.empty list)
        where prog = conv $ map read $ splitOn "," x
              list = [(Program 0 0 prog [x, y]) | y <- [0..49], x <- [0..49]]

p1_run :: Space -> Program -> Space
p1_run s p@(Program _ _ _ (x:y:zs)) = Map.insert (x, y) (snd (yield_prog p)) s

p1_display :: Space -> String
p1_display s = intercalate "\n" $ chunksOf 51 $
               [p1_lookup (x, y) s | y <- [0..50], x <- [0..50]] ++ "\n"

p1_lookup :: Point -> Space -> Char
p1_lookup p s = case Map.lookup p s of
                    Just 1 -> '#'
                    _      -> ' '

p2 :: String -> String
p2 x = show $ p2_run (Program 0 0 prog []) (0, 99)
        where prog  = conv $ map read $ splitOn "," x

p2_run :: Program -> Point -> Integer
p2_run p@(Program n q prog []) (x, y)
    | snd (yield_prog (Program n q prog [x, y]))           == 0 = p2_run p (x + 1, y)
    | snd (yield_prog (Program n q prog [x + 99, y - 99])) == 0 = p2_run p (x, y + 1)
    | otherwise = 10000 * x + (y - 99)
