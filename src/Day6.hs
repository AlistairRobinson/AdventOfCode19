module Day6 where

import System.Environment
import System.IO
import Data.List.Split
import Data.List

import qualified Data.Map as Map

run :: IO ()
run = do
    args <- getArgs
    file <- openFile (head args) ReadMode
    contents <- hGetContents file
    putStr (p1 contents ++ "\n")
    putStr (p2 contents ++ "\n")
    hClose file

p1 :: String -> String
p1 x = show $ p1_depth input 0 "COM"
       where input = Map.fromListWith (++) $ p1_input $ splitOn "\r\n" x

p1_input :: [String] -> [(String, [String])]
p1_input x = fmap (\a -> (head (splitOn ")" a), [last (splitOn ")" a)])) x

p1_depth :: Map.Map String [String] -> Int -> String -> Int
p1_depth m n s = case Map.lookup s m of
                 Just v  -> n + (sum $ map (p1_depth m (n + 1)) v)
                 Nothing -> n

p2_contains :: Map.Map String [String] -> String -> String -> Bool
p2_contains m s k = if s == k then True else
                    case Map.lookup k m of
                    Just v  -> any (p2_contains m s) v
                    Nothing -> False

p2 :: String -> String
p2 x = show $ length $ union (you \\ san) (san \\ you)
    where input = Map.fromListWith (++) $ p1_input $ splitOn "\r\n" x
          you   = (filter (p2_contains input "YOU") (Map.keys input))
          san   = (filter (p2_contains input "SAN") (Map.keys input))