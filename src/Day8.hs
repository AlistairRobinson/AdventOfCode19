module Day8 where

import System.Environment
import System.IO
import Data.List.Split
import Data.List
import Data.Char

run :: IO ()
run = do
    args <- getArgs
    file <- openFile (head args) ReadMode
    contents <- hGetContents file
    putStrLn (p1 contents)
    putStrLn (p2 contents)
    hClose file

p1 :: String -> String
p1 x = show $ maximum [(length (filter (/= 0) l),
                       (length (filter (== 1) l)) *
                       (length (filter (== 2) l)))
                      | l <- (p1_input x)]

p1_input :: String -> [[Int]]
p1_input x = map (map digitToInt) $ chunksOf (25*6) x

p2 :: String -> String
p2 x = p2_display $ foldr (zipWith p2_layer) (replicate (25*6) 0) (p1_input x)

p2_layer :: Int -> Int -> Int
p2_layer 2 z = z
p2_layer a z = a

p2_display :: [Int] -> String
p2_display d = intercalate "\n" $ chunksOf 25 (map p2_convert d)

p2_convert :: Int -> Char
p2_convert 0 = ' '
p2_convert 1 = '█'