module Day23 where

import System.Environment
import System.IO
import Data.List
import Data.List.Split

import qualified Data.Map as Map

import IntComp

type Computer = (Integer, Program, Bool)
type Packet = (Integer, Integer, Integer)

data Nat = Nat {
    p :: Maybe Packet,
    c :: Integer
}

run :: IO ()
run = do
    args <- getArgs
    file <- openFile (head args) ReadMode
    contents <- hGetContents file
    putStrLn (p1 contents)
    putStrLn (p2 contents)
    hClose file

fst' (x, _, _) = x
snd' (_, x, _) = x
trd' (_, _, x) = x

p1 :: String -> String
p1 x = show $ p1_run nics
    where prog = conv $ map read $ splitOn "," x
          nics = [(i, Program 0 0 prog [i], True) | i <- [0..49]]

p1_run :: [Computer] -> Packet
p1_run (c:cs) = case (step_prog (snd' c)) of
    (p, i, Just d) | d == 255  -> (d, x, y)
                   | otherwise -> p1_run $ map (p1_send (d, x, y)) (cs ++ [(fst' c, p'', True)])
        where (p',  x) = yield_prog p
              (p'', y) = yield_prog p'
    (p, i, _)     -> p1_run (cs ++ [(fst' c, p, True)])

p1_send :: Packet -> Computer -> Computer
p1_send (d, x, y) (i, Program n q m a, b) | i == d    = (i, Program n q m (a ++ [x, y]), b)
                                          | otherwise = (i, Program n q m a, b)

p2 :: String -> String
p2 x = show $ take 25 $ p2_run nics (Nat Nothing (-1))
    where prog = conv $ map read $ splitOn "," x
          nics = [(i, Program 0 0 prog [i], False) | i <- [0..49]]

p2_run :: [Computer] -> Nat -> [Integer]
p2_run (c:cs) n@(Nat q l) = case (step_prog (snd' c)) of
    (p, i, Just d) | d == 255  -> p2_run cs' (Nat (Just (0, x, y)) (fst' c))
                   | otherwise -> p2_run (map (p1_send (d, x, y)) cs') (Nat q (fst' c))
                 where (p',  x) = yield_prog p
                       (p'', y) = yield_prog p'
                       cs'      = (cs ++ [((fst' c), p'', False)])
    (p, i, _) | idle           -> case q of
                    Just (0, px, py) -> py:(p2_run (map (p1_send (0, px, py)) cs') (Nat Nothing l))
                    _                -> p2_run cs' n
              | otherwise      -> p2_run cs' n
                 where cs'   = (cs ++ [(fst' c, p, empty)])
                       empty = (o i == In && args (snd' c) == []) || trd' c
                       idle  = l == (fst' c) && p2_empty cs'

p2_empty :: [Computer] -> Bool
p2_empty cs = all (\(i, (Program n q m a), b) -> a == [] && b) cs

p2_duplicate :: [Integer] -> Integer
p2_duplicate (x:xs) | x `elem` xs = x
                    | otherwise   = p2_duplicate xs