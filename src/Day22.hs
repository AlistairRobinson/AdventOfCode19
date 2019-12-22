module Day22 where

import System.Environment
import System.IO
import Data.List.Split
import Data.List
import Data.Char
import GHC.Natural
import Data.Maybe
import Math.NumberTheory.Powers.Modular
import Math.NumberTheory.Moduli (modulo, SomeMod(..), getVal)

import qualified Data.Map as Map

data Function = Reverse | Cut SomeMod | Deal SomeMod
data State = State SomeMod SomeMod

run :: IO ()
run = do
    args <- getArgs
    file <- openFile (head args) ReadMode
    contents <- hGetContents file
    putStrLn (p1 contents)
    putStrLn (p2 contents)
    hClose file

type Deck = [Int]

p1 :: String -> String
p1 x = show $ fromJust $ elemIndex 2019 (foldl (flip p1_read) [0..10006] (lines x))

p1_read :: String -> (Deck -> Deck)
p1_read s | "deal into" `isInfixOf` s = p1_deal
          | "cut"       `isInfixOf` s = p1_cut ((read . last) (words s))
          | "deal with" `isInfixOf` s = p1_inc ((read . last) (words s))

p1_deal :: Deck -> Deck
p1_deal = reverse

p1_cut :: Int -> Deck -> Deck
p1_cut n d | n < 0     = p1_cut (n + length d) d
           | otherwise = (drop n d) ++ (take n d)

p1_inc :: Int -> Deck -> Deck
p1_inc n d = map snd $ Map.toList $ Map.fromList (zip (map (`mod` (length d)) [0, n..]) d)

p2 :: String -> String
p2 x = show $ p2_apply 2020 $ p2_replicate n l $
       foldl (p2_simplify) (State (1 `modulo` l) (0 `modulo` l)) (map (p2_read l) (lines x))
       where n = 101741582076661
             l = 119315717514047

p2_apply :: SomeMod -> State -> Integer
p2_apply i (State a b) = case a * i + b of
    SomeMod k -> getVal k
    _         -> error "impossible"

p2_simplify :: State -> Function -> State
p2_simplify (State a b) (Reverse) = State (-a) (b - a)
p2_simplify (State a b) (Cut c  ) = State a (b + c * a)
p2_simplify (State a b) (Deal c ) = State (a / c) b

p2_replicate :: Integer -> Natural -> State -> State
p2_replicate n l (State a b) = State (a ^ n) (b * (1 - (a ^ n)) / (1 - a))

p2_read :: Natural -> String -> Function
p2_read l s | "deal into" `isInfixOf` s = p2_deal l
            | "cut"       `isInfixOf` s = p2_cut  l ((read . last) (words s))
            | "deal with" `isInfixOf` s = p2_inc  l ((read . last) (words s))

p2_deal :: Natural -> Function
p2_deal l = Reverse

p2_cut :: Natural -> Integer -> Function
p2_cut l n = Cut (n `modulo` l)

p2_inc :: Natural -> Integer -> Function
p2_inc l n = Deal (n `modulo` l)