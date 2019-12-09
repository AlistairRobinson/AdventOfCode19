module IntComp where

import System.Environment
import System.IO
import Data.List.Split

data Instr = Instr {
    o :: Op,
    c :: Param,
    b :: Param,
    a :: Param
} deriving Show

data Program = Program {
    pc   :: Int,
    rel  :: Int,
    mem  :: [Int],
    args :: [Int]
} deriving Show

data Param = Addr {
    pos :: Int
} | Val {
    val :: Int
} | Rel {
    pos :: Int 
} | None deriving Show

data Result = Memory {
    state :: [Int],
    out   :: [Int],
    inp   :: [Int]
} | Jump {
    loc   :: Int
} | Shift {
    loc   :: Int
}

data Op = Add | Mult | In | Out | Stop | BrT | BrF | Lt | Eq | Mov deriving Show

next = Prelude.drop
set i v xs = take i xs ++ (v: next (i + 1) xs)

parse :: Int -> [Int] -> Instr
parse i []           = Instr Stop None None None
parse i (x:c:b:a:xs) = Instr (op x) (param x 1 c) (param x 10 b) (param x 100 a)
parse i (x:c:b:xs)   = Instr (op x) (param x 1 c) (param x 10 b)  None
parse i (x:c:xs)     = Instr (op x) (param x 1 c)  None           None
parse i (x:xs)       = Instr (op x)  None          None           None

param :: Int -> Int -> Int -> Param
param x y a = case (x `mod` (y * 1000)) `div` (y * 100) of
                  2 -> Rel  a
                  1 -> Val  a
                  _ -> Addr a

op :: Int -> Op
op x = case x `mod` 100 of
         1  -> Add
         2  -> Mult
         3  -> In
         4  -> Out
         5  -> BrT
         6  -> BrF
         7  -> Lt
         8  -> Eq
         9  -> Rel
         99 -> Stop

op_length :: Op -> Int
op_length Add  = 4
op_length Mult = 4
op_length Lt   = 4
op_length Eq   = 4
op_length BrT  = 3
op_length BrF  = 3
op_length In   = 2
op_length Out  = 2
op_length Rel  = 2
op_length Stop = 1

run_ic :: Int -> Int -> [Int] -> [Int] -> [Int]
run_ic n q mem a = case res of
                     Memory [] r as -> r
                     Memory m  r as -> r ++ (run_ic (n + op_length (o ins)) q m as)
                     Jump   l       -> run_ic l q mem a
                     Shift  l       -> run_ic (n + op_length (o ins)) l m as
                     where ins = parse n (drop n mem)
                           res = execute ins q mem a

run_prog :: Program -> [Int]
run_prog (Program n q mem a) = run_ic n mem a

yield_prog :: Program -> (Program, Int)
yield_prog (Program n q mem a) = case res of
        Memory [] r  as -> (Program n q [] as, 0)
        Memory m  [] as -> yield_prog (Program (n + op_length (o ins)) q m as)
        Memory m  r  as -> (Program (n + op_length (o ins)) q m as, head r)
        Jump   l        -> yield_prog (Program l q mem a)
        Shift  l        -> yield_prog (Program (n + op_length (o ins)) l m as)
        where ins = parse n (drop n mem)
              res = execute ins q mem a

execute :: Instr -> Int -> [Int] -> [Int] -> Result
execute (Instr Stop       _  _ _) q r i =
    Memory [] [] i
execute (Instr Add  c b (Addr a)) q r i =
    Memory (set a (eval b q r + eval c q r) r) [] i
execute (Instr Mult c b (Addr a)) q r i =
    Memory (set a (eval b q r * eval c q r) r) [] i
execute (Instr In   (Addr c) b a) q r i =
    Memory (set c (head i) r) [] (tail i)
execute (Instr Out         c b a) q r i =
    Memory r [eval c q r] i
execute (Instr BrT         c b a) q r i
    | eval c q r /= 0                   = Jump (eval b q r)
    | otherwise                         = Memory r [] i
execute (Instr BrF         c b a) q r i
    | eval c q r == 0                   = Jump (eval b q r)
    | otherwise                         = Memory r [] i
execute (Instr Lt   c b (Addr a)) q r i
    | eval c q r < eval b q r           = Memory (set a 1 r) [] i
    | otherwise                         = Memory (set a 0 r) [] i
execute (Instr Eq   c b (Addr a)) q r i
    | eval c q r == eval b q r          = Memory (set a 1 r) [] i
    | otherwise                         = Memory (set a 0 r) [] i
execute (Instr Mov         c b a) q r i = Shift (q + (eval c q r))

eval :: Param -> Int -> [Int] -> Int
eval (Addr  a) q ys = ys!!a
eval (Val   v) q ys = v
eval (Rel   s) q ys = ys!!(q + s) 