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
    mem  :: [Int],
    args :: [Int]
} deriving Show

data Param = Addr {
    pos :: Int
} | Val {
    val :: Int
} | None deriving Show

data Result = Memory {
    state :: [Int],
    out   :: [Int],
    inp   :: [Int]
} | Jump {
    loc   :: Int
}

data Op = Add | Mult | In | Out | Stop | BrT | BrF | Lt | Eq deriving Show

next = Prelude.drop
set i v xs = take i xs ++ (v: next (i + 1) xs)

parse :: Int -> [Int] -> Instr
parse i []           = Instr Stop None None None
parse i (x:c:b:a:xs) = Instr (op x)
                        (if x `mod` 1000  >= 100   then Val c else Addr c)
                        (if x `mod` 10000 >= 1000  then Val b else Addr b)
                        (if x             >= 10000 then Val a else Addr a)
parse i (x:c:b:xs)   = Instr (op x)
                        (if x `mod` 1000  >= 100   then Val c else Addr c)
                        (if x `mod` 10000 >= 1000  then Val b else Addr b)
                        None
parse i (x:c:xs)     = Instr (op x)
                        (if x `mod` 1000  >= 100   then Val c else Addr c)
                        None None
parse i (x:xs)       = Instr (op x) None None None

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
op_length Stop = 1

run_ic :: Int -> [Int] -> [Int] -> [Int]
run_ic n mem a = case res of
                   Memory [] r as -> r
                   Memory m  r as -> r ++ (run_ic (n + op_length (o ins)) m as)
                   Jump   l       -> run_ic l mem a
                   where ins = parse n (drop n mem)
                         res = execute ins mem a

run_prog :: Program -> [Int]
run_prog (Program n mem a) = run_ic n mem a

yield_prog :: Program -> (Program, Int)
yield_prog (Program n mem a) = case res of
            Memory [] r  as -> (Program n [] as, 0)
            Memory m  [] as -> yield_prog (Program (n + op_length (o ins)) m as)
            Memory m  r  as -> (Program (n + op_length (o ins)) m as, head r)
            Jump   l        -> yield_prog (Program l mem a)
            where ins = parse n (drop n mem)
                  res = execute ins mem a

execute :: Instr -> [Int] -> [Int] -> Result
execute (Instr Stop       _  _ _) r i =
    Memory [] [] i
execute (Instr Add  c b (Addr a)) r i =
    Memory (set a (eval b r + eval c r) r) [] i
execute (Instr Mult c b (Addr a)) r i =
    Memory (set a (eval b r * eval c r) r) [] i
execute (Instr In   (Addr c) b a) r i =
    Memory (set c (head i) r) [] (tail i)
execute (Instr Out         c b a) r i =
    Memory r [eval c r] i
execute (Instr BrT         c b a) r i
    | eval c r /= 0                   = Jump (eval b r)
    | otherwise                       = Memory r [] i
execute (Instr BrF         c b a) r i
    | eval c r == 0                   = Jump (eval b r)
    | otherwise                       = Memory r [] i
execute (Instr Lt   c b (Addr a)) r i
    | eval c r < eval b r             = Memory (set a 1 r) [] i
    | otherwise                       = Memory (set a 0 r) [] i
execute (Instr Eq   c b (Addr a)) r i
    | eval c r == eval b r            = Memory (set a 1 r) [] i
    | otherwise                       = Memory (set a 0 r) [] i

eval :: Param -> [Int] -> Int
eval (Addr  a) ys = ys!!a
eval (Val v)   ys = v