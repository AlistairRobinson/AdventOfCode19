module IntComp where

import System.Environment
import System.IO
import Data.List.Split

-- | Instr
--   Represents a single IntCode instruction with an operation and a maximum of
--   three parameters.
data Instr = Instr {
    o :: Op,
    c :: Param,
    b :: Param,
    a :: Param
} deriving Show

-- | Program
--   Represents the state of execution of an IntCode program.
data Program = Program {
    pc   :: Int,
    mem  :: [Int],
    args :: [Int]
} deriving Show

-- | Param
--   Represents an operation parameter, either a memory address (Addr), a
--   constant value (Val), or nothing (None).
data Param = Addr {
    pos :: Int
} | Val {
    val :: Int
} | None deriving Show

-- | Result
--   Represents the result of evaluating an IntCode instruction, either a
--   new memory state (Memory) or a jump in the program counter (Jump).
data Result = Memory {
    state :: [Int],
    out   :: [Int],
    inp   :: [Int]
} | Jump {
    loc   :: Int
}

-- | Op
--   Represents all possible IntCode operations.
data Op = Add | Mult | In | Out | Stop | BrT | BrF | Lt | Eq deriving Show

-- | set
--   set takes an index `i`, a value `v` and a list `xs` and returns a new
--   list with `v` replacing the previous value at index `i` in `xs`.
set :: Int -> Int -> [Int] -> [Int]
set i v xs = take i xs ++ (v: drop (i + 1) xs)

-- | parse
--   parse takes a list of integers and parses the first valid IntCode
--   instruction with as many parameters as possible. Note that if a parameter
--   with fewer than the maximum number of arguments is parsed (e.g. Stop),
--   the computer ignores additional arguments during execution.
parse :: [Int] -> Instr
parse []           = Instr Stop None None None
parse (x:c:b:a:xs) = Instr (op x)
                        (if x `mod` 1000  >= 100   then Val c else Addr c)
                        (if x `mod` 10000 >= 1000  then Val b else Addr b)
                        (if x             >= 10000 then Val a else Addr a)
parse (x:c:b:xs)   = Instr (op x)
                        (if x `mod` 1000  >= 100   then Val c else Addr c)
                        (if x `mod` 10000 >= 1000  then Val b else Addr b)
                        None
parse (x:c:xs)     = Instr (op x)
                        (if x `mod` 1000  >= 100   then Val c else Addr c)
                        None None
parse (x:xs)       = Instr (op x) None None None

-- | op
--   op takes an integer `x` and returns the operation corresponding to `x`.
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

-- | op_len
--   op_len takes an Op and returns the size of its corresponding instruction.
op_len :: Op -> Int
op_len Add  = 4
op_len Mult = 4
op_len Lt   = 4
op_len Eq   = 4
op_len BrT  = 3
op_len BrF  = 3
op_len In   = 2
op_len Out  = 2
op_len Stop = 1

-- | run_ic
--   run_ic takes a program counter `n`, a memory state `mem` and a list of
--   arguments `a` and evaluates the IntCode program stored in `mem` until the
--   program halts, returning a list of outputs from the program.
--   Recommendation: don't call this function directly, use `run_prog` instead.
run_ic :: Int -> [Int] -> [Int] -> [Int]
run_ic n mem a = case res of
                   Memory [] r as -> r
                   Memory m  r as -> r ++ (run_ic (n + op_len (o ins)) m as)
                   Jump   l       -> run_ic l mem a
                   where ins = parse (drop n mem)
                         res = execute ins mem a

-- | run_prog
--   run_prog takes a program state and evaluates the IntCode program until it
--   halts, returning a list of outputs from the program.
run_prog :: Program -> [Int]
run_prog (Program n mem a) = run_ic n mem a

-- | yield_prog
--   yield_prog takes a program state and evaluates the IntCode program until
--   it outputs a value, at which point the computer will freeze the program's
--   execution and return its current state, along with the output. If the
--   program halts, the computer will return the state of the completed program
--   with the output `0`.
yield_prog :: Program -> (Program, Int)
yield_prog (Program n mem a) = case res of
            Memory [] r  as -> (Program n [] as, 0)
            Memory m  [] as -> yield_prog (Program (n + op_len (o ins)) m as)
            Memory m  r  as -> (Program (n + op_len (o ins)) m as, head r)
            Jump   l        -> yield_prog (Program l mem a)
            where ins = parse (drop n mem)
                  res = execute ins mem a

-- | execute
--   execute takes an IntCode instruction, a memory state `r` and a list of
--   inputs `i` and returns a Result representing the outcome of evaluating the
--   instruction.
--   Throws an exception if the program attempts to write a value to a
--   non-address parameter.
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

-- | eval
--   eval takes a parameter and a memory state and returns the value of
--   the parameter, either by looking up its value in the memory state
--   or simply unwrapping its constant value.
eval :: Param -> [Int] -> Int
eval (Addr  a) ys = ys!!a
eval (Val v)   ys = v