module IntComp where

import System.Environment
import System.IO
import Data.List.Split

import qualified Data.Map as Map

-- | Memory
--   Represents the memory state of an IntCode program as a map of data indexes
--   to data values, both of type Integer
type Memory = Map.Map Integer Integer

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
--   Represents the state of execution of an IntCode program: a program counter
--   `pc`, a relative address pointer `rel`, a memory state `mem` and a list of
--   input arguments `args`.
data Program = Program {
    pc   :: Integer,
    rel  :: Integer,
    mem  :: Memory,
    args :: [Integer]
} deriving Show

-- | Param
--   Represents an operation parameter, either a memory address (Addr), a
--   constant value (Val), or nothing (None).
data Param = Addr {
    pos :: Integer
} | Val {
    val :: Integer
} | Rel {
    pos :: Integer
} | None deriving Show

-- | Result
--   Represents the result of evaluating an IntCode instruction, either a
--   new memory state (State), a jump in the program counter (Jump) or a
--   shift in the position of the relative address pointer (Shift).
data Result = State {
    state :: Memory,
    out   :: [Integer],
    inp   :: [Integer]
} | Jump {
    loc   :: Integer
} | Shift {
    loc   :: Integer
} deriving Show

-- | Op
--   Represents all possible IntCode operations.
data Op = Add | Mult | In | Out | Stop | BrT | BrF | Lt | Eq | Mov deriving Show

-- | conv
--   Shorthand for converting from an ordered List to a Memory state
conv :: [Integer] -> Memory
conv xs = Map.fromList $ zip [0..] xs

-- | set
--   set takes an index `i`, a value `v` and a memory state `xs` and returns
--   a new state with `v` replacing the previous value in memory.
set :: Integer -> Integer -> Memory -> Memory
set i v xs = Map.insert i v xs

-- | get
--   get takes an index `i` and a memory state `xs` and returns the item stored
--   at index `i` in `xs` if one exists or 0 if such an item does not exist.
get :: Integer -> Memory -> Integer
get i xs = case Map.lookup i xs of
            Just v  -> v
            Nothing -> 0

-- | parse
--   parse takes a list of integers and parses the first valid IntCode
--   instruction with as many parameters as possible.
parse :: [Integer] -> Instr
parse (0:xs)       = Instr Stop    None          None           None
parse (x:c:b:a:xs) = Instr (op x) (param x 1 c) (param x 10 b) (param x 100 a)

param :: Integer -> Integer -> Integer -> Param
param x y a = case (x `mod` (y * 1000)) `div` (y * 100) of
                  2 -> Rel  a
                  1 -> Val  a
                  _ -> Addr a

-- | op
--   op takes an integer `x` and returns the operation corresponding to `x`.
op :: Integer -> Op
op x = case x `mod` 100 of
         1  -> Add
         2  -> Mult
         3  -> In
         4  -> Out
         5  -> BrT
         6  -> BrF
         7  -> Lt
         8  -> Eq
         9  -> Mov
         99 -> Stop

-- | op_len
--   op_len takes an Op and returns the size of its corresponding instruction.
op_len :: Op -> Integer
op_len Add  = 4
op_len Mult = 4
op_len Lt   = 4
op_len Eq   = 4
op_len BrT  = 3
op_len BrF  = 3
op_len In   = 2
op_len Out  = 2
op_len Mov  = 2
op_len Stop = 1

-- | run_ic
--   run_ic takes a program counter `n`, a relative address pointer `q`, a
--   memory state `mem` and a list of arguments `a` and evaluates the IntCode
--   program stored in `mem` until the program halts, returning a list of
--   outputs from the program.
--   Recommendation: don't call this function directly, use `run_prog` instead.

run_ic :: Integer -> Integer -> Memory -> [Integer] -> [Integer]
run_ic n q mem a = case res of
    State m r as | Map.null m -> r
                 | null r     -> run_ic (n + op_len (o ins)) q m as
                 | otherwise  -> r ++ (run_ic (n + op_len (o ins)) q m as)
    Jump   l    -> run_ic l q mem a
    Shift  l    -> run_ic (n + op_len (o ins)) l mem a
    where ins = parse [get ind mem | ind <- [n..n + 4]]
          res = execute ins q mem a

-- | run_prog
--   run_prog takes a program state and evaluates the IntCode program until it
--   halts, returning a list of outputs from the program.
run_prog :: Program -> [Integer]
run_prog (Program n q mem a) = run_ic n q mem a

-- | yield_prog
--   yield_prog takes a program state and evaluates the IntCode program until
--   it outputs a value, at which point the computer will freeze the program's
--   execution and return its current state, along with the output. If the
--   program halts, the computer will return the state of the completed program
--   with the output `0`.
yield_prog :: Program -> (Program, Integer)
yield_prog (Program n q mem a) = case res of
    State m r  as | Map.null m -> (Program n q (Map.empty) as, 0)
                  | null r     -> yield_prog (Program (n + op_len (o ins)) q m as)
                  | otherwise  -> (Program (n + op_len (o ins)) q m as, head r)
    Jump   l     -> yield_prog (Program l q mem a)
    Shift  l     -> yield_prog (Program (n + op_len (o ins)) l mem a)
    where ins = parse [get ind mem | ind <- [n..n + 4]]
          res = execute ins q mem a

-- | execute
--   execute takes an IntCode instruction, a relative address pointer `r`, a
--   memory state `r` and a list of inputs `i` and returns a Result
--   representing the outcome of evaluating the instruction.
execute :: Instr -> Integer -> Memory -> [Integer] -> Result
execute (Instr Stop _ _ _) q r i =
    State (Map.empty) [] i
execute (Instr Add  c b a) q r i =
    State (insert a (eval b q r + eval c q r) q r) [] i
execute (Instr Mult c b a) q r i =
    State (insert a (eval b q r * eval c q r) q r) [] i
execute (Instr In   c b a) q r i =
    State (insert c (head i) q r) [] (tail i)
execute (Instr Out  c b a) q r i =
    State r [eval c q r] i
execute (Instr BrT  c b a) q r i
    | eval c q r /= 0            = Jump (eval b q r)
    | otherwise                  = State r [] i
execute (Instr BrF  c b a) q r i
    | eval c q r == 0            = Jump (eval b q r)
    | otherwise                  = State r [] i
execute (Instr Lt   c b a) q r i
    | eval c q r < eval b q r    = State (insert a 1 q r) [] i
    | otherwise                  = State (insert a 0 q r) [] i
execute (Instr Eq   c b a) q r i
    | eval c q r == eval b q r   = State (insert a 1 q r) [] i
    | otherwise                  = State (insert a 0 q r) [] i
execute (Instr Mov  c b a) q r i = Shift (q + (eval c q r))

-- | eval
--   eval takes a parameter, a relative address pointer `q` and a memory state
--   `mem` and returns the value of the parameter, either by looking up its
--   value in the memory state or simply unwrapping its constant value.
eval :: Param -> Integer -> Memory -> Integer
eval (Addr  a) q ys = get a ys
eval (Val   v) q ys = v
eval (Rel   s) q ys = get (q + s) ys

-- | insert
--   insert takes an address parameter, a value `v`, a relative address pointer
--   `q` and a memory state `mem` and sets the item pointed to in memory to `v`
insert :: Param -> Integer -> Integer -> Memory -> Memory
insert (Addr a) v q xs = set  a      v xs
insert (Rel  s) v q xs = set (q + s) v xs