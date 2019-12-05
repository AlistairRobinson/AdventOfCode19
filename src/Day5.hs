module Day5 where

import System.Environment
import System.IO
import Data.List.Split

data Instr = Instr {
    o :: Op,
    a :: Param,
    b :: Param,
    c :: Param,
    i :: Int
} deriving Show

data Param = Addr {
    pos  :: Int
} | Val {
    val :: Int
} | None deriving Show

data Op = Add | Mult | In | Out | Stop deriving Show

next = Prelude.drop
set i v xs = take i xs ++ (v: (next (i + 1) xs))

run :: IO ()
run = do
    args <- getArgs
    file <- openFile (head args) ReadMode
    contents <- hGetContents file
    putStr (p1 contents ++ "\n")
    -- putStr (p2 contents ++ "\n")
    hClose file

p1 :: String -> String
p1 x = show $ p1_run instr 0 input
       where input = map read $ splitOn "," x
             instr = p1_process 0 input

p1_process :: Int -> [Int] -> [Instr]
p1_process i []           = []
p1_process i (x:c:b:a:xs) = p1_instr (Instr (p1_op x)
                            (if x             >= 10000 then Val a else Addr a)
                            (if x `mod` 10000 >= 1000  then Val b else Addr b)
                            (if x `mod` 1000  >= 100   then Val c else Addr c)
                            i) (x:c:b:a:xs)
p1_process i (x:a:xs)     = p1_instr (Instr (p1_op x)
                            (if x `mod` 1000  >= 100   then Val a else Addr a)
                            None None i) (x:a:xs)
p1_process i (x:xs)       = p1_instr (Instr (p1_op x) None None None
                            i) (x:xs)

p1_instr :: Instr -> [Int] -> [Instr]
p1_instr instr@(Instr Add  a b c i) xs = instr:(p1_process (i + 4) (next 4 xs))
p1_instr instr@(Instr Mult a b c i) xs = instr:(p1_process (i + 4) (next 4 xs))
p1_instr instr@(Instr In   a b c i) xs = instr:(p1_process (i + 2) (next 2 xs))
p1_instr instr@(Instr Out  a b c i) xs = instr:(p1_process (i + 2) (next 2 xs))
p1_instr instr@(Instr Stop a b c i) xs = instr:(p1_process (i + 1) (next 1 xs))

p1_op :: Int -> Op
p1_op x | x `mod` 100 == 1  = Add
        | x `mod` 100 == 2  = Mult
        | x `mod` 100 == 3  = In
        | x `mod` 100 == 4  = Out
        | x `mod` 100 == 99 = Stop
        | otherwise         = Stop

p1_length :: Op -> Int
p1_length Add  = 4
p1_length Mult = 4
p1_length In   = 2
p1_length Out  = 2
p1_length Stop = 1

p1_run :: [Instr] -> Int -> [Int] -> [Int]
p1_run []  _ _     = []
p1_run ins n input = if (n < length ins) then (snd res) ++
                     (p1_run (p1_process 0 (fst res))
                     (n + (i (ins!!n) + 1)) (fst res))
                     else []
                     where res = p1_execute (ins!!n) input

p1_execute :: Instr -> [Int] -> ([Int], [Int])
p1_execute (Instr Stop       _  _ _ _) r = ([], [])
p1_execute (Instr Add  a b (Addr c) i) r =
    (set c (p1_eval b r + p1_eval a r) r, [])
p1_execute (Instr Mult a b (Addr c) i) r =
    (set c (p1_eval b r * p1_eval a r) r, [])
p1_execute (Instr In   (Addr a) b c i) r =
    (set a (1) r, [])
p1_execute (Instr Out        a  b c i) r =
    (r, [p1_eval a r])

p1_eval :: Param -> [Int] -> Int
p1_eval (Addr  a) ys = ys!!a
p1_eval (Val v)   ys = v