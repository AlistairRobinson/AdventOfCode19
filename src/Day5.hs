module Day5 where

import System.Environment
import System.IO
import Data.List.Split

data Instr = Instr {
    o :: Op,
    c :: Param,
    b :: Param,
    a :: Param,
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
p1 x = show $ p1_run 0 input
       where input = map read $ splitOn "," x

p1_parse :: Int -> [Int] -> Instr
p1_parse i []           = Instr Stop None None None i
p1_parse i (x:c:b:a:xs) = Instr (p1_op x)
                          (if x `mod` 1000  >= 100   then Val c else Addr c)
                          (if x `mod` 10000 >= 1000  then Val b else Addr b)
                          (if x             >= 10000 then Val a else Addr a) i
p1_parse i (x:c:xs)     = Instr (p1_op x) None None
                          (if x `mod` 1000  >= 100   then Val c else Addr c)
                          i
p1_parse i (x:xs)       = Instr (p1_op x) None None None i

p1_op :: Int -> Op
p1_op x | x `mod` 100 == 1  = Add
        | x `mod` 100 == 2  = Mult
        | x `mod` 100 == 3  = In
        | x `mod` 100 == 4  = Out
        | x `mod` 100 == 99 = Stop

p1_length :: Op -> Int
p1_length Add  = 4
p1_length Mult = 4
p1_length In   = 2
p1_length Out  = 2
p1_length Stop = 1

p1_run :: Int -> [Int] -> [Int]
p1_run n input | fst res == [] = []
               | otherwise     = (snd res) ++
                 (p1_run (n + p1_length (o instr)) (fst res))
                 where instr  = p1_parse n (drop n input)
                       res    = p1_execute instr input

p1_execute :: Instr -> [Int] -> ([Int], [Int])
p1_execute (Instr Stop       _  _ _ _) r = ([], [])
p1_execute (Instr Add  c b (Addr a) i) r =
    (set a (p1_eval b r + p1_eval c r) r, [])
p1_execute (Instr Mult c b (Addr a) i) r =
    (set a (p1_eval b r * p1_eval c r) r, [])
p1_execute (Instr In   c b (Addr a) i) r =
    (set a (1) r, [])
p1_execute (Instr Out  c b a i) r =
    (r, [p1_eval c r])

p1_eval :: Param -> [Int] -> Int
p1_eval (Addr  a) ys = ys!!a
p1_eval (Val v)   ys = v