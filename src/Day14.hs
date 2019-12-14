module Day14 where

import System.Environment
import System.IO
import Data.List.Split
import Data.List

import qualified Data.Map as Map

type Reactions = Map.Map String (Chem, [Chem])
type Surplus = Map.Map String Int
type State = (Reactions, Int, Surplus)

data Chem = Chem {
    t :: String,
    n :: Int
} deriving (Show, Eq, Ord)

run :: IO ()
run = do
    args <- getArgs
    file <- openFile (head args) ReadMode
    contents <- hGetContents file
    putStrLn (p1 contents)
    putStrLn (p2 contents)
    hClose file

p1 :: String -> String
p1 x = show $ p1_create (Chem "FUEL" 1) (reactions, 0, Map.empty)
       where reactions = foldl p1_process (Map.empty) (splitOn "\n" x)

p1_process :: Reactions -> String -> Reactions
p1_process r s = Map.insert (t out) (out, req) r
    where inp = map ((map words) . splitOn ", ") (splitOn "=>" s)
          req = map (\(x:y:xs) -> Chem y (read x)) (head inp)
          out = head $ map (\(x:y:xs) -> Chem y (read x)) (last inp)

p1_create :: Chem -> State -> State
p1_create c@(Chem "ORE" n) (r, i, s) = (r, i + n, s)
p1_create c@(Chem t     n) (r, i, s) = foldr p1_create (r, i, s'')
    [(\(Chem t'' n'') -> Chem t'' (n'' * quant)) c | c <- reqs]
    where ((Chem _ n'), s')       = p1_subtract c s
          Just ((Chem _ m), reqs) = Map.lookup t r
          quant                   = n' `div` m + min (n' `mod` m) 1
          s''                     = Map.insertWith (-) t (-n') (Map.insertWith (+) t (-m * quant) s')

p1_subtract :: Chem -> Surplus -> (Chem, Surplus)
p1_subtract c@(Chem t n) s = case Map.lookup t s of
    Nothing -> (c, s)
    Just n' -> if n <= n' then (Chem t 0, Map.insertWith (-) t n s)
                          else (Chem t (n - n'), Map.delete t s)

p2 :: String -> String
p2 x = show $ p2_binary (reactions, 0, Map.empty) 10000000 1
       where reactions = foldl p1_process Map.empty (splitOn "\n" x)

p2_binary :: State -> Int -> Int -> Int
p2_binary s u l | u < l              = p2_binary s l u
                | d == u || d == l   = d
                | r >  1000000000000 = p2_binary s d l
                | r <= 1000000000000 = p2_binary s u d
                where d         = (u + l) `div` 2
                      (_, r, _) = p1_create (Chem "FUEL" d) s
