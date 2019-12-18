module Day18 where

import System.Environment
import System.IO
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Char

import qualified Data.Map as Map
import qualified Data.Set as Set

import IntComp

type Point   = (Integer, Integer)
type Journey = (Set.Set Point, [Point])
type Space   = Map.Map Point Item
type KeyPath = Map.Map (Item, Item) Walk
type Cache   = (Int, Map.Map (Set.Set Item, Item) Int)

data Maze = Maze {
    space :: Space,
    keys  :: [Point],
    doors :: [Point],
    start :: Point
} deriving Show

data Walk = Walk {
    dist :: Int,
    obst :: Set.Set Item
} deriving (Show, Eq, Ord)

data Direction = N | E | S | W                 deriving (Eq, Show)
data Item = Wall | Path | Key Char | Door Char deriving (Eq, Ord)

instance Show Item where
    show (Wall)   = "█"
    show (Path)   = " "
    show (Key  c) = [c]
    show (Door c) = [c]

run :: IO ()
run = do
    args <- getArgs
    file <- openFile (head args) ReadMode
    contents <- hGetContents file
    putStrLn (p1 contents)
    -- putStrLn (p2 contents)
    hClose file

p1 :: String -> String
p1 x = show $ p1_move keyp (Set.delete (Key '@') keys) (Key '@') (maxBound, Map.empty)
       where maze = Map.fromList (zip [(x, y) | y <- [0..81], x <- [0..81]]
                    (map p1_item $ filter (/= '\n') x))
             keyp = foldl Map.union Map.empty [p1_explore maze k (Set.empty, [p])
                    (Walk 0 Set.empty) (Map.empty) | (p, k) <- p1_keypoints maze]
             keys = Set.fromList $ map snd (p1_keypoints maze)

p1_move :: KeyPath -> Set.Set Item -> Item -> Cache -> Cache
p1_move m r k c | Set.null r = (0, snd c)
                | otherwise  = case (Map.lookup (r, k) (snd c)) of
                    Just n  -> c
                    Nothing -> foldl' (\z a -> (min (fst z)
                               (fst (run a z) + dist (walk k a)),
                               (Map.insert (r, a) (fst (run a z)) (snd (run a z))))) c
                               (Set.filter (\i -> r `Set.disjoint` obst (walk k i)) r)
                    where walk k i = fromJust $ Map.lookup (k, i) m
                          run a z  = p1_move m (Set.delete a r) a z

p1_keypoints :: Space -> [(Point, Item)]
p1_keypoints s = [(p, p1_lookup p s) | p <- Map.keys s, p1_match (p1_lookup p s) (Key '#')]

p1_explore :: Space -> Item -> Journey -> Walk -> KeyPath -> KeyPath
p1_explore s k (e, []) (Walk n d) p = p
p1_explore s k (e, u)  (Walk n d) p = foldl (Map.unionWith (min)) p
    [p1_explore s k (Set.insert q e, [p1_step q d | d <- [N, S, W, E],
                                      p1_lookup (p1_step q d) s `notElem` [Wall],
                                      p1_step q d `Set.notMember` e])
     (Walk (n + 1) (if p1_match (p1_lookup q s) (Door '#')
                    || p1_match (p1_lookup q s) (Key  '#')
                    then Set.insert (p1_standard (p1_lookup q s)) d
                    else d))
     (if (p1_match (p1_lookup q s) (Key '#')) && (k /= (p1_lookup q s))
     then Map.insertWith (min) (k, p1_lookup q s) (Walk n d) p else p) | q <- u]

p1_standard :: Item -> Item
p1_standard (Key  x) = Key x
p1_standard (Door c) = Key (toLower c)

p1_match :: Item -> Item -> Bool
p1_match (Door _) (Door _) = True
p1_match (Key  _) (Key  _) = True
p1_match  _        _       = False

p1_item :: Char -> Item
p1_item '#' = Wall
p1_item '.' = Path
p1_item '@' = Key '@'
p1_item c   | isUpper c = Door c
            | isLower c = Key  c
            | otherwise = Path

p1_lookup :: Point -> Space -> Item
p1_lookup p s = case Map.lookup p s of
    Just i  -> i
    Nothing -> Path

p1_search :: Point -> Space -> [Item]
p1_search p s = [p1_lookup (p1_step p d) s | d <- [N, S, W, E]]

p1_step :: Point -> Direction -> Point
p1_step (x, y) N = (x, y - 1)
p1_step (x, y) E = (x + 1, y)
p1_step (x, y) S = (x, y + 1)
p1_step (x, y) W = (x - 1, y)

p1_display :: Space -> String
p1_display s = intercalate "\n" $ chunksOf 82 $
               concat [show (p1_lookup (x, y) s)
                      | y <- [0..81], x <- [0..81]] ++ "\n"
{-
p2 :: String -> String
p2 x = show $ p2_flood s [p] 0
       where prog   = conv $ map read $ splitOn "," x
             (s, p) = p1_run (Program 0 0 prog []) (0, 0) (Map.empty)

p2_flood :: Space -> [Point] -> Int -> Int
p2_flood s [] i = i - 1
p2_flood s p  i = p2_flood (foldl p2_insert s p) p' (i + 1)
                  where p' = [p1_step q d | q <- p, d <- [N, S, W, E],
                              p1_lookup (p1_step q d) s `elem` [Dead, Path]]

p2_insert :: Space -> Point -> Space
p2_insert s p = Map.insert p O2 s
-}