module Lib
    ( run
    ) where

import System.Environment

run :: IO ()
run = do
    args <- getArgs
    putStrLn (head args)
