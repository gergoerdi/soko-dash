module Main where

import Prelude hiding (Left, Right)
import SokoDash.World
import SokoDash.Simulator
import SokoDash.AStar

import Control.Applicative
import System.Environment

main = do
    [infile] <- getArgs
    s <- parse . unlines . takeWhile (not . null) . lines <$> getContents

    let inps = solve s
    putStrLn $ concatMap show inps
