module Main where

import Prelude hiding (Left, Right)
import SokoDash.World
import SokoDash.Simulator
import SokoDash.AStar

import Control.Applicative

main = do
    s <- parse . unlines . takeWhile (not . null) . lines <$> getContents

    let inps = solve s
    putStrLn $ concatMap show inps
