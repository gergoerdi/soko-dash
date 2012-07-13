{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module SokoDash.Simulator (simulate) where

import Data.Array (Array)
import qualified Data.Array as Array

import SokoDash.World

simulate :: State -> State
simulate s@State{..} = s{ stateWorld = simulateWorld statePos stateWorld }

simulateWorld :: Pos -> World -> World
simulateWorld (px, py) w = undefined
