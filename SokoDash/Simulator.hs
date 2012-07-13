{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module SokoDash.Simulator (simulate) where

import SokoDash.World

import Data.Ix
import Data.Array ((!))
import qualified Data.Array as Array
import Control.Arrow (second, (&&&))
import Data.Maybe (mapMaybe)

simulate :: State -> State
simulate s@State{..} = s{ stateWorld = simulateWorld statePos stateWorld }

simulateWorld :: Pos -> World -> World
simulateWorld pos w = Array.array b $ copy ++ rocks
  where
    b = Array.bounds w
    xs = Array.assocs w

    removeRock :: Field -> Field
    removeRock Rock = Empty
    removeRock f = f

    copy :: [(Pos, Field)]
    copy = map (second removeRock) xs

    rocks :: [(Pos, Field)]
    rocks = mapMaybe (fmap (, Rock) . rockPos') xs
      where
        rockPos' (p, f) = case f of
            Rock -> Just (rockPos p)
            _ -> Nothing

    rockPos :: Pos -> Pos
    rockPos (x, y) | empty (x, y+1) = (x, y+1)
                   | otherwise = case w!(x, y+1) of
        Rock   | rollRight  -> (x+1, y+1)
               | rollLeft   -> (x-1, y+1)
        Lambda | rollRight  -> (x+1, y+1)
        _                   -> (x, y)
      where
        empty p = p /= pos && w!p == Empty

        rollRight = all empty [(x+1, y), (x+1, y+1)]
        rollLeft = all empty [(x-1, y), (x-1, y+1)]
