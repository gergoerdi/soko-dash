{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module SokoDash.Simulator
       ( Dir(..), Input(..)
       , InputResult(..), processInput, checkIfBroken
       , simulate
       ) where

import SokoDash.World

import Prelude hiding (Left, Right)

import Data.Ix
import Data.Array ((!), (//))
import qualified Data.Array as Array
import Control.Arrow (second, (&&&))
import Data.Maybe (mapMaybe)

import Debug.Trace

data Dir = Left | Right | Up | Down
data Input = Dir Dir | Wait

moveDir :: Dir -> (Int, Int) -> (Int, Int)
moveDir d (x, y) = (x + dx, y + dy)
  where
    (dx, dy) = case d of
        Left  -> (-1, 0)
        Right -> (1, 0)
        Up    -> (0, -1)
        Down  -> (0, 1)

data InputResult = InvalidInput | NewState State | Finished Int | BrokenRobot State

processInput :: Input -> State -> InputResult
processInput input s@State{..} = case input of
    Wait -> NewState s
    Dir dir -> case stateWorld!pos' of
        Empty -> NewState $ move s
        Earth -> NewState $ clear . move $ s
        Lambda -> NewState $ collect . clear . move $ s
        LambdaLift | stateLambdaRemaining == 0 -> Finished stateLambdaCollected
        Rock f -> case dir of
            Left  | stateWorld!pos'' == Empty -> NewState $ push f . clear . move $ s
            Right | stateWorld!pos'' == Empty -> NewState $ push f . clear . move $ s
            _ -> InvalidInput
        _ -> InvalidInput
      where
        pos' = moveDir dir statePos
        pos'' = moveDir dir pos'
        move s = s{ statePos = pos' }
        clear s@State{..} = s{ stateWorld = stateWorld // [(pos', Empty)] }
        collect s = s{ stateLambdaRemaining = pred stateLambdaRemaining
                     , stateLambdaCollected = succ stateLambdaCollected
                     }
        push f s@State{..} = s{ stateWorld = stateWorld // [(pos'', Rock f)] }

checkIfBroken :: InputResult -> InputResult
checkIfBroken r@(NewState s) =
    case stateWorld!(moveDir Up statePos) of
        Rock True -> BrokenRobot s'
        _         -> r
    where s'@State{..} = simulate s
checkIfBroken r = r

simulate :: State -> State
simulate s@State{..} = s{ stateWorld = simulateWorld statePos stateWorld }

simulateWorld :: Pos -> World -> World
simulateWorld pos w = Array.array b $ copy ++ rocks
  where
    b = Array.bounds w
    xs = Array.assocs w

    removeRock :: Field -> Field
    removeRock (Rock _) = Empty
    removeRock f = f

    copy :: [(Pos, Field)]
    copy = map (second removeRock) xs

    rocks :: [(Pos, Field)]
    rocks = mapMaybe rockPos' xs
      where
        rockPos' (p, f) = case f of
            Rock _ ->
                let p' = rockPos p
                in  Just (p', if p == p' then Rock False else Rock True)
            _ -> Nothing

    rockPos :: Pos -> Pos
    rockPos (x, y) | empty (x, y+1) = (x, y+1)
                   | otherwise = case w!(x, y+1) of
        Rock _ | rollRight  -> (x+1, y+1)
               | rollLeft   -> (x-1, y+1)
        Lambda | rollRight  -> (x+1, y+1)
        _                   -> (x, y)
      where
        empty p = p /= pos && w!p == Empty

        rollRight = all empty [(x+1, y), (x+1, y+1)]
        rollLeft = all empty [(x-1, y), (x-1, y+1)]
