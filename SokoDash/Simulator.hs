{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module SokoDash.Simulator
       ( Dir(..), Input(..)
       , InputResult(..), processInput
       , SimulateResult(..), simulate
       ) where

import SokoDash.World

import Prelude hiding (Left, Right)

import Data.Ix
import Data.Array ((!), (//))
import qualified Data.Array as Array
import Control.Arrow (second, (&&&))
import Data.Maybe (mapMaybe)
import Control.Monad (guard)
import Control.Applicative ((<$>))

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

data InputResult = InvalidInput | NewState State | Finished Int

processInput :: Input -> State -> InputResult
processInput input s@State{..} = case input of
    Wait -> NewState s
    Dir dir -> case stateWorld!pos' of
        Empty -> NewState $ move s
        Earth -> NewState $ clear . move $ s
        Lambda -> NewState $ collect . clear . move $ s
        LambdaLift | stateLambdaRemaining == 0 -> Finished stateLambdaCollected
        Rock -> case dir of
            Left  | stateWorld!pos'' == Empty -> NewState $ push . clear . move $ s
            Right | stateWorld!pos'' == Empty -> NewState $ push . clear . move $ s
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
        push s@State{..} = s{ stateWorld = stateWorld // [(pos'', Rock)] }

data SimulateResult = SimulateNewState State | SimulateDead Int

simulate :: State -> SimulateResult
simulate s@State{..} = case simulateWorld statePos stateWorld of
    Just w -> SimulateNewState s{ stateWorld = w }
    Nothing -> SimulateDead stateLambdaCollected

simulateWorld :: Pos -> World -> Maybe World
simulateWorld pos w = Array.array b . (copy ++) <$> rocks
  where
    b = Array.bounds w
    xs = Array.assocs w

    removeRock :: Field -> Field
    removeRock Rock = Empty
    removeRock f = f

    copy :: [(Pos, Field)]
    copy = map (second removeRock) xs

    rocks :: Maybe [(Pos, Field)]
    rocks = sequence $ mapMaybe rockPos' xs
      where
        rockPos' (p, f) = case f of
            Rock ->
                let p' = rockPos p
                in Just $ (,Rock) <$> p'
            _ -> Nothing

    rockPos :: Pos -> Maybe Pos
    rockPos (x, y) | empty (x, y+1) = fall (x, y+1)
                   | otherwise = case w!(x, y+1) of
        Rock   | rollRight  -> fall (x+1, y+1)
               | rollLeft   -> fall (x-1, y+1)
        Lambda | rollRight  -> fall (x+1, y+1)
        _                   -> stay (x, y)
      where
        empty p = p /= pos && w!p == Empty

        rollRight = all empty [(x+1, y), (x+1, y+1)]
        rollLeft = all empty [(x-1, y), (x-1, y+1)]

        stay p = Just p
        fall (x, y) = do
            guard $ (x, y+1) /= pos
            return (x, y)
