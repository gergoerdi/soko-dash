{-# LANGUAGE RecordWildCards #-}
module SokoDash.AStar where

import SokoDash.World
import SokoDash.Simulator
import SokoDash.Prim

import Data.Function (on)
import qualified Data.Array as Array
import Data.Graph.AStar
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (mapMaybe, fromJust)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (guard)

import Debug.Trace

data Step = StepTo State | Win Int | Die Int
          deriving (Eq, Ord, Show)

data Move = Move Input Step
          deriving Show

moveStep :: Move -> Step
moveStep (Move _ step) = step

instance Eq Move where
    (==) = (==) `on` moveStep

instance Ord Move where
    compare = compare `on` moveStep

heuristic :: Step -> Int
heuristic (Die _) = 9999999
heuristic (Win _) = 0
heuristic (StepTo State{..}) = dists
  where
    dists | stateLambdaRemaining > 0 = manhattans $ statePos:stateLiftPos:lambdas
          | otherwise = rho statePos stateLiftPos
    lambdas :: [Pos]
    lambdas = map fst . filter ((== Lambda) . snd) . Array.assocs $ stateWorld

rho :: Pos -> Pos -> Int
rho (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

manhattans :: [Pos] -> Int
manhattans ps = sum . map (\(c, _, _) -> c) $ prim g
  where
    g = mkGraph False bounds edges'

    n = length ps
    bounds = (1, n)

    d :: Map Pos Int
    d = Map.fromList $ zip ps [1..n]

    encode :: Pos -> Int
    encode = fromJust . (Map.lookup `flip` d)

    edges =  [(p1, p2, rho p1 p2) | p1 <- ps, p2 <- ps, p1 < p2]
    edges' = map (\(p1, p2, c) -> (encode p1, encode p2, c)) edges

distance :: Step -> Step -> Int
distance (StepTo s1) (StepTo s2) = 1
distance (StepTo s) (Die _) = 999999
distance (StepTo s) (Win _) = 1

solve :: State -> [Input]
solve s0 = fixup steps
  where
    fixup :: Maybe [Move] -> [Input]
    fixup = maybe [] $ map f
      where
        f (Move inp _) = inp

    steps = aStar (neighbours . moveStep) (distance `on` moveStep) (heuristic . moveStep) (finished . moveStep) step0
    step0 = Move Wait $ StepTo s0

    neighbours (StepTo s) = Set.fromList $ mapMaybe f allInputs
      where
        f inp = fmap (Move inp) $ case processInput inp s of
            NewState s'    -> case simulate s' of
                SimulateNewState s'' -> do
                    guard $ s'' /= s
                    return $ StepTo s''
                SimulateDead n -> Just $ Die n
            Finished n     -> Just $ Win n
            InvalidInput   -> Nothing
    neighbours _ = Set.empty

    finished (Win _) = True
    finished (StepTo s) = False
    finished _ = False
