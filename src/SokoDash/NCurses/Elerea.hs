module SokoDash.NCurses.Elerea
       ( network
       , RunState(..)
       ) where

import SokoDash.World
import SokoDash.Simulator

import FRP.Elerea.Simple
import SokoDash.NCurses.Utils

import Control.Applicative
import Data.Maybe

data RunState = Running State
              | Win
              | Lose

network :: State
        -> Signal (Maybe Input)
        -> SignalGen (Signal Bool)
        -> SignalGen (Signal RunState)
network s0 input clock = do
    timer <- clock
    transfer (Running s0) (uncurry step) ((,) <$> input <*> timer)
  where
    move :: Input -> RunState -> RunState
    move inp rs = case rs of
        Running s -> case processInput inp s of
            NewState s' -> Running s'
            InvalidInput -> Running s
            Finished n -> Win
        _ -> rs

    evolve :: RunState -> RunState
    evolve rs = case rs of
        Running s -> case simulate s of
            SimulateNewState s' -> Running s'
            SimulateDead n -> Lose

    step :: Maybe Input -> Bool -> RunState -> RunState
    step minp tick rs = rs''
      where
        rs' = move (Wait `fromMaybe` minp) rs
        rs'' = (if tick then evolve else id) rs'
