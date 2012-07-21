{-# LANGUAGE RecordWildCards #-}
module SokoDash.NCurses.Render (renderState) where

import SokoDash.World

import Data.List (groupBy, transpose)
import Data.Maybe (fromJust)
import Control.Monad (zipWithM_)
import Data.Array (Array)
import qualified Data.Array as Array
import Data.Function

import UI.NCurses

renderState :: State -> Update ()
renderState State{..} = zipWithM_ `flip` [0..] `flip` cells $ \i row -> do
    moveCursor i 0
    drawString $ map fieldToChar' row
  where
    open = stateLambdaRemaining == 0

    -- TODO: clean this up...
    cells :: [[Maybe Field]]
    cells = transpose . strip . toRows $ stateWorld
      where
        toRows = groupBy ((==) `on` (fst . fst)) . map addRobot . Array.assocs
        addRobot (p, f) | p == statePos = (p, Nothing)
                        | otherwise = (p, Just f)
        strip = map (map snd)

    fieldToChar' = maybe 'R' (fieldToChar open)
