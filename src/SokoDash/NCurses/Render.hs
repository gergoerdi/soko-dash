{-# LANGUAGE RecordWildCards #-}
module SokoDash.NCurses.Render (renderState) where

import SokoDash.World

import Control.Monad (forM_)
import Data.Array (Array)
import qualified Data.Array as Array
import Data.Function

import UI.NCurses

renderState :: State -> Update ()
renderState State{..} = do
    forM_ (Array.assocs stateWorld) . uncurry $ \pos f -> do
        moveTo pos
        drawString [fieldToChar open f]
    moveTo statePos
    drawString "R"
  where
    open = stateLambdaRemaining == 0
    moveTo (x, y) = moveCursor (fromIntegral y) (fromIntegral x)
