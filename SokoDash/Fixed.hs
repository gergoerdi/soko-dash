module SokoDash.Fixed (fixed, showFixed) where

import SokoDash.World

import Data.Array (Array, (!))
import Data.Array as Array
import Control.Arrow ((&&&))
import Data.List (transpose, groupBy)
import Data.Function (on)

import Debug.Trace

fixed :: World -> Array Pos Bool
fixed w = arr
  where
    arr :: Array Pos Bool
    arr = Array.array (Array.bounds w) $ map (id &&& f) $ range (Array.bounds w)

    f pos@(x, y) = case w!pos of
        Wall -> True
        LambdaLift -> True
        Rock -> supported pos && (blocking (x-1, y) || blocking (x+1, y))
        _ -> False

    supported :: Pos -> Bool
    supported (x, y) = arr!(x, y+1)

    blocking :: Pos -> Bool
    blocking pos = case w!pos of
        Wall -> True
        LambdaLift -> True
        Rock -> supported pos
        _ -> False

showFixed :: Array Pos Bool -> String
showFixed = unlines . map (map toChar) . toLists
  where
    toChar True = '#'
    toChar False = ' '

    toRows = groupBy ((==) `on` (fst . fst)) . Array.assocs
    toLists = transpose . strip . toRows
      where
        strip = map (map snd)
