module SokoDash.Fixed (fixed, showFixed) where

import SokoDash.World

import Data.Array (Array, (!))
import qualified Data.Array as Array
import Data.Ix
import Control.Arrow ((&&&))
import Data.List (transpose, groupBy)
import Data.Function (on)

import Debug.Trace

data Fixedness = Fixed
               | Movable
               | NonFixed

fixed :: Bool -> World -> Array Pos Bool
fixed open w = arr
  where
    arr :: Array Pos Bool
    arr = Array.array (Array.bounds w) $ map (id &&& f) $ range (Array.bounds w)

    f pos@(x, y) = case fixedness pos of
        Fixed -> True
        Movable -> blocking (x-1, y) || blocking (x+1, y)
        NonFixed -> False

    supported :: Pos -> Bool
    supported (x, y) = arr!(x, y+1)

    blocking :: Pos -> Bool
    blocking pos = case fixedness pos of
        Fixed -> True
        Movable -> supported pos
        NonFixed -> False

    fixedness :: Pos -> Fixedness
    fixedness pos = case w!pos of
        Wall -> Fixed
        LambdaLift -> if open then NonFixed else Fixed
        Closure -> Movable
        Rock -> Movable
        _ -> NonFixed

showFixed :: Array Pos Bool -> String
showFixed = unlines . map (map toChar) . toLists
  where
    toChar True = '#'
    toChar False = ' '

    toRows = groupBy ((==) `on` (fst . fst)) . Array.assocs
    toLists = transpose . strip . toRows
      where
        strip = map (map snd)
