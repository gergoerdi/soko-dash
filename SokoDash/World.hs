{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module SokoDash.World
       ( Field(..), World, Pos, State(..)
       , parse
       ) where

import Data.Array (Array)
import qualified Data.Array as Array
import Control.Arrow (second)
import Control.Monad (msum)
import Data.Function (on)
import Data.List (groupBy, transpose)

data Field = Wall
           | Rock
           | Lambda
           | LambdaLift
           | Earth
           | Empty
           deriving (Eq, Show)

fieldToChar :: Bool -> Field -> Char
fieldToChar _ Wall = '#'
fieldToChar _ Rock = '*'
fieldToChar _ Lambda = 'λ'
fieldToChar False LambdaLift = 'L'
fieldToChar True LambdaLift = 'O'
fieldToChar _ Earth = '.'
fieldToChar _ Empty = ' '

charToField '#' = Wall
charToField '*' = Rock
charToField '\\' = Lambda
charToField 'L' = LambdaLift
charToField '.' = Earth
charToField ' ' = Empty

type Pos = (Int, Int)
type World = Array Pos Field

data State = State{ stateWorld :: World
                  , statePos :: Pos
                  , stateLambdaRemaining, stateLambdaCollected :: Int
                  }

instance Show State where
    show State{..} = unlines [worldMap, score]
      where
        worldMap = unlines . map (map (fieldToChar' $ stateLambdaRemaining == 0)) $ cells
        score = show stateLambdaCollected

        fieldToChar' open = maybe 'R' (fieldToChar open)

        cells :: [[Maybe Field]]
        cells = transpose . strip . toRows $ stateWorld
          where
            toRows = groupBy ((==) `on` (fst . fst)) . map addRobot . Array.assocs
            addRobot (p, f) | p == statePos = (p, Nothing)
                            | otherwise = (p, Just f)
            strip = map (map snd)

parseLine :: String -> ([Field], Maybe Int)
parseLine = second msum . unzip . zipWith fromChar [1..]
  where
    fromChar x 'R' = (Empty, Just x)
    fromChar _ c = (charToField c, Nothing)

parse :: String -> State
parse s = State{ stateWorld = world
               , statePos = pos
               , stateLambdaRemaining = sum . map (count (== Lambda)) $ rows
               , stateLambdaCollected = 0
               }
  where
    ss :: [String]
    ss = lines s

    (rows, Just pos) = second msum $ unzip $ zipWith row [1..] ss

    row :: Int -> String -> ([Field], Maybe Pos)
    row y = second (fmap (,y)) . parseLine
    world = fromLists rows

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

fromLists :: [[a]] -> Array Pos a
fromLists xss = Array.array ((1,1), (w, h))
                [ ((i,j), x) | (j, xs) <- zip [1..] xss,
                               (i, x) <- zip [1..] xs ]
  where
    h = length xss
    w = length $ head xss
