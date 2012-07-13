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
           | Space
           deriving Show

fieldToChar :: Field -> Char
fieldToChar Wall = '#'
fieldToChar Rock = '*'
fieldToChar Lambda = 'λ'
fieldToChar LambdaLift = 'L'
fieldToChar Earth = '.'
fieldToChar Space = ' '

charToField '#' = Wall
charToField '*' = Rock
charToField '\\' = Lambda
charToField 'L' = LambdaLift
charToField '.' = Earth
charToField ' ' = Space

type Pos = (Int, Int)
type World = Array Pos Field

data State = State{ stateWorld :: World
                  , statePos :: Pos
                  , stateLambdaCollected :: Int
                  }

instance Show State where
    show State{..} = unlines [worldMap, score]
      where
        worldMap = unlines . map (map fieldToChar') $ cells
        score = show stateLambdaCollected

        fieldToChar' = maybe 'R' fieldToChar

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
    fromChar x 'R' = (Space, Just x)
    fromChar _ c = (charToField c, Nothing)

parse :: [String] -> State
parse ss = State{ stateWorld = world
                , statePos = pos
                , stateLambdaCollected = 0
                }
  where
    (lines, Just pos) = second msum $ unzip $ zipWith line [1..] ss

    line :: Int -> String -> ([Field], Maybe Pos)
    line y = second (fmap (,y)) . parseLine
    world = fromLists lines

fromLists :: [[a]] -> Array Pos a
fromLists xss = Array.array ((1,1), (w, h))
                [ ((i,j), x) | (j, xs) <- zip [1..] xss,
                               (i, x) <- zip [1..] xs ]
  where
    h = length xss
    w = length $ head xss
