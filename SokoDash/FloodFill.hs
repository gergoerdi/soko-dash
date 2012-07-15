module SokoDash.FloodFill where

import Data.Array (Array, (!))
import qualified Data.Array as Array
import qualified Data.Array.ST as Array
import Data.Ix
import Control.Monad (replicateM_, forM_, when, unless)
import Data.Function (fix)

import Data.STRef

-- | Given a bitmap and a starting point, calculate all points
-- accessable from that starting point, not including the blocked points
fill :: (Num a, Num b, Ix a, Ix b) => Array (a, b) Bool -> (a, b) -> Array (a, b) Bool
fill arr p0 = Array.runSTArray $ do
    arr' <- Array.newArray bounds False
    Array.writeArray arr' p0 True
    fix $ \loop -> do
        changed <- newSTRef False
        forM_ (range bounds) $ \p -> do
            unless (arr!p) $ do
                bs <- mapM (Array.readArray arr') (neighbours p)
                b <- Array.readArray arr' p
                when (not b && or bs) $ do
                    writeSTRef changed True
                    Array.writeArray arr' p True
        changed <- readSTRef changed
        when changed loop
    return arr'
  where
    bounds = Array.bounds arr
    neighbours (x, y) = filter (inRange bounds) [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]
