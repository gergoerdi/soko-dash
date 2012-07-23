module SokoDash.NCurses.Utils
       ( sampleInput
       -- , runElerea
       , mkClock
       , driveNetwork
       ) where

import FRP.Elerea.Simple
import UI.NCurses

import Data.Function
import Control.Monad
import Control.Monad.Trans
import Control.Applicative
import Control.Concurrent (threadDelay, forkIO)

sampleInput :: Window -> (Maybe Event -> IO a) -> Curses a
sampleInput w pushInput = do
    liftIO yield
    liftIO . pushInput =<< getEvent w (Just 0)
  where
    yield = threadDelay 0

driveNetwork :: (MonadIO m) => IO (m ()) -> m Bool -> m ()
driveNetwork network clock = fix $ \loop -> do
    finished <- clock
    if finished
      then return ()
      else join (liftIO network) >> loop

runElerea :: Window -> (Signal (Maybe Event) -> SignalGen (Signal (Update ()))) -> Curses ()
runElerea w driver = do
    (input, pushInput) <- liftIO $ external Nothing
    network <- liftIO $ start (driver input)
    driveNetwork (update <$> network) (const False <$> (sampleInput w pushInput))
  where
    update u = updateWindow w u >> render

mkClock :: Double -> IO (SignalGen (Signal Bool))
mkClock seconds = do
    (clock, tick_) <- externalMulti
    let tick = tick_ ()
    forkIO $ forever $ do
        threadDelay μs
        tick
    return $ fmap (not . null <$>) clock
  where
    μs = round $ seconds * 10^6
