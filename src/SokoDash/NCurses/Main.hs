import SokoDash.NCurses.Render
import SokoDash.NCurses.Input

import SokoDash.World
import SokoDash.Simulator

import SokoDash.NCurses.Elerea
import SokoDash.NCurses.Utils
import FRP.Elerea.Simple

import Data.Function
import Data.Char

import UI.NCurses

import Control.Applicative
import System.IO
import Control.Monad.Trans
import System.Environment

main :: IO ()
main = do
    [infile] <- getArgs
    s0 <- parse . unlines . takeWhile (not . null) . lines <$> readFile infile

    clock <- mkClock 0.5

    (input, pushInput) <- external Nothing
    runCurses $ do
        setEcho False
        setCursorMode CursorInvisible

        w <- defaultWindow
        let runUpdate u = updateWindow w u >> render

        let inp = sampleInput w $ \mev -> do
                case decodeEvent =<< mev of
                    Nothing -> pushInput Nothing >> return False
                    Just CmdExit -> return True
                    Just (CmdInput inp) -> pushInput (Just inp) >> return False

        game <- liftIO . start $ network s0 input clock
        driveNetwork (runUpdate . renderRunState <$> game) inp
  where
    renderRunState (Running s) = renderState s
    renderRunState _ = undefined
