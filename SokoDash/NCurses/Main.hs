import SokoDash.NCurses.Render
import SokoDash.World
import SokoDash.Simulator

import Data.Function
import Data.Char

import UI.NCurses

import Control.Applicative
import System.IO
import System.Environment

decodeInput :: Event -> Maybe Input
decodeInput (EventCharacter ' ') = Just Wait
decodeInput (EventSpecialKey KeyUpArrow) = Just $ Dir DUp
decodeInput (EventSpecialKey KeyDownArrow) = Just $ Dir DDown
decodeInput (EventSpecialKey KeyLeftArrow) = Just $ Dir DLeft
decodeInput (EventSpecialKey KeyRightArrow) = Just $ Dir DRight
decodeInput _ = Nothing

main :: IO ()
main = do
    [infile] <- getArgs
    s <- parse . unlines . takeWhile (not . null) . lines <$> readFile infile

    runCurses $ do
        setEcho False
        w <- defaultWindow

        let redraw s = do
                updateWindow w $ do
                    renderState s
                    moveCursor 0 0
                render
            die n = return ()
            win n = return ()

        flip fix s $ \loop s -> do
            redraw s

            input <- waitFor w decodeInput
            case processInput input s of
                NewState s'    -> case simulate s' of
                    SimulateNewState s'' -> loop s''
                    SimulateDead n -> die n
                Finished n     -> win n
                InvalidInput   -> loop s

waitFor :: Window -> (Event -> Maybe a) -> Curses a
waitFor w decode = fix $ \loop -> do
    ev <- getEvent w Nothing
    maybe loop return $ decode =<< ev
