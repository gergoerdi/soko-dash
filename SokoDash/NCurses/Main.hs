import SokoDash.NCurses.Render
import SokoDash.World
import SokoDash.Simulator

import Data.Function
import Data.Char

import UI.NCurses

import Control.Applicative
import System.IO
import System.Environment

data Cmd = CmdInput Input
         | CmdExit

decodeDirKey :: Key -> Maybe Dir
decodeDirKey key = case key of
    KeyUpArrow -> Just DUp
    KeyDownArrow -> Just DDown
    KeyLeftArrow -> Just DLeft
    KeyRightArrow -> Just DRight
    _ -> Nothing

decodeInput :: Event -> Maybe Cmd
decodeInput (EventCharacter ' ') = Just $ CmdInput Wait
decodeInput (EventSpecialKey key) = CmdInput . Dir <$> decodeDirKey key
decodeInput (EventCharacter c) | toLower c == 'q' = Just CmdExit
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

            minput <- waitFor w decodeInput
            case minput of
                CmdExit -> return ()
                CmdInput input -> case processInput input s of
                    NewState s'    -> case simulate s' of
                        SimulateNewState s'' -> loop s''
                        SimulateDead n -> die n
                    Finished n     -> win n
                    InvalidInput   -> loop s

waitFor :: Window -> (Event -> Maybe a) -> Curses a
waitFor w decode = fix $ \loop -> do
    ev <- getEvent w Nothing
    maybe loop return $ decode =<< ev
