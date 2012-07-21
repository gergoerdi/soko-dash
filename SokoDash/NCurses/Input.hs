module SokoDash.NCurses.Input (Cmd(..), decodeEvent) where

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

decodeEvent :: Event -> Maybe Cmd
decodeEvent (EventCharacter ' ') = Just $ CmdInput Wait
decodeEvent (EventSpecialKey key) = CmdInput . Dir <$> decodeDirKey key
decodeEvent (EventCharacter c) | toLower c == 'q' = Just CmdExit
decodeEvent _ = Nothing
