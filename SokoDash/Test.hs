module SokoDash.Test where

import SokoDash.World

import Control.Applicative
import System.IO
import System.Environment

main = do
    [infile] <- getArgs
    s <- parse <$> readFile infile
    print s
