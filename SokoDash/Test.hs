module SokoDash.Test where

import Prelude hiding (Left, Right)
import SokoDash.World
import SokoDash.Simulator

import Control.Applicative
import System.IO
import System.Environment

import Data.Function (fix)
import Data.Char (toUpper)

main = do
    [infile] <- getArgs
    s <- parse <$> readFile infile

    flip fix s $ \loop s -> do
        print s
        inp <- getLine
        print inp
        let input = case map toUpper inp of
                "W" -> Dir Up
                "A" -> Dir Left
                "S" -> Dir Down
                "D" -> Dir Right
                _ -> Wait
        case processInput input s of
            NewState s' -> loop $ simulate s'
            Finished n -> putStrLn $ unwords ["Finished with", show n, "lambdas"]
            InvalidInput -> putStrLn "You can't get ye flask!" >> loop s

