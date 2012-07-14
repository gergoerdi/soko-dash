{-# OPTIONS_GHC -main-is SokoDash.Test #-}
module SokoDash.Test where

import Prelude hiding (Left, Right)
import SokoDash.World
import SokoDash.Simulator
import SokoDash.AStar

import Control.Applicative
import System.IO
import System.Environment

import Data.Function (fix)
import Data.Char (toUpper)

main = do
    [infile] <- getArgs
    s <- parse <$> readFile infile

    let inps = solve s
    putStrLn $ concatMap show inps

{-
    flip fix s $ \loop s -> do
        print s
        inp <- getLine
        let input = case map toUpper inp of
                "W" -> Dir Up
                "A" -> Dir Left
                "S" -> Dir Down
                "D" -> Dir Right
                _ -> Wait
        case processInput input s of
            NewState s'    -> case simulate s' of
                SimulateNewState s'' -> loop s''
                SimulateDead n -> putStrLn $ unwords ["Dead with", show n, "lambdas"]
            Finished n     -> putStrLn $ unwords ["Finished with", show n, "lambdas"]
            InvalidInput   -> putStrLn "You can't get ye flask!" >> loop s

-}
