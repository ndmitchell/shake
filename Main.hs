
module Main where

import Data.Maybe
import System.Environment

import qualified Examples.Tar.Main as Tar
import qualified Examples.Self.Main as Self
import qualified Examples.Test.Basic1 as Basic1


tests = ["tar" * Tar.main, "self" * Self.main, "basic1" * Basic1.main]
    where (*) = (,)


main :: IO ()
main = do
    xs <- getArgs
    case flip lookup tests =<< listToMaybe xs of
        Nothing -> error $ "Enter one of the examples: " ++ unwords (map fst tests)
        Just main -> main
