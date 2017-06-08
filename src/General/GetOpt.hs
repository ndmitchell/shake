
module General.GetOpt(
    OptDescr(..), ArgDescr(..),
    getOpt
    ) where

import qualified System.Console.GetOpt as O
import System.Console.GetOpt hiding (getOpt)
import Data.Either

getOpt :: [OptDescr (Either String a)] -> [String] -> ([a], [String], [String])
getOpt opts args = (flagGood, files, flagBad ++ errs)
    where (flags, files, errs) = O.getOpt O.Permute opts args
          (flagBad, flagGood) = partitionEithers flags
