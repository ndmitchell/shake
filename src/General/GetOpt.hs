
module General.GetOpt(
    OptDescr(..), ArgDescr(..),
    getOpt,
    showOptDescr
    ) where

import qualified System.Console.GetOpt as O
import System.Console.GetOpt hiding (getOpt)
import Data.Either
import Data.List

getOpt :: [OptDescr (Either String a)] -> [String] -> ([a], [String], [String])
getOpt opts args = (flagGood, files, flagBad ++ errs)
    where (flags, files, errs) = O.getOpt O.Permute opts args
          (flagBad, flagGood) = partitionEithers flags


showOptDescr :: [OptDescr a] -> [String]
showOptDescr xs = concat
    [ if nargs <= 26 then ["  " ++ args ++ replicate (28 - nargs) ' ' ++ desc]
                     else ["  " ++ args, replicate 30 ' ' ++ desc]
    | Option s l arg desc <- xs
    , let args = intercalate ", " $ map (short arg) s ++ map (long arg) l
    , let nargs = length args]
    where short NoArg{} x = "-" ++ [x]
          short (ReqArg _ b) x = "-" ++ [x] ++ " " ++ b
          short (OptArg _ b) x = "-" ++ [x] ++ "[" ++ b ++ "]"
          long NoArg{} x = "--" ++ x
          long (ReqArg _ b) x = "--" ++ x ++ "=" ++ b
          long (OptArg _ b) x = "--" ++ x ++ "[=" ++ b ++ "]"
