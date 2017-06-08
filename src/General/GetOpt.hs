
module General.GetOpt(
    OptDescr(..), ArgDescr(..),
    getOpt,
    fmapOptDescr,
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


-- fmap is only an instance in later GHC 7.8 and above, so fake our own version
fmapOptDescr :: (a -> b) -> OptDescr (Either String a) -> OptDescr (Either String b)
fmapOptDescr f (Option a b c d) = Option a b (g c) d
    where g (NoArg a) = NoArg $ fmap f a
          g (ReqArg a b) = ReqArg (fmap f . a) b
          g (OptArg a b) = OptArg (fmap f . a) b


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
