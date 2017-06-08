
module General.GetOpt(
    OptDescr(..), ArgDescr(..),
    getOpt,
    fmapOptDescr,
    showOptDescr,
    mergeOptDescr,
    removeOverlap,
    optionsEnum,
    optionsEnumDesc
    ) where

import qualified System.Console.GetOpt as O
import System.Console.GetOpt hiding (getOpt)
import qualified Data.HashSet as Set
import Data.Maybe
import Data.Either
import Data.List.Extra


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


-- | Remove flags from the first field that are present in the second
removeOverlap :: [OptDescr b] -> [OptDescr a] -> [OptDescr a]
removeOverlap bad = mapMaybe f
    where
        short = Set.fromList $ concat [x | Option x _ _ _ <- bad]
        long  = Set.fromList $ concat [x | Option _ x _ _ <- bad]
        f (Option a b c d) | null a2 && null b2 = Nothing
                           | otherwise = Just $ Option a2 b2 c d
            where a2 = filter (not . flip Set.member short) a
                  b2 = filter (not . flip Set.member long) b

mergeOptDescr :: [OptDescr (Either String a)] -> [OptDescr (Either String b)] -> [OptDescr (Either String (Either a b))]
mergeOptDescr xs ys = map (fmapOptDescr Left) xs ++ map (fmapOptDescr Right) ys

optionsEnum :: (Enum a, Bounded a, Show a) => [OptDescr (Either String a)]
optionsEnum = optionsEnumDesc [(x, "Flag " ++ lower (show x) ++ ".") | x <- [minBound..maxBound]]

optionsEnumDesc :: Show a => [(a, String)] -> [OptDescr (Either String a)]
optionsEnumDesc xs = [Option "" [lower $ show x] (NoArg $ Right x) d | (x,d) <- xs]
