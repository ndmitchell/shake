{-# LANGUAGE DeriveDataTypeable #-}

-- | Errors seen by the user
module Development.Shake.Errors(
    ShakeException(..),
    errorNoRuleToBuildType, errorRuleTypeMismatch, errorIncompatibleRules,
    errorMultipleRulesMatch, errorRuleRecursion,
    err
    ) where

import Control.Exception
import Data.Typeable
import Data.List
import Development.Shake.Value


err :: String -> a
err msg = error $ "Development.Shake: Internal error, please report to Neil Mitchell (" ++ msg ++ ")"

errorNoRuleToBuildType :: TypeRep -> Maybe Key -> Maybe TypeRep -> a
errorNoRuleToBuildType tk _ _ = error $
    "Error: couldn't find any rules to build type " ++ showTypeRepBracket tk ++
    ", perhaps you are missing a call to " ++
    (if isOracleType tk then "addOracle" else "defaultRule/rule") ++ "?"

errorRuleTypeMismatch :: TypeRep -> Maybe Key -> TypeRep -> TypeRep -> a
errorRuleTypeMismatch tk _ tvWant tvGot = error $
    "Error: rule to build type " ++ showTypeRepBracket tk ++
    " produces " ++ showTypeRepBracket tvGot ++ " but used as " ++ showTypeRepBracket tvWant ++
    ", perhaps you have the wrong types in a call to " ++
    (if isOracleType tk then "askOracle" else "apply") ++ "?"

errorIncompatibleRules :: TypeRep -> TypeRep -> TypeRep -> a
errorIncompatibleRules tk tv1 tv2 = error $
    "There are two incompatible rules for " ++ show tk ++ ", producing " ++ show tv1 ++ " and " ++ show tv2

errorMultipleRulesMatch :: Key -> Int -> a
errorMultipleRulesMatch k count = error $
    let s = if count == 0 then "no" else show count
    in "Error: " ++ s ++ " rules match for Rule " ++ show k ++ " of type " ++ show (typeKey k)

errorRuleRecursion :: Maybe Key -> a
errorRuleRecursion k = error $
    "Invalid rules, recursion detected when trying to build: " ++ maybe "<unknown>" show k

isOracleType :: TypeRep -> Bool
isOracleType t = con `elem` ["OracleQ","OracleA"]
    where con = show $ fst $ splitTyConApp t

showTypeRepBracket :: TypeRep -> String
showTypeRepBracket ty = ['(' | not safe] ++ show ty ++ [')' | not safe]
    where (t1,args) = splitTyConApp ty
          st1 = show t1
          safe = null args || st1 == "[]" || "(" `isPrefixOf` st1


-- NOTE: Not currently public, to avoid pinning down the API yet
-- | All foreseen exception conditions thrown by Shake, such problems with the rules or errors when executing
--   rules, will be raised using this exception type.
data ShakeException = ShakeException
        [String] -- Entries on the stack, starting at the top of the stack.
        SomeException -- Inner exception that was raised.
        -- If I make these Haddock comments, then Haddock dies
    deriving Typeable

instance Exception ShakeException

instance Show ShakeException where
    show (ShakeException stack inner) = unlines $
        "Error when running Shake build system:" :
        map ("* " ++) stack ++
        [show inner]
