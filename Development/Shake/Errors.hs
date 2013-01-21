{-# LANGUAGE DeriveDataTypeable #-}

-- | Errors seen by the user
module Development.Shake.Errors(
    ShakeException(..),
    errorNoRuleToBuildType, errorRuleTypeMismatch, errorIncompatibleRules,
    errorMultipleRulesMatch, errorRuleRecursion,
    err
    ) where

import Control.Arrow
import Control.Exception
import Data.Typeable
import Data.List


err :: String -> a
err msg = error $ "Development.Shake: Internal error, please report to Neil Mitchell (" ++ msg ++ ")"

alternatives = let (*) = (,) in
    ["_rule_" * "oracle"
    ,"_Rule_" * "Oracle"
    ,"_key_" * "question"
    ,"_Key_" * "Question"
    ,"_result_" * "answer"
    ,"_Result_" * "Answer"
    ,"_rule/defaultRule_" * "addOracle"
    ,"_apply_" * "askOracle"]

structured :: Bool -> String -> [(String, Maybe String)] -> String -> a
structured alt msg args hint = structured_ (f msg) (map (first f) args) (f hint)
    where
        f = filter (/= '_') . (if alt then g else id)
        g xs | (a,b):_ <- filter (\(a,b) -> a `isPrefixOf` xs) alternatives = b ++ g (drop (length a) xs)
        g (x:xs) = x : g xs
        g [] = []

structured_ :: String -> [(String, Maybe String)] -> String -> a
structured_ msg args hint = error $
        msg ++ ":\n" ++ unlines ["  " ++ a ++ ":" ++ replicate (as - length a + 2) ' ' ++ drp b | (a,b) <- args2] ++ hint
    where
        drp x | "OracleA " `isPrefixOf` x = drop 8 x
              | "OracleQ " `isPrefixOf` x = drop 8 x
              | otherwise = x
        as = maximum $ 0 : map (length . fst) args2
        args2 = [(a,b) | (a,Just b) <- args]


errorNoRuleToBuildType :: TypeRep -> Maybe String -> Maybe TypeRep -> a
errorNoRuleToBuildType tk k tv = structured (isOracle tk)
    "Build system error - no _rule_ matches the _key_ type"
    [("_Key_ type", Just $ show tk)
    ,("_Key_ value", k)
    ,("_Result_ type", fmap show tv)]
    "Either you are missing a call to _rule/defaultRule_, or your call to _apply_ has the wrong _key_ type"

errorRuleTypeMismatch :: TypeRep -> Maybe String -> TypeRep -> TypeRep -> a
errorRuleTypeMismatch tk k tvReal tvWant = structured (isOracle tk)
    "Build system error - _rule_ used at the wrong _result_ type"
    [("_Key_ type", Just $ show tk)
    ,("_Key_ value", k)
    ,("_Rule_ _result_ type", Just $ show tvReal)
    ,("Requested _result_ type", Just $ show tvWant)]
    "Either the function passed to _rule/defaultRule_ has the wrong _result_ type, or the result of _apply_ is used at the wrong type"

errorIncompatibleRules :: TypeRep -> TypeRep -> TypeRep -> a
errorIncompatibleRules tk tv1 tv2 = if isOracle tk then errorDuplicateOracle tk Nothing [tv1,tv2] else structured_
    "Build system error - rule has multiple result types"
    [("Key type", Just $ show tk)
    ,("First result type", Just $ show tv1)
    ,("Second result type", Just $ show tv2)]
    "A function passed to rule/defaultRule has the wrong result type"

errorMultipleRulesMatch :: TypeRep -> String -> Int -> a
errorMultipleRulesMatch tk k count
    | isOracle tk = if count == 0 then err $ "no oracle match for " ++ show tk else errorDuplicateOracle tk (Just k) []
    | otherwise = structured_
    ("Build system error - key matches " ++ (if count == 0 then "no" else "multiple") ++ " rules")
    [("Key type",Just $ show tk)
    ,("Key value",Just k)
    ,("Rules matched",Just $ show count)]
    (if count == 0 then "Either add a rule that produces the above key, or stop requiring the above key"
     else "Modify your rules/defaultRules so only one can produce the above key")

errorRuleRecursion :: Maybe TypeRep -> Maybe String -> a
errorRuleRecursion tk k = structured_ -- may involve both rules and oracle, so report as a rule
    "Build system error - recursion detected"
    [("Key type",fmap show tk)
    ,("Key value",k)]
    "Rules may not be recursive"

errorDuplicateOracle :: TypeRep -> Maybe String -> [TypeRep] -> a
errorDuplicateOracle tk k tvs = structured_
    "Build system error - duplicate oracles for the same question type"
    ([("Question type",Just $ show tk)
     ,("Question value",k)] ++
     [("Answer type " ++ show i, Just $ show tv) | (i,tv) <- zip [1..] tvs])
    "Only one call to addOracle is allowed per question type"


isOracle :: TypeRep -> Bool
isOracle t = con `elem` ["OracleQ","OracleA"]
    where con = show $ fst $ splitTyConApp t


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
