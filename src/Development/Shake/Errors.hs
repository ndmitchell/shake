{-# LANGUAGE DeriveDataTypeable, PatternGuards, RecordWildCards #-}

-- | Errors seen by the user
module Development.Shake.Errors(
    ShakeException(..),
    errorStructured, err,
    errorNoRuleToBuildType, errorRuleTypeMismatch, errorIncompatibleRules,
    errorMultipleRulesMatch, errorRuleRecursion, errorComplexRecursion, errorNoApply,
    ) where

import Control.Exception.Extra
import Data.Typeable


err :: String -> a
err msg = error $ "Development.Shake: Internal error, please report to Neil Mitchell (" ++ msg ++ ")"

errorStructured :: String -> [(String, Maybe String)] -> String -> IO a
errorStructured msg args hint = errorIO $ errorStructuredContents msg args hint

errorStructuredContents :: String -> [(String, Maybe String)] -> String -> String
errorStructuredContents msg args hint = unlines $
        [msg ++ ":"] ++
        ["  " ++ a ++ [':' | a /= ""] ++ replicate (as - length a + 2) ' ' ++ b | (a,b) <- args2] ++
        [hint | hint /= ""]
    where
        as = maximum $ 0 : map (length . fst) args2
        args2 = [(a,b) | (a,Just b) <- args]

errorNoRuleToBuildType :: TypeRep -> Maybe String -> Maybe TypeRep -> IO a
errorNoRuleToBuildType tk k tv = errorStructured
    "Build system error - no _rule_ matches the _key_ type"
    [("_Key_ type", Just $ show tk)
    ,("_Key_ value", k)
    ,("_Result_ type", fmap show tv)]
    "Either you are missing a call to _rule/defaultRule_, or your call to _apply_ has the wrong _key_ type"

errorRuleTypeMismatch :: TypeRep -> Maybe String -> TypeRep -> TypeRep -> IO a
errorRuleTypeMismatch tk k tvReal tvWant = errorStructured
    "Build system error - _rule_ used at the wrong _result_ type"
    [("_Key_ type", Just $ show tk)
    ,("_Key_ value", k)
    ,("_Rule_ _result_ type", Just $ show tvReal)
    ,("Requested _result_ type", Just $ show tvWant)]
    "Either the function passed to _rule/defaultRule_ has the wrong _result_ type, or the result of _apply_ is used at the wrong type"

errorIncompatibleRules :: TypeRep -> TypeRep -> TypeRep -> IO a
errorIncompatibleRules tk tv1 tv2 = errorStructured
    "Build system error - rule has multiple result types"
    [("Key type", Just $ show tk)
    ,("First result type", Just $ show tv1)
    ,("Second result type", Just $ show tv2)]
    "A function passed to rule/defaultRule has the wrong result type"

errorMultipleRulesMatch :: TypeRep -> Maybe String -> Int -> IO a
errorMultipleRulesMatch tk k count = errorStructured
    ("Build system error - key matches " ++ (if count == 0 then "no" else "multiple") ++ " rules")
    [("Key type",Just $ show tk)
    ,("Key value",k)
    ,("Rules matched",Just $ show count)]
    (if count == 0 then "Either add a rule that produces the above key, or stop requiring the above key"
     else "Modify your rules/defaultRules so only one can produce the above key")

errorRuleRecursion :: [String] -> Maybe String -> IO a
-- may involve both rules and oracle, so report as only rules
errorRuleRecursion stack k = throwIO $ wrap $ toException $ ErrorCall $ errorStructuredContents
    "Build system error - recursion detected"
    [("Key",k)]
    "Rules may not be recursive"
    where
        wrap = if null stack then id else toException . ShakeException (last stack) stack

errorComplexRecursion :: [String] -> IO a
errorComplexRecursion ks = errorStructured
    "Build system error - indirect recursion detected"
    [("Key value " ++ show i, Just k) | (i, k) <- zip [1..] ks]
    "Rules may not be recursive"

errorNoApply :: String -> Maybe String -> String -> IO a
errorNoApply k v msg = errorStructured
    "Build system error - cannot currently call _apply_"
    [("Reason", Just msg)
    ,("Key value", Just k)
    ,("Cached result", v)]
    "Move the _apply_ call earlier/later"

-- | Error representing all expected exceptions thrown by Shake.
--   Problems when executing rules will be raising using this exception type.
data ShakeException = ShakeException
    {shakeExceptionTarget :: String -- ^ The target that was being built when the exception occured.
    ,shakeExceptionStack :: [String]  -- ^ The stack of targets, where the 'shakeExceptionTarget' is last.
    ,shakeExceptionInner :: SomeException -- ^ The underlying exception that was raised.
    }
    deriving Typeable

instance Exception ShakeException

instance Show ShakeException where
    show ShakeException{..} = unlines $
        "Error when running Shake build system:" :
        map ("* " ++) shakeExceptionStack ++
        [show shakeExceptionInner]
