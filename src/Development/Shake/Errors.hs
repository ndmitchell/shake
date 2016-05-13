{-# LANGUAGE DeriveDataTypeable, PatternGuards, RecordWildCards #-}

-- | Errors seen by the user
module Development.Shake.Errors(
    ShakeException(..),
    errorStructured, err,
    errorNoRuleToBuildType, errorRuleTypeMismatch, errorIncompatibleRules,
    errorMultipleRulesMatch, errorRuleRecursion, errorComplexRecursion, errorNoApply,
    errorNoReference
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
    "Build system error - no rule matches the key type"
    [("Key type", Just $ show tk)
    ,("Key value", k)
    ,("Result type", fmap show tv)]
    "Either you are missing a call to rule/defaultRule, or your call to apply has the wrong key type"

errorRuleTypeMismatch :: TypeRep -> Maybe String -> TypeRep -> TypeRep -> IO a
errorRuleTypeMismatch tk k tvReal tvWant = errorStructured
    "Build system error - rule used at the wrong result type"
    [("Key type", Just $ show tk)
    ,("Key value", k)
    ,("Rule result type", Just $ show tvReal)
    ,("Requested result type", Just $ show tvWant)]
    "Either the function passed to rule/defaultRule has the wrong result type, or the result of apply is used at the wrong type"

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

wrap stack = if null stack then id else toException . ShakeException (last stack) stack

errorRuleRecursion :: [String] -> Maybe String -> IO a
errorRuleRecursion stack k = throwIO $ wrap stack $ toException $ ErrorCall $ errorStructuredContents
    "Build system error - recursion detected"
    [("Key",k)]
    "Rules may not be recursive"

errorComplexRecursion :: [String] -> IO a
errorComplexRecursion ks = errorStructured
    "Build system error - indirect recursion detected"
    [("Key value " ++ show i, Just k) | (i, k) <- zip [1..] ks]
    "Rules may not be recursive"

errorNoApply :: String -> Maybe String -> String -> IO a
errorNoApply k v msg = errorStructured
    "Build system error - cannot currently call apply"
    [("Reason", Just msg)
    ,("Key value", Just k)
    ,("Cached result", v)]
    "Move the apply call earlier/later"

errorNoReference :: [String] -> String -> IO a
errorNoReference stack i = throwIO $ wrap stack $ toException $ ErrorCall $ errorStructuredContents
    "Build system error - cannot find id referenced in dependencies"
    [("Id",Just i)]
    "Your database may be corrupted"

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
