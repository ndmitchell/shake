{-# LANGUAGE DeriveDataTypeable, PatternGuards, RecordWildCards, CPP #-}

-- | Errors seen by the user
module Development.Shake.Internal.Errors(
    ShakeException(..),
    throwM, throwImpure,
    errorInternal,
    errorStructured,
    errorNoRuleToBuildType, errorRuleDefinedMultipleTimes,
    errorMultipleRulesMatch, errorRuleRecursion, errorComplexRecursion, errorNoApply,
    errorDirectoryNotFile, errorNoHash
    ) where

import Data.Tuple.Extra
import Control.Exception.Extra
import Control.Monad.IO.Class
import Data.Typeable
import Data.List

throwM :: MonadIO m => SomeException -> m a
throwM = liftIO . throwIO

throwImpure :: SomeException -> a
throwImpure = throw


errorInternal :: String -> SomeException
errorInternal msg = toException $ ErrorCall $ "Development.Shake: Internal error, please report to Neil Mitchell (" ++ msg ++ ")"

alternatives = let (*) = (,) in
    ["_rule_" * "oracle"
    ,"_Rule_" * "Oracle"
    ,"_key_" * "question"
    ,"_Key_" * "Question"
    ,"_result_" * "answer"
    ,"_Result_" * "Answer"
    ,"_addBuiltinRule_" * "addOracle"
    ,"_apply_" * "askOracle"]


errorStructured :: String -> [(String, Maybe String)] -> String -> SomeException
errorStructured msg args hint = toException $ ErrorCall $ errorStructuredContents msg args hint

errorStructuredContents :: String -> [(String, Maybe String)] -> String -> String
errorStructuredContents msg args hint = unlines $
        [msg ++ ":"] ++
        ["  " ++ a ++ [':' | a /= ""] ++ replicate (as - length a + 2) ' ' ++ b | (a,b) <- args2] ++
        [hint | hint /= ""]
    where
        as = maximum $ 0 : map (length . fst) args2
        args2 = [(a,b) | (a,Just b) <- args]



structured :: Bool -> String -> [(String, Maybe String)] -> String -> SomeException
structured alt msg args hint = errorStructured (f msg) (map (first f) args) (f hint)
    where
        f = filter (/= '_') . (if alt then g else id)
        g xs | (a,b):_ <- filter (\(a,b) -> a `isPrefixOf` xs) alternatives = b ++ g (drop (length a) xs)
        g (x:xs) = x : g xs
        g [] = []


errorDirectoryNotFile :: FilePath -> SomeException
errorDirectoryNotFile dir = errorStructured
    "Build system error - expected a file, got a directory"
    [("Directory", Just dir)]
    "Probably due to calling 'need' on a directory. Shake only permits 'need' on files."

errorNoRuleToBuildType :: TypeRep -> Maybe String -> Maybe TypeRep -> SomeException
errorNoRuleToBuildType tk k tv = structured (specialIsOracleKey tk)
    "Build system error - no _rule_ matches the _key_ type"
    [("_Key_ type", Just $ show tk)
    ,("_Key_ value", k)
    ,("_Result_ type", fmap show tv)]
    "You are missing a call to _addBuiltinRule_, or your call to _apply_ has the wrong _key_ type"

errorRuleDefinedMultipleTimes :: TypeRep-> SomeException
errorRuleDefinedMultipleTimes tk = structured (specialIsOracleKey tk)
    "Build system error - _rule_ defined twice at one _key_ type"
    [("_Key_ type", Just $ show tk)]
    "You have called _addBuiltinRule_ more than once on the same key type"

errorMultipleRulesMatch :: TypeRep -> String -> Int -> SomeException
errorMultipleRulesMatch tk k count = errorStructured
    ("Build system error - key matches " ++ (if count == 0 then "no" else "multiple") ++ " rules")
    [("Key type",Just $ show tk)
    ,("Key value",Just k)
    ,("Rules matched",Just $ show count)]
    (if count == 0 then "Either add a rule that produces the above key, or stop requiring the above key"
    else "Modify your rules/defaultRules so only one can produce the above key")

errorNoHash :: SomeException
errorNoHash = errorStructured "Cannot use shakeChange=ChangeModTime with shakeCache" [] ""

errorRuleRecursion :: [String] -> TypeRep -> String -> SomeException
-- may involve both rules and oracle, so report as only rules
errorRuleRecursion stack tk k = wrap $ toException $ ErrorCall $ errorStructuredContents
    "Build system error - recursion detected"
    [("Key type",Just $ show tk)
    ,("Key value",Just k)]
    "Rules may not be recursive"
    where
        wrap = if null stack then id else toException . ShakeException (last stack) stack

errorComplexRecursion :: [String] -> SomeException
errorComplexRecursion ks = errorStructured
    "Build system error - indirect recursion detected"
    [("Key value " ++ show i, Just k) | (i, k) <- zip [1..] ks]
    "Rules may not be recursive"

errorNoApply :: TypeRep -> Maybe String -> String -> SomeException
errorNoApply tk k msg = errorStructured
    "Build system error - cannot currently introduce a dependency (e.g. calling 'apply')"
    [("Reason", Just msg)
    ,("Key type", Just $ show tk)
    ,("Key value", k)]
    "Move the call earlier/later"


-- Should be in Special, but then we get an import cycle
specialIsOracleKey :: TypeRep -> Bool
specialIsOracleKey t = con == "OracleQ"
    where con = show $ fst $ splitTyConApp t


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
        [displayException shakeExceptionInner]
