{-# LANGUAGE RecordWildCards, GeneralizedNewtypeDeriving, ScopedTypeVariables, PatternGuards, DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses, ConstraintKinds #-}

module Development.Shake.Internal.Core.Action(
    RuleInfo(..), BuiltinInfo(..), Changed(..), Global(..), Local(..), Action(..),
    newLocal,
    runAction, actionOnException, actionFinally,
    getShakeOptions, getProgress, runAfter,
    trackUse, trackChange, trackAllow, trackCheckUsed,
    getVerbosity, putWhen, putLoud, putNormal, putQuiet, withVerbosity, quietly,
    blockApply, unsafeAllowApply,
    traced
    ) where

import Control.Exception.Extra
import Control.Applicative
import Control.Monad.Extra
import Control.Monad.IO.Class
import Control.DeepSeq
import Data.Typeable
import Data.Function
import Data.Either.Extra
import qualified Data.HashMap.Strict as Map
import Data.Maybe
import Data.IORef
import Data.List
import System.IO.Extra
import System.Time.Extra
import Numeric.Extra

import Development.Shake.Internal.Core.Pool
import Development.Shake.Internal.Core.Database
import Development.Shake.Internal.Core.Monad
import Development.Shake.Internal.Value
import Development.Shake.Internal.Types
import Development.Shake.Internal.Errors
import General.Cleanup
import General.String
import Prelude

---------------------------------------------------------------------
-- UNDERLYING DATA TYPE

-- | The 'Action' monad, use 'liftIO' to raise 'IO' actions into it, and 'Development.Shake.need' to execute files.
--   Action values are used by 'addUserRule' and 'action'. The 'Action' monad tracks the dependencies of a rule.
--   To raise an exception call 'error', 'fail' or @'liftIO' . 'throwIO'@.
newtype Action a = Action {fromAction :: RAW Global Local a}
    deriving (Functor, Applicative, Monad, MonadIO, Typeable)

data Changed
    = ChangedNothing -- ^ Nothing has changed
    | ChangedStore -- ^ The value in the Store has changed, but in a way that should be considered equal
    | ChangedRecomputeSame -- ^ I recomputed the value and it was the same
    | ChangedRecomputeDiff -- ^ I recomputed the value and it was different
      deriving Eq


data BuiltinInfo value = BuiltinInfo
    {resultChanged :: Changed
        -- ^ Have the required dependencies of this action changed? Use 'True' to use the dependencies this time
        --   around as the future dependencies. Use 'False' to keep the previous dependencies.
    -- ,resultStore :: BS.ByteString
        -- ^ Return the new value to store, and a 'True' if that value has changed from the argument store.
    ,resultValue :: value
        -- ^ Return the produced value and a 'True' if that value has changed in a meaningful way from last time.
    }

data RuleInfo = RuleInfo
    {execute :: Key -> Maybe Value -> Bool -> Action (BuiltinInfo Value)
    ,lint :: Key -> Value -> IO (Maybe String)
    ,resultType :: TypeRep
    }


-- global constants of Action
data Global = Global
    {globalDatabase :: Database -- ^ Database, contains knowledge of the state of each key
    ,globalPool :: Pool -- ^ Pool, for queuing new elements
    ,globalCleanup :: Cleanup -- ^ Cleanup operations
    ,globalTimestamp :: IO Seconds -- ^ Clock saying how many seconds through the build
    ,globalRules :: Map.HashMap TypeRep RuleInfo -- ^ Rules for this build
    ,globalOutput :: Verbosity -> String -> IO () -- ^ Output function
    ,globalOptions  :: ShakeOptions -- ^ Shake options
    ,globalDiagnostic :: IO String -> IO () -- ^ Debugging function
    ,globalLint :: String -> IO () -- ^ Run lint checking
    ,globalAfter :: IORef [IO ()] -- ^ Operations to run on success, e.g. removeFilesAfter
    ,globalTrackAbsent :: IORef [(Key, Key)] -- ^ Tracked things, in rule fst, snd must be absent
    ,globalProgress :: IO Progress -- ^ Request current progress state
    }

-- local variables of Action
data Local = Local
    -- constants
    {localStack :: Stack -- ^ The stack that ran to get here.
    -- stack scoped local variables
    ,localVerbosity :: Verbosity -- ^ Verbosity, may be changed locally
    ,localBlockApply ::  Maybe String -- ^ Reason to block apply, or Nothing to allow
    -- mutable local variables
    ,localDepends :: [Depends] -- ^ Dependencies, built up in reverse
    ,localDiscount :: !Seconds -- ^ Time spend building dependencies
    ,localTraces :: [Trace] -- ^ Traces, built in reverse
    ,localTrackAllows :: [Key -> Bool] -- ^ Things that are allowed to be used
    ,localTrackUsed :: [Key] -- ^ Things that have been used
    }

newLocal :: Stack -> Verbosity -> Local
newLocal stack verb = Local stack verb Nothing [] 0 [] [] []


---------------------------------------------------------------------
-- RAW WRAPPERS

runAction :: Global -> Local -> Action a -> Capture (Either SomeException a)
runAction g l (Action x) = runRAW g l x


---------------------------------------------------------------------
-- EXCEPTION HANDLING

actionBoom :: Bool -> Action a -> IO b -> Action a
actionBoom runOnSuccess act clean = do
    cleanup <- Action $ getsRO globalCleanup
    clean <- liftIO $ addCleanup cleanup $ void clean
    res <- Action $ catchRAW (fromAction act) $ \e -> liftIO (clean True) >> throwRAW e
    liftIO $ clean runOnSuccess
    return res

-- | If an exception is raised by the 'Action', perform some 'IO'.
actionOnException :: Action a -> IO b -> Action a
actionOnException = actionBoom False

-- | After an 'Action', perform some 'IO', even if there is an exception.
actionFinally :: Action a -> IO b -> Action a
actionFinally = actionBoom True


---------------------------------------------------------------------
-- QUERIES

-- | Get the initial 'ShakeOptions', these will not change during the build process.
getShakeOptions :: Action ShakeOptions
getShakeOptions = Action $ getsRO globalOptions

-- | Get the current 'Progress' structure, as would be returned by 'shakeProgress'.
getProgress :: Action Progress
getProgress = do
    res <- Action $ getsRO globalProgress
    liftIO res

-- | Specify an action to be run after the database has been closed, if building completes successfully.
runAfter :: IO () -> Action ()
runAfter op = do
    Global{..} <- Action getRO
    liftIO $ atomicModifyIORef globalAfter $ \ops -> (op:ops, ())


---------------------------------------------------------------------
-- VERBOSITY

putWhen :: Verbosity -> String -> Action ()
putWhen v msg = do
    Global{..} <- Action getRO
    verb <- getVerbosity
    when (verb >= v) $
        liftIO $ globalOutput v msg


-- | Write an unimportant message to the output, only shown when 'shakeVerbosity' is higher than normal ('Loud' or above).
--   The output will not be interleaved with any other Shake messages (other than those generated by system commands).
putLoud :: String -> Action ()
putLoud = putWhen Loud

-- | Write a normal priority message to the output, only supressed when 'shakeVerbosity' is 'Quiet' or 'Silent'.
--   The output will not be interleaved with any other Shake messages (other than those generated by system commands).
putNormal :: String -> Action ()
putNormal = putWhen Normal

-- | Write an important message to the output, only supressed when 'shakeVerbosity' is 'Silent'.
--   The output will not be interleaved with any other Shake messages (other than those generated by system commands).
putQuiet :: String -> Action ()
putQuiet = putWhen Quiet


-- | Get the current verbosity level, originally set by 'shakeVerbosity'. If you
--   want to output information to the console, you are recommended to use
--   'putLoud' \/ 'putNormal' \/ 'putQuiet', which ensures multiple messages are
--   not interleaved. The verbosity can be modified locally by 'withVerbosity'.
getVerbosity :: Action Verbosity
getVerbosity = Action $ getsRW localVerbosity


-- | Run an action with a particular verbosity level.
--   Will not update the 'shakeVerbosity' returned by 'getShakeOptions' and will
--   not have any impact on 'Diagnostic' tracing.
withVerbosity :: Verbosity -> Action a -> Action a
withVerbosity new = Action . unmodifyRW f . fromAction
    where f s0 = (s0{localVerbosity=new}, \s -> s{localVerbosity=localVerbosity s0})


-- | Run an action with 'Quiet' verbosity, in particular messages produced by 'traced'
--   (including from 'Development.Shake.cmd' or 'Development.Shake.command') will not be printed to the screen.
--   Will not update the 'shakeVerbosity' returned by 'getShakeOptions' and will
--   not turn off any 'Diagnostic' tracing.
quietly :: Action a -> Action a
quietly = withVerbosity Quiet


---------------------------------------------------------------------
-- BLOCK APPLY

unsafeAllowApply :: Action a -> Action a
unsafeAllowApply  = applyBlockedBy Nothing

blockApply :: String -> Action a -> Action a
blockApply = applyBlockedBy . Just

applyBlockedBy :: Maybe String -> Action a -> Action a
applyBlockedBy reason = Action . unmodifyRW f . fromAction
    where f s0 = (s0{localBlockApply=reason}, \s -> s{localBlockApply=localBlockApply s0})


---------------------------------------------------------------------
-- TRACING

-- | Write an action to the trace list, along with the start/end time of running the IO action.
--   The 'Development.Shake.cmd' and 'Development.Shake.command' functions automatically call 'traced'.
--   The trace list is used for profile reports (see 'shakeReport').
--
--   By default 'traced' prints some useful extra context about what
--   Shake is building, e.g.:
--
-- > # traced message (for myobject.o)
--
--   To suppress the output of 'traced' (for example you want more control
--   over the message using 'putNormal'), use the 'quietly' combinator.
traced :: String -> IO a -> Action a
traced msg act = do
    Global{..} <- Action getRO
    stack <- Action $ getsRW localStack
    start <- liftIO globalTimestamp
    putNormal $ "# " ++ msg ++ " (for " ++ showTopStack stack ++ ")"
    res <- liftIO act
    stop <- liftIO globalTimestamp
    let trace = Trace (pack msg) (doubleToFloat start) (doubleToFloat stop)
    liftIO $ evaluate $ rnf trace
    Action $ modifyRW $ \s -> s{localTraces = trace : localTraces s}
    return res


---------------------------------------------------------------------
-- TRACKING

-- | Track that a key has been used by the action preceeding it.
trackUse :: ShakeValue key => key -> Action ()
-- One of the following must be true:
-- 1) you are the one building this key (e.g. key == topStack)
-- 2) you have already been used by apply, and are on the dependency list
-- 3) someone explicitly gave you permission with trackAllow
-- 4) at the end of the rule, a) you are now on the dependency list, and b) this key itself has no dependencies (is source file)
trackUse key = do
    let k = newKey key
    Global{..} <- Action getRO
    l@Local{..} <- Action getRW
    deps <- liftIO $ concatMapM (listDepends globalDatabase) localDepends
    let top = topStack localStack
    if top == Just k then
        return () -- condition 1
     else if k `elem` deps then
        return () -- condition 2
     else if any ($ k) localTrackAllows then
        return () -- condition 3
     else
        Action $ putRW l{localTrackUsed = k : localTrackUsed} -- condition 4


trackCheckUsed :: Action ()
trackCheckUsed = do
    Global{..} <- Action getRO
    Local{..} <- Action getRW
    liftIO $ do
        deps <- concatMapM (listDepends globalDatabase) localDepends

        -- check 3a
        bad <- return $ localTrackUsed \\ deps
        unless (null bad) $ do
            let n = length bad
            errorStructured
                ("Lint checking error - " ++ (if n == 1 then "value was" else show n ++ " values were") ++ " used but not depended upon")
                [("Used", Just $ show x) | x <- bad]
                ""

        -- check 3b
        bad <- flip filterM localTrackUsed $ \k -> (not . null) <$> lookupDependencies globalDatabase k
        unless (null bad) $ do
            let n = length bad
            errorStructured
                ("Lint checking error - " ++ (if n == 1 then "value was" else show n ++ " values were") ++ " depended upon after being used")
                [("Used", Just $ show x) | x <- bad]
                ""


-- | Track that a key has been changed by the action preceeding it.
trackChange :: ShakeValue key => key -> Action ()
-- One of the following must be true:
-- 1) you are the one building this key (e.g. key == topStack)
-- 2) someone explicitly gave you permission with trackAllow
-- 3) this file is never known to the build system, at the end it is not in the database
trackChange key = do
    let k = newKey key
    Global{..} <- Action getRO
    Local{..} <- Action getRW
    liftIO $ do
        let top = topStack localStack
        if top == Just k then
            return () -- condition 1
         else if any ($ k) localTrackAllows then
            return () -- condition 2
         else
            -- condition 3
            atomicModifyIORef globalTrackAbsent $ \ks -> ((fromMaybe k top, k):ks, ())


-- | Allow any matching key to violate the tracking rules.
trackAllow :: ShakeValue key => (key -> Bool) -> Action ()
trackAllow (test :: key -> Bool) = Action $ modifyRW $ \s -> s{localTrackAllows = f : localTrackAllows s}
    where
        tk = typeRep (Proxy :: Proxy key)
        f k = typeKey k == tk && test (fromKey k)


