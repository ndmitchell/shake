{-# LANGUAGE RecordWildCards, GeneralizedNewtypeDeriving, ScopedTypeVariables, PatternGuards, DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses, ConstraintKinds #-}

module Development.Shake.Internal.Core.Types(
    RuleInfo(..), BuiltinInfo(..), Changed(..), Global(..), Local(..), Action(..),
    newLocal
    ) where

import Control.Monad.IO.Class
import Data.Typeable
import qualified Data.HashMap.Strict as Map
import Data.IORef
import System.Time.Extra

import Development.Shake.Internal.Core.Pool
import Development.Shake.Internal.Core.Database
import Development.Shake.Internal.Core.Monad
import Development.Shake.Internal.Value
import Development.Shake.Internal.Types
import General.Cleanup


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
