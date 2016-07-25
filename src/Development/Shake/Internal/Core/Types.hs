{-# LANGUAGE RecordWildCards, GeneralizedNewtypeDeriving, ScopedTypeVariables, PatternGuards, DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses, ConstraintKinds, DeriveFunctor #-}

module Development.Shake.Internal.Core.Types(
    BuiltinRun, BuiltinLint, RunResult(..), RunChanged(..),
    UserRule(..), UserRule_(..),
    BuiltinRule(..), Global(..), Local(..), Action(..),
    newLocal
    ) where

import Control.DeepSeq
import Control.Monad.IO.Class
import Control.Applicative
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
import Prelude


---------------------------------------------------------------------
-- UNDERLYING DATA TYPE

-- | The 'Action' monad, use 'liftIO' to raise 'IO' actions into it, and 'Development.Shake.need' to execute files.
--   Action values are used by 'addUserRule' and 'action'. The 'Action' monad tracks the dependencies of a rule.
--   To raise an exception call 'error', 'fail' or @'liftIO' . 'throwIO'@.
newtype Action a = Action {fromAction :: RAW Global Local a}
    deriving (Functor, Applicative, Monad, MonadIO, Typeable)

data RunChanged
    = ChangedNothing -- ^ Nothing has changed
    | ChangedStore -- ^ The value in the Store has changed, but in a way that should be considered equal
    | ChangedRecomputeSame -- ^ I recomputed the value and it was the same
    | ChangedRecomputeDiff -- ^ I recomputed the value and it was different
      deriving (Eq,Show)

instance NFData RunChanged where rnf x = x `seq` ()


data RunResult value = RunResult
    {runChanged :: RunChanged
        -- ^ Have the required dependencies of this action changed? Use 'True' to use the dependencies this time
        --   around as the future dependencies. Use 'False' to keep the previous dependencies.
    -- ,resultStore :: BS.ByteString
        -- ^ Return the new value to store, and a 'True' if that value has changed from the argument store.
    ,runValue :: value
        -- ^ Return the produced value and a 'True' if that value has changed in a meaningful way from last time.
    } deriving Functor

instance NFData value => NFData (RunResult value) where
    rnf (RunResult x1 x2) = rnf x1 `seq` rnf x2


type BuiltinRun key value = key -> Maybe value -> Bool -> Action (RunResult value)

type BuiltinLint key value = key -> value -> IO (Maybe String)

data BuiltinRule = BuiltinRule
    {builtinRun :: BuiltinRun Key Value
    ,builtinLint :: BuiltinLint Key Value
    ,builtinResult :: TypeRep
    }


data UserRule_ = forall a . Typeable a => UserRule_ (UserRule a)

-- | A 'Match' data type, representing user-defined rules associated with a particular type.
--   As an example '?>' and '*>' will add entries to the 'Match' data type.
--
--   /Semantics/
--
-- > priority p1 (priority p2 x) == priority p1 x
-- > priority p (x `ordered` y) = priority p x `ordered` priority p y
-- > priority p (x `unordered` y) = priority p x `unordered` priority p y
-- > ordered is associative
-- > unordered is associative and commutative
-- > alternative does not obey priorities, until picking the best one
data UserRule a
    = UserRule a -- ^ Added to the state with @'addUserRule' :: Typeable a => a -> 'Rules' ()@.
    | Unordered [UserRule a] -- ^ Rules combined with the 'Monad'/'Monoid'.
    | Priority Double (UserRule a) -- ^ Rules defined under 'priority'.
    | Alternative (UserRule a) -- ^ Rule defined under 'alternative', matched in order.
      deriving (Eq,Show,Functor,Typeable)


-- global constants of Action
data Global = Global
    {globalDatabase :: Database -- ^ Database, contains knowledge of the state of each key
    ,globalPool :: Pool -- ^ Pool, for queuing new elements
    ,globalCleanup :: Cleanup -- ^ Cleanup operations
    ,globalTimestamp :: IO Seconds -- ^ Clock saying how many seconds through the build
    ,globalRules :: Map.HashMap TypeRep BuiltinRule -- ^ Rules for this build
    ,globalOutput :: Verbosity -> String -> IO () -- ^ Output function
    ,globalOptions  :: ShakeOptions -- ^ Shake options
    ,globalDiagnostic :: IO String -> IO () -- ^ Debugging function
    ,globalCurDir :: FilePath -- ^ getCurrentDirectory when we started
    ,globalAfter :: IORef [IO ()] -- ^ Operations to run on success, e.g. removeFilesAfter
    ,globalTrackAbsent :: IORef [(Key, Key)] -- ^ Tracked things, in rule fst, snd must be absent
    ,globalProgress :: IO Progress -- ^ Request current progress state
    ,globalUserRules :: Map.HashMap TypeRep UserRule_
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
