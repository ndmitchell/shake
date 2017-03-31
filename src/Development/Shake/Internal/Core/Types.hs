{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables, DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification, ConstraintKinds, DeriveFunctor #-}

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
import General.Binary
import qualified Data.HashMap.Strict as Map
import Data.IORef
import qualified Data.ByteString as BS
import System.Time.Extra

import Development.Shake.Internal.Core.Pool
import Development.Shake.Internal.Core.Database
import Development.Shake.Internal.Core.Monad
import Development.Shake.Internal.Value
import Development.Shake.Internal.Options
import General.Cleanup
import Prelude


---------------------------------------------------------------------
-- UNDERLYING DATA TYPE

-- | The 'Action' monad, use 'liftIO' to raise 'IO' actions into it, and 'Development.Shake.need' to execute files.
--   Action values are used by 'addUserRule' and 'action'. The 'Action' monad tracks the dependencies of a rule.
--   To raise an exception call 'error', 'fail' or @'liftIO' . 'throwIO'@.
newtype Action a = Action {fromAction :: RAW Global Local a}
    deriving (Functor, Applicative, Monad, MonadIO, Typeable)

-- | How has a rule changed.
data RunChanged
    = ChangedNothing -- ^ Nothing has changed.
    | ChangedStore -- ^ The persisted value has changed, but in a way that should be considered identical.
    | ChangedRecomputeSame -- ^ I recomputed the value and it was the same.
    | ChangedRecomputeDiff -- ^ I recomputed the value and it was different.
      deriving (Eq,Show)

instance NFData RunChanged where rnf x = x `seq` ()


-- | The result of 'BuiltinRun'.
data RunResult value = RunResult
    {runChanged :: RunChanged
        -- ^ What has changed from the previous time.
    ,runStore :: BS.ByteString
        -- ^ Return the new value to store. Often a serialised version of 'runValue'.
    ,runValue :: value
        -- ^ Return the produced value.
    } deriving Functor

instance NFData value => NFData (RunResult value) where
    rnf (RunResult x1 x2 x3) = rnf x1 `seq` x2 `seq` rnf x3


-- | Define a rule between @key@ and @value@. A rule for a class of artifacts (e.g. /files/) provides:
--
-- * How to identify individual artifacts, given by the @key@ type, e.g. with file names.
--
-- * How to describe the state of an artifact, given by the @value@ type, e.g. the file modification time.
--
-- * How to persist the state of an artifact, using the 'ByteString' values, e.g. seralised @value@.
--
--   The arguments comprise the @key@, the value of the previous serialisation or 'Nothing' if the rule
--   has not been run previously, and 'True' to indicate the dependencies have changed or 'False' that
--   they have not.
type BuiltinRun key value = key -> Maybe BS.ByteString -> Bool -> Action (RunResult value)

-- | The action performed by @--lint@ for a given @key@/@value@ pair.
--   At the end of the build the lint action will be called for each @key@ that was built this run,
--   passing the @value@ it produced. Return 'Nothing' to indicate the value has not changed and
--   is acceptable, or 'Just' an error message to indicate failure.
--
--   For builtin rules where the value is expected to change use 'Development.Shake.Rules.noLint'.
type BuiltinLint key value = key -> value -> IO (Maybe String)

data BuiltinRule = BuiltinRule
    {builtinRun :: BuiltinRun Key Value
    ,builtinLint :: BuiltinLint Key Value
    ,builtinResult :: TypeRep
    ,builtinKey :: BinaryOp Key
    }


data UserRule_ = forall a . Typeable a => UserRule_ (UserRule a)

-- | A 'UserRule' data type, representing user-defined rules associated with a particular type.
--   As an example 'Development.Shake.?>' and 'Development.Shake.%>' will add entries to the 'UserRule' data type.
data UserRule a
-- > priority p1 (priority p2 x) == priority p1 x
-- > priority p (x `ordered` y) = priority p x `ordered` priority p y
-- > priority p (x `unordered` y) = priority p x `unordered` priority p y
-- > ordered is associative
-- > unordered is associative and commutative
-- > alternative does not obey priorities, until picking the best one
    = UserRule a -- ^ Added to the state with @'addUserRule' :: Typeable a => a -> 'Rules' ()@.
    | Unordered [UserRule a] -- ^ Rules combined with the 'Monad' \/ 'Monoid'.
    | Priority Double (UserRule a) -- ^ Rules defined under 'priority'.
    | Alternative (UserRule a) -- ^ Rule defined under 'alternatives', matched in order.
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
