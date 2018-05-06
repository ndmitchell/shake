{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables, DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification, DeriveFunctor, RecordWildCards, FlexibleInstances #-}

module Development.Shake.Internal.Core.Types(
    BuiltinRun, BuiltinLint, BuiltinIdentity,
    RunMode(..), RunResult(..), RunChanged(..),
    UserRule(..), UserRuleVersioned(..),
    BuiltinRule(..), Global(..), Local(..), Action(..), runAction, addDiscount,
    newLocal, localClearMutable, localMergeMutable,
    Stack, Step(..), Result(..), Database(..), Depends(..), Status(..), Trace(..),
    getResult, exceptionStack, statusType, addStack, addCallStack,
    incStep, newTrace, nubDepends, emptyStack, topStack, showTopStack,
    stepKey, StepKey(..), toStepResult, fromStepResult, NoShow(..)
    ) where

import Control.Monad.IO.Class
import Control.Applicative
import Control.DeepSeq
import Foreign.Storable
import Data.Word
import Data.Typeable
import General.Binary
import Control.Exception
import Data.Maybe
import General.Extra
import Control.Concurrent.Extra
import Development.Shake.Internal.History.Shared
import General.Wait
import Development.Shake.Internal.Errors
import qualified General.TypeMap as TMap
import Data.IORef
import qualified Data.ByteString.Char8 as BS
import Numeric.Extra
import System.Time.Extra
import General.Intern(Id, Intern)
import qualified Data.HashSet as Set
import qualified Data.HashMap.Strict as Map
import qualified General.Ids as Ids
import Data.Tuple.Extra

import General.Pool
import Development.Shake.Internal.Core.Monad
import Development.Shake.Internal.Value
import Development.Shake.Internal.Options
import Development.Shake.Classes
import Data.Semigroup
import General.Cleanup
import Prelude

#if __GLASGOW_HASKELL__ >= 800
import Control.Monad.Fail
#endif


---------------------------------------------------------------------
-- UNDERLYING DATA TYPE

-- | The 'Action' monad, use 'liftIO' to raise 'IO' actions into it, and 'Development.Shake.need' to execute files.
--   Action values are used by 'addUserRule' and 'action'. The 'Action' monad tracks the dependencies of a rule.
--   To raise an exception call 'error', 'fail' or @'liftIO' . 'throwIO'@.
newtype Action a = Action {fromAction :: RAW Global Local a}
    deriving (Functor, Applicative, Monad, MonadIO, Typeable, Semigroup, Monoid
#if __GLASGOW_HASKELL__ >= 800
             ,MonadFail
#endif
        )

runAction :: Global -> Local -> Action a -> Capture (Either SomeException a)
runAction g l (Action x) = runRAW g l x


---------------------------------------------------------------------
-- PUBLIC TYPES

-- | What mode a rule is running in, passed as an argument to 'BuiltinRun'.
data RunMode
    = RunDependenciesSame -- ^ My dependencies have not changed.
    | RunDependenciesChanged -- ^ At least one of my dependencies from last time have changed, or I have no recorded dependencies.
      deriving (Eq,Show)

instance NFData RunMode where rnf x = x `seq` ()

-- | How the output of a rule has changed.
data RunChanged
    = ChangedNothing -- ^ Nothing has changed.
    | ChangedStore -- ^ The stored value has changed, but in a way that should be considered identical (used rarely).
    | ChangedRecomputeSame -- ^ I recomputed the value and it was the same.
    | ChangedRecomputeDiff -- ^ I recomputed the value and it was different.
      deriving (Eq,Show)

instance NFData RunChanged where rnf x = x `seq` ()


-- | The result of 'BuiltinRun'.
data RunResult value = RunResult
    {runChanged :: RunChanged
        -- ^ How has the 'RunResult' changed from what happened last time.
    ,runStore :: BS.ByteString
        -- ^ The value to store in the Shake database.
    ,runValue :: value
        -- ^ The value to return from 'Development.Shake.Rule.apply'.
    } deriving Functor

instance NFData value => NFData (RunResult value) where
    rnf (RunResult x1 x2 x3) = rnf x1 `seq` x2 `seq` rnf x3



---------------------------------------------------------------------
-- UTILITY TYPES

newtype Step = Step Word32 deriving (Eq,Ord,Show,Storable,BinaryEx,NFData,Hashable,Typeable)

incStep (Step i) = Step $ i + 1


-- To simplify journaling etc we smuggle the Step in the database, with a special StepKey
newtype StepKey = StepKey ()
    deriving (Show,Eq,Typeable,Hashable,Binary,BinaryEx,NFData)

stepKey :: Key
stepKey = newKey $ StepKey ()

toStepResult :: Step -> Result BS.ByteString
toStepResult i = Result (runBuilder $ putEx i) i i [] 0 []

fromStepResult :: Result BS.ByteString -> Step
fromStepResult = getEx . result



---------------------------------------------------------------------
-- CALL STACK

-- Invariant: Every key must have its Id in the set
data Stack = Stack (Maybe Key) [Either Key [String]] !(Set.HashSet Id) deriving Show

exceptionStack :: Stack -> SomeException -> ShakeException
exceptionStack stack@(Stack _ xs _) = ShakeException (showTopStack stack) $
    concatMap f (reverse xs) ++ ["* Raised the exception:" | not $ null xs]
    where
        f (Left x) = ["* Depends on: " ++ show x]
        f (Right x) = map ("  at " ++) x


showTopStack :: Stack -> String
showTopStack = maybe "<unknown>" show . topStack

addStack :: Id -> Key -> Stack -> Either SomeException Stack
addStack i k (Stack _ ks is)
    | i `Set.member` is = Left $ toException $ exceptionStack stack2 $ errorRuleRecursion (typeKey k) (show k)
    | otherwise = Right stack2
    where stack2 = Stack (Just k) (Left k:ks) (Set.insert i is)

addCallStack :: [String] -> Stack -> Stack
-- use group/head to squash adjacent duplicates, e.g. a want does an action and a need, both of which get the same location
addCallStack xs (Stack t a b) = Stack t (Right xs : dropWhile (== Right xs) a) b

topStack :: Stack -> Maybe Key
topStack (Stack t _ _) = t

emptyStack :: Stack
emptyStack = Stack Nothing [] Set.empty


---------------------------------------------------------------------
-- TRACE

data Trace = Trace
    {traceMessage ::  {-# UNPACK #-} !BS.ByteString
    ,traceStart :: {-# UNPACK #-} !Float
    ,traceEnd :: {-# UNPACK #-} !Float
    }
    deriving Show

instance NFData Trace where
    rnf x = x `seq` () -- all strict atomic fields

instance BinaryEx Trace where
    putEx (Trace a b c) = putEx b <> putEx c <> putEx a
    getEx x | (b,c,a) <- binarySplit2 x = Trace a b c

instance BinaryEx [Trace] where
    putEx = putExList . map putEx
    getEx = map getEx . getExList

newTrace :: String -> Seconds -> Seconds -> Trace
newTrace msg start stop = Trace (BS.pack msg) (doubleToFloat start) (doubleToFloat stop)


---------------------------------------------------------------------
-- CENTRAL TYPES

newtype NoShow a = NoShow a
instance Show (NoShow a) where show _ = "NoShow"

data Status
    = Ready (Result Value) -- ^ I have a value
    | Error SomeException -- ^ I have been run and raised an error
    | Loaded (Result BS.ByteString) -- ^ Loaded from the database
    | Running (NoShow (Either SomeException (Result Value) -> Locked ())) (Maybe (Result BS.ByteString)) -- ^ Currently in the process of being checked or built
    | Missing -- ^ I am only here because I got into the Intern table
      deriving Show

instance NFData Status where
    rnf x = case x of
        Ready x -> rnfResult rnf x
        Error x -> rnf $ show x -- Best I can do for arbitrary exceptions
        Loaded x -> rnfResult id x
        Running _ x -> maybe () (rnfResult id) x -- Can't RNF a waiting, but also unnecessary
        Missing -> ()
        where
            -- ignore the unpacked fields
            -- complex because ByteString lacks NFData in GHC 7.4 and below
            rnfResult by (Result a _ _ b _ c) = by a `seq` rnf b `seq` rnf c `seq` ()
            {-# INLINE rnfResult #-}

data Result a = Result
    {result :: a -- ^ the result associated with the Key
    ,built :: {-# UNPACK #-} !Step -- ^ when it was actually run
    ,changed :: {-# UNPACK #-} !Step -- ^ the step for deciding if it's valid
    ,depends :: [Depends] -- ^ dependencies (don't run them early)
    ,execution :: {-# UNPACK #-} !Float -- ^ how long it took when it was last run (seconds)
    ,traces :: [Trace] -- ^ a trace of the expensive operations (start/end in seconds since beginning of run)
    } deriving (Show,Functor)

statusType Ready{} = "Ready"
statusType Error{} = "Error"
statusType Loaded{} = "Loaded"
statusType Running{} = "Running"
statusType Missing{} = "Missing"


getResult :: Status -> Maybe (Result (Either BS.ByteString Value))
getResult (Ready r) = Just $ Right <$> r
getResult (Loaded r) = Just $ Left <$> r
getResult (Running _ r) = fmap Left <$> r
getResult _ = Nothing


---------------------------------------------------------------------
-- OPERATIONS

newtype Depends = Depends {fromDepends :: [Id]}
    deriving NFData

instance Show Depends where
    -- Appears in diagnostic output and the Depends ctor is just verbose
    show = show . fromDepends

instance BinaryEx Depends where
    putEx (Depends xs) = putExStorableList xs
    getEx = Depends . getExStorableList

instance BinaryEx [Depends] where
    putEx = putExList . map putEx
    getEx = map getEx . getExList

-- | Afterwards each Id must occur at most once and there are no empty Depends
nubDepends :: [Depends] -> [Depends]
nubDepends = fMany Set.empty
    where
        fMany seen [] = []
        fMany seen (Depends d:ds) = [Depends d2 | d2 /= []] ++ fMany seen2 ds
            where (d2,seen2) = fOne seen d

        fOne seen [] = ([], seen)
        fOne seen (x:xs) | x `Set.member` seen = fOne seen xs
        fOne seen (x:xs) = first (x:) $ fOne (Set.insert x seen) xs


-- | Define a rule between @key@ and @value@. As an example, a typical 'BuiltinRun' will look like:
--
-- > run key oldStore mode = do
-- >     ...
-- >     return $ RunResult change newStore newValue
--
--   Where you have:
--
-- * @key@, how to identify individual artifacts, e.g. with file names.
--
-- * @oldStore@, the value stored in the database previously, e.g. the file modification time.
--
-- * @mode@, either 'RunDependenciesSame' (none of your dependencies changed, you can probably not rebuild) or
--   'RunDependenciesChanged' (your dependencies changed, probably rebuild).
--
-- * @change@, usually one of either 'ChangedNothing' (no work was required) or 'ChangedRecomputeDiff'
--   (I reran the rule and it should be considered different).
--
-- * @newStore@, the new value to store in the database, which will be passed in next time as @oldStore@.
--
-- * @newValue@, the result that 'Development.Shake.Rule.apply' will return when asked for the given @key@.
type BuiltinRun key value
    = key
    -> Maybe BS.ByteString
    -> RunMode
    -> Action (RunResult value)

-- | The action performed by @--lint@ for a given @key@/@value@ pair.
--   At the end of the build the lint action will be called for each @key@ that was built this run,
--   passing the @value@ it produced. Return 'Nothing' to indicate the value has not changed and
--   is acceptable, or 'Just' an error message to indicate failure.
--
--   For builtin rules where the value is expected to change, or has no useful checks to perform.
--   use 'Development.Shake.Rules.noLint'.
type BuiltinLint key value = key -> value -> IO (Maybe String)


-- | Produce an identity for a @value@ that can be used to do direct equality. If you have a custom
--   notion of equality then the result should return only one member from each equivalence class,
--   as values will be compared for literal equality.
--   The result of the identity should be reasonably short (if it is excessively long, hash it).
--
--   For rules where the value is never compatible use 'Development.Shake.Rules.noIdentity' and
--   make sure to call 'Development.Shake.historyDisable' if you are ever depended upon.
type BuiltinIdentity key value = key -> value -> BS.ByteString

data BuiltinRule = BuiltinRule
    {builtinLint :: BuiltinLint Key Value
    ,builtinIdentity :: BuiltinIdentity Key Value
    ,builtinRun :: BuiltinRun Key Value
    ,builtinKey :: BinaryOp Key
    ,builtinVersion :: Ver
    ,builtinLocation :: String
    }

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
    | Versioned Ver (UserRule a) -- ^ Rule defined under 'versioned', attaches a version.
      deriving (Eq,Show,Functor,Typeable)

data UserRuleVersioned a = UserRuleVersioned
    {userRuleVersioned :: Bool -- ^ Does Versioned exist anywhere within userRuleContents
    ,userRuleContents :: UserRule a -- ^ The actual rules
    }

instance Semigroup (UserRuleVersioned a) where
    UserRuleVersioned b1 x1 <> UserRuleVersioned b2 x2 = UserRuleVersioned (b1 || b2) (x1 <> x2)

instance Monoid (UserRuleVersioned a) where
    mempty = UserRuleVersioned False mempty
    mappend = (<>)

instance Semigroup (UserRule a) where
    x <> y = Unordered $ fromUnordered x ++ fromUnordered y
        where
            fromUnordered (Unordered xs) = xs
            fromUnordered x = [x]

instance Monoid (UserRule a) where
    mempty = Unordered []
    mappend = (<>)


-- | Invariant: The database does not have any cycles where a Key depends on itself.
--   Everything is mutable. intern and status must form a bijecttion.
--   There may be dangling Id's as a result of version changes.
data Database = Database
    {intern :: IORef (Intern Key) -- ^ Key |-> Id mapping
    ,status :: Ids.Ids (Key, Status) -- ^ Id |-> (Key, Status) mapping
    ,journal :: Id -> Key -> Result BS.ByteString -> IO () -- ^ Record all changes to status
    }


-- global constants of Action
data Global = Global
    {globalDatabase :: Var Database -- ^ Database, contains knowledge of the state of each key
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
    ,globalUserRules :: TMap.Map UserRuleVersioned
    ,globalHistory :: Maybe History -- ^ The active history, if any
    ,globalStep :: {-# UNPACK #-} !Step
    }

-- local variables of Action
data Local = Local
    -- constants
    {localStack :: Stack -- ^ The stack that ran to get here.
    ,localBuiltinVersion :: Ver -- ^ The builtinVersion of the rule you are running
    -- stack scoped local variables
    ,localVerbosity :: Verbosity -- ^ Verbosity, may be changed locally
    ,localBlockApply ::  Maybe String -- ^ Reason to block apply, or Nothing to allow
    -- mutable local variables
    ,localDepends :: [Depends] -- ^ Dependencies, built up in reverse
    ,localDiscount :: !Seconds -- ^ Time spend building dependencies (may be negative for parallel)
    ,localTraces :: [Trace] -- ^ Traces, built in reverse
    ,localTrackAllows :: [Key -> Bool] -- ^ Things that are allowed to be used
    ,localTrackUsed :: [Key] -- ^ Things that have been used
    ,localProduces :: [(Bool, FilePath)] -- ^ Things this rule produces, True to check them
    ,localHistory :: !Bool -- ^ Is it valid to cache the result
    }

addDiscount :: Seconds -> Local -> Local
addDiscount s l = l{localDiscount = s + localDiscount l}

newLocal :: Stack -> Verbosity -> Local
newLocal stack verb = Local stack (Ver 0) verb Nothing [] 0 [] [] [] [] True

-- Clear all the local mutable variables
localClearMutable :: Local -> Local
localClearMutable Local{..} = (newLocal localStack localVerbosity){localBlockApply=localBlockApply, localBuiltinVersion=localBuiltinVersion}

-- Merge, works well assuming you clear the variables first
localMergeMutable :: Local -> [Local] -> Local
-- don't construct with RecordWildCards so any new fields raise an error
localMergeMutable root xs = Local
    -- immutable/stack that need copying
    {localStack = localStack root
    ,localBuiltinVersion = localBuiltinVersion root
    ,localVerbosity = localVerbosity root
    ,localBlockApply = localBlockApply root
    -- mutable locals that need integrating
        -- note that a lot of the lists are stored in reverse, assume root happened first
    ,localDepends =  concatMap localDepends xs ++ localDepends root
    ,localDiscount = sum $ map localDiscount $ root : xs
    ,localTraces = concatMap localTraces xs ++ localTraces root
    ,localTrackAllows = localTrackAllows root ++ concatMap localTrackAllows xs
    ,localTrackUsed = localTrackUsed root ++ concatMap localTrackUsed xs
    ,localProduces = concatMap localProduces xs ++ localProduces root
    ,localHistory = all localHistory $ root:xs
    }
