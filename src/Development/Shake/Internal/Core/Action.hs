{-# LANGUAGE RecordWildCards, GeneralizedNewtypeDeriving, ScopedTypeVariables, PatternGuards #-}
{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses, ConstraintKinds #-}

module Development.Shake.Internal.Core.Action(
    RuleInfo(..), Global(..), Local(..), Action(..),
    runAction, actionOnException, actionFinally
    ) where

import Control.Exception.Extra
import Control.Applicative
import Control.Monad.Extra
import Control.Monad.IO.Class
import Data.Typeable
import Data.Function
import Data.Either.Extra
import qualified Data.HashMap.Strict as Map
import Data.Maybe
import Data.IORef
import System.IO.Extra
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
--   Action values are used by 'rule' and 'action'. The 'Action' monad tracks the dependencies of a 'Rule'.
newtype Action a = Action {fromAction :: RAW Global Local a}
    deriving (Functor, Applicative, Monad, MonadIO)

data RuleInfo m = RuleInfo
    {stored :: Key -> IO (Maybe Value)
    ,equal :: Key -> Value -> Value -> EqualCost
    ,execute :: Key -> m Value
    ,resultType :: TypeRep
    }

-- global constants of Action
data Global = Global
    {globalDatabase :: Database
    ,globalPool :: Pool
    ,globalCleanup :: Cleanup
    ,globalTimestamp :: IO Seconds
    ,globalRules :: Map.HashMap TypeRep (RuleInfo Action)
    ,globalOutput :: Verbosity -> String -> IO ()
    ,globalOptions  :: ShakeOptions
    ,globalDiagnostic :: String -> IO ()
    ,globalLint :: String -> IO ()
    ,globalAfter :: IORef [IO ()]
    ,globalTrackAbsent :: IORef [(Key, Key)] -- in rule fst, snd must be absent
    ,globalProgress :: IO Progress
    }

-- local variables of Action
data Local = Local
    -- constants
    {localStack :: Stack
    -- stack scoped local variables
    ,localVerbosity :: Verbosity
    ,localBlockApply ::  Maybe String -- reason to block apply, or Nothing to allow
    -- mutable local variables
    ,localDepends :: [Depends] -- built up in reverse
    ,localDiscount :: !Seconds
    ,localTraces :: [Trace] -- in reverse
    ,localTrackAllows :: [Key -> Bool]
    ,localTrackUsed :: [Key]
    }

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
