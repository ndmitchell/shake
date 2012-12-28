{-# LANGUAGE DeriveDataTypeable #-}

-- | Types exposed to the user
module Development.Shake.Types where

import Control.Exception
import Data.Data


-- | Information about the current state of the build, obtained by passing a callback function
--   to 'shakeStatistics'. Typically a program will poll this value to provide progress messages.
data ShakeStatistics = ShakeStatistics
    {shakeRunning :: !Bool -- ^ Starts out True, becomes False once the build has completed
    ,shakeError :: !Bool -- ^ Has any rule failed to build, does not always indicate the build will fail (but usually it will)
    ,shakeSkipped :: {-# UNPACK #-} !Int -- ^ Number of rules which were required, but were already in a valid state
    ,shakeBuilt :: {-# UNPACK #-} !Int -- ^ Number of rules which were have been built in this run
    ,shakeTodo :: {-# UNPACK #-} !Int -- ^ Number of rules which are currently required (ignoring dependencies that do not change), but not built
    ,shakeUnknown :: {-# UNPACK #-} !Int -- ^ Number of rules which have been built previously, but are not yet known to be required
    ,shakeSkippedTime :: {-# UNPACK #-} !Double -- ^ Time spent building 'shakeSkipped' rules in previous runs
    ,shakeBuiltTime :: {-# UNPACK #-} !Double -- ^ Time spent building 'shakeBuilt' rules
    ,shakeTodoTime :: {-# UNPACK #-} !Double -- ^ Time spent building 'shakeTodo' rules in a previous runs, where known (see 'shakeTodoNoTime')
    ,shakeUnknownTime :: {-# UNPACK #-} !Double -- ^ Time spent building 'shakeUnknownTime' rules in previous runs
    ,shakeTodoNoTime :: {-# UNPACK #-} !Int -- ^ Number of rules in 'shakeTodo' which have no known time (e.g. never built before)
    }
    deriving (Eq,Ord,Show,Data,Typeable)


data Assume = AssumeClean | AssumeDirty deriving (Eq,Ord,Show,Data,Typeable,Bounded,Enum)


-- | Options to control the execution of Shake, usually specified by overriding fields in
--   'shakeOptions':
--
--   @ 'shakeOptions'{'shakeThreads'=4, 'shakeReport'=Just \"report.html\"} @
data ShakeOptions = ShakeOptions
    {shakeFiles :: FilePath -- ^ Where shall I store the database and journal files (defaults to @.shake@).
    ,shakeThreads :: Int -- ^ What is the maximum number of rules I should run in parallel (defaults to @1@).
                         --   To enable parallelism you may need to compile with @-threaded@.
    ,shakeVersion :: Int -- ^ What is the version of your build system, increment to force a complete rebuild (defaults to @1@).
    ,shakeVerbosity :: Verbosity -- ^ What messages to print out (defaults to 'Normal').
    ,shakeStaunch :: Bool -- ^ Operate in staunch mode, where building continues even after errors (defaults to 'False').
    ,shakeReport :: Maybe FilePath -- ^ Produce an HTML profiling report (defaults to 'Nothing').
    ,shakeLint :: Bool -- ^ Perform basic sanity checks after building (defaults to 'False').
    ,shakeDeterministic :: Bool -- ^ Build files in a deterministic order, as far as possible (defaults to 'False')
    ,shakeAssume :: Maybe Assume -- ^ TODO
    ,shakeStatistics :: IO ShakeStatistics -> IO () -- ^ A function called when the build starts, including a way of obtaining
                                                    --   information about the current state of the build
    }
    deriving Typeable

-- | The default set of 'ShakeOptions'.
shakeOptions :: ShakeOptions
shakeOptions = ShakeOptions ".shake" 1 1 Normal False Nothing False False Nothing (const $ return ())


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


-- | The verbosity data type, used by 'shakeVerbosity'.
data Verbosity
    = Silent -- ^ Don't print any messages.
    | Quiet  -- ^ Only print essential messages (typically errors).
    | Normal -- ^ Print normal messages (typically errors and warnings).
    | Loud   -- ^ Print lots of messages (typically errors, warnings and status updates).
    | Diagnostic -- ^ Print messages for virtually everything (for debugging a build system).
      deriving (Eq,Ord,Bounded,Enum,Show,Read,Typeable,Data)

