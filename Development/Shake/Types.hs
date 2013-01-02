{-# LANGUAGE DeriveDataTypeable, RecordWildCards, PatternGuards #-}

-- | Types exposed to the user
module Development.Shake.Types(
    Progress(..), Verbosity(..), Assume(..),
    ShakeOptions(..), shakeOptions,
    ShakeException(..)
    ) where

import Control.Exception
import Data.Data
import Data.List


-- | Information about the current state of the build, obtained by passing a callback function
--   to 'shakeProgress'. Typically a program will poll this value to provide progress messages.
--   The following example displays the approximate single-threaded time remaining
--   as the console title.
--
-- > showProgress :: IO Progress -> IO ()
-- > showProgress progress = void $ forkIO loop
-- >     where loop = do
-- >             current <- progress
-- >             when (isRunning current) $ do
-- >                 let (s,c) = timeTodo current
-- >                 setTitle $ "Todo = " ++ show (ceiling s) ++ "s (+ " ++ show c ++ " unknown)"
-- >                 threadDelay $ 5 * 1000000
-- >                 loop
-- >
-- > setTitle :: String -> IO ()
-- > setTitle s = putStr $ "\ESC]0;" ++ s ++ "\BEL"
data Progress = Progress
    {isRunning :: !Bool -- ^ Starts out 'True', becomes 'False' once the build has completed.
    ,countSkipped :: {-# UNPACK #-} !Int -- ^ Number of rules which were required, but were already in a valid state.
    ,countBuilt :: {-# UNPACK #-} !Int -- ^ Number of rules which were have been built in this run.
    ,countUnknown :: {-# UNPACK #-} !Int -- ^ Number of rules which have been built previously, but are not yet known to be required.
    ,countTodo :: {-# UNPACK #-} !Int -- ^ Number of rules which are currently required (ignoring dependencies that do not change), but not built.
    ,timeSkipped :: {-# UNPACK #-} !Double -- ^ Time spent building 'countSkipped' rules in previous runs.
    ,timeBuilt :: {-# UNPACK #-} !Double -- ^ Time spent building 'countBuilt' rules.
    ,timeUnknown :: {-# UNPACK #-} !Double -- ^ Time spent building 'countUnknown' rules in previous runs.
    ,timeTodo :: {-# UNPACK #-} !(Double,Int) -- ^ Time spent building 'countTodo' rules in previous runs, plus the number which have no known time (e.g. never built before).
    }
    deriving (Eq,Ord,Show,Data,Typeable)


-- | The current assumptions made by the build system, used by 'shakeAssume'. These options
--   allow the end user to specify that any rules run are either to be treated as clean, or as
--   dirty, regardless of what the build system thinks.
--
--   These assumptions only operate on files reached by the current 'action' commands. Any
--   other files in the database are left unchanged.
data Assume
    = AssumeDirty
        -- ^ Assume that all rules reached are dirty and require rebuilding, equivalent to 'storedValue' always
        --   returning 'Nothing'. Useful to undo the results of 'AssumeClean', for benchmarking rebuild speed and
        --   for rebuilding if untracked dependencies have changed. This assumption is safe, but may cause
        --   more rebuilding than necessary.
    | AssumeClean
        -- ^ /This assumption is unsafe, and may lead to incorrect build results/.
        --   Assume that all rules reached are clean and do not require rebuilding, provided the rule
        --   has a 'storedValue'. Useful if you have modified a file in some inconsequential way, such as only
        --   the comments or whitespace, and wish to avoid a rebuild.
    deriving (Eq,Ord,Show,Data,Typeable,Bounded,Enum)


-- | Options to control the execution of Shake, usually specified by overriding fields in
--   'shakeOptions':
--
--   @ 'shakeOptions'{'shakeThreads'=4, 'shakeReport'=Just \"report.html\"} @
--
--   The 'Data' instance for this type reports the 'shakeProgress' field as having the abstract type 'ShakeProgress',
--   because 'Data' cannot be defined for functions.
data ShakeOptions = ShakeOptions
    {shakeFiles :: FilePath -- ^ Where shall I store the database and journal files (defaults to @.shake@).
    ,shakeThreads :: Int -- ^ What is the maximum number of rules I should run in parallel (defaults to @1@).
                         --   To enable parallelism you may need to compile with @-threaded@.
    ,shakeVersion :: Int -- ^ What is the version of your build system, increment to force a complete rebuild (defaults to @1@).
    ,shakeVerbosity :: Verbosity -- ^ What messages to print out (defaults to 'Normal').
    ,shakeStaunch :: Bool -- ^ Operate in staunch mode, where building continues even after errors (defaults to 'False').
    ,shakeReport :: Maybe FilePath -- ^ Write an HTML profiling report to a file (defaults to 'Nothing').
    ,shakeLint :: Bool -- ^ Perform basic sanity checks after building (defaults to 'False').
    ,shakeDeterministic :: Bool -- ^ Run rules in a deterministic order, as far as possible (defaults to 'False').
    ,shakeFlush :: Maybe Double -- ^ How often to flush the journal file in seconds, or 'Nothing' to never flush explicitly (defaults to 'Just' 10)
    ,shakeAssume :: Maybe Assume -- ^ Assume all build objects are clean/dirty, see 'Assume' for details (defaults to 'Nothing').
    ,shakeProgress :: IO Progress -> IO ()
        -- ^ A function called when the build starts, allowing progress to be reported, see 'Progress' for details (defaults to no action).
    }
    deriving Typeable

-- | The default set of 'ShakeOptions'.
shakeOptions :: ShakeOptions
shakeOptions = ShakeOptions ".shake" 1 1 Normal False Nothing False False (Just 10) Nothing (const $ return ())

fieldsShakeOptions =
    ["shakeFiles", "shakeThreads", "shakeVersion", "shakeVerbosity", "shakeStaunch", "shakeReport"
    ,"shakeLint", "shakeDeterministic", "shakeFlush", "shakeAssume", "shakeProgress"]
tyShakeOptions = mkDataType "Development.Shake.Types.ShakeOptions" [conShakeOptions]
conShakeOptions = mkConstr tyShakeOptions "ShakeOptions" fieldsShakeOptions Prefix
unhide x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 = ShakeOptions x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 (fromProgress x11)

instance Data ShakeOptions where
    gfoldl k z (ShakeOptions x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11) =
        z unhide `k` x1 `k` x2 `k` x3 `k` x4 `k` x5 `k` x6 `k` x7 `k` x8 `k` x9 `k` x10 `k` ShakeProgress x11
    gunfold k z c = k $ k $ k $ k $ k $ k $ k $ k $ k $ k $ k $ z unhide
    toConstr ShakeOptions{} = conShakeOptions
    dataTypeOf _ = tyShakeOptions

instance Show ShakeOptions where
    show x = "ShakeOptions {" ++ intercalate ", " inner ++ "}"
        where
            inner = zipWith (\x y -> x ++ " = " ++ y) fieldsShakeOptions $ gmapQ f x

            f x | Just x <- cast x = show (x :: Int)
                | Just x <- cast x = show (x :: FilePath)
                | Just x <- cast x = show (x :: Verbosity)
                | Just x <- cast x = show (x :: Bool)
                | Just x <- cast x = show (x :: Maybe FilePath)
                | Just x <- cast x = show (x :: Maybe Assume)
                | Just x <- cast x = show (x :: Maybe Double)
                | Just x <- cast x = show (x :: ShakeProgress)
                | otherwise = error $ "Error while showing ShakeOptions, missing alternative for " ++ show (typeOf x)


-- | Internal type, copied from Hide in Uniplate
newtype ShakeProgress = ShakeProgress {fromProgress :: IO Progress -> IO ()}
    deriving Typeable

instance Show ShakeProgress where show _ = "<function>"

instance Data ShakeProgress where
    gfoldl k z x = z x
    gunfold k z c = error "Development.Shake.Types.ShakeProgress: gunfold not implemented - data type has no constructors"
    toConstr _ = error "Development.Shake.Types.ShakeProgress: toConstr not implemented - data type has no constructors"
    dataTypeOf _ = tyShakeProgress

tyShakeProgress = mkDataType "Development.Shake.Types.ShakeProgress" []



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

