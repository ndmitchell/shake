{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, PatternGuards #-}

-- | Types exposed to the user
module Development.Shake.Types(
    Progress(..), Verbosity(..), Assume(..), Lint(..), Change(..), EqualCost(..),
    ShakeOptions(..), shakeOptions,
    Targets, filePaths
    ) where

import Data.Data
import Data.List
import Data.Maybe (maybeToList)
import Development.Shake.Progress
import qualified Data.ByteString.Char8 as BS


-- | The current assumptions made by the build system, used by 'shakeAssume'. These options
--   allow the end user to specify that any rules run are either to be treated as clean, or as
--   dirty, regardless of what the build system thinks.
--
--   These assumptions only operate on files reached by the current 'Development.Shake.action' commands. Any
--   other files in the database are left unchanged.
data Assume
    = AssumeDirty
        -- ^ Assume that all rules reached are dirty and require rebuilding, equivalent to 'Development.Shake.storedValue' always
        --   returning 'Nothing'. Useful to undo the results of 'AssumeClean', for benchmarking rebuild speed and
        --   for rebuilding if untracked dependencies have changed. This assumption is safe, but may cause
        --   more rebuilding than necessary.
    | AssumeClean
        -- ^ /This assumption is unsafe, and may lead to incorrect build results in this run, and in future runs/.
        --   Assume and record that all rules reached are clean and do not require rebuilding, provided the rule
        --   has a 'Development.Shake.storedValue' and has been built before. Useful if you have modified a file in some
        --   inconsequential way, such as only the comments or whitespace, and wish to avoid a rebuild.
    | AssumeSkip
        -- ^ /This assumption is unsafe, and may lead to incorrect build results in this run/.
        --   Assume that all rules reached are clean in this run. Only useful for benchmarking, to remove any overhead
        --   from running 'Development.Shake.storedValue' operations.
    deriving (Eq,Ord,Show,Data,Typeable,Bounded,Enum)


-- | Which lint checks to perform, used by 'shakeLint'.
data Lint
    = LintBasic
        -- ^ The most basic form of linting. Checks that the current directory does not change and that results do not change after they
        --   are first written. Any calls to 'needed' will assert that they do not cause a rule to be rebuilt.
    | LintTracker
        -- ^ Track which files are accessed by command line programs run by 'command' or 'cmd', using @tracker.exe@ as supplied
        --   with the Microsoft .NET 4.5 SDK (Windows only). Also performs all checks from 'LintBasic'. Note that some programs are not
        --   tracked properly, particularly cygwin programs (it seems).
    deriving (Eq,Ord,Show,Data,Typeable,Bounded,Enum)


-- | How should you determine if a file has changed, used by 'shakeChange'. The most common values are
--   'ChangeModtime' (very fast, @touch@ causes files to rebuild) and 'ChangeModtimeAndDigestInput'
--   (a bit slower, @touch@ does not cause input files to rebuild).
data Change
    = ChangeModtime
        -- ^ Compare equality of modification timestamps, a file has changed if its last modified time changes.
        --   A @touch@ will force a rebuild. This mode is fast and usually sufficiently accurate, so is the default.
    | ChangeDigest
        -- ^ Compare equality of file contents digests, a file has changed if its digest changes.
        --   A @touch@ will not force a rebuild. Use this mode if modification times on your file system are unreliable.
    | ChangeModtimeAndDigest
        -- ^ A file is rebuilt if both its modification time and digest have changed. For efficiency reasons, the modification
        --   time is checked first, and if that has changed, the digest is checked.
    | ChangeModtimeAndDigestInput
        -- ^ Use 'ChangeModtimeAndDigest' for input\/source files and 'ChangeModtime' for output files.
    | ChangeModtimeOrDigest
        -- ^ A file is rebuilt if either its modification time or its digest has changed. A @touch@ will force a rebuild,
        --   but even if a files modification time is reset afterwards, changes will also cause a rebuild.
    deriving (Eq,Ord,Show,Data,Typeable,Bounded,Enum)


-- | Options to control the execution of Shake, usually specified by overriding fields in
--   'shakeOptions':
--
--   @ 'shakeOptions'{'shakeThreads'=4, 'shakeReport'=[\"report.html\"]} @
--
--   The 'Data' instance for this type reports the 'shakeProgress' and 'shakeOutput' fields as having the abstract type 'Function',
--   because 'Data' cannot be defined for functions.
data ShakeOptions = ShakeOptions
    {shakeFiles :: FilePath
        -- ^ Defaults to @.shake@. The prefix of the filename used for storing Shake metadata files.
        --   All metadata files will be named @'shakeFiles'./extension/@, for some @/extension/@.
        --   If the 'shakeFiles' directory does not exist it will be created.
    ,shakeThreads :: Int
        -- ^ Defaults to @1@. Maximum number of rules to run in parallel, similar to @make --jobs=/N/@.
        --   For many build systems, a number equal to or slightly less than the number of physical processors
        --   works well.
    ,shakeVersion :: String
        -- ^ Defaults to @"1"@. The version number of your build rules.
        --   Change the version number to force a complete rebuild, such as when making
        --   significant changes to the rules that require a wipe. The version number should be
        --   set in the source code, and not passed on the command line.
    ,shakeVerbosity :: Verbosity
        -- ^ Defaults to 'Normal'. What level of messages should be printed out.
    ,shakeStaunch :: Bool
        -- ^ Defaults to 'False'. Operate in staunch mode, where building continues even after errors,
        --   similar to @make --keep-going@.
    ,shakeReport :: [FilePath]
        -- ^ Defaults to @[]@. Write a profiling report to a file, showing which rules rebuilt,
        --   why, and how much time they took. Useful for improving the speed of your build systems.
        --   If the file extension is @.json@ it will write JSON data; if @.js@ it will write Javascript;
        --   if @.trace@ it will write trace events (load into @about:\/\/tracing@ in Chrome);
        --   otherwise it will write HTML.
    ,shakeLint :: Maybe Lint
        -- ^ Defaults to 'Nothing'. Perform sanity checks during building, see 'Lint' for details.
    ,shakeFlush :: Maybe Double
        -- ^ Defaults to @'Just' 10@. How often to flush Shake metadata files in seconds, or 'Nothing' to never flush explicitly.
        --   It is possible that on abnormal termination (not Haskell exceptions) any rules that completed in the last
        --   'shakeFlush' seconds will be lost.
    ,shakeAssume :: Maybe Assume
        -- ^ Defaults to 'Nothing'. Assume all build objects are clean/dirty, see 'Assume' for details.
        --   Can be used to implement @make --touch@.
    ,shakeAbbreviations :: [(String,String)]
        -- ^ Defaults to @[]@. A list of substrings that should be abbreviated in status messages, and their corresponding abbreviation.
        --   Commonly used to replace the long paths (e.g. @.make\/i586-linux-gcc\/output@) with an abbreviation (e.g. @$OUT@).
    ,shakeStorageLog :: Bool
        -- ^ Defaults to 'False'. Write a message to @'shakeFiles'.storage@ whenever a storage event happens which may impact
        --   on the current stored progress. Examples include database version number changes, database compaction or corrupt files.
    ,shakeLineBuffering :: Bool
        -- ^ Defaults to 'True'. Change 'stdout' and 'stderr' to line buffering while running Shake.
    ,shakeTimings :: Bool
        -- ^ Defaults to 'False'. Print timing information for each stage at the end.
    ,shakeRunCommands :: Bool
        -- ^ Default to 'True'. Should you run command line actions, set to 'False' to skip actions whose output streams and exit code
        --   are not used. Useful for profiling the non-command portion of the build system.
    ,shakeChange :: Change
        -- ^ Default to 'ChangeModtime'. How to check if a file has changed, see 'Change' for details.
    ,shakeCreationCheck :: Bool
        -- ^ Default to 'True'. After running a rule to create a file, is it an error if the file does not exist.
        --   Provided for compatibility with @make@ and @ninja@ (which have ugly file creation semantics).
    ,shakeLiveFiles :: [FilePath]
        -- ^ Default to '[]'. After the build system completes, write a list of all files which were /live/ in that run,
        --   i.e. those which Shake checked were valid or rebuilt. Produces best answers if nothing rebuilds.
    ,shakeProgress :: IO Progress -> IO ()
        -- ^ Defaults to no action. A function called when the build starts, allowing progress to be reported.
        --   The function is called on a separate thread, and that thread is killed when the build completes.
        --   For applications that want to display progress messages, 'progressSimple' is often sufficient, but more advanced
        --   users should look at the 'Progress' data type.
    ,shakeOutput :: Verbosity -> String -> IO ()
        -- ^ Defaults to writing using 'putStrLn'. A function called to output messages from Shake, along with the 'Verbosity' at
        --   which that message should be printed. This function will be called atomically from all other 'shakeOutput' functions.
        --   The 'Verbosity' will always be greater than or higher than 'shakeVerbosity'.
    }
    deriving Typeable

-- | The default set of 'ShakeOptions'.
shakeOptions :: ShakeOptions
shakeOptions = ShakeOptions
    ".shake" 1 "1" Normal False [] Nothing (Just 10) Nothing [] False True False
    True ChangeModtime True []
    (const $ return ())
    (const $ BS.putStrLn . BS.pack) -- try and output atomically using BS

fieldsShakeOptions =
    ["shakeFiles", "shakeThreads", "shakeVersion", "shakeVerbosity", "shakeStaunch", "shakeReport"
    ,"shakeLint", "shakeFlush", "shakeAssume", "shakeAbbreviations", "shakeStorageLog"
    ,"shakeLineBuffering", "shakeTimings", "shakeRunCommands", "shakeChange", "shakeCreationCheck"
    ,"shakeLiveFiles","shakeProgress", "shakeOutput"]
tyShakeOptions = mkDataType "Development.Shake.Types.ShakeOptions" [conShakeOptions]
conShakeOptions = mkConstr tyShakeOptions "ShakeOptions" fieldsShakeOptions Prefix
unhide x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 =
    ShakeOptions x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 (fromFunction x18) (fromFunction x19)

instance Data ShakeOptions where
    gfoldl k z (ShakeOptions x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19) =
        z unhide `k` x1 `k` x2 `k` x3 `k` x4 `k` x5 `k` x6 `k` x7 `k` x8 `k` x9 `k` x10 `k` x11 `k`
        x12 `k` x13 `k` x14 `k` x15 `k` x16 `k` x17 `k`
        Function x18 `k` Function x19
    gunfold k z c = k $ k $ k $ k $ k $ k $ k $ k $ k $ k $ k $ k $ k $ k $ k $ k $ k $ k $ k $ z unhide
    toConstr ShakeOptions{} = conShakeOptions
    dataTypeOf _ = tyShakeOptions

instance Show ShakeOptions where
    show x = "ShakeOptions {" ++ intercalate ", " inner ++ "}"
        where
            inner = zipWith (\x y -> x ++ " = " ++ y) fieldsShakeOptions $ gmapQ f x

            f x | Just x <- cast x = show (x :: Int)
                | Just x <- cast x = show (x :: FilePath)
                | Just x <- cast x = show (x :: Verbosity)
                | Just x <- cast x = show (x :: Change)
                | Just x <- cast x = show (x :: Bool)
                | Just x <- cast x = show (x :: [FilePath])
                | Just x <- cast x = show (x :: Maybe Assume)
                | Just x <- cast x = show (x :: Maybe Lint)
                | Just x <- cast x = show (x :: Maybe Double)
                | Just x <- cast x = show (x :: [(String,String)])
                | Just x <- cast x = show (x :: Function (IO Progress -> IO ()))
                | Just x <- cast x = show (x :: Function (Verbosity -> String -> IO ()))
                | otherwise = error $ "Error while showing ShakeOptions, missing alternative for " ++ show (typeOf x)


-- | Internal type, copied from Hide in Uniplate
newtype Function a = Function {fromFunction :: a}
    deriving Typeable

instance Show (Function a) where show _ = "<function>"

instance Typeable a => Data (Function a) where
    gfoldl k z x = z x
    gunfold k z c = error "Development.Shake.Types.ShakeProgress: gunfold not implemented - data type has no constructors"
    toConstr _ = error "Development.Shake.Types.ShakeProgress: toConstr not implemented - data type has no constructors"
    dataTypeOf _ = tyFunction

tyFunction = mkDataType "Development.Shake.Types.Function" []


-- | The verbosity data type, used by 'shakeVerbosity'.
data Verbosity
    = Silent -- ^ Don't print any messages.
    | Quiet  -- ^ Only print essential messages, typically errors.
    | Normal -- ^ Print errors and @# /command-name/ (for /file-name/)@ when running a 'Development.Shake.traced' command.
    | Loud   -- ^ Print errors and full command lines when running a 'Development.Shake.command' or 'Development.Shake.cmd' command.
    | Chatty -- ^ Print errors, full command line and status messages when starting a rule.
    | Diagnostic -- ^ Print messages for virtually everything (mostly for debugging).
      deriving (Eq,Ord,Bounded,Enum,Show,Read,Typeable,Data)

-- | An equality check and a cost.
data EqualCost
    = EqualCheap -- ^ The equality check was cheap.
    | EqualExpensive -- ^ The equality check was expensive, as the results are not trivially equal.
    | NotEqual -- ^ The values are not equal.
      deriving (Eq,Ord,Bounded,Enum,Show,Read,Typeable,Data)


-- | Everything that can be a target or a list of targets
class Targets targets where
    filePaths :: targets -> [FilePath]

instance Targets [FilePath] where
    filePaths = id

instance Targets FilePath where
    filePaths fp = [fp]

instance Targets (Maybe FilePath) where
    filePaths = maybeToList
