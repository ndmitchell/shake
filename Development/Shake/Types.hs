{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, RecordWildCards, PatternGuards #-}

-- | Types exposed to the user
module Development.Shake.Types(
    Progress(..), Verbosity(..), Assume(..),
    ShakeOptions(..), shakeOptions,
    BS, pack, unpack, pack_, unpack_, packU, unpackU
    ) where

import Data.Data
import Data.List
import Development.Shake.Progress
import Development.Shake.Classes
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.UTF8 as UTF8


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


-- | Options to control the execution of Shake, usually specified by overriding fields in
--   'shakeOptions':
--
--   @ 'shakeOptions'{'shakeThreads'=4, 'shakeReport'=Just \"report.html\"} @
--
--   The 'Data' instance for this type reports the 'shakeProgress' and 'shakeOutput' fields as having the abstract type 'Function',
--   because 'Data' cannot be defined for functions.
data ShakeOptions = ShakeOptions
    {shakeFiles :: FilePath
        -- ^ Defaults to @.shake@. The prefix of the filename used for storing Shake metadata files.
        --   All metadata files will be named @'shakeFiles'./extension/@, for some @/extension/@.
    ,shakeThreads :: Int
        -- ^ Defaults to @1@. Maximum number of rules to run in parallel, similar to @make --jobs=/N/@.
        --   To enable parallelism you may need to compile with @-threaded@.
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
    ,shakeReport :: Maybe FilePath
        -- ^ Defaults to 'Nothing'. Write an HTML profiling report to a file, showing which
        --   rules rebuilt, why, and how much time they took. Useful for improving the speed of your build systems.
    ,shakeLint :: Bool
        -- ^ Defaults to 'False'. Perform basic sanity checks during building, checking the current directory
        --   is not modified and that output files are not modified by multiple rules.
        --   These sanity checks do not check for missing or redundant dependencies.
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
        -- ^ Default to 'False'. Print timing information for each stage at the end.
    ,shakeProgress :: IO Progress -> IO ()
        -- ^ Defaults to no action. A function called on a separate thread when the build starts, allowing progress to be reported.
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
shakeOptions = ShakeOptions ".shake" 1 "1" Normal False Nothing False (Just 10) Nothing [] False True False
    (const $ return ())
    (const $ BS.putStrLn . BS.pack) -- try and output atomically using BS

fieldsShakeOptions =
    ["shakeFiles", "shakeThreads", "shakeVersion", "shakeVerbosity", "shakeStaunch", "shakeReport"
    ,"shakeLint", "shakeFlush", "shakeAssume", "shakeAbbreviations", "shakeStorageLog"
    ,"shakeLineBuffering", "shakeTimings", "shakeProgress", "shakeOutput"]
tyShakeOptions = mkDataType "Development.Shake.Types.ShakeOptions" [conShakeOptions]
conShakeOptions = mkConstr tyShakeOptions "ShakeOptions" fieldsShakeOptions Prefix
unhide x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 = ShakeOptions x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 (fromFunction x14) (fromFunction x15)

instance Data ShakeOptions where
    gfoldl k z (ShakeOptions x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) =
        z unhide `k` x1 `k` x2 `k` x3 `k` x4 `k` x5 `k` x6 `k` x7 `k` x8 `k` x9 `k` x10 `k` x11 `k` x12 `k` x13 `k` Function x14 `k` Function x15
    gunfold k z c = k $ k $ k $ k $ k $ k $ k $ k $ k $ k $ k $ k $ k $ k $ k $ z unhide
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
    | Normal -- ^ Print errors and @# /command-name/ /file-name/@ when running a 'Development.Shake.traced' command.
    | Loud   -- ^ Print errors and full command lines when running a 'Development.Shake.system'' command.
    | Chatty -- ^ Print errors, full command line and status messages when starting a rule.
    | Diagnostic -- ^ Print messages for virtually everything (mostly for debugging).
      deriving (Eq,Ord,Bounded,Enum,Show,Read,Typeable,Data)


---------------------------------------------------------------------
-- BYTESTRING WRAPPER
-- Only purpose is because ByteString does not have an NFData instance in older GHC

newtype BS = BS BS.ByteString
    deriving (Hashable, Binary, Eq)

instance NFData BS where
    -- some versions of ByteString do not have NFData instances, but seq is equivalent
    -- for a strict bytestring. Therefore, we write our own instance.
    rnf (BS x) = x `seq` ()

pack :: String -> BS
pack = pack_ . BS.pack

unpack :: BS -> String
unpack = BS.unpack . unpack_

pack_ :: BS.ByteString -> BS
pack_ = BS

unpack_ :: BS -> BS.ByteString
unpack_ (BS x) = x

packU :: String -> BS
packU = pack_ . UTF8.fromString

unpackU :: BS -> String
unpackU = UTF8.toString . unpack_

