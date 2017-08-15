{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables, DeriveDataTypeable, FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds, TypeFamilies #-}

-- | Both System.Directory and System.Environment wrappers
module Development.Shake.Internal.Rules.Directory(
    doesFileExist, doesDirectoryExist,
    getDirectoryContents, getDirectoryFiles, getDirectoryDirs,
    getEnv, getEnvWithDefault,
    removeFiles, removeFilesAfter,
    getDirectoryFilesIO,
    defaultRuleDirectory
    ) where

import Control.Applicative
import Control.Exception as C
import Control.Monad.Extra
import Control.Monad.IO.Class
import Data.Maybe
import Data.Binary
import Data.List
import Data.Tuple.Extra
import qualified Data.HashSet as Set
import qualified System.Directory as IO
import qualified System.Environment.Extra as IO

import Development.Shake.Internal.Core.Types
import Development.Shake.Internal.Core.Run
import Development.Shake.Internal.Core.Rules
import Development.Shake.Internal.Value
import Development.Shake.Classes
import Development.Shake.FilePath
import Development.Shake.Internal.FilePattern
import General.Extra
import General.Binary
import Prelude


---------------------------------------------------------------------
-- KEY/VALUE TYPES

type instance RuleResult DoesFileExistQ = DoesFileExistA

newtype DoesFileExistQ = DoesFileExistQ FilePath
    deriving (Typeable,Eq,Hashable,Binary,BinaryEx,NFData)

instance Show DoesFileExistQ where
    show (DoesFileExistQ a) = "doesFileExist " ++ wrapQuote a

newtype DoesFileExistA = DoesFileExistA {fromDoesFileExistA :: Bool}
    deriving (Typeable,Eq,Hashable,Binary,BinaryEx,NFData)

instance Show DoesFileExistA where
    show (DoesFileExistA a) = show a

type instance RuleResult DoesDirectoryExistQ = DoesDirectoryExistA

newtype DoesDirectoryExistQ = DoesDirectoryExistQ FilePath
    deriving (Typeable,Eq,Hashable,Binary,BinaryEx,NFData)

instance Show DoesDirectoryExistQ where
    show (DoesDirectoryExistQ a) = "doesDirectoryExist " ++ wrapQuote a

newtype DoesDirectoryExistA = DoesDirectoryExistA {fromDoesDirectoryExistA :: Bool}
    deriving (Typeable,Eq,Hashable,Binary,BinaryEx,NFData)

instance Show DoesDirectoryExistA where
    show (DoesDirectoryExistA a) = show a


type instance RuleResult GetEnvQ = GetEnvA

newtype GetEnvQ = GetEnvQ String
    deriving (Typeable,Eq,Hashable,Binary,BinaryEx,NFData)

instance Show GetEnvQ where
    show (GetEnvQ a) = "getEnv " ++ wrapQuote a

newtype GetEnvA = GetEnvA {fromGetEnvA :: Maybe String}
    deriving (Typeable,Eq,Hashable,Binary,BinaryEx,NFData)

instance Show GetEnvA where
    show (GetEnvA a) = maybe "<unset>" wrapQuote a


type instance RuleResult GetDirectoryContentsQ = GetDirectoryA
type instance RuleResult GetDirectoryFilesQ = GetDirectoryA
type instance RuleResult GetDirectoryDirsQ = GetDirectoryA

newtype GetDirectoryContentsQ = GetDirectoryContentsQ FilePath
    deriving (Typeable,Eq,Hashable,Binary,BinaryEx,NFData)

instance Show GetDirectoryContentsQ where
    show (GetDirectoryContentsQ dir) = "getDirectoryContents " ++ wrapQuote dir

newtype GetDirectoryFilesQ = GetDirectoryFilesQ (FilePath, [FilePattern])
    deriving (Typeable,Eq,Hashable,Binary,BinaryEx,NFData)

instance Show GetDirectoryFilesQ where
    show (GetDirectoryFilesQ (dir, pat)) = "getDirectoryFiles " ++ wrapQuote dir ++ " [" ++ unwords (map wrapQuote pat) ++ "]"

newtype GetDirectoryDirsQ = GetDirectoryDirsQ FilePath
    deriving (Typeable,Eq,Hashable,Binary,BinaryEx,NFData)

instance Show GetDirectoryDirsQ where
    show (GetDirectoryDirsQ dir) = "getDirectoryDirs " ++ wrapQuote dir

newtype GetDirectoryA = GetDirectoryA {fromGetDirectoryA :: [FilePath]}
    deriving (Typeable,Eq,Hashable,Binary,BinaryEx,NFData)

instance Show GetDirectoryA where
    show (GetDirectoryA xs) = unwords $ map wrapQuote xs


---------------------------------------------------------------------
-- RULE DEFINITIONS

queryRule :: (RuleResult key ~ value, BinaryEx key, BinaryEx witness, Eq witness, ShakeValue key, ShakeValue value)
          => (value -> witness) -> (BuiltinSummary key value) -> (key -> IO value) -> Rules ()
queryRule witness summary query = addBuiltinRuleEx newBinaryOp
    (\k old -> do
        new <- query k
        return $ if old == new then Nothing else Just $ show new)
    summary
    (\k old _ -> liftIO $ do
        new <- query k
        let wnew = witness new
        return $ case old of
            Just old | wnew == getEx old -> RunResult ChangedNothing old new
            _ -> RunResult ChangedRecomputeDiff (runBuilder $ putEx wnew) new)


defaultRuleDirectory :: Rules ()
defaultRuleDirectory = do
    -- for things we are always going to rerun, and which might take up a lot of memory to store,
    -- we only store their hash, so we can compute change, but not know what changed happened
    queryRule id showSummary (\(DoesFileExistQ x) -> DoesFileExistA <$> IO.doesFileExist x)
    queryRule id showSummary (\(DoesDirectoryExistQ x) -> DoesDirectoryExistA <$> IO.doesDirectoryExist x)
    queryRule hash binarySummary (\(GetEnvQ x) -> GetEnvA <$> IO.lookupEnv x)
    queryRule hash binarySummary (\(GetDirectoryContentsQ x) -> GetDirectoryA <$> getDirectoryContentsIO x)
    queryRule hash binarySummary (\(GetDirectoryFilesQ (a,b)) -> GetDirectoryA <$> getDirectoryFilesIO a b)
    queryRule hash binarySummary (\(GetDirectoryDirsQ x) -> GetDirectoryA <$> getDirectoryDirsIO x)

---------------------------------------------------------------------
-- RULE ENTRY POINTS

-- | Returns 'True' if the file exists. The existence of the file is tracked as a
--   dependency, and if the file is created or deleted the rule will rerun in subsequent builds.
--
--   You should not call 'doesFileExist' on files which can be created by the build system.
doesFileExist :: FilePath -> Action Bool
doesFileExist = fmap fromDoesFileExistA . apply1 . DoesFileExistQ . toStandard

-- | Returns 'True' if the directory exists. The existence of the directory is tracked as a
--   dependency, and if the directory is created or delete the rule will rerun in subsequent builds.
--
--   You should not call 'doesDirectoryExist' on directories which can be created by the build system.
doesDirectoryExist :: FilePath -> Action Bool
doesDirectoryExist = fmap fromDoesDirectoryExistA . apply1 . DoesDirectoryExistQ . toStandard

-- | Return 'Just' the value of the environment variable, or 'Nothing'
--   if the variable is not set. The environment variable is tracked as a
--   dependency, and if it changes the rule will rerun in subsequent builds.
--   This function is a tracked version of 'getEnv' / 'lookupEnv' from the base library.
--
-- @
-- flags <- getEnv \"CFLAGS\"
-- 'cmd' \"gcc -c\" [out] (maybe [] words flags)
-- @
getEnv :: String -> Action (Maybe String)
getEnv = fmap fromGetEnvA . apply1 . GetEnvQ

-- | Return the value of the environment variable (second argument), or the
--   default value (first argument) if it is not set. Similar to 'getEnv'.
--
-- @
-- flags <- getEnvWithDefault \"-Wall\" \"CFLAGS\"
-- 'cmd' \"gcc -c\" [out] flags
-- @
getEnvWithDefault :: String -> String -> Action String
getEnvWithDefault def var = fromMaybe def <$> getEnv var

-- | Get the contents of a directory. The result will be sorted, and will not contain
--   the entries @.@ or @..@ (unlike the standard Haskell version). The resulting paths will be relative
--   to the first argument. The result is tracked as a
--   dependency, and if it changes the rule will rerun in subsequent builds.
--
--   It is usually simpler to call either 'getDirectoryFiles' or 'getDirectoryDirs'.
getDirectoryContents :: FilePath -> Action [FilePath]
getDirectoryContents = fmap fromGetDirectoryA . apply1 . GetDirectoryContentsQ

-- | Get the files anywhere under a directory that match any of a set of patterns.
--   For the interpretation of the patterns see '?=='. All results will be
--   relative to the directory argument. The result is tracked as a
--   dependency, and if it changes the rule will rerun in subsequent builds.
--   Some examples:
--
-- > getDirectoryFiles "Config" ["//*.xml"]
-- >     -- All .xml files anywhere under the Config directory
-- >     -- If Config/foo/bar.xml exists it will return ["foo/bar.xml"]
-- > getDirectoryFiles "Modules" ["*.hs","*.lhs"]
-- >     -- All .hs or .lhs in the Modules directory
-- >     -- If Modules/foo.hs and Modules/foo.lhs exist, it will return ["foo.hs","foo.lhs"]
--
--   If you require a qualified file name it is often easier to use @\"\"@ as the 'FilePath' argument,
--   for example the following two expressions are equivalent:
--
-- > fmap (map ("Config" </>)) (getDirectoryFiles "Config" ["//*.xml"])
-- > getDirectoryFiles "" ["Config//*.xml"]
--
--   If the first argument directory does not exist it will raise an error.
--   If @foo@ does not exist, then the first of these error, but the second will not.
--
-- > getDirectoryFiles "foo" ["//*"] -- error
-- > getDirectoryFiles "" ["foo//*"] -- returns []
--
--   This function is tracked and serves as a dependency. If a rule calls
--   @getDirectoryFiles \"\" [\"*.c\"]@ and someone adds @foo.c@ to the
--   directory, that rule will rebuild. If someone changes one of the @.c@ files,
--   but the /list/ of @.c@ files doesn't change, then it will not rebuild.
--   As a consequence of being tracked, if the contents change during the build
--   (e.g. you are generating @.c@ files in this directory) then the build not reach
--   a stable point, which is an error - detected by running with @--lint@.
--   You should normally only call this function returning source files.
--
--   For an untracked variant see 'getDirectoryFilesIO'.
getDirectoryFiles :: FilePath -> [FilePattern] -> Action [FilePath]
getDirectoryFiles dir pat = fmap fromGetDirectoryA $ apply1 $ GetDirectoryFilesQ (dir,pat)

-- | Get the directories in a directory, not including @.@ or @..@.
--   All directories are relative to the argument directory. The result is tracked as a
--   dependency, and if it changes the rule will rerun in subsequent builds.
--   The rules about creating entries described in 'getDirectoryFiles' also apply here.
--
-- > getDirectoryDirs "/Users"
-- >    -- Return all directories in the /Users directory
-- >    -- e.g. ["Emily","Henry","Neil"]
getDirectoryDirs :: FilePath -> Action [FilePath]
getDirectoryDirs = fmap fromGetDirectoryA . apply1 . GetDirectoryDirsQ


---------------------------------------------------------------------
-- IO ROUTINES

getDirectoryContentsIO :: FilePath -> IO [FilePath]
-- getDirectoryContents "" is equivalent to getDirectoryContents "." on Windows,
-- but raises an error on Linux. We smooth out the difference.
getDirectoryContentsIO dir = fmap (sort . filter (not . all (== '.'))) $ IO.getDirectoryContents $ if dir == "" then "." else dir


getDirectoryDirsIO :: FilePath -> IO [FilePath]
getDirectoryDirsIO dir = filterM f =<< getDirectoryContentsIO dir
    where f x = IO.doesDirectoryExist $ dir </> x


-- | A version of 'getDirectoryFiles' that is in IO, and thus untracked.
getDirectoryFilesIO :: FilePath -> [FilePattern] -> IO [FilePath]
-- Known infelicity: on Windows, if you search for "foo", but have the file "FOO",
-- it will match if on its own, or not if it is paired with "*", since that forces
-- a full directory scan, and then it uses Haskell equality (case sensitive)
getDirectoryFilesIO root pat = f "" $ snd $ walk pat
    where
        -- Even after we know they are there because we called contents, we still have to check they are directories/files
        -- as required
        f dir (Walk op) = f dir . WalkTo . op =<< getDirectoryContentsIO (root </> dir)
        f dir (WalkTo (files, dirs)) = do
            files <- filterM (IO.doesFileExist . (root </>)) $ map (dir </>) files
            dirs <- concatMapM (uncurry f) =<< filterM (IO.doesDirectoryExist . (root </>) . fst) (map (first (dir </>)) dirs)
            return $ files ++ dirs


---------------------------------------------------------------------
-- REMOVE UTILITIES

-- | Remove all files and directories that match any of the patterns within a directory.
--   Some examples:
--
-- @
-- 'removeFiles' \"output\" [\"\/\/*\"]        -- delete everything inside \'output\'
-- 'removeFiles' \"output\" [\"\/\/\"]         -- delete \'output\' itself
-- 'removeFiles' \".\" [\"\/\/*.hi\",\"\/\/*.o\"] -- delete all \'.hi\' and \'.o\' files
-- @
--
--   If the argument directory is missing no error is raised.
--   This function will follow symlinks, so should be used with care.
--
--   This function is often useful when writing a @clean@ action for your build system,
--   often as a 'phony' rule.
removeFiles :: FilePath -> [FilePattern] -> IO ()
removeFiles dir pat =
    whenM (IO.doesDirectoryExist dir) $ do
        let (b,w) = walk pat
        if b then removeDir dir else f dir w
    where
        f dir (Walk op) = f dir . WalkTo . op =<< getDirectoryContentsIO dir
        f dir (WalkTo (files, dirs)) = do
            forM_ files $ \fil ->
                try $ removeItem $ dir </> fil :: IO (Either IOException ())
            let done = Set.fromList files
            forM_ (filter (not . flip Set.member done . fst) dirs) $ \(d,w) -> do
                let dir2 = dir </> d
                whenM (IO.doesDirectoryExist dir2) $ f dir2 w

        removeItem :: FilePath -> IO ()
        removeItem x = IO.removeFile x `C.catch` \(_ :: IOException) -> removeDir x

        -- In newer GHC's removeDirectoryRecursive is probably better, but doesn't follow
        -- symlinks, so it's got different behaviour
        removeDir :: FilePath -> IO ()
        removeDir x = do
            mapM_ (removeItem . (x </>)) =<< getDirectoryContentsIO x
            IO.removeDirectory x


-- | Remove files, like 'removeFiles', but executed after the build completes successfully.
--   Useful for implementing @clean@ actions that delete files Shake may have open for building.
removeFilesAfter :: FilePath -> [FilePattern] -> Action ()
removeFilesAfter a b = do
    putLoud $ "Will remove " ++ unwords b ++ " from " ++ a
    runAfter $ removeFiles a b
