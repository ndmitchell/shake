{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving, ScopedTypeVariables, DeriveDataTypeable, RecordWildCards, FlexibleContexts #-}

-- | Both System.Directory and System.Environment wrappers
module Development.Shake.Rules.Directory(
    doesFileExist, doesDirectoryExist,
    getDirectoryContents, getDirectoryFiles, getDirectoryDirs,
    getEnv, getEnvWithDefault,
    removeFiles, removeFilesAfter,
    defaultRuleDirectory
    ) where

import Control.Monad.Extra
import Control.Monad.IO.Class
import Data.Maybe
import Data.Binary
import Data.List
import Data.Tuple.Extra
import qualified System.Directory as IO
import qualified System.Environment.Extra as IO

import Development.Shake.Core
import Development.Shake.Classes
import Development.Shake.FilePath
import Development.Shake.FilePattern
import General.Extra
import Prelude


newtype DoesFileExistQ = DoesFileExistQ FilePath
    deriving (Typeable,Eq,Hashable,Binary,NFData)

instance Show DoesFileExistQ where
    show (DoesFileExistQ a) = "doesFileExist " ++ showQuote a

newtype DoesFileExistA = DoesFileExistA Bool
    deriving (Typeable,Eq,Hashable,Binary,NFData)

instance Show DoesFileExistA where
    show (DoesFileExistA a) = show a


newtype DoesDirectoryExistQ = DoesDirectoryExistQ FilePath
    deriving (Typeable,Eq,Hashable,Binary,NFData)

instance Show DoesDirectoryExistQ where
    show (DoesDirectoryExistQ a) = "doesDirectoryExist " ++ showQuote a

newtype DoesDirectoryExistA = DoesDirectoryExistA Bool
    deriving (Typeable,Eq,Hashable,Binary,NFData)

instance Show DoesDirectoryExistA where
    show (DoesDirectoryExistA a) = show a


newtype GetEnvQ = GetEnvQ String
    deriving (Typeable,Eq,Hashable,Binary,NFData)

instance Show GetEnvQ where
    show (GetEnvQ a) = "getEnv " ++ showQuote a

newtype GetEnvA = GetEnvA (Maybe String)
    deriving (Typeable,Eq,Hashable,Binary,NFData)

instance Show GetEnvA where
    show (GetEnvA a) = maybe "<unset>" showQuote a


data GetDirectoryQ
    = GetDir {dir :: FilePath}
    | GetDirFiles {dir :: FilePath, pat :: [FilePattern]}
    | GetDirDirs {dir :: FilePath}
    deriving (Typeable,Eq)

newtype GetDirectoryA = GetDirectoryA [FilePath]
    deriving (Typeable,Eq,Hashable,Binary,NFData)

instance Show GetDirectoryQ where
    show (GetDir x) = "getDirectoryContents " ++ showQuote x
    show (GetDirFiles a b) = "getDirectoryFiles " ++ showQuote a ++ " [" ++ unwords (map showQuote b) ++ "]"
    show (GetDirDirs x) = "getDirectoryDirs " ++ showQuote x

instance Show GetDirectoryA where
    show (GetDirectoryA xs) = unwords $ map showQuote xs

instance NFData GetDirectoryQ where
    rnf (GetDir a) = rnf a
    rnf (GetDirFiles a b) = rnf a `seq` rnf b
    rnf (GetDirDirs a) = rnf a

instance Hashable GetDirectoryQ where
    hashWithSalt salt = hashWithSalt salt . f
        where f (GetDir x) = (0 :: Int, x, [])
              f (GetDirFiles x y) = (1, x, y)
              f (GetDirDirs x) = (2, x, [])

instance Binary GetDirectoryQ where
    get = do
        i <- getWord8
        case i of
            0 -> liftM  GetDir get
            1 -> liftM2 GetDirFiles get get
            2 -> liftM  GetDirDirs get

    put (GetDir x) = putWord8 0 >> put x
    put (GetDirFiles x y) = putWord8 1 >> put x >> put y
    put (GetDirDirs x) = putWord8 2 >> put x


instance Rule DoesFileExistQ DoesFileExistA where
    storedValue _ (DoesFileExistQ x) = (Just . DoesFileExistA) <$> IO.doesFileExist x

instance Rule DoesDirectoryExistQ DoesDirectoryExistA where
    storedValue _ (DoesDirectoryExistQ x) = (Just . DoesDirectoryExistA) <$> IO.doesDirectoryExist x

instance Rule GetEnvQ GetEnvA where
    storedValue _ (GetEnvQ x) = (Just . GetEnvA) <$> IO.lookupEnv x

instance Rule GetDirectoryQ GetDirectoryA where
    storedValue _ x = Just <$> getDir x


-- | This function is not actually exported, but Haddock is buggy. Please ignore.
defaultRuleDirectory :: Rules ()
defaultRuleDirectory = do
    rule $ \(DoesFileExistQ x) -> Just $
        liftIO $ DoesFileExistA <$> IO.doesFileExist x
    rule $ \(DoesDirectoryExistQ x) -> Just $
        liftIO $ DoesDirectoryExistA <$> IO.doesDirectoryExist x
    rule $ \(x :: GetDirectoryQ) -> Just $
        liftIO $ getDir x
    rule $ \(GetEnvQ x) -> Just $
        liftIO $ GetEnvA <$> IO.lookupEnv x


-- | Returns 'True' if the file exists. The existence of the file is tracked as a
--   dependency, and if the file is created or deleted the rule will rerun in subsequent builds.
--
--   You should not call 'doesFileExist' on files which can be created by the build system.
doesFileExist :: FilePath -> Action Bool
doesFileExist file = do
    DoesFileExistA res <- apply1 $ DoesFileExistQ $ toStandard file
    return res

-- | Returns 'True' if the directory exists. The existence of the directory is tracked as a
--   dependency, and if the directory is created or delete the rule will rerun in subsequent builds.
--
--   You should not call 'doesDirectoryExist' on directories which can be created by the build system.
doesDirectoryExist :: FilePath -> Action Bool
doesDirectoryExist file = do
    DoesDirectoryExistA res <- apply1 $ DoesDirectoryExistQ $ toStandard file
    return res

-- | Return 'Just' the value of the environment variable, or 'Nothing'
--   if the variable is not set. The environment variable is tracked as a
--   dependency, and if it changes the rule will rerun in subsequent builds.
--
--   This function is a tracked version of 'getEnv'/'lookupEnv' from the base library.
getEnv :: String -> Action (Maybe String)
getEnv var = do
    GetEnvA res <- apply1 $ GetEnvQ var
    return res

-- | Return the value of the environment variable, or the default value if it is
--   not set. Similar to 'getEnv'.
getEnvWithDefault :: String -> String -> Action String
getEnvWithDefault def var = fromMaybe def <$> getEnv var

-- | Get the contents of a directory. The result will be sorted, and will not contain
--   the entries @.@ or @..@ (unlike the standard Haskell version). The resulting paths will be relative
--   to the first argument. The result is tracked as a
--   dependency, and if it changes the rule will rerun in subsequent builds.
--
--   It is usually simpler to call either 'getDirectoryFiles' or 'getDirectoryDirs'.
getDirectoryContents :: FilePath -> Action [FilePath]
getDirectoryContents x = getDirAction $ GetDir x

-- | Get the files anywhere under a directory that match any of a set of patterns.
--   For the interpretation of the patterns see '?=='. All results will be
--   relative to the 'FilePath' argument. The result is tracked as a
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
getDirectoryFiles :: FilePath -> [FilePattern] -> Action [FilePath]
getDirectoryFiles x f = getDirAction $ GetDirFiles x f

-- | Get the directories in a directory, not including @.@ or @..@.
--   All directories are relative to the argument directory. The result is tracked as a
--   dependency, and if it changes the rule will rerun in subsequent builds.
--
--
-- > getDirectoryDirs "/Users"
-- >    -- Return all directories in the /Users directory
-- >    -- e.g. ["Emily","Henry","Neil"]
getDirectoryDirs :: FilePath -> Action [FilePath]
getDirectoryDirs x = getDirAction $ GetDirDirs x

getDirAction x = do GetDirectoryA y <- apply1 x; return y

contents :: FilePath -> IO [FilePath]
-- getDirectoryContents "" is equivalent to getDirectoryContents "." on Windows,
-- but raises an error on Linux. We smooth out the difference.
contents x = fmap (filter $ not . all (== '.')) $ IO.getDirectoryContents $ if x == "" then "." else x


answer :: [FilePath] -> GetDirectoryA
answer = GetDirectoryA . sort

getDir :: GetDirectoryQ -> IO GetDirectoryA
getDir GetDir{..} = answer <$> contents dir

getDir GetDirDirs{..} = fmap answer $ filterM f =<< contents dir
    where f x = IO.doesDirectoryExist $ dir </> x

-- Known infelicity: on Windows, if you search for "foo", but have the file "FOO",
-- it will match if on its own, or not if it is paired with "*", since that forces
-- a full directory scan, and then it uses Haskell equality (case sensitive)
getDir GetDirFiles{..} = fmap answer $ f "" $ walk pat
    where
        root = dir

        -- Even after we know they are there because we called contents, we still have to check they are directories/files
        -- as required
        f dir (Walk op) = f dir . WalkTo . op =<< contents (root </> dir)
        f dir (WalkTo (files, dirs)) = do
            files <- filterM (IO.doesFileExist . (root </>)) $ map (dir </>) files
            dirs <- concatMapM (uncurry f) =<< filterM (IO.doesDirectoryExist . (root </>) . fst) (map (first (dir </>)) dirs)
            return $ files ++ dirs


-- | Remove all files and directories that match any of the patterns within a directory.
--   Some examples:
--
-- @
-- 'removeFiles' \"output\" [\"\/\/*\"]
-- 'removeFiles' \".\" [\"\/\/*.hi\",\"\/\/*.o\"]
-- @
--
--   Any directories that become empty after deleting items from within them will themselves be deleted,
--   up to (and including) the containing directory.
--   This function is often useful when writing a @clean@ action for your build system,
--   often as a 'phony' rule.
removeFiles :: FilePath -> [FilePattern] -> IO ()
removeFiles dir pat = do
    b <- IO.doesDirectoryExist dir
    when b $ void $ f ""
    where
        -- because it is generate and match anything like ../ will be ignored, since we never generate ..
        -- therefore we can safely know we never escape the containing directory
        test = let ps = map (?==) pat in \x -> any ($ x) ps

        -- dir </> dir2 is the part to operate on, return True if you deleted the directory
        f :: FilePath -> IO Bool
        f dir2 | test dir2 = do
            IO.removeDirectoryRecursive $ dir </> dir2
            return True
        f dir2 = do
            xs <- fmap (map (dir2 </>)) $ contents $ dir </> dir2
            (dirs,files) <- partitionM (\x -> IO.doesDirectoryExist $ dir </> x) xs
            noDirs <- and <$> mapM f dirs
            let (del,keep) = partition test files
            forM_ del $ \d -> IO.removeFile $ dir </> d
            let die = noDirs && null keep && not (null xs)
            when die $ IO.removeDirectory $ dir </> dir2
            return die


-- | Remove files, like 'removeFiles', but executed after the build completes successfully.
--   Useful for implementing @clean@ actions that delete files Shake may have open for building.
removeFilesAfter :: FilePath -> [FilePattern] -> Action ()
removeFilesAfter a b = do
    putLoud $ "Will remove " ++ unwords b ++ " from " ++ a
    runAfter $ removeFiles a b
