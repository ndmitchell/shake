{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving, ScopedTypeVariables, DeriveDataTypeable, RecordWildCards #-}

-- | Both System.Directory and System.Environment wrappers
module Development.Shake.Directory(
    doesFileExist, doesDirectoryExist,
    getDirectoryContents, getDirectoryFiles, getDirectoryDirs,
    getEnv,
    removeFiles,
    defaultRuleDirectory
    ) where

import Control.Monad
import Control.Monad.IO.Class
import System.IO.Error
import Data.Binary
import Data.List
import Data.Maybe
import qualified System.Directory as IO
import qualified System.Environment as IO

import Development.Shake.Core
import Development.Shake.Classes
import Development.Shake.FilePath
import Development.Shake.FilePattern


newtype DoesFileExistQ = DoesFileExistQ FilePath
    deriving (Typeable,Eq,Hashable,Binary,NFData)

instance Show DoesFileExistQ where
    show (DoesFileExistQ a) = "Exists? " ++ a

newtype DoesFileExistA = DoesFileExistA Bool
    deriving (Typeable,Eq,Hashable,Binary,NFData)

instance Show DoesFileExistA where
    show (DoesFileExistA a) = show a


newtype DoesDirectoryExistQ = DoesDirectoryExistQ FilePath
    deriving (Typeable,Eq,Hashable,Binary,NFData)

instance Show DoesDirectoryExistQ where
    show (DoesDirectoryExistQ a) = "Exists dir? " ++ a

newtype DoesDirectoryExistA = DoesDirectoryExistA Bool
    deriving (Typeable,Eq,Hashable,Binary,NFData)

instance Show DoesDirectoryExistA where
    show (DoesDirectoryExistA a) = show a


newtype GetEnvQ = GetEnvQ String
    deriving (Typeable,Eq,Hashable,Binary,NFData)

instance Show GetEnvQ where
    show (GetEnvQ a) = "getEnv " ++ a

newtype GetEnvA = GetEnvA (Maybe String)
    deriving (Typeable,Eq,Hashable,Binary,NFData)

instance Show GetEnvA where
    show (GetEnvA a) = fromMaybe "<unset>" a


data GetDirectoryQ
    = GetDir {dir :: FilePath}
    | GetDirFiles {dir :: FilePath, pat :: [FilePattern]}
    | GetDirDirs {dir :: FilePath}
    deriving (Typeable,Eq)
newtype GetDirectoryA = GetDirectoryA [FilePath]
    deriving (Typeable,Show,Eq,Hashable,Binary,NFData)

instance Show GetDirectoryQ where
    show (GetDir x) = "Listing " ++ x
    show (GetDirFiles a b) = "Files " ++ a </> ['{'|m] ++ unwords b ++ ['}'|m]
        where m = length b > 1
    show (GetDirDirs x) = "Dirs " ++ x

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
    storedValue (DoesFileExistQ x) = fmap (Just . DoesFileExistA) $ IO.doesFileExist x
    -- invariant _ = True

instance Rule DoesDirectoryExistQ DoesDirectoryExistA where
    storedValue (DoesDirectoryExistQ x) = fmap (Just . DoesDirectoryExistA) $ IO.doesDirectoryExist x
    -- invariant _ = True

instance Rule GetEnvQ GetEnvA where
    storedValue (GetEnvQ x) = fmap (Just . GetEnvA) $ getEnvIO x
    -- invariant _ = True

instance Rule GetDirectoryQ GetDirectoryA where
    storedValue x = fmap Just $ getDir x
    -- invariant _ = True


-- | This function is not actually exported, but Haddock is buggy. Please ignore.
defaultRuleDirectory :: Rules ()
defaultRuleDirectory = do
    defaultRule $ \(DoesFileExistQ x) -> Just $
        liftIO $ fmap DoesFileExistA $ IO.doesFileExist x
    defaultRule $ \(DoesDirectoryExistQ x) -> Just $
        liftIO $ fmap DoesDirectoryExistA $ IO.doesDirectoryExist x
    defaultRule $ \(x :: GetDirectoryQ) -> Just $
        liftIO $ getDir x
    defaultRule $ \(GetEnvQ x) -> Just $
        liftIO $ fmap GetEnvA $ getEnvIO x


-- | Returns 'True' if the file exists.
doesFileExist :: FilePath -> Action Bool
doesFileExist file = do
    DoesFileExistA res <- apply1 $ DoesFileExistQ file
    return res

-- | Returns 'True' if the directory exists.
doesDirectoryExist :: FilePath -> Action Bool
doesDirectoryExist file = do
    DoesDirectoryExistA res <- apply1 $ DoesDirectoryExistQ file
    return res

-- | Return 'Just' the value of the environment variable, or 'Nothing'
--   if the variable is not set.
getEnv :: String -> Action (Maybe String)
getEnv var = do
    GetEnvA res <- apply1 $ GetEnvQ var
    return res

getEnvIO :: String -> IO (Maybe String)
getEnvIO x = catchIOError (fmap Just $ IO.getEnv x) $
    \e -> if isDoesNotExistError e then return Nothing else ioError e

-- | Get the contents of a directory. The result will be sorted, and will not contain
--   the entries @.@ or @..@ (unlike the standard Haskell version). The resulting paths will be relative
--   to the first argument.
--
--   It is usually simpler to call either 'getDirectoryFiles' or 'getDirectoryDirs'.
getDirectoryContents :: FilePath -> Action [FilePath]
getDirectoryContents x = getDirAction $ GetDir x

-- | Get the files anywhere under a directory that match any of a set of patterns.
--   For the interpretation of the patterns see '?=='. All results will be
--   relative to the 'FilePath' argument. Some examples:
--
-- > getDirectoryFiles "Config" ["//*.xml"]
-- >     -- All .xml files anywhere under the Config directory
-- >     -- If Config/foo/bar.xml exists it will return ["foo/bar.xml"]
-- > getDirectoryFiles "Modules" ["*.hs","*.lhs"]
-- >     -- All .hs or .lhs in the Modules directory
-- >     -- If Modules/foo.hs and Modules/foo.lhs exist, it will return ["foo.hs","foo.lhs"]
getDirectoryFiles :: FilePath -> [FilePattern] -> Action [FilePath]
getDirectoryFiles x f = getDirAction $ GetDirFiles x f

-- | Get the directories in a directory, not including @.@ or @..@.
--   All directories are relative to the argument directory.
--
-- > getDirectoryDirs "/Users"
-- >    -- Return all directories in the /Users directory
-- >    -- e.g. ["Emily","Henry","Neil"]
getDirectoryDirs :: FilePath -> Action [FilePath]
getDirectoryDirs x = getDirAction $ GetDirDirs x

getDirAction x = do GetDirectoryA y <- apply1 x; return y

contents :: FilePath -> IO [FilePath]
contents = fmap (filter $ not . all (== '.')) . IO.getDirectoryContents


answer :: [FilePath] -> GetDirectoryA
answer = GetDirectoryA . sort

getDir :: GetDirectoryQ -> IO GetDirectoryA
getDir GetDir{..} = fmap answer $ contents dir

getDir GetDirDirs{..} = fmap answer $ filterM f =<< contents dir
    where f x = IO.doesDirectoryExist $ dir </> x

getDir GetDirFiles{..} = fmap answer $ concatMapM f $ directories pat
    where
        test = let ps = map (?==) pat in \x -> any ($ x) ps

        f (dir2,False) = do
            xs <- fmap (map (dir2 </>)) $ contents $ dir </> dir2
            flip filterM xs $ \x -> if not $ test x then return False else IO.doesFileExist $ dir </> x

        f (dir2,True) = do
            xs <- fmap (map (dir2 </>)) $ contents $ dir </> dir2
            (files,dirs) <- partitionM (\x -> IO.doesFileExist $ dir </> x) xs
            rest <- concatMapM (\d -> f (d, True)) dirs
            return $ filter test files ++ rest


concatMapM f xs = fmap concat $ mapM f xs

partitionM f [] = return ([], [])
partitionM f (x:xs) = do
    t <- f x
    (a,b) <- partitionM f xs
    return $ if t then (x:a,b) else (a,x:b)


-- | Remove all empty directories and files that match any of the patterns beneath a directory.
--   Some examples:
--
-- @
--   'removeFiles' \"output\" [\"\/\/*\"]
--   'removeFiles' \".\" [\"\/\/*.hi\",\"\/\/*.o\"]
-- @
--
--   This function is often useful when writing a \'clean\' function for your build system,
--   often as the first argument to 'shakeWithArgs'.
removeFiles :: FilePath -> [FilePattern] -> IO ()
removeFiles dir ["//*"] = IO.removeDirectoryRecursive dir -- optimisation
removeFiles dir pat = f "" >> return ()
    where
        test = let ps = map (?==) pat in \x -> any ($ x) ps

        -- dir </> dir2 is the part to operate on, return True if you cleaned everything
        f :: FilePath -> IO Bool
        f dir2 = do
            xs <- fmap (map (dir2 </>)) $ contents $ dir </> dir2
            (files,dirs) <- partitionM (\x -> IO.doesFileExist $ dir </> x) xs
            noDirs <- fmap and $ mapM f dirs
            let (del,keep) = partition test files
            mapM_ IO.removeFile del
            let die = noDirs && null keep
            when die $ IO.removeDirectory $ dir </> dir2
            return die
