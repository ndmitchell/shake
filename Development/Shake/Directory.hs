{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving, ScopedTypeVariables, DeriveDataTypeable #-}

module Development.Shake.Directory(
    doesFileExist,
    getDirectoryContents, getDirectoryFiles, getDirectoryDirs,
    defaultRuleDirectory
    ) where

import Control.DeepSeq
import Control.Monad
import Control.Monad.IO.Class
import Data.Binary
import Data.Hashable
import Data.List
import Data.Typeable
import qualified System.Directory as IO
import System.FilePath

import Development.Shake.Core
import Development.Shake.FilePattern



newtype Exist = Exist FilePath
    deriving (Typeable,Eq,Hashable,Binary,NFData)

instance Show Exist where
    show (Exist a) = "Exists? " ++ a


data GetDir
    = GetDir {dir :: FilePath}
    | GetDirFiles {dir :: FilePath, pat :: FilePattern}
    | GetDirDirs {dir :: FilePath}
    deriving (Typeable,Show,Eq)
newtype GetDir_ = GetDir_ [FilePath]
    deriving (Typeable,Show,Eq,Hashable,Binary,NFData)

instance NFData GetDir where
    rnf (GetDir a) = rnf a
    rnf (GetDirFiles a b) = rnf a `seq` rnf b
    rnf (GetDirDirs a) = rnf a

instance Hashable GetDir where
    hash = hash . f
        where f (GetDir x) = (0 :: Int, x, "")
              f (GetDirFiles x y) = (1, x, y)
              f (GetDirDirs x) = (2, x, "")

instance Binary GetDir where
    get = do
        i <- getWord8
        case i of
            0 -> liftM  GetDir get
            1 -> liftM2 GetDirFiles get get
            2 -> liftM  GetDirDirs get

    put (GetDir x) = putWord8 0 >> put x
    put (GetDirFiles x y) = putWord8 1 >> put x >> put y
    put (GetDirDirs x) = putWord8 2 >> put x


instance Rule Exist Bool where
    validStored (Exist x) b = fmap (== b) $ IO.doesFileExist x
    invariant _ = True

instance Rule GetDir GetDir_ where
    validStored x y = fmap (== y) $ getDir x
    invariant _ = True


-- | This function is not actually exported, but Haddock is buggy. Please ignore.
defaultRuleDirectory :: Rules ()
defaultRuleDirectory = do
    defaultRule $ \(Exist x) -> Just $
        liftIO $ IO.doesFileExist x
    defaultRule $ \(x :: GetDir) -> Just $
        liftIO $ getDir x


-- | Returns 'True' if the file exists.
doesFileExist :: FilePath -> Action Bool
doesFileExist = apply1 . Exist

-- | Get the contents of a directory. The result will be sorted, and will not contain
--   the files @.@ or @..@ (unlike the standard Haskell version). It is usually better to
--   call either 'getDirectoryFiles' or 'getDirectoryDirs'. The resulting paths will be relative
--   to the first argument.
getDirectoryContents :: FilePath -> Action [FilePath]
getDirectoryContents x = getDirAction $ GetDir x

-- | Get the files in a directory that match a particular pattern.
--   For the interpretation of the pattern see '?=='.
getDirectoryFiles :: FilePath -> FilePattern -> Action [FilePath]
getDirectoryFiles x f = getDirAction $ GetDirFiles x f

-- | Get the directories contained by a directory, does not include @.@ or @..@.
getDirectoryDirs :: FilePath -> Action [FilePath]
getDirectoryDirs x = getDirAction $ GetDirDirs x

getDirAction x = do GetDir_ y <- apply1 x; return y


getDir :: GetDir -> IO GetDir_
getDir x = fmap (GetDir_ . sort) $ f x . filter validName =<< IO.getDirectoryContents (dir x)
    where
        validName = not . all (== '.')

        f GetDir{} xs = return xs
        f GetDirFiles{} xs = flip filterM xs $ \s -> do
            if not $ pat x ?== s then return False else IO.doesFileExist $ dir x </> s
        f GetDirDirs{} xs = flip filterM xs $ \s -> IO.doesDirectoryExist $ dir x </> s
