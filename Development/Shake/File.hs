{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving, DeriveDataTypeable #-}

module Development.Shake.File(
    FilePattern, need, want,
    defaultRuleFile,
    (?==), (?>), (**>), (*>)
    ) where

import Control.Monad.IO.Class
import Data.Binary
import Data.Hashable
import Data.List
import Data.Maybe
import Data.Typeable
import System.Directory
import System.Time

import Development.Shake.Core
import System.FilePath(takeDirectory)


type FilePattern = String

newtype File = File FilePath
    deriving (Typeable,Eq,Hashable,Binary)

newtype FileTime = FileTime Int
    deriving (Typeable,Show,Eq,Hashable,Binary)

instance Show File where show (File x) = x


getFileTime :: FilePath -> IO (Maybe FileTime)
getFileTime x = do
    b <- doesFileExist x
    if not b then return Nothing else do
        TOD t _ <- getModificationTime x
        return $ Just $ FileTime $ fromIntegral t


instance Rule File FileTime where
    validStored (File x) t = fmap (== Just t) $ getFileTime x


defaultRuleFile :: Rules ()
defaultRuleFile = defaultRule $ \(File x) -> Just $ do
    res <- liftIO $ getFileTime x
    let msg = "Error, file does not exist and no available rule: " ++ x
    return $ fromMaybe (error msg) res


need :: [FilePath] -> Action ()
need xs = (apply $ map File xs :: Action [FileTime]) >> return ()

want :: [FilePath] -> Rules ()
want xs = action $ need xs


(?>) :: (FilePath -> Bool) -> (FilePath -> Action ()) -> Rules ()
(?>) test act = rule $ \(File x) ->
    if not $ test x then Nothing else Just $ do
        liftIO $ createDirectoryIfMissing True $ takeDirectory x
        act x
        res <- liftIO $ getFileTime x
        let msg = "Error, rule failed to build the file: " ++ x
        return $ fromMaybe (error msg) res


(**>) :: [FilePattern] -> (FilePath -> Action ()) -> Rules ()
(**>) test act = (\x -> any (x ?==) test) ?> act

(*>) :: FilePattern -> (FilePath -> Action ()) -> Rules ()
(*>) test act = (test ?==) ?> act


(?==) :: FilePattern -> FilePath -> Bool
(?==) ('/':'/':x) y = any (x ?==) $ y : [i | '/':i <- tails y]
(?==) ('*':x) y = any (x ?==) $ a ++ take 1 b
    where (a,b) = break ("/" `isPrefixOf`) $ tails y
(?==) (x:xs) (y:ys) | x == y = xs ?== ys
(?==) [] [] = True
(?==) _ _ = False
