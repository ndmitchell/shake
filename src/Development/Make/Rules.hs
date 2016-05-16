{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving, DeriveDataTypeable #-}

-- | These are the additional rule types required by Makefile
module Development.Make.Rules(
    need_, want_,
    defaultRuleFile_,
    (??>), Phony(..)
    ) where

import Control.Monad.IO.Class
import System.Directory
import Control.Applicative
import Data.Binary
import Data.Maybe
import Prelude

import Development.Shake
import Development.Shake.Core
import Development.Shake.Classes
import Development.Shake.FilePath

-- Internal imports
import General.String(BSU, unpackU, packU)
import Development.Shake.FileInfo(ModTime, getFileInfo)

infix 1 ??>

---------------------------------------------------------------------
-- FILE_ RULES
-- These are like file rules, but matching the (insane) semantics of make:
-- A rule may not bother creating its file; in that case, it will rebuild forever

newtype File_Q = File_Q BSU
    deriving (Typeable,Eq,Hashable,Binary,NFData)

instance Show File_Q where show (File_Q x) = unpackU x

newtype File_Rule = File_Rule { fromFile_Rule :: BSU -> Maybe (Action Phony) }

type File_A = Maybe ModTime

getModTime :: BSU -> Action File_A
getModTime x = fmap fst <$> liftIO (getFileInfo x)

defaultRuleFile_ :: Rules ()
defaultRuleFile_ = addBuiltinRule $ \(File_Q x) vo dep -> do
    res <- getModTime x
    if vo == Just res && isJust res then
        return $ BuiltinResult (encode res) res False True
    else do
        act <- userRule fromFile_Rule x
        phony <- case act of
            Nothing | res == Nothing -> error $ "Error, file does not exist and no rule available:\n  " ++ unpackU x
                    | otherwise -> return Phony
            Just a -> a
        mtime <- case phony of
            Phony -> return Nothing
            NotPhony -> getModTime x
        return $ BuiltinResult (encode mtime) mtime True (vo == Just mtime)

need_ :: [FilePath] -> Action ()
need_ xs = (apply $ map (File_Q . packU) xs :: Action [File_A]) >> return ()

want_ :: [FilePath] -> Rules ()
want_ = action . need_

data Phony = Phony | NotPhony deriving Eq

(??>) :: (FilePath -> Bool) -> (FilePath -> Action Phony) -> Rules ()
(??>) test act = addUserRule . File_Rule $ \x_ -> let x = unpackU x_ in
    if not $ test x then Nothing else Just $ do
        liftIO $ createDirectoryIfMissing True $ takeDirectory x
        act x
