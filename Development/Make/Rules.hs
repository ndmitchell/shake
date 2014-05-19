{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving, DeriveDataTypeable #-}

-- | These are the additional rule types required by Makefile
module Development.Make.Rules(
    need_, want_,
    defaultRuleFile_,
    (??>), Phony(..)
    ) where

import Control.Monad.IO.Class
import System.Directory

import Development.Shake.Core
import General.String
import Development.Shake.Classes
import Development.Shake.FilePath
import Development.Shake.FileInfo

infix 1 ??>

---------------------------------------------------------------------
-- FILE_ RULES
-- These are like file rules, but a rule may not bother creating the result
-- Which matches the (insane) semantics of make
-- If a file is not produced, it will rebuild forever

newtype File_Q = File_Q BSU
    deriving (Typeable,Eq,Hashable,Binary,NFData)

instance Show File_Q where show (File_Q x) = unpackU x

newtype File_A = File_A (Maybe FileTime)
    deriving (Typeable,Eq,Hashable,Binary,Show,NFData)

instance Rule File_Q File_A where
    storedValue _ (File_Q x) = fmap (fmap (File_A . Just)) $ getModTimeMaybe x


defaultRuleFile_ :: Rules ()
defaultRuleFile_ = priority 0 $ rule $ \(File_Q x) -> Just $
    liftIO $ fmap (File_A . Just) $ getModTimeError "Error, file does not exist and no rule available:" x


need_ :: [FilePath] -> Action ()
need_ xs = (apply $ map (File_Q . packU) xs :: Action [File_A]) >> return ()

want_ :: [FilePath] -> Rules ()
want_ = action . need_

data Phony = Phony | NotPhony deriving Eq

(??>) :: (FilePath -> Bool) -> (FilePath -> Action Phony) -> Rules ()
(??>) test act = rule $ \(File_Q x_) -> let x = unpackU x_ in
    if not $ test x then Nothing else Just $ do
        liftIO $ createDirectoryIfMissing True $ takeDirectory x
        res <- act x
        liftIO $ fmap File_A $ if res == Phony
            then return Nothing
            else getModTimeMaybe x_
