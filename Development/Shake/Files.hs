{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving, DeriveDataTypeable #-}

module Development.Shake.Files(
    (*>>)
    ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Binary
import Data.Hashable
import Data.List
import Data.Maybe
import Data.Typeable

import Development.Shake.Core
import Development.Shake.File
import Development.Shake.FilePattern
import Development.Shake.ModTime




newtype Files = Files [FilePath]
    deriving (Typeable,Eq,Hashable,Binary)

newtype FileTimes = FileTimes [ModTime]
    deriving (Typeable,Show,Eq,Hashable,Binary)


instance Show Files where show (Files xs) = unwords xs


instance Rule Files FileTimes where
    validStored (Files xs) (FileTimes ts) = fmap (== map Just ts) $ mapM getModTimeMaybe xs


-- | All the file patterns must be compatible, so same // and * placements.
--   Can then substitute one to get the other.
(*>>) :: [FilePattern] -> ([FilePath] -> Action ()) -> Rules ()
ps *>> act
    | not $ compatible ps = error "all must be compatible"
    | otherwise = do
        forM ps $ \p ->
            p *> \file -> do
                apply1 $ Files $ map (substitute $ extract p file) ps :: Action FileTimes
                return ()
        rule $ \(Files xs) -> if not $ length xs == length ps && and (zipWith (?==) ps xs) then Nothing else Just $ do
            act xs
            liftIO $ fmap FileTimes $ mapM (getModTimeError "Error, multi rule failed to build the file:") xs
