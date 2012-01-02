{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving, DeriveDataTypeable #-}

module Development.Shake.Files(
    (*>>)
    ) where

import Control.DeepSeq
import Control.Monad
import Control.Monad.IO.Class
import Data.Binary
import Data.Hashable
import Data.Typeable

import Development.Shake.Core
import Development.Shake.File
import Development.Shake.FilePattern
import Development.Shake.FileTime

infix 1 *>>


newtype Files = Files [FilePath]
    deriving (Typeable,Eq,Hashable,Binary,NFData)

newtype FileTimes = FileTimes [FileTime]
    deriving (Typeable,Show,Eq,Hashable,Binary,NFData)

instance Show Files where show (Files xs) = unwords xs


instance Rule Files FileTimes where
    validStored (Files xs) (FileTimes ts) = fmap (== map Just ts) $ mapM getModTimeMaybe xs


-- | Define a rule for building multiple files at the same time.
--   As an example, a single invokation of GHC produces both @.hi@ and @.o@ files:
--
-- > ["*.o","*.hi"] *>> \[o,hi] -> do
-- >    let hs = replaceExtension o "hs"
-- >    need ... -- all files the .hs import
-- >    system' "ghc" ["-c",hs]
--
--   However, in practice, it's usually easier to define rules with '*>' and make the @.hi@ depend
--   on the @.o@. When defining rules that build multiple files, all the 'FilePattern' values must
--   have the same sequence of @\/\/@ and @*@ wildcards in the same order.
(*>>) :: [FilePattern] -> ([FilePath] -> Action ()) -> Rules ()
ps *>> act
    | not $ compatible ps = error $
        "All patterns to *>> must have the same number and position of // and * wildcards\n" ++
        unwords ps
    | otherwise = do
        forM ps $ \p ->
            p *> \file -> do
                apply1 $ Files $ map (substitute $ extract p file) ps :: Action FileTimes
                return ()
        rule $ \(Files xs) -> if not $ length xs == length ps && and (zipWith (?==) ps xs) then Nothing else Just $ do
            act xs
            liftIO $ fmap FileTimes $ mapM (getModTimeError "Error, multi rule failed to build the file:") xs
