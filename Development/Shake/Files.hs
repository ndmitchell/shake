{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving, DeriveDataTypeable #-}

module Development.Shake.Files(
    (?>>), (*>>)
    ) where

import Control.DeepSeq
import Control.Monad
import Control.Monad.IO.Class
import Data.Binary
import Data.Hashable
import Data.Maybe
import Data.Typeable
import qualified Data.ByteString.Char8 as BS

import Development.Shake.Core
import Development.Shake.File
import Development.Shake.FilePattern
import Development.Shake.FileTime

infix 1 ?>>, *>>


newtype Files = Files [BS.ByteString]
    deriving (Typeable,Eq,Hashable,Binary)

instance NFData Files where
    rnf (Files xs) = f xs
        where f [] = ()
              f (x:xs) = x `seq` f xs

newtype FileTimes = FileTimes [FileTime]
    deriving (Typeable,Show,Eq,Hashable,Binary,NFData)

instance Show Files where show (Files xs) = unwords $ map BS.unpack xs


instance Rule Files FileTimes where
    validStored (Files xs) (FileTimes ts) = fmap (== map Just ts) $ mapM getModTimeMaybe xs


-- | Define a rule for building multiple files at the same time.
--   As an example, a single invokation of GHC produces both @.hi@ and @.o@ files:
--
-- @
-- [\"*.o\",\"*.hi\"] '*>>' \\[o,hi] -> do
--     let hs = 'Development.Shake.FilePath.replaceExtension' o \"hs\"
--     'Development.Shake.need' ... -- all files the .hs import
--     'Development.Shake.system'' \"ghc\" [\"-c\",hs]
-- @
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
        forM_ ps $ \p ->
            p *> \file -> do
                apply1 $ Files $ map (BS.pack . substitute (extract p file)) ps :: Action FileTimes
                return ()
        rule $ \(Files xs_) -> let xs = map BS.unpack xs_ in
            if not $ length xs == length ps && and (zipWith (?==) ps xs) then Nothing else Just $ do
                act xs
                liftIO $ getFileTimes "*>>" xs_


-- | Define a rule for building multiple files at the same time, a more powerful
--   and more dangerous version of '*>>'.
--
--   Given an application @test ?>> ...@, @test@ should return @Just@ if the rule applies, and should
--   return the list of files that will be produced. This list /must/ include the file passed as an argument and should
--   obey the invariant:
--
-- > test x == Just ys ==> x `elem` ys && all ((== Just ys) . test) ys
--
--   As an example of a function satisfying the invariaint:
--
-- > test x | takeExtension x `elem` [".hi",".o"]
-- >        = Just [dropExtension x <.> "hi", dropExtension x <.> "o"]
-- > test _ = Nothing
--
--   Regardless of whether @Foo.hi@ or @Foo.o@ is passed, the function always returns @[Foo.hi, Foo.o]@.
(?>>) :: (FilePath -> Maybe [FilePath]) -> ([FilePath] -> Action ()) -> Rules ()
(?>>) test act = do
    let checkedTest x = case test x of
            Nothing -> Nothing
            Just ys | x `elem` ys && all ((== Just ys) . test) ys -> Just ys
                    | otherwise -> error $ "Invariant broken in ?>> when trying on " ++ x

    isJust . checkedTest ?> \x -> do
        apply1 $ Files $ map BS.pack $ fromJust $ test x :: Action FileTimes
        return ()

    rule $ \(Files xs_) -> let xs@(x:_) = map BS.unpack xs_ in
        case checkedTest x of
            Just ys | ys == xs -> Just $ do
                act xs
                liftIO $ getFileTimes "?>>" xs_
            Just ys -> error $ "Error, ?>> is incompatible with " ++ show xs ++ " vs " ++ show ys
            Nothing -> Nothing


getFileTimes :: String -> [BS.ByteString] -> IO FileTimes
getFileTimes name xs = fmap FileTimes $ mapM (getModTimeError ("Error, " ++ name ++ " rule failed to build the file:")) xs
