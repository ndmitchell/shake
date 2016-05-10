{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances #-}

module Development.Shake.Experiment.Rules where

import Development.Shake.Experiment.Interface
import Development.Shake.Classes
import Development.Shake.Core
import Data.Proxy
import Data.Maybe
import Data.List
import System.Directory
import Control.Monad.Extra
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as BS


---------------------------------------------------------------------
-- SUPER-FAST STRINGS

newtype SpecialString = SpecialString BS.ByteString
    deriving (Encoder, Hashable, Typeable, Eq)

packSpecialString :: String -> SpecialString
packSpecialString = undefined

unpackSpecialString :: SpecialString -> String
unpackSpecialString = undefined

instance Encoder String where
    encode = BS.pack
    decode = BS.unpack

instance Encoder Int where
    encode = encode . show
    decode = read . decode

data FileA = FileA deriving Eq

getFileA :: FilePath -> IO FileA
getFileA = undefined

apply2 :: [k] -> Action [v]
apply2 = undefined

instance Encoder [FilePath]
instance Encoder [FileA]
instance Encoder FileA


---------------------------------------------------------------------
-- ALWAYS RERUN

newtype AlwaysRerun = AlwaysRerun ()
    deriving (Eq, Hashable, Encoder, Typeable)

addAlwaysRerun = addBuiltinRule $ \_ _ AlwaysRerun{} _ _ -> return BuiltinInfo
    {changedDependencies = False
    ,changedStore = False
    ,resultStore = BS.empty
    ,changedValue = True
    ,resultValue = ()}


---------------------------------------------------------------------
-- FILES

data FilesRule = FilesRule (FilePath -> Maybe [FilePath]) ([FilePath] -> Action ())

(&?>) :: (FilePath -> Maybe [FilePath]) -> ([FilePath] -> Action ()) -> Rules ()
test &?> run = do
    addUserRule $ FilesRule test run
    addUserRule $ FileRule $ \x -> case test x of
        Nothing -> Nothing
        Just xs -> Just $ Forward $ do
            res :: [FileA] <- apply2 [xs :: [FilePath]]
            return $ res !! fromJust (elemIndex x xs)

files = addBuiltinRule $ \opts ask xs old check -> do  
    rebuild <- case old of
        Nothing -> return True
        Just bs -> liftIO ((/= decode bs) <$> mapM getFileA xs) ||^ check
    if not rebuild then 
        return BuiltinInfo
            {changedDependencies = False
            ,changedStore = False
            ,resultStore = fromJust old
            ,changedValue = False
            ,resultValue = decode $ fromJust old}
     else do
        [act] <- return $ userRuleMatch (fromJust $ ask Proxy) $ \(FilesRule test act) ->
            case test $ head xs of
                Nothing -> Nothing
                Just ys | xs == ys -> Just $ act xs
                        | otherwise -> error "Invariant violated"
        act
        res <- liftIO $ mapM getFileA xs
        let changed = maybe True ((==) res . decode) old
        return BuiltinInfo
            {changedDependencies = True
            ,changedStore = changed
            ,resultStore = encode res
            ,changedValue = changed
            ,resultValue = res}


---------------------------------------------------------------------
-- FILE

data FileResult = Phony (Action ()) | File (Action ()) | Forward (Action FileA)

newtype FileRule = FileRule (FilePath -> Maybe FileResult)

phony x = phonys (== x)

phonys f act = addUserRule $ FileRule $ \x -> if f x then Just $ Phony act else Nothing

(?>) :: (FilePath -> Bool) -> (FilePath -> Action ()) -> Rules ()
test ?> run = addUserRule $ FileRule $ \x -> if test x then Just $ File $ run x else Nothing

file = addBuiltinRule $ \_ ask x old check -> do
    rebuild <- case old of
        Nothing -> return True
        Just bs | BS.length bs == 0 -> return True
                | BS.length bs == 12 -> liftIO ((/= decode bs) <$> getFileA x) ||^ check
                | BS.length bs == 13 -> check
    if not rebuild then
        return BuiltinInfo
            {changedDependencies = False
            ,changedStore = False
            ,resultStore = fromJust old
            ,changedValue = False
            ,resultValue = decode $ fromJust old}
     else do
        [act] <- return $ userRuleMatch (fromJust $ ask Proxy) $ \(FileRule f) -> f x
        case act of
            Phony act -> do
                act
                return BuiltinInfo
                    {changedDependencies = True
                    ,changedStore = True
                    ,resultStore = BS.empty
                    ,changedValue = True
                    ,resultValue = ()
                    }
            Forward act -> do
                res <- act
                let store = encode res `BS.append` BS.singleton '_'
                return BuiltinInfo
                    {changedDependencies = True
                    ,changedStore = True
                    ,resultStore = store
                    ,changedValue = maybe True ((/=) store) old
                    ,resultValue = ()
                    }
            File act -> do
                act
                res <- liftIO $ getFileA x
                let changed = maybe True ((==) res . decode) old
                return BuiltinInfo
                    {changedDependencies = True
                    ,changedStore = True
                    ,resultStore = encode res
                    ,changedValue = maybe True ((==) res . decode) old
                    ,resultValue = ()}


---------------------------------------------------------------------
-- DIRECTORY RULES

newtype DoesFileExist = DoesFileExist String
    deriving (Eq, Hashable, Encoder, Typeable)

addDoesFileExist = addBuiltinRule $ \_ _ (DoesFileExist x) old _ -> do
    b <- liftIO $ doesFileExist x
    let diff = maybe True ((/=) b . decode) old
    return BuiltinInfo
        {changedDependencies = False
        ,changedStore = diff
        ,resultStore = encode b
        ,changedValue = diff
        ,resultValue = b}


newtype DirectoryContents = DirectoryContents String
    deriving (Eq, Hashable, Encoder, Typeable)

addDirectoryContents = addBuiltinRule $ \_ _ (DirectoryContents x) old _ -> do
    xs <- liftIO $ getDirectoryContents x
    let h = hash xs
    let diff = maybe True ((/=) h . decode) old
    return BuiltinInfo
        {changedDependencies = False
        ,changedStore = diff
        ,resultStore = encode h
        ,changedValue = diff
        ,resultValue = xs}
