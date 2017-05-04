{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances #-}
{-# ANN module "HLint: ignore" #-}

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
import Data.Word
import Foreign.Storable
import System.Info.Extra
import Development.Shake.Types
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.UTF8 as UTF8


apply_ :: [k] -> Action [v]
apply_ = undefined


---------------------------------------------------------------------
-- UTF8 STRINGS

newtype UTF8 = UTF8 BS.ByteString
    deriving (Eq, Hashable, Typeable, Encoder)

utf8Pack :: String -> UTF8
utf8Pack = UTF8 . UTF8.fromString

utf8Unpack :: UTF8 -> String
utf8Unpack (UTF8 x) = UTF8.toString x


---------------------------------------------------------------------
-- FILE STUFF

-- Format is platform specific:
-- * On Windows if it has a leading 0 it's UTF8, otherwise ANSI
-- * On Linux it's always UTF8
newtype File = File BS.ByteString
    deriving (Encoder, Hashable, Typeable, Eq)

filePack :: String -> File
filePack x | isWindows = File $
    if any (> '\xff') x then BS.cons '\0' $ UTF8.fromString x else BS.pack x
filePack x = File $ UTF8.fromString x

fileUnpack :: File -> String
fileUnpack (File x) | isWindows =
    if BS.head x == '\0' then UTF8.toString $ BS.tail x else BS.unpack x
fileUnpack (File x) = UTF8.toString x


data FileInfo = FileInfo {-# UNPACK #-} !Word32 {-# UNPACK #-} !Word32{-# UNPACK #-} !Word32
    deriving Eq

instance Storable FileInfo where
    sizeOf _ = 12
    pokeByteOff p i (FileInfo x y z) = pokeByteOff p i x >> pokeByteOff p (i+4) y >> pokeByteOff p (i+8) y
    peekByteOff p i = FileInfo <$> peekByteOff p i <*> peekByteOff p (i+4) <*> peekByteOff p (i+8)
    alignment _ = alignment (0 :: Word32)

instance Encoder FileInfo where
    encode = encodeStorable
    decode = decodeStorable

getFileInfo :: ShakeOptions -> File -> IO FileInfo
getFileInfo = error "todo"

eqFileInfo :: ShakeOptions -> FileInfo -> FileInfo -> Bool
eqFileInfo = error "todo"

instance Encoder [File] where
    encode xs = encode [x | File x <- xs]
    decode = map File . decode

instance Encoder [FileInfo] where
    encode = encodeStorableList
    decode = decodeStorableList


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
            res :: [FileInfo] <- apply_ [map filePack xs :: [File]]
            return $ res !! fromJust (elemIndex x xs)

files = addBuiltinRule $ \opts ask (xs :: [File]) old check -> do  
    rebuild <- case old of
        Nothing -> return True
        Just bs -> liftIO ((/= decode bs) <$> mapM (getFileInfo opts) xs) ||^ check
    if not rebuild then 
        return BuiltinInfo
            {changedDependencies = False
            ,changedStore = False
            ,resultStore = fromJust old
            ,changedValue = False
            ,resultValue = decode $ fromJust old}
     else do
        let xs_ = map fileUnpack xs
        [act] <- return $ userRuleMatch (fromJust $ ask Proxy) $ \(FilesRule test act) ->
            case test $ head xs_ of
                Nothing -> Nothing
                Just ys | xs_ == ys -> Just $ act xs_
                        | otherwise -> error "Invariant violated"
        act
        res <- liftIO $ mapM (getFileInfo opts) xs
        let changed = maybe True ((==) res . decode) old
        return BuiltinInfo
            {changedDependencies = True
            ,changedStore = changed
            ,resultStore = encode res
            ,changedValue = changed
            ,resultValue = res}


---------------------------------------------------------------------
-- FILE

data FileResult = Phony (Action ()) | One (Action ()) | Forward (Action FileInfo)

newtype FileRule = FileRule (FilePath -> Maybe FileResult)

phony x = phonys (== x)

phonys f act = addUserRule $ FileRule $ \x -> if f x then Just $ Phony act else Nothing

(?>) :: (FilePath -> Bool) -> (FilePath -> Action ()) -> Rules ()
test ?> run = addUserRule $ FileRule $ \x -> if test x then Just $ One $ run x else Nothing

file = addBuiltinRule $ \opts ask (x :: File) old check -> do
    rebuild <- case old of
        Nothing -> return True
        Just bs | BS.length bs == 0 -> return True
                | BS.length bs == 12 -> liftIO ((/= decode bs) <$> getFileInfo opts x) ||^ check
                | BS.length bs == 13 -> check
    if not rebuild then
        return BuiltinInfo
            {changedDependencies = False
            ,changedStore = False
            ,resultStore = fromJust old
            ,changedValue = False
            ,resultValue = decode $ fromJust old}
     else do
        let x_ = fileUnpack x
        [act] <- return $ userRuleMatch (fromJust $ ask Proxy) $ \(FileRule f) -> f x_
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
                let store = encode res `BS.append` BS.singleton '\0'
                return BuiltinInfo
                    {changedDependencies = True
                    ,changedStore = True
                    ,resultStore = store
                    ,changedValue = maybe True ((/=) store) old
                    ,resultValue = ()
                    }
            One act -> do
                act
                res <- liftIO $ getFileInfo opts x
                return BuiltinInfo
                    {changedDependencies = True
                    ,changedStore = True
                    ,resultStore = encode res
                    ,changedValue = maybe True ((==) res . decode) old
                    ,resultValue = ()}


---------------------------------------------------------------------
-- DIRECTORY RULES

newtype DoesFileExist = DoesFileExist UTF8
    deriving (Eq, Hashable, Encoder, Typeable)

addDoesFileExist = addBuiltinRule $ \_ _ (DoesFileExist x) old _ -> liftIO $ do
    b <- doesFileExist $ utf8Unpack x
    let changed = maybe True ((/=) b . decode) old
    return BuiltinInfo
        {changedDependencies = False
        ,changedStore = changed
        ,resultStore = encode b
        ,changedValue = changed
        ,resultValue = b}


newtype DirectoryContents = DirectoryContents UTF8
    deriving (Eq, Hashable, Encoder, Typeable)

addDirectoryContents = addBuiltinRule $ \_ _ (DirectoryContents x) old _ -> do
    xs <- liftIO $ getDirectoryContents $ utf8Unpack x
    let h = encode (fromIntegral $ hash xs :: Word32)
    let changed = maybe True (/= h) old
    return BuiltinInfo
        {changedDependencies = False
        ,changedStore = changed
        ,resultStore = h
        ,changedValue = changed
        ,resultValue = xs}
