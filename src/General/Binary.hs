{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module General.Binary(
    Store(..), newStore,
    BinaryWith(..), module Data.Binary,
    BinList(..), BinFloat(..)
    ) where

import Control.Applicative
import Control.Monad
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Data.Binary.Builder
import Data.List
import Foreign
import System.IO.Unsafe as U
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

---------------------------------------------------------------------
-- STORE TYPE

data Store v = Store
    {store :: v -> Builder
    ,unstore :: BS.ByteString -> v
    }

newStore :: (a -> Put) -> (Get a) -> Store a
newStore put get = Store (execPut . put) (runGet get . LBS.fromStrict)


---------------------------------------------------------------------
-- BINARYWITH

class BinaryWith ctx a where
    putWith :: ctx -> a -> Put
    getWith :: ctx -> Get a

instance (BinaryWith ctx a, BinaryWith ctx b) => BinaryWith ctx (a,b) where
    putWith ctx (a,b) = putWith ctx a >> putWith ctx b
    getWith ctx = liftA2 (,) (getWith ctx) (getWith ctx)

instance BinaryWith ctx a => BinaryWith ctx [a] where
    putWith ctx xs = put (length xs) >> mapM_ (putWith ctx) xs
    getWith ctx = do n <- get; replicateM n $ getWith ctx

instance BinaryWith ctx a => BinaryWith ctx (Maybe a) where
    putWith _ Nothing = putWord8 0
    putWith ctx (Just x) = putWord8 1 >> putWith ctx x
    getWith ctx = do i <- getWord8; if i == 0 then return Nothing else Just <$> getWith ctx


newtype BinList a = BinList {fromBinList :: [a]}

instance Show a => Show (BinList a) where show = show . fromBinList

instance Binary a => Binary (BinList a) where
    put (BinList xs) = case splitAt 254 xs of
        (_, []) -> putWord8 (genericLength xs) >> mapM_ put xs
        (a, b) -> putWord8 255 >> mapM_ put a >> put (BinList b)
    get = do
        x <- getWord8
        case x of
            255 -> do xs <- replicateM 254 get; BinList ys <- get; return $ BinList $ xs ++ ys
            n -> BinList <$> replicateM (fromInteger $ toInteger n) get


newtype BinFloat = BinFloat {fromBinFloat :: Float}

instance Show BinFloat where show = show . fromBinFloat

instance Binary BinFloat where
    put (BinFloat x) = put (convert x :: Word32)
    get = fmap (BinFloat . convert) (get :: Get Word32)


-- Originally from data-binary-ieee754 package

convert :: (Storable a, Storable b) => a -> b
convert x = U.unsafePerformIO $ alloca $ \buf -> do
    poke (castPtr buf) x
    peek buf
