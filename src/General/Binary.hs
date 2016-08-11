{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module General.Binary(
    BinaryEx(..), newBinaryEx,
    binarySplit,
    module Data.Binary,
    BinList(..), BinFloat(..)
    ) where

import Control.Monad
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Data.Binary.Builder
import Data.List
import Foreign.Storable
import Foreign.Ptr
import Foreign.Marshal.Alloc
import System.IO.Unsafe as U
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Functor
import Prelude


---------------------------------------------------------------------
-- STORE TYPE

-- | An explicit and more efficient version of Binary
data BinaryEx v = BinaryEx
    {putEx :: v -> Builder
    ,getEx :: BS.ByteString -> v
    }

newBinaryEx :: (a -> Put) -> (Get a) -> BinaryEx a
newBinaryEx put get = BinaryEx (execPut . put) (runGet get . LBS.fromChunks . return)

binarySplit :: Storable a => BS.ByteString -> (a, BS.ByteString)
binarySplit bs | BS.length bs < n = error "Reading from ByteString, insufficient left"
               | otherwise = (v, BS.unsafeDrop (sizeOf v) bs)
    where
        v = unsafePerformIO $ BS.unsafeUseAsCString bs $ \ptr -> peek (castPtr ptr)
        n = sizeOf v

---------------------------------------------------------------------
-- BINARY

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
