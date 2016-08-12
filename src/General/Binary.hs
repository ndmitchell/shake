{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, ExplicitForAll, ScopedTypeVariables, Rank2Types #-}

module General.Binary(
    BinaryOp(..), newBinaryOp, encode', decode',
    binarySplit, unsafeBinarySplit, binaryCreate,
    Builder(..), runBuilder, sizeBuilder,
    BinaryEx(..), putExStorable, getExStorable, putExStorableList, getExStorableList,
    BinList(..), BinFloat(..)
    ) where

import Control.Monad
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import qualified Data.Binary.Builder as Bin
import Data.List.Extra
import Data.Tuple.Extra
import Foreign.Storable
import Foreign.Ptr
import Foreign.Marshal.Alloc
import System.IO.Unsafe as U
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.UTF8 as UTF8
import Data.Functor
import Data.Monoid
import Prelude


---------------------------------------------------------------------
-- STORE TYPE

-- | An explicit and more efficient version of Binary
data BinaryOp v = BinaryOp
    {putOp :: v -> Builder
    ,getOp :: BS.ByteString -> v
    }

newBinaryOp :: (a -> Put) -> (Get a) -> BinaryOp a
newBinaryOp put get = BinaryOp
    (putEx . Bin.toLazyByteString . execPut . put)
    (runGet get . LBS.fromChunks . return)

binarySplit :: forall a . Storable a => BS.ByteString -> (a, BS.ByteString)
binarySplit bs | BS.length bs < sizeOf (undefined :: a) = error "Reading from ByteString, insufficient left"
               | otherwise = unsafeBinarySplit bs

unsafeBinarySplit :: Storable a => BS.ByteString -> (a, BS.ByteString)
unsafeBinarySplit bs = (v, BS.unsafeDrop (sizeOf v) bs)
    where v = unsafePerformIO $ BS.unsafeUseAsCString bs $ \ptr -> peek (castPtr ptr)

binaryCreate :: Storable a => a -> BS.ByteString
binaryCreate x = unsafePerformIO $ BS.create (sizeOf x) $ \ptr -> poke (castPtr ptr) x

encode' :: Binary a => a -> BS.ByteString
encode' = BS.concat . LBS.toChunks . encode

decode' :: Binary a => BS.ByteString -> a
decode' = decode . LBS.fromChunks . return


-- forM for zipWith
for2M_ as bs f = zipWithM_ f as bs

---------------------------------------------------------------------
-- BINARY SERIALISATION

data Builder = Builder {-# UNPACK #-} !Int (forall a . Ptr a -> Int -> IO ())

sizeBuilder :: Builder -> Int
sizeBuilder (Builder i _) = i

runBuilder :: Builder -> BS.ByteString
runBuilder (Builder i f) = unsafePerformIO $ BS.create i $ \ptr -> f ptr 0

instance Monoid Builder where
    mempty = Builder 0 $ \_ _ -> return ()
    mappend (Builder x1 x2) (Builder y1 y2) = Builder (x1+y1) $ \p i -> do x2 p i; y2 p $ i+x1


-- | Methods for Binary serialisation that go directly between strict ByteString values.
--   When the Database is read each key/value will be loaded as a separate ByteString,
--   and for certain types (e.g. file rules) this may remain the preferred format for storing keys.
--   Optimised for performance.
class BinaryEx a where
    putEx :: a -> Builder
    getEx :: BS.ByteString -> a

instance BinaryEx BS.ByteString where
    putEx x = Builder n $ \ptr i -> BS.useAsCString x $ \bs -> BS.memcpy (ptr `plusPtr` i) (castPtr bs) (fromIntegral n)
        where n = BS.length x
    getEx = id

instance BinaryEx LBS.ByteString where
    putEx x = Builder (fromIntegral $ LBS.length x) $ \ptr i -> do
        let go i [] = return ()
            go i (x:xs) = do
                let n = BS.length x
                BS.useAsCString x $ \bs -> BS.memcpy (ptr `plusPtr` i) (castPtr bs) (fromIntegral n)
                go (i+n) xs
        go i $ LBS.toChunks x
    getEx = LBS.fromChunks . return

instance BinaryEx [BS.ByteString] where
    -- Format:
    -- n :: Word32 - number of strings
    -- ns :: [Word32]{n} - length of each string
    -- contents of each string concatenated (sum ns bytes)
    putEx xs = Builder (4 + (n * 4) + sum ns) $ \p i -> do
        pokeByteOff p i (fromIntegral n :: Word32)
        for2M_ [4+i,8+i..] ns $ \i x -> pokeByteOff p i (fromIntegral x :: Word32)
        p <- return $ p `plusPtr` (i + 4 + (n * 4))
        for2M_ (scanl (+) 0 ns) xs $ \i x -> BS.useAsCStringLen x $ \(bs, n) ->
            BS.memcpy (p `plusPtr` i) (castPtr bs) (fromIntegral n)
        where ns = map BS.length xs
              n = length ns

    getEx bs = unsafePerformIO $ BS.useAsCString bs $ \p -> do
        n <- fromIntegral <$> (peekByteOff p 0 :: IO Word32)
        ns :: [Word32] <- forM [1..fromIntegral n] $ \i -> peekByteOff p (i * 4)
        return $ snd $ mapAccumL (\bs i -> swap $ BS.splitAt (fromIntegral i) bs) (BS.drop (4 + (n * 4)) bs) ns

instance BinaryEx () where
    putEx () = mempty
    getEx _ = ()

instance BinaryEx String where
    putEx = putEx . UTF8.fromString
    getEx = UTF8.toString

instance BinaryEx (Maybe String) where
    putEx Nothing = mempty
    putEx (Just xs) = putEx $ UTF8.fromString $ '\0' : xs
    getEx = fmap snd . uncons . UTF8.toString

instance BinaryEx [String] where
    putEx = putEx . map UTF8.fromString
    getEx = map UTF8.toString . getEx

instance BinaryEx (String, [String]) where
    putEx (a,bs) = putEx $ a:bs
    getEx x = let a:bs = getEx x in (a,bs)

instance BinaryEx Bool where
    putEx False = Builder 1 $ \ptr i -> pokeByteOff ptr i (0 :: Word8)
    putEx True = mempty
    getEx = BS.null

instance BinaryEx Word16 where
    putEx = putExStorable
    getEx = getExStorable

instance BinaryEx Word32 where
    putEx = putExStorable
    getEx = getExStorable

instance BinaryEx Int where
    putEx = putExStorable
    getEx = getExStorable

instance BinaryEx Float where
    putEx = putExStorable
    getEx = getExStorable


putExStorable :: forall a . Storable a => a -> Builder
putExStorable x = Builder (sizeOf x) $ \p i -> pokeByteOff p i x

getExStorable :: forall a . Storable a => BS.ByteString -> a
getExStorable = \bs -> unsafePerformIO $ BS.useAsCStringLen bs $ \(p, size) ->
        if size /= n then error "size mismatch" else peek (castPtr p)
    where n = sizeOf (undefined :: a)


putExStorableList :: forall a . Storable a => [a] -> Builder
putExStorableList xs = Builder (n * length xs) $ \ptr i ->
    for2M_ [i,i+n..] xs $ \i x -> pokeByteOff (castPtr ptr) i x
    where n = sizeOf (undefined :: a)

getExStorableList :: forall a . Storable a => BS.ByteString -> [a]
getExStorableList = \bs -> unsafePerformIO $ BS.useAsCStringLen bs $ \(p, size) ->
    let (d,m) = size `divMod` n in
    if m /= 0 then error "size mismatch" else forM [0..d-1] $ \i -> peekElemOff (castPtr p) d
    where n = sizeOf (undefined :: a)

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
