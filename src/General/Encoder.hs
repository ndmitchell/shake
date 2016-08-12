{-# LANGUAGE Rank2Types, DeriveFunctor, TupleSections, ScopedTypeVariables, FlexibleInstances #-}

module General.Encoder(
    Encoder(..), encodeStorable, decodeStorable, encodeStorableList, decodeStorableList,
    ) where

import Data.Word
import Data.List.Extra
import Control.Monad
import Foreign.Storable
import Data.Tuple.Extra
import Foreign.Ptr
import System.IO.Unsafe
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import Data.Functor
import Prelude


-- forM for zipWith
for2M_ as bs f = zipWithM_ f as bs

---------------------------------------------------------------------
-- BINARY SERIALISATION

data Builder = Builder {-# UNPACK #-} !Int (forall a . Ptr a -> Int -> IO ())

instance Monoid Builder where
    mempty = Builder 0 $ \_ _ -> return ()
    mappend (Builder x1 x2) (Builder y1 y2) = Builder (x1+y1) $ \p i -> do x2 p i; y2 p $ i+x1


-- | Methods for Binary serialisation that go directly between strict ByteString values.
--   When the Database is read each key/value will be loaded as a separate ByteString,
--   and for certain types (e.g. file rules) this may remain the preferred format for storing keys.
--   Optimised for performance.
class Encoder a where
    encode :: a -> Builder
    decode :: BS.ByteString -> a

instance Encoder BS.ByteString where
    encode x = Builder n $ \ptr i -> BS.useAsCString x $ \bs -> BS.memcpy (castPtr bs) (ptr `plusPtr` i) (fromIntegral n)
        where n = BS.length x
    decode = id

instance Encoder [BS.ByteString] where
    -- Format:
    -- n :: Word32 - number of strings
    -- ns :: [Word32]{n} - length of each string
    -- contents of each string concatenated (sum ns bytes)
    encode xs = Builder (4 + (n * 4) + sum ns) $ \p i -> do
        pokeByteOff p i (fromIntegral n :: Word32)
        for2M_ [4+i,8+i..] ns $ \i x -> pokeByteOff p i (fromIntegral x :: Word32)
        p <- return $ p `plusPtr` (i + 4 + (n * 4))
        for2M_ (scanl (+) 0 ns) xs $ \i x -> BS.useAsCStringLen x $ \(bs, n) ->
            BS.memcpy (castPtr bs) (p `plusPtr` i) (fromIntegral n)
        where ns = map BS.length xs
              n = length ns

    decode bs = unsafePerformIO $ BS.useAsCString bs $ \p -> do
        n <- fromIntegral <$> (peekByteOff p 0 :: IO Word32)
        ns :: [Word32] <- forM [1..fromIntegral n] $ \i -> peekByteOff p (i * 4)
        return $ snd $ mapAccumL (\bs i -> swap $ BS.splitAt (fromIntegral i) bs) (BS.drop (4 + (n * 4)) bs) ns

instance Encoder () where
    encode () = mempty
    decode _ = ()

instance Encoder String where
    encode = encode . UTF8.fromString
    decode = UTF8.toString

instance Encoder (Maybe String) where
    encode Nothing = mempty
    encode (Just xs) = encode $ UTF8.fromString $ '\0' : xs
    decode = fmap snd . uncons . UTF8.toString

instance Encoder [String] where
    encode = encode . map UTF8.fromString
    decode = map UTF8.toString . decode

instance Encoder (String, [String]) where
    encode (a,bs) = encode $ a:bs
    decode x = let a:bs = decode x in (a,bs)

instance Encoder Bool where
    encode False = Builder 1 $ \ptr i -> pokeByteOff ptr i (0 :: Word8)
    encode True = mempty
    decode = BS.null

instance Encoder Word32 where
    encode = encodeStorable
    decode = decodeStorable

instance Encoder Int where
    encode = encodeStorable
    decode = decodeStorable


encodeStorable :: forall a . Storable a => a -> Builder
encodeStorable x = Builder (sizeOf x) $ \p i -> pokeByteOff p i x

decodeStorable :: forall a . Storable a => BS.ByteString -> a
decodeStorable = \bs -> unsafePerformIO $ BS.useAsCStringLen bs $ \(p, size) ->
        if size /= n then error "size mismatch" else peek (castPtr p)
    where n = sizeOf (undefined :: a)


encodeStorableList :: forall a . Storable a => [a] -> Builder
encodeStorableList xs = Builder (n * length xs) $ \ptr i ->
    for2M_ [i,i+n..] xs $ \i x -> pokeByteOff (castPtr ptr) i x
    where n = sizeOf (undefined :: a)

decodeStorableList :: forall a . Storable a => BS.ByteString -> [a]
decodeStorableList = \bs -> unsafePerformIO $ BS.useAsCStringLen bs $ \(p, size) ->
    let (d,m) = size `divMod` n in
    if m /= 0 then error "size mismatch" else forM [0..d-1] $ \i -> peekElemOff (castPtr p) d
    where n = sizeOf (undefined :: a)
