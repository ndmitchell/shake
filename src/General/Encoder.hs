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

-- forM for zipWith
for2M_ as bs f = zipWithM_ f as bs


---------------------------------------------------------------------
-- BINARY SERIALISATION

-- | Methods for Binary serialisation that go directly between strict ByteString values.
--   When the Database is read each key/value will be loaded as a separate ByteString,
--   and for certain types (e.g. file rules) this may remain the preferred format for storing keys.
--   Optimised for performance.
class Encoder a where
    encode :: a -> BS.ByteString
    decode :: BS.ByteString -> a

instance Encoder BS.ByteString where
    encode = id
    decode = id

instance Encoder [BS.ByteString] where
    -- Format:
    -- n :: Word32 - number of strings
    -- ns :: [Word32]{n} - length of each string
    -- contents of each string concatenated (sum ns bytes)
    encode xs = BS.unsafeCreate (4 + (n * 4) + sum ns) $ \p -> do
        pokeByteOff p 0 (fromIntegral n :: Word32)
        for2M_ [4,8..] ns $ \i x -> pokeByteOff p i (fromIntegral x :: Word32)
        p <- return $ p `plusPtr` (4 + (n * 4))
        for2M_ (scanl (+) 0 ns) xs $ \i x -> BS.useAsCStringLen x $ \(bs, n) ->
            BS.memcpy (castPtr bs) (p `plusPtr` i) n
        where ns = map BS.length xs
              n = length ns

    decode bs = unsafePerformIO $ BS.useAsCString bs $ \p -> do
        n <- fromIntegral <$> (peekByteOff p 0 :: IO Word32)
        ns :: [Word32] <- forM [1..fromIntegral n] $ \i -> peekByteOff p (i * 4)
        return $ snd $ mapAccumL (\bs i -> swap $ BS.splitAt (fromIntegral i) bs) (BS.drop (4 + (n * 4)) bs) ns

instance Encoder () where
    encode () = BS.empty
    decode _ = ()

instance Encoder String where
    encode = UTF8.fromString
    decode = UTF8.toString

instance Encoder (Maybe String) where
    encode Nothing = BS.empty
    encode (Just xs) = UTF8.fromString $ '\0' : xs
    decode = fmap snd . uncons . UTF8.toString

instance Encoder [String] where
    encode = encode . map UTF8.fromString
    decode = map UTF8.toString . decode

instance Encoder (String, [String]) where
    encode (a,bs) = encode $ a:bs
    decode x = let a:bs = decode x in (a,bs)

instance Encoder Bool where
    encode False = bsFalse
    encode True = BS.empty
    decode = BS.null

-- CAF so the True ByteString is shared
bsFalse = BS.singleton 0

instance Encoder Word32 where
    encode = encodeStorable
    decode = decodeStorable

instance Encoder Int where
    encode = encodeStorable
    decode = decodeStorable


encodeStorable :: forall a . Storable a => a -> BS.ByteString
encodeStorable = \x -> BS.unsafeCreate n $ \p -> poke (castPtr p) x
    where n = sizeOf (undefined :: a)

decodeStorable :: forall a . Storable a => BS.ByteString -> a
decodeStorable = \bs -> unsafePerformIO $ BS.useAsCStringLen bs $ \(p, size) ->
        if size /= n then error "size mismatch" else peek (castPtr p)
    where n = sizeOf (undefined :: a)


encodeStorableList :: forall a . Storable a => [a] -> BS.ByteString
encodeStorableList = \xs -> BS.unsafeCreate (n * length xs) $ \p ->
    for2M_ [0,n..] xs $ \i x -> pokeByteOff (castPtr p) i x
    where n = sizeOf (undefined :: a)

decodeStorableList :: forall a . Storable a => BS.ByteString -> [a]
decodeStorableList = \bs -> unsafePerformIO $ BS.useAsCStringLen bs $ \(p, size) ->
    let (d,m) = size `divMod` n in
    if m /= 0 then error "size mismatch" else forM [0..d-1] $ \i -> peekElemOff (castPtr p) d
    where n = sizeOf (undefined :: a)
