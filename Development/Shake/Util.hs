{-# LANGUAGE BangPatterns, GeneralizedNewtypeDeriving #-}

module Development.Shake.Util(
    modifyIORef'', writeIORef'',
    whenJust,
    BS, pack, unpack, pack_, unpack_,
    BSU, packU, unpackU, packU_, unpackU_, requireU
    ) where

import Data.IORef
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.UTF8 as UTF8
import Development.Shake.Classes


---------------------------------------------------------------------
-- Data.IORef

-- Two 's because GHC 7.6 has a strict modifyIORef
modifyIORef'' :: IORef a -> (a -> a) -> IO ()
modifyIORef'' ref f = do
    x <- readIORef ref
    writeIORef'' ref $ f x

writeIORef'' :: IORef a -> a -> IO ()
writeIORef'' ref !x = writeIORef ref x


---------------------------------------------------------------------
-- Control.Monad

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust (Just a) f = f a
whenJust Nothing f = return ()


---------------------------------------------------------------------
-- Data.ByteString
-- Mostly because ByteString does not have an NFData instance in older GHC

-- | ASCII ByteString
newtype BS = BS BS.ByteString
    deriving (Hashable, Binary, Eq)

instance NFData BS where
    -- some versions of ByteString do not have NFData instances, but seq is equivalent
    -- for a strict bytestring. Therefore, we write our own instance.
    rnf (BS x) = x `seq` ()


-- | UTF8 ByteString
newtype BSU = BSU BS.ByteString
    deriving (Hashable, Binary, Eq)

instance NFData BSU where
    rnf (BSU x) = x `seq` ()



pack :: String -> BS
pack = pack_ . BS.pack

unpack :: BS -> String
unpack = BS.unpack . unpack_

pack_ :: BS.ByteString -> BS
pack_ = BS

unpack_ :: BS -> BS.ByteString
unpack_ (BS x) = x

packU :: String -> BSU
packU = packU_ . UTF8.fromString

unpackU :: BSU -> String
unpackU = UTF8.toString . unpackU_

unpackU_ :: BSU -> BS.ByteString
unpackU_ (BSU x) = x

packU_ :: BS.ByteString -> BSU
packU_ = BSU

requireU :: BSU -> Bool
requireU x = BS.unpack (unpackU_ x) /= unpackU x
