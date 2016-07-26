{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Development.Shake.Internal.FileName(
    BSU, packU, unpackU, packU_, unpackU_, requireU
    ) where

import qualified Data.ByteString as BS (any)
import qualified Data.ByteString.Char8 as BS hiding (any)
import qualified Data.ByteString.UTF8 as UTF8
import Development.Shake.Classes


---------------------------------------------------------------------
-- Data.ByteString
-- Mostly because ByteString does not have an NFData instance in GHC 7.4

-- | ASCII ByteString
newtype BS = BS BS.ByteString
    deriving (Hashable, Binary, Eq)

instance Show BS where
    show (BS x) = show x

instance NFData BS where
    -- some versions of ByteString do not have NFData instances, but seq is equivalent
    -- for a strict bytestring. Therefore, we write our own instance.
    rnf (BS x) = x `seq` ()


-- | UTF8 ByteString
newtype BSU = BSU BS.ByteString
    deriving (Hashable, Binary, Eq)

instance NFData BSU where
    rnf (BSU x) = x `seq` ()

instance Show BSU where
    show = unpackU


packU :: String -> BSU
packU = packU_ . UTF8.fromString

unpackU :: BSU -> String
unpackU = UTF8.toString . unpackU_

unpackU_ :: BSU -> BS.ByteString
unpackU_ (BSU x) = x

packU_ :: BS.ByteString -> BSU
packU_ = BSU

requireU :: BSU -> Bool
requireU = BS.any (>= 0x80) . unpackU_
