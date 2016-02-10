{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module General.String(
    abbreviate,
    BS, pack, unpack, pack_, unpack_,
    BSU, packU, unpackU, packU_, unpackU_, requireU
    ) where

import qualified Data.ByteString as BS (any)
import qualified Data.ByteString.Char8 as BS hiding (any)
import qualified Data.ByteString.UTF8 as UTF8
import Development.Shake.Classes
import Data.List.Extra

abbreviate :: [(String,String)] -> String -> String
abbreviate [] = id
abbreviate abbrev = f
    where
        -- order so longer abbreviations are preferred
        ordAbbrev = sortOn (negate . length . fst) abbrev

        f [] = []
        f x | (to,rest):_ <- [(to,rest) | (from,to) <- ordAbbrev, Just rest <- [stripPrefix from x]] = to ++ f rest
        f (x:xs) = x : f xs

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
requireU = BS.any (>= 0x80) . unpackU_
