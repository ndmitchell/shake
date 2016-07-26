{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Development.Shake.Internal.FileName(
    BSU, packU, unpackU, packU_, unpackU_, requireU,
    filepathNormalise
    ) where

import qualified Data.ByteString as BS (any)
import qualified Data.ByteString.Char8 as BS hiding (any)
import qualified Data.ByteString.UTF8 as UTF8
import Development.Shake.Classes
import qualified System.FilePath as Native
import System.Info.Extra
import Data.List


---------------------------------------------------------------------
-- Data.ByteString
-- Mostly because ByteString does not have an NFData instance in GHC 7.4

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


---------------------------------------------------------------------
-- NORMALISATION

-- | Equivalent to @toStandard . normaliseEx@ from "Development.Shake.FilePath".
filepathNormalise :: BS.ByteString -> BS.ByteString
filepathNormalise xs
    | isWindows, Just (a,xs) <- BS.uncons xs, sep a, Just (b,_) <- BS.uncons xs, sep b = '/' `BS.cons` f xs
    | otherwise = f xs
    where
        sep = Native.isPathSeparator
        f o = deslash o $ BS.concat $ (slash:) $ intersperse slash $ reverse $ (BS.empty:) $ g 0 $ reverse $ split o

        deslash o x
            | x == slash = case (pre,pos) of
                (True,True) -> slash
                (True,False) -> BS.pack "/."
                (False,True) -> BS.pack "./"
                (False,False) -> dot
            | otherwise = (if pre then id else BS.tail) $ (if pos then id else BS.init) x
            where pre = not (BS.null o) && sep (BS.head o)
                  pos = not (BS.null o) && sep (BS.last o)

        g i [] = replicate i dotDot
        g i (x:xs) | BS.null x = g i xs
        g i (x:xs) | x == dotDot = g (i+1) xs
        g i (x:xs) | x == dot = g i xs
        g 0 (x:xs) = x : g 0 xs
        g i (x:xs) = g (i-1) xs

        split = BS.splitWith sep

dotDot = BS.pack ".."
dot = BS.singleton '.'
slash = BS.singleton '/'
