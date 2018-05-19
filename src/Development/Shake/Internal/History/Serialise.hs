{-# LANGUAGE FlexibleInstances #-}

-- | The endpoints on the cloud server
module Development.Shake.Internal.History.Serialise(
    BuildTree(..),
    SendAllKeys(..), RecvAllKeys(..),
    SendOneKey(..), RecvOneKey(..),
    SendDownloadFiles(..),
    SendUpload(..)
    ) where

import Development.Shake.Internal.History.Bloom
import General.Extra
import General.Binary
import Data.List.Extra
import Development.Shake.Internal.FileInfo
import Development.Shake.Internal.History.Types
import Data.Semigroup


data BuildTree key
    -- invariant: Entries are sorted
    = Depend [key] [([BS_Identity], BuildTree key)]
    | Done BS_Store [(FilePath, FileSize, FileHash)]

instance BinaryEx (BuildTree Int) where
    getEx = undefined
    putEx = undefined

instance Eq key => Semigroup (BuildTree key) where
    Depend ks1 vs1 <> Depend ks2 vs2
        | ks1 == ks2 = Depend ks1 $ mergeBy undefined vs1 vs2
        | otherwise = Depend ks2 vs2 -- this shouldn't happen, so give up
    x@Done{} <> _ = x
    _ <> y@Done{} = y

instance Eq key => Monoid (BuildTree key) where
    mempty = Depend [] []
    mappend = (<>)


data SendAllKeys typ = SendAllKeys Ver [(typ, Ver)]

instance BinaryEx (SendAllKeys Int) where
    putEx = undefined
    getEx = undefined

newtype RecvAllKeys key = RecvAllKeys [(key, Ver, [key], Bloom [BS_Identity])]

instance BinaryEx (RecvAllKeys Int) where
    getEx = undefined
    putEx = undefined

data SendOneKey key = SendOneKey Ver key Ver Ver [(key, BS_Identity)]

instance BinaryEx (SendOneKey Int) where
    getEx = undefined
    putEx = undefined

type RecvOneKey key = BuildTree key

data SendDownloadFiles key = SendDownloadFiles Ver key Ver Ver [(FilePath, FileSize, FileHash)]

instance BinaryEx (SendDownloadFiles Int) where
    getEx = undefined
    putEx = undefined

data SendUpload key = SendUpload Ver key Ver Ver [[(key, BS_Identity)]] BS_Store [(FilePath, FileSize, FileHash)]

instance BinaryEx (SendUpload Int) where
    getEx = undefined
    putEx = undefined
