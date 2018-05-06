{-# LANGUAGE RecordWildCards, TupleSections #-}

module Development.Shake.Internal.History.Types(
    BS_Store, BS_Identity
    ) where

import qualified Data.ByteString as BS

type BS_Store = BS.ByteString
type BS_Identity = BS.ByteString
