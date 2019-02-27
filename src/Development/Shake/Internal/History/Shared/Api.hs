{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}
{-# OPTIONS -Wall #-}

module Development.Shake.Internal.History.Shared.Api
  ( SharedCache(..)
  , SharedFileSystem(..)
  ) where

import qualified Data.ByteString                          as BS
import           Data.Maybe
import           Development.Shake.Classes
import           Development.Shake.Internal.FileInfo
import           Development.Shake.Internal.History.Types
import           Development.Shake.Internal.Value
import           General.Binary
import           General.Extra
import           General.Wait
import           Prelude

data SharedCache shared = SharedCache
    { addShared :: shared -> Key -> Ver -> Ver -> [[(Key, BS_Identity)]] -> BS_Store -> [FilePath] -> IO ()
    , lookupShared :: (Eq shared, Storable shared) =>
                          shared -> (Key -> Wait Locked (Maybe BS_Identity)) -> Key -> Ver -> Ver -> Wait Locked (Maybe (BS_Store, [[Key]], IO ()))
    , removeShared :: shared -> (Key -> Bool) -> IO ()
    , listShared :: shared -> IO ()
    }

-- | An abstraction for shared cache filesystems.
data SharedFileSystem shared = SharedFileSystem
  {
    loadSharedEntry :: shared -> Key -> IO [BS.ByteString]
  , saveSharedEntry :: shared -> Key -> [(FilePath, FileHash)] -> Builder -> IO ()
  -- | Remove keys for which the predicate returns False
  , updateSharedKeys :: shared -> (Key -> IO Bool) -> IO ()
  }
