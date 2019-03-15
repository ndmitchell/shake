{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables, DeriveDataTypeable, ViewPatterns #-}
{-# LANGUAGE ExistentialQuantification, DeriveFunctor, RecordWildCards, FlexibleInstances #-}

module Development.Shake.Internal.Core.Database(
    DatabasePoly(..)
    ) where

import Data.IORef
import General.Intern(Id, Intern)
import qualified General.Ids as Ids


-- | Invariant: The database does not have any cycles where a Key depends on itself.
--   Everything is mutable. intern and status must form a bijecttion.
--   There may be dangling Id's as a result of version changes.
data DatabasePoly key vMem vDisk = Database
    {intern :: IORef (Intern key) -- ^ Key |-> Id mapping
    ,status :: Ids.Ids (key, vMem) -- ^ Id |-> (Key, Status) mapping
    ,journal :: Id -> key -> vDisk -> IO () -- ^ Record all changes to status
    }
