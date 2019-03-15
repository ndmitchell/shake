{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables, DeriveDataTypeable, ViewPatterns #-}
{-# LANGUAGE ExistentialQuantification, DeriveFunctor, RecordWildCards, FlexibleInstances #-}

module Development.Shake.Internal.Core.Database(
    Locked, runLocked,
    DatabasePoly(..), getKey
    ) where

import Data.IORef
import General.Intern(Id, Intern)
import Control.Concurrent.Extra
import Control.Monad.IO.Class
import qualified General.Ids as Ids
import Data.Maybe

#if __GLASGOW_HASKELL__ >= 800
import Control.Monad.Fail
#endif


newtype Locked a = Locked (IO a)
    deriving (Functor, Applicative, Monad, MonadIO
#if __GLASGOW_HASKELL__ >= 800
             ,MonadFail
#endif
        )

runLocked :: Var a -> (a -> Locked b) -> IO b
runLocked var act = withVar var $ \v -> case act v of Locked x -> x


-- | Invariant: The database does not have any cycles where a Key depends on itself.
--   Everything is mutable. intern and status must form a bijecttion.
--   There may be dangling Id's as a result of version changes.
data DatabasePoly key vMem vDisk = Database
    {intern :: IORef (Intern key) -- ^ Key |-> Id mapping
    ,status :: Ids.Ids (key, vMem) -- ^ Id |-> (Key, Status) mapping
    ,journal :: Id -> key -> vDisk -> IO () -- ^ Record all changes to status
    }


getKey :: DatabasePoly key vMem vDisk -> Id -> IO key
getKey Database{..} x = fst . fromJust <$> Ids.lookup status x
