{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables, DeriveDataTypeable, ViewPatterns #-}
{-# LANGUAGE ExistentialQuantification, DeriveFunctor, RecordWildCards, FlexibleInstances #-}

module Development.Shake.Internal.Core.Database(
    Locked, runLocked, unsafeRunLocked,
    DatabasePoly(..),
    getId, getKey, getKeyValue,
    getAllKeyValues,
    setMem, setDisk
    ) where

import Data.IORef.Extra
import General.Intern(Id, Intern)
import Data.Hashable
import qualified General.Intern as Intern
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

runLocked :: Var (DatabasePoly key vMem vDisk) -> (DatabasePoly key vMem vDisk -> Locked b) -> IO b
runLocked var act = withVar var $ \v -> case act v of Locked x -> x

unsafeRunLocked :: Locked a -> IO a
unsafeRunLocked (Locked act) = act


-- | Invariant: The database does not have any cycles where a Key depends on itself.
--   Everything is mutable. intern and status must form a bijecttion.
--   There may be dangling Id's as a result of version changes.
data DatabasePoly key vMem vDisk = Database
    {intern :: IORef (Intern key) -- ^ Key |-> Id mapping
    ,status :: Ids.Ids (key, vMem) -- ^ Id |-> (Key, Status) mapping
    ,journal :: Id -> key -> vDisk -> IO () -- ^ Record all changes to status
    ,vMemDefault :: vMem
    }


getKey :: DatabasePoly key vMem vDisk -> Id -> IO key
getKey Database{..} x = fst . fromJust <$> Ids.lookup status x


getId :: (Eq key, Hashable key) => DatabasePoly key vMem vDisk -> key -> Locked Id
getId Database{..} k = liftIO $ do
    is <- readIORef intern
    case Intern.lookup k is of
        Just i -> return i
        Nothing -> do
            (is, i) <- return $ Intern.add k is
            -- make sure to write it into Status first to maintain Database invariants
            Ids.insert status i (k, vMemDefault)
            writeIORef' intern is
            return i

-- Returns Nothing only if the Id was serialised previously but then the Id disappeared
getKeyValue :: DatabasePoly key vMem vDisk -> Id -> Locked (Maybe (key, vMem))
getKeyValue Database{..} i = liftIO $ Ids.lookup status i

getAllKeyValues :: DatabasePoly key vMem vDisk -> IO [(key, vMem)]
getAllKeyValues Database{..} = Ids.elems status

setMem :: DatabasePoly key vMem vDisk -> Id -> key -> vMem -> Locked ()
setMem Database{..} i k v = liftIO $ Ids.insert status i (k,v)

setDisk :: DatabasePoly key vMem vDisk -> Id -> key -> vDisk -> IO ()
setDisk = journal
