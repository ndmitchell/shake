{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards #-}

module Development.Shake.Internal.Core.Database(
    Locked, runLocked, unsafeRunLocked,
    DatabasePoly, createDatabase,
    getId, getKey, getKeyValue, getIdMaybe,
    getAllKeyValues, getIdMap,
    setMem, setDisk, modifyAllMem
    ) where

import Data.IORef.Extra
import General.Intern(Id, Intern)
import Development.Shake.Classes
import qualified Data.HashMap.Strict as Map
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

runLocked :: DatabasePoly k v -> Locked b -> IO b
runLocked db (Locked act) = withLock (lock db) act

unsafeRunLocked :: Locked a -> IO a
unsafeRunLocked (Locked act) = act


-- | Invariant: The database does not have any cycles where a Key depends on itself.
--   Everything is mutable. intern and status must form a bijecttion.
--   There may be dangling Id's as a result of version changes.
data DatabasePoly k v = Database
    {lock :: Lock
    ,intern :: IORef (Intern k) -- ^ Key |-> Id mapping
    ,status :: Ids.Ids (k, v) -- ^ Id |-> (Key, Status) mapping
    ,journal :: Id -> k -> v -> IO () -- ^ Record all changes to status
    ,vDefault :: v
    }


createDatabase
    :: (Eq k, Hashable k)
    => Ids.Ids (k, v)
    -> (Id -> k -> v -> IO ())
    -> v
    -> IO (DatabasePoly k v)
createDatabase status journal vDefault = do
    xs <- Ids.toList status
    intern <- newIORef $ Intern.fromList [(k, i) | (i, (k,_)) <- xs]
    lock <- newLock
    return Database{..}


getKey :: DatabasePoly k v -> Id -> IO k
getKey Database{..} x = fst . fromJust <$> Ids.lookup status x

getIdMaybe :: (Eq k, Hashable k) => DatabasePoly k v -> IO (k -> Maybe Id)
getIdMaybe Database{..} = do
    is <- readIORef intern
    return $ flip Intern.lookup is

getId :: (Eq k, Hashable k) => DatabasePoly k v -> k -> Locked Id
getId Database{..} k = liftIO $ do
    is <- readIORef intern
    case Intern.lookup k is of
        Just i -> return i
        Nothing -> do
            (is, i) <- return $ Intern.add k is
            -- make sure to write it into Status first to maintain Database invariants
            Ids.insert status i (k, vDefault)
            writeIORef' intern is
            return i

-- Returns Nothing only if the Id was serialised previously but then the Id disappeared
getKeyValue :: DatabasePoly k v -> Id -> Locked (Maybe (k, v))
getKeyValue Database{..} i = liftIO $ Ids.lookup status i

getAllKeyValues :: DatabasePoly k v -> IO [(k, v)]
getAllKeyValues Database{..} = Ids.elems status

getIdMap :: DatabasePoly k v -> IO (Map.HashMap Id (k, v))
getIdMap Database{..} = Ids.toMap status

setMem :: DatabasePoly k v -> Id -> k -> v -> Locked ()
setMem Database{..} i k v = liftIO $ Ids.insert status i (k,v)

modifyAllMem :: DatabasePoly k v -> (v -> v) -> Locked ()
modifyAllMem Database{..} f = liftIO $ Ids.forMutate status $ \(k, s) -> (k, f s)

setDisk :: DatabasePoly k v -> Id -> k -> v -> IO ()
setDisk = journal
