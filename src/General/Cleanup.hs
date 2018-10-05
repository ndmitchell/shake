
-- | Code for ensuring cleanup actions are run.
module General.Cleanup(
    Cleanup, newCleanup, withCleanup,
    addCleanup, addCleanup_,
    allocateCleanup
    ) where

import Control.Exception
import qualified Data.HashMap.Strict as Map
import Control.Monad
import Data.IORef.Extra
import Data.List.Extra


data S = S
    {unique :: {-# UNPACK #-} !Int -- next index to be used to items
    ,items :: !(Map.HashMap Int (IO ()))
    }

newtype Cleanup = Cleanup (IORef S)


-- | Run with some cleanup scope. Regardless of exceptions/threads, all 'addCleanup' actions
--   will be run by the time it exits. The 'addCleanup' actions will be run in reverse order.
withCleanup :: (Cleanup -> IO a) -> IO a
withCleanup act = do
    (c, clean) <- newCleanup
    act c `finally` clean

newCleanup :: IO (Cleanup, IO ())
newCleanup = do
    ref <- newIORef $ S 0 Map.empty
    let clean = do
            items <- atomicModifyIORef' ref $ \s -> (s{items=Map.empty}, items s)
            mapM_ snd $ sortOn (negate . fst) $ Map.toList items
    return (Cleanup ref, clean)

-- | Add a cleanup action to a 'Cleanup' scope, returning a way to remove (but not perform) that action.
--   If not removed by the time 'withCleanup' terminates then the cleanup action will be run then.
--   Returns True for delete successful, False for already deleted by (potentially by runCleanup).
addCleanup :: Cleanup -> IO () -> IO (IO Bool)
addCleanup (Cleanup ref) act = atomicModifyIORef' ref $ \s -> let i = unique s in
    (,) (S (unique s + 1) (Map.insert i act $ items s)) $
        atomicModifyIORef' ref $ \s -> (s{items = Map.delete i $ items s}, Map.member i $ items s)

addCleanup_ :: Cleanup -> IO () -> IO ()
-- we could avoid inserting into the Map, but we need to store the pairs anyway
-- to unregister them in order, so might as well keep it simple
addCleanup_ c act = void $ addCleanup c act

allocateCleanup :: Cleanup -> IO a -> (a -> IO ()) -> IO a
allocateCleanup cleanup acquire release =
    mask_ $ do
        v <- acquire
        addCleanup_ cleanup $ release v
        return v
