
-- | Code for ensuring cleanup actions are run.
module General.Cleanup(
    Cleanup, withCleanup, addCleanup, addCleanup_
    ) where

import Control.Exception
import qualified Data.HashMap.Strict as Map
import Data.Function
import Control.Monad
import Data.IORef
import Data.List


data S = S {unique :: {-# UNPACK #-} !Int, items :: !(Map.HashMap Int (IO ()))}

newtype Cleanup = Cleanup (IORef S)


-- | Run with some cleanup scope. Regardless of exceptions/threads, all 'addCleanup' actions
--   will be run by the time it exits. The 'addCleanup' actions will be run in reverse order.
withCleanup :: (Cleanup -> IO a) -> IO a
withCleanup act = do
    ref <- newIORef $ S 0 Map.empty
    act (Cleanup ref) `finally` do
        items <- atomicModifyIORef' ref $ \s -> (s{items=Map.empty}, items s)
        mapM_ snd $ sortBy (compare `on` negate . fst) $ Map.toList items


-- | Add a cleanup action to a 'Cleanup' scope, returning a way to remove (but not perform) that action.
--   If not removed by the time 'withCleanup' terminates then the cleanup action will be run then.
addCleanup :: Cleanup -> IO () -> IO (IO ())
addCleanup (Cleanup ref) act = atomicModifyIORef' ref $ \s -> let i = unique s in
    (,) (S (unique s + 1) (Map.insert i act $ items s)) $
        atomicModifyIORef' ref $ \s -> (s{items = Map.delete i $ items s}, ())

addCleanup_ :: Cleanup -> IO () -> IO ()
-- we could avoid inserting into the Map, but we need to store the pairs anyway
-- to unregister them in order, so might as well keep it simple
addCleanup_ c act = void $ addCleanup c act
