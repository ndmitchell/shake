
-- | Code for ensuring cleanup actions are run.
module General.Cleanup(
    Cleanup, withCleanup, addCleanup
    ) where

import Control.Exception as E
import Control.Monad
import qualified Data.HashMap.Strict as Map
import Data.IORef


data S = S {unique :: !Int, items :: Map.HashMap Int (IO ())}

newtype Cleanup = Cleanup (IORef S)


-- | Run with some cleanup scope. Regardless of exceptions/threads, all 'addCleanup' actions
--   will be run by the time it exits.
withCleanup :: (Cleanup -> IO a) -> IO a
withCleanup act = do
    ref <- newIORef $ S 0 Map.empty
    act (Cleanup ref) `finally` do
        join $ atomicModifyIORef ref $ \s -> (s{items=Map.empty}, sequence_ $ Map.elems $ items s)


-- | Add a cleanup action to a 'Cleanup' scope. If the return action is not run by the time
--   'withCleanup' terminates then it will be run then.
addCleanup :: Cleanup -> IO () -> IO (IO ())
addCleanup (Cleanup ref) act = atomicModifyIORef ref $ \s -> let i = unique s in
    (,) (S (unique s + 1) (Map.insert i act $ items s)) $ do
        join $ atomicModifyIORef ref $ \s ->
            if not $ i `Map.member` items s then (s, return ()) else
                (s{items = Map.delete i $ items s}, act)
