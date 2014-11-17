
-- | Code for ensuring cleanup actions are run.
module General.Cleanup(
    Cleanup, withCleanup, addCleanup
    ) where

import Control.Exception as E
import Control.Monad
import qualified Data.HashMap.Strict as Map
import Data.Function
import Data.IORef
import Data.List


data S = S {unique :: !Int, items :: Map.HashMap Int (IO ())}

newtype Cleanup = Cleanup (IORef S)


-- | Run with some cleanup scope. Regardless of exceptions/threads, all 'addCleanup' actions
--   will be run by the time it exits. The 'addCleanup' actions will be run in reverse order.
withCleanup :: (Cleanup -> IO a) -> IO a
withCleanup act = do
    ref <- newIORef $ S 0 Map.empty
    act (Cleanup ref) `finally` do
        items <- atomicModifyIORef ref $ \s -> (s{items=Map.empty}, items s)
        mapM_ snd $ sortBy (compare `on` negate . fst) $ Map.toList items


-- | Add a cleanup action to a 'Cleanup' scope. If the return action is not run by the time
--   'withCleanup' terminates then it will be run then. The argument 'Bool' is 'True' to say
--   run the action, 'False' to say ignore the action (and never run it).
addCleanup :: Cleanup -> IO () -> IO (Bool -> IO ())
addCleanup (Cleanup ref) act = atomicModifyIORef ref $ \s -> let i = unique s in
    (,) (S (unique s + 1) (Map.insert i act $ items s)) $ \b -> do
        join $ atomicModifyIORef ref $ \s -> case Map.lookup i $ items s of
            Nothing -> (s, return ())
            Just act -> (s{items = Map.delete i $ items s}, when b act)
