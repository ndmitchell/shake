
module Development.Shake.Continue(
    Continue(..), ($$),
    Fence, newFence, signalFence, waitFence, testFence,
    mendFences,
    addPoolContinue
    ) where

import Control.Applicative
import Control.Exception as E
import Control.Monad
import Data.IORef
import Development.Shake.Pool


newtype Continue a = Continue {runContinue :: Either SomeException a -> IO ()}

($$) :: Continue b -> (a -> IO b) -> Continue a
($$) c op = Continue $ \v -> case v of
    Left e -> runContinue c $ Left e
    Right v -> runContinue c . Right =<< op v


-- | Like a barrier, but based on callbacks
newtype Fence a = Fence (IORef (Either [Continue a] (Either SomeException a)))
instance Show (Fence a) where show _ = "Fence"


newFence :: IO (Fence a)
newFence = Fence <$> newIORef (Left [])

signalFence :: Fence a -> Continue a
signalFence (Fence ref) = Continue $ \v -> join $ atomicModifyIORef ref $ \x -> case x of
    Left queue -> (Right v, mapM_ (($ v) . runContinue) $ reverse queue)
    Right v -> error "Shake internal error, signalFence called twice on one Fence"

waitFence :: Fence a -> Continue a -> IO ()
waitFence (Fence ref) call = join $ atomicModifyIORef ref $ \x -> case x of
    Left queue -> (Left (call:queue), return ())
    Right v -> (Right v, runContinue call v)

testFence :: Fence a -> IO (Maybe (Either SomeException a))
testFence (Fence x) = either (const Nothing) Just <$> readIORef x


-- | Left is continue, Right is abort
mendFences :: [Fence v] -> r -> (r -> v -> Either r r) -> IO (Fence r)
mendFences deps r op = do
    res <- newFence
    if null deps then
        signalFence res `runContinue` Right r
     else do
        ref <- newIORef $ Just (length deps, r)
        forM_ deps $ \dep ->
            waitFence dep $ Continue $ \a ->
                join $ atomicModifyIORef ref $ \v -> case v of
                    Nothing -> (Nothing, return ())
                    Just (n, r) -> case a of
                        Left e -> (Nothing, signalFence res `runContinue` Left e)
                        Right a -> case op r a of
                            Left r | n > 1 -> (Just (n-1, r), return ())
                            r -> (Nothing, signalFence res `runContinue` Right (either id id r))
    return res


-- FIXME: Move this to Pool
addPoolContinue :: Pool -> Continue a -> Continue a
addPoolContinue pool c = Continue $ \v ->
    addPool pool $ runContinue c v
