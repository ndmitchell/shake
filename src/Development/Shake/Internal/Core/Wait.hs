{-# LANGUAGE ExistentialQuantification #-}

module Development.Shake.Internal.Core.Wait(
    Wait, newWait, afterWait,
    Waits(..),
    waitExcept
    ) where

import Control.Monad
import Data.IORef.Extra
import Data.Primitive.Array
import Data.Either
import Development.Shake.Internal.Errors


-- | A compuation that either has a result available immediate,
--   or has a result that can be collected later.
data Waits a
    = Now a
    | Later (Wait a)


partitionCompute :: [Waits a] -> ([a], [Wait a])
partitionCompute = foldr f ([],[])
    where f (Now   x) ~(xs,ws) = (x:xs,ws)
          f (Later w) ~(xs,ws) = (xs,w:ws)


-- | A type representing someone waiting for a result.
data Wait a = forall b . Wait (b -> a) (IORef (b -> IO ()))
    -- Contains a functor value to apply, along with somewhere to register callbacks

instance Functor Wait where
    fmap f (Wait op ref) = Wait (f . op) ref

instance Show (Wait a) where
    show _ = "Wait"


newWait :: IO (Wait a, a -> IO ())
newWait = do
    ref <- newIORef $ \_ -> return ()
    let run x = ($ x) =<< readIORef ref
    return (Wait id ref, run)

afterWait :: Wait a -> (a -> IO ()) -> IO ()
afterWait (Wait op ref) act = modifyIORef' ref (\a s -> a s >> act (op s))


waitExcept :: [Waits (Either e a)] -> IO (Waits (Either e [a]))
waitExcept xs = do
    let (now, later) = partitionCompute xs
    let (abort, continue) = partitionEithers now
    if not $ null abort then
        return $ Now $ Left $ head abort
     else if null later then
        return $ Now $ Right continue
     else do
        (waiting, run) <- newWait
        let n = length xs
        result <- newArray n $ throwImpure $ errorInternal "rendezvous"
        todo <- newIORef $ length later
        forM_ (zip [0..] xs) $ \(i,x) -> case x of
            Now (Right c) -> writeArray result i c
            Later w -> afterWait w $ \v -> do
                t <- readIORef todo
                case v of
                    _ | t == 0 -> return () -- must have already aborted
                    Left a -> do
                        writeIORef todo 0
                        run $ Left a
                    Right c -> do
                        writeArray result i c
                        writeIORef' todo $ t-1
                        when (t == 1) $ do
                            rs <- unsafeFreezeArray result
                            run $ Right $ map (indexArray rs) [0..n-1]
        return $ Later waiting
