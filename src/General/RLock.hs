{-# LANGUAGE LambdaCase #-}
module General.RLock (RLock, new, acquire, release, with) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception

-- | A reentrant lock inspired by the one in the concurrent-extra package, to
--   work around https://github.com/basvandijk/concurrent-extra/issues/20
--   This implementation uses a single 'TVar' and therefore it is not *fair*
newtype RLock = RLock {_rlock :: TVar State}

data State
  = Locked !ThreadId !Int
  | Unlocked

new :: IO RLock
new = RLock <$> newTVarIO Unlocked

acquire :: RLock -> IO ()
acquire (RLock tv) = do
  tid <- myThreadId
  atomically $ do
    readTVar tv >>= \case
      Locked tid' n
        | tid == tid' ->
            writeTVar tv $! Locked tid' (n+1)
        | otherwise -> retry
      Unlocked ->
          writeTVar tv $! Locked tid 1

release :: RLock -> IO ()
release (RLock tv) = do
  tid <- myThreadId
  atomically $ do
    readTVar tv >>= \case
      Locked tid' n
        | tid == tid' ->
          writeTVar tv $! if n == 1 then Unlocked else Locked tid (n-1)
        | otherwise ->
          error "This thread does not hold the lock"
      Unlocked ->
        error "The lock is not held"

with :: RLock -> IO a -> IO a
with = liftA2 bracket_ acquire release
