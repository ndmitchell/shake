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
  = Locked !ThreadId
  | Unlocked

new :: IO RLock
new = RLock <$> newTVarIO Unlocked

acquire :: RLock -> IO Bool
acquire (RLock tv) = do
  tid <- myThreadId
  atomically $ do
    readTVar tv >>= \case
      Locked tid'
        | tid == tid' ->
            return False
        | otherwise -> retry
      Unlocked -> do
          writeTVar tv $! Locked tid
          return True

release :: RLock -> Bool -> IO ()
release (RLock tv) True  = atomically $ writeTVar tv Unlocked
release _          False = return ()

with :: RLock -> IO a -> IO a
with rl act = bracket (acquire rl) (release rl) (const act)
