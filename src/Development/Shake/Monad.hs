{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Development.Shake.Monad(
    RAWP(..), RAW, Capture, runRAW,
    getRO, getRW, getsRO, getsRW, putRW, modifyRW,
    withRO, withRW,
    catchRAW, tryRAW, throwRAW,
    unmodifyRW, captureRAW,
    ) where

import Control.Exception.Extra
import Control.Monad.IO.Class
import Control.Monad.Trans.Cont as Cont
import Control.Monad.Trans.Reader
import Data.IORef
import Control.Applicative
import Control.Monad
import Prelude


data S ro rw = S
    {handler :: IORef (SomeException -> IO ())
    ,ro :: ro
    ,rww :: IORef rw -- Read/Write Writeable var (rww)
    }

newtype RAWP rd r m a = RAW {fromRAW :: ReaderT rd (ContT r m) a }
    deriving (Functor, Applicative, Monad, MonadIO)

type RAW ro rw a = RAWP (S ro rw) () IO a
type Capture a = (a -> IO ()) -> IO ()

-- See https://ghc.haskell.org/trac/ghc/ticket/11555
catchSafe :: IO a -> (SomeException -> IO a) -> IO a
catchSafe = catch_

-- | Run and then call a continuation.
runRAW :: ro -> rw -> RAW ro rw a -> Capture (Either SomeException a)
runRAW ro rw m k = do
    rww <- newIORef rw
    handler <- newIORef $ k . Left
    -- see https://ghc.haskell.org/trac/ghc/ticket/11555
    fromRAW m `runReaderT` S handler ro rww `runContT` (k . Right)
        `catchSafe` \e -> ($ e) =<< readIORef handler

---------------------------------------------------------------------
-- STANDARD

getRO :: RAWP (S ro rw) r m ro
getRO = RAW $ asks ro

getRW :: MonadIO m => RAWP (S ro rw) r m rw
getRW = RAW $ liftIO . readIORef =<< asks rww

getsRO :: (ro -> a) -> RAWP (S ro rw) r m a
getsRO f = fmap f getRO

getsRW :: MonadIO m => (rw -> a) -> RAWP (S ro rw) r m a
getsRW f = fmap f getRW

-- | Strict version
putRW :: MonadIO m => rw -> RAWP (S ro rw) r m ()
putRW rw = rw `seq` RAW $ liftIO . flip writeIORef rw =<< asks rww

withRAW :: (rd -> rd2) -> RAWP rd2 r m a -> RAWP rd r m a
withRAW f m = RAW $ withReaderT f $ fromRAW m

modifyRW :: MonadIO m => (rw -> rw) -> RAWP (S ro rw) r m ()
modifyRW f = do x <- getRW; putRW $ f x

withRO :: (ro -> ro2) -> RAWP (S ro2 rw) r m a -> RAWP (S ro rw) r m a
withRO f = withRAW $ \s -> s{ro=f $ ro s}

withRW :: MonadIO m => (rw -> rw2) -> RAWP (S ro rw2) r m a -> RAWP (S ro rw) r m a
withRW f m = do
    rw <- getRW
    rww <- liftIO $ newIORef $ f rw
    withRAW (\s -> s{rww=rww}) m

---------------------------------------------------------------------
-- EXCEPTIONS

catchRAW :: RAW ro rw a -> (SomeException -> RAW ro rw a) -> RAW ro rw a
catchRAW m hdl = RAW $ ReaderT $ \s -> ContT $ \k -> do
    old <- readIORef $ handler s
    writeIORef (handler s) $ \e -> do
        writeIORef (handler s) old
        fromRAW (hdl e) `runReaderT` s `runContT` k `catchSafe`
            \e -> ($ e) =<< readIORef (handler s)
    fromRAW m `runReaderT` s `runContT` \v -> do
        writeIORef (handler s) old
        k v


tryRAW :: RAW ro rw a -> RAW ro rw (Either SomeException a)
tryRAW m = catchRAW (fmap Right m) (return . Left)

throwRAW :: (Exception e, MonadIO m) => e -> m a
throwRAW = liftIO . throwIO


---------------------------------------------------------------------
-- Delimited control flow

-- | @'resetT' m@ delimits the continuation of any 'shiftT' inside @m@.
resetT :: (Monad m) => RAWP rd r m r -> RAWP rd r' m r
resetT (RAW c) = RAW $ ReaderT $ \s -> Cont.resetT (runReaderT c s)

-- | @'shiftT' f@ captures the continuation up to the nearest enclosing
-- 'resetT' and passes it to @f@
shiftT :: (Monad m) => ((a -> m r) -> RAWP rd r m r) -> RAWP rd r m a
shiftT f = RAW $ ReaderT $ \s -> Cont.shiftT (\c -> fromRAW (f c) `runReaderT` s)

---------------------------------------------------------------------
-- WEIRD STUFF

-- | Apply a modification, run an action, then undo the changes after.
unmodifyRW :: MonadIO m => (rw -> (rw, rw -> rw)) -> RAWP (S ro rw) r m a -> RAWP (S ro rw) r m a
unmodifyRW f m = do
    (s2,undo) <- fmap f getRW
    putRW s2
    res <- m
    modifyRW undo
    return res


-- | Capture a continuation. The continuation should be called at most once.
--   Calling the same continuation, multiple times, in parallel, results in incorrect behaviour.
captureRAW :: Capture (Either SomeException a) -> RAW ro rw a
captureRAW f = RAW $ ReaderT $ \s -> ContT $ \k -> do
    old <- readIORef (handler s)
    writeIORef (handler s) throwIO
    f $ \x -> case x of
        Left e -> old e
        Right v -> do
            writeIORef (handler s) old
            k v `catchSafe` \e -> ($ e) =<< readIORef (handler s)
            writeIORef (handler s) throwIO
