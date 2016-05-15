{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Development.Shake.Monad(
    RAWP(..), RAW, Capture, runRAW,
    getRO, getRW, getsRO, getsRW, putRW, modifyRW,
    withRO, withRW,
    catchRAW, tryRAW, throwRAW,
    resetRAW, shiftRAW,
    unmodifyRW, captureRAW,
    ) where

import Control.Exception.Extra
import Control.Monad.IO.Class
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Reader
import Data.IORef
import Control.Applicative
import Control.Monad
import Prelude


data S ro rw = S
    {handler :: IORef (SomeException -> IO ())
      -- ^ exception handler. Whenever we run a RAW, we call catch in IO with an exception handler that forwards to the IORef
    ,ro :: ro
    ,rww :: IORef rw -- ^ Read/Write Writeable var (rww)
    }

-- | A continuation monad transformer with extra data available
newtype RAWP rd r m a = RAW {fromRAW :: ReaderT rd (ContT r m) a }
    deriving (Functor, Applicative, Monad, MonadIO)

type RAW ro rw a = RAWP (S ro rw) () IO a
type Capture a = (a -> IO ()) -> IO ()

-- See https://ghc.haskell.org/trac/ghc/ticket/11555
catchSafe :: IO a -> (SomeException -> IO a) -> IO a
catchSafe = catch_

-- | Un-MTL'd type
runRAWP :: RAWP rd r m a -> rd -> (a -> m r) -> m r
runRAWP r rd k = fromRAW r `runReaderT` rd `runContT` k

-- | Run and then call a continuation.
runRAW :: ro -> rw -> RAW ro rw a -> Capture (Either SomeException a)
runRAW ro rw m k = do
    rww <- newIORef rw
    handler <- newIORef $ k . Left
    runRAWP m (S handler ro rww) (k . Right)
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

-- | @'catchRAW' m hdl@ catches any exceptions from @m@, including those thrown by users calling 'error', and runs the handler.
--   It obeys the rule @catchM (x >> throwM e) (\_ -> y) >> z === x >> y >> z@
catchRAW :: RAW ro rw a -> (SomeException -> RAW ro rw a) -> RAW ro rw a
catchRAW m hdl = RAW $ ReaderT $ \s -> ContT $ \k -> do
    -- store previous exception handler (to get a stack of them)
    old <- readIORef $ handler s
    writeIORef (handler s) $ \e -> do
        writeIORef (handler s) old
        runRAWP (hdl e) s k `catchSafe`
            \e -> ($ e) =<< readIORef (handler s)
    runRAWP m s $ \v -> do
        writeIORef (handler s) old
        k v

tryRAW :: RAW ro rw a -> RAW ro rw (Either SomeException a)
tryRAW m = catchRAW (fmap Right m) (return . Left)

-- | Throw an exception in the monad. It uses 'throwIO' for its guarantees about when the exception is raised
throwRAW :: (Exception e, MonadIO m) => e -> m a
throwRAW = liftIO . throwIO

-- if the continuation's caller raises an exception, the continuation is never called, so you cannot implement a robust finally function

---------------------------------------------------------------------
-- Delimited control flow

-- | @'resetRAW' m@ delimits the continuation of any 'shiftRAW' inside @m@.
resetRAW :: (Monad m) => RAWP rd r m r -> RAWP rd r' m r
resetRAW (RAW c) = RAW $ ReaderT $ \s -> resetT (runReaderT c s)

-- | @'shiftRAW' f@ captures the continuation up to the nearest enclosing
-- 'resetRAW' and passes it to @f@. It does not handle errors.
shiftRAW :: (Monad m) => ((a -> m r) -> RAWP rd r m r) -> RAWP rd r m a
shiftRAW f = RAW $ ReaderT $ \s -> shiftT (\c -> fromRAW (f c) `runReaderT` s)

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
--   Calling the same continuation multiple times may result in incorrect behaviour.
--   Properties:
--   1. If you execute something not using the continuation inside captureM it must behave like it does outside captureM.
--        In particular, if the captureM is inside a catchM, that catchM must not catch the exception.
--    captureM (\k -> x) >>= y === x
--    catchM (captureM (\k -> x)) hdl >>= y === x
--   2. If you capture the continuation, then continue, that must be equivalent to not capturing the continuation.
--    captureM (\k -> k x) >>= y === x >>= y
--   3. (Not implemented) If you run the continuation twice, it must do the same IO actions each time.
--        In particular, if the first gets its exceptions caught, the second must also.
--    captureM (\k -> k x >> k x) >>= y === (x >>= y) >> (x >>= y)
captureRAW :: Capture (Either SomeException a) -> RAW ro rw a
captureRAW f = RAW $ ReaderT $ \s -> ContT $ \k -> do
    old <- readIORef (handler s)
    writeIORef (handler s) throwIO
    -- run user's code
    f $ \x -> case x of
        Left e -> old e
        Right v -> do
            writeIORef (handler s) old
            k v `catchSafe` \e -> ($ e) =<< readIORef (handler s)
            -- back to user's code
            writeIORef (handler s) throwIO
