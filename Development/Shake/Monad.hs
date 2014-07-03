{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Development.Shake.Monad(
    RAW, runRAW,
    getRO, getRW, getsRO, getsRW, putRW, modifyRW,
    withRO, withRW,
    catchRAW, tryRAW, throwRAW,
    evalRAW, unmodifyRW, captureRAW,
    ) where

import Control.Applicative
import Control.Exception as E
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.IORef

data S ro rw = S
    {ro :: ro
    ,rww :: IORef rw -- Read/Write Writeable var (rww)
    }

newtype RAW ro rw a = RAW {fromRAW :: ReaderT (S ro rw) IO a}
    deriving (Functor, Applicative, Monad, MonadIO)

runRAW :: ro -> rw -> RAW ro rw a -> IO a
runRAW ro rw (RAW m) = do
    rww <- newIORef rw
    runReaderT m $ S ro rww


---------------------------------------------------------------------
-- STANDARD

getRO :: RAW ro rw ro
getRO = RAW $ asks ro

getRW :: RAW ro rw rw
getRW = RAW $ liftIO . readIORef =<< asks rww

getsRO :: (ro -> a) -> RAW ro rw a
getsRO f = fmap f getRO

getsRW :: (rw -> a) -> RAW ro rw a
getsRW f = fmap f getRW

-- | Strict version
putRW :: rw -> RAW ro rw ()
putRW rw = rw `seq` RAW $ liftIO . flip writeIORef rw =<< asks rww

withRAW :: (S ro rw -> S ro2 rw2) -> RAW ro2 rw2 a -> RAW ro rw a
withRAW f m = RAW $ withReaderT f $ fromRAW m

modifyRW :: (rw -> rw) -> RAW ro rw ()
modifyRW f = do x <- getRW; putRW $ f x

withRO :: (ro -> ro2) -> RAW ro2 rw a -> RAW ro rw a
withRO f = withRAW $ \s -> s{ro=f $ ro s}

withRW :: (rw -> rw2) -> RAW ro rw2 a -> RAW ro rw a
withRW f m = do
    rw <- getRW
    rww <- liftIO $ newIORef $ f rw
    withRAW (\s -> s{rww=rww}) m


---------------------------------------------------------------------
-- EXCEPTIONS

catchRAW :: RAW ro rw a -> (SomeException -> RAW ro rw a) -> RAW ro rw a
catchRAW m handle = RAW $ liftCatch E.catch (fromRAW m) (fromRAW . handle)

tryRAW :: RAW ro rw a -> RAW ro rw (Either SomeException a)
tryRAW m = catchRAW (fmap Right m) (return . Left)

throwRAW :: Exception e => e -> RAW ro rw a
throwRAW = liftIO . throwIO


---------------------------------------------------------------------
-- WEIRD STUFF

-- | Given an action, produce a 'RAW' that runs fast, containing
--   an 'IO' that runs slowly (the bulk of the work) and a 'RAW'
--   that runs fast.
evalRAW :: RAW ro rw a -> RAW ro rw (IO (RAW ro rw a))
evalRAW m = do
        ro <- getRO
        rw <- getRW
        return $ do
            (a,rw) <- runRAW ro rw $ liftA2 (,) m getRW
            return $ do
                putRW rw
                return a


-- | Apply a modification, run an action, then undo the changes after.
unmodifyRW :: (rw -> (rw, rw -> rw)) -> RAW ro rw a -> RAW ro rw a
unmodifyRW f m = do
    (s2,undo) <- fmap f getRW
    putRW s2
    res <- m
    modifyRW undo
    return res


-- | Capture a continuation. The continuation must be called exactly once, either with an
--   exception, or with a result.
captureRAW :: ((Either SomeException a -> IO ()) -> IO ()) -> RAW ro rw a
captureRAW f = do
    s <- RAW ask
    undefined {- ContT $ \c -> do
        lift $ f $ \a -> do runReaderT (c a) s; return () -}


{-
type CaptureT m a = ContT () m a

runCaptureT :: Monad m => CaptureT m a -> m ()
runCaptureT act = runContT act $ \c -> return ()

capture :: ((a -> m ()) -> m ()) -> CaptureT m a
capture = ContT


type CaptureStateT s m a = CaptureT (StateT s m) a

runCaptureStateT :: Monad m => CaptureStateT s m a -> s -> m ()
runCaptureStateT act s = do runStateT (runCaptureT act) s; return ()

captureState :: Monad m => ((a -> m ()) -> m ()) -> CaptureStateT s m a
captureState f = do
    s <- lift State.get
    capture $ \c -> do
        lift $ f $ \a -> do runStateT (c a) s; return ()
-}
