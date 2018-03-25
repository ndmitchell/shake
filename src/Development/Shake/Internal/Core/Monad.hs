{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs, ScopedTypeVariables #-}

module Development.Shake.Internal.Core.Monad(
    RAW, Capture, runRAW,
    getRO, getRW, putRW, modifyRW,
    catchRAW, tryRAW, throwRAW,
    captureRAW,
    ) where

import Control.Exception.Extra
import Control.Monad.IO.Class
import Data.IORef
import Control.Applicative
import Control.Monad
import Prelude

#if __GLASGOW_HASKELL__ >= 800
import Control.Monad.Fail
#endif


data RAW ro rw a where
    Fmap :: (a -> b) -> RAW ro rw a -> RAW ro rw b
    Pure :: a -> RAW ro rw a
    Ap :: RAW ro rw (a -> b) -> RAW ro rw a -> RAW ro rw b
    Next :: RAW ro rw a -> RAW ro rw b -> RAW ro rw b
    Bind :: RAW ro rw a -> (a -> RAW ro rw b) -> RAW ro rw b
    LiftIO :: IO a -> RAW ro rw a
    GetRO :: RAW ro rw ro
    GetRW :: RAW ro rw rw
    PutRW :: !rw -> RAW ro rw ()
    ModifyRW :: (rw -> rw) -> RAW ro rw ()
    CaptureRAW :: Capture (Either SomeException a) -> RAW ro rw a
    CatchRAW :: RAW ro rw a -> (SomeException -> RAW ro rw a) -> RAW ro rw a

instance Functor (RAW ro rw) where
    fmap = Fmap

instance Applicative (RAW ro rw) where
    pure = Pure
    (*>) = Next
    (<*>) = Ap

instance Monad (RAW ro rw) where
    return = Pure
    (>>) = Next
    (>>=) = Bind

instance MonadIO (RAW ro rw) where
    liftIO = LiftIO

#if __GLASGOW_HASKELL__ >= 800
instance MonadFail (RAW ro rw) where
    fail = liftIO . Control.Monad.Fail.fail
#endif

type Capture a = (a -> IO ()) -> IO ()


-- | Run and then call a continuation.
runRAW :: ro -> rw -> RAW ro rw a -> Capture (Either SomeException a)
runRAW ro rw m k = do
    rw <- newIORef rw
    handler <- newIORef $ k . Left
    goRAW handler ro rw m (k . Right)
        `catch_` \e -> ($ e) =<< readIORef handler

goRAW :: forall ro rw a . IORef (SomeException -> IO ()) -> ro -> IORef rw -> RAW ro rw a -> Capture a
goRAW handler ro rw = go
    where
        go :: RAW ro rw b -> Capture b
        go x k = case x of
            Fmap f a -> go a $ \v -> k $ f v
            Pure a -> k a
            Ap f x -> go f $ \f -> go x $ \v -> k $ f v
            Bind a b -> go a $ \a -> go (b a) k
            Next a b -> go a $ \_ -> go b k
            LiftIO x -> k =<< x

            GetRO -> k ro
            GetRW -> k =<< readIORef rw
            PutRW x -> writeIORef rw x >> k ()
            ModifyRW f -> modifyIORef' rw f >> k ()

            CatchRAW m hdl -> do
                old <- readIORef handler
                writeIORef handler $ \e -> do
                    writeIORef handler old
                    go (hdl e) k `catch_`
                        \e -> ($ e) =<< readIORef handler
                go m $ \x -> writeIORef handler old >> k x

            CaptureRAW f -> do
                old <- readIORef handler
                writeIORef handler throwIO
                f $ \x -> case x of
                    Left e -> old e
                    Right v -> do
                        writeIORef handler old
                        k v `catch_` \e -> ($ e) =<< readIORef handler
                        writeIORef handler throwIO


---------------------------------------------------------------------
-- STANDARD

getRO :: RAW ro rw ro
getRO = GetRO

getRW :: RAW ro rw rw
getRW = GetRW

-- | Strict version
putRW :: rw -> RAW ro rw ()
putRW = PutRW

modifyRW :: (rw -> rw) -> RAW ro rw ()
modifyRW = ModifyRW


---------------------------------------------------------------------
-- EXCEPTIONS

catchRAW :: RAW ro rw a -> (SomeException -> RAW ro rw a) -> RAW ro rw a
catchRAW = CatchRAW

tryRAW :: RAW ro rw a -> RAW ro rw (Either SomeException a)
tryRAW m = catchRAW (fmap Right m) (return . Left)

throwRAW :: Exception e => e -> RAW ro rw a
throwRAW = liftIO . throwIO


---------------------------------------------------------------------
-- CONTINUATIONS

-- | Capture a continuation. The continuation should be called at most once.
--   Calling the same continuation, multiple times, in parallel, results in incorrect behaviour.
captureRAW :: Capture (Either SomeException a) -> RAW ro rw a
captureRAW = CaptureRAW
