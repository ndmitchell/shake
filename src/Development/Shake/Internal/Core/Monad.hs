{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs, ScopedTypeVariables, TupleSections #-}

module Development.Shake.Internal.Core.Monad(
    RAW, Capture, runRAW,
    getRO, getRW, putRW, modifyRW,
    stepRAW,
    catchRAW, tryRAW, throwRAW, finallyRAW,
    captureRAW,
    ) where

import Control.Exception.Extra
import Control.Monad.IO.Class
import General.ListBuilder
import Data.IORef
import Control.Monad
import System.IO
import Data.Semigroup
import Prelude

#if __GLASGOW_HASKELL__ >= 800
import Control.Monad.Fail
#endif


data RAW k v ro rw a where
    Fmap :: (a -> b) -> RAW k v ro rw a -> RAW k v ro rw b
    Pure :: a -> RAW k v ro rw a
    Ap :: RAW k v ro rw (a -> b) -> RAW k v ro rw a -> RAW k v ro rw b
    Next :: RAW k v ro rw a -> RAW k v ro rw b -> RAW k v ro rw b
    Bind :: RAW k v ro rw a -> (a -> RAW k v ro rw b) -> RAW k v ro rw b
    LiftIO :: IO a -> RAW k v ro rw a
    GetRO :: RAW k v ro rw ro
    GetRW :: RAW k v ro rw rw
    PutRW :: !rw -> RAW k v ro rw ()
    ModifyRW :: (rw -> rw) -> RAW k v ro rw ()
    StepRAW :: (Tree v -> a) -> Tree k -> RAW k v ro rw a
    CaptureRAW :: Capture (Either SomeException a) -> RAW k v ro rw a
    CatchRAW :: RAW k v ro rw a -> (SomeException -> RAW k v ro rw a) -> RAW k v ro rw a

instance Functor (RAW k v ro rw) where
    fmap = Fmap

instance Applicative (RAW k v ro rw) where
    pure = Pure
    (*>) = Next
    (<*>) = Ap

instance Monad (RAW k v ro rw) where
    return = pure
    (>>) = (*>)
    (>>=) = Bind

instance MonadIO (RAW k v ro rw) where
    liftIO = LiftIO

#if __GLASGOW_HASKELL__ >= 800
instance MonadFail (RAW k v ro rw) where
    fail = liftIO . Control.Monad.Fail.fail
#endif

instance Semigroup a => Semigroup (RAW k v ro rw a) where
    (<>) a b = (<>) <$> a <*> b

instance (Semigroup a, Monoid a) => Monoid (RAW k v ro rw a) where
    mempty = pure mempty
    mappend = (<>)


type Capture a = (a -> IO ()) -> IO ()


-- Useful for checking that all continuations are run only once
-- Cannot be enabled for performance reasons and because some of
-- "monad test" deliberately breaks the invariant to check it doesn't go wrong
assertOnceCheck = False

assertOnce :: MonadIO m => String -> (a -> m b) -> IO (a -> m b)
assertOnce msg k
    | not assertOnceCheck = return k
    | otherwise = do
        ref <- liftIO $ newIORef False
        return $ \v -> do
            liftIO $ join $ atomicModifyIORef ref $ \old -> (True,) $ when old $ do
                hPutStrLn stderr "FATAL ERROR: assertOnce failed"
                Prelude.fail $ "assertOnce failed: " ++ msg
            k v

-- | Run and then call a continuation.
runRAW :: ([k] -> RAW k v ro rw [v]) -> ro -> rw -> RAW k v ro rw a -> Capture (Either SomeException a)
runRAW step ro rw m k = do
    k <- assertOnce "runRAW" k
    rw <- newIORef rw
    handler <- newIORef throwIO
    writeIORef handler $ \e -> do
        -- make sure we never call the error continuation twice
        writeIORef handler throwIO
        k $ Left e
    -- If the continuation itself throws an error we need to make sure we
    -- don't end up running it twice (once with its result, once with its own exception)
    goRAW step handler ro rw m (\v -> do writeIORef handler throwIO; k $ Right v)
        `catch_` \e -> ($ e) =<< readIORef handler


goRAW :: forall k v ro rw a . ([k] -> RAW k v ro rw [v]) -> IORef (SomeException -> IO ()) -> ro -> IORef rw -> RAW k v ro rw a -> Capture a
goRAW step handler ro rw = go
    where
        go :: RAW k v ro rw b -> Capture b
        go x k = case x of
            Fmap f a -> go a $ \v -> k $ f v
            Pure a -> k a
            Ap f x -> go f $ \f -> go x $ \v -> k $ f v
            Next a b -> go a $ \_ -> go b k
            Bind a b -> go a $ \a -> go (b a) k
            LiftIO x -> k =<< x

            GetRO -> k ro
            GetRW -> k =<< readIORef rw
            PutRW x -> writeIORef rw x >> k ()
            ModifyRW f -> modifyIORef' rw f >> k ()

            StepRAW f qs -> go (step $ flattenTree qs) $ k . f . unflattenTree qs

            CatchRAW m hdl -> do
                hdl <- assertOnce "CatchRAW" hdl
                old <- readIORef handler
                writeIORef handler $ \e -> do
                    writeIORef handler old
                    go (hdl e) k `catch_`
                        \e -> ($ e) =<< readIORef handler
                go m $ \x -> writeIORef handler old >> k x

            CaptureRAW f -> do
                f <- assertOnce "CaptureRAW" f
                old <- readIORef handler
                writeIORef handler throwIO
                f $ \x -> case x of
                    Left e -> old e
                    Right v -> do
                        writeIORef handler old
                        k v `catch_` \e -> ($ e) =<< readIORef handler


---------------------------------------------------------------------
-- STANDARD

getRO :: RAW k v ro rw ro
getRO = GetRO

getRW :: RAW k v ro rw rw
getRW = GetRW

-- | Strict version
putRW :: rw -> RAW k v ro rw ()
putRW = PutRW

modifyRW :: (rw -> rw) -> RAW k v ro rw ()
modifyRW = ModifyRW


---------------------------------------------------------------------
-- EXCEPTIONS

catchRAW :: RAW k v ro rw a -> (SomeException -> RAW k v ro rw a) -> RAW k v ro rw a
catchRAW = CatchRAW

tryRAW :: RAW k v ro rw a -> RAW k v ro rw (Either SomeException a)
tryRAW m = catchRAW (fmap Right m) (return . Left)

throwRAW :: Exception e => e -> RAW k v ro rw a
-- Note that while we could directly pass this to the handler
-- that would avoid triggering the catch, which would mean they built up on the stack
throwRAW = liftIO . throwIO

finallyRAW :: RAW k v ro rw a -> RAW k v ro rw b -> RAW k v ro rw a
finallyRAW a undo = do
    r <- catchRAW a (\e -> undo >> throwRAW e)
    undo
    return r


---------------------------------------------------------------------
-- CONTINUATIONS

-- | Capture a continuation. The continuation should be called at most once.
--   Calling the same continuation, multiple times, in parallel, results in incorrect behaviour.
captureRAW :: Capture (Either SomeException a) -> RAW k v ro rw a
captureRAW = CaptureRAW


---------------------------------------------------------------------
-- STEPS

stepRAW :: k -> RAW k v ro rw v
stepRAW k = StepRAW (\(Leaf v) -> v) (Leaf k)
