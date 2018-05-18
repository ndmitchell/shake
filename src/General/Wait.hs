{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving, CPP #-}

-- | A bit like 'Fence', but not thread safe and optimised for avoiding taking the fence
module General.Wait(
    Locked, runLocked,
    Wait(..), runWait, quickly, fromLater,
    firstJustWaitUnordered, firstLeftWaitUnordered
    ) where

import Control.Monad.Extra
import Control.Monad.IO.Class
import Control.Concurrent.Extra
import Data.IORef.Extra
import Data.List.Extra
import Data.Primitive.Array
import GHC.Exts(RealWorld)
import Control.Applicative
import Prelude

#if __GLASGOW_HASKELL__ >= 800
import Control.Monad.Fail
#endif


runWait :: Monad m => Wait m a -> m (Wait m a)
runWait (Lift x) = runWait =<< x
runWait x = return x

fromLater :: Monad m => Wait m a -> (a -> m ()) -> m ()
fromLater (Lift x) f = do x <- x; fromLater x f
fromLater (Now x) f = f x
fromLater (Later x) f = x f

newtype Locked a = Locked (IO a)
    deriving (Functor, Applicative, Monad, MonadIO
#if __GLASGOW_HASKELL__ >= 800
             ,MonadFail
#endif
        )

runLocked :: Var a -> (a -> Locked b) -> IO b
runLocked var act = withVar var $ \v -> case act v of Locked x -> x

quickly :: Functor m => m a -> Wait m a
quickly = Lift . fmap Now

data Wait m a = Now a
              | Lift (m (Wait m a))
              | Later ((a -> m ()) -> m ())
                deriving Functor

instance (Monad m, Applicative m) => Applicative (Wait m) where
    pure = Now
    Now x <*> y = x <$> y
    Lift x <*> y = Lift $ (<*> y) <$> x
    Later x <*> Now y = Later $ \c -> x $ \x -> c $ x y
    -- Note: We pull the Lift from the right BEFORE the Later, to enable parallelism
    Later x <*> Lift y = Lift $ do y <- y; return $ Later x <*> y
    Later x <*> Later y = Later $ \c -> x $ \x -> y $ \y -> c $ x y

instance (Monad m, Applicative m) => Monad (Wait m) where
    return = pure
    (>>) = (*>)
    Now x >>= f = f x
    Lift x >>= f = Lift $ do x <- x; return $ x >>= f
    Later x >>= f = Later $ \c -> x $ \x -> do
        x <- runWait $ f x
        case x of
            Now x -> c x
            Later x -> x c

instance (MonadIO m,  Applicative m) => MonadIO (Wait m) where
    liftIO = Lift . liftIO . fmap Now

#if __GLASGOW_HASKELL__ >= 800
instance MonadFail m => MonadFail (Wait m) where
    fail = Lift . Control.Monad.Fail.fail
#endif


firstJustWaitUnordered :: MonadIO m => (a -> Wait m (Maybe b)) -> [a] -> Wait m (Maybe b)
firstJustWaitUnordered f = go [] . map f
    where
        -- keep a list of those things we might visit later, and ask for each we see in turn
        go :: MonadIO m => [(Maybe a -> m ()) -> m ()] -> [Wait m (Maybe a)] -> Wait m (Maybe a)
        go later (x:xs) = case x of
            Now (Just a) -> Now $ Just a
            Now Nothing -> go later xs
            Later l -> go (l:later) xs
            Lift x -> Lift $ do
                x <- x
                return $ go later (x:xs)
        go [] [] = Now Nothing
        go [l] [] = Later l
        go ls [] = Later $ \callback -> do
            ref <- liftIO $ newIORef $ length ls
            forM_ ls $ \l -> l $ \r -> do
                old <- liftIO $ readIORef ref
                when (old > 0) $ case r of
                    Just a -> do
                        liftIO $ writeIORef' ref 0
                        callback $ Just a
                    Nothing -> do
                        liftIO $ writeIORef' ref $ old-1
                        when (old == 1) $ callback Nothing


firstLeftWaitUnordered :: (Applicative m, MonadIO m) => (a -> Wait m (Either e b)) -> [a] -> Wait m (Either e [b])
firstLeftWaitUnordered f xs = do
        let n = length xs
        mut <- liftIO $ newArray (length xs) undefined
        res <- go mut [] $ zipFrom 0 $ map f xs
        case res of
            Just e -> return $ Left e
            Nothing -> liftIO $ Right <$> mapM (readArray mut) [0..n-1]
    where
        -- keep a list of those things we might visit later, and ask for each we see in turn
        go :: (Applicative m, MonadIO m) => MutableArray RealWorld b -> [(Int, (Either e b -> m ()) -> m ())] -> [(Int, Wait m (Either e b))] -> Wait m (Maybe e)
        go mut later ((i,x):xs) = case x of
            Now (Left e) -> Now $ Just e
            Now (Right b) -> do
                liftIO $ writeArray mut i b
                go mut later xs
            Later l -> go mut ((i,l):later) xs
            Lift x -> Lift $ do
                x <- x
                return $ go mut later ((i,x):xs)
        go mut [] [] = Now Nothing
        go mut ls [] = Later $ \callback -> do
            ref <- liftIO $ newIORef $ length ls
            forM_ ls $ \(i,l) -> l $ \r -> do
                old <- liftIO $ readIORef ref
                when (old > 0) $ case r of
                    Left a -> do
                        liftIO $ writeIORef' ref 0
                        callback $ Just a
                    Right v -> do
                        liftIO $ writeArray mut i v
                        liftIO $ writeIORef' ref $ old-1
                        when (old == 1) $ callback Nothing
