{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving, CPP #-}

module Development.Shake.Internal.Core.Wait3(
    Locked, Wait(..), firstJustWaitOrdered, firstJustWaitUnordered, fromLater, fmapWait, runLocked
    ) where

import Control.Monad
import Control.Applicative
import Control.Concurrent.Extra
import Data.IORef.Extra
import Control.Monad.IO.Class
import Prelude

#if __GLASGOW_HASKELL__ >= 800
import Control.Monad.Fail
#endif


newtype Locked a = Locked (IO a)
    deriving (Functor, Applicative, Monad, MonadIO
#if __GLASGOW_HASKELL__ >= 800
             ,MonadFail
#endif
        )


runLocked :: Var a -> (a -> Locked b) -> IO b
runLocked var act = withVar var $ \v -> case act v of Locked x -> x

-- all operations on the Database must be run while Locked

data Wait a = Now a
            | Later ((a -> Locked ()) -> Locked ())
              deriving Functor

fromLater :: Wait a -> ((a -> Locked ()) -> Locked ())
fromLater (Now x) = \f -> f x
fromLater (Later x) = x


-- | Find the first Nothing in any position, waiting on all if you have to
firstJustWaitUnordered :: [Locked (Wait (Maybe a))] -> Locked (Wait (Maybe a))
firstJustWaitUnordered = go []
    where
        -- keep a list of those things we might visit later, and ask for each we see in turn
        go later (x:xs) = do
            x <- x
            case x of
                Now (Just a) -> return $ Now $ Just a
                Now Nothing -> go later xs
                Later l -> go (l:later) xs
        go [] [] = return $ Now Nothing
        go [l] [] = return $ Later l
        go ls [] = return $ Later $ \callback -> do
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


-- | Find the first Nothing in order, waiting one at a time whenever it is necessary
firstJustWaitOrdered :: [Locked (Wait (Maybe a))] -> Locked (Wait (Maybe a))
firstJustWaitOrdered [] = return $ Now Nothing
firstJustWaitOrdered (x:xs) = do
    x <- x
    case x of
        Now Nothing -> firstJustWaitOrdered xs
        Now (Just a) -> return $ Now $ Just a
        Later x -> return $ Later $ \k -> x $ \x -> case x of
            Nothing -> do res <- firstJustWaitOrdered xs; fromLater res k
            Just a -> k $ Just a


fmapWait :: (a -> Locked b) -> Locked (Wait a) -> Locked (Wait b)
fmapWait f x = do
    x <- x
    case x of
        Now x -> Now <$> f x
        Later x -> return $ Later $ \c -> x $ c <=< f
