{-# LANGUAGE DeriveFunctor #-}

module Development.Shake.Internal.Core.Wait3(
    Wait(..), firstJustWaitOrdered, firstJustWaitUnordered, fromLater, fmapWait
    ) where

import Control.Monad
import Data.IORef.Extra


data Wait a = Now a
            | Later ((a -> IO ()) -> IO ())
              deriving Functor

fromLater :: Wait a -> ((a -> IO ()) -> IO ())
fromLater (Now x) = \f -> f x
fromLater (Later x) = x


-- | Find the first Nothing in any position, waiting on all if you have to
firstJustWaitUnordered :: [IO (Wait (Maybe a))] -> IO (Wait (Maybe a))
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
            ref <- newIORef $ length ls
            forM_ ls $ \l -> l $ \r -> do
                old <- readIORef ref
                when (old > 0) $ case r of
                    Just a -> do
                        writeIORef' ref 0
                        callback $ Just a
                    Nothing -> do
                        writeIORef' ref $ old-1
                        when (old == 1) $ callback Nothing


-- | Find the first Nothing in order, waiting one at a time whenever it is necessary
firstJustWaitOrdered :: [IO (Wait (Maybe a))] -> IO (Wait (Maybe a))
firstJustWaitOrdered [] = return $ Now Nothing
firstJustWaitOrdered (x:xs) = do
    x <- x
    case x of
        Now Nothing -> firstJustWaitOrdered xs
        Now (Just a) -> return $ Now $ Just a
        Later x -> return $ Later $ \k -> x $ \x -> case x of
            Nothing -> do res <- firstJustWaitOrdered xs; fromLater res k
            Just a -> k $ Just a


fmapWait :: (a -> IO b) -> IO (Wait a) -> IO (Wait b)
fmapWait f x = do
    x <- x
    case x of
        Now x -> Now <$> f x
        Later x -> return $ Later $ \c -> x $ c <=< f
