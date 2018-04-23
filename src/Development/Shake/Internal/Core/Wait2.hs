{-# LANGUAGE RecordWildCards #-}

module Development.Shake.Internal.Core.Wait2(
    addPoolWait, actionFence, actionFenceRequeue, actionRequeue
    ) where

import Control.Exception
import Development.Shake.Internal.Core.Pool
import Development.Shake.Internal.Core.Types
import Development.Shake.Internal.Core.Monad
import System.Time.Extra
import Control.Monad.IO.Class
import General.Concurrent
import Data.Functor
import Prelude


addPoolWait :: PoolPriority -> Action a -> Action (Fence (Either SomeException a))
addPoolWait pri act = do
    ro@Global{..} <- Action getRO
    rw <- Action getRW
    liftIO $ do
        fence <- newFence
        addPool pri globalPool $ runAction ro rw act $ signalFence fence
        return fence


actionFence :: Fence (Either SomeException a) -> Action a
actionFence fence = do
    res <- liftIO $ testFence fence
    case res of
        Just (Left e) -> Action $ throwRAW e
        Just (Right v) -> return v
        Nothing -> Action $ captureRAW $ waitFence fence


actionFenceRequeue :: PoolPriority -> Fence (Either SomeException a) -> Action (Seconds, a)
actionFenceRequeue pri fence = Action $ do
    res <- liftIO $ testFence fence
    case res of
        Just (Left e) -> throwRAW e
        Just (Right v) -> return (0, v)
        Nothing -> do
            Global{..} <- getRO
            offset <- liftIO offsetTime
            captureRAW $ \continue -> waitFence fence $ \v ->
                addPool pri globalPool $ do
                    offset <- offset
                    continue $ (,) offset <$> v


actionRequeue :: PoolPriority -> Either SomeException a -> Action a
actionRequeue pri res = Action $ do
    Global{..} <- getRO
    captureRAW $ \continue -> addPool pri globalPool $ continue res
