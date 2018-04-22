{-# LANGUAGE RecordWildCards #-}

module Development.Shake.Internal.Core.Wait2(
    addPoolWait, actionFence,
    ) where

import Control.Exception
import Development.Shake.Internal.Core.Pool
import Development.Shake.Internal.Core.Types
import Development.Shake.Internal.Core.Monad
import Control.Monad.IO.Class
import General.Concurrent


addPoolWait :: PoolPriority -> Action a -> Action (Fence (Either SomeException a))
addPoolWait pri act = do
    ro@Global{..} <- Action getRO
    rw <- Action getRW
    liftIO $ do
        fence <- newFence
        addPool pri globalPool $ runAction ro rw act $ signalFence fence
        return fence


actionFence :: Fence (Either SomeException a) -> Action a
actionFence fence = Action $ captureRAW $ \continue ->
    waitFence fence continue
