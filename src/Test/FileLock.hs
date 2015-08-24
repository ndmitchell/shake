
module Test.FileLock(main) where

import Development.Shake
import Control.Concurrent.Extra
import Control.Exception.Extra
import Control.Monad
import Data.Either.Extra
import Test.Type


main = shaken test $ \args obj -> do
    action $ liftIO $ sleep 2


test build obj = do
    -- check it fails exactly once
    a <- onceFork $ build []
    b <- onceFork $ build []
    a <- try_ a
    b <- try_ b
    when (length (filter isLeft [a,b]) /= 1) $
        error $ "Expected one success and one failure, got " ++ show [a,b]
    -- check it succeeds after the lock has been held
    build []
