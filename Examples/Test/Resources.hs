
module Examples.Test.Resources(main) where

import Development.Shake
import Examples.Util
import Control.Monad
import Data.IORef


main extra = do
    let cap = 2

    ref <- newIORef 0
    flip (shaken test) extra $ \args obj -> do
        want $ map obj ["file1.txt","file2.txt","file3.txt","file4.txt"]
        res <- newResource "test" cap
        obj "*.txt" *> \out ->
            withResource res 1 $ do
                old <- liftIO $ atomicModifyIORef ref $ \i -> (i+1,i)
                when (old >= cap) $ error "Too many resources in use at one time"
                liftIO $ sleep 0.1
                liftIO $ atomicModifyIORef ref $ \i -> (i-1,i)
                writeFile' out ""

test build obj = do
    build ["clean"]
    build ["-j2"]
    build ["clean"]
    build ["-j4"]
    build ["clean"]
    build ["-j10"]
