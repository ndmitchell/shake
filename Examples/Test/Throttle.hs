
module Examples.Test.Throttle(main) where

import Development.Shake
import General.Base
import Development.Shake.FilePath
import Examples.Util
import Control.Monad


main = shaken test $ \args obj -> do
    res <- newThrottle "test" 2 0.2
    want $ map obj ["file1.1","file2.1","file3.2","file4.1","file5.2"]
    obj "*.*" *> \out -> do
        withResource res (read $ drop 1 $ takeExtension out) $ return ()
        writeFile' out ""

test build obj = do
    forM_ [[],["-j8"]] $ \flags -> do
        -- we are sometimes over the window if the machine is "a bit loaded" at some particular time
        -- therefore we rerun the test three times, and only fail if it fails on all of them
        flip loop 3 $ \i -> do
            build ["clean"]
            (s, _) <- duration $ build []
            -- the 0.1s cap is a guess at an upper bound for how long everything else should take
            -- and should be raised on slower machines
            let good = s >= 0.6 && s < 0.7
            if good then return $ Right ()
             else if i > 1 then do
                putStrLn $ "Throttle failed (took " ++ show s ++ "s), retrying"
                return $ Left (i-1)
             else do
                assert False $ "Bad throttling, expected to take 0.6s + computation time (cap of 0.1s), took " ++ show s ++ "s (three times in a row)"
                return $ Right ()
