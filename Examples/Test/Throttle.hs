
module Examples.Test.Throttle(main) where

import Development.Shake
import Development.Shake.Util
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
        build ["clean"]
        (s, _) <- duration $ build []
        assert (s >= 0.6 && s < 0.7) $
            -- the 0.1s cap is a guess at an upper bound for how long everything else should take
            -- and should be raised on slower machines
            "Bad throttling, expected to take 0.6s + computation time (cap of 0.1s), took " ++ show s ++ "s"
