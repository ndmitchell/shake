
module Test.Throttle(main) where

import Development.Shake
import Development.Shake.FilePath
import Test.Type
import Control.Exception.Extra hiding (assert)
import Control.Monad.Extra
import System.Time.Extra


main = shaken test $ \args obj -> do
    res <- newThrottle "test" 2 0.4
    want $ map obj ["file1.1","file2.1","file3.2","file4.1","file5.2"]
    obj "*.*" %> \out -> do
        withResource res (read $ drop 1 $ takeExtension out) $
            when (takeBaseName out == "file3") $ liftIO $ sleep 0.2
        writeFile' out ""

test build obj =
    forM_ [["-j1"],["-j8"]] $ \flags ->
        -- we are sometimes over the window if the machine is "a bit loaded" at some particular time
        -- therefore we rerun the test three times, and only fail if it fails on all of them
        retry 3 $ do
            build ["clean"]
            (s, _) <- duration $ build $ flags ++ ["--no-report"]
            -- the 0.1s cap is a guess at an upper bound for how long everything else should take
            -- and should be raised on slower machines
            assert (s >= 1.4 && s < 1.6) $
                "Bad throttling, expected to take 1.4s + computation time (cap of 0.2s), took " ++ show s ++ "s"
