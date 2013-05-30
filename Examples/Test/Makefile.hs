
module Examples.Test.Makefile(main) where

import Development.Shake(action, liftIO)
import Development.Shake.FilePath
import qualified Start as Makefile
import System.Environment
import System.Directory
import Examples.Util
import Control.Monad
import Data.List
import Data.Maybe


main = shaken test $ \args obj ->
    action $ liftIO $ do
        withArgs [fromMaybe x $ stripPrefix "@" x | x <- args] Makefile.main


test build obj = do
    let copyTo from to = do
            xs <- getDirectoryContents from
            createDirectoryIfMissing True (obj to)
            forM_ xs $ \x ->
                when (not $ all (== '.') x) $
                    copyFile (from </> x) (obj to </> x)

    copyTo "Examples/MakeTutor" "MakeTutor"
    build ["--directory=" ++ obj "MakeTutor","--no-report"]
    build ["--directory=" ++ obj "MakeTutor","--no-report"]
    build ["--directory=" ++ obj "MakeTutor","@clean","--no-report"]
