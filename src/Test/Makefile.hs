
module Test.Makefile(main) where

import Development.Shake(action, liftIO)
import qualified Run as Makefile
import System.Environment
import Test.Type
import Control.Monad
import Data.List
import Data.Maybe


main = shaken test $ \args obj ->
    action $ liftIO $ do
        unless (["@@"] `isPrefixOf` args) $
            error "The 'makefile' example should only be used in test mode, to test using a makefile use the 'make' example."
        withArgs [fromMaybe x $ stripPrefix "@" x | x <- drop 1 args] Makefile.main


test build obj = do
    copyDirectoryChanged "src/Test/MakeTutor" $ obj "MakeTutor"
    build ["@@","--directory=" ++ obj "MakeTutor","--no-report"]
    build ["@@","--directory=" ++ obj "MakeTutor","--no-report"]
    build ["@@","--directory=" ++ obj "MakeTutor","@clean","--no-report"]
