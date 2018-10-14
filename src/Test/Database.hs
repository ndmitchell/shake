
module Test.Database(main) where

import Control.Concurrent.Extra
import Control.Exception.Extra
import Control.Monad
import Data.List
import Development.Shake
import Development.Shake.Database
import Development.Shake.FilePath
import System.Time.Extra
import System.Directory as IO
import Test.Type
import Data.Functor
import Prelude


rules = do
    "*.out" %> \out -> do
        liftIO $ appendFile "log.txt" "x"
        copyFile' (out -<.> "in") out
        removeFilesAfter "." ["log.txt"]

    "*.err" %> \out -> fail out

    phony "sleep" $ liftIO $ sleep 20

main = testSimple $ do
    let opts = shakeOptions{shakeFiles="/dev/null"}
    writeFile "a.in" "a"
    writeFile "b.in" "b"
    sleepFileTime
    writeFile "log.txt" ""
    (open, close) <- shakeOpenDatabase opts rules
    db <- open

    ([12], after) <- shakeRunDatabase db [need ["a.out"] >> return 12]
    assertContents "log.txt" "x"

    writeFile "a.in" "A"
    shakeRunDatabase db [need ["a.out","b.out"]]
    assertContents "a.out" "A"
    assertContents "log.txt" "xxx"

    ([13,14], _) <- shakeRunDatabase db [need ["a.out"] >> return 13, return 14]
    assertContents "log.txt" "xxx"

    live <- shakeLiveFilesDatabase db
    sort live === ["a.in","a.out"]

    -- check that parallel runs blow up, and that we can throw async exceptions to kill it
    assertWithin 10 $ do
        threads <- newBarrier
        results <- replicateM 2 newBarrier
        ts <- forM results $ \result -> forkFinally (void $ shakeRunDatabase db [need ["sleep"]]) $ \r -> mask_ $ do
            print $ "Failed with " ++ show r
            signalBarrier result r
            threads <- waitBarrier threads
            me <- myThreadId
            forM_ threads $ \t -> when (t /= me) $ throwTo t $ ErrorCall "ab123c"
        signalBarrier threads ts
        results <- show <$> mapM waitBarrier results
        assertBool ("ab123c" `isInfixOf` results) "Contains ab123c"
        assertBool ("currently running" `isInfixOf` results) "Contains 'currently using'"

    close
    assertException ["already closed"] $ void $ shakeRunDatabase db []

    shakeRunAfter opts after
    assertBoolIO (not <$> IO.doesFileExist "log.txt") "Log must be deleted"

    errs <- shakeWithDatabase opts{shakeStaunch=True, shakeVerbosity=Silent} rules $ \db -> do
        assertException ["Error when running"] $ void $ shakeRunDatabase db [need ["foo.err","bar.err"]]
        shakeErrorsDatabase db
    sort (map fst errs) === ["bar.err","foo.err"]
