
module Test.Batch(main) where

import Development.Shake
import Development.Shake.FilePath
import System.Directory
import General.Extra
import Test.Type
import Control.Monad


main = shakeTest_ test $ do
    let inp x = x -<.> "in"
    file <- newResource "log.txt" 1
    batch 3 ("*.out" %>) (\out -> do need [inp out]; return out) $ \outs -> do
        liftIO $ assertBool (length outs <= 3) "length outs <= 3"
        withResource file 1 $ liftIO $ appendFile "log.txt" $ show (length outs) ++ "\n"
        putNormal $ "Building batch: " ++ unwords outs
        forM_ outs $ \out -> liftIO $ copyFile (inp out) out
    want [show i <.> "out" | i <- [1..6]]

    "ABn.txt" %> \out -> do
        xs <- needHasChanged ["An.txt", "Bn.txt"]
        writeFileLines out xs

    ["An", "Bn"] &?%> \outs -> do
        xs <- needHasChanged $ map (-<.> "in") outs
        os <- mapM resultHasChanged outs
        forM_ (zip outs os) $ \(out, o) ->
            when (o || (out -<.> "in" `elem` xs)) $
                writeFile' out "1"

    "On" %> \out -> do
        xs <- needHasChanged ["An", "Bn"]
        o <- resultHasChanged out
        writeFileLines out $ xs ++ ["On" | o]


test build = do
    forM_ [1..6] $ \i -> writeFile (show i <.> "in") $ show i
    build ["--sleep","-j2"]
    assertBoolIO (do src <- readFile "log.txt"; return $ length (lines src) < 6) "some batching"
    writeFile "log.txt" ""
    writeFile "2.in" "22"
    writeFile "5.in" "55"
    build []
    assertContents "log.txt" "2\n"

    writeFile "An.txt" "1"
    writeFile "Bn.txt" "1"
    build ["ABn.txt", "--sleep"]
    assertContents "ABn.txt" "An.txt\nBn.txt\n"
    writeFile "An.txt" "1"
    build ["ABn.txt", "--sleep"]
    assertContents "ABn.txt" "An.txt\n"
    writeFile "Bn.txt" "1"
    build ["ABn.txt", "--sleep"]
    assertContents "ABn.txt" "Bn.txt\n"
    build ["ABn.txt", "--sleep"]
    assertContents "ABn.txt" "Bn.txt\n"
    writeFile "ABn.txt" "bogus"
    build ["ABn.txt", "--sleep"]
    assertContents "ABn.txt" ""

    forM_ [[],["--usepredicate"]] $ \args -> do
        writeFile "An.in" "1"
        writeFile "Bn.in" "1"
        removeFile_ "On"
        build $ ["On", "--sleep"] ++ args
        assertContents "On" "An\nBn\nOn\n"
        writeFile "An.in" "1"
        build $ ["On", "--sleep"] ++ args
        assertContents "On" "An\n"
        writeFile "Bn.in" "1"
        build $ ["On", "--sleep"] ++ args
        assertContents "On" "Bn\n"
        build $ ["On", "--sleep"] ++ args
        assertContents "On" "Bn\n"
        removeFile "An"
        build $ ["On", "--sleep"] ++ args
        assertContents "On" "An\n"
        removeFile "An"
        writeFile "Bn.in" "2"
        build $ ["On", "--sleep"] ++ args
        assertContents "On" "An\nBn\n"
        removeFile "On"
        build $ ["On", "--sleep"] ++ args
        assertContents "On" "On\n"
