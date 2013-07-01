
module Examples.Ninja.Main(main) where

import Development.Shake
import Development.Shake.FilePath
import System.Directory(copyFile)
import Examples.Util
import Data.List
import qualified Start
import System.Environment


main = shaken test $ \args obj -> do
    let args2 = ("-C" ++ obj "") : map tail (filter ("@" `isPrefixOf`) args)
    let real = "real" `elem` args
    action $
        if real then cmd "ninja" args2 else liftIO $ withArgs args2 Start.main


test build obj = do
    let run xs = build $ map ('@':) $ words xs
    build ["clean"]
    run "-f../../Examples/Ninja/test1.ninja"
    assertExists $ obj "out1.txt"

    run "-f../../Examples/Ninja/test2.ninja"
    assertExists $ obj "out2.2"
    assertMissing $ obj "out2.1"
    build ["clean"]
    run "-f../../Examples/Ninja/test2.ninja out2.1"
    assertExists $ obj "out2.1"
    assertMissing $ obj "out2.2"

    copyFile "Examples/Ninja/test3-sub.ninja" $ obj "test3-sub.ninja"
    copyFile "Examples/Ninja/test3-inc.ninja" $ obj "test3-inc.ninja"
    copyFile ("Examples/Ninja/" ++ if null exe then "test3-unix.ninja" else "test3-win.ninja") $ obj "test3-platform.ninja"
    run "-f../../Examples/Ninja/test3.ninja"
    assertNonSpace (obj "out3.1") "g4+b1+++i1"
    assertNonSpace (obj "out3.2") "g4++++i1"
    assertNonSpace (obj "out3.3") "g4++++i1"
    assertNonSpace (obj "out3.4") "g4+++s1+s2"

    run "-f../../Examples/Ninja/test4.ninja out"
    assertExists $ obj "out.txt"
    assertExists $ obj "out2.txt"

    run "-f../../Examples/Ninja/test5.ninja"
    assertExists $ obj "output file"
