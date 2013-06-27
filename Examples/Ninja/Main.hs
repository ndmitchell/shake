
module Examples.Ninja.Main(main) where

import Development.Shake
import Examples.Util
import qualified Start
import System.Environment

main = shaken test $ \args obj ->
    action $ liftIO $ withArgs (("-C" ++ obj "") : map (drop 1) args) Start.main


test build obj = do
    build ["clean"]
    let run xs = build $ map ('@':) $ words xs
    run "-f../../Examples/Ninja/test1.ninja"
    assertExists $ obj "out1.txt"
    run "-f../../Examples/Ninja/test2.ninja"
    assertExists $ obj "out2.2"
    assertMissing $ obj "out2.1"
    build ["clean"]
    run "-f../../Examples/Ninja/test2.ninja out2.1"
    assertExists $ obj "out2.1"
    assertMissing $ obj "out2.2"

