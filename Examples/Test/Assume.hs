
module Examples.Test.Assume(main) where

import Development.Shake
import Examples.Util
import Control.Monad
import Development.Shake.FilePath


main = shaken test $ \args obj -> do
    want $ map obj args
    obj "*.out" *> \out -> do
        cs <- mapM (readFile' . obj . (:".src")) $ takeBaseName out
        writeFile' out $ concat cs


test build obj = do
    let set file c = writeFile (obj $ file : ".src") [c]
    let ask file c = do src <- readFile (obj $ file ++ ".out"); src === c

    forM_ ['a'..'f'] $ \c -> set c c
    build ["abc.out"]
    ask "abc" "abc"

    set 'b' 'd'
    sleepFileTime
    build ["abc.out"]
    ask "abc" "adc"
    set 'b' 'p'
    sleepFileTime
    build ["abc.out","--assume-clean"]
    build ["abc.out"]
    ask "abc" "adc"
    set 'c' 'z'
    sleepFileTime
    build ["abc.out"]
    ask "abc" "apz"
