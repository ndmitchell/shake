
module Test.Rebuild(main) where

import Development.Shake
import Test.Type
import Control.Monad
import Development.Shake.FilePath


main = shakeTest_ test $
    "*.out" %> \out -> do
        cs <- mapM (readFile' . (:".src")) $ takeBaseName out
        writeFile' out $ concat cs


test build = do
    let set file c = writeFile (file : ".src") [c]
    let ask file c = do src <- readFile (file ++ ".out"); src === c

    forM_ ['a'..'f'] $ \c -> set c c
    build ["--sleep","abc.out"]
    ask "abc" "abc"

    set 'b' 'd'
    build ["--sleep","abc.out"]
    ask "abc" "adc"
    set 'b' 'p'
    build ["--sleep","abc.out","--skip-forever"]
    build ["abc.out"]
    ask "abc" "adc"
    set 'c' 'z'
    build ["--sleep","abc.out"]
    ask "abc" "apz"

    build ["bc.out","c.out"]
    ask "bc" "pz"
    set 'b' 'r'
    set 'c' 'n'
    build ["--sleep","abc.out","--skip-forever"]
    ask "abc" "apz"
    build ["ab.out","--rebuild"]
    ask "ab" "ar"
    build ["c.out"]
    ask "c" "z"
