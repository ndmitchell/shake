
module Examples.Test.Util(main) where

import Development.Shake.Util
import Examples.Util


main = shaken test $ \args obj -> return ()


test build obj = do
    parseMakefile "" === []
    parseMakefile "a:b c\ndef : ee" === [("a",["b","c"]),("def",["ee"])]
    parseMakefile "a: #comment\n#comment : b\nc : d" === [("a",[]),("c",["d"])]
    parseMakefile "a \\\n\t:b" === [("a",["b"])]
    parseMakefile "#comment\\    a : b" === []
    parseMakefile "a: b c \\\n    d e\n\nf:g" === [("a",["b","c","d","e"]),("f",["g"])]
