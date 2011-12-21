
module Examples.Test.Basic1(main) where

import Development.Shake
import Examples.Util

main = shaken $ \obj -> do
    want [obj "AB.txt"]
    obj "AB.txt" *> \out -> do
        need [obj "A.txt", obj "B.txt"]
        text1 <- readFile' $ obj "A.txt"
        text2 <- readFile' $ obj "B.txt"
        writeFile' out $ text1 ++ text2
