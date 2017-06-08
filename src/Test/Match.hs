
-- | Test the rule matching facilities - alternatives, priority etc.
module Test.Match(main) where

import Development.Shake
import Test.Type


main = shakeTest_ test $ do
    let obj = id
    let output x file = writeFile' file x

    ["or*","*or"] |%> output ""

    alternatives $ do
        obj "alternative.t*" %> output "alternative.t*"
        obj "alternative.*" %> output "alternative.*"

    priority 100 $ priority 0 $ obj "priority.txt" %> output "100"
    priority 50 $ obj "priority.txt" %> output "50"

    alternatives $ do
        priority 20 $ obj "altpri.txt" %> output "20"
        priority 40 $ obj "altpri.txt" %> output "40"
    priority 30 $ obj "altpri.txt" %> output "30"

    alternatives $ do
        priority 21 $ obj "altpri2.txt" %> output "21"
        priority 22 $ obj "altpri2.txt" %> output "22"
    priority 23 $ obj "altpri2.txt" %> output "23"

    priority 55 $ alternatives $ obj "x" %> output "55"
    priority 51 $ obj "x" %> output "51"

    priority 42 $ alternatives $ obj "xx" %> output "42"
    priority 43 $ obj "xx" %> output "43"

    priority 10 $ do
        priority 7 $ obj "change" %> output "7"
        priority 8 $ obj "change" %> output "8"
    priority 9 $ obj "change" %> output "9"


test build = do
    let obj = id
    build ["clean"]
    build ["or"]

    build ["alternative.foo","alternative.txt"]
    assertContents (obj "alternative.foo") "alternative.*"
    assertContents (obj "alternative.txt") "alternative.t*"

    build ["priority.txt"]
    assertContents (obj "priority.txt") "100"

    build ["altpri.txt","altpri2.txt"]
    assertContents (obj "altpri.txt") "20"
    assertContents (obj "altpri2.txt") "23"

    build ["x","xx"]
    assertContents (obj "x") "55"
    assertContents (obj "xx") "43"

    assertException ["matches multiple rules"] $ build ["change","--quiet"]
