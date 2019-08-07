
-- | Test the rule matching facilities - alternatives, preference etc.
module Test.Match(main) where

import Development.Shake
import Test.Type


main = testBuild test $ do
    let output x file = writeFile' file x

    ["or*","*or"] |%> output ""

    alternatives $ do
        "alternative.t*" %> output "alternative.t*"
        "alternative.*" %> output "alternative.*"

    preference 100 $ preference 0 $ "preference.txt" %> output "100"
    preference 50 $ "preference.txt" %> output "50"

    alternatives $ do
        preference 20 $ "altpref.txt" %> output "20"
        preference 40 $ "altpref.txt" %> output "40"
    preference 30 $ "altpref.txt" %> output "30"

    alternatives $ do
        preference 21 $ "altpref2.txt" %> output "21"
        preference 22 $ "altpref2.txt" %> output "22"
    preference 23 $ "altpref2.txt" %> output "23"

    preference 55 $ alternatives $ "x" %> output "55"
    preference 51 $ "x" %> output "51"

    preference 42 $ alternatives $ "xx" %> output "42"
    preference 43 $ "xx" %> output "43"

    preference 10 $ do
        preference 6 $ "change" %> output "6"
        preference 7 $ "change" %> output "7"
        preference 8 $ "change" %> output "8"
    preference 9 $ "change" %> output "9"


test build = do
    build ["clean"]
    build ["or"]

    build ["alternative.foo","alternative.txt"]
    assertContents "alternative.foo" "alternative.*"
    assertContents "alternative.txt" "alternative.t*"

    build ["preference.txt"]
    assertContents "preference.txt" "100"

    build ["altpref.txt","altpref2.txt"]
    assertContents "altpref.txt" "30"
    assertContents "altpref2.txt" "23"

    build ["x","xx"]
    assertContents "x" "55"
    assertContents "xx" "43"

    assertException ["matches multiple rules","3"] $ build ["change","--quiet"]
