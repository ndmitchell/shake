module Test.Targets(main) where

import Development.Shake
import Development.Shake.Internal.Core.Rules (getHelpSuffix)
import Test.Type
import System.Environment
import qualified System.IO.Extra as IO

main :: IO () -> IO ()
main _sleeper = do
    targets <- getTargets shakeOptions rules
    targets === expected
    helpSuffix <- getHelpSuffix shakeOptions rules
    helpSuffix === ["Don't Panic", "Know where your towel is"]

    let listTargets args = lines . fst <$> IO.captureOutput (withArgs args $ shakeArgs shakeOptions rules)
    xs <- listTargets ["--list-targets"]
    xs === [a ++ maybe "" (" - " ++) b | (a,b) <- expected]
    xs <- listTargets ["--list-targets=file1*", "--quiet"]
    xs === ["file1", "file10", "file11 - a great file", "file12 - awesome files", "file13 - awesome files"]
    xs <- listTargets ["--list-targets=phony*", "--list-targets=Foo"]
    xs === ["phony1 - A phony target", "phony2 - Builds something really good", "Foo - bad docs"]

rules :: Rules ()
rules = do
    withTargetDocs "A phony target" $ phony "phony1" $ pure ()

    "file1" %> \_ -> pure ()
    ["file2", "file3"] |%> \_ -> pure ()
    ["file4", "file5"] &%> \_ -> pure ()

    "file6" %> \_ -> pure ()
    ["file7", "file8"] |%> \_ -> pure ()
    ["file9", "file10"] &%> \_ -> pure ()

    withTargetDocs "Builds something really good" $ phony "phony2" $ pure ()
    withTargetDocs "bad docs" $ do
        withTargetDocs "a great file" $ "file11" %> \_ -> pure ()
        withTargetDocs "awesome files" $ ["file12", "file13"] &%> \_ -> pure ()
        phony "Foo" $ pure ()
        withoutTargets $ phony "Bar" $ pure ()

    addHelpSuffix "Don't Panic"
    addHelpSuffix "Know where your towel is"


expected :: [(String, Maybe String)]
expected =
    [ "phony1" * Just "A phony target"

    , "file1" *  Nothing
    , "file2" *  Nothing
    , "file3" *  Nothing
    , "file4" *  Nothing
    , "file5" *  Nothing

    , "file6" *  Nothing
    , "file7" *  Nothing
    , "file8" *  Nothing
    , "file9" *  Nothing
    , "file10" * Nothing

    , "phony2" * Just "Builds something really good"

    , "file11" * Just "a great file"
    , "file12" * Just "awesome files"
    , "file13" * Just "awesome files"
    , "Foo" * Just "bad docs"
    ]
    where (*) = (,)
