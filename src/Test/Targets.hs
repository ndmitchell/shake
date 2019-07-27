module Test.Targets(main) where

import Development.Shake
import Development.Shake.Internal.Core.Rules (getHelpSuffix)
import Test.Type

main :: IO () -> IO ()
main _sleeper = do
    targets <- getTargets shakeOptions rules
    targets === expected
    helpSuffix <- getHelpSuffix shakeOptions rules
    helpSuffix === ["Don't Panic", "Know where your towel is"]

rules :: Rules ()
rules = do
    withTargetDocs "A phony target" $ phony "phony1" $ return ()

    "file1" %> \_ -> return ()
    ["file2", "file3"] |%> \_ -> return ()
    ["file4", "file5"] &%> \_ -> return ()

    "file6" %> \_ -> return ()
    ["file7", "file8"] |%> \_ -> return ()
    ["file9", "file10"] &%> \_ -> return ()

    withTargetDocs "Builds something really good" $ phony "phony2" $ return ()
    withTargetDocs "bad docs" $ do
        withTargetDocs "a great file" $ "file11" %> \_ -> return ()
        withTargetDocs "awesome files" $ ["file12", "file13"] &%> \_ -> return ()
        phony "Foo" $ return ()
        withoutTargets $ phony "Bar" $ return ()

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
