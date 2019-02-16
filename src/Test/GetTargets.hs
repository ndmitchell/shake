module Test.GetTargets(main) where

import Development.Shake
import Test.Type

main :: IO () -> IO ()
main _sleeper = do
    targets <- getTargets shakeOptions rules
    targets === expected

rules :: Rules ()
rules = do
  phony "phony1" $ return ()

  "file1" %> \_ -> return ()
  ["file2", "file3"] |%> \_ -> return ()
  ["file4", "file5"] &%> \_ -> return ()

  build "file6" $ \_ -> return ()
  buildAny ["file7", "file8"] $ \_ -> return ()
  buildAll ["file9", "file10"] $ \_ -> return ()

  phonyWithDocs
    "Builds something really good"
    "phony2" $ return ()

  buildWithDocs
    "a great file"
    "file11" $ \_ -> return ()

  buildAnyWithDocs
    "awesome files"
    ["file12", "file13"] $ \_ -> return ()

  buildAllWithDocs
    "just briliant"
    ["file14", "file15"] $ \_ -> return ()

expected :: [Target]
expected =
    [ Target "phony1" (Just "A phony target")

    , Target "file1"  Nothing
    , Target "file2"  Nothing
    , Target "file3"  Nothing
    , Target "file4"  Nothing
    , Target "file5"  Nothing

    , Target "file6"  Nothing
    , Target "file7"  Nothing
    , Target "file8"  Nothing
    , Target "file9"  Nothing
    , Target "file10" Nothing

    , Target "phony2" (Just "Builds something really good")

    , Target "file11" (Just "a great file")
    , Target "file12" (Just "awesome files")
    , Target "file13" (Just "awesome files")
    , Target "file14" (Just "just briliant")
    , Target "file15" (Just "just briliant")

    ]
