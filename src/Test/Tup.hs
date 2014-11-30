{-# LANGUAGE PatternGuards #-}

module Test.Tup(main) where

import Development.Shake
import Development.Shake.Config
import Development.Shake.FilePath
import Test.Type


main = shaken noTest $ \args obj -> do
    usingConfigFile "src/Test/Tup/root.cfg"

    action $ do
        keys <- getConfigKeys
        need $ take 0 [x -<.> exe | x <- keys, takeExtension x == ".exe"]

    (\x -> x -<.> exe == x) ?> \out -> do
        Just x <- getConfig (takeBaseName out <.> "exe")
        error $ show x

{-

Tuprules.tup
CFLAGS += -Wall
CFLAGS += -O2

!cc = |> gcc $(CFLAGS) -c %f -o %o |> %B.o
!ar = |> ar crs %o %f |>
Tupfile
include_rules
CFLAGS += -Inewmath
: foreach *.c |> !cc |>
: *.o newmath/libnewmath.a |> gcc %f -o %o |> hello
newmath/Tupfile
include_rules
: foreach *.c |> !cc |>
: *.o |> !ar |> libnewmath.a
-}
