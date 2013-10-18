# Shake Manual

Shake is a Haskell library for writing build systems - designed as a replacement for `make`. This document describes how to get started with Shake, assuming no prior Haskell knowledge. First, let's take a look at a Shake build system:

    import Deveopment.Shake
    import Deveopment.Shake.FilePath
    import Deveopment.Shake.Util
    
    main :: IO ()
    main = shakeArgs shakeOptions $ do
        want ["_make/run" <.> exe]
        
        phony "clean" $ removeFilesAfter "_make" ["//*"]
        
        "_make/run" <.> exe *> \out -> do
            cs <- getDirectoryContents "//*.c"
            let os = ["_make" </> c -<.> "o" | c <- cs]
            need os
            cmd_ "gcc -o" [exe] os

        "_make//*.o" *> \o -> do
            let c = dropDirectory1 $ out -<.> "c"
            let m = out -<.> "m"
            cmd_ "gcc -c" [c] "-o" [out] "-MM" [m]
            need $ parseMakeFile m

This build system is complete and follows best practices. It builds an executable `_make/run` from all `.c` files in the current directory. It tracks both the list of `.c` files, the contents of the `.c` files and any header files they import. If any of the above change then it will rebuild. All generated files are placed in `_make`, and a `clean` target is provided that will wipe all the generated files. In the result of this manual we'll explain how the above code works, then extend the example with more features. 

#### Running this example

1. Install the [Haskell Platform](http://www.haskell.org/platform/), which provides a Haskell compiler and standard libraries.
2. Type `cabal update`, to download information about the latest versions of all Haskell packages.
3. Type `cabal install shake --global --profile`, to build and install Shake and all its dependencies.
4. Save the example as `Main.hs` in a directory containing some `.c` files.
5. Run `runhaskell Main.hs` to confirm.

## The basic syntax

Treat this as boilerplate:

    import Deveopment.Shake
    import Deveopment.Shake.FilePath
    import Deveopment.Shake.Util
    
    main :: IO ()
    main = shakeArgs shakeOptions $ do
        ...

Where all your code goes under `...`.

#### Defining targets

`want` says what you want to obtain at the end. We use a list with the `[a,b,c]`  and strings which are `"_make/Main.exe"`. Generally, even on Windows, just use `/` characters for path separators.

#### Defining rules

    pattern *> \out -> do
        ...

`*>` is used with the file path on the left. The thing may be a pattern which means you can define 

#### Expressing dependencies

* `need`
* `getDirectoryContents`

#### Filepath manipulation functions

`</>` and `<.>`

#### Running external commands

`cmd`

#### A clean command

`phony` and `removeFilesAfter`

## Running 

#### Compiling the build system

#### Command line flags


## Prediction and Progress

Use `--assume-dirty`


## Profiling


## Extending with Haskell

 