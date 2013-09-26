# Shake Manual

This document describes how to get started with Shake, and does not assume any prior Haskell knowledge. Let's take a look at a Shake build system.

    module Main(main) where
    
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

#### The basic syntax


#### Filepath manipulation functions

#### Running external commands

#### A clean command


#### Using command line flags


#### 

