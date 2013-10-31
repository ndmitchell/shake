# Shake Manual

Shake is a Haskell library for writing build systems - designed as a replacement for `make`. This document describes how to get started with Shake, assuming no prior Haskell knowledge. First, let's take a look at a Shake build system:

    import Development.Shake
    import Development.Shake.Command
    import Development.Shake.FilePath
    import Development.Shake.Util
    
    main :: IO ()
    main = shakeArgs shakeOptions{shakeFiles="_build/"} $ do
        want ["_build/run" <.> exe]
    
        phony "clean" $ do
            removeFilesAfter "_build" ["//*"]
    
        "_build/run" <.> exe *> \out -> do
            cs <- getDirectoryFiles "" ["//*.c"]
            let os = ["_build" </> c -<.> "o" | c <- cs]
            need os
            cmd "gcc -o" [out] os
    
        "_build//*.o" *> \out -> do
            let c = dropDirectory1 $ out -<.> "c"
            let m = out -<.> "m"
            () <- cmd "gcc -c" [c] "-o" [out] "-MMD -MF" [m]
            needMakefileDependencies m

This build system builds the executable `_build/run` from all C source files in the current directory. It will rebuild if you add/remove any C files to the directory, if the C files themselves change, or if any headers used by the C files change. All generated files are placed in `_build`, and a `clean` command is provided that will wipe all the generated files. In the rest of this manual we'll explain how the above code works and how to extend it. 

#### Running this example

To run the example above:

1. Install the [Haskell Platform](http://www.haskell.org/platform/), which provides a Haskell compiler and standard libraries.
2. Type `cabal update`, to download information about the latest versions of all Haskell packages.
3. Type `cabal install shake --global --profile`, to build and install Shake and all its dependencies.
4. Grab a tarball of the example, which includes the code at the beginning as `Build.hs`, along with some small sample C files.
5. In the directory of the example, type `runhaskell Build.hs`, which should produce an executable `_build/run`.
6. Run `_build/run` to confirm everything worked.

## Basic syntax

This section explains enough syntax to write a basic Shake build script.

#### Boilerplate

The build system above starts with the following boilerplate:

<pre>
import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="_build/"} $ do
    <i>build rules</i>
</pre>

All the interesting build-specific code is placed under <tt><i>build rules</i></tt>. Many build systems will be able to reuse that boilerplate unmodified.

#### Defining targets

A target is a file we want the build system to produce (typically executable files). For example, if we want to produce the file `manual/examples.txt` we can write:

    want ["manual/examples.txt"]

The `want` function takes a list of strings. In Shake lists are written `[item1,item2,item2]` and strings are written `"contents of a string"`. Special characters in strings can be escaped using `\` (e.g. `"\n"` for newline) and directory separators are always written `/`, even on Windows.

Most files have the same name on all platforms, but executable files on Windows usually have the `.exe` extension, while on POSIX they have no extension. When writing cross-platform build systems (like the initial example), we can write:

    want ["_build/run" <.> exe]

The `<.>` function adds an extension to a file path, and the built-in `exe` variable evaluates to `"exe"` on Windows and `""` otherwise.

#### Defining rules

A rule describes the steps required to build a file. A rule has two components, a <tt><i>pattern</i></tt> and some <tt><i>actions</i></tt>:

<pre>
<i>pattern</i> *&gt; \out -> do
    <i>actions</i>
</pre>

The <tt><i>pattern</i></tt> is a string saying which files this rule can build. It may be a specific file (e.g.  `"manual/examples.txt" *> ...`) or may use wildcards:

* The `*` wildcard matches anything apart from a directory separator. For example `"manual/*.txt"` would define a rule for any `.txt` file in the `manual` directory, including `manual/examples.txt`, but would not match `manual/examples.zip`, `examples.txt` or `manual/docs/examples.txt`.
* The `//` wildcard matches any number of complete path components. For example `//*.txt` would define a rule for any `.txt` file, including `manual/examples.txt`. As another example, `manual//examples.txt` would match any file named `examples.txt` inside `manual`, including both `manual/examples.txt` and `manual/docs/examples.txt`.

It is an error for multiple patterns to match a file being built, so you should keep patterns minimal. Looking at the two rules in the initial example:

    "_build/run" <.> exe *> ...
    "_build//*.o" *> ...

The first matches only the `run` executable, using `<.> exe` to ensure the executable is correctly named on all platforms. The second matches any `.o` file anywhere under `_build`. As examples, `_build/main.o` and `_build/foo/bar.o` both match while `main.o` and `_build/main.txt` do not.

The <tt><i>actions</i></tt> are a list of steps to perform and are listed one per line, indented beneath the rule. Actions both express dependencies (say what this rule uses) and run commands (actually generate the file). During the action the `out` variable is bound to the file that is being produced.

#### A simple rule

Let's look at a simple example of a rule:

    "*.rot13" *> \out ->
        let src = out -<.> "txt"
        need [src]
        cmd "rot13" src "-o" out

This rule can build any `.rot13` file. Imagine we are building `"file.rot13"`, it proceeds by:

* Using `let` to define a local variable `src`, using the `-<.>` extension replacement method, which removes the extension from a file and adds a new extension. When `out` is `"file.rot13"` the variable `src` will become `file.txt`.
* Using `need` to introduce a dependency on the `src` file, ensuring that if `src` changes then `out` will be rebuilt and that `src` will be up-to-date before any further commands are run.
* Using `cmd` to run the command line `rot13 file.txt -o file.rot13`, which should read `file.txt` and write out `file.rot13` being the ROT13 encoding of the file.

Many rules follow this pattern - calculate some local variables, `need` some dependencies, then use `cmd` to perform some actions. We now discuss each of the three statements.

#### Local variables

Local variables can be defined as:

<pre>
let <i>variable</i> = <i>expression</i>
</pre>

Where <tt><i>variable</i></tt> is a name consisting of letters, numbers and underscores (a-z, A-Z, 0-9 and \_). All variables _must_ start with a lower-case letter.

An <tt><i>expression</i></tt> is any combination of variables and function calls, for example `out -<.> "txt"`. A list of some common functions is discussed later.

Variables are evaluated by substituting the <tt><i>expression</i></tt> everywhere the <tt><i>variable</i></tt> is used. In the simple example we could have equivalently written: 

    "*.rot13" *> \out ->
        need [out -<.> "txt"]
        cmd "rot13" (out -<.> "txt") "-o" out

Variables are local to the rule they are defined in, cannot be modified, and should not be defined multiple times within a single rule.

#### File dependencies

You can express a dependency on a file with:

    need ["file.src"]

To depend on multiple files you can write:

    need ["file.1","file.2"]

Or alternatively:

    need ["file.1"]
    need ["file.2"]

It is preferable to use fewer calls to `need`, if possible, as multiple files required by a `need` can be built in parallel.

#### Running external commands

The `cmd` function allows you to call system commands, e.g. `gcc`. Taking the initial example, we see: 

    cmd "gcc -o" [out] os

After substituting `out` (a string variable) and `os` (a list of strings variable) we might get:

    cmd "gcc -o" ["_make/run"] ["_build/main.o","_build/constants.o"]

The `cmd` function takes any number of space-separated expressions. Each expression can be either a string (which is treated as a space-separated list of arguments) or a list of strings (which is treated as a direct list of arguments).  Therefore the above command line is equivalent to either of:

    cmd "gcc -o _make/run _build/main.o _build/constants.o"
    cmd ["gcc","-o","_make/run","_build/main.o","_build/constants.o"]

To properly handle unknown string variables it is recommended to enclose them in a list, e.g. `[out]`, so that even if `out` contains a space it will be treated as a single argument.

The `cmd` function as presented here will fail if the system command returns a non-zero exit code, but see later for how to treat failing commands differently.

As a wart, if the `cmd` call is _not_ the last line of a rule, you must precede it with `() <- cmd ...`.

#### Filepath manipulation functions

Shake provides a complete library of filepath manipulation functions (see the manual docs for `Development.Shake.FilePath`), but the most common are:

* `a </> b` - add the path components together with a slash, e.g. `"_build" </> "main.o"` equals `"_build/main.o"`.
* `a <.> b` - add an extension, e.g. `"main" <.> "o"` equals `"main.o"`.
* `a ++ b` - append two strings together, e.g. `"hello" ++ "world"` equals `"hello world"`.
* `a -<.> b` - replace an extension, e.g. `"main.c" -<.> "o"` equals `"main.o"`.
* `dropExtension a` - drop the final extension of a filepath if it has one, e.g. `dropExtension "main.o"` equals `"main"`, while `dropExtension "main"` equals `"main"`.
* `takeFileName a` - drop the path component, e.g. `takeFileName "_build/src/main.o"` equals `"main.o"`.
* `dropDirectory1 a` - drop the first path component, e.g. `dropDirectory1 "_build/src/main.o"` equals `"src/main.o"`. 

## Advanced Syntax

The following section covers more advanced operations that are necessary for moderately complex build systems, but not simple ones.

#### Directory listing dependencies

The function `getDirectoryFiles` can retrieve a list of files within a directory:

    cs <- getDirectoryFiles "" ["//*.c"]

After this operation `cs` will be a variable containing all the files matching the pattern `"//*.c"` (those with the extension `.c`) starting at the directory `""` (the current directory). To obtain all `.c` and `.cpp` files in the src directory we can write:

    cs <- getDirectoryFiles "src" ["//*.c","//*.cpp"]

The `getDirectoryFiles` operation is tracked by the build system, so if the files in a directory changes the rule will rebuild in the next run. You should only use `getDirectoryFiles` on source files, not files that are generated by the build system, otherwise the results will change while you are running the build and the build may be inconsistent.

#### List manipulations

Many functions work with lists of values. The simplest operation on lists is to join two lists together, which we do with `++`. For example, `["main.c"] ++ ["constants.c"]` equals `["main.c","constants.c"]`.


Using a _list comprehension_ we can produce new lists, apply functions to the elements and filtering them. As an example:

    ["_build" </> c -<.> "o" | c <- cs]

This expression grabs each element from `cs` and names it `c` (the `c <- cs`, pronounced "`c` is drawn from `cs`"), then applies the expression  `"_build" </> c -<.> "o"` to each element. If we start with the list `["main.c","constants.c"]`, we would end up with `["_build/main.o","_build/constants.o"]`.

List expressions also allow us to filter the list, for example we could know that the file `"evil.c"` is in the directory, but should not be compiled. We can extend that to:

    ["_build" </> c -<.> "o" | c <- cs, c /= "evil.c"]

The `/=` operator checks for inequality, and any predicate after the drawn from is used to first restrict which elements of the list are available.

#### Using `gcc` to collect headers

One common problem when building `.c` files is tracking down which headers they transitively import, and thus must be added as a dependency. We can solve this problem by asking `gcc` to create a file while building that contains a list of all the imports. If we run:

    gcc -c main.c -o main.o -MMD -MF main.m

That will compile `main.c` to `main.o`, and also produce a file `main.m` containing the dependencies. To add these dependencies as dependencies of this rule we can call:

    needMakefileDependencies "main.m"

Now, if either `main.c` or any headers transitively imported by `main.c` change, the file will be rebuilt. In the initial example the complete rule is:

    "_build//*.o" *> \out -> do
        let c = dropDirectory1 $ out -<.> "c"
        let m = out -<.> "m"
        () <- cmd "gcc -c" [c] "-o" [out] "-MMD -MF" [m]
        needMakefileDependencies m

We first compute the source file `c` (e.g. `"main.c"`) that is associated with the `out` file (e.g. `"_build/main.o"`). We then compute a temporary file `m` to write the dependencies to (e.g. `"_build/main.m"`). We then call `gcc` using the `-MMD -MF` flags and then finally call `needMakefileDependencies`.

#### Global variables

Variables local to a rule are defined using `let`, but you can also define global variables. Global variables are defined before the `main` call, for example:

    buildDir = "_build"
 
You can now use `buildDir` in place of `"_build"` throughout. You can also define parametrised variables (functions) by adding argument names:

    buildDir x = "_build" </> x

We can now write:

    buildDir ("run" <.> exe) $ \out -> do
        ...

All global variables and functions can be though of as being expanded wherever they are used, although in practice may have their evaluation shared.

#### A clean command

A standard clean command is defined as:

    phony "clean" $ do
        removeFilesAfter "_build" ["//*"]

Running the build system with the `clean` argument, e.g. `runhaskell _build/run clean` will remove all files under the `_build` directory. This clean command is formed from two separate pieces. Firstly, we can define `phony` commands as:

<pre>
phony "<i>name</i>" $ do
    <i>actions</i>
</pre>

Where <tt><i>name</i></tt> is the name used on the command line to invoke the actions, and <tt><i>actions</i></tt> are the list of things to do in response. These names are not dependency tracked and are simply run afresh each time they are requested.

The <tt><i>actions</i></tt> can be any standard build actions, although for a `clean` rule, `removeFilesAfter` is typical. This function waits until after any files have finished building (which will be none, if you do `runhaskell _build/run clean`) then deletes all files matching `//*` in the `_build` directory.

## Running

This section covers how to run the build system you have written.

#### Compiling the build system

As shown before, we can use `runhaskell _build/run` to execute our build system, but doing so causes the build script to be compiled afresh each time. A more common approach is to add a shell script that compiles the build system and runs it. In the example directory you will find `build.sh` (Linux) and `build.bat` (Windows), both of which execute the same interesting commands. Looking at `build.sh`:

    #!/bin/sh
    mkdir -p _shake
    ghc --make Build.hs -rtsopts "-with-rtsopts=-I0 -qg -qb" -outputdir=_shake -o _shake/build && _shake/build $*

This script creates a folder named `_shake` for the build system objects to live in, then runs `ghc --make Build.hs` to produce `_shake/build`, then executes `_shake/build` with all arguments it was given.

Now you can run a build by simply typing `./build.sh` on Linux, or `build` on Windows. On Linux you may want to alias `build` to `./build.sh`. For the rest of this document we will assume `build` runs the build system.

#### Command line flags

The initial example build system supports a number of command line flags, including:

* `build` will compile all files required by `want`.
* `build _build/main.o` will compile enough to create `_build/main.o`, ignoring all `want` requirements.
* `build clean` will delete the contents of `_build`, because of our `phony` command.
* `build --help` will list out all flags supported by the build system, currently 36 flags. Most flags supported by `make` are also supported by Shake based build systems.
* `build -j8` will compile up to 8 rules simultaneously, by default Shake uses 1 processor.

Most flags can also be set within the program by modifying the `shakeOptions` value. As an example, `build --metadata=_metadata/` causes all Shake metadata files to be stored with names such as `_metadata/.database`. Alternatively we can write `shakeOptions{shakeFiles="_metadata/"}` instead of our existing `shakeFiles="_build/"`. Values passed on the command line take preference over those given by `shakeOptions`. Multiple overrides can be given to `shakeOptions` by separating them with a comma, for example `shakeOptions{shakeFiles="_build/",shakeThreads=8}`.

#### Progress prediction

One useful feature of Shake is that it can predict the remaining build time, based on how long previous builds have taken. The number is only a prediction, but it does take account of which files require rebuilding, how fast your machine is currently running, parallelism settings etc. You can display progress messages in the titlebar of a Window by either:

* Running `build --progress`
* Setting `shakeOptions{shakeProgress=progressSimple}`

The progress message will be displayed in the titlebar of the Window, for example `3m12s (82%)` to indicate that the build is 82% complete and is predicted to take a further 3 minutes and 12 seconds. If you are running Windows 7 or higher and place the [`shake-progress`](http://github.org/ndmitchell/shake) utility somewhere on your `%PATH%` then the progress will also be displayed in the taskbar progress indicator:

![](shake-progress.png)

Progress prediction is likely to be relatively poor during the first build and after running `build clean`, as then Shake has no information about the predicted execution time for each rule. To rebuild from scratch without running clean (because you really want to see the progress bar!) you can use the argument `--always-make`, which assumes all rules need rerunning. 

#### Lint

Shake features a built in "lint" features to check the build system is well formed. To run `build --lint`. You are likely to catch more lint violations if you first `build clean`. Sadly, lint does _not_ catch missing dependencies. However, it does catch:

* The current directory does not change. You should never change the current directory within the build system as multiple rules running at the same time will share the current directory. You can still run `cmd` calls in different directories using the `Cwd` argument.
* Dependencies are not modified after they are depended upon. The common reason for violating this check is that you one rule writing to a file which has a different rule associated with it.

There is a small performance penalty for building with `--lint`, but it is typically small.

#### Profiling

Shake features an advanced profiling mode. To build with profiling run `build --report` which will generate an interactive HTML profile at `report.html`. This report lets you examine what happened in that run, what takes more time to run, what rules depend on what etc. There is a help page as part of the profiling output.

To view profiling information for the _previous_ program run, you can run `build --no-build --report`. This feature is useful if you have a build execution where a file unexpectedly rebuilds, you can generate a profiling report afterwards.

#### Tracing and debugging

Verbosity, plus adding error or print statements, putLoud.

## Extensions

#### Advanced `cmd` usage

The `cmd` function can also obtain the stdout and stderr streams, along with the  exit code. As an example:

    (ExitCode code, Stdout out, Stderr err) <- cmd "gcc --version"

Now the variable `code` is bound to the exit code, while `out` and `err` are bound to the stdout and stderr streams. If `ExitCode` is not requested then any non-zero return value will raise an error.

You can also use `Shell` and `Cwd`.

#### Dependencies on extra information

Oracles, get the gcc version number and use that. Show how to depend on the `Stdout` of `gcc --version`.

#### Dependencies on environment variables

Allow picking a different gcc from the environment variable

#### Resources

Generalise to building multiple things, then ensure we only ever link one at a time using resources to limit.

## The Haskell Zone

Most of the rest doesn't require very much Haskell, however, as you start getting deeper, you do require more Haskell. Most of the things in this area are either impossible to do with other build systems or can be faked by shell script. None of the Haskell is advanced.

#### Generated header files

Need to generate them first, so need to write something that guesses, or a specific rule knowing what does it.

#### Recursive imports

The standard .hs/.hi and recursive tricks.

#### Haskell Expressions

Use pure functions by importing them and just using them however you want. Example of importing Data.Char and making all output file names which don't have numbers.

#### Haskell Actions

`liftIO` to run any IO stuff.

#### Adding command line flags

Requires real Haskell code.

Define a C compile flag that works with both GCC and MSVC.
Let's add a flag to make it use a different version of GCC.
