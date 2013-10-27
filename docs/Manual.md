# Shake Manual

Shake is a Haskell library for writing build systems - designed as a replacement for `make`. This document describes how to get started with Shake, assuming no prior Haskell knowledge. First, let's take a look at a Shake build system:

    import Development.Shake
    import Development.Shake.Command
    import Development.Shake.FilePath
    import Development.Shake.Util
    
    main :: IO ()
    main = shakeArgs shakeOptions{shakeFiles="_build/"} $ do
        want ["_build/run" <.> exe]
    
        phony "clean" $ removeFilesAfter "_build" ["//*"]
    
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

A rule describes the steps required to build a file. A rule has two components, a <tt style="font-style:italic;color:purple;">pattern</tt> and some <tt style="font-style:italic;color:purple;">actions</tt>:

<pre>
<i>pattern</i> *&gt; \out -> do
    <i>actions</i>
</pre>

The <tt><i>pattern</i></tt> is a string saying which files this rule can build. It may be a specific file (e.g.  `"manual/examples.txt" *> ...`) or may use wildcards:

* The `*` wildcard matches anything apart from a directory separator. For example `"manual/*.txt"` would define a rule for any `.txt` file in the `manual` directory, including `manual/examples.txt`, but would not match `manual/examples.zip`, `examples.txt` or `manual/docs/examples.txt`.
* The `//` wildcard matches any number of complete path components. For example `//*.txt` would define a rule for any `.txt` file, including `manual/examples.txt`. As another example, `manual//examples.txt` would match any file named `examples.txt` inside `manual`, including both `manual/examples.txt` and `manual/docs/examples.txt`.

It is an error for multiple patterns to match a file being built, so you should keep patterns minimal. Looking at the two examples in the initial example:

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
* Using `cmd` to run the command line `rot13 file.txt -o file.rot13`, which hopefully reads `file.txt` and writes out `file.rot13` being the ROT13 encoding of the file.

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

To properly handle string variables it is recommended to enclose them in a list, e.g. `[out]`, so that even if `out` contains a space it will be treated as a single argument.

As a wart, if the `cmd` call is _not_ the last line of a rule, you must precede it with `() <- cmd ...`.

#### Filepath manipulation functions

`</>` and `<.>`


## Advanced Syntax


#### List manipulations

list comp.

#### Using `gcc` to collect headers

`-MDD -MF file`

`needMakefileDependencies`

#### Results from `cmd`


#### Directory listing dependencies

`getDirectoryFiles`

#### Global variables

Include how to define functions.


#### A clean command

`phony` and `removeFilesAfter`

## Running

#### Compiling the build system

Create a file named `build.sh` or `build.bat` with:

    ghc --make -special-link-flags -o _make/maker && _make/maker 

This ensures your build runs faster and you don't have to recompile each time.

Now you can run `build` directly to start your build system.

#### Command line flags

Run `build --help` to see which flags are available. Most of the features from make are available, along with a few additional. As an example we can set where the `.database` file goes. Note we already customise this with `shakeOptions` - in general most flags you can set could also be specified as `shakeOption`.

## Extensions

#### Dependencies on extra information

Oracles, get the gcc version number and use that.

#### Dependencies on environment variables

Allow picking a different gcc from the environment variable

#### Adding command line flags

Let's add a flag to make it use a different version of GCC.

#### Resources

Generalise to building multiple things, then ensure we only ever link one at a time using resources to limit.

#### Prediction and progress

Use `--assume-dirty`.

You can get progress messages.


#### Profiling

#### Lint

#### Generated header files

Need to generate them first, so need to write something that guesses, or a specific rule knowing what does it.

#### Recursive imports

The standard .hs/.hi and recursive tricks.

#### Additional Haskell

Define a C compile flag that works with both GCC and MSVC.
