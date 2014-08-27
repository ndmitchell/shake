# Home page

Shake is a tool for writing build systems - an alternative to make, Scons, Ant etc. Shake has been used commercially for over five years, running thousands of builds per day.

<!--<neil:home>-->

## Why try Shake?

Shake lets you express lots of dependencies more precisely and directly than other build systems. This power is coupled with a fast and robust implementation that ensures builds run quickly and produce results you can rely on. In particular, Shake excels at working with generated files, somewhere other build systems often struggle. [Read more](Why.md).

## Getting started

You need to install the [Haskell Platform](http://www.haskell.org/platform/), type `cabal update && cabal install shake`, then run `shake --demo` which will create a sample Shake file build system and compile it.

## Going deeper

Read the [user manual](Manual.md) and see the [individual function documentation](http://hackage.haskell.org/packages/archive/shake/latest/doc/html/Development-Shake.html), including lots of examples. Read about the [theory behind Shake](http://community.haskell.org/~ndm/downloads/paper-shake_before_building-10_sep_2012.pdf) and a [video introducing the theory](http://www.youtube.com/watch?v=xYCPpXVlqFM). Read about the [history and philosophy](About.md).

## Asking questions

Stuck? Confused? Frustrated? Please get in contact! You can ask questions on [StackOverflow](http://stackoverflow.com/questions/tagged/shake-build-system) using the tag `shake-build-system`. You can email the [Mailing list](https://groups.google.com/forum/?fromgroups#!forum/shake-build-system) with anything about Shake. If you find a bug you can report it at the [GitHub issue tracker](https://github.com/ndmitchell/shake/issues). If your question needs to remain confidential you can [email me](http://community.haskell.org/~ndm/contact/), although any insights from your question won't be public, so the other approaches are preferred. If in doubt, just use the [Mailing list](https://groups.google.com/forum/?fromgroups#!forum/shake-build-system).

## Technical details

Shake build systems are Haskell programs that make heavy use of the `shake` library. While Shake build systems are Haskell programs, they can be treated as a powerful version of make with slightly funny syntax. The build system requires no significant Haskell knowledge, and is designed so that most features are accessible by learning the "Shake syntax", without any appreciation of what the underlying Haskell means. The library features monadic dependencies (new dependencies can be discovered after files can depend on other files), polymorphic dependencies (your dependencies don't have to be files) and dependencies that rebuild but don't change stop rebuilding.

## What else?

Shake can execute [Ninja files](Ninja.md), integrate with [CMake](http://www.cmake.org/) and [Meson](https://jpakkane.github.io/meson/), predict completion time progress messages, profiling support, robust underlying theory.

## Used by

Shake is used by lots of companies, but only a few have declared so publicly.
 
* [Standard Chartered](http://www.standardchartered.com/) have been using Shake since 2009, as described in the section 6 of the [academic paper](http://community.haskell.org/~ndm/downloads/paper-shake_before_building-10_sep_2012.pdf).
* [factis research GmbH](http://www.factisresearch.com/), as described in their [blog post](http://funktionale-programmierung.de/2014/01/16/build-system-haskell.html).
* [Samplecount](http://samplecount.com/) have been using Shake since 2012, as mentioned in their [tweet](https://twitter.com/samplecount/status/491581551730511872).

<!--</neil:home>-->
