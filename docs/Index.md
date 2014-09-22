# Shake is...

* **A build system** - an alternative to make, Scons, Ant etc.
* **Reliable and robust** - having been relied on commercially for over five years.
* **Powerful dependencies** - letting you express many relationships precisely and directly.
* **Fast to run** - both to build from scratch and to rebuild.

Large build systems written using Shake tend to be significantly shorter and simpler, while also running faster. If your project can use a canned build system (e.g. Visual Studio, cabal) do that; if your project is very simple use a Makefile; otherwise use Shake.

[Click to read about why you should use Shake.](Why.md)

## Using Shake

To try Shake you need to install the [Haskell Platform](http://www.haskell.org/platform/), then type `cabal update && cabal install shake`, then run `shake --demo`. The final step will create a sample Shake build system and run it (you should see [this output](Demo.md)).

To write your own Shake build system, read the [user manual](Manual.md) and refer to the [API documentation](http://hackage.haskell.org/packages/archive/shake/latest/doc/html/Development-Shake.html). Shake build systems are [Haskell](http://haskell.org/) programs, but can be treated as a powerful version of make with slightly funny syntax. The build system requires no significant Haskell knowledge, and is designed so that most features are accessible by learning the "Shake syntax", without any appreciation of what the underlying Haskell means.

[Click to read about the background and approach of Shake.](Why.md)

## Asking questions

Stuck? Confused? Frustrated? Please get in contact! If in doubt, just email the [mailing list](https://groups.google.com/forum/?fromgroups#!forum/shake-build-system).

You can ask questions on [StackOverflow](http://stackoverflow.com/questions/tagged/shake-build-system) using the tag `shake-build-system`. You can email the [mailing list](https://groups.google.com/forum/?fromgroups#!forum/shake-build-system) with anything about Shake. If you find a bug you can report it at the [GitHub issue tracker](https://github.com/ndmitchell/shake/issues). If your question needs to remain confidential you can [email me](http://community.haskell.org/~ndm/contact/), although any insights from your question won't be public, so the other approaches are preferred. 

## What else?

Shake can execute [Ninja files](Ninja.md), integrate with [CMake](http://www.cmake.org/) and [Meson](https://jpakkane.github.io/meson/), predict completion time progress messages, profiling support, robust underlying theory.

## Who uses Shake?

Shake is used by lots of companies, but only a few have declared so publicly.
 
* [Standard Chartered](http://www.standardchartered.com/) have been using Shake since 2009, as described in the section 6 of the [academic paper](http://community.haskell.org/~ndm/downloads/paper-shake_before_building-10_sep_2012.pdf).
* [factis research GmbH](http://www.factisresearch.com/), as described in their [blog post](http://funktionale-programmierung.de/2014/01/16/build-system-haskell.html).
* [Samplecount](http://samplecount.com/) have been using Shake since 2012, as mentioned in their [tweet](https://twitter.com/samplecount/status/491581551730511872).

There are also lots of examples:
