# Shake is...

* **A build system** - an alternative to make, Scons, Ant etc.
* **Reliable and robust** - having been relied on commercially for over five years.
* **Powerful dependencies** - letting you express the problem precisely and directly.
* **Fast to run** - both to build from scratch and to rebuild.

Large build systems written using Shake tend to be significantly shorter and simpler, while also running faster. If your project can use a canned build system (e.g. Visual Studio, cabal) do that; if your project is very simple use a Makefile; otherwise use Shake.

[Click to read about why you should use Shake.](Why.md)

## Using Shake

To try Shake you need to install the [Haskell Platform](http://www.haskell.org/platform/), then type `cabal update && cabal install shake`, then run `shake --demo`. The final step will create a sample Shake build system and run it (you should see [this output](Demo.md)).

To write your own Shake build system, read the [user manual](Manual.md) and refer to the [API documentation](http://hackage.haskell.org/packages/archive/shake/latest/doc/html/Development-Shake.html). Shake build systems are [Haskell](http://haskell.org/) programs, but can be treated as a powerful version of make with slightly funny syntax. The build system requires no significant Haskell knowledge, and is designed so that most features are accessible by learning the "Shake syntax", without any appreciation of what the underlying Haskell means.

[Click to read about the background and approach of Shake.](About.md)

## Asking questions

Stuck? Confused? Frustrated? Please get in contact! If in doubt, just email the [mailing list](https://groups.google.com/forum/?fromgroups#!forum/shake-build-system).

You can ask questions on [StackOverflow](http://stackoverflow.com/questions/tagged/shake-build-system) using the tag `shake-build-system`. You can email the [mailing list](https://groups.google.com/forum/?fromgroups#!forum/shake-build-system) with anything about Shake. If you find a bug you can report it at the [GitHub issue tracker](https://github.com/ndmitchell/shake/issues). If your question needs to remain confidential you can [email me](http://community.haskell.org/~ndm/contact/), although any insights from your question won't be public, so the other approaches are preferred. 

## What else?

Shake can execute [Ninja files](Ninja.md), allowing integration with [CMake](http://www.cmake.org/) and [Meson](https://jpakkane.github.io/meson/). Shake can [predict completion time](Manual.md#progress), [profile build systems](Manual.md#profiling) and [sanity check builds](Manual.md#lint). Shake is based on a robust underlying theory from [this academic paper](http://community.haskell.org/~ndm/downloads/paper-shake_before_building-10_sep_2012.pdf). Shake is an open source project under the [BSD license](https://github.com/ndmitchell/shake/blob/master/LICENSE) hosted on [GitHub](https://github.com/ndmitchell/shake/).

## Who uses Shake?

Shake is used by lots of companies, but only a few have declared so publicly.
 
* [Standard Chartered](http://www.standardchartered.com/) have been using Shake since 2009, as described in the section 6 of the [academic paper](http://community.haskell.org/~ndm/downloads/paper-shake_before_building-10_sep_2012.pdf).
* [factis research GmbH](http://www.factisresearch.com/) use Shake to compile their [Checkpad MED](http://www.checkpad.de/) application, as described in their [blog post](http://funktionale-programmierung.de/2014/01/16/build-system-haskell.html).
* [Samplecount](http://samplecount.com/) have been using Shake since 2012, as mentioned in their [tweet](https://twitter.com/samplecount/status/491581551730511872), producing several [open-source projects](https://github.com/samplecount) for working with Shake.
* [CovenantEyes](http://samplecount.com/) use Shake to build their Windows client, as mentioned in their [tweet](https://twitter.com/eacameron88/status/543219899599163392).

There are several libraries providing pre-made rules for Shake:

* [shake-language-c](http://hackage.haskell.org/package/shake-language-c) allows cross-compiling C, C++ and Objective-C code to various target platforms.
* [shake-cpp](https://github.com/jfeltz/shake-cpp) is an abstraction layer for Shake, providing simple C++ rules.
* [shake-cabal-build](http://hackage.haskell.org/package/shake-cabal-build) is a small script that uses Cabal sandboxes for initialising and updating build systems which use Shake.
* [Kansas Lava build rules](http://hackage.haskell.org/package/kansas-lava-shake) provide a set of rules to work with [Kansas Lava](https://hackage.haskell.org/package/kansas-lava) and compile down to [Xilinx chips](http://www.xilinx.com/).
* [avr-shake](https://hackage.haskell.org/package/avr-shake) provides rules for building things with [AVR Crosspack](http://www.obdev.at/products/crosspack/index.html).

Several open-source projects make key use of Shake:

* [ToolCabal](https://github.com/TiborIntelSoft/ToolCabal) is a rewrite of [Cabal](https://www.haskell.org/cabal/) using Shake as the dependency engine.
* [ghc-make](https://github.com/ndmitchell/ghc-make) uses Shake to build programs with GHC, speeding up checking if the build is clean.
* [GHC](https://ghc.haskell.org/trac/ghc/wiki/Building/Shake) is in the process of migrating to a Shake-based build system.
* [shake-install](https://github.com/alphaHeavy/shake-install) helps build a set of cabal packages in the correct order.

And finally, here are a few tutorials and blog posts:

* [Many articles from the author of Shake](http://neilmitchell.blogspot.co.uk/search/label/shake), covering ongoing development.
* [Writing a simple Blog with Shake and Pandoc](http://declaredvolatile.org/blog/2014-09-14-writing-a-simple-blog-with-shake-and-pandoc/).

Do you have a link that should be included above? Let me know with a [tweet to `@ndm_haskell`](https://twitter.com/ndm_haskell).
