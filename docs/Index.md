# Shake is...

* **A build system** -- an alternative to make, Scons, Ant etc.
* **Reliable and robust** -- having been relied on commercially for over five years.
* **Powerful** -- letting you express the problem precisely and directly.
* **Fast to run** -- both to build from scratch and to rebuild.

Large build systems written using Shake tend to be significantly simpler, while also running faster. If your project can use a canned build system (e.g. Visual Studio, cabal) do that; if your project is very simple use a Makefile; otherwise use Shake.

The original motivation behind the creation of Shake was to allow rules to discover additional dependencies after running previous rules, allowing the build system to generate files and then examine them to determine their dependencies -- something that cannot be expressed directly in most build systems. However, now Shake is a suitable build tool even if you do not require that feature.

[Click to read about why you should use Shake.](Why.md)

## Using Shake

To try Shake you need to install the [Haskell Stack](http://haskellstack.org/), then type `stack install shake`, then run `stack exec -- shake --demo`. The final step will create a sample Shake build system and run it (you should see [this output](Demo.md)).

To write your own Shake build system, read the [user manual](Manual.md) and refer to the [API documentation](https://hackage.haskell.org/packages/archive/shake/latest/doc/html/Development-Shake.html). Further documentation on specific topics, including more examples, is available from [the FAQ](FAQ.md). Shake build systems are [Haskell](https://haskell.org/) programs, but can be treated as a powerful version of make with slightly funny syntax. The build system requires no significant Haskell knowledge, and is designed so that most features are accessible by learning the "Shake syntax", without any appreciation of what the underlying Haskell means.

[Click to read the user manual.](Manual.md)

## Asking questions

Stuck? Confused? Frustrated? Please get in contact! If in doubt, just email the [mailing list](https://groups.google.com/forum/?fromgroups#!forum/shake-build-system).

You can ask questions on [StackOverflow](https://stackoverflow.com/questions/tagged/shake-build-system) using the tag `shake-build-system`. You can email the [mailing list](https://groups.google.com/forum/?fromgroups#!forum/shake-build-system) with anything about Shake. If you find a bug you can report it at the [GitHub issue tracker](https://github.com/ndmitchell/shake/issues). If your question needs to remain confidential you can [email me](http://ndmitchell.com/), although any insights from your question won't be public, so the other approaches are preferred. 

## What else?

Shake can execute [Ninja files](Ninja.md), allowing integration with [CMake](http://www.cmake.org/) and [Meson](http://mesonbuild.com/). Shake can [predict completion time](Manual.md#progress), [profile build systems](Profiling.md) and [sanity check builds](Manual.md#lint). Shake is based on a robust underlying theory from [this academic paper](http://ndmitchell.com/downloads/paper-shake_before_building-10_sep_2012.pdf). Shake is an open source project under the [BSD license](https://github.com/ndmitchell/shake/blob/master/LICENSE) hosted on [GitHub](https://github.com/ndmitchell/shake/) with a [range of contributors](https://github.com/ndmitchell/shake/graphs).

## Who uses Shake?

Shake is used by lots of companies, but only a few have declared so publicly.

* [Standard Chartered](https://www.standardchartered.com/) have been using Shake since 2009, as described in the section 6 of the [academic paper](http://ndmitchell.com/downloads/paper-shake_before_building-10_sep_2012.pdf).
* [factis research GmbH](http://www.factisresearch.com/) use Shake to compile their [Checkpad MED](http://www.checkpad.de/) application, as described in their [blog post](http://funktionale-programmierung.de/2014/01/16/build-system-haskell.html).
* [Samplecount](http://samplecount.com/) have been using Shake since 2012, as mentioned in their [tweet](https://twitter.com/samplecount/status/491581551730511872), producing several [open-source projects](https://github.com/samplecount) for working with Shake.
* [CovenantEyes](http://www.covenanteyes.com/) use Shake to build their Windows client, as mentioned in their [tweet](https://twitter.com/eacameron88/status/543219899599163392).
* [Keystone Tower Systems](http://keystonetowersystems.com/) has a robotic welder with a Shake build system plus Haskell code running in the control system, as mentioned in their [tweet](https://twitter.com/eric_oconnor/status/581576757062434816).
* [FP Complete](https://www.fpcomplete.com/) use Shake to [create Docker images](https://www.fpcomplete.com/blog/2015/08/stack-docker#images).
* [Genomics Plc](http://www.genomicsplc.com/) use Shake for the build system, their first major use of Haskell in the company.
* [codebender](https://codebender.cc/) use Shake to manage JavaScript packages that implement Arduino protocols and handle communication between the browser and Arduino devices.

There are several libraries providing pre-made rules for Shake:

* [shake-language-c](https://hackage.haskell.org/package/shake-language-c) allows cross-compiling C, C++ and Objective-C code to various target platforms.
* [shake-cpp](https://github.com/jfeltz/shake-cpp) is an abstraction layer for Shake, providing simple C++ rules.
* [kansas-lava-shake](https://hackage.haskell.org/package/kansas-lava-shake) provides a set of rules to work with [Kansas Lava](https://hackage.haskell.org/package/kansas-lava) and compile down to [Xilinx chips](http://www.xilinx.com/).
* [avr-shake](https://hackage.haskell.org/package/avr-shake) provides rules for building things with [AVR Crosspack](http://www.obdev.at/products/crosspack/index.html).
* [shake-minify](https://hackage.haskell.org/package/shake-minify) uses native Haskell code (no external `$PATH` dependencies) to minify CSS and JS files.
* [shake-pack](https://hackage.haskell.org/package/shake-pack) uses bz2 lib on the system to tar and bzip compress given files.

Several open-source projects make key use of Shake:

* [ToolCabal](https://github.com/TiborIntelSoft/ToolCabal) is a rewrite of [Cabal](https://www.haskell.org/cabal/) using Shake as the dependency engine.
* [ghc-make](https://github.com/ndmitchell/ghc-make) uses Shake to build programs with GHC, speeding up checking if the build is clean.
* [GHC](https://ghc.haskell.org/trac/ghc/wiki/Building/Shake) is in the process of migrating to a Shake-based build system.
* [shake-install](https://github.com/alphaHeavy/shake-install) helps build a set of cabal packages in the correct order.
* [OpenSUSE Haskell packaging](https://github.com/opensuse-haskell) makes use of Shake to [convert a Stack project to OBS](https://github.com/opensuse-haskell/cabal2obs).

And finally, here are a few tutorials and blog posts:

* [Many articles from the author of Shake](http://neilmitchell.blogspot.co.uk/search/label/shake), covering ongoing development.
* [Writing a simple Blog with Shake and Pandoc](http://declaredvolatile.org/blog/2014-09-14-writing-a-simple-blog-with-shake-and-pandoc/).

Do you have a link that should be included above? Let me know with a [tweet to `@ndm_haskell`](https://twitter.com/ndm_haskell).
