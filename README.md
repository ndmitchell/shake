# Shake [![Hackage version](https://img.shields.io/hackage/v/shake.svg?style=flat)](https://hackage.haskell.org/package/shake) [![Build Status](https://img.shields.io/travis/ndmitchell/shake.svg?style=flat)](https://travis-ci.org/ndmitchell/shake)

Shake is a tool for writing build systems - an alternative to make, Scons, Ant etc. Shake has been used commercially for over five years, running thousands of builds per day.

#### Documentation

* **Why use Shake?** Shake lets you write large robust build systems, which deal properly with generated source files and run quickly. If you are writing a custom build system of any moderate size (more than a few rules) you should use Shake. The advantages over other build systems are detailed in the document [Why choose Shake?](https://github.com/ndmitchell/shake/blob/master/docs/Why.md#readme).
* **How do I use Shake?** Shake is a Haskell library that you use to define your rules. The [Shake manual](https://github.com/ndmitchell/shake/blob/master/docs/Manual.md#readme) provides a walk through of a small but realistic example, assuming no Haskell knowledge.
* [Generated documentation](http://hackage.haskell.org/packages/archive/shake/latest/doc/html/Development-Shake.html) for all functions, includes lots of examples.
* [Running Ninja builds](https://github.com/ndmitchell/shake/blob/master/docs/Ninja.md#readme) using Shake.
* [Blog posts](http://neilmitchell.blogspot.co.uk/search/label/shake) detailing ongoing development work.
* [Profile report demo](https://cdn.rawgit.com/ndmitchell/shake/35fbe03c8d3bafeae17b58af89497ff3fdd54b22/html/demo.html) explaining what the profile reports mean.
* [Academic paper](http://community.haskell.org/~ndm/downloads/paper-shake_before_building-10_sep_2012.pdf) on the underlying principles behind Shake.
* [Video](http://www.youtube.com/watch?v=xYCPpXVlqFM) of a talk introducing Shake.

#### Other links

* [Download the Haskell package](http://hackage.haskell.org/package/shake) from Hackage and install it using Cabal.
* [Mailing list](https://groups.google.com/forum/?fromgroups#!forum/shake-build-system) for any questions/bugs/thoughts on Shake. If you need more information and aren't sure where to start, use the mailing list.
* [Questions](http://stackoverflow.com/questions/tagged/shake-build-system) can be asked on StackOverflow with the tag `shake-build-system`.
* [Bugs](https://github.com/ndmitchell/shake/issues) can be reported on the GitHub issue tracker.
* [Source code](http://github.com/ndmitchell/shake) in a git repo, stored at GitHub.
* Continuous integration with [Travis](https://travis-ci.org/ndmitchell/shake) and [Hydra](http://hydra.cryp.to/jobset/shake/master).

#### Companies using Shake

* [Standard Chartered](http://www.standardchartered.com/) have been using Shake since 2009, as described in the section 6 of the [academic paper](http://community.haskell.org/~ndm/downloads/paper-shake_before_building-10_sep_2012.pdf).
* [factis research GmbH](http://www.factisresearch.com/), as described in their [blog post](http://funktionale-programmierung.de/2014/01/16/build-system-haskell.html).
* [Samplecount](http://samplecount.com/) have been using Shake since 2012, as mentioned in their [tweet](https://twitter.com/samplecount/status/491581551730511872).
* [CovenantEyes](http://samplecount.com/) use Shake to build their Windows client, as mentioned in their [tweet](https://twitter.com/eacameron88/status/543219899599163392).
* At least 10 other companies are using Shake internally.

Is your company using Shake? Write something public (even just a [tweet  to `@ndm_haskell`](https://twitter.com/ndm_haskell)) and I'll include a link.

#### Projects using Shake

* [Kansas Lava build rules](https://github.com/gergoerdi/kansas-lava-shake).
* [shake-language-c](http://hackage.haskell.org/package/shake-language-c) is a small library on top of Shake that allows cross-compiling C, C++ and Objective-C code to various target platforms.
* [shake-cabal-build](http://hackage.haskell.org/package/shake-cabal-build) is a small script that uses Cabal sandboxes for initialising and updating build systems based on Shake.
