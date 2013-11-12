# Shake [![Build Status](https://travis-ci.org/ndmitchell/shake.png)](https://travis-ci.org/ndmitchell/shake)

**What is Shake?** Shake is a tool for writing build systems - an alternative to make, Scons, Ant etc.

**Why use Shake?** Shake lets you write large robust build systems, which deal properly with generated source files and run quickly. If you are writing a custom build system of any moderate size (more than a few rules) you should use Shake. The specific advantages are detailed in [this document](https://github.com/ndmitchell/shake/blob/master/docs/Why.md#readme).

**How do I use Shake?** Shake is a Haskell library that you use to define your rules. The [Shake manual](https://github.com/ndmitchell/shake/blob/master/docs/Manual.md#readme) provides a walk through of a small but realistic example, assuming no Haskell knowledge.

#### More Documentation

* [Running Ninja builds](https://github.com/ndmitchell/shake/blob/master/docs/Ninja.md#readme) using Shake.
* [Generated documentation](http://hackage.haskell.org/packages/archive/shake/latest/doc/html/Development-Shake.html) for all functions, including examples.
* [Blog posts](http://neilmitchell.blogspot.co.uk/search/label/shake) detailing ongoing development work.
* [Academic paper](http://community.haskell.org/~ndm/downloads/paper-shake_before_building-10_sep_2012.pdf) on the underlying principles behind Shake.
* [Video](http://www.youtube.com/watch?v=xYCPpXVlqFM) of a talk introducing Shake.

#### Other links

* [Download the Haskell package](http://hackage.haskell.org/package/shake) from Hackage and install it using Cabal.
* [Mailing list](https://groups.google.com/forum/?fromgroups#!forum/shake-build-system) for any questions/bugs/thoughts on Shake. If you need more information and aren't sure where to start, use the mailing list.
* [Questions](http://stackoverflow.com/questions/tagged/shake-build-system) can be asked on StackOverflow with the tag `shake-build-system`.
* [Bugs](https://github.com/ndmitchell/shake/issues) can be reported on the GitHub issue tracker.
* [Source code](http://github.com/ndmitchell/shake) in a git repo, stored at GitHub.
