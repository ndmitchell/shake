# FAQ

#### Q: Where's the documentation?

The main documentation is the [user manual](Manual.md#readme), which also serves as a tutorial. After the user manual, the following pages may be useful:

* [Function documentation](https://hackage.haskell.org/packages/archive/shake/latest/doc/html/Development-Shake.html) - a list of the functions available in Shake, with individual documentation/examples about each.
* [FAQ](FAQ.md) - which answers common questions.
* [Includes](Includes.md) - how to deal with include files, import statements and other dependencies between files.
* [Profiling](Profiling.md) - how to speed up an existing Shake build system.
* [Blog posts](http://neilmitchell.blogspot.co.uk/search/label/shake) - sporadic postings about ongoing development work or other Shake-related thoughts.

<!--
Shake is suitable for all sizes of build systems, from a simple C project to a huge cross-platform multi-language project. However, at different scales, different techniques tend to be applicable.
* [Small/simple build systems](Small.md#readme) - some simpler build systems can be written as _forward_ build systems, without the need to explicitly think about dependencies or targets. Useful for getting started, relies on a tool to automatically track your dependencies.
* [Large frequently changing build systems](Large.md#readme) - for large build systems, it is useful to split the build system interpreter and metadata apart, making changes to the Haskell build system comparatively rare.
-->

Much of the theory behind Shake is covered in [a conference paper](http://ndmitchell.com/downloads/paper-shake_before_building-10_sep_2012.pdf) which was accompanied by [this video](https://www.youtube.com/xYCPpXVlqFM).

If you have any further questions:

* [Ask on StackOverflow](https://stackoverflow.com/questions/tagged/shake-build-system), using the tag `shake-build-system`.
* [Email us](https://groups.google.com/forum/?fromgroups#!forum/shake-build-system) for any questions/bugs/thoughts on Shake. If you need more information and aren't sure where to start, use the mailing list.


#### Q: Is Shake limited to building Haskell?

Not at all - Shake can build any project in any combination of languages. In fact, Shake isn't typically necessary for vanilla Haskell projects, as you can use [`cabal`](https://haskell.org/cabal) or [`stack`](http://haskellstack.org/). Shake is often used for building C/C++, Docker containers and Javascript/HTML/CSS projects.

#### Q: Where are functions for string manipulation?

Shake is a Haskell package focused on providing build-system functionality. Since Shake scripts are written in Haskell, they can easily access other Haskell packages. Most general needs are met by the standard [`base` library](https://hackage.haskell.org/package/base), but a few other useful general functions can be found in [the `extra` library](https://hackage.haskell.org/package/extra) (e.g. `trim` to remove whitespace around strings). For more specific functionality (e.g. parsing, databases, JSON) find a [suitable Haskell library](https://hackage.haskell.org/packages) and use that.

#### Q: Why is there a Shake.exe tool

If you have a file Shakefile.hs it will run it. If you have a Ninja build system it will run it. Generally, most people write their own Shake executable.

#### Q: What's the history of Shake?

I ([Neil Mitchell](http://ndmitchell.com)) was one of the people behind the [Yhc project](https://www.haskell.org/haskellwiki/Yhc), a Haskell compiler that died in a large part because of its build system. To quote from [the final blog post](http://yhc06.blogspot.co.uk/2011/04/yhc-is-dead.html):

> The biggest challenge for Yhc was the build system - we ended up with 10,000 lines of Python Scons scripts. Without a robust build system nothing else matters. When our sole Python hacker left the team that was the beginning of the end.

A Haskell compiler is a big undertaking, but the build system for a simple Haskell compiler shouldn't be that complicated.

When writing my thesis I needed a build system, and decided to try writing a simple Haskell DSL, which is still online [here](https://github.com/ndmitchell/thesis/blob/master/old/Main.hs). I defined a single operator [`<==`](https://github.com/ndmitchell/thesis/blob/master/old/Main.hs#L71) which let me express a relationship between an output and its dependencies - very simple, but it worked.

Later I moved to [Standard Chartered](https://www.sc.com/), where the build system was a mass of Makefiles, and it quickly became apparent that the project had outgrown the current approach. Without really surveying the alternatives, I decided that a Haskell DSL would be easiest to fit in with the existing infrastructure, so started writing some code. The first version of the build library took under a week, followed by a month of reimplementing the existing system. It wasn't until many months later I realised that the reason everything was suddenly so much easier was because we had monadic dependencies.

While people at Standard Chartered wanted to open source Shake, that turned out not to be possible. A number of people in the Haskell community implemented their own versions of Shake, but none were as polished or as strong as our internal one. Eventually, I reimplemented Shake, from scratch, in my spare time. Writing Shake from scratch, without the original source code or documentation, it naturally turned out better than the first attempt. A while later Standard Chartered migrated to the open-source version.

I still maintain Shake, but am fortunate to have [other contributors](https://github.com/ndmitchell/shake/graphs) extending and improving Shake. If you want to join in, see [notes for developers](Developing.md#readme).
