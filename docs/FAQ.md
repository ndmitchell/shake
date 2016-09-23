# FAQ

#### Q: What else is on this website?

* [User manual](Manual.md) -- the place to start, which also serves as a tutorial.
* [Why](Why.md) -- details on why you should use Shake, for those who are easily influenced.
* [Includes](Includes.md) -- how to deal with `#include` files, import statements and other dependencies between files.
* [Profiling and optimisation](Profiling.md) -- how to speed up an existing Shake build system.
* [Command line flags](CommandLine.md) -- the flags and settings supported by Shake, a better version of `--help`.
* [Developing Shake](Developing.md) -- notes for people who want to contribute to Shake itself.
* [Ninja](Ninja.md) -- features of Shake for those people who use Ninja.

<!--
Shake is suitable for all sizes of build systems, from a simple C project to a huge cross-platform multi-language project. However, at different scales, different techniques tend to be applicable.
* [Small/simple build systems](Small.md#readme) -- some simpler build systems can be written as _forward_ build systems, without the need to explicitly think about dependencies or targets. Useful for getting started, relies on a tool to automatically track your dependencies.
* [Large frequently changing build systems](Large.md#readme) -- for large build systems, it is useful to split the build system interpreter and metadata apart, making changes to the Haskell build system comparatively rare.
-->

#### Q: Any more documentation?

There is a complete list of [every function in Shake](https://hackage.haskell.org/packages/archive/shake/latest/doc/html/Development-Shake.html) which [can be searched](http://hoogle.haskell.org/?package=shake). Each function comes with documentation and examples.

Much of the theory behind Shake is covered in [a conference paper](http://ndmitchell.com/downloads/paper-shake_before_building-10_sep_2012.pdf) which was accompanied by [this video](https://www.youtube.com/watch?v=xYCPpXVlqFM) ([slides](http://ndmitchell.com/downloads/slides-shake_before_building-10_sep_2012.pdf)). Since then I've given videoed talks on [small worked examples](http://www.infoq.com/presentations/shake) ([slides](http://ndmitchell.com/downloads/slides-building_stuff_with_shake-20_nov_2014.pdf)) and [how to structure large Shake systems](https://skillsmatter.com/skillscasts/6548-defining-your-own-build-system-with-shake) ([slides](http://ndmitchell.com/downloads/slides-defining_your_own_build_system_with_shake-09_oct_2015.pdf)).

I sometimes write about ongoing development work or other Shake-related things on [my blog](http://neilmitchell.blogspot.co.uk/search/label/shake).

If you have any further questions:

* [Ask on StackOverflow](https://stackoverflow.com/questions/tagged/shake-build-system), using the tag `shake-build-system`.
* [Email us](https://groups.google.com/forum/?fromgroups#!forum/shake-build-system) for any questions/bugs/thoughts on Shake. If you need more information and aren't sure where to start, use the mailing list.


#### Q: Is Shake limited to building Haskell?

Not at all -- Shake can build any project in any combination of languages. In fact, Shake isn't typically necessary for vanilla Haskell projects, as you can use [`cabal`](https://haskell.org/cabal) or [`stack`](http://haskellstack.org/). Shake is often used for building C/C++, Docker containers and Javascript/HTML/CSS projects.

#### Q: Where are functions for string manipulation?

Shake is a Haskell package focused on providing build-system functionality. Since Shake scripts are written in Haskell, they can easily access other Haskell packages. Most general needs are met by the standard [`base` library](https://hackage.haskell.org/package/base), but a few other useful general functions can be found in [the `extra` library](https://hackage.haskell.org/package/extra) (e.g. [`trim`](https://hackage.haskell.org/package/extra/docs/Data-List-Extra.html#v:trim) and [`replace`](https://hackage.haskell.org/package/extra/docs/Data-List-Extra.html#v:replace)). For more specific functionality (e.g. parsing, databases, JSON) find a [suitable Haskell library](https://hackage.haskell.org/packages) and use that.

#### Q: Why is there a `shake` executable?

Most users will write their own Haskell file and compile it to produce an executable that is their build tool. The `shake` executable is there to [run the demo](Demo.md), run [Ninja build files](Ninja.md) and will also run a `Shakefile.hs` if present.

#### Q: Can file patterns overlap?

No. If two patterns overlap for a file being built it will result in a runtime error -- you cannot have a pattern for `*.txt`, and another for `foo.*`, and then build a file named `foo.txt`. For objects that typically share the same extension (e.g. C and Haskell both produce `.o` objects), either disambiguate with a different extension (e.g. `.c.o` and `.hs.o`), or different directory (e.g. `obj/c/**/.o` and `obj/hs/**/.o`). For more information, including ways to enable overlap and set priorities, see `%>`.

#### Q: Why do multiple calls to `need` run sequentially? Are `Applicative` actions run in parallel?

In Shake, `need xs >> need ys` will build `xs` in parallel, then afterwards build `ys` in parallel. The same is true of `need xs *> need ys`, where `*>` is the applicative equivalent of `>>`. In contrast, [Haxl](https://hackage.haskell.org/package/haxl) will execute both arguments to `*>` in parallel. For Shake, you are encouraged to merge adjacent `need` operations (e.g. `need (xs++ys)`), and where that is not possible (e.g. when using `askOracle`) use `parallel` explicitly.

Shake _could_ follow the Haxl approach, but does not, mainly because they are targeting different problems. In Haxl, the operations are typically read-only, and any single step is likely to involve lots of operations. In contrast, with Shake the operations definitely change the file system, and there are typically only one or two per rule. Consequently, Shake opts for an explicit approach, rather than allow users to use `*>` (and then inevitably add a comment because its an unusual thing to do).

#### Q: How can I depend on directories?

Think of directories as containers for files. They exist or don't pretty randomly, but if they have files, they must exist. In particular, you can't depend on a directory with `need` or write a rule to create a directory. Directories are created as needed -- the rule for `bar/baz.exe` will create the `bar` directory if necessary. If you want to depend on a `git clone` having being performed, depend on a particular checked-out file instead (e.g. `README.md`), with the rule to create it being `git clone`.

There is a tracked function `doesDirectoryExist`, to depend on the presence or absence of a directory, but you should not call it on directories which might be created by the build system.

#### Q: What's the history of Shake?

I ([Neil Mitchell](http://ndmitchell.com)) was one of the people behind the [Yhc project](https://www.haskell.org/haskellwiki/Yhc), a Haskell compiler that died in a large part because of its build system. To quote from [the final blog post](http://yhc06.blogspot.co.uk/2011/04/yhc-is-dead.html):

> The biggest challenge for Yhc was the build system -- we ended up with 10,000 lines of Python Scons scripts. Without a robust build system nothing else matters. When our sole Python hacker left the team that was the beginning of the end.

A Haskell compiler is a big undertaking, but the build system for a simple Haskell compiler shouldn't be that complicated.

When writing my thesis I needed a build system, and decided to try writing a simple Haskell DSL, which is still online [here](https://github.com/ndmitchell/thesis/blob/master/old/Main.hs). I defined a single operator [`<==`](https://github.com/ndmitchell/thesis/blob/master/old/Main.hs#L71) which let me express a relationship between an output and its dependencies -- very simple, but it worked.

Later I moved to [Standard Chartered](https://www.sc.com/), where the build system was a mass of Makefiles, and it quickly became apparent that the project had outgrown the current approach. Without really surveying the alternatives, I decided that a Haskell DSL would be easiest to fit in with the existing infrastructure, so started writing some code. The first version of the build library took under a week, followed by a month of reimplementing the existing system. It wasn't until many months later I realised that the reason everything was suddenly so much easier was because we had monadic dependencies.

While people at Standard Chartered wanted to open source Shake, that turned out not to be possible. A number of people in the Haskell community implemented their own versions of Shake, but none were as polished or as strong as our internal one. Eventually, I reimplemented Shake, from scratch, in my spare time. Writing Shake from scratch, without the original source code or documentation, it naturally turned out better than the first attempt. A while later Standard Chartered migrated to the open-source version.

I still maintain Shake, but am fortunate to have [other contributors](https://github.com/ndmitchell/shake/graphs) extending and improving Shake. If you want to join in, see [notes for developers](Developing.md).
