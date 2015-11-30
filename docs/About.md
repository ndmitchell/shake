# Background information

This page gives a technical summary of Shake, some of the philosophy behind Shake today, and also some of the history. The information might be interesting, but not necessarily useful.

## Technical Details

Shake build systems are Haskell programs that make heavy use of the `shake` library. While Shake build systems are Haskell programs, they can be treated as a powerful version of make with slightly funny syntax. The build system requires no significant Haskell knowledge, and is designed so that most features are accessible by learning the "Shake syntax", without any appreciation of what the underlying Haskell means. The main technical innovations over other build systems are:

* A Haskell domain-specific language, meaning you can reuse the Haskell packaging/abstraction mechanisms.
* [Monadic dependencies](http://neilmitchell.blogspot.co.uk/2014/07/applicative-vs-monadic-build-systems.html), allowing new dependencies to be discovered after examining previous dependencies.
* Polymorphic dependencies, allowing dependencies on things other than files.
* Dependencies that rebuild but don't change don't cause more things to rebuild.

Much of the theory behind Shake is covered in [a conference paper](http://community.haskell.org/~ndm/downloads/paper-shake_before_building-10_sep_2012.pdf) which was accompanied by the following video:

<center>
<iframe width="420" height="315" src="http://www.youtube.com/embed/xYCPpXVlqFM" frameborder="0" allowfullscreen>
</iframe>
</center>

## Philosophy

Shake is designed to give the user lots of power. The idea is to make it easy to express things directly and clearly, without having to "encode" the problem in a way the build system can understand. Hopefully by expressing things directly, the result is more likely to do what you expect, and consequently be more robust.

On top of that power, we want build systems to be fast. The hope is that expressing things clearly lets the build system access all the information and thus go faster, but sometimes that doesn't work out. Where speed and power are in conflict, we try and tread a delicate line.

Finally, a lot of attention has been given to the engineering and code quality. Shake is well engineered, with a comprehensive test suite and good test coverage.

## History

I ([Neil Mitchell](http://ndmitchell.com)) was one of the people behind the [Yhc project](https://www.haskell.org/haskellwiki/Yhc), a Haskell compiler that died in a large part because of its build system. To quote from [the final blog post](http://yhc06.blogspot.co.uk/2011/04/yhc-is-dead.html):

> The biggest challenge for Yhc was the build system - we ended up with 10,000 lines of Python Scons scripts. Without a robust build system nothing else matters. When our sole Python hacker left the team that was the beginning of the end.

A Haskell compiler is a big undertaking, but the build system for a simple Haskell compiler shouldn't be that complicated.

When writing my thesis I needed a build system, and decided to try writing a simple Haskell DSL, which is still online [here](https://github.com/ndmitchell/thesis/blob/master/old/Main.hs). I defined a single operator [`<==`](https://github.com/ndmitchell/thesis/blob/master/old/Main.hs#L71) which let me express a relationship between an output and its dependencies - very simple, but it worked.

Later I moved to [Standard Chartered](http://www.sc.com/), where the build system was a mass of Makefiles, and it quickly became apparent that the project had outgrown the current approach. Without really surveying the alternatives, I decided that a Haskell DSL would be easiest to fit in with the existing infrastructure, so started writing some code. The first version of the build library took under a week, followed by a month of reimplementing the existing system. It wasn't until many months later I realised that the reason everything was suddenly so much easier was because we had monadic dependencies.

While people at Standard Chartered wanted to open source Shake, that turned out not to be possible. A number of people in the Haskell community implemented their own versions of Shake, but none were as polished or as strong as our internal one. Eventually, I reimplemented Shake, from scratch, in my spare time. Writing Shake from scratch, without the original source code or documentation, it naturally turned out better than the first attempt. A while later Standard Chartered migrated to the open-source version.

I still maintain Shake, but am fortunate to have [other contributors](https://github.com/ndmitchell/shake/graphs) extending and improving Shake. If you want to join in, see [notes for developers](Developing.md#readme).
