# Philosophy and History

This page gives the background philosophy behind Shake today, and also some of the history. The information is intended to be interesting, but not necessarily useful.

## Philosophy

Shake is about giving you lots of power. The idea is we don't want to restrict you, so you can express the things you need directly and clearly. Hopefully by expressing things directly, you have less distance between what you were aiming for and what you produce, and the result is more robust.

On top of that power, we want a fast build system. Expressing things clearly allows you to get at the speed if you need it, but there are some things that aren't possible because they would hurt the speed. Hopefully, not too much.

On top of that, you want something which is well engineered and thoroughly tested. We do that with a large test suite, large real-world projects.

## History

The Yhc project died because we had to write 10,000 lines of Python build code for SCons, it was awful. The project was about 10,000 line of Haskell (a compiler), 10,000 lines of C (a runtime system) and 10,000 lines of Python (a build system). The build system regularly went wrong.

For my thesis I needed a fairly simple build system, which you can see an early version of here: https://github.com/ndmitchell/thesis/blob/master/old/Main.hs. It's not a https://github.com/ndmitchell/thesis/blob/master/old/Main.hs#L71

When I arrived at Standard Chartered the build system was a large amount of Makefiles, so I rewrote it using Haskell. Initially there were just helper functions the build system called, but gradually these became abstracted. I eventually gave a talk at the Haskell Implementors Workshop.

While people at Standard Chartered wanted to open source Shake, that turned out not to be possible. A number of people implemented their own versions of Shake, but none were as polished or as strong as the internal one. Eventually, I reimplemented Shake, from scratch, in my spare time. The released version of Shake is that available from the new website.

Writing Shake from scratch, without the original source code or documentation, it naturally turned out better than the first. A while later Standard Chartered migrated to the open source version.

