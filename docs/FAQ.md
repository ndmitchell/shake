# FAQ

#### Q: Where are functions for trimming lines?

Shake is a Haskell package focused on providing build-system functionality. Since Shake scripts are written in Haskell, they can easily access other Haskell packages. Most general needs are met by the standard [`base` library](https://hackage.haskell.org/package/base), but a few other useful general functions can be found in [the `extra` library](https://hackage.haskell.org/package/extra), including `trim`. For more specific functionality (e.g. parsing, databases, JSON) find a suitable Haskell library and use that.

#### Q: Is Shake limited to building Haskell?

Not at all - Shake can build any project in any combination of languages. In fact, Shake isn't typically necessary for vanilla Haskell projects, as you can use [`cabal`](https://haskell.org/cabal) or [`stack`](https://stackage.org/). Shake is often used for building C/C++, Docker containers and Javascript/HTML/CSS projects.
