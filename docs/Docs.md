# Shake Documentation

First [read the manual](Manual.md#readme), which also serves as a tutorial. After that, the following pages may be useful:

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

If you have any further questions:

* [Ask on StackOverflow](https://stackoverflow.com/questions/tagged/shake-build-system), using the tag `shake-build-system`.
* [Email us](https://groups.google.com/forum/?fromgroups#!forum/shake-build-system) for any questions/bugs/thoughts on Shake. If you need more information and aren't sure where to start, use the mailing list.
