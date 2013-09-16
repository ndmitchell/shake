# Why choose Shake?

Shake is a library for writing build systems. Most large projects have a custom-written build system, and developers working on the project are likely to run the build system many times a day, spending a noticable amount of time [waiting for the build system](http://xkcd.com/303/). Why might you pick Shake over alternative tools for writing build systems (e.g. make, Ant, Scons)? 

* Easier and faster for developers running the build system - Shake based build systems run quickly, require little manual intervention and report estimated completion time as they go.
* Easier and faster for developers writing the build system - Shake provides a powerful language for writing build systems, has excellent support for writing large robust build systems, can express many types of build rules and provides profiling information to help speed up builds.

In the rest of this document we explain and justify the above claims. Shake combines [cutting edge research](http://community.haskell.org/~ndm/downloads/paper-shake_before_building-10_sep_2012.pdf) with a [robust industrial-quality implementation](http://hackage.haskell.org/packages/shake/). Shake is in constant use at many large organisations, including [Standard Chartered](http://sc.com/), where is was originally developed and has been in use since 2009.

## Expresses many types of build rule

All build systems are based around the notion of expressing dependencies, and many of the advantages of Shake are a direct result of Shake having more powerful dependencies than anything else. In particular Shake does not have a fixed list of dependencies at the start of a build, but can add additional dependencies as a result of existing Shake rules. This distinction makes it much easier to express things.

Shake also provides parameterisable dependencies, instead of just allowing dependencies between files. Shake can introduce real dependencies on the existance of files, environment variables, directory contents, and you can even write your own. The advanced system ensures you can depend on whatever you want, without trying to shoehorn your build into files.


 Many of the advantages of Shake are a direct result of being able to express 


Many of the advantages of Shake fall out from being able to express dependencies in a very fine-grained level. 

The rules can be very powerful, expressing things very specifically.

## Build systems run quickly

The implementation is optimised.

Runs on multiple threads (common to most build systems), but has support for respecting other machine resources.

Low overhead zero build.

Builds with generated files go massively faster with special techniques for optimising.

## Build systems run reliably

The design 

## Requires little manual intervention

Since many more things can be trakced it can do things automatically. No need for manual intervention. This reduces the number of times the build system developers need to communicate to the build system users, and simplifies and eliminates potential errors.

## Reports estimated completion time

Shake can display both the amount of time left (in minutes and second) and the percentage completion. Such information cab help your developers plan better.

## Powerful language

Shake is implemented as a Haskell library.

## Supports large robust systems

Shake does not provide any support for large build systems, relying on a robust and huge language of Haskell.

Provides lint checking to do many sanity checks of the build system, catching errors sooner.

## Provides profiling information

Provides information about the dependencies, dependency graph, what takes most time etc. Allows developers to understand the build system as run, debug anomolies, and figure out how to make things go faster.
