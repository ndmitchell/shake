# Profiling and optimisation

_FIXME: This page is a list of random thoughts that need collecting into a coherent whole._

After you've written the build system, and it's correct, a very natural instinct is to make it faster. This page details how to do that with Shake.

Before you start, make sure you are passing suitable `-j` flags to get the necessary parallelism. Also note that there is the zero build (nothing to do), and full rebuilds. You may find full rebuilds vary because Shake uses random scheduling.

## Viewing interactive progress

I would also suggest more tracing. Perhaps add a putStrLn before the alwaysRerun.

I'm using a windowing manager which doesn't have titlebars, so I can't actually easily see asynchronous progress messages that Shake is configured to do by default. I'd like to have Shake output progress to the command line as it handles "big" commands, but there does not seem to be any good way to actually do this. I can get Progress, of course (on HEAD) but I have to then manually format it myself, which is a chore.

The fact that Shake prints its async progress messages to the titlebar is entirely controlled by https://hackage.haskell.org/package/shake-0.15.5/docs/Development-Shake.html#v:shakeProgress, which defaults to:

progressSimple :: IO Progress -> IO ()
progressSimple p = do
    program <- progressProgram
    progressDisplay 5 (\s -> progressTitlebar s >> program s) p
How do you mean output to the command line? Have you considered having a separate status window where Shake can put this output? Overwriting progressSimple to instead use:

 progressDisplay 5 print
Might be sufficient.

Tagging this as website, since the website could do with a whole page detailing options for progress, how it works etc.


If it's going to be asynchronous to the command line, then a better aesthetic is for it to "overwrite the old status with a new status" terminal stuff. But that's pretty nontrivial to do if you also want to support other diagnostic output.

But, I guess what I could do is put the (asynchronously) passed in string to an MVar and then read it out when I want to give an update...
 @ndmitchell
     Owner
ndmitchell commented on Dec 20, 2015
Stack does something quite nice - the last line is always the "progress", and then the diagnostics scroll above that, which is quite easy to implement. Writing the output async into an MVar isn't a bad plan.
 @ezyang
     
ezyang commented on Dec 20, 2015
Yes, but Shake needs to play ball: for example, the tracing functions need to be responsible for clearing out the bar, and then redrawing it after you've put in the diagnostic. It's not too difficult to implement but you need cooperation from the messagers.
 @ndmitchell
     Owner
ndmitchell commented on Dec 20, 2015
Shake only writes messages via https://hackage.haskell.org/package/shake-0.15.5/docs/Development-Shake.html#v:shakeOutput - so you should be able to do that yourself. Some people have changed that function to write to a web server, rather than a console.
 @ezyang
     
ezyang commented on Dec 20, 2015
Nifty. OK, that should be enough for my use case. Closed!
 @ezyangezyang closed this on Dec 20, 2015
 @ndmitchellndmitchell changed the title from Expose some formatting functions for Progress to Add website page detailing progress options and tricks on Dec 20, 2015
@ndmitchell
     Owner
ndmitchell commented on Dec 20, 2015
This ticket seems to have turned into a list of neat tricks and explanations for progress reporting. It should be collected with other information into a web page.


## Shake timing

Add `--timing` to see where the time is going.

if you print a line at the very beginning of your build script in main, how long does it take to show up?

It's worthwhile to check the database size, make sure its not too huge.

## Shake profiling

Shake features an advanced profiling feature. To build with profiling run `build --report`, which will generate an interactive HTML profile named `report.html`. This report lets you examine what happened in that run, what takes most time to run, what rules depend on what etc. There is a help page included in the profiling output, and a [profiling tutorial/demo](https://cdn.rawgit.com/ndmitchell/shake/35fbe03c8d3bafeae17b58af89497ff3fdd54b22/html/demo.html).

To view profiling information for the _previous_ build, you can run `build --no-build --report`. This feature is useful if you have a build execution where a file unexpectedly rebuilds, you can generate a profiling report afterwards and see why. To generate a lightweight report (about 5 lines) printed to the console run `build --report=-`.

Given the time is in want, you may get more out of Shake profiling. Pass --profile=- to get a brief profile summary, and --profile=profile.html to generate a file listing what Shake is doing. Also doing --verbose (up to 3 times) will give you increasingly detailed levels of what Shake is doing. That should let you find out if something is running, or is paused and waiting.


## Haskell profiling

generate a Haskell profile.

You can use the normal Haskell profiler

You may find that you get better profiling without -I0 - it's certainly a flag that has messed up profiling in some versions of GHC. Generally it makes Shake performance better to include it, but for profiling it doesn't matter too much.
