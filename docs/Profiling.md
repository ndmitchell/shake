# Shake Manual

#### Profiling

Shake features an advanced profiling feature. To build with profiling run `build --report`, which will generate an interactive HTML profile named `report.html`. This report lets you examine what happened in that run, what takes most time to run, what rules depend on what etc. There is a help page included in the profiling output, and a [profiling tutorial/demo](https://cdn.rawgit.com/ndmitchell/shake/35fbe03c8d3bafeae17b58af89497ff3fdd54b22/html/demo.html).

To view profiling information for the _previous_ build, you can run `build --no-build --report`. This feature is useful if you have a build execution where a file unexpectedly rebuilds, you can generate a profiling report afterwards and see why. To generate a lightweight report (about 5 lines) printed to the console run `build --report=-`. 
