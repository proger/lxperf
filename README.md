Linux Kit
=========

Because Linux introspection sucks.

* `pi $pid` - get process `$pid` info using pstack, systack and lsof
* `presolve $str` - resolve pid and executable by matching args
* `pstack6 $pid` - get process user stack using GDB 6.3 (ugly but portable)
* `pstack7 $pid` - get process user stack using GDB 7+
* `systack $pid` - strace a little and get per-process kernel stack
* `timeout X command` - coreutils/timeout bash-based shim for distributions from dinosaur sage
* `poorprof $pid` - poor man's profiler (http://poormansprofiler.org/), nicely collapses stacks
