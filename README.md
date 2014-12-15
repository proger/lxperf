lxperf
======

Because Linux introspection sucks.
Things dirty enough not to be PR-ed to [perf-tools](https://github.com/brendangregg/perf-tools).

* `pi $pid` - get process `$pid` info using pstack, systack and lsof
* `presolve $str` - resolve pid and executable by matching args
* `pstack6 $pid` - get process user stack using GDB 6.3 (ugly but portable)
* `pstack7 $pid` - get process user stack using GDB 7+
* `systack $pid` - strace a little and get per-process kernel stack
* `timeout X command` - coreutils/timeout bash-based shim for distributions from dinosaurs age
* `poorprof $pid` - poor man's profiler (http://poormansprofiler.org/), nicely collapses stacks

SystemTap scripts:

* [stap/sigmon.stp](stap/sigmon.stp) - monitoring signals (also see [killsnoop](https://github.com/brendangregg/perf-tools/blob/master/killsnoop))

Perf scripts:

* [perf/sysfailed](perf/sysfailed) - trace failing syscalls and their exit codes
