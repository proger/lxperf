lxperf
======

Because default Linux introspection sucks.
Things dirty enough not to be PR-ed to [perf-tools](https://github.com/brendangregg/perf-tools).

For Darwin/OS X see [darwinkit](http://github.com/proger/darwinkit).

* `pi $pid` - get process `$pid` info using pstack, systack and lsof
* `presolve $str` - resolve pid and executable by matching args
* `pstack6 $pid` - get process user stack using GDB 6.3 (ugly but portable)
* `pstack7 $pid` - get process user stack using GDB 7+
* `systack $pid` - strace a little and get per-process kernel stack
* `timeout X command` - coreutils/timeout bash-based shim for distributions from dinosaurs age
* `poorprof $pid` - poor man's profiler (http://poormansprofiler.org/), nicely collapses stacks

Stats:

* [connstat](connstat.hs) - samples the count of established connections per process name (It used to be a bash/awk script but I fed up. Compiled with ghc 7.8.3, make sure you `cabal install attoparsec boxes`)

SystemTap scripts:

* [stap/sigmon.stp](stap/sigmon.stp) - monitoring signals (also see [killsnoop](https://github.com/brendangregg/perf-tools/blob/master/killsnoop))

Perf scripts:

* [perf/sysfailed](perf/sysfailed) - trace failing syscalls and their exit codes

## perf_events on ubuntu

Get `perf(1)`:

```bash
apt-get install linux-tools-common linux-cloud-tools-generic \
    linux-tools-$(uname -r) linux-cloud-tools-$(uname -r)
```

Get debuginfo (source: [Ubuntu: getting kernel debuginfo](https://wiki.ubuntu.com/Kernel/Systemtap#Where_to_get_debug_symbols_for_kernel_X.3F)):

```bash
codename=$(lsb_release -c | awk  '{print $2}')
sudo tee /etc/apt/sources.list.d/ddebs.list << EOF
deb http://ddebs.ubuntu.com/ ${codename}      main restricted universe multiverse
deb http://ddebs.ubuntu.com/ ${codename}-security main restricted universe multiverse
deb http://ddebs.ubuntu.com/ ${codename}-updates  main restricted universe multiverse
deb http://ddebs.ubuntu.com/ ${codename}-proposed main restricted universe multiverse
EOF

sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys ECDCAD72428D7C01
sudo apt-get update
sudo apt-get install linux-image-$(uname -r)-dbgsym
```


Test debuginfo:

```bash
perf probe \
    --vmlinux "$(dpkg -L linux-image-$(uname -r)-dbgsym | grep vmlinux)" \
    -nv 'tcp_sendmsg size'
```
