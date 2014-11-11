# kraken

This library allows to define and run so-called `Target`s. `Target`s are used
to run operations to create certain outputs, e.g. files, database tables or
similar things. Every `Target` has a `TargetName` with which it can be addressed.

See examples/example.hs for a simple example.


## Monitors

`Target`s can have a so-called `Monitor` attached to it. The monitor is meant
to check whether the output of the corresponding `Target` is up-to-date or not.
By default, when a `Target` (that has a `Monitor`) is run, its `Monitor` will
be executed first. If the `Monitor` says everything is up-to-date, then the
`Target` operation is deemed unnecessary and won't be performed. If the
`Monitor` says the output is out-of-date, then the `Target` operation is run.
Afterwards, the `Monitor` is performed again (possibly with some cashing of the
input value, see `Kraken.ActionM.memoizeMonitorInput`) and if the output
appears to be up-to-date, the `Target` is considered to have run successfully.
If the output is still out-of-date, an error is thrown.

`Monitor`s behave like `Target`s (without their own `Monitor`s) and can be run,
listed, etc.


## Dependencies

`Target`s can depend on other targets. The dependencies a `Target` has are
given by `TargetName`s.

In the default mode, `kraken` ensures that dependencies of a `Target` have
successfully run before the `Target` itself is run. Typically a project using
`kraken` defines a number of `Target`s that form a directed acyclic dependency
graph.


## Target Store

`kraken` exposes functions to build a so-called target `Store` which is
essentially a bunch of targets stored in a dependency graph. Some static checks
on the dependency graph are performed while creating the `Store`, e.g. that
there are no dependency cycles.

`kraken` also supplies functions that are meant to create executables that
expose a `Store` through a command line interface (see `Kraken.Run.runAsMain`).
This executable then allows to

  - perform static checks on the store (and the supplied configuration files,
    if any),
  - list all targets,
  - output the dependency graph in dot format,
  - run specific targets while doing dependency resolution and
  - run a web server (coined `kraken` daemon) that exposes certain
    functionality through a REST API (see `Kraken.Daemon`). Currently the web
    server
    - returns the target graph under `/targetGraph`.

## kraken-web

This package also provides a web application called `kraken-web`. In its
configuration it has to be given a set of urls to running `kraken` daemons. It
then provides a human-accessible web interface to monitor and/or control the
`kraken` daemons.

(Both the `kraken` daemons and `kraken-web` are meant to provide much more
functionality in the future.)

![.](http://i.imgur.com/alTGSXS.jpg)
