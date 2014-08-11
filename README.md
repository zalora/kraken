kraken
------

This library allows to define and run so-called targets. Targets are used to
run operations to create files, database tables or similar things. Every target
has a name with which it can be addressed. Targets can depend on other targets.

`kraken` exposes functions to build a so-called target store which is essentially
a bunch of targets stored in a dependency graph. It also allows to create
executables for given target stores that expose decent command line interfaces.
These interfaces allow you to

  - list all targets
  - output the dependency graph in dot format
  - run specific targets while doing dependency resolution

See examples/example.hs for a simple introduction.
