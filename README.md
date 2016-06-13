Preliminary note for Windows users
==================================

The build instructions for `gprbuild` may have a slight UNIX flavor but they can
be used on Windows platforms with a full Cygwin installation. The latter makes
it simpler to build `gprbuild` but is not required to use it.

Configuring
===========

Configuring is usually done simply as:

    $ ./configure

Two parameters may be worth specifying: `--prefix` for specifying the
installation root and `--build` for specifying the build host.

In particular, on Windows, when using cygwin to build, it is necessary to
configure with `--build=i686-pc-mingw32` if one wants to use 32 bit mingw based
compilers such as GNAT Pro or GNAT GPL, and with `--build=x86_64-pc-mingw32` for
64 bit compilers. Here are examples of such commands:

    $ ./configure --build=i686-pc-mingw32 --prefix=$HOME/local

    $ ./configure --build=x86_64-pc-mingw32 --prefix=$HOME/local

Building and Installing
=======================

XML/Ada must be installed before building.

Building the main executables is done simply with:

    $ make all

When compiling, you can choose whether you want to link statically with XML/Ada
(the default), or dynamically. To compile dynamically, you should run:

    $ make LIBRARY_TYPE=relocatable all

instead of the above.

Installation is done with:

    $ make install

Doc & Examples
==============

The documentation is provided in various formats in the doc subdirectory.

It refers to concrete examples that are to be found in the examples
subdirectory. Each example can be built easily using the simple attached
Makefile:

    $ make all    # build the example
    $ make run    # run the executable(s)
    $ make clean  # cleanup

All the examples can be `built/run/cleaned` using the same targets and the top
level examples Makefile.
