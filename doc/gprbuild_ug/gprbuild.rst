.. _GPRbuild:

********
GPRbuild
********

`GPRbuild` is a generic build tool designed for the construction of
large multi-language systems organized into subsystems and libraries.
It is well-suited for compiled languages supporting separate compilation,
such as Ada, C, C++ and Fortran.

`GPRbuild` manages a three step build process.

* compilation phase:

  Each compilation unit of each subsystem is examined in turn, checked for
  consistency, and compiled or recompiled when necessary by the appropriate
  compiler.  The recompilation decision is based on dependency information
  that is typically produced by a previous compilation.

* post-compilation phase (or binding):

  Compiled units from a given language are passed to a language-specific
  post-compilation tool if any. Also during this phase
  objects are grouped into static or dynamic libraries as specified.

* linking phase:

  All units or libraries from all subsystems are passed to a linker tool
  specific to the set of toolchains being used.


The tool is generic in that it provides, when possible, equivalent
build capabilities for all supported languages. For this, it uses a
configuration file :file:`<file>.cgpr` that has a syntax and structure very
similar to a project file, but which defines the characteristics
of the supported languages and toolchains. The configuration file contains
information such as:

* the default source naming conventions for each language,
* the compiler name, location and required options,
* how to compute inter-unit dependencies,
* how to build static or dynamic libraries,
* which post-compilation actions are needed,
* how to link together units from different languages.


On the other hand, `GPRbuild` is not a replacement for general-purpose
build tools such as `make` or `ant` which give the user a high
level of control over the build process itself. When building a system
requires complex actions that do not fit well in the three-phase process
described above, `GPRbuild` might not be sufficient.
In such situations, `GPRbuild` can still
be used to manage the appropriate part of the build. For
instance it can be called from within a Makefile.

.. _Building_with_GPRbuild:

Building with GPRbuild
======================


.. _Command_Line:

Command Line
------------

Three elements can optionally be specified on GPRbuild's command line:

* the main project file,
* the switches for GPRbuild itself or for the tools it
  drives, and
* the main source files.

The general syntax is thus:

::

   gprbuild [<proj>.gpr] [switches] [names]
    {[-cargs opts] [-cargs:lang opts] [-largs opts] [-gargs opts]}


GPRbuild requires a project file, which may be specified on the
command line either directly or through the :samp:`-P` switch. If not
specified, GPRbuild uses the project file :file:`default.gpr` if there
is one in the current working directory. Otherwise, if there is only
one project file in the current working directory, GPRbuild uses this
project file.

Main source files represent the sources to be used as the main
programs. If they are not specified on the command line, GPRbuild uses
the source files specified with the `Main` attribute in the project
file. If none exists, then no executable will be built.
It is also possible to specify absolute file names, or file names relative
to the current directory.

When source files are specified along with the option :samp:`-c`, then
recompilation will be considered only for those source files. In all
other cases, GPRbuild compiles or recompiles all sources in the
project tree that are not up to date, and builds or rebuilds libraries
that are not up to date.

If invoked without the :samp:`--config=` or
:samp:`--autoconf=` options, then GPRbuild will look for a configuration
project file. The file name or path name of this configuration project file
depends on the target, the runtime and environment variable `GPR_CONFIG`
See :ref:`Configuring_with_GPRconfig`. If there is no such file in the default
locations expected by GPRbuild (<install>/share/gpr and the current
directory) then GPRbuild will invoke GPRconfig with
the languages from the project files, and create a configuration project
file :file:`auto.cgpr` in the object directory of the main project. The project
:file:`auto.cgpr` will be rebuilt at each GPRbuild invocation unless you use
the switch :samp:`--autoconf=path/auto.cgpr`, which will use the configuration
project file if it exists and create it otherwise.

Options given on the GPRbuild command line may be passed along to
individual tools by preceding them with one of the "command line separators"
shown below. Options following the separator, up to the
next separator (or end of the command line), are passed along.  The
different command line separators are:

* :samp:`-cargs`

  The arguments that follow up to the next command line separator are
  options for all compilers for all languages.
  Example: :samp:`-cargs` :samp:`-g`

* :samp:`-cargs:{language name}`

  The arguments that follow up to the next command line separator are
  options for the compiler of the specific language.

  Examples:

  * :samp:`-cargs:Ada -gnatf`
  * :samp:`-cargs:C -E`

* :samp:`-bargs`

  The arguments that follow up to the next command line separator are
  options for all binder drivers.

* :samp:`-bargs:{language name}`

  The arguments that follow up to the next command line separators are
  options for the binder driver of the specific language.

  Examples:

  * :samp:`-bargs:Ada binder_prefix=ppc-elf`
  * :samp:`-bargs:C++ c_compiler_name=ccppc`

* :samp:`-largs`

  The arguments that follow up to the next command line separator are
  options for the linker, when linking an executable.

* :samp:`-gargs`

  The arguments that follow up to the next command line separator are
  options for GPRbuild itself. Usually :samp:`-gargs` is specified after one or
  several other command line separators.

* :samp:`-margs`

  Equivalent to :samp:`-gargs`, provided for compatibility with
  *gnatmake*.

.. _Switches:

Switches
--------

GPRbuild takes into account switches that may be specified on the command
line or in attributes Switches(<main or language>) or Default_Switches
(<language>) in package Builder of the main project.

When there are a single main (specified on the command line or in
attribute Main in the main project), the switches that are taken into account
in package Builder of the main project are Switches (<main>), if declared, or
Switches (<language of main>), if declared.

When there are several mains, if there are sources of the same language, then
Switches (<language of main>) is taken into account, if specified.

When there are no main specified, if there is only one compiled language
(that is a language with a non empty Compiler Driver), then
Switches (<single language>) is taken into account, if specified.

The switches that are interpreted directly by GPRbuild are listed below.

First, the switches that may be specified only on the command line, but not in
package Builder of the main project:

* :samp:`--complete-output`

  This switch is not compatible with :samp:`--distributed=`.

  When this switch is specified, the standard output and the standard error of
  the compilations are redirected to different text files. When a source is up to
  date, if such text files exist, their contents are send to standard output
  and standard error. This allows to redisplay any warning or info from the
  last invocation of gprbuild --complete-output.

* :samp:`--distributed[={slave1}[,{slave2}]]`

  Activate the distributed compilation on the listed slaves nodes (IP or
  name). Or if no slave are specified they are search in `GPR_SLAVES` or
  `GPR_SLAVES_FILE` environment variables.
  see :ref:`Distributed_compilation`.

* :samp:`--slave-env={name}`

  Use name as the slave's environment directory instead of the default one.
  This options is only used in distributed mode.

* :samp:`--version`

  Display information about GPRbuild: version, origin and legal status, then
  exit successfully, ignoring other options.

* :samp:`--help`

  Display GPRbuild usage, then exit successfully, ignoring other options.

* :samp:`--display-paths`

  Display two lines: the configuration project file search path and the user
  project file search path, then exit successfully, ignoring other options.

* :samp:`--config={config project file name}`

  This specifies the configuration project file name. By default, the
  configuration project file name is :file:`default.cgpr`. Option :samp:`--config=`
  cannot be specified more than once. The configuration project file specified
  with :samp:`--config=` must exist.

* :samp:`--autoconf={config project file name}`

  This specifies a configuration project file name that already exists
  or will be created automatically. Option :samp:`--autoconf=` cannot
  be specified more than once. If the configuration project file
  specified with :samp:`--autoconf=` exists, then it is
  used. Otherwise, GPRconfig is invoked to create it automatically.

* :samp:`--target={targetname}`

  This specifies that the default configuration project file is
  :file:`<targetname>.cgpr`. If no configuration project file with this name
  is found, then GPRconfig is invoked with option
  :samp:`--target={targetname}` to create a configuration project file
  :file:`auto.cgpr`.

  Note: only one of :samp:`--config`, :samp:`--autoconf` or :samp:`--target=`
  can be specified.

* :samp:`--subdirs={subdir}`

  This indicates that the real directories (except the source directories) are
  subdirectories of the directories specified in the project files. This applies
  in particular to object directories, library directories and exec directories.
  If the directories do not exist, they are created automatically.

* :samp:`--relocate-build-tree[={dir}]`

  With this option it is possible to achieve out-of-tree build. That
  is, real object, library or exec directories are relocated to the
  current working directory or dir if specificed.

* :samp:`--root-dir={dir}`

  This option is to be used with --relocate-build-tree above and
  cannot be specified alone. This option specify the root directory
  for artifacts for proper relocation. The default value is the main
  project directory. This may not be suitable for relocation if for the
  example some artifact directories are in a directory upper. The
  specified directory must be a parent of all artifact directories.

* :samp:`--unchecked-shared-lib-imports`

  Allow shared library projects to import projects that are not shared
  library projects.

* :samp:`--source-info={source info file}`

  Specify a source info file. If the source info file is specified as a
  relative path, then it is relative to the object directory of the main
  project. If the source info file does not exist, then after the Project
  Manager has successfully parsed and processed the project files and found
  the sources, it creates the source info file. If the source info file
  already exists and can be read successfully, then the Project Manager will
  get all the needed information about the sources from the source info file
  and will not look for them. This reduces the time to process the project
  files, especially when looking for sources that take a long time. If the
  source info file exists but cannot be parsed successfully, the Project
  Manager will attempt to recreate it. If the Project Manager fails to create
  the source info file, a message is issued, but GPRbuild does not fail.

* :samp:`--restricted-to-languages={list of language names}`

  Restrict the sources to be compiled to one or several languages. Each
  language name in the list is separated from the next by a comma, without any
  space.

  Example: :samp:`--restricted-to-languages=Ada,C`

  When this switch is used, switches :samp:`-c`, :samp:`-b` and
  :samp:`-l` are ignored. Only the compilation phase is performed and the
  sources that are not in the list of restricted languages are not compiled,
  including mains specified in package Builder of the main project.

* :samp:`-aP {dir}` (Add directory :file:`dir` to project search path)

  Specify to GPRbuild to add directory :file:`dir` to the user project file search
  path, before the default directory.

* :samp:`-d` (Display progress)

  Display progress for each source, up to date or not, as a single
  line *completed x out of y (zz%)...*. If the file needs to be compiled
  this is displayed after the invocation of the compiler. These lines are
  displayed even in quiet output mode (switch :samp:`-q`).

* :samp:`-I{nn}` (Index of main unit in multi-unit source file)
  Indicate the index of the main unit in a multi-unit source file.
  The index must be a positive number and there should be one and only
  one main source file name on the command line.

* :samp:`-eL` (Follow symbolic links when processing project files)

  By default, symbolic links on project files are not taken into account
  when processing project files. Switch :samp:`-eL` changes this default
  behavior.

* :samp:`-eS` (no effect)

  This switch is only accepted for compatibility with gnatmake, but it has
  no effect. For gnatmake, it means: echo commands to standard output instead
  of standard error, but for gprbuild, commands are always echoed to standard
  output.

* :samp:`-F` (Full project path name in brief error messages)

  By default, in non verbose mode, when an error occurs while processing
  a project file, only the simple name of the project file is displayed in the
  error message. When switch :samp:`-F` is used, the full path of the project
  file is used. This switch has no effect when switch :samp:`-v` is used.

* :samp:`-o {name}` (Choose an alternate executable name)

  Specify the file name of the executable. Switch :samp:`-o` can
  be used only if there is exactly one executable being built;
  that is, there is exactly one main on the command line,
  or there are no mains on the command line and exactly one
  main in attribute `Main` of the main project.

* :samp:`-P {proj}` (use Project file *proj*)

  Specify the path name of the main project file. The space between :samp:`-P`
  and the project file name is optional. Specifying a project file name (with
  suffix :file:`.gpr`) may be used in place of option :samp:`-P`. Exactly one main
  project file can be specified.

* :samp:`-r` (Recursive)

  This switch has an effect only when :samp:`-c` or :samp:`-u` is also
  specified and there are no mains: it means that all sources of all projects
  need to be compiled or recompiled.

* :samp:`-u` (Unique compilation, only compile the given files)

  If there are sources specified on the command line, only compile these
  sources. If there are no sources specified on the command line, compile
  all the sources of the main project.

  In both cases, do not attempt the binding and the linking phases.

* :samp:`-U` (Compile all sources of all projects)

  If there are sources specified on the command line, only compile these
  sources. If there are no sources specified on the command line, compile
  all the sources of all the projects in the project tree.

  In both cases, do not attempt the binding and the linking phases.

* :samp:`-vP{x}` (Specify verbosity when parsing Project Files)

  By default, GPRbuild does not display anything when processing project files,
  except when there are errors. This default behavior is obtained with switch
  :samp:`-vP0`. Switches :samp:`-vP1` and :samp:`-vP2` yield increasingly
  detailed output.

* :samp:`-Xnm={val}` (Specify an external reference for Project Files)

  Specify an external reference that may be queried inside the project files
  using built-in function `external`. For example, with
  :samp:`-XBUILD=DEBUG`,
  `external("BUILD")` inside a project file will have the value
  `"DEBUG"`.

* :samp:`--compiler-subst={lang},{tool}` (Specify alternative compiler)

  Use *tool* for compiling files in language *lang*,
  instead of the normal compiler. For example, if
  :samp:`--compiler-subst=ada,my-compiler` is given, then Ada files
  will be compiled with *my-compiler* instead of the usual
  *gcc*. This and :samp:`--compiler-pkg-subst` are intended
  primarily for use by ASIS tools using :samp:`--incremental` mode.

* :samp:`--compiler-pkg-subst={pkg}` (Specify alternative package)

  Use the switches in project-file package *pkg* when running
  the compiler, instead of the ones in package Compiler.


Then, the switches that may be specified on the command line as well as in
package Builder of the main project (attribute Switches):

* :samp:`--create-map-file`

  When linking an executable, if supported by the platform, create a map file
  with the same name as the executable, but with suffix :file:`.map`.

* :samp:`--create-map-file={map file}`

  When linking an executable, if supported by the platform, create a map file
  with file name :file:`map file`.

* :samp:`--no-indirect-imports`

  This indicates that sources of a project should import only sources or
  header files from directly imported projects, that is those projects mentioned
  in a with clause and the projects they extend directly or indirectly.
  A check is done in the compilation phase, after a successful compilation, that
  the sources follow these restrictions. For Ada sources, the check is fully
  enforced. For non Ada sources, the check is partial, as in the dependency
  file there is no distinction between header files directly included and those
  indirectly included. The check will fail if there is no possibility that a
  header file in a non directly imported project could have been indirectly
  imported. If the check fails, the compilation artifacts (dependency file,
  object file, switches file) are deleted.

* :samp:`--indirect-imports`

  This indicates that sources of a project can import sources or header files
  from directly or indirectly imported projects. This is the default behavior.
  This switch is provided to cancel a previous switch
  :samp:`--no-indirect-imports` on the command line.

* :samp:`--no-object-check`

  Do not check if an object has been created after compilation.

* :samp:`--no-split-units`

  Forbid the sources of the same Ada unit to be in different projects.

* :samp:`--single-compile-per-obj-dir`

  Disallow several simultaneous compilations for the same object directory.

* :samp:`-b` (Bind only)

  Specify to GPRbuild that the post-compilation (or binding) phase is to be
  performed, but not the other phases unless they are specified by appropriate
  switches.

* :samp:`-c` (Compile only)

  Specify to GPRbuild that the compilation phase is to be performed, but not
  the other phases unless they are specified by appropriate switches.

* :samp:`-f` (Force recompilations)

  Force the complete processing of all phases (or of those explicitly specified)
  even when up to date.

* :samp:`-j{num}` (use *num* simultaneous compilation jobs)

  By default, GPRbuild invokes one compiler at a time. With switch :samp:`-j`,
  it is possible to instruct GPRbuild to spawn several simultaneous compilation
  jobs if needed. For example, :samp:`-j2` for two simultaneous compilation
  jobs or :samp:`-j4` for four. On a multi-processor system,
  :samp:`-j{num}` can greatly speed up the build process. If :samp:`-j0` is
  used, then the maximum number of simultaneous compilation jobs is the number
  of core processors on the platform.

  Switch :samp:`-j{num}` is also used to spawned several simultaneous binding
  processes and several simultaneous linking processes when there are several
  mains to be bound and/or linked.

* :samp:`-k` (Keep going after compilation errors)

  By default, GPRbuild stops spawning new compilation jobs at the first
  compilation failure. Using switch :samp:`-k`, it is possible to attempt to
  compile/recompile all the sources that are not up to date, even when some
  compilations failed. The post-compilation phase and the linking phase are never
  attempted if there are compilation failures, even when switch :samp:`-k` is
  used.

* :samp:`-l` (Link only)

  Specify to GPRbuild that the linking phase is to be performed, but not
  the other phases unless they are specified by appropriate switches.

* :samp:`-m` (Minimum Ada recompilation)

  Do not recompile Ada code if timestamps are different but checksums are the
  same.

* :samp:`-p` or :samp:`--create-missing-dirs` (Create missing object, library and exec directories)

  By default, GPRbuild checks that the object, library and exec directories
  specified in project files exist. Switch :samp:`-p` instructs GPRbuild to
  attempt to create missing directories. Note that these switches may be
  specified in package Builder of the main project, but they are useless there
  as either the directories already exist or the processing of the project
  files has failed before the evaluation of the Builder switches, because there
  is at least one missing directory.

* :samp:`-q` (Quiet output)

  Do not display anything except errors and progress (switch :samp:`-d`).
  Cancel any previous switch :samp:`-v`.

* :samp:`-R` (no run path option)

  Do not use a run path option to link executables or shared libraries,
  even when attribute Run_Path_Option is specified.

* :samp:`-s` (recompile if compilation switches have changed)

  By default, GPRbuild will not recompile a source if all dependencies are
  satisfied. Switch :samp:`-s` instructs GPRbuild to recompile sources when a
  different set of compilation switches has been used in the previous
  compilation, even if all dependencies are satisfied. Each time GPRbuild
  invokes a compiler, it writes a text file that lists the switches used in the
  invocation of the compiler, so that it can retrieve these switches if
  :samp:`-s` is used later.

* :samp:`-v` (Verbose output)

  Display full paths, all options used in spawned processes, and reasons why
  these processes are spawned. Cancel any previous switch :samp:`-q`.

* :samp:`-vl` (Verbose output, low level)

  Verbose output. Some verbose messages are not displayed.

* :samp:`-vm` (Verbose output, medium level)

  Verbose output. Some verbose messages may not be displayed.

* :samp:`-vh` (Verbose output, high level)

  Equivalent to :samp:`-v`.

* :samp:`-we` (Treat all warnings as errors)

  When :samp:`-we` is used, any warning during the processing of the project
  files becomes an error and GPRbuild does not attempt any of the phases.

* :samp:`-wn` (Treat warnings as warnings)

  Switch :samp:`-wn` may be used to restore the default after :samp:`-we` or
  :samp:`-ws`.

* :samp:`-ws` (Suppress all warnings)

  Do not generate any warnings while processing the project files.


Switches that are accepted for compatibility with gnatmake, either on the
command line or in the Builder Ada switches in the main project file:

* :samp:`-nostdinc`
* :samp:`-nostdlib`
* :samp:`-fstack-check`
* :samp:`-fno-inline`
* :samp:`-g{*}` Any switch starting with :samp:`-g`
* :samp:`-O{*}` Any switch starting with :samp:`-O`


These switches are passed to the Ada compiler.

.. _Initialization:

Initialization
--------------

Before performing one or several of its three phases, GPRbuild has to read the
command line, obtain its configuration, and process the project files.

If GPRbuild is invoked with an invalid switch or without any project file on
the command line, it will fail immediately.

Examples:


::

  $ gprbuild -P
  gprbuild: project file name missing after -P

  $ gprbuild -P c_main.gpr -WW
  gprbuild: illegal option "-WW"


GPRbuild looks for the configuration project file first in the current
working directory, then in the default configuration project directory.
If the GPRbuild executable is located in a subdirectory :file:`<prefix>/bin`,
then
the default configuration project directory is :file:`<prefix>/share/gpr`,
otherwise there is no default configuration project directory.

When it has found its configuration project path, GPRbuild needs to obtain its
configuration. By default, the file name of the main configuration project
is :file:`default.cgpr`. This default may be modified using the switch
:samp:`--config=...`

Example:


::

  $ gprbuild --config=my_standard.cgpr -P my_project.gpr


If GPRbuild cannot find the main configuration project on the configuration
project path, then it will look for all the languages specified in the user
project tree and invoke GPRconfig to create a configuration project file
named :file:`auto.cgpr` that is located in the object directory of the main
project file.

Once it has found the configuration project, GPRbuild will process its
configuration: if a single string attribute is specified in the configuration
project and is not specified in a user project, then the attribute is added
to the user project. If a string list attribute is specified in the
configuration project then its value is prepended to the corresponding
attribute in the user project.

After GPRbuild has processed its configuration, it will
process the user project file or files. If these user project files are
incorrect then GPRbuild will fail with the appropriate error messages:


::

  $ gprbuild -P my_project.gpr
  ada_main.gpr:3:26: "src" is not a valid directory
  gprbuild: "my_project.gpr" processing failed


Once the user project files have been dealt with successfully, GPRbuild
will start its processing.

.. _Compilation_of_one_or_several_sources:

Compilation of one or several sources
-------------------------------------

If GPRbuild is invoked with :samp:`-u` or :samp:`-U` and there are one or
several source file names specified on the command line, GPRbuild will compile
or recompile these sources, if they are not up to date or if :samp:`-f` is
also specified. Then GPRbuild will stop its execution.

The options/switches used to compile these sources are described in section
:ref:`Compilation_Phase`.

If GPRbuild is invoked with :samp:`-u` and no source file name is specified
on the command line, GPRbuild will compile or recompile all the sources of the
*main* project and then stop.

In contrast, if GPRbuild is invoked with :samp:`-U`, and again no source file name is specified
on the command line, GPRbuild will compile or recompile all the sources of
*all the projects in the project tree* and then stop.

.. _Compilation_Phase:

Compilation Phase
-----------------

When switch :samp:`-c` is used or when switches :samp:`-b` or :samp:`-l`
are not used, GPRbuild will first compile or recompile the sources that
are not up to date in all the projects in the project tree. The sources
considered are:

* all the sources in languages other than Ada

* if there are no main specified, all the Ada sources

* if there is a non Ada main, but no attribute `Roots` specified for
  this main, all the Ada sources

* if there is a main with an attribute `Roots` specified, all
  the Ada sources in the closures of these Roots.

* if there is an Ada main specified, all the Ada sources in the closure
  of the main


Attribute Roots takes as an index a main and a string list value. Each string
in the list is the name of an Ada library unit.

Example:

::

     for Roots ("main.c") use ("pkga", "pkgb");

Package PkgA and PkgB will be considered, and all the Ada units in their
closure will also be considered.

GPRbuild will first consider each source and decide if it needs to be
(re)compiled.

A source needs to be compiled in the following cases:

* Switch :samp:`-f` (force recompilations) is used

* The object file does not exist

* The source is more recent than the object file

* The dependency file does not exist

* The source is more recent than the dependency file

* When :samp:`-s` is used: the switch file does not exist

* When :samp:`-s` is used: the source is more recent than the switch file

* The dependency file cannot be read

* The dependency file is empty

* The dependency file has a wrong format

* A source listed in the dependency file does not exist

* A source listed in the dependency file has an incompatible time stamp

* A source listed in the dependency file has been replaced

* Switch :samp:`-s` is used and the source has been compiled with
  different switches or with the same switches in a different order


When a source is successfully compiled, the following files are normally
created in the object directory of the project of the source:

* An object file

* A dependency file, except when the dependency kind for the language
  is `none`

* A switch file if switch :samp:`-s` is used


The compiler for the language corresponding to the source file name is invoked with the following
switches/options:

* The required compilation switches for the language

* The compilation switches coming from package `Compiler` of the
  project of the source

* The compilation switches specified on the command line for all compilers,
  after :samp:`-cargs`

* The compilation switches for the language of the source, specified
  after :samp:`-cargs:{language}`

* Various other options including a switch to create the dependency file
  while compiling, a switch to specify a configuration file, a switch
  to specify a mapping file, and switches to indicate where to look for
  other source or header files that are needed to compile the source.


If compilation is needed, then all the options/switches, except those
described as 'Various other options' are written to the switch file.
The switch file is a text file. Its file name is obtained by replacing
the suffix of the source with :file:`.cswi`. For example, the switch file
for source :file:`main.adb` is :file:`main.cswi` and for
:file:`toto.c` it is :file:`toto.cswi`.

If the compilation is successful, then if the creation of the dependency
file is not done during compilation but after (see configuration attribute
`Compute_Dependency`), then the process to create the dependency file is
invoked.

If GPRbuild is invoked with a switch :samp:`-j` specifying more than one
compilation process, then several compilation processes for several sources of
possibly different languages are spawned concurrently.

For each project file, attribute Interfaces may be declared. Its value is a
list of sources or header files of the project file. For a project file
extending another one, directly or indirectly, inherited sources may be in
the list. When Interfaces is not declared, all sources or header files are
part of the interface of the project. When Interfaces is declared, only those
sources or header files are part of the interface of the project file. After
a successful compilation, gprbuild checks that all imported or included sources
or header files that are from an imported project are part of the interface of
the imported project. If this check fails, the compilation is invalidated and
the compilation artifacts (dependency, object and switches files) are deleted.

Example:

::

     project Prj is
        for Languages use ("Ada", "C");
        for Interfaces use ("pkg.ads", "toto.h");
     end Prj;

If a source from a project importing project Prj imports sources from Prj other
than package Pkg or includes header files from Prj other than "toto.h", then
its compilation will be invalidated.


.. _Post-Compilation_Phase:

Post-Compilation Phase
----------------------

The post-compilation phase has two parts: library building and program binding.

If there are libraries that need to be built or rebuilt, *gprbuild* will
call the library builder, specified by attribute `Library_Builder`.
This is generally the tool *gprlib*, provided with GPRbuild. If gprbuild
can determine that a library is already up to date, then the library builder
will not be called.

If there are mains specified, and for these mains there are sources of
languages with a binder driver (specified by attribute Binder'Driver
(<language>), then the binder driver is called for each such main, but only
if it needs to.

For Ada, the binder driver is normally *gprbind*, which will call
the appropriate version of *gnatbind*, that either the one in the same
directory as the Ada compiler or the fist one found on the path.
When neither of those is appropriate, it is possible to specify to
*gprbind* the full path of *gnatbind*, using the Binder switch
`--gnatbind_path=`.

Example:

::

     package Binder is
        for Switches ("Ada") use ("--gnatbind_path=/toto/gnatbind");
     end Binder;

If GPRbuild can determine that the artifacts from a previous
post-compilation phase are already up to date, the binder driver is not called.

If there are no libraries and no binder drivers, then the post-compilation
phase is empty.


.. _Linking_Phase:

Linking Phase
-------------

When there are mains specified, either in attribute Main or on the command
line, and these mains are not up to date, the linker is invoked for each main,
with all the specified or implied options, including the object files generated
during the post-compilation phase by the binder drivers.

If switch :samp:`-j{nnn}` is used, with `nnn` other than 1, gprbuild will attempt to link
simultaneously up to `nnn` executables.


.. _Distributed_compilation:

Distributed compilation
-----------------------

.. _Introduction_to_distributed_compilation:

Introduction to distributed compilation
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

For large projects the compilation time can become a limitation in
the development cycle. To cope with that, GPRbuild supports
distributed compilation.

In the distributed mode, the local machine (called the build master)
compile locally but also sends compilation requests to some remote
machines (called the build slaves). The compilation process can use
one or more build slaves. Once the compilation phase is done, the
build master will conduct the binding and linking phases locally.

.. _Setup_build_environments:

Setup build environments
^^^^^^^^^^^^^^^^^^^^^^^^

The configuration process to be able to use the distributed compilation
support is the following:

* Optionaly add a Remote package in the main project file

  This Remote package is to be placed into the project file that is passed
  to GPRbuild to build the application.

  The Root_Dir default value is the project's directory. This attribute
  designates the sources root directory. That is, the directory from which
  all the sources are to be found to build the application. If the project
  passed to GPRbuild to build the application is not at the top-level
  directory but in a direct sub-directory the Remote package should be:

  .. code-block:: gpr

      package Remote is
         for Root_Dir use "..";
      end Remote;

* Launch a slave driver on each build slave

  The build master will communicate with each build slave with a specific driver
  in charge of running the compilation process and returning statuses. This
  driver is *gprslave*, :ref:`GPRslave`.

  The requirement for the slaves are:

  * The same build environment must be setup (same compiler version).
  * The same libraries must be installed. That is, if the GNAT
    project makes use of external libraries the corresponding C headers or
    Ada units must be installed on the remote slaves.

  When all the requirement are set, just launch the slave driver:

  ::

      $ gprslave

When all this is done, the remote compilation can be used simply by
running GPRbuild in distributed mode from the build master:

::

    $ gprbuild --distributed=comp1.xyz.com,comp2.xyz.com prj.gpr

Alternatively the slaves can be set using the `GPR_SLAVES` environment
variable. So the following command is equivalent to the above:

::

    $ export GPR_SLAVES=comp1.xyz.com,comp2.xyz.com
    $ gprbuild --distributed prj.gpr

A third alternative is proposed using a list of slaves in a file (one
per line). In this case the `GPR_SLAVES_FILE` environment variable
must contain the path name to this file:

::

    $ export GPR_SLAVES_FILE=$HOME/slave-list.txt
    $ gprbuild --distributed prj.gpr

Finally note that the search for the slaves are in this specific
order. First the command line values, then `GPR_SLAVES` if set and
finally `GPR_SLAVES_FILES`.

The build slaves are specified with the following form:

::

    <machine_name>[:port]


.. _GPRslave:

GPRslave
^^^^^^^^

This is the slave driver in charge of running the compilation
jobs as request by the build master. One instance of this tool must be
launched in each build slaves referenced in the project file.

Compilations for a specific project is conducted under a sub-directory
from where the slave is launched by default. This can be overriden
with the `-d` option below.

The current options are:

* :samp:`-v, --verbose`

  Activate the verbose mode

* :samp:`-vv`, :samp:`--debug`

  Activate the debug mode (very verbose)

* :samp:`-h`, :samp:`--help`

  Display the usage

* :samp:`-d`, :samp:`--directory=`

  Set the work directory for the
  slave. This is where the sources will be copied and where the
  compilation will take place. A sub-directory will be created for each
  root project built.

* :samp:`-j{N}`, :samp:`--jobs={N}`

  Set the maximum simultaneous compilation.
  The default for `N` is the number of cores.

* :samp:`-p`, :samp:`--port={N}`

  Set the port the slave will listen to.
  The default value is 8484. The same port must be specified for the
  build slaves on `GPRbuild` command line.

* :samp:`-r`, :samp:`--response-handler={N}`

  Set maximum number of simultaneous responses.
  With this option it is possible to control the number of simultaneous
  responses (sending back object code and ALI files) supported. The
  value must be between 1 and the maximum number of simultaneous
  compilations.

.. _Configuring_with_GPRconfig:

Configuring with GPRconfig
==========================

.. _Configuration:

Configuration
-------------

GPRbuild requires one configuration file describing the languages and
toolchains to be used, and project files describing the
characteristics of the user project. Typically the configuration
file can be created automatically by `GPRbuild` based on the languages
defined in your projects and the compilers on your path. In more
involved situations --- such as cross compilation, or
environments with several compilers for the same language ---
you may need to control more precisely the generation of
the desired configuration of toolsets. A tool, GPRconfig, described in
:ref:`Configuring_with_GPRconfig`), offers this capability. In this
chapter most of the examples can use autoconfiguration.

GPRbuild will start its build process by trying to locate a configuration
file. The following tests are performed in the specified order, and the
first that matches provides the configuration file to use.

* If a file has a base names that matches `<target>-<rts>.cgpr`,
  `<target.cgpr`, `<rts>.cgpr` or `default.cgpr` is found in
  the default configuration files directory, this file is used. The target
  and rts parameters are specified via the `--target` and `--RTS`
  switches of `gprbuild`. The default directory is is :file:`share/gpr`
  in the installation directory of `gprbuild`

* If not found, the environment variable `GPR_CONFIG` is tested
  to check whether it contains the name of a valid configuration file. This
  can either be an absolute path name or a base name that will be searched
  in the same default directory as above.

* If still not found and you used the `--autoconf` switch, then
  a new configuration file is automatically generated based on the specified
  target and on the list of languages specified in your projects.

  GPRbuild assumes that there are known compilers on your path for each of
  the necessary languages. It is preferable and often necessary to manually
  generate your own configuration file when:

  * using cross compilers (in which case you need to use gprconfig's
    :samp:`--target=`) option,
  * using a specific Ada runtime (e.g. :samp:`--RTS=sjlj`),
  * working with compilers not in the path or not first in the path, or
  * autoconfiguration does not give the expected results.


GPRconfig provides several ways of generating configuration files. By
default, a simple interactive mode lists all the known compilers for all
known languages. You can then select a compiler for each of the languages;
once a compiler has been selected, only compatible compilers for other
languages are proposed. Here are a few examples of GPRconfig
invocation:

* The following command triggers interactive mode. The configuration will be
  generated in GPRbuild's default location, `./default.cgpr)`, unless
  :samp:`-o` is used.

  ::

      gprconfig

* The first command below also triggers interactive mode, but the resulting
  configuration
  file has the name and path selected by the user. The second command shows
  how GPRbuild can make use of this specific configuration file instead of
  the default one.

  ::

      gprconfig -o path/my_config.cgpr
      gprbuild --config=path/my_config.cgpr

* The following command again triggers interactive mode, and only the
  relevant cross compilers for target ppc-elf will be proposed.

  ::

      gprconfig --target=ppc-elf

* The next command triggers batch mode and generates at the default location
  a configuration file using the first native Ada and C compilers on
  the path.

  ::

      gprconfig --config=Ada --config=C --batch

* The next command, a combination of the previous examples, creates in
  batch mode a configuration file named :file:`x.cgpr` for cross-compiling
  Ada with a run-time called `hi` and using C for the LEON
  processor.

  ::

      gprconfig --target=leon-elf --config=Ada,,hi --config=C --batch -o x.cgpr


.. _Using_GPRconfig:

Using GPRconfig
---------------

Description
^^^^^^^^^^^

The GPRconfig tool helps you generate the configuration
files for GPRbuild. It automatically detects the available compilers
on your system and, after you have selected the one needed for your
application, it generates the proper configuration file.

.. note::

  In general, you will not launch GPRconfig
  explicitly. Instead, it is used implicitly by GPRbuild through the use
  of `--config` and `--autoconf` switches.

Command line arguments
^^^^^^^^^^^^^^^^^^^^^^

GPRconfig supports the following command line switches:

.. index:: --target (gprconfig)

:samp:`--target={platform}`

  ..  -- @TIPHTML{Use :samp:`--target` to specify on which machine your application will run}

  This switch indicates the target computer on which your application will
  be run. It is mostly useful for cross configurations. Examples include
  :samp:`ppc-elf`, :samp:`ppc-vx6-windows`. It can also be used in native
  configurations and is useful when the same machine can run different kind
  of compilers such as mingw32 and cygwin on Windows or x86-32 and x86-64
  on GNU Linux. Since different compilers will often return a different
  name for those targets, GPRconfig has an extensive knowledge of which
  targets are compatible, and will for example accept :samp:`x86-linux` as
  an alias for :samp:`i686-pc-linux-gnu`.
  The default target is the machine on which GPRconfig is run.

  If you enter the special target :samp:`all`, then all compilers found on the
  :envvar:`PATH` will be displayed.

.. index:: --show-target (gprconfig)

:samp:`--show-targets`

  As mentioned above, GPRconfig knows which targets are compatible. You
  can use this switch to find the list of targets that are compatible
  with `--target`.

.. index:: --config (gprconfig)

:samp:`--config={language}[,{version}[,{runtime}[,{path}[,{name}]]]]`

  .. -- @TIPHTML{Use :samp:`--config` to automatically select the first matching compiler}

  The intent of this switch is to preselect one or more compilers directly
  from the command line. This switch takes several optional arguments, which
  you can omit simply by passing the empty string. When omitted, the arguments
  will be computed automatically by GPRconfig.

  In general, only *language* needs to be specified, and the first
  compiler on the :envvar:`PATH` that can compile this language will be selected.
  As an example, for a multi-language application programmed in C and Ada,
  the command line would be:

  ::

      --config=Ada --config=C

  *path* is the directory that contains the compiler executable, for
  instance :file:`/usr/bin` (and not the installation prefix :file:`/usr`).

  *name* should be one of the compiler names defined in the GPRconfig
  knowledge base. The list of supported names includes :samp:`GNAT`,
  :samp:`GCC`,.... This name is
  generally not needed, but can be used to distinguish among several compilers
  that could match the other arguments of :samp:`--config`.

  Another possible more frequent use of *name* is to specify the base
  name of an executable. For instance, if you prefer to use a diab C compiler
  (executable is called :file:`dcc`) instead of :file:`gcc`, even if the latter
  appears first in the path, you could specify :file:`dcc` as the name parameter.

  ::

      gprconfig --config Ada,,,/usr/bin       # automatic parameters
      gprconfig --config C,,,/usr/bin,GCC     # automatic version
      gprconfig --config C,,,/usr/bin,gcc     # same as above, with exec name

  This switch is also the only possibility to include in your project some
  languages that are not associated with a compiler. This is sometimes useful
  especially when you are using environments like GPS that support project files.
  For instance, if you select "Project file" as a language, the files matching
  the :file:`.gpr` extension will be shown in the editor, although they of course
  play no role for gprbuild itself.

.. index:: --batch (gprconfig)

:samp:`--batch`

  .. -- @TIPHTML{Use :samp:`--batch` to generate the configuration file with no user interaction}

  If this switch is specified, GPRconfig automatically selects the first
  compiler matching each of the `--config` switches, and generates the
  configuration file immediately. It will not display an interactive menu.

.. index:: -o (gprconfig)

:samp:`-o {file}`

  .. -- @TIPHTML{Use :samp:`-o` to specify the name of the configuration file to generate}

  This specifies the name of the configuration file that will be generated.
  If this switch is not specified, a default file is generated in the
  installation directory of GPRbuild (assuming you have write access to
  that directory), so that it is automatically picked up by GPRbuild later
  on. If you select a different output file, you will need to specify it
  to GPRbuild.

.. index:: --db (gprconfig)

:samp:`--db {directory}`, :samp:`--db-`
  Indicates another directory that should be parsed for GPRconfig's knowledge
  base. Most of the time this is only useful if you are creating your own
  XML description files locally. The second version of the switch prevents
  GPRconfig from reading its default knowledge base.

.. index:: -h (gprconfig)

:samp:`-h`
  Generates a brief help message listing all GPRconfig switches and the
  default value for their arguments. This includes the location of the
  knowledge base, the default target, etc.


Interactive use
^^^^^^^^^^^^^^^

When you launch GPRconfig, it first searches for all compilers it
can find on your :envvar:`PATH`, that match the target specified by
:samp:`--target`. It is recommended, although not
required, that you place the compilers that you expect to use for your
application in your :envvar:`PATH` before you launch *gprconfig*,
since that simplifies the setup.

.. -- @TIPHTML{The list of compilers is sorted so that the most likely compilers appear first}

GPRconfig then displays the list of all the compilers
it has found, along with the language they can compile, the run-time
they use (when applicable),.... It then waits for you to select
one of the compilers.  This list is sorted by language, then by order
in the :envvar:`PATH` environment variable (so that compilers that you
are more likely to use appear first), then by run-time names and
finally by version of the compiler.  Thus the first
compiler for any language is most likely the one you want to use.

You make a selection by entering the letter that appears on the line for
each compiler (be aware that this letter is case sensitive). If the compiler was
already selected, it is deselected.

.. -- @TIPHTML{The list of compilers is filtered, so that only compatible compilers can be selected}

A filtered list of compilers is then displayed:
only compilers that target the same platform as the selected
compiler are now shown. GPRconfig then checks whether it is
possible to link sources compiled with the selected compiler and each of
the remaining compilers; when linking is not possible, the compiler is not
displayed. Likewise, all compilers for the same language are hidden, so that
you can only select one compiler per language.

As an example, if you need to compile your application with several C compilers,
you should create another language, for instance called C2, for that purpose.
That will give you the flexibility to indicate in the project files which
compiler should be used for which sources.

The goal of this filtering is to make it more obvious whether you have
a good chance of being able to link. There is however no guarantee that
GPRconfig will know for certain how to link any combination of the
remaining compilers.

You can select as many compilers as are needed by your application. Once you
have finished selecting the compilers, select :kbd:`s`, and GPRconfig will
generate the configuration file.


.. _The_GPRconfig_knowledge_base:

The GPRconfig knowledge base
----------------------------

GPRconfig itself has no hard-coded knowledge of compilers. Thus there
is no need to recompile a new version of GPRconfig when a new compiler
is distributed.

.. note::

   The role and format of the knowledge base are irrelevant for most users
   of GPRconfig, and are only needed when you need to add support for new
   compilers. You can skip this section if you only want to learn how to use
   GPRconfig.

All knowledge of compilers is embedded in a set of XML files called the
*knowledge base*.
Users can easily contribute to this general knowledge base, and have
GPRconfig immediately take advantage of any new data.

The knowledge base contains various kinds of information:

* Compiler description

  When it is run interactively, GPRconfig searches the user's
  :envvar:`PATH` for known compilers, and tries to deduce their
  configuration (version, supported languages, supported targets, run-times,
  ...). From the knowledge base GPRconfig knows how to extract the
  relevant information about a compiler.

  This step is optional, since a user can also enter all the information
  manually. However, it is recommended that the knowledge base explicitly
  list its known compilers, to make configuration easier for end users.

* Specific compilation switches

  When a compiler is used, depending on its version, target, run-time,...,
  some specific command line switches might have to be supplied. The
  knowledge base is a good place to store such information.

  For instance, with the GNAT compiler, using the soft-float runtime should
  force *gprbuild* to use the :samp:`-msoft-float` compilation switch.

* Linker options

  Linking a multi-language application often has some subtleties,
  and typically requires specific linker switches.
  These switches depend on the list of languages, the list
  of compilers,....

* Unsupported compiler mix

  It is sometimes not possible to link together code compiled with two
  particular compilers. The knowledge base should store this information,
  so that end users are informed immediately when attempting to
  use such a compiler combination.

The end of this section will describe in more detail the format of this
knowledge base, so that you can add your own information
and have GPRconfig advantage of it.


.. _General_file_format:

General file format
^^^^^^^^^^^^^^^^^^^

The knowledge base is implemented as a set of XML files. None of these
files has a special name, nor a special role. Instead, the user can
freely create new files, and put them in the knowledge base directory,
to contribute new knowledge.

The location of the knowledge base is :file:`$prefix/share/gprconfig`,
where :file:`$prefix` is the directory in which GPRconfig was
installed. Any file with extension :file:`.xml` in this directory will
be parsed automatically by GPRconfig at startup.

All files must have the following format:

::

    <?xml version="1.0" ?>
    <gprconfig>
       ...
    </gprconfig>

The root tag must be `<gprconfig>`.

The remaining sections in this chapter will list the valid XML tags that
can be used to replace the '...' code above. These tags can either all be
placed in a single XML file, or split across several files.


.. _Compiler_description:

Compiler description
^^^^^^^^^^^^^^^^^^^^

One of the XML tags that can be specified as a child of `<gprconfig>` is
`<compiler_description>`. This node and its children describe one of
the compilers known to GPRconfig. The tool uses them when it
initially looks for all compilers known on the user's :envvar:`PATH`
environment variable.

This is optional information, but simplifies the use of GPRconfig,
since the user is then able to omit some parameters from the :samp:`--config`
command line argument, and have them automatically computed.

The `<compiler_description>` node doesn't accept any XML
attribute.  However, it accepts a number of child tags that explain
how to query the various attributes of the compiler.  The child tags
are evaluated (if necessary) in the same order as they are documented below.


*<name>*
  This tag contains a simple string, which is the name of the compiler.
  This name must be unique across all the configuration files, and is used to
  identify that `compiler_description` node.

  ::

       <compiler_description>
       <name>GNAT</name>
       </compiler_description>

*<executable>*
  This tag contains a string, which is the name of an executable
  to search for on the PATH. Examples are :samp:`gnatls`, :samp:`gcc`,...

  In some cases, the tools have a common suffix, but a prefix that might depend
  on the target. For instance, GNAT uses :samp:`gnatmake` for native platforms,
  but :samp:`powerpc-wrs-vxworks-gnatmake` for cross-compilers to VxWorks.
  Most of the compiler description is the same, however.
  For such cases, the value of the `executable` node is considered as
  beginning a regular expression. The tag also accepts an optional
  attribute `prefix`,
  which is an integer indicating the parenthesis group that contains the prefix.
  In the following example, you obtain the version of the GNAT compiler by running
  either *gnatls* or *powerpc-wrs-vxworks-gnatls*, depending on
  the name of the executable that was found.

  The regular expression needs to match the whole name of the file, i.e. it
  contains an implicit '^' at the start, and an implicit '$' at the end.
  Therefore if you specify :samp:`.*gnatmake` as the regexp, it will not match
  :samp:`gnatmake-debug`.

  A special case is when this node is empty (but it must be specified!). In
  such a case, you must also specify the language (see <language> below) as a
  simple string. It is then assumed that the specified language does not
  require a compiler. In the configurations file (:ref:`Configurations`),
  you can test whether that language was specified on the command line by
  using a filter such as

  ::

      <compilers>
       <compiler language="name"/>
      </compilers>


  ::

      <executable prefix="1">(powerpc-wrs-vxworks-)?gnatmake</executable>
      <version><external>${PREFIX}gnatls -v</external></version>

  GPRconfig searches in all directories listed on the PATH for such
  an executable. When one is found, the rest of the `<compiler_description>`
  children are checked to know whether the compiler is valid. The directory
  in which the executable was found becomes the 'current directory' for
  the remaining XML children.

*<target>*
  This node indicates how to query the target architecture for the compiler.
  See :ref:`GPRconfig_external_values` for valid children.

  If this isn't specified, the compiler will always be considered as matching
  on the current target.

*<version>*
  This tag contains any of the nodes defined in :ref:`GPRconfig_external_values` below.
  It shows how to query the version number of the compiler. If the version
  cannot be found, the executable will not be listed in the list of compilers.


*<variable name="varname">*
  This node will define a user variable which may be later referenced.  The
  variables are evaluated just after the version but before the languages
  and the runtimes nodes.  See :ref:`GPRconfig_external_values`
  below for valid children of
  this node.  If the evaluation of this variable is empty then the compiler
  is considered as invalid.

*<languages>*
  This node indicates how to query the list of languages. See
  :ref:`GPRconfig_external_values`
  below for valid children of this node.

  The value returned by the system will be split into words. As a result, if
  the returned value is 'ada,c,c++', there are three languages supported by the
  compiler (and three entries are added to the menu when using GPRconfig
  interactively).

  If the value is a simple string, the words must be comma-separated, so that
  you can specify languages whose names include spaces. However, if the actual
  value is computed from the result of a command, the words can also be
  space-separated, to be compatible with more tools.

*<runtimes>*
  This node indicates how to query the list of supported runtimes for the
  compiler. See :ref:`GPRconfig_external_values`
  below for valid children. The returned value
  is split into words as for `<languages>`.

  This node accepts one attribute, `"default"`, which contains a list
  of comma-separated names of runtimes. It is used to sort the runtimes when
  listing which compilers were found on the PATH.

  As a special case, gprconfig will merge two runtimes if the XML nodes
  refer to the same directories after normalization and resolution of
  links. As such, on Unix systems, the "adalib" link to "rts-native/adalib"
  (or similar) will be ignored and only the "native" runtime will be
  displayed.


.. _GPRconfig_external_values:

GPRconfig external values
^^^^^^^^^^^^^^^^^^^^^^^^^

A number of the XML nodes described above can contain one or more children,
and specify how to query a value from an executable. Here is the list of
valid contents for these nodes. The `<directory>` and `<external>`
children can be repeated multiple times, and the `<filter>` and
`<must_match>` nodes will be applied to each of these. The final
value of the external value is the concatenation of the computation for each
of the `<directory>` and `<external>` nodes.

.. index:: gprconfig external values

* A simple string

  A simple string given in the node indicates a constant. For
  instance, the list of supported languages might be defined as:

  ::

      <compiler_description>
      <name>GNAT</name>
      <executable>gnatmake</executable>
      <languages>Ada</languages>
      </compiler_description>

  for the GNAT compiler, since this is an Ada-only compiler.

  Variables can be referenced in simple strings.

* `<getenv name="variable" />`

  If the contents of the node is a `<getenv>` child, the value of
  the environment variable `variable` is returned. If the variable is
  not defined, this is an error and the compiler is ignored.

  ::

      <compiler_description>
      <name>GCC-WRS</name>
      <executable prefix="1">cc(arm|pentium)</executable>
      <version>
      <getenv name="WIND_BASE" />
      </version>
      </compile_description>

* `<external>command</external>`

  If the contents of the node is an `<external>` child, this indicates
  that a command should be run on the system.
  When the command is run, the current directory (i.e., the one that contains
  the executable found through the `<executable>` node), is placed first
  on the :envvar:`PATH`. The output of the command is returned and may be later
  filtered. The command is not executed through a shell; therefore you cannot
  use output redirection, pipes, or other advanced features.

  For instance, extracting the target processor from *gcc* can be done
  with:

  ::

      <version>
      <external>gcc -dumpmachine</external>
      </version>

  Since the :envvar:`PATH` has been modified, we know that the *gcc* command
  that is executed is the one from the same directory as the `<external>`
  node.

  Variables are substituted in `command`.

* `<grep regexp="regexp" group="0" />`

  This node must come after the previously described ones. It is used to
  further filter the output. The previous output is matched against the regular
  expression `regexp` and the parenthesis group specified by
  `group` is returned. By default, group is 0, which indicates the
  whole output of the command.

  For instance, extracting the version number from *gcc* can be done
  with:

  ::

      <version>
      <external>gcc -v</external>
      <grep regexp="^gcc version (\S+)" group="1" />
      </version>

* `<directory group="0" contents="">regexp</directory>`

  If the contents of the node is a `<directory`> child, this
  indicates that GPRconfig should find all the files matching the
  regular expression. Regexp is a path relative to the directory that contains
  the `<executable>` file, and should use unix directory separators
  (ie '/'), since the actual directory will be converted into this format
  before the match, for system independence of the knowledge base.

  The group attribute indicates which parenthesis group should be returned.
  It defaults to 0 which indicates the whole matched path. If this attribute is
  a string rather than an integer, then it is the value returned.

  `regexp` can be any valid regular expression. This will only match
  a directory or file name, not a subdirectory. Remember to quote special
  characters, including '.', if you do not mean to use a regexp.

  The optional attribute `contents` can be used to indicate that the
  contents of the file should be read. The first line that matches the regular
  expression given by `contents` will be used as a file path instead of
  the file matched by `regexp`. This is in general used on platforms that
  do not have symbolic links, and a file is used instead of a symbolic link.
  In general, this will work better than `group` specifies a string rather
  than a parenthesis group, since the latter will match the path matched by
  `regexp`, not the one read in the file.

  For instance, finding the list of supported runtimes for the GNAT compiler
  is done with:

  ::

      <runtimes>
      <directory group="1">
      \.\./lib/gcc/${TARGET/.*/rts-(.*)/adainclude
      </directory>
      <directory group="default">
      \.\./lib/gcc/${TARGET}/.*/adainclude
      </directory>
      </runtimes>}

  Note the second node, which matches the default run-time, and displays it as
  such.

* `<filter>value1,value2,...</filter>`

  This node must come after one of the previously described ones. It is used to
  further filter the output. The previous output is split into words (it is
  considered as a comma-separated or space-separated list of words), and only
  those words in :samp:`value1`, :samp:`value2`,... are kept.

  For instance, the *gcc* compiler will return a variety of supported
  languages, including 'ada'. If we do not want to use it as an Ada
  compiler we can specify:

  ::

      <languages>
      <external regexp="languages=(\S+)" group="1">gcc -v</external>
      <filter>c,c++,fortran</filter>
      </languages>

* `<must_match>regexp</must_match>`

  If this node is present, then the filtered output is compared with the
  specified regular expression. If no match is found, then the executable
  is not stored in the list of known compilers.

  For instance, if you want to have a `<compiler_description>` tag
  specific to an older version of GCC, you could write:

  ::

      <version>
      <external regexp="gcc version (\S+)"
      group="1">gcc -v </external>
      <must_match>2.8.1</must_match>
      </version>

  Other versions of gcc will not match this `<compiler_description>`
  node.

.. _GPRconfig_variable_substitution:

GPRconfig variable substitution
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The various compiler attributes defined above are made available as
variables in the rest of the XML files. Each of these variables can be used
in the value of the various nodes (for instance in `<directory>`),
and in the configurations (:ref:`Configuration`).

A variable is referenced by `${name}` where `name` is either
a user variable or a predefined variable.  An alternate reference is
`$name` where `name` is a sequence of alpha numeric characters or
underscores.  Finally `$$` is replaced by a simple `$`.

User variables are defined by `<variable>` nodes and may override
predefined variables.  To avoid a possible override use lower case names.

The variables are used in two contexts: either in a
`<compiler_description>` node, in which case the variable refers to
the compiler we are describing, or within a `<configuration>` node.
In the latter case, and since there might be several compilers selected,
you need to further specify the variable by adding in parenthesis the
language of the compiler you are interested in.

For instance, the following is invalid:

::

    <configuration>
    <compilers>
    <compiler name="GNAT" />
    </compilers>
    <targets negate="true">
    <target name="^powerpc-elf$"/>
    </targets>
    <config>
    package Compiler is
      for Driver ("Ada") use "${PATHgcc";   --  Invalid !
    end Compiler;
    </config>
    </configuration>

The trouble with the above is that if you are using multiple languages
like C and Ada, both compilers will match the "negate" part, and therefore
there is an ambiguity for the value of `${PATH}`. To prevent such
issues, you need to use the following syntax instead when inside a
`<configuration>` node:

.. code-block:: gpr

    for Driver ("Ada") use "${PATH(ada)gcc";   --  Correct

Predefined variables are always in upper case.  Here is the list of
predefined variables

* *EXEC*
    is the name of the executable that was found through `<executable>`. It
    only contains the basename, not the directory information.


* *HOST*
    is replaced by the architecture of the host on which GPRconfig is
    running. This name is hard-coded in GPRconfig itself, and is generated
    by *configure* when GPRconfig was built.


* *TARGET*
    is replaced by the target architecture of the compiler, as returned by the
    `<target>` node. This is of course not available when computing the
    target itself.

    This variable takes the language of the compiler as an optional index when
    in a `<configuration>` block: if the language is specified, the target
    returned by that specific compiler is used; otherwise, the normalized target
    common to all the selected compilers will be returned (target normalization
    is also described in the knowledge base's XML files).


* *VERSION*
    is replaced by the version of the compiler. This is not available when
    computing the target or, of course, the version itself.


* *PREFIX*
    is replaced by the prefix to the executable name, as defined by the
    `<executable>` node.


* *PATH*
    is the current directory, i.e. the one containing the executable found through
    `<executable>`. It always ends with a directory separator.


* *LANGUAGE*
    is the language supported by the compiler, always folded to lower-case


* *RUNTIME*, *RUNTIME_DIR*
    This string will always be substituted by the empty string when the
    value of the external value is computed. These are special strings
    used when substituting text in configuration chunks.

    `RUNTIME_DIR` always end with a directory separator.


* *GPRCONFIG_PREFIX*
    is the directory in which GPRconfig was installed (e.g
    :file:`"/usr/local/"` if the executable is :file:`"/usr/local/bin/gprconfig"`.
    This directory always ends with a directory separator.
    This variable never takes a language in parameter, even within a
    `<configuration>` node.


If a variable is not defined, an error message is issued and the variable
is substituted by an empty string.


.. _Configurations:

Configurations
^^^^^^^^^^^^^^

The second type of information stored in the knowledge base are the chunks
of *gprbuild* configuration files.

Each of these chunks is also placed in an XML node that provides optional
filters. If all the filters match, then the chunk will be merged with other
similar chunks and placed in the final configuration file that is generated
by GPRconfig.

For instance, it is possible to indicate that a chunk should only be
included if the GNAT compiler with the soft-float runtime is used. Such
a chunk can for instance be used to ensure that Ada sources are always
compiled with the `-msoft-float` command line switch.

GPRconfig does not perform sophisticated merging of chunks. It simply
groups packages together. For example, if the two chunks are:

.. code-block:: gpr

   chunk1:
      package Language_Processing is
        for Attr1 use ("foo");
      end Language_Processing;
   chunk2:
      package Language_Processing is
        for Attr1 use ("bar");
      end Language_Processing;

Then the final configuration file will look like:

.. code-block:: gpr

    package Language_Processing is
      for Attr1 use ("foo");
      for Attr1 use ("bar");
    end Language_Processing;

As a result, to avoid conflicts, it is recommended that the chunks be
written so that they easily collaborate together. For instance,
to obtain something equivalent to

.. code-block:: gpr

   package Language_Processing is
     for Attr1 use ("foo", "bar");
   end Language_Processing;

the two chunks above should be written as:

.. code-block:: gpr

    chunk1:
      package Language_Processing is
        for Attr1 use Language_Processing'Attr1 & ("foo");
      end Language_Processing;
    chunk2:
      package Language_Processing is
        for Attr1 use Language_Processing'Attr1 & ("bar");
    end Language_Processing;

The chunks are described in a `<configuration>` XML node. The most
important child of such a node is `<config>`, which contains the
chunk itself. For instance, you would write:

::

   <configuration>
     ...  list of filters, see below
     <config>
     package Language_Processing is
        for Attr1 use Language_Processing'Attr1 & ("foo");
     end Language_Processing;
     </config>
   </configuration>

If `<config>` is an empty node (i.e., :samp:`<config/>` or
:samp:`<config></config>` was used), then the combination of selected
compilers will be reported as invalid, in the sense that code
compiled with these compilers cannot be linked together. As a result,
GPRconfig will not create the configuration file.

The special variables (:ref:`GPRconfig_variable_substitution`) are also
substituted in the chunk. That allows you to compute some attributes of the
compiler (its path, the runtime,...), and use them when generating the
chunks.

The filters themselves are of course defined through XML tags, and can
be any of:

*<compilers negate="false">*
  This filter contains a list of `<compiler>` children. The
  `<compilers>` filter matches if any of its children match.
  However, you can have several `<compilers>` filters, in which
  case they must all match. This can be used to include linker switches
  chunks. For instance, the following code would be used to describe
  the linker switches to use when GNAT 5.05 or 5.04 is used in addition to
  g++ 3.4.1:

  ::

     <configuration>
       <compilers>
         <compiler name="GNAT" version="5.04" />
         <compiler name="GNAT" version="5.05" />
       </compilers>
       <compilers>
         <compiler name="G++" version="3.4.1" />
       </compilers>
       ...
     </configuration>

  If the attribute `negate` is :samp:`true`, then the meaning of this
  filter is inverted, and it will match if none of its children matches.

  The format of the `<compiler>` is the following:

  ::

     <compiler name="name" version="..."
     runtime="..." language="..." />

  The name and language attributes, when specified, match
  the corresponding attributes used in the `<compiler_description>`
  children. All other attributes are regular expressions, which are matched
  against the corresponding selected compilers. When an attribute is not
  specified, it will always match. Matching is done in a case-insensitive
  manner.

  For instance, to check a GNAT compiler in the 5.x family, use:

  ::

     <compiler name="GNAT" version="5.\d+" />

*<hosts negate="false">*
  This filter contains a list of `<host>` children. It matches when
  any of its children matches. You can specify only one `<hosts>`
  node.
  The format of `<host>` is a node with a single mandatory attribute
  `name`, which is a regexp matched against the architecture on
  which GPRconfig is running. The name of the architecture was
  computed by `configure` when GPRconfig was built. Note that
  the regexp might match a substring of the host name, so you might want
  to surround it with "^" and "$" so that it only matches the whole host
  name (for instance, "elf" would match "powerpc-elf", but "^elf$" would
  not).

  If the `negate` attribute is :samp:`true`, then the meaning of this
  filter is inverted, and it will match when none of its children matches.

  For instance, to active a chunk only if the compiler is running on an
  intel linux machine, use:

  ::

     <hosts>
       <host name="i.86-.*-linux(-gnu)?" />
     </hosts>

*<targets negate="false">*
  This filter contains a list of `<target>` children. It behaves
  exactly like `<hosts>`, but matches against the architecture
  targeted by the selected compilers. For instance, to activate a chunk
  only when the code is targeted for linux, use:

  If the `negate` attribute is :samp:`true`, then the meaning of this filter
  is inverted, and it will match when none of its children matches.

  ::

     <targets>
       <target name="i.86-.*-linux(-gnu)?" />
     </targets>

.. _Configuration_File_Reference:

Configuration File Reference
============================

A text file using the project file syntax. It defines languages and
their characteristics as well as toolchains for those languages and their
characteristics.

GPRbuild needs to have a configuration file to know the different
characteristics of the toolchains that can be used to compile sources and
build libraries and executables.

A configuration file is a special kind of project file: it uses the same
syntax as a standard project file. Attributes in the configuration file
define the configuration. Some of these attributes have a special meaning
in the configuration.

The default name of the configuration file, when not specified to
GPRbuild by switches :samp:`--config=` or :samp:`--autoconf=` is
:file:`default.cgpr`. Although the name of the configuration file can
be any valid file name, it is recommended that its suffix be
:file:`.cgpr` (for Configuration GNAT Project), so that it cannot be
confused with a standard project file which has the suffix
:file:`.gpr`.

When :file:`default.cgpr` cannot be found in the configuration project path,
GPRbuild invokes GPRconfig to create a configuration file.

In the following description of the attributes, when an attribute is an
indexed attribute and its index is a language name, for example
`Spec_Suffix (<language>)`, then the name of the language is case insensitive.
For example, both `C` and `c` are allowed.

Any attribute may appear in a configuration project file. All attributes in
a configuration project file are inherited by each user project file in the
project tree. However, usually only the attributes listed below make sense
in the configuration project file.


.. _Project_Level_Configuration_Attributes:

Project Level Configuration Attributes
--------------------------------------


.. _General_Attributes:

General Attributes
^^^^^^^^^^^^^^^^^^


* Default_Language

  Specifies the name of the language of the immediate sources of a project when
  attribute `Languages` is not declared in the project. If attribute
  `Default_Language` is not declared in the configuration file, then each user
  project file in the project tree must have an attribute `Languages` declared,
  unless it extends another project. Example:

  .. code-block:: gpr

       for Default_Language use "ada";

* Run_Path_Option

  Specifies a 'run path option'; i.e., an option to use when linking an
  executable or a shared library to indicate the path (Rpath) where to look
  for other libraries. The value of this attribute is a string list.
  When linking an executable or a shared library, the search path is
  concatenated with the last string in the list, which may be an empty string.

  Example:

  .. code-block:: gpr

        for Run_Path_Option  use ("-Wl,-rpath,");

* Run_Path_Origin

  Specifies the string to be used in an Rpath to indicate the directory
  of the executable, allowing then to have Rpaths specified as relative paths.

  Example:

  .. code-block:: gpr

        for Run_Path_Origin use "$ORIGIN";

* Toolchain_Version (<language>)

  Specifies a version for a toolchain, as a single string. This toolchain
  version is passed to the library builder. Example:

  .. code-block:: gpr

        for Toolchain_Version ("Ada") use "GNAT 6.1";

  This attribute is used by GPRbind to decide on the names of the shared GNAT
  runtime libraries.

* Toolchain_Description (<language>)

  Specifies as a single string a description of a toolchain. This attribute is
  not directly used by GPRbuild or its auxiliary tools (GPRbind and GPRlib) but
  may be used by other tools, for example GPS. Example:

  .. code-block:: gpr

        for Toolchain_Description ("C") use "gcc version 4.1.3 20070425";


.. _General_Library_Related_Attributes:

General Library Related Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

* Library_Support

  Specifies the level of support for library project. If this attribute is not
  specified, then library projects are not supported. The only potential values
  for this attribute are `none`, `static_only` and `full`. Example:

  .. code-block:: gpr

       for Library_Support use "full";

* Library_Builder

  Specifies the name of the executable for the library builder. Example:

  .. code-block:: gpr

       for Library_Builder use "/.../gprlib";


.. _Archive_Related_Attributes:

Archive Related Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^^

* Archive_Builder

  Specifies the name of the executable of the archive builder with the minimum
  options, if any. Example:

  .. code-block:: gpr

       for Archive_Builder use ("ar", "cr");

* Archive_Indexer

  Specifies the name of the executable of the archive indexer with the minimum
  options, if any. If this attribute is not specified, then there is no
  archive indexer. Example:

  .. code-block:: gpr

        for Archive_Indexer use ("ranlib");

* Archive_Suffix

  Specifies the suffix of the archives. If this attribute is not specified, then
  the suffix of the archives is defaulted to :file:`.a`. Example:

  .. code-block:: gpr

       for Archive_Suffix use ".olb"; --  for VMS

* Library_Partial_Linker

  Specifies the name of the executable of the partial linker with the options
  to be used, if any. If this attribute is not specified, then there is no
  partial linking. Example:

  .. code-block:: gpr

       for Library_Partial_Linker use ("gcc", "-nostdlib", "-Wl,-r", "-o");


.. _Shared_Library_Related_Attributes:

Shared Library Related Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

* Shared_Library_Prefix

  Specifies the prefix of the file names of shared libraries. When this attribute
  is not specified, the prefix is `lib`. Example:

  .. code-block:: gpr

       for Shared_Library_Prefix use ""; --  for Windows, if needed

* Shared_Library_Suffix

  Specifies the suffix of the file names of shared libraries. When this attribute
  is not specified, the suffix is :file:`.so`. Example:

  .. code-block:: gpr

       for Shared_Library_Suffix use ".dll"; --  for Windows

* Symbolic_Link_Supported

  Specifies if symbolic links are supported by the platforms. The possible values
  of this attribute are `"false"` (the default) and `"true"`. When this attribute is
  not specified, symbolic links are not supported.

  .. code-block:: gpr

       for Symbolic_Link_Supported use "true";

* Library_Major_Minor_ID_Supported

  Specifies if major and minor IDs are supported for shared libraries.
  The possible values of this attribute are `"false"` (the default) and `"true"`.
  When this attribute is not specified, major and minor IDs are not supported.

  .. code-block:: gpr

       for Library_Major_Minor_ID_Supported use "True";

* Library_Auto_Init_Supported

  Specifies if library auto initialization is supported. The possible values of
  this attribute are `"false"` (the default) and `"true"`. When this attribute is not
  specified, library auto initialization is not supported.

  .. code-block:: gpr

       for Library_Auto_Init_Supported use "true";

* Shared_Library_Minimum_Switches

  Specifies the minimum options to be used when building a shared
  library. These options are put in the appropriate section in the
  library exchange file when the library builder is invoked. Example:

  .. code-block:: gpr

       for Shared_Library_Minimum_Switches use  ("-shared");

* Library_Version_Switches

  Specifies the option or options to be used when a library version is used.
  These options are put in the appropriate section in the library exchange file
  when the library builder is invoked. Example:

  .. code-block:: gpr

       for Library_Version_Switches use ("-Wl,-soname,");

* Runtime_Library_Dir (<language>)

  Specifies the directory for the runtime libraries for the language.
  Example:

  .. code-block:: gpr

       for Runtime_Library_Dir ("Ada") use "/path/to/adalib";

  This attribute is used by GPRlib to link shared libraries with Ada code.


.. _Package_Naming:

Package Naming
--------------

Attributes in package `Naming` of a configuration file specify defaults. These
attributes may be used in user project files to replace these defaults.

The following attributes usually appear in package `Naming` of a configuration
file:

* Spec_Suffix (<language>)

  Specifies the default suffix for a 'spec' or header file. Examples:

  .. code-block:: gpr

       for Spec_Suffix ("Ada") use ".ads";
       for Spec_Suffix ("C")   use ".h";
       for Spec_Suffix ("C++") use ".hh";

* Body_Suffix (<language>)

  Specifies the default suffix for a 'body' or a source file. Examples:

  .. code-block:: gpr

       for Body_Suffix ("Ada") use ".adb";
       for Body_Suffix ("C")   use ".c";
       for Body_Suffix ("C++") use ".cpp";

* Separate_Suffix

  Specifies the suffix for a subunit source file (separate) in Ada. If attribute
  `Separate_Suffix` is not specified, then the default suffix of subunit source
  files is the same as the default suffix for body source files. Example:

  .. code-block:: gpr

       for Separate_Suffix use ".sep";

* Casing

  Specifies the casing of spec and body files in a unit based language
  (such as Ada) to know how to map a unit name to its file name. The values for
  this attribute may only be `"lowercase"`, `"UPPERCASE"` and `"Mixedcase"`.
  The default, when attribute `Casing` is not specified is lower case.
  This attribute rarely needs to be specified, since on
  platforms where file names are not case sensitive (such as Windows or VMS)
  the default (lower case) will suffice.

* Dot_Replacement

  Specifies the string to replace a dot ('.') in unit names of a unit based
  language (such as Ada) to obtain its file name. If there is any unit based
  language in the configuration, attribute `Dot_Replacement` must be declared.
  Example:

  .. code-block:: gpr

       for Dot_Replacement use "-";


.. _Package_Builder:

Package Builder
---------------


* Executable_Suffix

  Specifies the default executable suffix. If no attribute `Executable_Suffix` is
  declared, then the default executable suffix for the host platform is used.
  Example:

  .. code-block:: gpr

       for Executable_Suffix use ".exe";


.. _Package_Compiler:

Package Compiler
----------------

.. _General_Compilation_Attributes:

General Compilation Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


* Driver (<language>)

  Specifies the name of the executable for the compiler of a language. The single
  string value of this attribute may be an absolute path or a relative path. If
  relative, then the execution path is searched. Specifying the empty string for
  this attribute indicates that there is no compiler for the language.

  Examples:

  .. code-block:: gpr

       for Driver ("C++") use "g++";
       for Driver ("Ada") use "/.../bin/gcc";
       for Driver ("Project file") use "";

* Required_Switches (<language>)

  Specifies the minimum options that must be used when invoking the compiler
  of a language. Examples:

  .. code-block:: gpr

       for Required_Switches ("C")   use ("-c", "-x", "c");
       for Required_Switches ("Ada") use ("-c", "-x", "ada", "-gnatA");

* PIC_Option (<language>)

  Specifies the option or options that must be used when compiling a source of
  a language to be put in a shared library. Example:

  .. code-block:: gpr

       for PIC_Option ("C") use ("-fPIC");


.. _Mapping_File_Related_Attributes:

Mapping File Related Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

* Mapping_File_Switches (<language>)

  Specifies the switch or switches to be used to specify a mapping file to the
  compiler. When attribute `Mapping_File_Switches` is not declared, then no
  mapping file is specified to the compiler. The value of this attribute is a
  string list. The path name of the mapping file is concatenated with the last
  string in the string list, which may be empty. Example:

  .. code-block:: gpr

       for Mapping_File_Switches ("Ada") use ("-gnatem=");

* Mapping_Spec_Suffix (<language>)

  Specifies, for unit based languages that support mapping files, the suffix in
  the mapping file that needs to be added to the unit name for specs. Example:

  .. code-block:: gpr

        for Mapping_Spec_Suffix ("Ada") use "%s";

* Mapping_Body_Suffix (<language>)

  Specifies, for unit based languages that support mapping files, the suffix in
  the mapping file that needs to be added to the unit name for bodies. Example:

  .. code-block:: gpr

        for Mapping_Spec_Suffix ("Ada") use "%b";


.. _Config_File_Related_Attributes:

Config File Related Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In the value of config file attributes defined below, there are some
placeholders that GPRbuild will replace. These placeholders are:

=========== =====================
Placeholder Interpretation
----------- ---------------------
:samp:`%u`  unit name
:samp:`%f`  source file name
:samp:`%s`  spec suffix
:samp:`%b`  body suffix
:samp:`%c`  casing
:samp:`%d`  dot replacement string
=========== =====================


Attributes:

* Config_File_Switches (<language>)

  Specifies the switch or switches to be used to specify a configuration file to
  the compiler. When attribute `Config_File_Switches` is not declared, then no
  config file is specified to the compiler. The value of this attribute is a
  string list. The path name of the config file is concatenated with the last
  string in the string list, which may be empty. Example:

  .. code-block:: gpr

       for Config_File_Switches ("Ada") use ("-gnatec=");

* Config_Body_File_Name (<language>)

  Specifies the line to be put in a config file to indicate the file name of a
  body. Example:

  .. code-block:: gpr

       for Config_Body_File_Name ("Ada") use
           "pragma Source_File_Name_Project (%u, Body_File_Name => ""%f"");";

* Config_Spec_File_Name (<language>)

  Specifies the line to be put in a config file to indicate the file name of a
  spec. Example:

  .. code-block:: gpr

       for Config_Spec_File_Name ("Ada") use
           "pragma Source_File_Name_Project (%u, Spec_File_Name => ""%f"");";

* Config_Body_File_Name_Pattern (<language>)

  Specifies the line to be put in a config file to indicate a body file name
  pattern. Example:

  .. code-block:: gpr

       for Config_Body_File_Name_Pattern ("Ada") use
           "pragma Source_File_Name_Project " &
           "  (Body_File_Name  => ""*%b""," &
           "   Casing          => %c," &
           "   Dot_Replacement => ""%d"");";

* Config_Spec_File_Name_Pattern (<language>)

  Specifies the line to be put in a config file to indicate a spec file name
  pattern. Example:

  .. code-block:: gpr

       for Config_Spec_File_Name_Pattern ("Ada") use
           "pragma Source_File_Name_Project " &
           "  (Spec_File_Name  => ""*%s""," &
           "   Casing          => %c," &
           "   Dot_Replacement => ""%d"");";

* Config_File_Unique (<language>)

  Specifies, for languages that support config files, if several config files
  may be indicated to the compiler, or not. This attribute may have only two
  values: `"true"` or `"false"` (case insensitive). The default, when this attribute
  is not specified, is `"false"`. When the value `"true"` is specified for this
  attribute, GPRbuild will concatenate the config files, if there are more than
  one. Example:

  .. code-block:: gpr

       for Config_File_Unique ("Ada") use "True";


.. _Dependency_Related_Attributes:

Dependency Related Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

There are two dependency-related attributes: `Dependency_Switches` and
`Dependency_Driver`. If neither of these two attributes are specified for
a language other than Ada, then the source needs to be (re)compiled if
the object file does not exist or the source file is more recent than
the object file or the switch file.

* Dependency_Switches (<language>)

  For languages other than Ada, attribute `Dependency_Switches` specifies
  the option or options to add to the compiler invocation so that it creates
  the dependency file at the same time. The value of attribute `Dependency_Option`
  is a string list. The name of the dependency file is added to the last string
  in the list, which may be empty. Example:

  .. code-block:: gpr

       for Dependency_Switches ("C") use ("-Wp,-MD,");

  With these `Dependency_Switches`, when compiling :file:`file.c` the compiler will be
  invoked with the option :samp:`-Wp,-MD,file.d`.

* Dependency_Driver (<language>)

  Specifies the command and options to create a dependency file for a source.
  The full path name of the source is appended to the last string of the string
  list value. Example:

  .. code-block:: gpr

       for Dependency_Driver ("C") use ("gcc", "-E", "-Wp,-M", "");

  Usually, attributes `Dependency_Switches` and `Dependency_Driver` are not both
  specified.


.. _Search_Path_Related_Attributes:

Search Path Related Attributes
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

* Include_Switches (<language>)

  Specifies the option or options to use when invoking the compiler to indicate
  that a directory is part of the source search path. The value of this
  attribute is a string list. The full path name of the directory is concatenated
  with the last string in the string list, which may be empty. Example:

  .. code-block:: gpr

       for Include_Switches ("C") use ("-I");

  Attribute `Include_Switches` is ignored if either one of the attributes
  `Include_Path` or `Include_Path_File` are specified.

* Include_Path (<language>)

  Specifies the name of an environment variable that is used by the compiler to
  get the source search path. The value of the environment variable is the source
  search path to be used by the compiler. Example:

  .. code-block:: gpr

       for Include_Path ("C")   use "CPATH";
       for Include_Path ("Ada") use "ADA_INCLUDE_PATH";

  Attribute `Include_Path` is ignored if attribute `Include_Path_File` is declared
  for the language.

* Include_Path_File (<language>)

  Specifies the name of an environment variable that is used by the compiler to
  get the source search path. The value of the environment variable is the path
  name of a text file that contains the path names of the directories of the
  source search path. Example:

  .. code-block:: gpr

       for Include_Path_File ("Ada") use "ADA_PRJ_INCLUDE_FILE";


.. _Package_Binder:

Package Binder
--------------

* Driver (<language>)

  Specifies the name of the executable of the binder driver. When this attribute
  is not specified, there is no binder for the language. Example:

  .. code-block:: gpr

       for Driver ("Ada") use "/.../gprbind";

* Required_Switches (<language>)

  Specifies the minimum options to be used when invoking the binder driver.
  These options are put in the appropriate section in the binder exchange file,
  one option per line. Example:

  .. code-block:: gpr

       for Required_Switches ("Ada") use ("--prefix=<prefix>");

* Prefix (<language>)

  Specifies the prefix to be used in the name of the binder exchange file.
  Example:

  .. code-block:: gpr

       for Prefix ("C++") use ("c__");

* Objects_Path (<language>)

  Specifies the name of an environment variable that is used by the compiler to
  get the object search path. The value of the environment variable is the object
  search path to be used by the compiler. Example:

  .. code-block:: gpr

       for Objects_Path ("Ada") use "ADA_OBJECTS_PATH";

* Objects_Path_File (<language>)

  Specifies the name of an environment variable that is used by the compiler to
  get the object search path. The value of the environment variable is the path
  name of a text file that contains the path names of the directories of the
  object search path. Example:

  .. code-block:: gpr

       for Objects_Path_File ("Ada") use "ADA_PRJ_OBJECTS_FILE";


.. _Package_Linker:

Package Linker
--------------

* Driver

  Specifies the name of the executable of the linker. Example:

  .. code-block:: gpr

       for Driver use "g++";

* Required_Switches

  Specifies the minimum options to be used when invoking the linker. Those
  options are happened at the end of the link command so that potentially
  conflicting user options take precedence.

* Map_File_Option

  Specifies the option to be used when the linker is asked to produce
  a map file.

  .. code-block:: gpr

       for Map_File_Option use "-Wl,-Map,";

* Max_Command_Line_Length

  Specifies the maximum length of the command line to invoke the linker.
  If this maximum length is reached, a response file will be used to shorten
  the length of the command line. This is only taken into account when
  attribute Response_File_Format is specified.

  .. code-block:: gpr

       for Max_Command_Line_Length use "8000";

* Response_File_Format

  Specifies the format of the response file to be generated when the maximum
  length of the command line to invoke the linker is reached. This is only
  taken into account when attribute Max_Command_Line_Length is specified.

  The allowed case-insensitive values are:

  * "GNU"
     Used when the underlying linker is gnu ld.

  * "Object_List"
     Used when the response file is a list of object files, one per line.

  * "GCC_GNU"
     Used with recent version of gcc when the underlined linker is gnu ld.

  * "GCC_Object_List"
     Used with recent version of gcc when the underlying linker is not gnu ld.

  .. code-block:: gpr

       for Response_File_Format use "GCC_GNU";

* Response_File_Switches

  Specifies the option(s) that must precede the response file name when
  when invoking the linker. This is only taken into account when both
  attributes Max_Command_Line_Length and Response_File_Format are specified.

  .. code-block:: gpr

        for Response_File_Switches  use ("-Wl,-f,");


.. _Cleaning_up_with_GPRclean:

Cleaning up with GPRclean
=========================

The GPRclean tool removes the files created by GPRbuild.
At a minimum, to invoke GPRclean you must specify a main project file
in a command such as `gprclean proj.gpr` or `gprclean -P proj.gpr`.

Examples of invocation of GPRclean:

.. code-block:: gpr

     gprclean -r prj1.gpr
     gprclean -c -P prj2.gpr


.. _Switches_for_GPRclean:

Switches for GPRclean
---------------------

The switches for GPRclean are:

* :samp:`--distributed`

  Also clean-up the sources on build slaves,
  see :ref:`Distributed_compilation`.

* :samp:`--slave-env={name}`

  Use `name` as the slave's environment directory instead of the default one.
  This options is only used in distributed mode.

* :samp:`--config={config project file name}`

  Specify the configuration project file name.

* :samp:`--autoconf={config project file name}`

  This specifies a configuration project file name that already exists or will
  be created automatically. Option :samp:`--autoconf=`
  cannot be specified more than once. If the configuration project file
  specified with :samp:`--autoconf=` exists, then it is used. Otherwise,
  GPRconfig is invoked to create it automatically.

* :samp:`--target={targetname}`

  Specify a target for cross platforms.

* :samp:`--db {dir}`

  Parse `dir` as an additional knowledge base.

* :samp:`--db-`

  Do not parse the standard knowledge base.

* :samp:`--RTS={runtime}`

  Use runtime `runtime` for language Ada.

* :samp:`--RTS:{lang}={runtime}`

  Use runtime `runtime` for language `lang`.

* :samp:`--subdirs={dir}`

  Real object, library or exec directories are subdirectories `dir` of the specified ones.

* :samp:`--relocate-build-tree[={dir}]`

  With this option it is possible to achieve out-of-tree build. That
  is, real object, library or exec directories are relocated to the
  current working directory or dir if specificed.

* :samp:`--root-dir={dir}`

  This option is to be used with --relocate-build-tree above and
  cannot be specified alone. This option specify the root directory
  for artifacts for proper relocation. The default value is the main
  project directory. This may not be suitable for relocation if for the
  example some artifact directories are in a directory upper. The
  specified directory must be a parent of all artifact directories.

* :samp:`--unchecked-shared-lib-imports`

  Shared library projects may import any project.

* :samp:`-aP{dir}`

  Add directory `dir` to the project search path.

* :samp:`-c`

  Only delete compiler-generated files. Do not delete
  executables and libraries.

* :samp:`-eL`

  Follow symbolic links when processing project files.

* :samp:`-f`

  Force deletions of unwritable files.

* :samp:`-F`

  Display full project path name in brief error messages.

* :samp:`-h`

  Display the usage.

* :samp:`-n`

  Do not delete files, only list files that would be deleted.

* :samp:`-P{proj}`

  Use Project File `proj`.

* :samp:`-q`

  Be quiet/terse. There is no output, except to report problems.

* :samp:`-r`

  Recursive. Clean all projects referenced by the main
  project directly or indirectly. Without this switch, GPRclean only
  cleans the main project.

* :samp:`-v`

  Verbose mode.

* :samp:`-vP{x}`

  Specify verbosity when parsing Project Files.
  `x` = 0 (default), 1 or 2.

* :samp:`-Xnm={val}`

  Specify an external reference for Project Files.


.. _Installing_with_GPRinstall:

Installing with GPRinstall
==========================

The GPRinstall tool installs projects. With GPRinstall it is not
needed to create complex `makefiles` to install the components. This
also removes the need for OS specific commands (like `cp`,
`mkdir` on UNIXs) and so makes the installation process easier on
all supported platforms.

After building a project it is often needed to install the project to
make it accessible to other projects. GPRinstall installs only what is
necessary and nothing more. That is for a library projects the library
itself is installed with the corresponding ALI files for Ada
sources. But the object code is not installed as not needed. Also if
the Ada specs are installed the bodies are not as not needed in most
cases. The cases where the bodies are required (if the spec has inline
routines or is a generic) are properly detected by GPRinstall.

Furthermore, we can note that GPRinstall handles the pre-processed
sources. So it installs the correct variant of the source after resolving
the pre-processing directives.

The parts of a project that can be installed are:

* sources of a project

* a static or shared library built from a library project

* objects built from a standard project

* executables built from a standard project

Moreover, GPRinstall will create, when needed, a project to use the
installed sources, objects or library. By default, this project file
is installed in the GPRbuild's default path location so that it can be
"with"ed easily without further configuration. The installation
process keeps record of every file installed for easy and safe removal.

GPRinstall supports all kind of project:

* standard projects

  The object files, executable and source files are considered for installation.

* library and aggregate library projects

  The library itself and the source files are considered for installation.

* aggregate projects

  All aggregated projects are considered for installation.


Projects that won't be installed are:

* Project explicitly disabled for installation

  A project with the Active attribute set to False in the project's
  Install package.

* Abstract project

  A project explicitly set with the abstract discriminant or a standard project
  without any sources.

At a minimum, to invoke GPRinstall you must specify a main project file
in a command such as `gprinstall proj.gpr` or
`gprinstall -P proj.gpr`.

Examples of invocation of GPRinstall:

::

     gprinstall prj1.gpr
     gprinstall -r --prefix=/my/root/install -P prj2.gpr

GPRinstall will record the installation under the *install name*
which is by default the name of the project without the
extension. That is above the project install names are `prj1` and
`prj2`.

The installation name can be specified with the option
`--install-name`. This makes it possible to record the
installation of multiple projects under the same name. This is handy
if an application comes with a library and a set of tools built with
multiple projects. In this case we may want to record the installation
under the same name. The install name is also used as a suffix to
group include and library directories.

Examples of installation under the same name:

::

     gprinstall --install-name=myapp lib.gpr
     gprinstall --install-name=myapp --mode=usage tools/tools.gpr

Note the `--mode=usage` option above. This tells GPRinstall to
only install the executable built as part of the project.

It is possible to uninstall a project by using the `--uninstall`
option. In this case we just pass the install name to GPRinstall:

::

     gprinstall --uninstall prj1
     gprinstall --uninstall prj2

And both `lib.gpr` and `tools.gpr` above will be uninstalled with:

::

     gprinstall --uninstall myapp


Note that GPRinstall does not deal with dependencies between projects.


.. _Switches_for_GPRinstall:

Switches for GPRinstall
-----------------------

The switches for GPRinstall are:

* :samp:`--config={main config project file name}`

  Specify the configuration project file name

* :samp:`--autoconf={config project file name}`

  This specifies a configuration project file name that already exists or will
  be created automatically. Option :samp:`--autoconf=`
  cannot be specified more than once. If the configuration project file
  specified with :samp:`--autoconf=` exists, then it is used. Otherwise,
  GPRconfig is invoked to create it automatically.

* :samp:`--build-name`

  Specify under which name the current
  project build must be installed. The default value is
  `default`. Using this option it is possible to install different
  builds (using different configuration, options, etc...) of the same
  project. The given name will be used by client to select which build
  they want to use (link against).

* :samp:`--build-var`

  Specify the name of the build variable in
  the installed project. The default value being `<PROJECT_NAME>_BUILD`.

* :samp:`--no-build-var`

  Specify that no build/scenario
  variable should be generated. This option can be use for a project
  where there is single configuration, so a single installation. This
  option cannot be used with :samp:`--build-var`.

* :samp:`--dry-run`

  Install nothing, just display the actions
  that would have been done.

* :samp:`-m`

  Install only the interface sources (minimal set of sources).

* :samp:`-f`

  Force overwriting of existing files

* :samp:`-h`

  Display this message

* :samp:`--mode=[dev/usage]`

  Specify the installation mode.

  * dev
    This is the default mode. The installation is done in developer
    mode. All files to use the project are copied to to install prefix. For a
    library this means that the specs, the corresponding ALI files for
    Ada units and the library itself (static or relocatable) are
    installed. For a standard project the object files are installed
    instead of the library.

  * usage
    The installation is done in usage mode. This means that only the
    library or the executable is installed. In this installation mode
    there is no project generated, nor specs or ALI files installed.

  ======== ================================================================
  Mode     Interpretation
  -------- ----------------------------------------------------------------
  `dev`    For this mode the binaries (built libraries and
           executable) are installed together with the sources to use them.
  `usage`  For this mode only the binaries are installed and no project are
           created.
  ======== ================================================================

* :samp:`-p`, :samp:`--create-missing-dirs`

  Create missing directories in the installation location.

* :samp:`-P{proj}`

  Specify the project file to install.

* :samp:`--prefix={path}`

  Specify the location of the installation.
  If not specified, the default location for the current
  compiler is used. That is, `path` corresponds to parent directory
  where `gprinstall` is found.

* :samp:`--install-name={name}`

  Specify the name to use for recording the installation.
  The default is the project name without the extension. If set this
  option is also used as include or library directories' suffix to
  group all related installations under a common directory.

* :samp:`--sources-subdir={path}`

  Specify the value for the sources installation directory if an absolute path.
  Otherwise it is appended to the prefix above. The default is
  `include/<project_name>[.<build-name>]`

* :samp:`--lib-subdir={path}`

  Specify the value for the library and object installation
  directory if an absolute path.
  Otherwise it is appended to the prefix above. The default is
  `lib/<project_name>[.<build-name>]`

* :samp:`--link-lib-subdir={path}`

  Specify the value for the
  library symlink directory if an absolute path. Otherwise it is
  appended to the prefix above.

* :samp:`---exec-subdir={path}`

  Specify the value for the
  executables installation directory if an absolute path. Otherwise it is
  appended to the prefix above. The default is `bin`.

* :samp:`--project-subdir={path}`

  Specify the value for the
  project installation directory if an absolute path. Otherwise it is
  appended to the prefix above. The default is `share/gpr`.

* :samp:`--target={targetname}`

  Specify a target for cross platforms.

* :samp:`--no-lib-link`

  Disable copy of shared libraries into
  the executable directory on Windows or creation of symlink in the lib
  directory on UNIX. This is done by default to place the shared
  libraries into a directory where application will look for them.

* :samp:`--sources-only`

  Copy only sources part of the project,
  the object, library or executable files are never copied. When this
  switch is used the installed project is not set as externally built.

* :samp:`--subdirs={subdir}`

  This indicates that the real directories (except the source directories) are
  subdirectories of the directories specified in the project files. This applies
  in particular to object directories, library directories and exec directories.
  If the directories do not exist, they are created automatically. It is
  expected that the sub-dir option value here is the one used with gprbuild.

* :samp:`--relocate-build-tree[={dir}]`

  With this option it is possible to achieve out-of-tree build. That
  is, real object, library or exec directories are relocated to the
  current working directory or dir if specificed.

* :samp:`--root-dir={dir}`

  This option is to be used with --relocate-build-tree above and
  cannot be specified alone. This option specify the root directory
  for artifacts for proper relocation. The default value is the main
  project directory. This may not be suitable for relocation if for the
  example some artifact directories are in a directory upper. The
  specified directory must be a parent of all artifact directories.

* :samp:`-q`

  Be quiet/terse. There is no output, except to report problems.

* :samp:`-r`

  (Recursive.) Install all projects referenced by the main
  project directly or indirectly. Without this switch, GPRinstall only
  installs the main project.

* :samp:`--uninstall`

  Uninstall mode, files installed for a
  given project or install name will be removed. A check is done that
  no manual changes have been applied to the files before removing.
  Deletion of the files can be forced in this case by using the
  :samp:`-f` option.

* :samp:`--list`

  List mode, displays all the installed packaged.

* :samp:`--stat`

  Apply to list mode above, displays also some
  statistics about the installed packages : number of files, total size
  used on disk, and whether there is some files missing.

* :samp:`-v`

  Verbose mode

* :samp:`-Xnm={val}`

  Specify an external reference for Project Files.


.. _Specific Naming Scheme with GPRname:

Specific Naming Scheme with GPRname
===================================

When the Ada source file names do not follow a regular naming
scheme, the mapping of Ada units to source file names must be indicated
in package Naming with attributes Spec and Body.

To help maintain the correspondence between compilation unit names and
source file names within the compiler,
the tool `gprname` may be used to generate automatically these attributes.



.. _Running_gprname:

Running `gprname`
-----------------

The usual form of the `gprname` command is:

.. code-block:: sh

      $ gprname [`switches`] `naming_pattern` [`naming_patterns`]
          [--and [`switches`] `naming_pattern` [`naming_patterns`]]


Most of the arguments are optional: switch *-P* must be specified to indicate
the project file and at least one Naming Pattern.

`gprname` will attempt to
find all the compilation units in files that follow at least one of the
naming patterns. To find Ada compilation units,
`gprname` will use the GNAT compiler in syntax-check-only mode on all
regular files.

One or several Naming Patterns may be given as arguments to `gprname`.
Each Naming Pattern is enclosed between double quotes (or single
quotes on Windows).
A Naming Pattern is a regular expression similar to the wildcard patterns
used in file names by the Unix shells or the DOS prompt.

`gprname` may be called with several sections of directories/patterns.
Sections are separated by switch `--and`. In each section, there must be
at least one pattern. If no directory is specified in a section, the
project directory is implied.
The options other that the directory switches and the patterns apply globally
even if they are in different sections.

Examples of Naming Patterns are::

     "*.[12].ada"
     "*.ad[sb]*"
     "body_*"    "spec_*"

For a more complete description of the syntax of Naming Patterns,
see the second kind of regular expressions described in :file:`g-regexp.ads`
(the 'Glob' regular expressions).

.. _Switches_for_pgprname:

Switches for GPRname
---------------------

Switches for `gprname` must precede any specified Naming Pattern.

You may specify any of the following switches to `gprname`:

.. index:: --version (gprname)

* :samp:`--version`

  Display Copyright and version, then exit disregarding all other options.

.. index:: --target= (gprname)

* :samp:`--target=<targ>`

  Indicates the target of the GNAT compiler. This may be needed if there is
  no native compiler available.

.. index:: --help (gprname)

* :samp:`--help`

  If *--version* was not used, display usage, then exit disregarding
  all other options.

* :samp:`--subdirs={dir}`

  Real object, library or exec directories are subdirectories <dir> of the
  specified ones.

* :samp:`--no-backup`

  Do not create a backup copy of the project file if it already exists.

* :samp:`--and`

  Start another section of directories/patterns.

.. index:: -d (gprname)

* :samp:`-d{dir}`

  Look for source files in directory :file:`dir`. There may be zero, one or more
  spaces between *-d* and :file:`dir`.
  :file:`dir` may end with `/**`, that is it may be of the form
  `root_dir/**`. In this case, the directory `root_dir` and all of its
  subdirectories, recursively, have to be searched for sources.
  When a switch *-d*
  is specified, the current working directory will not be searched for source
  files, unless it is explicitly specified with a *-d*
  or *-D* switch.
  Several switches *-d* may be specified.
  If :file:`dir` is a relative path, it is relative to the directory of
  the project file specified with switch *-P*. The directory
  specified with switch *-d* must exist and be readable.

.. index:: -D (gprname)

* :samp:`-D{filename}`

  Look for source files in all directories listed in text file :file:`filename`.
  There may be zero, one or more spaces between *-D*
  and :file:`filename`.
  :file:`filename` must be an existing, readable text file.
  Each nonempty line in :file:`filename` must be a directory.
  Specifying switch *-D* is equivalent to specifying as many
  switches *-d* as there are nonempty lines in
  :file:`file`.

* :samp:`-eL`

  Follow symbolic links when processing project files.

  .. index:: -f (gprname)

* :samp:`-f{pattern}`

  Foreign C language patterns. Using this switch, it is possible to add sources
  of language C to the list of sources of a project file.

  For example,

  .. code-block:: sh

     gprname -P prj.gpr -f"*.c" "*.ada" -f "*.clang"

  will look for Ada units in all files with the :file:`.ada` extension,
  and will add to the list of file for project :file:`prj.gpr` the C files
  with extensions :file:`.c` and :file:`.clang`. Attribute Languages will be
  declared with the list of languages with sources. In the above example,
  it will be ("Ada", "C") if Ada and C sources have been found.

* :samp:`-f:{<lang>} {pattern}`

  Foreign language {<lang>} patterns. Using this switch, it is possible to add
  sources of language <lang> to the list of sources of a project file.

  For example,

  .. code-block:: sh

     gprname -P prj.gpr "*.ada" -f:C++ "*.cpp" -f:C++ "*.CPP"

  Files with extensions :file:`.cpp` and :file:`*.CPP` are C++ sources.
  Attribute Languages will have value ("Ada", "C++") if Ada and C++ sources
  are found.

  .. index:: -h (gprname)

* :samp:`-h`

  Output usage (help) information. The output is written to :file:`stdout`.

  .. index:: -P (gprname)

* :samp:`-P{proj}`

  Create or update project file :file:`proj`. There may be zero, one or more
  space between *-P* and :file:`proj`. :file:`proj` may include directory
  information. :file:`proj` must be writable.
  There must be only one switch *-P*.
  If switch *--no-backup* is not specified, a backup copy of the project file is created
  in the project directory with file name <proj>.gpr.saved_x. 'x' is the first
  non negative number that makes this backup copy a new file.

  .. index:: -v (gprname)

* :samp:`-v`

  Verbose mode. Output detailed explanation of behavior to :file:`stdout`.
  This includes name of the file written, the name of the directories to search
  and, for each file in those directories whose name matches at least one of
  the Naming Patterns, an indication of whether the file contains a unit,
  and if so the name of the unit.

.. index:: -v -v (gprname)

* :samp:`-v -v`

  Very Verbose mode. In addition to the output produced in verbose mode,
  for each file in the searched directories whose name matches none of
  the Naming Patterns, an indication is given that there is no match.

  .. index:: -x (gprname)

* :samp:`-x{pattern}`

  Excluded patterns. Using this switch, it is possible to exclude some files
  that would match the name patterns. For example,

  .. code-block:: sh

      gprname -P prj.gpr -x "*_nt.ada" "*.ada"

  will look for Ada units in all files with the :file:`.ada` extension,
  except those whose names end with :file:`_nt.ada`.

.. _Example_of_gprname_Usage:

Example of `gprname` Usage
--------------------------

.. code-block:: sh

     $ gprname -P/home/me/proj.gpr -x "*_nt_body.ada"
     -dsources -dsources/plus -Dcommon_dirs.txt "body_*" "spec_*"

Note that several switches *-d* may be used,
even in conjunction with one or several switches
*-D*. Several Naming Patterns and one excluded pattern
are used in this example.
