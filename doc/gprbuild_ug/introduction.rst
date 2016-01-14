.. _Introduction:

************
Introduction
************

This User's Guide describes several software tools that use GNAT Projects to
drive their behavior. GNAT Projects are stored in text files with the extension
:samp:`.gpr`, commonly called :samp:`GPR files`.

These GPR tools have in common a facility called the GNAT Project Manager that
is fully described in the next section.

The main GPR tool is :samp:`GPRbuild`, a multi-language builder for systems
organized into subsystems and libraries.

The other GPR tools are:

* :samp:`GPRconfig`

  A configuration project file generator

* :samp:`GPRclean`

  A tool to remove compilation artifacts created by GPRbuild.

* :samp:`GPRinstall`

  Executable and library installer using GPR files.

* :samp:`GPRname`

  Naming scheme generator.
