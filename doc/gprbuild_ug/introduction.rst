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
  (see :ref:`Configuring with GPRconfig<Configuring_with_GPRconfig>`).

* :samp:`GPRclean`

  A tool to remove compilation artifacts created by GPRbuild
  (see :ref:`Cleaning up with GPRclean<Cleaning_up_with_GPRclean>`).

* :samp:`GPRinstall`

  Executable and library installer using GPR files
  (see :ref:`Installing with GPRinstall<Installing_with_GPRinstall>`).

* :samp:`GPRname`

  Naming scheme generator
  (see :ref:`Specific Naming Scheme with GPRname`).
