------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              G P R E X C H                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2006-2007, Free Software Foundation, Inc.       --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  These package defines sections and the corresponding labels for exchange
--  files between gprmake and gprbind (binding exchange files) and gprlib
--  (library exchange files).

--  All labels must start with a square bracket '['

package Gprexch is

   type String_Ptr is access constant String;

   --  Common labels

   Binding_Options_Label  : aliased constant String := "[BINDING OPTIONS]";
   Dependency_Files_Label : aliased constant String := "[DEPENDENCY FILES]";
   Quiet_Label            : aliased constant String := "[QUIET]";
   Run_Path_Option_Label  : aliased constant String := "[RUN PATH OPTION]";
   Verbose_Label          : aliased constant String := "[VERBOSE]";

   --  Binding exchange file sections

   type Binding_Section is
     (No_Binding_Section,
      Quiet,
      Verbose,
      Shared_Libs,
      Generated_Source_File,
      Compiler_Path,
      Compiler_Options,
      Main_Dependency_File,
      Dependency_Files,
      Binding_Options,
      Generated_Object_File,
      Resulting_Options,
      Run_Path_Option);

   --  Binding exchange file section labels

   Shared_Libs_Label           : aliased constant String :=
                                   "[SHARED LIBS]";
   Generated_Source_File_Label : aliased constant String :=
                                   "[GENERATED SOURCE FILE]";
   Compiler_Path_Label         : aliased constant String :=
                                   "[COMPILER PATH]";
   Compiler_Options_Label      : aliased constant String :=
                                   "[COMPILER OPTIONS]";
   Main_Dependency_File_Label  : aliased constant String :=
                                   "[MAIN DEPENDENCY FILE]";
   Generated_Object_File_Label : aliased constant String :=
                                   "[GENERATED OBJECT FILE]";
   Resulting_Options_Label     : aliased constant String :=
                                   "[RESULTING OPTIONS]";

   Binding_Labels : constant array (Binding_Section) of String_Ptr :=
                 (No_Binding_Section    => null,
                  Quiet                 => Quiet_Label'Access,
                  Verbose               => Verbose_Label'Access,
                  Shared_Libs           => Shared_Libs_Label'Access,
                  Generated_Source_File => Generated_Source_File_Label'Access,
                  Compiler_Path         => Compiler_Path_Label'Access,
                  Compiler_Options      => Compiler_Options_Label'Access,
                  Main_Dependency_File  => Main_Dependency_File_Label'Access,
                  Dependency_Files      => Dependency_Files_Label'Access,
                  Binding_Options       => Binding_Options_Label'Access,
                  Generated_Object_File => Generated_Object_File_Label'Access,
                  Resulting_Options     => Resulting_Options_Label'Access,
                  Run_Path_Option       => Run_Path_Option_Label'Access);

   --  Library exchange file sections

   type Library_Section is
     (No_Library_Section,
      Quiet,
      Verbose,
      Relocatable,
      Static,
      Object_Files,
      Options,
      Object_Directory,
      Library_Name,
      Library_Directory,
      Library_Dependency_Directory,
      Library_Version,
      Library_Options,
      Library_Path,
      Library_Version_Options,
      Shared_Lib_Prefix,
      Shared_Lib_Suffix,
      Shared_Lib_Minimum_Options,
      Symbolic_Link_Supported,
      Major_Minor_Id_Supported,
      PIC_Option,
      Imported_Libraries,
      Driver_Name,
      Runtime_Directory,
      Toolchain_Version,
      Archive_Builder,
      Archive_Indexer,
      Partial_Linker,
      Archive_Suffix,
      Run_Path_Option,
      Auto_Init,
      Interface_Dep_Files,
      Dependency_Files,
      Binding_Options,
      Copy_Source_Dir,
      Sources);

   --  Library exchange file section labels

   Relocatable_Label         : aliased constant String :=
                                 "[RELOCATABLE]";
   Static_Label              : aliased constant String :=
                                 "[STATIC]";
   Object_Files_Label        : aliased constant String :=
                                 "[OBJECT FILES]";
   Options_Label             : aliased constant String :=
                                 "[OPTIONS]";
   Object_Directory_Label    : aliased constant String :=
                                 "[OBJECT DIRECTORY]";
   Library_Name_Label        : aliased constant String :=
                                 "[LIBRARY NAME]";
   Library_Directory_Label   : aliased constant String :=
                                 "[LIBRARY DIRECTORY]";
   Library_Dependency_Directory_Label : aliased String :=
                                          "[LIBRARY DEPENDENCY DIRECTORY]";
   Library_Version_Label     : aliased constant String :=
                                 "[LIBRARY VERSION]";
   Library_Options_Label     : aliased constant String :=
                                 "[LIBRARY OPTIONS]";
   Library_Path_Label        : aliased constant String :=
                                 "[LIBRARY PATH]";
   Imported_Libraries_Label  : aliased constant String :=
                                 "[IMPORTED LIBRARIES]";
   Driver_Name_Label         : aliased constant String :=
                                 "[DRIVER NAME]";
   Runtime_Directory_Label   : aliased constant String :=
                                 "[RUNTIME DIRECTORY]";
   Toolchain_Version_Label   : aliased constant String :=
                                 "[TOOLCHAIN VERSION]";
   Archive_Builder_Label     : aliased constant String :=
                                 "[ARCHIVE BUILDER]";
   Archive_Indexer_Label     : aliased constant String :=
                                 "[ARCHIVE INDEXER]";
   Partial_Linker_Label      : aliased constant String :=
                                 "[PARTIAL LINKER]";
   Archive_Suffix_Label      : aliased constant String :=
                                 "[ARCHIVE SUFFIX]";
   Auto_Init_Label           : aliased constant String :=
                                 "[AUTO INIT]";
   Interface_Dep_Files_Label : aliased constant String :=
                                 "[INTERFACE DEPENDENCY FILES]";
   Copy_Source_Dir_Label     : aliased constant String :=
                                 "[COPY SOURCE DIRECTORY]";
   Sources_Label             : aliased constant String :=
                                 "[SOURCES]";
   Library_Version_Options_Label : aliased constant String :=
                                 "[LIBRARY VERSION OPTIONS]";
   Shared_Lib_Prefix_Label    : aliased constant String :=
                                 "[SHARED LIB PREFIX]";
   Shared_Lib_Suffix_Label    : aliased constant String :=
                                  "[SHARED LIB SUFFIX]";
   PIC_Option_Label           : aliased constant String :=
                                  "[PIC OPTION]";
   Shared_Lib_Minimum_Options_Label : aliased constant String :=
                                        "[SHARED LIB MINIMUM OPTIONS]";
   Symbolic_Link_Supported_Label : aliased constant String :=
                                  "[SYMBOLIC LINK SUPPORTED]";
   Major_Minor_Id_Supported_Label : aliased constant String :=
                                  "[MAJOR MINOR ID SUPPORTED]";

   Library_Labels : constant array (Library_Section) of String_Ptr :=
                      (No_Library_Section  => null,
                       Quiet               => Quiet_Label'Access,
                       Verbose             => Verbose_Label'Access,
                       Relocatable         => Relocatable_Label'Access,
                       Static              => Static_Label'Access,
                       Object_Files        => Object_Files_Label'Access,
                       Options             => Options_Label'Access,
                       Object_Directory    => Object_Directory_Label'Access,
                       Library_Name        => Library_Name_Label'Access,
                       Library_Directory   => Library_Directory_Label'Access,
                       Library_Dependency_Directory =>
                         Library_Dependency_Directory_Label'Access,
                       Library_Version     => Library_Version_Label'Access,
                       Library_Options     => Library_Options_Label'Access,
                       Library_Path        => Library_Path_Label'Access,
                       Library_Version_Options =>
                         Library_Version_Options_Label'Access,
                       Shared_Lib_Prefix   => Shared_Lib_Prefix_Label'Access,
                       Shared_Lib_Suffix   => Shared_Lib_Suffix_Label'Access,
                       Shared_Lib_Minimum_Options =>
                         Shared_Lib_Minimum_Options_Label'Access,
                       Symbolic_Link_Supported =>
                         Symbolic_Link_Supported_Label'Access,
                       Major_Minor_Id_Supported =>
                         Major_Minor_Id_Supported_Label'Access,
                       PIC_Option          => PIC_Option_Label'Access,
                       Imported_Libraries  => Imported_Libraries_Label'Access,
                       Driver_Name         => Driver_Name_Label'Access,
                       Runtime_Directory   => Runtime_Directory_Label'Access,
                       Toolchain_Version   => Toolchain_Version_Label'Access,
                       Archive_Builder     => Archive_Builder_Label'Access,
                       Archive_Indexer     => Archive_Indexer_Label'Access,
                       Partial_Linker      => Partial_Linker_Label'Access,
                       Archive_Suffix      => Archive_Suffix_Label'Access,
                       Run_Path_Option     => Run_Path_Option_Label'Access,
                       Auto_Init           => Auto_Init_Label'Access,
                       Interface_Dep_Files => Interface_Dep_Files_Label'Access,
                       Dependency_Files    => Dependency_Files_Label'Access,
                       Binding_Options     => Binding_Options_Label'Access,
                       Copy_Source_Dir     => Copy_Source_Dir_Label'Access,
                       Sources             => Sources_Label'Access);

   function Get_Binding_Section (Label : String) return Binding_Section;
   --  Get the current section from a label in a binding exchange file

   function Get_Library_Section (Label : String) return Library_Section;
   --  Get the current section from a label in a library exchange file

end Gprexch;
