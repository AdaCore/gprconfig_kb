------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              G P R E X C H                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2006-2009, Free Software Foundation, Inc.       --
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

--  All labels start with '[' and end with ']'

package Gprexch is

   --  Binding exchange file sections

   type Binding_Section is
     (No_Binding_Section,
      Quiet,
      Verbose,
      Shared_Libs,
      Main_Base_Name,
      Compiler_Path,
      Compiler_Leading_Switches,
      Compiler_Trailing_Switches,
      Main_Dependency_File,
      Dependency_Files,
      Binding_Options,
      Generated_Object_File,
      Bound_Object_Files,
      Generated_Source_Files,
      Resulting_Options,
      Run_Path_Option,
      Project_Files,
      Toolchain_Version,
      Delete_Temp_Files,
      Object_File_Suffix,
      There_Are_Stand_Alone_Libraries);

   function Binding_Label (Section : Binding_Section) return String;
   --  Return the label for a section in a binder exchange file

   function Get_Binding_Section (Label : String) return Binding_Section;
   --  Get the current section from a label in a binding exchange file

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
      Runtime_Library_Dir,
      Driver_Name,
      Compilers,
      Toolchain_Version,
      Archive_Builder,
      Archive_Builder_Append_Option,
      Archive_Indexer,
      Partial_Linker,
      Archive_Suffix,
      Run_Path_Option,
      Separate_Run_Path_Options,
      Auto_Init,
      Interface_Dep_Files,
      Dependency_Files,
      Binding_Options,
      Copy_Source_Dir,
      Sources,
      Generated_Object_Files,
      Generated_Source_Files,
      Max_Command_Line_Length,
      Response_File_Format,
      Response_File_Switches,
      Keep_Response_File);

   function Library_Label (Section : Library_Section) return String;
   --  Return the label for a section in a library exchange file

   function Get_Library_Section (Label : String) return Library_Section;
   --  Get the current section from a label in a library exchange file

end Gprexch;
