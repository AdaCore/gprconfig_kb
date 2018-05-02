------------------------------------------------------------------------------
--                                                                          --
--                             GPR TECHNOLOGY                               --
--                                                                          --
--                     Copyright (C) 2006-2018, AdaCore                     --
--                                                                          --
-- This is  free  software;  you can redistribute it and/or modify it under --
-- terms of the  GNU  General Public License as published by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for more details.  You should have received  a copy of the  GNU  --
-- General Public License distributed with GNAT; see file  COPYING. If not, --
-- see <http://www.gnu.org/licenses/>.                                      --
--                                                                          --
------------------------------------------------------------------------------

--  These package defines sections and the corresponding labels for exchange
--  files between gprbuild and gprbind (binding exchange files) and gprlib
--  (library exchange files).

--  All labels start with '[' and end with ']'

package Gprexch is

   --  Binding exchange file sections

   type Binding_Section is
     (No_Binding_Section,
      Quiet,
      Verbose_Low,
      Verbose_Higher,
      Nothing_To_Bind,
      Shared_Libs,
      Main_Base_Name,
      Mapping_File,
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
      There_Are_Stand_Alone_Libraries,
      Script_Path);

   function Binding_Label (Section : Binding_Section) return String;
   --  Return the label for a section in a binder exchange file

   function Get_Binding_Section (Label : String) return Binding_Section;
   --  Get the current section from a label in a binding exchange file

   --  Library exchange file sections

   type Library_Section is
     (No_Library_Section,
      No_Create,
      Quiet,
      Verbose_Low,
      Verbose_Higher,
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
      Library_Rpath_Options,
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
      Compiler_Leading_Switches,
      Compiler_Trailing_Switches,
      Toolchain_Version,
      Archive_Builder,
      Archive_Builder_Append_Option,
      Archive_Indexer,
      Partial_Linker,
      Archive_Suffix,
      Run_Path_Option,
      Run_Path_Origin,
      Separate_Run_Path_Options,
      Install_Name,
      Auto_Init,
      Interface_Dep_Files,
      Other_Interfaces,
      Interface_Obj_Files,
      Standalone_Mode,
      Dependency_Files,
      Binding_Options,
      Leading_Library_Options,
      Copy_Source_Dir,
      Sources,
      Generated_Object_Files,
      Generated_Source_Files,
      Max_Command_Line_Length,
      Response_File_Format,
      Response_File_Switches,
      Keep_Temporary_Files,
      Object_Lister,
      Object_Lister_Matcher,
      Export_File,
      Library_Symbol_File,
      Script_Path,
      No_SAL_Binding,
      Mapping_File,
      Project_Directory,
      CodePeer_Mode);

   function Library_Label (Section : Library_Section) return String;
   --  Return the label for a section in a library exchange file

   function Get_Library_Section (Label : String) return Library_Section;
   --  Get the current section from a label in a library exchange file

end Gprexch;
