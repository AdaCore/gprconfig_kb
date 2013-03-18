------------------------------------------------------------------------------
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             G P R _ U T I L                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2007-2013, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

--  This package contains constants, variable and subprograms used by gprbuild
--  and gprclean.

with GNAT.MD5;    use GNAT.MD5;
with GNAT.OS_Lib; use GNAT.OS_Lib;

with ALI;
with Namet;    use Namet;
with Prj;      use Prj;
with Prj.Tree; use Prj.Tree;

package Gpr_Util is

   Partial_Prefix : constant String := "p__";

   Begin_Info : constant String := "--  BEGIN Object file/option list";
   End_Info   : constant String := "--  END Object file/option list   ";

   Project_Node_Tree : constant Project_Node_Tree_Ref :=
                         new Project_Node_Tree_Data;
   --  This is also used to hold project path and scenario variables

   Success : Boolean := False;

   --  Config project

   Config_Project_Option : constant String := "--config=";

   Autoconf_Project_Option : constant String := "--autoconf=";

   Target_Project_Option : constant String := "--target=";

   Prefix_Project_Option : constant String := "--prefix";

   No_Name_Map_File_Option : constant String := "--map-file-option";

   Restricted_To_Languages_Option : constant String :=
                                               "--restricted-to-languages=";

   Distributed_Option : constant String := "--distributed=";

   Dry_Run_Option : constant String := "--dry-run";

   Named_Map_File_Option   : constant String := No_Name_Map_File_Option & '=';

   Config_Path : String_Access := null;

   Target_Name : String_Access := null;

   Config_Project_File_Name   : String_Access := null;
   Configuration_Project_Path : String_Access := null;
   --  Base name and full path to the configuration project file

   Autoconfiguration : Boolean := True;
   --  Whether we are using an automatically config (from gprconfig)

   Autoconf_Specified : Boolean := False;
   --  Whether the user specified --autoconf on the gprbuild command line

   Delete_Autoconf_File : Boolean := False;
   --  This variable is used by gprclean to decide if the config project file
   --  should be cleaned. It is set to True when the config project file is
   --  automatically generated or --autoconf= is used.

   --  Default project

   Default_Project_File_Name : constant String := "default.gpr";

   --  Implicit project

   Implicit_Project_File_Path : constant String :=
     "share" &
     Directory_Separator &
     "gpr" &
     Directory_Separator &
     '_' &
     Default_Project_File_Name;

   --  User projects

   Project_File_Name          : String_Access := null;
   --  The name of the project file specified with switch -P

   No_Project_File_Found : Boolean := False;
   --  True when no project file is specified and there is no .gpr file
   --  in the current working directory.

   Main_Project : Project_Id;
   --  The project id of the main project

   RTS_Option : constant String := "--RTS=";

   RTS_Language_Option : constant String := "--RTS:";

   Db_Directory_Expected : Boolean := False;
   --  True when last switch was --db

   Distributed_Mode : Boolean := False;
   --  Wether the distributed compilation mode has been activated

   --  Packages of project files where unknown attributes are errors

   Naming_String   : aliased String := "naming";
   Builder_String  : aliased String := "builder";
   Compiler_String : aliased String := "compiler";
   Binder_String   : aliased String := "binder";
   Linker_String   : aliased String := "linker";
   Clean_String    : aliased String := "clean";
   --  Name of packages to be checked when parsing/processing project files

   List_Of_Packages : aliased String_List :=
                        (Naming_String'Access,
                         Builder_String'Access,
                         Compiler_String'Access,
                         Binder_String'Access,
                         Linker_String'Access,
                         Clean_String'Access);
   Packages_To_Check : constant String_List_Access := List_Of_Packages'Access;
   --  List of the packages to be checked when parsing/processing project files

   --  Local subprograms

   function Binder_Exchange_File_Name
     (Main_Base_Name : File_Name_Type; Prefix : Name_Id) return String_Access;
   --  Returns the name of the binder exchange file corresponding to an
   --  object file and a language.
   --  Main_Base_Name must have no extension specified

   procedure Create_Response_File
     (Format            : Response_File_Format;
      Objects           : String_List;
      Other_Arguments   : String_List;
      Resp_File_Options : String_List;
      Name_1            : out Path_Name_Type;
      Name_2            : out Path_Name_Type);
   --  Create a temporary file as a response file that contains either the list
   --  of Objects in the correct Format, or for Format GCC the list of all
   --  arguments. It is the responsibility of the caller to delete this
   --  temporary file if needed.

   ----------
   -- Misc --
   ----------

   procedure Find_Binding_Languages
     (Tree         : Project_Tree_Ref;
      Root_Project : Project_Id);
   --  Check if in the project tree there are sources of languages that have
   --  a binder driver.
   --  Populates Tree's appdata (Binding and There_Are_Binder_Drivers).
   --  Nothing is done if the binding languages were already searched for
   --  this Tree.
   --  This also performs the check for aggregated project trees.

   function Get_Compiler_Driver_Path
     (Project_Tree : Project_Tree_Ref;
      Lang         : Language_Ptr) return String_Access;
   --  Get, from the config, the path of the compiler driver. This is first
   --  looked for on the PATH if needed.
   --  Returns "null" if no compiler driver was specified for the language, and
   --  exit with an error if one was specified but not found.

   procedure Look_For_Default_Project;
   --  Check if default.gpr exists in the current directory. If it does, use
   --  it. Otherwise, if there is only one file ending with .gpr, use it.

   function Partial_Name
     (Lib_Name      : String;
      Number        : Natural;
      Object_Suffix : String) return String;
   --  Returns the name of an object file created by the partial linker

   function Shared_Libgcc_Dir (Run_Time_Dir : String) return String;
   --  Returns the directory of the shared version of libgcc, if it can be
   --  found, otherwise returns an empty string.

   package Knowledge is

      function Normalized_Hostname return String;
      --  Return the normalized name of the host on which gprbuild is running.
      --  The knowledge base must have been parsed first.

      procedure Parse_Knowledge_Base
        (Project_Tree : Project_Tree_Ref;
         Directory : String := "");

   end Knowledge;

   procedure Need_To_Compile
     (Source         : Source_Id;
      Tree           : Project_Tree_Ref;
      In_Project     : Project_Id;
      Must_Compile   : out Boolean;
      The_ALI        : out ALI.ALI_Id;
      Object_Check   : Boolean;
      Always_Compile : Boolean);
   --  Check if a source need to be compiled.
   --  A source need to be compiled if:
   --    - Force_Compilations is True
   --    - No object file generated for the language
   --    - Object file does not exist
   --    - Dependency file does not exist
   --    - Switches file does not exist
   --    - Either of these 3 files are older than the source or any source it
   --      depends on.
   --  If an ALI file had to be parsed, it is returned as The_ALI, so that the
   --  caller does not need to parse it again.
   --
   --  Object_Check should be False when switch --no-object-check is used. When
   --  True, presence of the object file and its time stamp are checked to
   --  decide if a file needs to be compiled.
   --
   --  Tree is the project tree in which Source is found (or the root tree when
   --  not using aggregate projects).
   --
   --  Always_Compile should be True when gprbuid is called with -f -u and at
   --  least one source on the command line.

   function Project_Compilation_Failed
     (Prj       : Project_Id;
      Recursive : Boolean := True) return Boolean;
   --  Returns True if all compilations for Prj (and all projects it depends on
   --  if Recursive is True) were successful and False otherwise.

   procedure Set_Failed_Compilation_Status (Prj : Project_Id);
   --  Record compilation failure status for the given project

   Maximum_Size : Integer;
   pragma Import (C, Maximum_Size, "__gnat_link_max");
   --  Maximum number of bytes to put in an invocation of the
   --  Archive_Builder.

   function Ensure_Directory (Path : String) return String;
   --  Returns Path with an ending directory separator

   function File_MD5 (Pathname : String) return Message_Digest;
   --  Returns the file MD5 signature. Raises Name_Error if Pathname does not
   --  exists.

   function Relative_Path (Pathname, To : String) return String;
   --  Returns the relative pathname which corresponds to Pathname when
   --  starting from directory to. Both Pathname and To must be absolute paths.

   procedure Create_Sym_Link (From, To : String);
   --  Create a relative symlink in From pointing to To

   --  Architecture

   function Get_Target return String;
   --  Returns the current target for the compilation

end Gpr_Util;
