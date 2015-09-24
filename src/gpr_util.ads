------------------------------------------------------------------------------
--                                                                          --
--                             GPR TECHNOLOGY                               --
--                                                                          --
--                     Copyright (C) 2007-2015, AdaCore                     --
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

--  This package contains constants, variable and subprograms used by gprbuild
--  and gprclean.

with Ada.Calendar; use Ada;

with GNAT.HTable;
with GNAT.MD5;     use GNAT.MD5;
with GNAT.OS_Lib;  use GNAT.OS_Lib;

with GPR.ALI;
with GPR;       use GPR;
with GPR.Tree;  use GPR.Tree;

package Gpr_Util is

   Partial_Prefix : constant String := "p__";

   Begin_Info : constant String := "--  BEGIN Object file/option list";
   End_Info   : constant String := "--  END Object file/option list   ";

   Project_Node_Tree : constant Project_Node_Tree_Ref :=
                         new Project_Node_Tree_Data;
   --  This is also used to hold project path and scenario variables

   Success : Boolean := False;

   Complete_Output_Option : constant String := "--complete-output";

   Complete_Output : Boolean := False;
   --  Set to True with switch Complete_Output_Option

   --  Config project

   Config_Project_Option : constant String := "--config=";

   Autoconf_Project_Option : constant String := "--autoconf=";

   Target_Project_Option : constant String := "--target=";

   Prefix_Project_Option : constant String := "--prefix";

   No_Name_Map_File_Option : constant String := "--map-file-option";

   Restricted_To_Languages_Option : constant String :=
                                               "--restricted-to-languages=";

   Distributed_Option : constant String := "--distributed";

   Slave_Env_Option : constant String := "--slave-env";
   Slave_Env_Auto   : Boolean := False;

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

   Slave_Env : String_Access;
   --  The name of the distributed build environment

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

   Gprname_Packages : aliased String_List := (1 => Naming_String'Access);

   Packages_To_Check_By_Gprname : constant String_List_Access :=
                                    Gprname_Packages'Access;

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

   procedure Create_Sym_Links
     (Lib_Path    : String;
      Lib_Version : String;
      Lib_Dir     : String;
      Maj_Version : String);
   --  Copy Lib_Version to Lib_Path (removing Lib_Path if it exists). If
   --  Maj_Version is set it also link Lib_Version into Lib_Dir with the
   --  specified Maj_Version.

   procedure Create_Sym_Link (From, To : String);
   --  Create a relative symlink in From pointing to To

   procedure Display_Usage_Version_And_Help;
   --  Output the two lines of usage for switches --version and --help

   procedure Display_Version
     (Tool_Name      : String;
      Initial_Year   : String;
      Version_String : String);
   --  Display version of a tool when switch --version is used

   generic
      with procedure Usage;
      --  Print tool-specific part of --help message
   procedure Check_Version_And_Help_G
     (Tool_Name      : String;
      Initial_Year   : String;
      Version_String : String);
   --  Check if switches --version or --help is used. If one of this switch is
   --  used, issue the proper messages and end the process.

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
   --
   --  The --compiler-subst switch is taken into account. For example, if
   --  "--compiler-subst=ada,gnatpp" was given, and Lang is the Ada language,
   --  this will return the full path name for gnatpp.

   procedure Locate_Runtime
     (Project_Tree : Project_Tree_Ref;
      Language     : Name_Id);
   --  Wrapper around Set_Runtime_For. Search RTS name in the project path and
   --  if found convert it to an absolute path. Emit an error message if a
   --  full RTS name (an RTS name that contains a directory separator) is not
   --  found.

   procedure Look_For_Default_Project;
   --  Check if default.gpr exists in the current directory. If it does, use
   --  it. Otherwise, if there is only one file ending with .gpr, use it.

   function Major_Id_Name
     (Lib_Filename : String;
      Lib_Version  : String) return String;
   --  Returns the major id library file name, if it exists.
   --  For example, if Lib_Filename is "libtoto.so" and Lib_Version is
   --  "libtoto.so.1.2", then "libtoto.so.1" is returned.

   function Object_Project (Project : Project_Id) return Project_Id;
   --  For a non aggregate project, returns the project.
   --  For an aggrete project or an aggregate library project, returns an
   --  aggregated project that is not an aggregate project.

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

   --  Architecture

   function Get_Target return String;
   --  Returns the current target for the compilation

   function Compute_Slave_Env
     (Project : Project_Tree_Ref; Auto : Boolean) return String;
   --  Compute a slave environment based on the command line parameter and
   --  the project variables. We want the same slave environment for identical
   --  build. Data is a string that must be taken into account in the returned
   --  value.

   function Get_Slaves_Hosts
     (Project_Tree : Project_Tree_Ref;
      Arg          : String) return String;
   --  Given the actual argument "--distributed[=...]" return the coma
   --  separated list of slave hosts. This routine handle the GPR_SLAVE and
   --  GPR_SLAVES_FILE environment variables.

   function UTC_Time return Stamps.Time_Stamp_Type;
   --  Returns the UTC time

   function Check_Diff
     (Ts1, Ts2  : Stamps.Time_Stamp_Type;
      Max_Drift : Duration := 5.0) return Boolean;
   --  Check two time stamps, returns True if both time are in a range of
   --  Max_Drift seconds maximum.

   function To_Time_Stamp (Time : Calendar.Time) return Stamps.Time_Stamp_Type;
   --  Returns Time as a time stamp type

   --  Compiler and package substitutions

   --  The following are used to support the --compiler-subst and
   --  --compiler-pkg-subst switches, which are used by tools such as gnatpp to
   --  have gprbuild drive gnatpp, thus calling gnatpp only on files that need
   --  it.
   --
   --  gnatpp will pass --compiler-subst=ada,gnatpp to tell gprbuild to run
   --  gnatpp instead of gcc. It will also pass
   --  --compiler-pkg-subst=pretty_printer to tell gprbuild to get switches
   --  from "package Pretty_Printer" instead of from "package Compiler".

   Compiler_Subst_Option     : constant String := "--compiler-subst=";
   Compiler_Pkg_Subst_Option : constant String := "--compiler-pkg-subst=";

   package Compiler_Subst_HTable is new GNAT.HTable.Simple_HTable
     (Header_Num => GPR.Header_Num,
      Element    => Name_Id,
      No_Element => No_Name,
      Key        => Name_Id,
      Hash       => GPR.Hash,
      Equal      => "=");
   --  A hash table to get the compiler to substitute from the from the
   --  language name. For example, if the command line option
   --  "--compiler-subst=ada,gnatpp" was given, then this mapping will include
   --  the key-->value pair "ada" --> "gnatpp". This causes gprbuild to call
   --  gnatpp instead of gcc.

   Compiler_Pkg_Subst : Name_Id := No_Name;
   --  A package name to be used when invoking the compiler, in addition to
   --  "package Compiler". Normally, this is No_Name, indicating no additional
   --  package, but it can be set by the --compiler-pkg-subst option. For
   --  example, if --compiler-pkg-subst=pretty_printer was given, then this
   --  will be "pretty_printer", and gnatpp will be invoked with switches from
   --  "package Pretty_Printer", and -inner-cargs followed by switches from
   --  "package Compiler".

   package Project_Output is
      --  Support for Gprname

      Output_FD : File_Descriptor;
      --  To save the project file and its naming project file

      procedure Write_Eol;
      --  Output an empty line

      procedure Write_A_Char (C : Character);
      --  Write one character to Output_FD

      procedure Write_A_String (S : String);
      --  Write a String to Output_FD
   end Project_Output;

end Gpr_Util;
