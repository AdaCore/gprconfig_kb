------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             B U I L D G P R                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2004-2011, Free Software Foundation, Inc.         --
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

with Ada.Command_Line;  use Ada.Command_Line;
with Ada.Exceptions;    use Ada.Exceptions;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with System.Case_Util;       use System.Case_Util;
with System.Multiprocessors; use System.Multiprocessors;

with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.Dynamic_HTables;      use GNAT.Dynamic_HTables;
with GNAT.Dynamic_Tables;
with GNAT.HTable;
with GNAT.OS_Lib;               use GNAT.OS_Lib;

with ALI;
with ALI.Util;
with Csets;
with Debug;       use Debug;
with Err_Vars;    use Err_Vars;
with Gpr_Util;    use Gpr_Util;
with GPR_Version; use GPR_Version;
with Gprexch;     use Gprexch;
with Hostparm;
with Makeutl;     use Makeutl;
with Namet;       use Namet;
with Output;      use Output;
with Opt;
with Osint;       use Osint;
with Prj;         use Prj;
with Prj.Conf;    use Prj.Conf;
with Prj.Env;
with Prj.Err;
with Prj.Tree;    use Prj.Tree;
with Prj.Util;    use Prj.Util;
with Snames;      use Snames;
with Switch;      use Switch;
with System;
with Table;
with Tempdir;
with Types;       use Types;

package body Buildgpr is

   use Gpr_Util.Knowledge;
   use type ALI.ALI_Id, Opt.Verbosity_Level_Type, Opt.Warning_Mode_Type;

   Object_Suffix : constant String := Get_Target_Object_Suffix.all;
   --  The suffix of object files on this platform

   Maximum_Size : Integer;
   pragma Import (C, Maximum_Size, "__gnat_link_max");
   --  Maximum number of bytes to put in an invocation of the
   --  Archive_Builder.

   Dash_L : Name_Id;
   --  "-L", initialized in procedure Initialize

   Current_Working_Dir : constant String := Get_Current_Dir;
   --  The current working directory

   Main_Project_Dir : String_Access;
   --  The absolute path of the project directory of the main project,
   --  initialized in procedure Initialize.

   Executable_Suffix : constant String_Access := Get_Executable_Suffix;
   --  The suffix of executables on this platforms

   Main_Index : Int := 0;

   Project_Tree : constant Project_Tree_Ref :=
                    new Project_Tree_Data (Is_Root_Tree => True);
   --  The project tree

   Never : constant Time_Stamp_Type := (others => '9');
   --  A time stamp that is greater than any real one

   Copyright_Output : Boolean := False;
   Usage_Output     : Boolean := False;
   --  Flags to avoid multiple displays of Copyright notice and of Usage

   Usage_Needed : Boolean := False;
   --  Set by swith -h: usage will be displayed after all command line
   --  switches have been scanned.

   Display_Paths : Boolean := False;
   --  Set by switch --display-paths: config project path and user project path
   --  will be displayed after all command lines witches have been scanned.

   Output_File_Name           : String_Access := null;
   --  The name given after a switch -o

   Output_File_Name_Expected  : Boolean := False;
   --  True when last switch was -o

   Project_File_Name_Expected : Boolean := False;
   --  True when last switch was -P

   Search_Project_Dir_Expected : Boolean := False;
   --  True when last switch was -aP

   No_Object_Check_Switch : constant String  := "--no-object-check";
   Object_Checked         : Boolean := True;
   --  False when switch --no-object-check is used. When True, presence of
   --  the object file and its time stamp are checked to decide if a file needs
   --  to be compiled.

   Map_File : String_Access := null;
   --  Value of switch --create-map-file

   Direct_Import_Only_Switch  : constant String  := "--direct-import-only";
   Indirect_Imports_Switch    : constant String  := "--indirect-imports";
   No_Indirect_Imports_Switch : constant String  := "--no-indirect-imports";
   Indirect_Imports           : Boolean := True;
   --  False when switch --no-indirect-imports is used. Sources are only
   --  allowed to import from the projects that are directly withed.

   Recursive : Boolean := False;

   Unique_Compile : Boolean := False;
   --  Set to True if -u or -U or a project file with no main is used

   Unique_Compile_All_Projects : Boolean := False;
   --  Set to True if -U is used

   Always_Compile : Boolean := False;
   --  Set to True when gprbuid is called with -f -u and at least one source
   --  on the command line.

   Naming_String   : aliased String := "naming";
   Builder_String  : aliased String := "builder";
   Compiler_String : aliased String := "compiler";
   Binder_String   : aliased String := "binder";
   Linker_String   : aliased String := "linker";
   --  Name of packages to be checked when parsing/processing project files

   List_Of_Packages : aliased String_List :=
                        (Naming_String              'Access,
                         Builder_String             'Access,
                         Compiler_String            'Access,
                         Binder_String              'Access,
                         Linker_String              'Access);
   Packages_To_Check : constant String_List_Access := List_Of_Packages'Access;
   --  List of the packages to be checked when parsing/processing project files

   type Processor is (None, Linker, Binder, Compiler);
   Current_Processor : Processor := None;
   --  This variable changes when switches -*args are used

   Outstanding_Compiles : Natural := 0;
   --  The number of compilation jobs currently spawned

   package Bad_Compilations is new Table.Table
     (Table_Component_Type => Source_Id,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 20,
      Table_Increment      => 100,
      Table_Name           => "Buildgpr.Bad_Compilations");
   --  Full name of all the source files for which compilation fails

   -------------------------------------------
   -- Options specified on the command line --
   -------------------------------------------

   package Options is

      type Option_Type is
        (Force_Compilations_Option,
         Keep_Going_Option,
         Maximum_Processes_Option,
         Quiet_Output_Option,
         Check_Switches_Option,
         Verbose_Mode_Option,
         Verbose_Low_Mode_Option,
         Verbose_Medium_Mode_Option,
         Warnings_Treat_As_Error,
         Warnings_Normal,
         Warnings_Suppress,
         Indirect_Imports);

      procedure Register_Command_Line_Option
        (Option : Option_Type; Value : Natural := 0);
      --  Record a command line option

      procedure Process_Command_Line_Options;
      --  Reprocess the recorded command line options that have priority over
      --  the options in package Builder of the main project.

   end Options;

   use Options;

   type Process_Purpose is (Compilation, Dependency);
   --  A type to distinguish between compilation jobs and dependency file
   --  building jobs.

   type Process_Data is record
      Pid            : Process_Id         := Invalid_Pid;
      Source         : Queue.Source_Info  := Queue.No_Source_Info;
      Source_Project : Project_Id         := null;
      Mapping_File   : Path_Name_Type     := No_Path;
      Purpose        : Process_Purpose    := Compilation;
      Options        : String_List_Access := null;
   end record;
   --  Data recorded for each spawned jobs, compilation of dependency file
   --  building.

   No_Process_Data : constant Process_Data :=
                       (Pid             => Invalid_Pid,
                        Source          => Queue.No_Source_Info,
                        Source_Project  => null,
                        Mapping_File    => No_Path,
                        Purpose         => Compilation,
                        Options         => null);

   type Header_Num is range 0 .. 2047;

   function Hash (Pid : Process_Id) return Header_Num;

   package Compilation_Htable is new GNAT.HTable.Simple_HTable
     (Header_Num => Header_Num,
      Element    => Process_Data,
      No_Element => No_Process_Data,
      Key        => Process_Id,
      Hash       => Hash,
      Equal      => "=");
   --  Hash table to keep data for all spawned jobs

   package All_Language_Builder_Compiling_Options is new Table.Table
     (Table_Component_Type => String_Access,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 10,
      Table_Increment      => 100,
      Table_Name           =>
        "Makegpr.All_Language_Builder_Compiling_Options");
   --  Table to store the options for all compilers, that is those that
   --  follow the switch "-cargs" without any mention of language in the
   --  Builder switches.

   package All_Language_Compiling_Options is new Table.Table
     (Table_Component_Type => String_Access,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 10,
      Table_Increment      => 100,
      Table_Name           => "Makegpr.All_Language_Compiling_Options");
   --  Table to store the options for all compilers, that is those that
   --  follow the switch "-cargs" without any mention of language on the
   --  command line.

   package Builder_Compiling_Options is new GNAT.Dynamic_Tables
     (Table_Component_Type => String_Access,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 10,
      Table_Increment      => 100);
   --  Tables to store the options for the compilers of the different
   --  languages, that is those after switch "-cargs:<lang>", in the Builder
   --  switches.

   package Compiling_Options is new GNAT.Dynamic_Tables
     (Table_Component_Type => String_Access,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 10,
      Table_Increment      => 100);
   --  Tables to store the options for the compilers of the different
   --  languages, that is those after switch "-cargs:<lang>", on the command
   --  line.

   Builder_Switches_Lang : Name_Id := No_Name;
   --  Used to decide to what compiler the Builder'Default_Switches that
   --  are not recognized by gprbuild should be given.

   type Boolean_Array is array (Positive range <>) of Boolean;
   type Booleans is access Boolean_Array;

   procedure Free is new Ada.Unchecked_Deallocation (Boolean_Array, Booleans);

   Initial_Number_Of_Options : constant Natural := 10;

   type Options_Data is record
      Options     : String_List_Access :=
                      new String_List (1 .. Initial_Number_Of_Options);
      Visible     : Booleans :=
                      new Boolean_Array (1 .. Initial_Number_Of_Options);
      Simple_Name : Booleans :=
                      new Boolean_Array (1 .. Initial_Number_Of_Options);
      Last        : Natural := 0;
   end record;
   --  A record type to keep different options with a boolean for each that
   --  indicates if it should be displayed.

   All_Options : Options_Data;
   --  A cache for all options, to avoid too many allocations

   Compilation_Options : Options_Data;
   --  The compilation options coming from package Compiler

   type Comp_Option_Table_Ref is access Compiling_Options.Instance;
   No_Comp_Option_Table : constant Comp_Option_Table_Ref := null;

   Current_Comp_Option_Table : Comp_Option_Table_Ref := No_Comp_Option_Table;

   type Builder_Comp_Option_Table_Ref is
     access Builder_Compiling_Options.Instance;
   No_Builder_Comp_Option_Table : constant Builder_Comp_Option_Table_Ref :=
                                    null;

   Current_Builder_Comp_Option_Table : Builder_Comp_Option_Table_Ref :=
                                         No_Builder_Comp_Option_Table;

   package Compiling_Options_HTable is new GNAT.HTable.Simple_HTable
     (Header_Num => Prj.Header_Num,
      Element    => Comp_Option_Table_Ref,
      No_Element => No_Comp_Option_Table,
      Key        => Name_Id,
      Hash       => Prj.Hash,
      Equal      => "=");
   --  A hash table to get the command line compilation option table from the
   --  language name.

   package Builder_Compiling_Options_HTable is new GNAT.HTable.Simple_HTable
     (Header_Num => Prj.Header_Num,
      Element    => Builder_Comp_Option_Table_Ref,
      No_Element => No_Builder_Comp_Option_Table,
      Key        => Name_Id,
      Hash       => Prj.Hash,
      Equal      => "=");
   --  A hash table to get the builder compilation option table from the
   --  language name.

   package All_Language_Binder_Options is new Table.Table
     (Table_Component_Type => String_Access,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 10,
      Table_Increment      => 100,
      Table_Name           => "Makegpr.All_Language_Binder_Options");
   --  Table to store the options for all binders, that is those that
   --  follow the switch "-bargs" without any mention of language.

   package Binder_Options is new GNAT.Dynamic_Tables
     (Table_Component_Type => String_Access,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 10,
      Table_Increment      => 100);
   --  Tables to store the options for the binders of the different
   --  languages, that is those after switch "-bargs:<lang>".

   type Bind_Option_Table_Ref is access Binder_Options.Instance;
   No_Bind_Option_Table : constant Bind_Option_Table_Ref := null;

   Current_Bind_Option_Table : Bind_Option_Table_Ref := No_Bind_Option_Table;

   package Binder_Options_HTable is new GNAT.HTable.Simple_HTable
     (Header_Num => Prj.Header_Num,
      Element    => Bind_Option_Table_Ref,
      No_Element => No_Bind_Option_Table,
      Key        => Name_Id,
      Hash       => Prj.Hash,
      Equal      => "=");
   --  A hash table to get the compilation option table from the language name

   package Binding_Options is new Table.Table
     (Table_Component_Type => String_Access,
      Table_Index_Type     => Integer,
      Table_Low_Bound      => 1,
      Table_Initial        => 20,
      Table_Increment      => 100,
      Table_Name           => "Makegpr.Binding_Options");
   --  Table to store the linking options coming from the binder

   package Command_Line_Linker_Options is new Table.Table
     (Table_Component_Type => String_Access,
      Table_Index_Type     => Integer,
      Table_Low_Bound      => 1,
      Table_Initial        => 20,
      Table_Increment      => 100,
      Table_Name           => "Makegpr.Command_Line_Linker_Options");
   --  Table to store the linking options

   type Linker_Options_Data is record
      Project : Project_Id;
      Options : String_List_Id;
   end record;

   package Linker_Opts is new Table.Table
     (Table_Component_Type => Linker_Options_Data,
      Table_Index_Type     => Integer,
      Table_Low_Bound      => 1,
      Table_Initial        => 20,
      Table_Increment      => 100,
      Table_Name           => "Makegpr.Linker_Opts");
   --  Table to store the Linker'Linker_Options in the project files

   Project_Of_Current_Object_Directory : Project_Id := No_Project;
   --  The object directory of the project for the last binding. Avoid
   --  calling Change_Dir if the current working directory is already this
   --  directory.

   type Archive_Data is record
      Checked        : Boolean := False;
      Has_Been_Built : Boolean := False;
      Exists         : Boolean := False;
   end record;

   No_Archive_Data : constant Archive_Data :=
                       (Checked        => False,
                        Has_Been_Built => False,
                        Exists         => False);

   package Global_Archives_Built is new GNAT.HTable.Simple_HTable
     (Header_Num => Prj.Header_Num,
      Element    => Archive_Data,
      No_Element => No_Archive_Data,
      Key        => Name_Id,
      Hash       => Prj.Hash,
      Equal      => "=");
   --  A hash table to record what global archives have been already built

   --  Archive builder name, path and options

   Archive_Builder_Name        : String_Access := null;
   Archive_Builder_Path        : String_Access := null;
   Archive_Builder_Opts        : Options_Data;
   Archive_Builder_Append_Opts : Options_Data;

   --  Archive indexer name, path and options

   Archive_Indexer_Name : String_Access := null;
   Archive_Indexer_Path : String_Access := null;
   Archive_Indexer_Opts : Options_Data;

   type Source_Index_Rec is record
      Project : Project_Id;
      Id      : Source_Id;
      Found   : Boolean := False;
   end record;
   --  Used as Source_Indexes component to check if archive needs to be rebuilt

   type Source_Index_Array is array (Positive range <>) of Source_Index_Rec;
   type Source_Indexes_Ref is access Source_Index_Array;

   procedure Free is new Ada.Unchecked_Deallocation
     (Source_Index_Array, Source_Indexes_Ref);

   Initial_Source_Index_Count : constant Positive := 20;
   Source_Indexes : Source_Indexes_Ref :=
     new Source_Index_Array (1 .. Initial_Source_Index_Count);
   --  A list of the Source_Ids, with an indication that they have been found
   --  in the archive dependency file.

   Last_Source : Natural := 0;
   --  The index of the last valid component of Source_Indexes

   Initial_Argument_Count : constant Positive := 20;
   Arguments : Argument_List_Access :=
                 new Argument_List (1 .. Initial_Argument_Count);
   --  Used to store lists of arguments to be used when spawning a process

   Arguments_Displayed : Booleans :=
                           new Boolean_Array (1 .. Initial_Argument_Count);
   --  For each argument in Arguments, indicate if the argument should be
   --  displayed when procedure Display_Command is called.

   Arguments_Simple_Name : Booleans :=
                             new Boolean_Array (1 .. Initial_Argument_Count);
   --  For each argument that should be displayed, indicate that the argument
   --  is a path name and that only the simple name should be displayed.

   Last_Argument : Natural := 0;
   --  Index of the last valid argument in Arguments

   package Cache_Args is new Table.Table
     (Table_Component_Type => String_Access,
      Table_Index_Type     => Integer,
      Table_Low_Bound      => 1,
      Table_Initial        => 200,
      Table_Increment      => 100,
      Table_Name           => "Buildgpr.Cache_Args");
   --  A table to cache arguments, to avoid multiple allocation of the same
   --  strings. It is not possible to use a hash table, because String is
   --  an unconstrained type.

   --  Libraries

   type Library_Object is record
      Path  : Path_Name_Type;
      TS    : Time_Stamp_Type;
      Known : Boolean;
   end record;

   package Library_Objs is new Table.Table
     (Table_Component_Type => Library_Object,
      Table_Index_Type     => Integer,
      Table_Low_Bound      => 1,
      Table_Initial        => 10,
      Table_Increment      => 10,
      Table_Name           => "Buildgpr.Library_Objs");
   --  Library objects with their time stamps

   package Processed_Projects is new GNAT.HTable.Simple_HTable
     (Header_Num => Prj.Header_Num,
      Element    => Boolean,
      No_Element => False,
      Key        => Name_Id,
      Hash       => Hash,
      Equal      => "=");
   --  Projects that have already been processed

   package Library_Projs is new Table.Table (
     Table_Component_Type => Project_Id,
     Table_Index_Type     => Integer,
     Table_Low_Bound      => 1,
     Table_Initial        => 10,
     Table_Increment      => 10,
     Table_Name           => "Buildgpr.Library_Projs");
   --  Library projects imported directly or indirectly

   package Non_Library_Projs is new Table.Table (
     Table_Component_Type => Project_Id,
     Table_Index_Type     => Integer,
     Table_Low_Bound      => 1,
     Table_Initial        => 10,
     Table_Increment      => 10,
     Table_Name           => "Buildgpr.Non_Library_Projs");
   --  Non library projects imported directly or indirectly

   package Rpaths is new Table.Table
     (Table_Component_Type => String_Access,
      Table_Index_Type     => Integer,
      Table_Low_Bound      => 1,
      Table_Initial        => 200,
      Table_Increment      => 50,
      Table_Name           => "Makegpr.Rpaths");
   --  Directories to be put in the run path option

   package Naming_Datas is new Table.Table
     (Table_Component_Type => Lang_Naming_Data,
      Table_Index_Type     => Integer,
      Table_Low_Bound      => 1,
      Table_Initial        => 10,
      Table_Increment      => 100,
      Table_Name           => "Makegpr.Naming_Datas");
   --  Naming data when creating config files

   package Project_File_Paths is new GNAT.HTable.Simple_HTable
     (Header_Num => Prj.Header_Num,
      Element    => Boolean,
      No_Element => False,
      Key        => Name_Id,
      Hash       => Hash,
      Equal      => "=");

   package Imports is new GNAT.HTable.Simple_HTable
     (Header_Num => Prj.Header_Num,
      Element    => Boolean,
      No_Element => False,
      Key        => Project_Id,
      Hash       => Hash,
      Equal      => "=");
   --  When --direct-import-only is used, contains the project ids a non Ada
   --  source is allowed to import source from.

   package Included_Sources is new Table.Table
     (Table_Component_Type => Source_Id,
      Table_Index_Type     => Integer,
      Table_Low_Bound      => 1,
      Table_Initial        => 10,
      Table_Increment      => 100,
      Table_Name           => "Makegpr.Included_Sources");

   package Subunits is new Table.Table
     (Table_Component_Type => String_Access,
      Table_Index_Type     => Integer,
      Table_Low_Bound      => 1,
      Table_Initial        => 10,
      Table_Increment      => 100,
      Table_Name           => "Makegpr.Subunits");
   --  A table to store the subunit names when switch --no-split-units ia used

   package Library_Dirs is new GNAT.HTable.Simple_HTable
     (Header_Num => Prj.Header_Num,
      Element    => Boolean,
      No_Element => False,
      Key        => Path_Name_Type,
      Hash       => Hash,
      Equal      => "=");
   --  A hash table to store the library dirs, to avoid repeating uselessly
   --  the same switch when linking executables.

   type Sigint_Handler is access procedure;
   pragma Convention (C, Sigint_Handler);

   procedure Install_Int_Handler (Handler : Sigint_Handler);
   pragma Import (C, Install_Int_Handler, "__gnat_install_int_handler");
   --  Called by Gnatmake to install the SIGINT handler below

   function Absolute_Path
     (Path    : Path_Name_Type;
      Project : Project_Id) return Path_Name_Type;
   --  Returns an absolute path for a config file

   procedure Add_Argument
     (Arg         : String_Access;
      Display     : Boolean;
      Simple_Name : Boolean := False);
   procedure Add_Argument
     (Arg         : String;
      Display     : Boolean;
      Simple_Name : Boolean := False);
   --  Add an argument to Arguments. Reallocate if necessary

   procedure Add_Arguments
     (Args        : Argument_List;
      Display     : Boolean;
      Simple_Name : Boolean := False);
   --  Add a list of arguments to Arguments. Reallocate if necessary

   procedure Add_Compilation_Switches (Source : Source_Id);
   --  Add to the compilation option, the switches clared in
   --  Compiler'Switches(<source file name>), if it is defined, otherwise in
   --  Compiler'Default_Switches (<language name>), if it is defined.

   procedure Add_Option (Arg : String; Command_Line : Boolean);
   --  Add a switch for a compiler or all compilers, or for the binder or for
   --  the linker. The table where this option is stored depends on the value
   --  of Current_Processor and other global variables.

   procedure Add_Option_Internal
     (Value       : String_Access;
      To          : in out Options_Data;
      Display     : Boolean;
      Simple_Name : Boolean := False);
   procedure Add_Option
     (Value       : String;
      To          : in out Options_Data;
      Display     : Boolean;
      Simple_Name : Boolean := False);
   procedure Add_Option
     (Value       : Name_Id;
      To          : in out Options_Data;
      Display     : Boolean;
      Simple_Name : Boolean := False);
   procedure Add_Options
     (Value         : String_List_Id;
      To            : in out Options_Data;
      Display_All   : Boolean;
      Display_First : Boolean;
      Simple_Name   : Boolean := False);
   --  Add one or several options to a list of options. Increase the size
   --  of the list, if necessary.

   function Add_Global_Switches
     (Switch      : String;
      For_Lang    : Name_Id;
      For_Builder : Boolean;
      Has_Global_Compilation_Switches : Boolean) return Boolean;
   --  Take into account a global switch (builder or global compilation switch)
   --  read from the project file.

   procedure Add_Rpath (Path : String);
   --  Add a path name to Rpath

   function Archive_Suffix (For_Project : Project_Id) return String;
   --  Return the archive suffix for the project, if defined, otherwise
   --  return ".a".

   procedure Await_Compile
     (Source        : out Queue.Source_Info;
      OK            : out Boolean);
   --  Wait for the end of a compilation and indicate that the object directory
   --  is free.

   procedure Build_Global_Archive
     (For_Project    : Project_Id;
      Project_Tree   : Project_Tree_Ref;
      Has_Been_Built : out Boolean;
      Exists         : out Boolean);
   --  Build, if necessary, the global archive for a main project.
   --  Out parameter Has_Been_Built is True iff the global archive has been
   --  built/rebuilt. Exists is False if there is no need for a global archive.

   procedure Build_Library
     (For_Project  : Project_Id;
      Project_Tree : Project_Tree_Ref);
   --  Build, if necessary, the library of a library project

   procedure Change_To_Object_Directory (Project : Project_Id);
   --  Change to the object directory of project Project, if this is not
   --  already the current working directory.

   procedure Check_Archive_Builder;
   --  Check if the archive builder (ar) is there

   procedure Add_Mains_To_Queue;
   --  Check that each main is a single file name and that it is a source
   --  of a project from the tree.

   procedure Compilation_Phase
     (Main_Project : Project_Id;
      Project_Tree : Project_Tree_Ref);
   procedure Compilation_Phase;
   --  The first version compilations for a specific project tree. This needs
   --  to be called one for each aggregated projects, too.
   --  The second version will process all the main root project and all
   --  aggregated projects.

   function Config_File_For
     (Project        : Project_Id;
      Package_Name   : Name_Id;
      Attribute_Name : Name_Id;
      Language       : Name_Id)
      return Path_Name_Type;
   --  Returns the name of a config file. Returns No_Name if there is no
   --  config file

   procedure Copyright;
   --  Output the Copyright notice

   procedure Create_Config_File
     (For_Project  : Project_Id;
      Config       : Language_Config;
      Language     : Name_Id);
   --  Create a new config file

   function Create_Path_From_Dirs return String_Access;
   --  Concatenate all directories in the Directories table into a path.
   --  Caller is responsible for freeing the result

   function Directly_Imports
     (Project  : Project_Id;
      Imported : Project_Id) return Boolean;
   --  Returns True if Project directly withs Imported or a project extending
   --  Imported.

   procedure Display_Command
     (Name    : String;
      Path    : String_Access;
      Ellipse : Boolean := False);
   --  Display the command for a spawned process, if in Verbose_Mode or not in
   --  Quiet_Output. In non verbose mode, when Ellipse is True, display "..."
   --  in place of the first argument that has Display set to False.

   procedure Get_Linker_Options (For_Project : Project_Id);
   --  Get the Linker_Options from a project

   function Get_Option (Option : Name_Id) return String_Access;
   --  Get a string access corresponding to Option. Either find the string
   --  access in the All_Options cache, or create a new entry in All_Options.

   function Global_Archive_Name (For_Project : Project_Id) return String;
   --  Returns the name of the global archive for a project

   procedure Initialize;
   --  Do the necessary package intialization and process the command line
   --  arguments.

   function Is_Included_In_Global_Archive
     (Object_Name : File_Name_Type;
      Project     : Project_Id) return Boolean;
   --  Return True if the object Object_Name is not overridden by a source
   --  in a project extending project Project.

   function Is_In_Library_Project (Object_Path : String) return Boolean;
   --  Return True if Object_Path is the path of an object file in a library
   --  project.

   procedure Linking_Phase;
   --  Perform linking, if necessary, for all registered mains: main project,
   --  aggregated projects,...

   procedure Link_Main (Main_File  : Main_Info);
   --  Link a specific main unit

   procedure Post_Compilation_Phase
     (Main_Project : Project_Id; Project_Tree : Project_Tree_Ref);
   procedure Post_Compilation_Phase;
   --  Build libraries, if needed, and perform binding, if needed.
   --  This is either for a specific project tree, or for the root project and
   --  all its aggregated projects.

   procedure Process_Imported_Libraries
     (For_Project        : Project_Id;
      There_Are_SALs     : out Boolean;
      And_Project_Itself : Boolean := False);
   --  Get the imported library project ids in table Library_Projs

   procedure Process_Imported_Non_Libraries (For_Project : Project_Id);
   --  Get the imported non library project ids in table Non_Library_Projs

   function Project_Extends
     (Extending : Project_Id;
      Extended  : Project_Id) return Boolean;
   --  Returns True if Extending is Extended or is extending Extended directly
   --  or indirectly.

   procedure Rpaths_Relative_To
     (Exec_Dir : Path_Name_Type; Origin : Name_Id);
   --  Change all paths in table Rpaths to paths relative to Exec_Dir, if they
   --  have at least one non root directory in common.

   procedure Record_Failure (Source : Source_Id);
   --  Record that compilation of a source failed

   procedure Scan_Arg
     (Arg          : String;
      Command_Line : Boolean;
      Language     : Name_Id;
      Success      : out Boolean);
   --  Process one gprbuild argument Arg. Command_Line is True if the argument
   --  is specified on the command line. Optional parameter Additional gives
   --  additional information about the origin of the argument if it is found
   --  illegal.

   procedure Recursive_Import (Project : Project_Id);
   --  Add to table Imports the projects imported by Project, recursively

   procedure Sigint_Intercepted;
   pragma Convention (C, Sigint_Intercepted);
   --  Called when the program is interrupted by Ctrl-C to delete the
   --  temporary mapping files and configuration pragmas files.

   procedure Test_If_Relative_Path
     (Switch           : in out String_Access;
      Parent           : String;
      Including_Switch : Name_Id);

   procedure Usage;
   --  Display the usage

   procedure Check_Version_And_Help is new
     Check_Version_And_Help_G (Usage);

   -------------------
   -- Absolute_Path --
   -------------------

   function Absolute_Path
     (Path    : Path_Name_Type;
      Project : Project_Id) return Path_Name_Type
   is
   begin
      Get_Name_String (Path);

      if not Is_Absolute_Path (Name_Buffer (1 .. Name_Len)) then
         Get_Name_String (Project.Directory.Display_Name);
         if Name_Buffer (Name_Len) /= Directory_Separator then
            Add_Char_To_Name_Buffer (Directory_Separator);
         end if;

         Add_Str_To_Name_Buffer (Get_Name_String (Path));
      end if;

      return Name_Find;
   end Absolute_Path;

   ------------------
   -- Add_Argument --
   ------------------

   procedure Add_Argument
     (Arg         : String_Access;
      Display     : Boolean;
      Simple_Name : Boolean := False)
   is
   begin
      --  Nothing to do if no argument is specified or if argument is empty

      if Arg /= null and then Arg'Length /= 0 then

         --  Reallocate arrays if necessary

         if Last_Argument = Arguments'Last then
            declare
               New_Arguments : constant Argument_List_Access :=
                                 new Argument_List
                                   (1 .. Last_Argument +
                                           Initial_Argument_Count);

               New_Arguments_Displayed : constant Booleans :=
                                           new Boolean_Array
                                             (1 .. Last_Argument +
                                                     Initial_Argument_Count);

               New_Arguments_Simple_Name : constant Booleans :=
                                             new Boolean_Array
                                               (1 .. Last_Argument +
                                                       Initial_Argument_Count);

            begin
               New_Arguments (Arguments'Range) := Arguments.all;

               --  To avoid deallocating the strings, nullify all components
               --  of Arguments before calling Free.

               Arguments.all := (others => null);

               Free (Arguments);
               Arguments := New_Arguments;

               New_Arguments_Displayed (Arguments_Displayed'Range) :=
                 Arguments_Displayed.all;
               Free (Arguments_Displayed);
               Arguments_Displayed := New_Arguments_Displayed;

               New_Arguments_Simple_Name (Arguments_Simple_Name'Range) :=
                 Arguments_Simple_Name.all;
               Free (Arguments_Simple_Name);
               Arguments_Simple_Name := New_Arguments_Simple_Name;
            end;
         end if;

         --  Add the argument and its display indication

         Last_Argument := Last_Argument + 1;
         Arguments (Last_Argument) := Arg;
         Arguments_Displayed (Last_Argument) := Display;
         Arguments_Simple_Name (Last_Argument) := Simple_Name;
      end if;
   end Add_Argument;

   procedure Add_Argument
     (Arg         : String;
      Display     : Boolean;
      Simple_Name : Boolean := False)
   is
      Argument : String_Access := null;

   begin
      --  Nothing to do if argument is empty

      if Arg'Length > 0 then

         --  Check if the argument is already in the Cache_Args table. If it is
         --  already there, reuse the allocated value.

         for Index in 1 .. Cache_Args.Last loop
            if Cache_Args.Table (Index).all = Arg then
               Argument := Cache_Args.Table (Index);
               exit;
            end if;
         end loop;

         --  If the argument is not in the cache, create a new entry in the
         --  cache.

         if Argument = null then
            Argument := new String'(Arg);
            Cache_Args.Increment_Last;
            Cache_Args.Table (Cache_Args.Last) := Argument;
         end if;

         --  And add the argument

         Add_Argument (Argument, Display, Simple_Name);
      end if;
   end Add_Argument;

   -------------------
   -- Add_Arguments --
   -------------------

   procedure Add_Arguments
     (Args        : Argument_List;
      Display     : Boolean;
      Simple_Name : Boolean := False)
   is
   begin
      --  Reallocate the arrays, if necessary

      if Last_Argument + Args'Length > Arguments'Last then
         declare
            New_Arguments : constant Argument_List_Access :=
                              new Argument_List
                                    (1 .. Last_Argument + Args'Length +
                                          Initial_Argument_Count);

            New_Arguments_Displayed : constant Booleans :=
                                        new Boolean_Array
                                              (1 .. Last_Argument +
                                                    Args'Length +
                                                    Initial_Argument_Count);

         begin
            New_Arguments (1 .. Last_Argument) :=
              Arguments (1 .. Last_Argument);

            --  To avoid deallocating the strings, nullify all components
            --  of Arguments before calling Free.

            Arguments.all := (others => null);
            Free (Arguments);

            Arguments := New_Arguments;
            New_Arguments_Displayed (1 .. Last_Argument) :=
              Arguments_Displayed (1 .. Last_Argument);
            Free (Arguments_Displayed);
            Arguments_Displayed := New_Arguments_Displayed;
         end;
      end if;

      --  Add the new arguments and the display indications

      Arguments (Last_Argument + 1 .. Last_Argument + Args'Length) := Args;
      Arguments_Displayed (Last_Argument + 1 .. Last_Argument + Args'Length) :=
        (others => Display);
      Arguments_Simple_Name (Last_Argument + 1 .. Last_Argument + Args'Length)
        := (others => Simple_Name);
      Last_Argument := Last_Argument + Args'Length;
   end Add_Arguments;

   ------------------------------
   -- Add_Compilation_Switches --
   ------------------------------

   procedure Add_Compilation_Switches (Source : Source_Id) is
      Options : Variable_Value;
      Is_Default : Boolean;
   begin
      Makeutl.Get_Switches
        (Source, Name_Compiler, Project_Tree, Options, Is_Default);
      if Options /= Nil_Variable_Value then
         Add_Options
           (Options.Values,
            To            => Compilation_Options,
            Display_All   => True,
            Display_First => True);
      end if;
   end Add_Compilation_Switches;

   ----------------
   -- Add_Option --
   ----------------

   procedure Add_Option (Arg : String; Command_Line : Boolean) is
      Option : String_Access := new String'(Arg);

   begin
      case Current_Processor is
         when None =>
            null;

         when Linker =>

            --  Add option to the linker table

            if Command_Line then
               Test_If_Relative_Path
                 (Switch           => Option,
                  Parent           => Current_Working_Dir,
                  Including_Switch => Dash_L);

            else
               Test_If_Relative_Path
                 (Switch           => Option,
                  Parent           => Main_Project_Dir.all,
                  Including_Switch => Dash_L);
            end if;

            Command_Line_Linker_Options.Append (Option);

         when Binder =>

            if Command_Line then
               Test_If_Relative_Path
                 (Switch           => Option,
                  Parent           => Current_Working_Dir,
                  Including_Switch => No_Name);

            else
               Test_If_Relative_Path
                 (Switch           => Option,
                  Parent           => Main_Project_Dir.all,
                  Including_Switch => No_Name);
            end if;

            if Current_Bind_Option_Table = No_Bind_Option_Table then
               --  Option for all binder

               All_Language_Binder_Options.Append (Option);

            else
               --  Option for a single binder

               Binder_Options.Append
                 (Current_Bind_Option_Table.all, Option);
            end if;

         when Compiler =>

            if Command_Line then
               if Current_Comp_Option_Table = No_Comp_Option_Table then
                  --  Option for all compilers

                  All_Language_Compiling_Options.Append (Option);

               else
                  --  Option for a single compiler

                  Compiling_Options.Append
                    (Current_Comp_Option_Table.all, Option);
               end if;

            else
               if Current_Builder_Comp_Option_Table =
                    No_Builder_Comp_Option_Table
               then
                  --  Option for all compilers

                  All_Language_Builder_Compiling_Options.Append (Option);

               else
                  --  Option for a single compiler

                  Builder_Compiling_Options.Append
                    (Current_Builder_Comp_Option_Table.all, Option);
               end if;
            end if;
      end case;
   end Add_Option;

   procedure Add_Option
     (Value       : String;
      To          : in out Options_Data;
      Display     : Boolean;
      Simple_Name : Boolean := False)
   is
   begin
      Name_Len := Value'Length;
      Name_Buffer (1 .. Name_Len) := Value;
      Add_Option_Internal (Get_Option (Name_Find), To, Display, Simple_Name);
   end Add_Option;

   procedure Add_Option
     (Value       : Name_Id;
      To          : in out Options_Data;
      Display     : Boolean;
      Simple_Name : Boolean := False)
   is
   begin
      Add_Option_Internal (Get_Option (Value), To, Display, Simple_Name);
   end Add_Option;

   -------------------------
   -- Add_Option_Internal --
   -------------------------

   procedure Add_Option_Internal
     (Value       : String_Access;
      To          : in out Options_Data;
      Display     : Boolean;
      Simple_Name : Boolean := False)
   is
   begin
      --  For compatibility with gnatmake, do not consider empty options

      if Value'Length = 0 then
         return;
      end if;

      To.Last := To.Last + 1;

      if To.Last > To.Options'Last then
         declare
            New_Options     : constant String_List_Access :=
                                new String_List (1 .. 2 * To.Options'Last);
            New_Visible     : constant Booleans :=
                                new Boolean_Array (1 .. 2 * To.Visible'Last);
            New_Simple_Name : constant Booleans :=
                                new Boolean_Array (1 .. 2 * To.Visible'Last);

         begin
            New_Options (To.Options'Range) := To.Options.all;
            To.Options.all := (others => null);
            Free (To.Options);
            To.Options := New_Options;
            New_Visible (To.Visible'Range) := To.Visible.all;
            Free (To.Visible);
            To.Visible := New_Visible;
            New_Simple_Name (To.Simple_Name'Range) := To.Simple_Name.all;
            Free (To.Simple_Name);
            To.Simple_Name := New_Simple_Name;
         end;
      end if;

      To.Options (To.Last)     := Value;
      To.Visible (To.Last)     := Display;
      To.Simple_Name (To.Last) := Simple_Name;
   end Add_Option_Internal;

   -----------------
   -- Add_Options --
   -----------------

   procedure Add_Options
     (Value         : String_List_Id;
      To            : in out Options_Data;
      Display_All   : Boolean;
      Display_First : Boolean;
      Simple_Name   : Boolean := False)
   is
      List            : String_List_Id := Value;
      Element         : String_Element;
      Option          : String_Access;
      First_Display   : Boolean := Display_First;
   begin
      while List /= Nil_String loop
         Element := Project_Tree.Shared.String_Elements.Table (List);

         --  Ignore empty options

         if Element.Value /= Empty_String then
            Option := Get_Option (Element.Value);

            Add_Option_Internal
              (Value       => Option,
               To          => To,
               Display     => Display_All or First_Display,
               Simple_Name => Simple_Name);
            First_Display := False;
         end if;

         List := Element.Next;
      end loop;
   end Add_Options;

   ---------------
   -- Add_Rpath --
   ---------------

   procedure Add_Rpath (Path : String) is
   begin
      --  Nothing to do if Path is empty

      if Path'Length > 0 then
         --  Nothing to do if the directory is already in the Rpaths table

         for J in 1 .. Rpaths.Last loop
            if Rpaths.Table (J).all = Path then
               return;
            end if;
         end loop;

         Rpaths.Append (new String'(Path));
      end if;
   end Add_Rpath;

   --------------------
   -- Archive_Suffix --
   --------------------

   function Archive_Suffix (For_Project : Project_Id) return String is
   begin
      if For_Project.Config.Archive_Suffix = No_File then
         return ".a";

      else
         return Get_Name_String (For_Project.Config.Archive_Suffix);
      end if;
   end Archive_Suffix;

   -------------------
   -- Await_Compile --
   -------------------

   procedure Await_Compile
     (Source : out Queue.Source_Info;
      OK     : out Boolean)
   is
      Pid       : Process_Id;
      Comp_Data : Process_Data;
      Language  : Language_Ptr;
      Config    : Language_Config;

   begin
      loop
         Source := Queue.No_Source_Info;

         Wait_Process (Pid, OK);

         if Pid = Invalid_Pid then
            return;
         end if;

         Comp_Data := Compilation_Htable.Get (Pid);

         if Comp_Data /= No_Process_Data then
            Source := Comp_Data.Source;

            Queue.Set_Obj_Dir_Free (Source.Id.Project.Object_Directory.Name);

            if Comp_Data.Purpose = Compilation then

               if OK then
                  --  We created a new ALI file, so reset the attributes of
                  --  the old one.

                  Source.Id.Dep_TS := Unknown_Attributes;

                  if Comp_Data.Options /= null
                    and then Source.Id.Switches_Path /= No_Path
                    and then Opt.Check_Switches
                  then
                     --  First, update the time stamp of the object file
                     --  that wil be written in the switches file.

                     Source.Id.Object_TS := File_Stamp (Source.Id.Object_Path);

                     --  Write the switches file, now that we have the
                     --  updated time stamp for the object file.

                     declare
                        File : Ada.Text_IO.File_Type;

                     begin
                        Create
                          (File,
                           Out_File,
                           Get_Name_String (Source.Id.Switches_Path));

                        Put_Line (File, String (Source.Id.Object_TS));

                        for J in Comp_Data.Options'Range loop
                           Put_Line (File, Comp_Data.Options (J).all);
                        end loop;

                        Close (File);

                     exception
                        when others =>
                           Fail_Program
                             (Source.Tree,
                              "could not create switches file """ &
                              Get_Name_String (Source.Id.Switches_Path) & '"');
                     end;

                     --  For all languages other than Ada, update the time
                     --  stamp of the object file as it is written in the
                     --  global archive dependency file. For all languages,
                     --  update the time stamp of the object file if it is
                     --  in a library project.

                  elsif Source.Id.Language.Config.Dependency_Kind /= ALI_File
                    or else Source.Id.Project.Library
                  then
                     Source.Id.Object_TS := File_Stamp (Source.Id.Object_Path);
                  end if;

               else
                  Set_Failed_Compilation_Status (Comp_Data.Source_Project);
               end if;

               Language := Source.Id.Language;

               --  If there is a mapping file used, recycle it in the hash
               --  table of the language.

               if Comp_Data.Mapping_File /= No_Path
                 and then Language /= No_Language_Index
               then
                  Mapping_Files_Htable.Set
                    (T => Language.Mapping_Files,
                     K => Comp_Data.Mapping_File,
                     E => Comp_Data.Mapping_File);
               end if;

               Config := Language.Config;

               if Config.Dependency_Kind = Makefile
                 and then Config.Compute_Dependency /= No_Name_List
               then
                  declare
                     List      : Name_List_Index :=
                                   Config.Compute_Dependency;
                     Nam       : Name_Node :=
                                   Source.Tree.Shared.Name_Lists.Table (List);
                     Exec_Name : constant String :=
                                   Get_Name_String (Nam.Name);
                     Exec_Path : String_Access;
                  begin
                     Comp_Data.Mapping_File := No_Path;
                     Comp_Data.Purpose := Dependency;

                     --  ??? We search for it on the PATH for every file,
                     --  this is very inefficient
                     Exec_Path := Locate_Exec_On_Path (Exec_Name);

                     if Exec_Path = null then
                        Fail_Program
                          (Source.Tree,
                           "unable to find dependency builder " &
                           Exec_Name);
                     end if;

                     List := Nam.Next;
                     Compilation_Options.Last := 0;

                     if List = No_Name_List then
                        Name_Len := 0;

                     else
                        loop
                           Nam := Source.Tree.Shared.Name_Lists.Table (List);
                           List := Nam.Next;

                           if List = No_Name_List then
                              Get_Name_String (Nam.Name);
                              exit;
                           end if;

                           Add_Option
                             (Nam.Name, Compilation_Options, Opt.Verbose_Mode);
                        end loop;
                     end if;

                     Add_Str_To_Name_Buffer
                       (Get_Name_String (Source.Id.Path.Display_Name));
                     Add_Option
                       (Name_Buffer (1 .. Name_Len),
                        Compilation_Options,
                        Opt.Verbose_Mode,
                        Simple_Name => not Opt.Verbose_Mode);

                     if not Opt.Quiet_Output then
                        if Opt.Verbose_Mode then
                           Write_Str (Exec_Path.all);
                        else
                           Write_Str (Exec_Name);
                        end if;

                        Write_Str (" ");

                        for Option in 1 .. Compilation_Options.Last loop
                           if Compilation_Options.Visible (Option) then
                              Write_Str
                                (Compilation_Options.Options (Option).all);
                              Write_Str (" ");
                           end if;
                        end loop;

                        Write_Eol;
                     end if;

                     Comp_Data.Pid :=
                       GNAT.OS_Lib.Non_Blocking_Spawn
                         (Program_Name => Exec_Path.all,
                          Args         =>
                            Compilation_Options.Options
                              (1 .. Compilation_Options.Last),
                          Output_File  => Get_Name_String (Source.Id.Dep_Path),
                          Err_To_Out   => True);

                     Compilation_Htable.Set (Comp_Data.Pid, Comp_Data);

                     Free (Exec_Path);
                  end;

               else
                  Outstanding_Compiles := Outstanding_Compiles - 1;
                  return;
               end if;

            elsif Comp_Data.Purpose = Dependency then
               Outstanding_Compiles := Outstanding_Compiles - 1;
               return;
            end if;
         end if;
      end loop;
   end Await_Compile;

   --------------------------
   -- Build_Global_Archive --
   --------------------------

   procedure Build_Global_Archive
     (For_Project    : Project_Id;
      Project_Tree   : Project_Tree_Ref;
      Has_Been_Built : out Boolean;
      Exists         : out Boolean)
   is
      Archive_Name : constant String :=
                       "lib" &
                       Get_Name_String (For_Project.Name) &
                       Archive_Suffix (For_Project);
      --  The name of the archive file for this project

      Archive_Dep_Name : constant String :=
        "lib" & Get_Name_String (For_Project.Name) & ".deps";
      --  The name of the archive dependency file for this project

      File : Prj.Util.Text_File;

      Object_Path  : Path_Name_Type;
      Time_Stamp   : Time_Stamp_Type;

      First_Object : Natural;

      Discard : Boolean;

      Proj_List    : Project_List;

      Src_Id       : Source_Id;
      S_Id         : Source_Id;

      Success      : Boolean;

      Real_Last_Argument : Positive;
      Current_Object_Pos : Positive;

      Size : Natural;

      Global_Archive_Data : Archive_Data;

      Need_To_Build : Boolean;

      procedure Add_Sources (Proj : Project_Id);
      --  Add all the sources of project Proj to Sources_Index

      procedure Add_Objects (Proj : Project_Id);
      --  Add all the object paths of project Proj to Arguments

      -----------------
      -- Add_Sources --
      -----------------

      procedure Add_Sources (Proj : Project_Id) is
         Project : Project_Id := Proj;
         Id      : Source_Id;
         Iter    : Source_Iterator;

         procedure Add_Source_Id (Project : Project_Id; Id : Source_Id);
         --  Add a source id to Source_Indexes, with Found set to False

         -------------------
         -- Add_Source_Id --
         -------------------

         procedure Add_Source_Id (Project : Project_Id; Id : Source_Id) is
         begin
            --  Reallocate the array, if necessary

            if Last_Source = Source_Indexes'Last then
               declare
                  New_Indexes : constant Source_Indexes_Ref :=
                                  new Source_Index_Array
                                    (1 .. Source_Indexes'Last +
                                                   Initial_Source_Index_Count);
               begin
                  New_Indexes (Source_Indexes'Range) := Source_Indexes.all;
                  Free (Source_Indexes);
                  Source_Indexes := New_Indexes;
               end;
            end if;

            Last_Source := Last_Source + 1;
            Source_Indexes (Last_Source) := (Project, Id, False);
         end Add_Source_Id;

      begin
         while Project /= No_Project loop
            Iter := For_Each_Source (Project_Tree, Project);
            loop
               Id := Prj.Element (Iter);
               exit when Id = No_Source;

               if Is_Compilable (Id)
                 and then Id.Kind = Impl
                 and then Id.Unit = No_Unit_Index
               then
                  Add_Source_Id (Proj, Id);
               end if;

               Next (Iter);
            end loop;

            Project := Project.Extends;
         end loop;
      end Add_Sources;

      -----------------
      -- Add_Objects --
      -----------------

      procedure Add_Objects (Proj : Project_Id) is
         Project : Project_Id := Proj;
         Id      : Source_Id;
         Iter    : Source_Iterator;

      begin
         loop
            if Project.Object_Directory /= No_Path_Information then
               if Project.Externally_Built then
                  --  If project is externally built, include all object files
                  --  in the object directory in the global archive.

                  declare
                     Obj_Dir : constant String :=
                                 Get_Name_String
                                   (Project.Object_Directory.Display_Name);
                     Dir_Obj : Dir_Type;

                  begin
                     Open (Dir_Obj, Obj_Dir);

                     loop
                        Read (Dir_Obj, Name_Buffer, Name_Len);
                        exit when Name_Len = 0;

                        Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));

                        if Name_Len > Object_Suffix'Length and then
                          Name_Buffer
                            (Name_Len - Object_Suffix'Length + 1 .. Name_Len) =
                            Object_Suffix
                        then
                           Add_Argument
                             (Obj_Dir & Directory_Separator &
                              Name_Buffer (1 .. Name_Len),
                              Opt.Verbose_Mode,
                              Simple_Name => not Opt.Verbose_Mode);
                        end if;
                     end loop;

                     Close (Dir_Obj);
                  end;

               else
                  Iter := For_Each_Source (Project_Tree, Project);
                  loop
                     Id := Prj.Element (Iter);
                     exit when Id = No_Source;

                     if Object_To_Global_Archive (Id) then
                        Add_Argument
                          (Get_Name_String (Id.Object_Path),
                           Opt.Verbose_Mode,
                           Simple_Name => not Opt.Verbose_Mode);
                     end if;

                     Next (Iter);
                  end loop;
               end if;
            end if;

            Project := Project.Extends;

            exit when Project = No_Project;
         end loop;
      end Add_Objects;

   begin
      Exists := False;
      Has_Been_Built := False;

      --  No need to build the global archive, if it has already been done

      if For_Project.Object_Directory /= No_Path_Information then
         Global_Archive_Data := Global_Archives_Built.Get (For_Project.Name);

         if Global_Archive_Data.Checked then
            Exists         := Global_Archive_Data.Exists;
            Has_Been_Built := Global_Archive_Data.Has_Been_Built;

         else
            Change_To_Object_Directory (For_Project);

            --  Put all non Ada sources in the project tree in Source_Indexes

            Last_Source := 0;

            Add_Sources (For_Project);

            Proj_List := For_Project.All_Imported_Projects;

            while Proj_List /= null loop
               if not Proj_List.Project.Library then
                  Add_Sources (Proj_List.Project);
               end if;

               Proj_List := Proj_List.Next;
            end loop;

            Need_To_Build := Opt.Force_Compilations;

            if not Need_To_Build then
               if Opt.Verbose_Mode then
                  Write_Str  ("   Checking ");
                  Write_Str  (Archive_Name);
                  Write_Line (" ...");
               end if;

               --  If the archive does not exist, of course it needs to be
               --  built.

               if not Is_Regular_File (Archive_Name) then
                  Need_To_Build := True;

                  if Opt.Verbose_Mode then
                     Write_Line ("      -> archive does not exist");
                  end if;

               else
                  --  Archive does exist

                  --  Check the archive dependency file

                  Open (File, Archive_Dep_Name);

                  --  If the archive dependency file does not exist, we need to
                  --  to rebuild the archive and to create its dependency file.

                  if not Is_Valid (File) then
                     Need_To_Build := True;

                     if Opt.Verbose_Mode then
                        Write_Str  ("      -> archive dependency file ");
                        Write_Str  (Archive_Dep_Name);
                        Write_Line (" does not exist");
                     end if;

                  else
                     --  Read the dependency file, line by line

                     while not End_Of_File (File) loop
                        Get_Line (File, Name_Buffer, Name_Len);

                        --  First line is the path of the object file

                        Object_Path := Name_Find;
                        Src_Id := No_Source;

                        --  Check if this object file is for a source of this
                        --  project.

                        for S in 1 .. Last_Source loop
                           S_Id := Source_Indexes (S).Id;

                           if (not Source_Indexes (S).Found)
                             and then S_Id.Object_Path = Object_Path
                           then
                              --  We have found the object file: get the
                              --  source data, and mark it as found.

                              Src_Id := S_Id;
                              Source_Indexes (S).Found := True;
                              exit;
                           end if;
                        end loop;

                        --  If it is not for a source of this project, then the
                        --  archive needs to be rebuilt.

                        if Src_Id = No_Source then
                           Need_To_Build := True;
                           if Opt.Verbose_Mode then
                              Write_Str  ("      -> ");
                              Write_Str  (Get_Name_String (Object_Path));
                              Write_Line (" is not an object of any project");
                           end if;

                           exit;
                        end if;

                        --  The second line is the time stamp of the object
                        --  file. If there is no next line, then the dependency
                        --  file is truncated, and the archive need to be
                        --  rebuilt.

                        if End_Of_File (File) then
                           Need_To_Build := True;

                           if Opt.Verbose_Mode then
                              Write_Str  ("      -> archive dependency file ");
                              Write_Line (" is truncated");
                           end if;

                           exit;
                        end if;

                        Get_Line (File, Name_Buffer, Name_Len);

                        --  If the line has the wrong number of characters,
                        --  then the dependency file is incorrectly formatted,
                        --  and the archive needs to be rebuilt.

                        if Name_Len /= Time_Stamp_Length then
                           Need_To_Build := True;

                           if Opt.Verbose_Mode then
                              Write_Str  ("      -> archive dependency file ");
                              Write_Line
                                (" is incorrectly formatted (time stamp)");
                           end if;

                           exit;
                        end if;

                        Time_Stamp :=
                          Time_Stamp_Type (Name_Buffer (1 .. Name_Len));

                        --  If the time stamp in the dependency file is
                        --  different from the time stamp of the object file,
                        --  then the archive needs to be rebuilt. The
                        --  comparaison is done with String type values,
                        --  because two values of type Time_Stamp_Type are
                        --  equal if they differ by 2 seconds or less; here the
                        --  check is for an exact match.

                        if String (Time_Stamp) /=
                          String (Src_Id.Object_TS)
                        then
                           Need_To_Build := True;

                           if Opt.Verbose_Mode then
                              Write_Str  ("      -> time stamp of ");
                              Write_Str  (Get_Name_String (Object_Path));
                              Write_Str  (" is incorrect in the archive");
                              Write_Line (" dependency file");
                              Write_Str  ("         recorded time stamp: ");
                              Write_Line (String (Time_Stamp));
                              Write_Str  ("           actual time stamp: ");
                              Write_Line (String (Src_Id.Object_TS));
                           end if;

                           exit;

                        elsif Debug_Flag_T then
                           Write_Str  ("      -> time stamp of ");
                           Write_Str  (Get_Name_String (Object_Path));
                           Write_Str  (" is correct in the archive");
                           Write_Line (" dependency file");
                           Write_Str  ("         recorded time stamp: ");
                           Write_Line (String (Time_Stamp));
                           Write_Str  ("           actual time stamp: ");
                           Write_Line (String (Src_Id.Object_TS));
                        end if;
                     end loop;

                     Close (File);
                  end if;
               end if;
            end if;

            if not Need_To_Build then
               for S in 1 .. Last_Source loop
                  if not Source_Indexes (S).Found
                    and then Object_To_Global_Archive (Source_Indexes (S).Id)
                  then
                     Need_To_Build := True;

                     if Opt.Verbose_Mode then
                        Write_Str ("      -> object file ");
                        Write_Str (Get_Name_String
                                   (Source_Indexes (S).Id.Object_Path));
                        Write_Line (" is not in the dependency file");
                     end if;

                     exit;
                  end if;
               end loop;
            end if;

            if not Need_To_Build then
               if Opt.Verbose_Mode then
                  Write_Line  ("      -> up to date");
               end if;

               Exists         := True;
               Has_Been_Built := False;

               --  Archive needs to be rebuilt

            else
               Check_Archive_Builder;

               --  If archive already exists, first delete it, but if this is
               --  not possible, continue: if archive cannot be built, we will
               --  fail later on.

               if Is_Regular_File (Archive_Name) then
                  Delete_File (Archive_Name, Discard);
               end if;

               Last_Argument := 0;

               --  Start with the minimal options

               Add_Arguments
                 (Archive_Builder_Opts.Options
                    (1 .. Archive_Builder_Opts.Last),
                  True);

               --  Followed by the archive name

               Add_Argument
                 (Archive_Name, True, Simple_Name => not Opt.Verbose_Mode);

               First_Object := Last_Argument + 1;

               --  Followed by all the object files of the non library projects

               Add_Objects (For_Project);

               Proj_List := For_Project.All_Imported_Projects;

               while Proj_List /= null loop
                  if not Proj_List.Project.Library then
                     Add_Objects (Proj_List.Project);
                  end if;

                  Proj_List := Proj_List.Next;
               end loop;

               --  No global archive, if there is no object file to put into

               if Last_Argument < First_Object then
                  Has_Been_Built := False;
                  Exists         := False;

                  if Opt.Verbose_Mode then
                     Write_Line ("      -> there is no global archive");
                  end if;

               else
                  Real_Last_Argument := Last_Argument;

                  --  If there is an Archive_Builder_Append_Option, we may have
                  --  to build the archive in chuck.

                  if Archive_Builder_Append_Opts.Last = 0 then
                     Current_Object_Pos := Real_Last_Argument + 1;

                  else
                     Size := 0;
                     for J in 1 .. First_Object - 1 loop
                        Size := Size + Arguments (J)'Length + 1;
                     end loop;

                     Current_Object_Pos := First_Object;
                     while Current_Object_Pos <= Real_Last_Argument loop
                        Size :=
                          Size + Arguments (Current_Object_Pos)'Length + 1;
                        exit when Size > Maximum_Size;
                        Current_Object_Pos := Current_Object_Pos + 1;
                     end loop;

                     Last_Argument := Current_Object_Pos - 1;
                  end if;

                  Display_Command
                    (Archive_Builder_Name.all,
                     Archive_Builder_Path,
                     Ellipse => True);

                  Spawn
                    (Archive_Builder_Path.all,
                     Arguments (1 .. Last_Argument),
                     Success);

                  --  If the archive has not been built completely, add the
                  --  remaining chunks.

                  if Success
                    and then Current_Object_Pos <= Real_Last_Argument
                  then
                     Last_Argument := 0;
                     Add_Arguments
                       (Archive_Builder_Append_Opts.Options
                          (1 .. Archive_Builder_Append_Opts.Last),
                        True);
                     Add_Argument
                       (Archive_Name, True,
                        Simple_Name => not Opt.Verbose_Mode);
                     First_Object := Last_Argument + 1;

                     while Current_Object_Pos <= Real_Last_Argument loop
                        Size := 0;
                        for J in 1 .. First_Object - 1 loop
                           Size := Size + Arguments (J)'Length + 1;
                        end loop;

                        Last_Argument := First_Object - 1;

                        while Current_Object_Pos <= Real_Last_Argument loop
                           Size :=
                             Size + Arguments (Current_Object_Pos)'Length + 1;
                           exit when Size > Maximum_Size;
                           Last_Argument := Last_Argument + 1;
                           Arguments (Last_Argument) :=
                             Arguments (Current_Object_Pos);
                           Current_Object_Pos := Current_Object_Pos + 1;
                        end loop;

                        Display_Command
                          (Archive_Builder_Name.all,
                           Archive_Builder_Path,
                           Ellipse => True);

                        Spawn
                          (Archive_Builder_Path.all,
                           Arguments (1 .. Last_Argument),
                           Success);

                        exit when not Success;
                     end loop;
                  end if;

                  --  If the archive was built, run the archive indexer
                  --  (ranlib) if there is one.

                  if Success then

                     --  If the archive was built, run the archive indexer
                     --  (ranlib), if there is one.

                     if Archive_Indexer_Path /= null then
                        Last_Argument := 0;
                        Add_Arguments
                          (Archive_Indexer_Opts.Options
                             (1 .. Archive_Indexer_Opts.Last),
                           True);
                        Add_Argument
                          (Archive_Name,
                           True,
                           Simple_Name => not Opt.Verbose_Mode);

                        Display_Command
                          (Archive_Indexer_Name.all, Archive_Indexer_Path);

                        Spawn
                          (Archive_Indexer_Path.all,
                           Arguments (1 .. Last_Argument),
                           Success);

                        if not Success then

                           --  Running the archive indexer failed, delete the
                           --  dependency file, if it exists.

                           if Is_Regular_File (Archive_Dep_Name) then
                              Delete_File (Archive_Dep_Name, Success);
                           end if;

                           Fail_Program
                             (Project_Tree,
                              "global archive could not be built");
                           return;
                        end if;
                     end if;

                     --  The archive was correctly built, create its dependency
                     --  file.

                     declare
                        Dep_File : Ada.Text_IO.File_Type;

                     begin
                        --  Create the file in Append mode, to avoid automatic
                        --  insertion of an end of line if file is empty.

                        Create (Dep_File, Append_File, Archive_Dep_Name);

                        for S in 1 .. Last_Source loop
                           Src_Id := Source_Indexes (S).Id;
                           if Object_To_Global_Archive (Src_Id) then
                              Put_Line
                                (Dep_File,
                                 Get_Name_String (Src_Id.Object_Path));
                              Put_Line (Dep_File, String (Src_Id.Object_TS));
                           end if;
                        end loop;

                        Close (Dep_File);

                     exception
                        when others =>
                           if Is_Open (Dep_File) then
                              Close (Dep_File);
                           end if;
                     end;

                     Has_Been_Built := True;
                     Exists         := True;

                  else
                     --  Building the archive failed, delete dependency file if
                     --  one exists.

                     if Is_Regular_File (Archive_Dep_Name) then
                        Delete_File (Archive_Dep_Name, Success);
                     end if;

                     Fail_Program
                       (Project_Tree,
                        "global archive could not be built");

                     Has_Been_Built := False;
                     Exists         := True;

                     return;
                  end if;
               end if;
            end if;

            Global_Archives_Built.Set
              (For_Project.Name,
               (Checked        => True,
                Has_Been_Built => Has_Been_Built,
                Exists         => Exists));
         end if;
      end if;
   end Build_Global_Archive;

   -------------------
   -- Build_Library --
   -------------------

   procedure Build_Library
     (For_Project  : Project_Id;
      Project_Tree : Project_Tree_Ref)
   is
      Object_Directory_Path : constant String :=
        Get_Name_String (For_Project.Object_Directory.Display_Name);

      Project_Name          : constant String :=
        Get_Name_String (For_Project.Name);

      Current_Dir           : constant String := Get_Current_Dir;

      Exchange_File : Ada.Text_IO.File_Type;

      Exchange_File_Name : String_Access;

      Latest_Object_TS : Time_Stamp_Type := Empty_Time_Stamp;

      Library_Builder_Name : String_Access;
      Library_Builder      : String_Access;

      Toolchain_Version_Label_Written : Boolean;
      Lang_Index : Language_Ptr;

      Leading_Library_Options : Variable_Value := Nil_Variable_Value;
      Library_Options : Variable_Value := Nil_Variable_Value;

      Library_Needs_To_Be_Built : Boolean := False;

      Object_Path : Path_Name_Type;
      Object_TS   : Time_Stamp_Type;

      Source      : Source_Id;
      Project     : Project_Id;

      Disregard : Boolean;

      Path_Found : Boolean;

      Iter : Source_Iterator;

      procedure Get_Objects;
      --  Get the paths of the object files of the library in table
      --  Library_Objs.

      -----------------
      -- Get_Objects --
      -----------------

      procedure Get_Objects is
         Source : Source_Id;
         Iter   : Source_Iterator;
         Proj   : Project_Id := For_Project;

      begin
         Library_Objs.Init;

         loop
            Iter := For_Each_Source (Project_Tree, Proj);
            loop
               Source := Prj.Element (Iter);
               exit when Source = No_Source;

               Initialize_Source_Record (Source);

               if Is_Compilable (Source)
                 and then Source.Language.Config.Objects_Linked
                 and then
                   ((Source.Unit = No_Unit_Index
                     and then Source.Kind = Impl)
                    or else
                      (Source.Unit /= No_Unit_Index
                       and then (Source.Kind = Impl
                                 or else Other_Part (Source) = No_Source)
                       and then not Is_Subunit (Source)))
               then
                  Library_Objs.Append
                    ((Path  => Source.Object_Path,
                      TS    => Source.Object_TS,
                      Known => False));

                  if Source.Object_TS = Empty_Time_Stamp then
                     Latest_Object_TS := Never;

                     if not Library_Needs_To_Be_Built then
                        Library_Needs_To_Be_Built := True;

                        if Opt.Verbose_Mode then
                           Write_Str ("      -> missing object file: ");
                           Get_Name_String (Source.Object);
                           Write_Line (Name_Buffer (1 .. Name_Len));
                        end if;
                     end if;

                  elsif Source.Object_TS > Latest_Object_TS then
                     Latest_Object_TS := Source.Object_TS;
                  end if;
               end if;

               Next (Iter);
            end loop;

            Proj := Proj.Extends;
            exit when Proj = No_Project;
         end loop;
      end Get_Objects;

      --  Start of processing for Build_Library

   begin
      if For_Project.Config.Lib_Support = None then
         Fail_Program (Project_Tree,
                       "library projects not supported on this platform");

      elsif For_Project.Library_Kind /= Static and then
            For_Project.Config.Lib_Support /= Full
      then
         Fail_Program
           (Project_Tree,
            "shared library projects not supported on this platform");
      end if;

      if For_Project.Config.Library_Builder = No_Path then
         Fail_Program (Project_Tree, "no library builder specified");

      else
         Library_Builder :=
           Locate_Exec_On_Path
             (Get_Name_String (For_Project.Config.Library_Builder));

         if Library_Builder = null then
            Fail_Program
              (Project_Tree,
               "could not locate library builder """ &
               Get_Name_String (For_Project.Config.Library_Builder) & '"');

         else
            Library_Builder_Name :=
              new String'(Base_Name (Library_Builder.all));
         end if;
      end if;

      if For_Project.Library_Kind = Static then
         Check_Archive_Builder;
      end if;

      --  Work occurs in the object directory

      Change_To_Object_Directory (For_Project);

      Library_Needs_To_Be_Built := Opt.Force_Compilations;

      if (not Library_Needs_To_Be_Built) and then Opt.Verbose_Mode then
         Write_Str ("   Checking library ");
         Get_Name_String (For_Project.Library_Name);
         Write_Str (Name_Buffer (1 .. Name_Len));
         Write_Line (" ...");
      end if;

      Get_Objects;

      --  Get the name of of the library exchange file

      Get_Name_String (For_Project.Library_Name);
      Add_Str_To_Name_Buffer (Library_Exchange_Suffix);
      Exchange_File_Name := new String'(Name_Buffer (1 .. Name_Len));

      if not Library_Needs_To_Be_Built then
         declare
            TS : constant Time_Stamp_Type :=
                   File_Stamp (File_Name_Type'(Name_Find));

         begin
            if String (TS) < String (Latest_Object_TS) then
               Library_Needs_To_Be_Built := True;

               if Opt.Verbose_Mode then
                  if TS = Empty_Time_Stamp then
                     Write_Line
                       ("      -> library exchange file " &
                        Exchange_File_Name.all &
                        " does not exist");

                  else
                     Write_Line
                       ("      -> object files more recent than" &
                        " library exchange file " &
                        Exchange_File_Name.all);
                  end if;
               end if;

            else
               begin
                  Open (Exchange_File, In_File, Exchange_File_Name.all);

                  if End_Of_File (Exchange_File) then
                     if Opt.Verbose_Mode then
                        Write_Str ("      -> library exchange file """);
                        Write_Str (Exchange_File_Name.all);
                        Write_Line (""" is empty");
                     end if;

                     Library_Needs_To_Be_Built := True;
                  end if;

               exception
                  when others =>
                     if Opt.Verbose_Mode then
                        Write_Str
                          ("      -> library exchange file """);
                        Write_Str (Exchange_File_Name.all);
                        Write_Line (""" cannot be open");
                     end if;

                     Library_Needs_To_Be_Built := True;
               end;

            end if;
         end;
      end if;

      if not Library_Needs_To_Be_Built then
         --  The exchange file is open in input

         --  Get the path of the library file that should be the first field

         Get_Line (Exchange_File, Name_Buffer, Name_Len);

         if Name_Buffer (1 .. Name_Len) /= Library_Label (Library_Path) then
            Library_Needs_To_Be_Built := True;
            Close (Exchange_File);

            if Opt.Verbose_Mode then
               Write_Line ("      -> library exchange file " &
                           Exchange_File_Name.all &
                           " has wrong format");
            end if;

         else
            Get_Line (Exchange_File, Name_Buffer, Name_Len);

            if String (File_Stamp (File_Name_Type'(Name_Find))) <
               String (Latest_Object_TS)
            then
               Library_Needs_To_Be_Built := True;
               Close (Exchange_File);

               if Opt.Verbose_Mode then
                  Write_Line
                    ("      -> object file(s) more recent than library file " &
                     Exchange_File_Name.all);
               end if;

            end if;
         end if;
      end if;

      if not Library_Needs_To_Be_Built then
         --  The next line should be the object file label, followed by the
         --  object paths and time stamps.

         Get_Line (Exchange_File, Name_Buffer, Name_Len);

         if Name_Buffer (1 .. Name_Len) /= Library_Label (Object_Files) then
            Library_Needs_To_Be_Built := True;

            if Opt.Verbose_Mode then
               Write_Line ("      -> library exchange file " &
                           Exchange_File_Name.all &
                           " has wrong format");
            end if;
         end if;

         while (not Library_Needs_To_Be_Built) and then
           (not End_Of_File (Exchange_File))
         loop
            Get_Line (Exchange_File, Name_Buffer, Name_Len);

            exit when Name_Buffer (1) = '[';

            Object_Path := Name_Find;

            Library_Needs_To_Be_Built := True;

            if End_Of_File (Exchange_File) then
               if Opt.Verbose_Mode then
                  Write_Line
                    ("      -> library exchange file " &
                     Exchange_File_Name.all &
                     " has wrong format");
               end if;

            else
               Get_Line (Exchange_File, Name_Buffer, Name_Len);

               if Name_Len = Time_Stamp_Length then
                  Object_TS :=
                    Time_Stamp_Type (Name_Buffer (1 .. Name_Len));

                  Path_Found := False;
                  for Index in 1 .. Library_Objs.Last loop
                     if Object_Path = Library_Objs.Table (Index).Path then
                        Path_Found := True;
                        Library_Needs_To_Be_Built :=
                          Object_TS /= Library_Objs.Table (Index).TS;
                        Library_Objs.Table (Index).Known := True;
                        exit;
                     end if;
                  end loop;

                  --  If the object file is not found, it may be that the path
                  --  in the library is the same as the path of the object
                  --  files, but with different symbolic links. So, we try
                  --  again resolving the symbolic links.

                  if not Path_Found then
                     declare
                        Norm_Path : constant String :=
                          Normalize_Pathname (Get_Name_String (Object_Path));

                     begin
                        for Index in 1 .. Library_Objs.Last loop
                           if Norm_Path =
                             Normalize_Pathname
                               (Get_Name_String
                                    (Library_Objs.Table (Index).Path))
                           then
                              Library_Needs_To_Be_Built :=
                                Object_TS /= Library_Objs.Table (Index).TS;
                              Library_Objs.Table (Index).Known := True;
                              exit;
                           end if;
                        end loop;
                     end;
                  end if;

                  if Library_Needs_To_Be_Built and then Opt.Verbose_Mode then
                     Write_Str ("      -> object file ");
                     Write_Str (Get_Name_String (Object_Path));
                     Write_Line
                       (" does not exist or have wrong time stamp");
                  end if;

               else
                  if Opt.Verbose_Mode then
                     Write_Line
                       ("      -> library exchange file " &
                        Exchange_File_Name.all &
                        " has wrong format");
                  end if;
               end if;
            end if;
         end loop;

         Close (Exchange_File);

         if not Library_Needs_To_Be_Built then
            for Index in 1 .. Library_Objs.Last loop
               if not Library_Objs.Table (Index).Known then
                  Library_Needs_To_Be_Built := True;

                  if Opt.Verbose_Mode then
                     Write_Str
                       ("      -> library was built without object file ");
                     Write_Line
                       (Get_Name_String (Library_Objs.Table (Index).Path));
                  end if;

                  exit;
               end if;
            end loop;
         end if;
      end if;

      if not Library_Needs_To_Be_Built then
         if Opt.Verbose_Mode then
            Write_Line ("      -> up to date");
         end if;

      else
         --  Create the library exchange file
         begin
            Create (Exchange_File, Out_File, Exchange_File_Name.all);

         exception
            when others =>
               Fail_Program
                 (Project_Tree,
                  "unable to create library exchange file " &
                  Exchange_File_Name.all);
         end;

         if Opt.Quiet_Output then
            Put_Line (Exchange_File, Library_Label (Quiet));

         elsif Opt.Verbose_Mode then
            Put_Line (Exchange_File, Library_Label (Verbose));
         end if;

         if Library_Objs.Last > 0 then
            Put_Line (Exchange_File, Library_Label (Object_Files));

            for J in 1 .. Library_Objs.Last loop
               Put_Line
                 (Exchange_File,
                  Get_Name_String (Library_Objs.Table (J).Path));
            end loop;
         end if;

         Put_Line (Exchange_File, Library_Label (Library_Name));
         Put_Line (Exchange_File, Get_Name_String (For_Project.Library_Name));

         if For_Project.Lib_Internal_Name /= No_Name then
            Put_Line (Exchange_File, Library_Label (Library_Version));
            Put_Line (Exchange_File,
                      Get_Name_String (For_Project.Lib_Internal_Name));
         end if;

         Put_Line (Exchange_File, Library_Label (Library_Directory));
         Put_Line
           (Exchange_File,
            Get_Name_String (For_Project.Library_Dir.Display_Name));

         if For_Project.Library_ALI_Dir /= No_Path_Information and then
           For_Project.Library_ALI_Dir.Name /= For_Project.Library_Dir.Name
         then
            Put_Line
              (Exchange_File, Library_Label (Library_Dependency_Directory));
            Put_Line
              (Exchange_File,
               Get_Name_String (For_Project.Library_ALI_Dir.Display_Name));
         end if;

         Put_Line (Exchange_File, Library_Label (Object_Directory));
         Put_Line (Exchange_File, Object_Directory_Path);

         --  Add object directory of project being extended, if any

         declare
            Proj : Project_Id := For_Project.Extends;

         begin
            while Proj /= No_Project loop
               if Proj.Object_Directory /= No_Path_Information then
                  Put_Line
                    (Exchange_File,
                     Get_Name_String (Proj.Object_Directory.Display_Name));
               end if;
               Proj := Proj.Extends;
            end loop;
         end;

         --  Add object directories of imported non library projects

         Process_Imported_Non_Libraries (For_Project);

         declare
            Proj : Project_Id;
         begin
            for J in 1 .. Non_Library_Projs.Last loop
               Proj := Non_Library_Projs.Table (J);

               if Proj.Object_Directory /= No_Path_Information then
                  Put_Line
                    (Exchange_File,
                     Get_Name_String (Proj.Object_Directory.Display_Name));
               end if;
            end loop;
         end;

         --  Add ALI dir directories of imported projects

         declare
            List : Project_List := For_Project.All_Imported_Projects;

         begin
            while List /= null loop
               if List.Project.Library_ALI_Dir /= No_Path_Information then
                  Put_Line
                    (Exchange_File,
                     Get_Name_String
                       (List.Project.Library_ALI_Dir.Display_Name));

               elsif List.Project.Library_Dir /= No_Path_Information then
                  Put_Line
                    (Exchange_File,
                     Get_Name_String (List.Project.Library_Dir.Display_Name));
               end if;

               List := List.Next;
            end loop;
         end;

         Put_Line (Exchange_File, Library_Label (Compilers));

         declare
            Lang : Language_Ptr := For_Project.Languages;
            Compiler : String_Access;

         begin
            while Lang /= No_Language_Index loop
               Compiler := Get_Compiler_Driver_Path
                 (Project_Tree, Lang);
               if Compiler /= null then
                  Put_Line (Exchange_File, Get_Name_String (Lang.Name));
                  Put_Line (Exchange_File, Compiler.all);

               elsif Lang.Config.Compiler_Driver /= No_File then
                  Put_Line (Exchange_File, Get_Name_String (Lang.Name));
                  Put_Line
                    (Exchange_File,
                     Get_Name_String (Lang.Config.Compiler_Driver));
               end if;

               Lang := Lang.Next;
            end loop;
         end;

         Put_Line (Exchange_File, Library_Label (Compiler_Leading_Switches));

         declare
            Lang : Language_Ptr := For_Project.Languages;
            Indx : Name_List_Index;
            Node : Name_Node;

         begin
            while Lang /= No_Language_Index loop
               Indx := Lang.Config.Compiler_Leading_Required_Switches;

               if Indx /= No_Name_List then
                  Put_Line
                    (Exchange_File,
                     "language=" & Get_Name_String (Lang.Name));

                  while Indx /= No_Name_List loop
                     Node := Project_Tree.Shared.Name_Lists.Table (Indx);
                     Put_Line (Exchange_File, Get_Name_String (Node.Name));
                     Indx := Node.Next;
                  end loop;
               end if;

               Lang := Lang.Next;
            end loop;
         end;

         Put_Line (Exchange_File, Library_Label (Compiler_Trailing_Switches));

         declare
            Lang : Language_Ptr := For_Project.Languages;
            Indx : Name_List_Index;
            Node : Name_Node;

         begin
            while Lang /= No_Language_Index loop
               Indx := Lang.Config.Compiler_Trailing_Required_Switches;

               if Indx /= No_Name_List then
                  Put_Line
                    (Exchange_File,
                     "language=" & Get_Name_String (Lang.Name));

                  while Indx /= No_Name_List loop
                     Node := Project_Tree.Shared.Name_Lists.Table (Indx);
                     Put_Line (Exchange_File, Get_Name_String (Node.Name));
                     Indx := Node.Next;
                  end loop;
               end if;

               Lang := Lang.Next;
            end loop;
         end;

         if For_Project.Config.Lib_Partial_Linker /= No_Name_List then
            Put_Line (Exchange_File, Library_Label (Partial_Linker));

            declare
               List    : Name_List_Index :=
                           For_Project.Config.Lib_Partial_Linker;
               Nam_Nod : Name_Node;

            begin
               while List /= No_Name_List loop
                  Nam_Nod := Project_Tree.Shared.Name_Lists.Table (List);
                  Put_Line
                    (Exchange_File,
                     Get_Name_String (Nam_Nod.Name));
                  List := Nam_Nod.Next;
               end loop;
            end;
         end if;

         if For_Project.Library_Kind = Static then
            Put_Line (Exchange_File, Library_Label (Static));

            Put_Line (Exchange_File, Library_Label (Archive_Builder));
            Put_Line (Exchange_File, Archive_Builder_Path.all);

            for J in 1 .. Archive_Builder_Opts.Last loop
               Put_Line (Exchange_File, Archive_Builder_Opts.Options (J).all);
            end loop;

            if Archive_Builder_Append_Opts.Last > 0 then
               Put_Line
                 (Exchange_File,
                  Library_Label (Archive_Builder_Append_Option));

               for J in 1 .. Archive_Builder_Append_Opts.Last loop
                  Put_Line
                    (Exchange_File,
                     Archive_Builder_Append_Opts.Options (J).all);
               end loop;
            end if;

            if For_Project.Config.Archive_Suffix /= No_File then
               Put_Line (Exchange_File, Library_Label (Archive_Suffix));
               Put_Line
                 (Exchange_File,
                  Get_Name_String (For_Project.Config.Archive_Suffix));
            end if;

            if Archive_Indexer_Path /= null then
               Put_Line (Exchange_File, Library_Label (Archive_Indexer));
               Put_Line (Exchange_File, Archive_Indexer_Path.all);

               for J in 1 .. Archive_Indexer_Opts.Last loop
                  Put_Line
                    (Exchange_File, Archive_Indexer_Opts.Options (J).all);
               end loop;
            end if;

         else
            if For_Project.Config.Shared_Lib_Driver /= No_File then
               Put_Line (Exchange_File, Library_Label (Driver_Name));
               Put_Line
                 (Exchange_File,
                  Get_Name_String (For_Project.Config.Shared_Lib_Driver));
            end if;

            if For_Project.Config.Shared_Lib_Prefix /= No_File then
               Put_Line (Exchange_File, Library_Label (Shared_Lib_Prefix));
               Put_Line
                 (Exchange_File,
                  Get_Name_String (For_Project.Config.Shared_Lib_Prefix));
            end if;

            if For_Project.Config.Shared_Lib_Suffix /= No_File then
               Put_Line (Exchange_File, Library_Label (Shared_Lib_Suffix));
               Put_Line
                 (Exchange_File,
                  Get_Name_String (For_Project.Config.Shared_Lib_Suffix));
            end if;

            if For_Project.Config.Shared_Lib_Min_Options /= No_Name_List then
               Put_Line
                 (Exchange_File, Library_Label (Shared_Lib_Minimum_Options));
               declare
                  List : Name_List_Index :=
                           For_Project.Config.Shared_Lib_Min_Options;
                  Nam_Nod : Name_Node;

               begin
                  while List /= No_Name_List loop
                     Nam_Nod := Project_Tree.Shared.Name_Lists.Table (List);
                     Put_Line
                       (Exchange_File,
                        Get_Name_String (Nam_Nod.Name));
                     List := Nam_Nod.Next;
                  end loop;
               end;
            end if;

            if For_Project.Config.Lib_Version_Options /= No_Name_List then
               Put_Line
                 (Exchange_File, Library_Label (Library_Version_Options));
               declare
                  List : Name_List_Index :=
                           For_Project.Config.Lib_Version_Options;
                  Nam_Nod : Name_Node;

               begin
                  while List /= No_Name_List loop
                     Nam_Nod := Project_Tree.Shared.Name_Lists.Table (List);
                     Put_Line
                       (Exchange_File,
                        Get_Name_String (Nam_Nod.Name));
                     List := Nam_Nod.Next;
                  end loop;
               end;
            end if;

            if For_Project.Config.Symbolic_Link_Supported then
               Put_Line
                 (Exchange_File, Library_Label (Symbolic_Link_Supported));
            end if;

            if For_Project.Config.Lib_Maj_Min_Id_Supported then
               Put_Line
                 (Exchange_File, Library_Label (Major_Minor_Id_Supported));
            end if;

            Process_Imported_Libraries
              (For_Project, There_Are_SALs => Disregard);

            --  Check for runtime library dir

            declare
               List : Language_Ptr := For_Project.Languages;
               First : Boolean := True;

            begin
               while List /= No_Language_Index loop
                  if List.Config.Runtime_Library_Dir /= No_Name then
                     if First then
                        Put_Line
                          (Exchange_File, Library_Label (Runtime_Library_Dir));
                        First := False;
                     end if;

                     Put_Line (Exchange_File, Get_Name_String (List.Name));
                     Put_Line
                       (Exchange_File,
                        Get_Name_String (List.Config.Runtime_Library_Dir));
                  end if;

                  List := List.Next;
               end loop;
            end;

            if For_Project.Library_Kind /= Static then
               Put_Line (Exchange_File, Library_Label (Relocatable));
            end if;

            if For_Project.Standalone_Library then
               if For_Project.Lib_Auto_Init then
                  Put_Line (Exchange_File, Library_Label (Auto_Init));
               end if;

               declare
                  Binder_Package : constant Package_Id :=
                                     Value_Of
                                       (Name        => Name_Binder,
                                        In_Packages =>
                                          For_Project.Decl.Packages,
                                        Shared      => Project_Tree.Shared);

               begin
                  if Binder_Package /= No_Package then
                     declare
                        Defaults : constant Array_Element_Id :=
                                     Value_Of
                                       (Name      => Name_Default_Switches,
                                        In_Arrays =>
                                          Project_Tree.Shared.Packages.Table
                                            (Binder_Package).Decl.Arrays,
                                        Shared    => Project_Tree.Shared);
                        Switches : Variable_Value := Nil_Variable_Value;

                        Switch   : String_List_Id := Nil_String;

                     begin
                        if Defaults /= No_Array_Element then
                           Switches :=
                             Value_Of
                               (Index     => Name_Ada,
                                Src_Index => 0,
                                In_Array  => Defaults,
                                Shared    => Project_Tree.Shared);

                           if not Switches.Default then
                              Put_Line
                                (Exchange_File,
                                 Library_Label (Gprexch.Binding_Options));
                              Switch := Switches.Values;

                              while Switch /= Nil_String loop
                                 Put_Line
                                   (Exchange_File,
                                    Get_Name_String
                                      (Project_Tree.Shared.String_Elements
                                        .Table (Switch).Value));
                                 Switch := Project_Tree.Shared.String_Elements.
                                   Table (Switch).Next;
                              end loop;
                           end if;
                        end if;
                     end;
                  end if;
               end;

            end if;

            if
              For_Project.Config.Library_Install_Name_Option /= No_Name
            then
               Put_Line (Exchange_File, Library_Label (Gprexch.Install_Name));
               Put_Line
                 (Exchange_File,
                  Get_Name_String
                    (For_Project.Config.Library_Install_Name_Option));
            end if;

            if Opt.Run_Path_Option and then
              For_Project.Config.Run_Path_Option /= No_Name_List
            then
               Put_Line
                 (Exchange_File, Library_Label (Gprexch.Run_Path_Option));

               declare
                  List : Name_List_Index :=
                           For_Project.Config.Run_Path_Option;
                  Nam  : Name_Node;

               begin
                  while List /= No_Name_List loop
                     Nam := Project_Tree.Shared.Name_Lists.Table (List);
                     Put_Line (Exchange_File, Get_Name_String (Nam.Name));
                     List := Nam.Next;
                  end loop;
               end;

               if For_Project.Config.Separate_Run_Path_Options then
                  Put_Line
                    (Exchange_File,
                     Library_Label (Gprexch.Separate_Run_Path_Options));
               end if;
            end if;

            --  If attribute Leading_Library_Options was specified, add these
            --  additional options.

            Leading_Library_Options :=
              Value_Of
                (Name_Leading_Library_Options,
                 For_Project.Decl.Attributes, Project_Tree.Shared);

            if not Leading_Library_Options.Default then
               declare
                  Current      : String_List_Id :=
                                   Leading_Library_Options.Values;
                  Element      : String_Element;
                  Output_Label : Boolean := True;

               begin
                  while Current /= Nil_String loop
                     Element :=
                       Project_Tree.Shared.String_Elements.Table (Current);
                     Get_Name_String (Element.Value);

                     if Name_Len /= 0 then
                        if Output_Label then
                           Put_Line
                             (Exchange_File,
                              Library_Label (Gprexch.Leading_Library_Options));
                           Output_Label := False;
                        end if;

                        Put_Line (Exchange_File, Name_Buffer (1 .. Name_Len));
                     end if;

                     Current := Element.Next;
                  end loop;
               end;
            end if;

            --  If attribute Library_Options was specified, add these
            --  additional options.

            Library_Options :=
              Value_Of
                (Name_Library_Options,
                 For_Project.Decl.Attributes, Project_Tree.Shared);

            if not Library_Options.Default then
               declare
                  Current      : String_List_Id := Library_Options.Values;
                  Element      : String_Element;
                  Output_Label : Boolean := True;

               begin
                  while Current /= Nil_String loop
                     Element :=
                       Project_Tree.Shared.String_Elements.Table (Current);
                     Get_Name_String (Element.Value);

                     if Name_Len /= 0 then
                        if Output_Label then
                           Put_Line
                             (Exchange_File,
                              Library_Label (Gprexch.Library_Options));
                           Output_Label := False;
                        end if;

                        Put_Line (Exchange_File, Name_Buffer (1 .. Name_Len));
                     end if;

                     Current := Element.Next;
                  end loop;
               end;
            end if;

            --  If there are imported libraries, put their data in the exchange
            --  file.

            if Library_Projs.Last > 0 then
               Put_Line (Exchange_File, Library_Label (Imported_Libraries));

               for J in reverse 1 .. Library_Projs.Last loop
                  Put_Line
                    (Exchange_File,
                     Get_Name_String
                       (Library_Projs.Table (J).Library_Dir.Display_Name));
                  Put_Line
                    (Exchange_File,
                     Get_Name_String
                       (Library_Projs.Table (J).Library_Name));
               end loop;
            end if;
         end if;

         Put_Line (Exchange_File, Library_Label (Dependency_Files));

         declare
            Current_Proj : Project_Id := For_Project;
            Source       : Source_Id;
         begin
            while Current_Proj /= No_Project loop
               declare
                  Iter      : Source_Iterator;
               begin
                  Iter := For_Each_Source (Project_Tree, Current_Proj);
                  loop
                     Source := Prj.Element (Iter);
                     exit when Source = No_Source;

                     if not Source.Locally_Removed
                       and then Source.Dep_Path /= No_Path
                     then
                        if Source.Kind = Spec then
                           if Other_Part (Source) = No_Source then
                              Put_Line
                                (Exchange_File,
                                 Get_Name_String (Source.Dep_Path));
                           end if;

                        elsif not Is_Subunit (Source) then
                           Put_Line
                             (Exchange_File,
                              Get_Name_String (Source.Dep_Path));
                        end if;
                     end if;

                     Next (Iter);
                  end loop;

                  Current_Proj := Current_Proj.Extends;
               end;
            end loop;
         end;

         Lang_Index := For_Project.Languages;
         Toolchain_Version_Label_Written := False;

         while Lang_Index /= No_Language_Index loop
            if Lang_Index.Config.Toolchain_Version /= No_Name then
               if not Toolchain_Version_Label_Written then
                  Put_Line (Exchange_File, Library_Label (Toolchain_Version));
                  Toolchain_Version_Label_Written := True;
               end if;

               Put_Line (Exchange_File, Get_Name_String (Lang_Index.Name));
               Put_Line
                 (Exchange_File,
                  Get_Name_String (Lang_Index.Config.Toolchain_Version));
            end if;

            Lang_Index := Lang_Index.Next;
         end loop;

         if For_Project.Standalone_Library then
            if For_Project.Lib_Auto_Init then
               Put_Line (Exchange_File, Library_Label (Auto_Init));
            end if;

            declare
               Interface_ALIs : String_List_Id :=
                 For_Project.Lib_Interface_ALIs;
               Element        : String_Element;

            begin
               Put_Line (Exchange_File, Library_Label (Interface_Dep_Files));

               while Interface_ALIs /= Nil_String loop
                  Element :=
                    Project_Tree.Shared.String_Elements.Table (Interface_ALIs);

                  --  Find the source to get the absolute path of the ALI file

                  declare
                     Next_Proj : Project_Id;
                     Iter      : Source_Iterator;
                  begin
                     Next_Proj := For_Project.Extends;
                     Iter := For_Each_Source (Project_Tree, For_Project);
                     loop
                        while Prj.Element (Iter) /= No_Source
                          and then
                            (Prj.Element (Iter).Unit = null
                             or else Prj.Element (Iter).Dep_Name /=
                                      File_Name_Type (Element.Value))
                        loop
                           Next (Iter);
                        end loop;

                        Source := Prj.Element (Iter);
                        exit when Source /= No_Source
                          or else Next_Proj = No_Project;

                        Iter := For_Each_Source (Project_Tree, Next_Proj);
                        Next_Proj := Next_Proj.Extends;
                     end loop;

                     if Source /= No_Source then
                        if Source.Kind = Sep then
                           Source := No_Source;

                        elsif Source.Kind = Spec
                          and then Other_Part (Source) /= No_Source
                        then
                           Source := Other_Part (Source);
                        end if;
                     end if;

                     if Source /= No_Source then
                        if Source.Project /= Project
                          and then
                            not Is_Extending (For_Project, Source.Project)
                        then
                           Source := No_Source;
                        end if;
                     end if;

                     if Source /= No_Source then
                        Put_Line
                          (Exchange_File,
                           Get_Name_String (Source.Dep_Path));
                     end if;
                  end;

                  Interface_ALIs := Element.Next;
               end loop;
            end;

            if For_Project.Library_Src_Dir /= No_Path_Information then
               Put_Line (Exchange_File, Library_Label (Copy_Source_Dir));
               Put_Line
                 (Exchange_File,
                  Get_Name_String (For_Project.Library_Src_Dir.Display_Name));

               Put_Line (Exchange_File, Library_Label (Sources));

               --  Copy the path of the sources

               Project := For_Project;

               while Project /= No_Project loop
                  Iter := For_Each_Source (Project_Tree, Project);

                  loop
                     Source := Prj.Element (Iter);
                     exit when Source = No_Source;

                     if Source.Language.Config.Kind = Unit_Based
                       and then not Source.Locally_Removed
                       and then Source.Replaced_By = No_Source
                     then
                        Put_Line
                          (Exchange_File,
                           Get_Name_String (Source.Path.Display_Name));
                     end if;

                     Next (Iter);
                  end loop;

                  Project := Project.Extends;
               end loop;

            end if;

         end if;

         --  Response files

         if For_Project.Config.Max_Command_Line_Length > 0
           and then For_Project.Config.Resp_File_Format /= None then
            Put_Line (Exchange_File, Library_Label (Max_Command_Line_Length));
            Put_Line
              (Exchange_File, For_Project.Config.Max_Command_Line_Length'Img);

            Put_Line
              (Exchange_File, Library_Label (Gprexch.Response_File_Format));
            Put_Line
              (Exchange_File, For_Project.Config.Resp_File_Format'Img);

            if For_Project.Config.Resp_File_Options /= No_Name_List then
               Put_Line
                 (Exchange_File, Library_Label (Response_File_Switches));

               declare
                  List : Name_List_Index :=
                    For_Project.Config.Resp_File_Options;

               begin
                  while List /= No_Name_List loop
                     Put_Line
                       (Exchange_File,
                        Get_Name_String
                          (Project_Tree.Shared.Name_Lists.Table (List).Name));
                     List := Project_Tree.Shared.Name_Lists.Table (List).Next;
                  end loop;
               end;
            end if;

            if Debug.Debug_Flag_N then
               Put_Line (Exchange_File, Library_Label (Keep_Response_File));
            end if;
         end if;

         Close (Exchange_File);

         declare
            Arguments : constant Argument_List := (1 => Exchange_File_Name);
            Success   : Boolean;

         begin
            if not Opt.Quiet_Output then
               if Opt.Verbose_Mode then
                  Write_Str (Library_Builder.all);

               else
                  Write_Str (Library_Builder_Name.all);
               end if;

               Write_Char (' ');
               Write_Line (Exchange_File_Name.all);
            end if;

            Spawn (Library_Builder.all, Arguments, Success);

            if not Success then
               Fail_Program
                 (Project_Tree,
                  "could not build library for project " & Project_Name);
            end if;
         end;
      end if;

      --  Restore the current working directory to its previous value

      Change_Dir (Current_Dir);
   end Build_Library;

   --------------------------------
   -- Change_To_Object_Directory --
   --------------------------------

   procedure Change_To_Object_Directory (Project : Project_Id) is
   begin
      --  Nothing to do if the current working directory is already the correct
      --  object directory.

      if Project_Of_Current_Object_Directory /= Project then
         Project_Of_Current_Object_Directory := Project;

         --  Set the working directory to the object directory of the actual
         --  project.

         Change_Dir (Get_Name_String (Project.Object_Directory.Display_Name));

         if Opt.Verbose_Mode and then Opt.Verbosity_Level > Opt.Low then
            Write_Str  ("Changing to object directory of """);
            Write_Name (Project.Display_Name);
            Write_Str  (""": """);
            Write_Name (Project.Object_Directory.Display_Name);
            Write_Line ("""");
         end if;
      end if;

   exception
      --  Fail if unable to change to the object directory

      when Directory_Error =>
         Fail_Program
           (Project_Tree,
            "unable to change to object directory """ &
            Get_Name_String (Project.Object_Directory.Display_Name) &
            """ of project " &
            Get_Name_String (Project.Display_Name));
   end Change_To_Object_Directory;

   ---------------------------
   -- Check_Archive_Builder --
   ---------------------------

   procedure Check_Archive_Builder is
      List : Name_List_Index;
   begin
      --  First, make sure that the archive builder (ar) is on the path

      if Archive_Builder_Path = null then
         List := Main_Project.Config.Archive_Builder;

         if List = No_Name_List then
            Fail_Program
              (Project_Tree, "no archive builder in configuration");

         else
            Archive_Builder_Name :=
              new String'(Get_Name_String
                                     (Project_Tree.Shared.Name_Lists.Table
                                        (List).Name));
            Archive_Builder_Path :=
              Locate_Exec_On_Path (Archive_Builder_Name.all);

            if Archive_Builder_Path = null then
               Fail_Program
                 (Project_Tree,
                  "unable to locate archive builder """ &
                  Archive_Builder_Name.all & '"');
            end if;

            loop
               List := Project_Tree.Shared.Name_Lists.Table (List).Next;
               exit when List = No_Name_List;
               Add_Option
                 (Value   => Project_Tree.Shared.Name_Lists.Table (List).Name,
                  To      => Archive_Builder_Opts,
                  Display => True);
            end loop;

            List := Main_Project.Config.Archive_Builder_Append_Option;
            while List /= No_Name_List loop
               Add_Option
                 (Value   => Project_Tree.Shared.Name_Lists.Table (List).Name,
                  To      => Archive_Builder_Append_Opts,
                  Display => True);
               List := Project_Tree.Shared.Name_Lists.Table (List).Next;
            end loop;

            --  If there is an archive indexer (ranlib), try to locate it on
            --  the path. Don't fail if it is not found.

            List := Main_Project.Config.Archive_Indexer;

            if List /= No_Name_List then
               Archive_Indexer_Name :=
                 new String'(Get_Name_String
                   (Project_Tree.Shared.Name_Lists.Table (List).Name));
               Archive_Indexer_Path :=
                 Locate_Exec_On_Path (Archive_Indexer_Name.all);

               if Archive_Builder_Path /= null then
                  loop
                     List := Project_Tree.Shared.Name_Lists.Table (List).Next;
                     exit when List = No_Name_List;
                     Add_Option
                       (Value   =>
                          Project_Tree.Shared.Name_Lists.Table (List).Name,
                        To      => Archive_Indexer_Opts,
                        Display => True);
                  end loop;
               end if;
            end if;
         end if;
      end if;
   end Check_Archive_Builder;

   ------------------------
   -- Add_Mains_To_Queue --
   ------------------------

   procedure Add_Mains_To_Queue is
      Main_Id : Main_Info;

   begin
      Mains.Reset;

      loop
         Main_Id := Mains.Next_Main;
         exit when Main_Id = No_Main_Info;

         if Main_Id.Source /= No_Source then
            --  Fail if any main is declared as an excluded source file

            if Main_Id.Source.Locally_Removed then
               Fail_Program
                 (Project_Tree,
                  "main """ &
                  Get_Name_String (Main_Id.Source.File) &
                  """ cannot also be an excluded file");
            end if;

            Queue.Insert
              (Source     => (Format => Format_Gprbuild,
                              Tree   => Main_Id.Tree,
                              Id     => Main_Id.Source),
               With_Roots => Builder_Data (Main_Id.Tree).Closure_Needed);

            --  If a non Ada main has no roots, then all sources need to be
            --  compiled, so no need to check for closure.

            if Main_Id.Source.Language.Config.Kind /= Unit_Based
              and then Main_Id.Source.Roots = null
            then
               Builder_Data (Main_Id.Tree).Closure_Needed := False;
            end if;
         end if;
      end loop;

      if Err_Vars.Total_Errors_Detected /= 0 then
         Fail_Program (Project_Tree, "cannot continue");
      end if;

      Queue.Insert_Project_Sources
        (Project        => Main_Project,
         Project_Tree   => Project_Tree,
         Unique_Compile => Unique_Compile,
         All_Projects =>
            not Unique_Compile
            or else (Unique_Compile_All_Projects or Recursive));
   end Add_Mains_To_Queue;

   -----------------------
   -- Compilation_Phase --
   -----------------------

   procedure Compilation_Phase is

      procedure Do_Compile (Project : Project_Id; Tree : Project_Tree_Ref);

      ----------------
      -- Do_Compile --
      ----------------

      procedure Do_Compile (Project : Project_Id; Tree : Project_Tree_Ref) is
      begin
         if Builder_Data (Tree).Need_Compilation then
            Compilation_Phase (Project, Tree);

            if Total_Errors_Detected > 0
              or else Bad_Compilations.Last > 0
            then
               --  If switch -k or -jnn (with nn > 1), output a summary of the
               --  sources that could not be compiled.

               if (Opt.Keep_Going or else Opt.Maximum_Processes > 1)
                 and then Bad_Compilations.Last > 0
               then
                  declare
                     Source : Source_Id;
                  begin
                     Write_Eol;

                     for Index in 1 .. Bad_Compilations.Last loop
                        Source := Bad_Compilations.Table (Index);

                        if Source /= No_Source then
                           Write_Str ("   compilation of ");
                           Write_Str (Get_Name_String (Source.Display_File));
                           Write_Line (" failed");
                        end if;
                     end loop;

                     Write_Eol;
                  end;
               end if;

               if Opt.Keep_Going and then Project.Qualifier = Aggregate then
                  Bad_Compilations.Init;
               else
                  Fail_Program (Tree, "*** compilation phase failed");
               end if;
            end if;
         end if;
      end Do_Compile;

      procedure Compile_All is new For_Project_And_Aggregated (Do_Compile);
   begin
      Compile_All (Main_Project, Project_Tree);
   end Compilation_Phase;

   -----------------------
   -- Compilation_Phase --
   -----------------------

   procedure Compilation_Phase
     (Main_Project : Project_Id;
      Project_Tree : Project_Tree_Ref)
   is
      type Local_Project_Data is record
         Include_Language       : Language_Ptr := No_Language_Index;
         --  Prepared arguments for "include" parameters (-I or include file).
         --  These are specific to each language and project.

         Include_Path_File      : Path_Name_Type;
         --  The path name of the of the source search directory file

         Imported_Dirs_Switches : Argument_List_Access;
         --  List of the source search switches (-I<source dir>) to be used
         --  when compiling.

         Include_Path           : String_Access := null;
         --  The search source path for the project. Used as the value for an
         --  environment variable, specified by attribute Include_Path
         --  (<langu>). The names of the environment variables are in component
         --  Include_Path of the records Language_Config.
      end record;
      --  project-specific data required for this procedure. These are not
      --  stored in the Project_Data record so that projects kept in memory do
      --  not have to allocate space for these temporary data

      No_Local_Project_Data : constant Local_Project_Data :=
        (Include_Language       => No_Language_Index,
         Include_Path           => null,
         Imported_Dirs_Switches => null,
         Include_Path_File      => No_Path);
      package Local_Projects_HT is new Simple_HTable
        (Header_Num => Prj.Header_Num,
         Element    => Local_Project_Data,
         No_Element => No_Local_Project_Data,
         Key        => Project_Id,
         Hash       => Prj.Hash,
         Equal      => "=");

      Local_Projects : Local_Projects_HT.Instance;

      Current_Project      : Project_Id := No_Project;
      Current_Language_Ind : Language_Ptr := No_Language_Index;
      --  The project for which the include path environment has been set last,
      --  to avoid computing it several times.

      Dep_File         : Text_File;

      Start            : Natural;
      Finish           : Natural;
      Last_Obj         : Natural;

      procedure Add_Config_File_Switch
        (Config    : Language_Config;
         Path_Name : Path_Name_Type);

      procedure Record_ALI_For
        (Source_Identity : Queue.Source_Info;
         The_ALI         : ALI.ALI_Id := ALI.No_ALI_Id);
      --  Record the Id of an ALI file in Good_ALI table.
      --  The_ALI can contain the pre-parsed ali file, to save time.
      --  Tree is the tree to which Source_Identity belongs

      function Phase_2_Makefile (Src_Data : Queue.Source_Info) return Boolean;
      function Phase_2_ALI (Src_Data : Queue.Source_Info) return Boolean;
      --  Process Wait_For_Available_Slot depending on Src_Data.Dependency type
      --  This returns whether the compilation is considered as successful or
      --  not.

      procedure Set_Options_For_File (Id : Source_Id);
      --  Prepare the compiler options to use when building Id

      procedure Process_Project_Phase_1 (Source : Queue.Source_Info);
      --  If some compilation is needed for this project, perform it.

      function Must_Exit_Because_Of_Error return Boolean;
      --  Return True if there were errors and the user decided to exit in such
      --  a case. This waits for any outstanding compilation.

      function Check_Switches_File (Id : Source_Id) return Boolean;
      --  Check in its switches file where Id was compiled with the same
      --  switches

      procedure Update_Object_Path
        (Id : Source_Id; Source_Project : Project_Id);
      --  Update, if necessary, the path of the object file, of the dependency
      --  file and of the switches file, in the case of the compilation of a
      --  source in an extended project, when the source is in a project being
      --  extended.

      procedure Add_Dependency_Options (Id : Source_Id);
      --  Add switches to the compilation command line to create the
      --  dependency file

      procedure Add_Object_File_Switches (Id : Source_Id);
      --  If there are switches to specify the name of the object file, add
      --  them.

      procedure Add_Config_File_Switches
        (Id             : Source_Id;
         Source_Project : Project_Id);
      --  If Config_File_Switches is specified, check if a config file need to
      --  be specified. Return the path to the config file

      procedure Add_Trailing_Switches (Id : Source_Id);
      --  Add the trailing required switches, if any, so that they will be put
      --  in the switches file.

      procedure Add_Name_Of_Source_Switches (Id : Source_Id);
      --  Add the name of the source to be compiled

      function Add_Mapping_File_Switches
        (Source         : Queue.Source_Info;
         Source_Project : Project_Id) return Path_Name_Type;
      --  If the compiler supports mapping files, add the necessary switch.
      --  Returns the name of the mapping file to use (or No_File)

      procedure Add_Multi_Unit_Switches (Id : Source_Id);
      --  Add, if needed, the required switches to compile a multi-unit source
      --  file

      procedure Spawn_Compiler_And_Register
        (Source                 : Queue.Source_Info;
         Source_Project         : Project_Id;
         Compiler_Path          : String;
         Mapping_File_Path      : Path_Name_Type;
         Last_Switches_For_File : Integer);
      --  Spawn the compiler with the arguments currently set in
      --  Compiler_Options. It registers the process we just spawned, so that
      --  we start monitoring it.
      --  This also displays on the output the command we are spawning.
      --  Last_Switches_For_File is the index in Compilation_Options of the
      --  last switch that should be written to the switches file. All
      --  following switches are not output in that file.

      function Get_Compatible_Languages (Lang : Language_Ptr) return Name_Ids;
      --  Return the list of languages that Id could potentially include (for
      --  instance "C" if Id is a "C++" file. This also includes Id's own
      --  language.

      procedure Prepare_Imported_Dirs_Switches
        (Data    : out Local_Project_Data;
         Project : Project_Id;
         Lang    : Language_Ptr);
      --  Add the switches for include directories to the command line (these
      --  are the "-I" switches in the case of C for instance).

      procedure Prepare_Include_Path_File
        (Data    : out Local_Project_Data;
         Project : Project_Id;
         Lang    : Language_Ptr);
      --  Create a file to pass the include directories to the compiler

      procedure Start_Compile_If_Possible;
      --  Checks if there is more work that we can do (ie the Queue is non
      --  empty). If there is, do it only if we have not yet used up all the
      --  available processes.

      procedure Wait_For_Available_Slot;
      --  Check if we should wait for a compilation to finish. This is the case
      --  if all the available processes are busy compiling sources or there is
      --  nothing else to do (that is the Q is empty and there are outstanding
      --  compilations).

      procedure Set_Env_For_Include_Dirs (Id : Source_Id);
      --  Set environment variables or switches to pass the include directories
      --  to the compiler

      ----------------------------
      -- Add_Config_File_Switch --
      ----------------------------

      procedure Add_Config_File_Switch
        (Config    : Language_Config;
         Path_Name : Path_Name_Type)
      is
         List : Name_List_Index := Config.Config_File_Switches;
         Nam  : Name_Node;

      begin
         while List /= No_Name_List loop
            Nam := Project_Tree.Shared.Name_Lists.Table (List);
            Get_Name_String (Nam.Name);

            if Nam.Next = No_Name_List then
               Add_Str_To_Name_Buffer (Get_Name_String (Path_Name));
            end if;

            Add_Option
              (Name_Buffer (1 .. Name_Len),
               To      => Compilation_Options,
               Display => Opt.Verbose_Mode);

            List := Nam.Next;
         end loop;
      end Add_Config_File_Switch;

      --------------------
      -- Record_ALI_For --
      --------------------

      procedure Record_ALI_For
        (Source_Identity : Queue.Source_Info;
         The_ALI         : ALI.ALI_Id := ALI.No_ALI_Id)
      is
         Local_ALI : ALI.ALI_Id := The_ALI;
         Text     : Text_Buffer_Ptr;

      begin
         if The_ALI = ALI.No_ALI_Id then
            Text := Read_Library_Info_From_Full
              (File_Name_Type (Source_Identity.Id.Dep_Path),
               Source_Identity.Id.Dep_TS'Access);

            if Text /= null then
               --  Read the ALI file but read only the necessary lines.

               Local_ALI :=
                 ALI.Scan_ALI
                   (File_Name_Type (Source_Identity.Id.Dep_Path),
                    Text,
                    Ignore_ED     => False,
                    Err           => True,
                    Ignore_Errors => True,
                    Read_Lines    => "W");
               Free (Text);
            end if;
         end if;

         if Local_ALI /= ALI.No_ALI_Id then
            Queue.Insert_Withed_Sources_For (Local_ALI, Source_Identity.Tree);

            ALI.Initialize_ALI;
            ALI.Util.Initialize_ALI_Source;
         end if;
      end Record_ALI_For;

      ----------------------
      -- Phase_2_Makefile --
      ----------------------

      function Phase_2_Makefile
        (Src_Data : Queue.Source_Info) return Boolean
      is
         Compilation_OK : Boolean := True;
      begin
         Open (Dep_File, Get_Name_String (Src_Data.Id.Dep_Path));

         if Is_Valid (Dep_File) then

            Big_Loop :
            loop
               declare
                  End_Of_File_Reached : Boolean := False;
                  Object_Found        : Boolean := False;
               begin
                  loop
                     if End_Of_File (Dep_File) then
                        End_Of_File_Reached := True;
                        exit;
                     end if;

                     Get_Line (Dep_File, Name_Buffer, Name_Len);

                     if Name_Len > 0
                       and then Name_Buffer (1) /= '#'
                     then
                        --  Skip a first line that is an empty
                        --  continuation line.

                        for J in 1 .. Name_Len - 1 loop
                           if Name_Buffer (J) /= ' ' then
                              Object_Found := True;
                              exit;
                           end if;
                        end loop;

                        exit when Object_Found
                          or else Name_Buffer (Name_Len) /= '\';
                     end if;
                  end loop;

                  exit Big_Loop when End_Of_File_Reached;
               end;

               Start  := 1;
               Finish := Index (Name_Buffer (1 .. Name_Len), ": ");

               exit Big_Loop when Finish = 0;

               Last_Obj := Finish;
               loop
                  Last_Obj := Last_Obj - 1;
                  exit when Last_Obj = Start
                    or else Name_Buffer (Last_Obj) /= ' ';
               end loop;

               while Start < Last_Obj
                 and then Name_Buffer (Start) = ' '
               loop
                  Start := Start + 1;
               end loop;

               Start := Finish + 2;

               --  Process each line

               Line_Loop : loop
                  declare
                     Line : String  := Name_Buffer (1 .. Name_Len);
                     Last : Natural := Name_Len;

                  begin
                     Name_Loop : loop

                        --  Find the beginning of the next source path
                        --  name.

                        while Start < Last and then
                        Line (Start) = ' '
                        loop
                           Start := Start + 1;
                        end loop;

                        --  Go to next line when there is a
                        --  continuation character \ at the end of the
                        --  line.

                        exit Name_Loop when Start = Last
                          and then Line (Start) = '\';

                        --  We should not be at the end of the line,
                        --  without a continuation character \.

                        exit Name_Loop when Start = Last;

                        --  Look for the end of the source path name

                        Finish := Start;

                        while Finish < Last loop
                           if Line (Finish) = '\' then
                              --  On Windows, a '\' is part of the
                              --  path name, except when it is not the
                              --  first character followed by another
                              --  '\' or by a space. On other
                              --  platforms, when we are getting a '\'
                              --  that is not the last character of
                              --  the line, the next character is part
                              --  of the path name, even if it is a
                              --  space.

                              if On_Windows and then
                                Finish = Start and then
                                Line (Finish + 1) = '\'
                              then
                                 Finish := Finish + 2;

                              elsif On_Windows and then
                                Line (Finish + 1) /= '\' and then
                                Line (Finish + 1) /= ' '
                              then
                                 Finish := Finish + 1;

                              else
                                 Line (Finish .. Last - 1) :=
                                   Line (Finish + 1 .. Last);
                                 Last := Last - 1;
                              end if;

                           else
                              --  A space that is not preceded by '\'
                              --  indicates the end of the path name.

                              exit when Line (Finish + 1) = ' ';
                              Finish := Finish + 1;
                           end if;
                        end loop;

                        --  Check this source

                        declare
                           Src_Name : constant String :=
                             Normalize_Pathname
                               (Name           =>
                                    Line (Start .. Finish),
                                Resolve_Links  => False,
                                Case_Sensitive => False);
                           Source_2   : Source_Id;

                        begin
                           Name_Len := 0;
                           Add_Str_To_Name_Buffer (Src_Name);
                           Source_2 := Source_Paths_Htable.Get
                             (Src_Data.Tree.Source_Paths_HT,
                              Name_Find);

                           if Source_2 /= No_Source then
                              --  It is a source of a project

                              if not Project_Extends
                                   (Src_Data.Id.Project, Source_2.Project)
                                and then
                                  not Project_Extends
                                   (Source_2.Project, Src_Data.Id.Project)
                              then
                                 --  It is not a source of the same project
                                 --  as the source just compiled. Check if
                                 --  it can be imported.

                                 if not Indirect_Imports then
                                    if Directly_Imports
                                      (Src_Data.Id.Project, Source_2.Project)
                                    then
                                       --  It is a source of a directly
                                       --  imported project. Record its
                                       --  project, for later processing.

                                       Imports.Set
                                         (Source_2.Project, True);

                                    else
                                       --  It is a source of a project that
                                       --  is not directly imported. Record
                                       --  the source for later processing.

                                       Included_Sources.Append (Source_2);
                                    end if;
                                 end if;

                                 if not Source_2.In_Interfaces then
                                    --  It is not a source in the interfaces
                                    --  of its project. Report an error and
                                    --  invalidate the compilation.

                                    Write_Char ('"');
                                    Write_Str
                                      (Get_Name_String
                                         (Src_Data.Id.Path.Display_Name));
                                    Write_Str (""" cannot import """);
                                    Write_Str (Src_Name);
                                    Write_Line (""":");

                                    Write_Str
                                      ("  it is not part of the " &
                                       "interfaces of its project """);
                                    Write_Str
                                      (Get_Name_String
                                         (Source_2.Project.Display_Name));
                                    Write_Line ("""");

                                    Compilation_OK := False;
                                 end if;
                              end if;
                           end if;
                        end;

                        exit Line_Loop when Finish = Last;

                        --  Go get the next source on the line

                        Start := Finish + 1;
                     end loop Name_Loop;
                  end;

                  --  If we are here, we had a continuation character
                  --  \ at the end of the line, so we continue with
                  --  the next line.

                  Get_Line (Dep_File, Name_Buffer, Name_Len);
                  Start  := 1;
                  Finish := 1;
               end loop Line_Loop;
            end loop Big_Loop;

            Close (Dep_File);

            if Included_Sources.Last > 0 then
               --  Sources in projectly that are not directly imported
               --  have been found. Check if they may be imported by
               --  other allowed imported sources.

               declare
                  L : Project_List := Src_Data.Id.Project.Imported_Projects;

               begin

                  --  Put in hash table Imports the project trees
                  --  rooted at the projects that are already in
                  --  Imports.

                  while L /= null loop
                     if Imports.Get (L.Project) then
                        Recursive_Import (L.Project);
                     end if;

                     L := L.Next;
                  end loop;

                  --  For all the imported sources from project not
                  --  directly imported, check if their projects are
                  --  in has thable imports.

                  for J in 1 .. Included_Sources.Last loop
                     declare
                        Included : constant Source_Id :=
                          Included_Sources.Table (J);
                     begin
                        if not Imports.Get (Included.Project) then
                           --  This source is either directly imported or
                           --  imported from another source that should not be
                           --  imported. Report an error and invalidate the
                           --  compilation.

                           Write_Char ('"');
                           Write_Str
                             (Get_Name_String (Src_Data.Id.Path.Display_Name));
                           Write_Str (""" cannot import """);
                           Write_Str
                             (Get_Name_String (Included.Path.Display_Name));
                           Write_Line (""":");

                           Write_Str ("  """);
                           Write_Str
                             (Get_Name_String
                                (Src_Data.Id.Project.Display_Name));
                           Write_Str
                             (""" does not directly import project """);
                           Write_Str
                             (Get_Name_String (Included.Project.Display_Name));
                           Write_Line ("""");

                           Compilation_OK := False;
                        end if;
                     end;
                  end loop;
               end;
            end if;
         end if;
         return Compilation_OK;
      end Phase_2_Makefile;

      -----------------
      -- Phase_2_ALI --
      -----------------

      function Phase_2_ALI (Src_Data : Queue.Source_Info) return Boolean is
         Compilation_OK : Boolean := True;
         Text           : Text_Buffer_Ptr :=
                            Read_Library_Info_From_Full
                              (File_Name_Type (Src_Data.Id.Dep_Path),
                               Src_Data.Id.Dep_TS'Access);
         The_ALI        : ALI.ALI_Id := ALI.No_ALI_Id;
         Sfile          : File_Name_Type;
         Afile          : File_Name_Type;
         Source_2       : Source_Id;

         procedure Check_Source (Sfile : File_Name_Type);
         --  Check if source Sfile is in the same project file as the Src_Data
         --  source file. Invaidate the compilation if it is not.

         ------------------
         -- Check_Source --
         ------------------

         procedure Check_Source (Sfile : File_Name_Type) is
            Source_3 : constant Source_Id :=
                         Find_Source
                           (Src_Data.Tree, No_Project, Base_Name => Sfile);

         begin
            if Source_3 = No_Source then
               Write_Str ("source ");
               Write_Str (Get_Name_String (Sfile));
               Write_Line (" is not a source of a project");
               Compilation_OK := False;

            elsif Ultimate_Extending_Project_Of (Source_3.Project) /=
              Ultimate_Extending_Project_Of (Src_Data.Id.Project)
            then
               Write_Str ("sources ");
               Write_Str (Get_Name_String (Source_3.File));
               Write_Str (" and ");
               Write_Str (Get_Name_String (Src_Data.Id.File));
               Write_Str (" belong to different projects: ");
               Write_Str (Get_Name_String (Source_3.Project.Display_Name));
               Write_Str (" and ");
               Write_Line (Get_Name_String (Src_Data.Id.Project.Display_Name));
               Compilation_OK := False;
            end if;
         end Check_Source;

      begin
         if Text /= null then
            --  Read the ALI file but read only the necessary lines

            The_ALI :=
              ALI.Scan_ALI
                (File_Name_Type (Src_Data.Id.Dep_Path),
                 Text,
                 Ignore_ED     => False,
                 Err           => True,
                 Ignore_Errors => True,
                 Read_Lines    => "DW");

            if The_ALI /= ALI.No_ALI_Id then
               for J in ALI.ALIs.Table (The_ALI).First_Unit ..
                 ALI.ALIs.Table (The_ALI).Last_Unit
               loop
                  for K in ALI.Units.Table (J).First_With ..
                    ALI.Units.Table (J).Last_With
                  loop
                     Sfile := ALI.Withs.Table (K).Sfile;

                     --  Skip generics

                     if Sfile /= No_File then

                        --  Look for this source

                        Afile := ALI.Withs.Table (K).Afile;
                        Source_2 := Source_Files_Htable.Get
                          (Src_Data.Tree.Source_Files_HT, Sfile);

                        while Source_2 /= No_Source loop
                           if Is_Compilable (Source_2)
                             and then  Source_2.Dep_Name = Afile
                           then
                              case Source_2.Kind is
                                 when Spec => null;

                                 when Impl =>
                                    if Is_Subunit (Source_2) then
                                       Source_2 := No_Source;
                                    end if;

                                 when Sep =>
                                    Source_2 := No_Source;
                              end case;

                              exit;
                           end if;

                           Source_2 := Source_2.Next_With_File_Name;
                        end loop;

                        --  If it is the source of a project that is not the
                        --  project of the source just compiled, check if it is
                        --  allowed to be imported.

                        if Source_2 /= No_Source then
                           if not Project_Extends
                                (Src_Data.Id.Project, Source_2.Project)
                               and then
                                not Project_Extends
                                 (Source_2.Project, Src_Data.Id.Project)
                           then
                              if not Indirect_Imports and then
                                not Directly_Imports
                                  (Src_Data.Id.Project, Source_2.Project)
                              then
                                 --  It is in a project that is not directly
                                 --  imported. Report an error and
                                 --  invalidate the compilation.

                                 Write_Str ("Unit """);
                                 Write_Str
                                   (Get_Name_String (Src_Data.Id.Unit.Name));
                                 Write_Str (""" cannot import unit """);
                                 Write_Str
                                   (Get_Name_String (Source_2.Unit.Name));
                                 Write_Line (""":");

                                 Write_Str ("  """);
                                 Write_Str
                                   (Get_Name_String
                                      (Src_Data.Id.Project.Display_Name));
                                 Write_Str
                                   (""" does not directly import " &
                                    "project """);
                                 Write_Str
                                   (Get_Name_String
                                      (Source_2.Project.Display_Name));
                                 Write_Line ("""");

                                 Compilation_OK := False;

                              elsif not Source_2.In_Interfaces then
                                 --  It is not an interface of its project.
                                 --  Report an error and invalidate the
                                 --  compilation.

                                 Write_Str ("Unit """);
                                 Write_Str
                                   (Get_Name_String (Src_Data.Id.Unit.Name));
                                 Write_Str (""" cannot import unit """);
                                 Write_Str
                                   (Get_Name_String (Source_2.Unit.Name));
                                 Write_Line (""":");

                                 Write_Str
                                   ("  it is not part of the " &
                                    "interfaces of its project """);
                                 Write_Str
                                   (Get_Name_String
                                      (Source_2.Project.Display_Name));
                                 Write_Line ("""");
                                 Compilation_OK := False;
                              end if;
                           end if;
                        end if;
                     end if;
                  end loop;
               end loop;

               if Opt.No_Split_Units then

                  --  Initialized the list of subunits with the unit name

                  Subunits.Init;
                  Subunits.Append
                    (new String'(Get_Name_String (Src_Data.Id.Unit.Name)));

                  --  First check that the spec and the body are in the same
                  --  project.

                  for J in ALI.ALIs.Table (The_ALI).First_Unit ..
                    ALI.ALIs.Table (The_ALI).Last_Unit
                  loop
                     Check_Source (ALI.Units.Table (J).Sfile);
                  end loop;

                  --  Next, check the subunits, if any

                  declare
                     Subunit_Found : Boolean;
                     Already_Found : Boolean;
                     Last          : Positive;
                  begin
                     --  Loop until we don't find new subunits

                     loop
                        Subunit_Found := False;

                        for D in ALI.ALIs.Table (The_ALI).First_Sdep
                          .. ALI.ALIs.Table (The_ALI).Last_Sdep
                        loop
                           if ALI.Sdep.Table (D).Subunit_Name /= No_Name then
                              Get_Name_String
                                (ALI.Sdep.Table (D).Subunit_Name);

                              --  First check if we already found this subunit

                              Already_Found := False;
                              for K in 1 .. Subunits.Last loop
                                 if Name_Buffer (1 .. Name_Len) =
                                   Subunits.Table (K).all
                                 then
                                    Already_Found := True;
                                    exit;
                                 end if;
                              end loop;

                              if not Already_Found then
                                 --  Find the name of the parent

                                 Last := Name_Len - 1;
                                 while Last > 1 and then
                                       Name_Buffer (Last + 1) /= '.'
                                 loop
                                    Last := Last - 1;
                                 end loop;

                                 for J in 1 .. Subunits.Last loop
                                    if Subunits.Table (J).all =
                                      Name_Buffer (1 .. Last)
                                    then
                                       --  It is a new subunit, add it o the
                                       --  list and check if it is in the right
                                       --  project.

                                       Subunits.Append
                                         (new String'
                                            (Name_Buffer (1 .. Name_Len)));
                                       Subunit_Found := True;
                                       Check_Source (ALI.Sdep.Table (D).Sfile);
                                       exit;
                                    end if;
                                 end loop;
                              end if;
                           end if;
                        end loop;

                        exit when not Subunit_Found;
                     end loop;
                  end;
               end if;

               if Compilation_OK
                 and then Builder_Data (Src_Data.Tree).Closure_Needed
               then
                  Record_ALI_For (Src_Data, The_ALI);
               end if;
            end if;

            Free (Text);
         end if;
         return Compilation_OK;
      end Phase_2_ALI;

      --------------------------
      -- Set_Options_For_File --
      --------------------------

      procedure Set_Options_For_File (Id : Source_Id) is

         Config                   : Language_Config renames Id.Language.Config;
         Builder_Options_Instance : constant Builder_Comp_Option_Table_Ref :=
                                      Builder_Compiling_Options_HTable.Get
                                        (Id.Language.Name);
         Comp_Opt                 : constant Comp_Option_Table_Ref :=
                                      Compiling_Options_HTable.Get
                                        (Id.Language.Name);

         List    : Name_List_Index;
         Nam_Nod : Name_Node;
         First   : Boolean;
      begin
         Compilation_Options.Last := 0;

         --  1) The leading required switches

         List := Config.Compiler_Leading_Required_Switches;
         First := True;
         while List /= No_Name_List loop
            Nam_Nod := Project_Tree.Shared.Name_Lists.Table (List);
            Add_Option
              (Value   => Nam_Nod.Name,
               To      => Compilation_Options,
               Display => First or Opt.Verbose_Mode);
            First := False;
            List := Nam_Nod.Next;
         end loop;

         --  2) the compilation switches specified in package Builder
         --  for all compilers, following "-cargs", if any.

         for Index in 1 .. All_Language_Builder_Compiling_Options.Last loop
            Add_Option_Internal
              (Value   => All_Language_Builder_Compiling_Options.Table (Index),
               To      => Compilation_Options,
               Display => True);
         end loop;

         --  3) the compilation switches specified in package Builder
         --  for the compiler of the language, following
         --  -cargs:<language>.

         if Builder_Options_Instance /= null then
            for Index in 1 ..
              Builder_Compiling_Options.Last (Builder_Options_Instance.all)
            loop
               Add_Option_Internal
                 (Value   => Builder_Options_Instance.Table (Index),
                  To      => Compilation_Options,
                  Display => True);
            end loop;
         end if;

         --  4) The PIC option if it exists, for shared libraries

         if Id.Project.Library
           and then Id.Project.Library_Kind /= Static
         then
            List := Config.Compilation_PIC_Option;
            while List /= No_Name_List loop
               Nam_Nod := Project_Tree.Shared.Name_Lists.Table (List);
               Add_Option
                 (Value   => Nam_Nod.Name,
                  To      => Compilation_Options,
                  Display => True);
               List := Nam_Nod.Next;
            end loop;
         end if;

         --  5) Compiler'Switches(<source file name>), if it is
         --  defined, otherwise Compiler'Switches (<language name>),
         --  if defined.

         Add_Compilation_Switches (Id);

         --  4) the switches specified on the gprbuild command line
         --  for all compilers, following "-cargs", if any.

         for Index in 1 .. All_Language_Compiling_Options.Last loop
            Add_Option_Internal
              (Value   => All_Language_Compiling_Options.Table (Index),
               To      => Compilation_Options,
               Display => True);
         end loop;

         --  6) the switches specified on the gprbuild command line
         --  for the compiler of the language, following
         --  -cargs:<language>.

         if Comp_Opt /= null then
            for Index in 1 .. Compiling_Options.Last (Comp_Opt.all) loop
               Add_Option_Internal
                 (Value   => Comp_Opt.Table (Index),
                  To      => Compilation_Options,
                  Display => True);
            end loop;
         end if;
      end Set_Options_For_File;

      -------------------------
      -- Check_Switches_File --
      -------------------------

      function Check_Switches_File (Id : Source_Id) return Boolean is

         File : Ada.Text_IO.File_Type;

         function Assert_Line (Current : String) return Boolean;
         --  Return False if Current is not the next line in the switches file

         -----------------
         -- Assert_Line --
         -----------------

         function Assert_Line (Current : String) return Boolean is
            Line : String (1 .. 1_000);
            Last : Natural;
         begin
            if End_Of_File (File) then
               if Opt.Verbose_Mode then
                  Write_Line ("    -> switches file has fewer switches");
               end if;

               Close (File);
               return False;
            end if;

            Get_Line (File, Line, Last);

            if Line (1 .. Last) /= Current then
               if Opt.Verbose_Mode then
                  Write_Line ("    -> switches file has different line");
                  Write_Line ("       " & Line (1 .. Last));
                  Write_Line ("       " & Current);
               end if;

               Close (File);
               return False;
            end if;
            return True;
         end Assert_Line;

         List    : Name_List_Index;
         Nam_Nod : Name_Node;
      begin
         Open (File, In_File, Get_Name_String (Id.Switches_Path));

         if not Assert_Line (String (Id.Object_TS)) then
            return True;
         end if;

         for Index in 1 .. Compilation_Options.Last loop
            if not Assert_Line (Compilation_Options.Options (Index).all) then
               return True;
            end if;
         end loop;

         List := Id.Language.Config.Compiler_Trailing_Required_Switches;

         while List /= No_Name_List loop
            Nam_Nod := Project_Tree.Shared.Name_Lists.Table (List);

            if not Assert_Line (Get_Name_String (Nam_Nod.Name)) then
               return True;
            end if;

            List := Nam_Nod.Next;
         end loop;

         if not End_Of_File (File) then
            if Opt.Verbose_Mode then
               Write_Line ("    -> switches file has more switches");
            end if;

            Close (File);
            return True;
         end if;

         Close (File);
         return False;

      exception
         when others =>
            if Opt.Verbose_Mode then
               Write_Line ("    -> no switches file");
            end if;
            return True;
      end Check_Switches_File;

      ------------------------
      -- Update_Object_Path --
      ------------------------

      procedure Update_Object_Path
        (Id : Source_Id; Source_Project : Project_Id) is
      begin
         Id.Object_Project := Source_Project;

         if Id.Object_Project /= Id.Project then
            if Id.Object /= No_File then
               Get_Name_String
                 (Id.Object_Project.Object_Directory.Display_Name);
               Add_Str_To_Name_Buffer (Get_Name_String (Id.Object));
               Id.Object_Path := Name_Find;
            end if;

            if Id.Dep_Name /= No_File then
               Get_Name_String
                 (Id.Object_Project.Object_Directory.Display_Name);
               Add_Str_To_Name_Buffer (Get_Name_String (Id.Dep_Name));
               Id.Dep_Path := Name_Find;
            end if;

            if Id.Switches /= No_File then
               Get_Name_String
                 (Id.Object_Project.Object_Directory.Display_Name);
               Add_Str_To_Name_Buffer (Get_Name_String (Id.Switches));
               Id.Switches_Path := Name_Find;
            end if;
         end if;
      end Update_Object_Path;

      ----------------------------
      -- Add_Dependency_Options --
      ----------------------------

      procedure Add_Dependency_Options (Id : Source_Id) is
         List : Name_List_Index := Id.Language.Config.Dependency_Option;
         Node : Name_Node;
      begin
         while List /= No_Name_List loop
            Node := Project_Tree.Shared.Name_Lists.Table (List);
            List := Node.Next;

            if List = No_Name_List then
               Add_Option
                 (Value   => Get_Name_String (Node.Name)
                    & Get_Name_String (Id.Dep_Name),
                  To      => Compilation_Options,
                  Display => Opt.Verbose_Mode);
            else
               Add_Option
                 (Value   => Node.Name,
                  To      => Compilation_Options,
                  Display => Opt.Verbose_Mode);
            end if;
         end loop;
      end Add_Dependency_Options;

      ------------------------------
      -- Add_Object_File_Switches --
      ------------------------------

      procedure Add_Object_File_Switches (Id : Source_Id) is
         List : Name_List_Index := Id.Language.Config.Object_File_Switches;
         Node : Name_Node;
      begin
         if List /= No_Name_List then
            loop
               Node := Project_Tree.Shared.Name_Lists.Table (List);
               exit when Node.Next = No_Name_List;

               Add_Option
                 (Node.Name,
                  To      => Compilation_Options,
                  Display => Opt.Verbose_Mode or else Id.Index /= 0);
               List := Node.Next;
            end loop;

            Get_Name_String (Node.Name);
            Add_Str_To_Name_Buffer (Get_Name_String (Id.Object));

            Add_Option
              (Name_Buffer (1 .. Name_Len),
               To      => Compilation_Options,
               Display => Opt.Verbose_Mode or else Id.Index /= 0);

         --  Always specify object-file for a multi-unit source file

         elsif Id.Index /= 0 then
            Add_Option
              ("-o",
               To      => Compilation_Options,
               Display => True);
            Add_Option
              (Get_Name_String (Id.Object),
               To      => Compilation_Options,
               Display => True);
         end if;
      end Add_Object_File_Switches;

      ------------------------------
      -- Add_Config_File_Switches --
      ------------------------------

      procedure Add_Config_File_Switches
        (Id             : Source_Id;
         Source_Project : Project_Id)
      is
         Config           : constant Language_Config := Id.Language.Config;
         Config_File_Path : Path_Name_Type;
      begin
         if Config.Config_File_Switches /= No_Name_List
           and then (Config.Config_Body /= No_Name
                     or else Config.Config_Body_Index /= No_Name
                     or else Config.Config_Body_Pattern /= No_Name
                     or else Config.Config_Spec /= No_Name
                     or else Config.Config_Spec_Index /= No_Name
                     or else Config.Config_Spec_Pattern /= No_Name)
         then
            Create_Config_File
              (For_Project => Source_Project,
               Config      => Config,
               Language    => Id.Language.Name);

            if Source_Project.Config_File_Name /= No_Path then
               Add_Config_File_Switch
                 (Config    => Config,
                  Path_Name => Source_Project.Config_File_Name);
            end if;

            if not Config.Config_File_Unique then
               Config_File_Path :=
                 Config_File_For
                   (Project        => Main_Project,
                    Package_Name   => Name_Builder,
                    Attribute_Name => Name_Global_Config_File,
                    Language       => Id.Language.Name);

               if Config_File_Path /= No_Path then
                  Add_Config_File_Switch
                    (Config    => Config,
                     Path_Name => Config_File_Path);
               end if;

               Config_File_Path :=
                 Config_File_For
                   (Project        => Source_Project,
                    Package_Name   => Name_Compiler,
                    Attribute_Name => Name_Local_Config_File,
                    Language       => Id.Language.Name);

               if Config_File_Path /= No_Path then
                  Add_Config_File_Switch
                    (Config    => Config,
                     Path_Name => Config_File_Path);
               end if;
            end if;
         end if;
      end Add_Config_File_Switches;

      -------------------------------
      -- Add_Mapping_File_Switches --
      -------------------------------

      function Add_Mapping_File_Switches
        (Source         : Queue.Source_Info;
         Source_Project : Project_Id) return Path_Name_Type
      is
         List              : Name_List_Index :=
                               Source.Id.Language.Config.Mapping_File_Switches;
         Node              : Name_Node;
         Mapping_File_Path : Path_Name_Type;
      begin
         if List /= No_Name_List then

            --  Check if there is a temporary mapping file we can use

            Mapping_File_Path := Mapping_Files_Htable.Get_First
              (Source.Id.Language.Mapping_Files);

            if Mapping_File_Path /= No_Path then
               --  Reuse this temporary mapping file and remove its
               --  name from the HTable so that it is not reused
               --  before the compilation terminates.

               Mapping_Files_Htable.Remove
                 (Source.Id.Language.Mapping_Files, Mapping_File_Path);

            else
               --  Create a new temporary mapping file, as there are
               --  none that can be reused.

               Prj.Env.Create_Mapping_File
                 (Project  => Source_Project,
                  Language => Source.Id.Language.Name,
                  In_Tree  => Source.Tree,
                  Name     => Mapping_File_Path);
            end if;

            while List /= No_Name_List loop
               Node := Source.Tree.Shared.Name_Lists.Table (List);
               List := Node.Next;

               if List /= No_Name_List then
                  Add_Option
                    (Value   => Node.Name,
                     To      => Compilation_Options,
                     Display => Opt.Verbose_Mode);

               else
                  Get_Name_String (Node.Name);
                  Add_Str_To_Name_Buffer (Get_Name_String (Mapping_File_Path));
                  Add_Option
                    (Name_Buffer (1 .. Name_Len),
                     To      => Compilation_Options,
                     Display => Opt.Verbose_Mode);
               end if;
            end loop;

            return Mapping_File_Path;
         else
            return No_Path;
         end if;
      end Add_Mapping_File_Switches;

      -----------------------------
      -- Add_Multi_Unit_Switches --
      -----------------------------

      procedure Add_Multi_Unit_Switches (Id : Source_Id) is
         List : Name_List_Index := Id.Language.Config.Multi_Unit_Switches;
      begin
         if Id.Index /= 0
           and then List /= No_Name_List
         then
            declare
               Index_Img : constant String := Id.Index'Img;
               Node      : Name_Node;

            begin
               loop
                  Node := Project_Tree.Shared.Name_Lists.Table (List);
                  exit when Node.Next = No_Name_List;

                  Add_Option
                    (Node.Name,
                     To      => Compilation_Options,
                     Display => True);
                  List := Node.Next;
               end loop;

               Get_Name_String (Node.Name);
               Add_Str_To_Name_Buffer (Index_Img (2 .. Index_Img'Last));
               Add_Option
                 (Name_Buffer (1 .. Name_Len),
                  To      => Compilation_Options,
                  Display => True);
            end;
         end if;
      end Add_Multi_Unit_Switches;

      ---------------------------
      -- Add_Trailing_Switches --
      ---------------------------

      procedure Add_Trailing_Switches (Id : Source_Id) is
         List : Name_List_Index :=
                  Id.Language.Config.Compiler_Trailing_Required_Switches;
         Node : Name_Node;
      begin
         while List /= No_Name_List loop
            Node := Project_Tree.Shared.Name_Lists.Table (List);
            Add_Option
              (Node.Name,
               To      => Compilation_Options,
               Display => Opt.Verbose_Mode);
            List := Node.Next;
         end loop;
      end Add_Trailing_Switches;

      ---------------------------------
      -- Add_Name_Of_Source_Switches --
      ---------------------------------

      procedure Add_Name_Of_Source_Switches (Id : Source_Id) is
         List        : Name_List_Index :=
                         Id.Language.Config.Source_File_Switches;
         Node        : Name_Node;
         Source_Path : String_Access;
      begin
         --  Add any source file prefix

         if List /= No_Name_List then
            loop
               Node := Project_Tree.Shared.Name_Lists.Table (List);
               exit when Node.Next = No_Name_List;

               Add_Option
                 (Node.Name,
                  To      => Compilation_Options,
                  Display => Opt.Verbose_Mode or else Id.Index /= 0);
               List := Node.Next;
            end loop;
         end if;

         --  Then handle the source file

         Get_Name_String (Id.Path.Display_Name);

         case Id.Language.Config.Path_Syntax is
            when Canonical =>
               Source_Path := new String'(Name_Buffer (1 .. Name_Len));

            when Host =>
               Source_Path := To_Host_File_Spec (Name_Buffer (1 .. Name_Len));
         end case;

         if Node.Name = No_Name then
            Add_Option_Internal
              (Source_Path,
               To          => Compilation_Options,
               Display     => True,
               Simple_Name => not Opt.Verbose_Mode);

         else
            Get_Name_String (Node.Name);
            Add_Option
              (Name_Buffer (1 .. Name_Len) & Source_Path.all,
               To          => Compilation_Options,
               Display     => True,
               Simple_Name => not Opt.Verbose_Mode);
         end if;
      end Add_Name_Of_Source_Switches;

      ---------------------------------
      -- Spawn_Compiler_And_Register --
      ---------------------------------

      procedure Spawn_Compiler_And_Register
        (Source                 : Queue.Source_Info;
         Source_Project         : Project_Id;
         Compiler_Path          : String;
         Mapping_File_Path      : Path_Name_Type;
         Last_Switches_For_File : Integer)
      is

         procedure Add_Process
           (Pid            : Process_Id;
            Source         : Queue.Source_Info;
            Source_Project : Project_Id;
            Mapping_File   : Path_Name_Type;
            Purpose        : Process_Purpose;
            Options        : String_List_Access);
         --  Add compilation process and indicate that the object directory is
         --  busy.

         -----------------
         -- Add_Process --
         -----------------

         procedure Add_Process
           (Pid            : Process_Id;
            Source         : Queue.Source_Info;
            Source_Project : Project_Id;
            Mapping_File   : Path_Name_Type;
            Purpose        : Process_Purpose;
            Options        : String_List_Access)
         is
         begin
            Compilation_Htable.Set
              (Pid,
               (Pid, Source, Source_Project, Mapping_File, Purpose, Options));
            Outstanding_Compiles := Outstanding_Compiles + 1;

            Queue.Set_Obj_Dir_Busy (Source.Id.Project.Object_Directory.Name);
         end Add_Process;

         Pid     : Process_Id;
         Options : String_List_Access;
      begin
         if not Opt.Quiet_Output then
            if Opt.Verbose_Mode then
               Write_Str (Compiler_Path);

            else
               Name_Len := 0;
               Add_Str_To_Name_Buffer (Base_Name (Compiler_Path));

               if Executable_Suffix'Length /= 0 and then
                 Name_Len > Executable_Suffix'Length and then
                 Name_Buffer
                   (Name_Len - Executable_Suffix'Length + 1 .. Name_Len)
                   = Executable_Suffix.all
               then
                  Name_Len := Name_Len - Executable_Suffix'Length;
               end if;

               Write_Str (Name_Buffer (1 .. Name_Len));
            end if;

            for Option in 1 .. Compilation_Options.Last loop
               if Compilation_Options.Visible (Option) then
                  Write_Char (' ');

                  if Compilation_Options.Simple_Name (Option) then
                     Write_Str
                       (Base_Name (Compilation_Options.Options (Option).all));

                  else
                     Write_Str (Compilation_Options.Options (Option).all);
                  end if;
               end if;
            end loop;

            Write_Eol;
         end if;

         Pid := GNAT.OS_Lib.Non_Blocking_Spawn
           (Compiler_Path,
            Compilation_Options.Options (1 .. Compilation_Options.Last));

         if Last_Switches_For_File >= 0 then
            Compilation_Options.Last := Last_Switches_For_File;
            Add_Trailing_Switches (Source.Id);
            Options :=
              new String_List'
                (Compilation_Options.Options (1 .. Compilation_Options.Last));
         end if;

         Add_Process
           (Pid            => Pid,
            Source         => Source,
            Source_Project => Source_Project,
            Mapping_File   => Mapping_File_Path,
            Purpose        => Compilation,
            Options        => Options);
      end Spawn_Compiler_And_Register;

      ------------------------------
      -- Get_Compatible_Languages --
      ------------------------------

      function Get_Compatible_Languages
        (Lang : Language_Ptr) return Name_Ids
      is
         NL        : Name_List_Index :=
                       Lang.Config.Include_Compatible_Languages;
         Languages : Name_Ids
           (1 .. 1 + Length (Project_Tree.Shared.Name_Lists, NL));
         Index     : Positive := 1;
      begin
         Languages (Index) := Lang.Name;

         while NL /= No_Name_List loop
            Index := Index + 1;
            Languages (Index) :=
              Project_Tree.Shared.Name_Lists.Table (NL).Name;
            NL := Project_Tree.Shared.Name_Lists.Table (NL).Next;
         end loop;

         return Languages;
      end Get_Compatible_Languages;

      -------------------------------
      -- Prepare_Include_Path_File --
      -------------------------------

      procedure Prepare_Include_Path_File
        (Data    : out Local_Project_Data;
         Project : Project_Id;
         Lang    : Language_Ptr)
      is
         FD     : File_Descriptor;
         Status : Boolean;
      begin
         Get_Directories
           (Project_Tree => Project_Tree,
            For_Project  => Project,
            Activity     => Compilation,
            Languages    => Get_Compatible_Languages (Lang));

         Prj.Env.Create_New_Path_File
           (Shared    => Project_Tree.Shared,
            Path_FD   => FD,
            Path_Name => Data.Include_Path_File);

         if FD = Invalid_FD then
            Fail_Program
              (Project_Tree, "could not create temporary path file");
         end if;

         for Index in 1 .. Directories.Last loop
            Get_Name_String (Directories.Table (Index));
            Name_Len := Name_Len + 1;
            Name_Buffer (Name_Len) := ASCII.LF;
            if Write (FD, Name_Buffer (1)'Address, Name_Len) /= Name_Len then
               Fail_Program
                 (Project_Tree,
                  "disk full when writing include path file");
            end if;
         end loop;

         Close (FD, Status);

         if not Status then
            Fail_Program
              (Project_Tree,
               "disk full when writing include path file");
         end if;
      end Prepare_Include_Path_File;

      ------------------------------------
      -- Prepare_Imported_Dirs_Switches --
      ------------------------------------

      procedure Prepare_Imported_Dirs_Switches
        (Data     : out Local_Project_Data;
         Project  : Project_Id;
         Lang     : Language_Ptr)
      is
         Len       : constant Natural :=
                       Length
                         (Project_Tree.Shared.Name_Lists,
                          Lang.Config.Include_Option);
         Host_Path : String_Access;
         Last      : Natural := 0;
         List      : Name_List_Index;
         Nam       : Name_Node;
      begin
         Get_Directories
           (Project_Tree => Project_Tree,
            For_Project  => Project,
            Activity     => Compilation,
            Languages    => Get_Compatible_Languages (Lang));

         Free (Data.Imported_Dirs_Switches);
         Data.Imported_Dirs_Switches :=
           new String_List (1 .. Directories.Last * Len);

         for Index in 1 .. Directories.Last loop
            List := Lang.Config.Include_Option;
            while List /= No_Name_List loop
               Nam := Project_Tree.Shared.Name_Lists.Table (List);
               exit when Nam.Next = No_Name_List;
               Last := Last + 1;
               Data.Imported_Dirs_Switches (Last) :=
                 new String'(Get_Name_String (Nam.Name));
               List := Nam.Next;
            end loop;

            Get_Name_String (Directories.Table (Index));

            while Name_Len > 1
              and then (Name_Buffer (Name_Len) = Directory_Separator
                        or else Name_Buffer (Name_Len) = '/')
            loop
               Name_Len := Name_Len - 1;
            end loop;

            Last := Last + 1;

            --  Concatenate the last switch and the path in a single option

            case Lang.Config.Path_Syntax is
               when Canonical =>
                  Data.Imported_Dirs_Switches (Last) := new String'
                    (Get_Name_String (Nam.Name) & Name_Buffer (1 .. Name_Len));

               when Host =>
                  Host_Path := To_Host_Dir_Spec
                    (Name_Buffer (1 .. Name_Len), False);
                  Data.Imported_Dirs_Switches (Last) := new String'
                    (Get_Name_String (Nam.Name) & Host_Path.all);
                  Free (Host_Path);
            end case;
         end loop;
      end Prepare_Imported_Dirs_Switches;

      ------------------------------
      -- Set_Env_For_Include_Dirs --
      ------------------------------

      procedure Set_Env_For_Include_Dirs (Id : Source_Id) is
         Data : Local_Project_Data :=
                  Local_Projects_HT.Get (Local_Projects, Id.Object_Project);
      begin
         --  Prepare (if not already done) the data for Project/Lang.
         --  All files for a given language are processed sequentially, before
         --  we switch to the next language, so we are only preparing once per
         --  language here

         if Data.Include_Language /= Id.Language then
            Free (Data.Include_Path);
            Free (Data.Imported_Dirs_Switches);
            Data := No_Local_Project_Data;

            if Id.Language.Config.Include_Option /= No_Name_List then
               Prepare_Imported_Dirs_Switches
                 (Data, Id.Object_Project, Id.Language);

            elsif Id.Language.Config.Include_Path_File /= No_Name then
               if Id.Language.Config.Mapping_File_Switches = No_Name_List
                 or else
                  Opt.Use_Include_Path_File
               then
                  Prepare_Include_Path_File
                    (Data, Id.Object_Project, Id.Language);
               end if;

            elsif Id.Language.Config.Include_Path /= No_Name then
               Get_Directories
                 (Project_Tree => Project_Tree,
                  For_Project  => Id.Object_Project,
                  Activity     => Compilation,
                  Languages    => Get_Compatible_Languages (Id.Language));
               Data.Include_Path := Create_Path_From_Dirs;
            end if;

            Data.Include_Language := Id.Language;

            Local_Projects_HT.Set (Local_Projects, Id.Object_Project, Data);
         end if;

         --  Reset environment variables if they have changed
         --  ??? Ideally, we should set them when spawning the process, in
         --  which case it would be less expensive to set and could be set
         --  every time

         if Id.Object_Project /= Current_Project
           or else Id.Language /= Current_Language_Ind
         then
            Current_Project      := Id.Object_Project;
            Current_Language_Ind := Id.Language;

            if Data.Include_Path_File /= No_Path then
               Setenv (Get_Name_String (Id.Language.Config.Include_Path_File),
                       Get_Name_String (Data.Include_Path_File));

            elsif Data.Include_Path /= null then
               Setenv (Get_Name_String (Id.Language.Config.Include_Path),
                       Data.Include_Path.all);

               if Opt.Verbose_Mode then
                  Write_Str
                    (Get_Name_String (Id.Language.Config.Include_Path));
                  Write_Str (" = ");
                  Write_Line (Data.Include_Path.all);
               end if;
            end if;
         end if;

         --  But always set the switches

         if Data.Imported_Dirs_Switches /= null then
            for J in Data.Imported_Dirs_Switches'Range loop
               if Data.Imported_Dirs_Switches (J)'Length > 0 then
                  Add_Option_Internal
                    (Value   => Data.Imported_Dirs_Switches (J),
                     To      => Compilation_Options,
                     Display => Opt.Verbose_Mode);
               end if;
            end loop;
         end if;
      end Set_Env_For_Include_Dirs;

      -----------------------------
      -- Process_Project_Phase_1 --
      -----------------------------

      procedure Process_Project_Phase_1 (Source : Queue.Source_Info) is
         Id                     : constant Source_Id := Source.Id;
         Project_Tree           : constant Project_Tree_Ref := Source.Tree;
         Source_Project         : constant Project_Id :=
                                    Ultimate_Extending_Project_Of
                                      (Source.Id.Project);
         Compilation_Needed     : Boolean := True;
         Last_Switches_For_File : Integer;
         Mapping_File           : Path_Name_Type;
         The_ALI                : ALI.ALI_Id;

      begin
         if Always_Compile or else not Source_Project.Externally_Built then
            Need_To_Compile
              (Source         => Source.Id,
               Tree           => Source.Tree,
               In_Project     => Source_Project,
               Must_Compile   => Compilation_Needed,
               The_ALI        => The_ALI,
               Object_Check   => Object_Checked,
               Always_Compile => Always_Compile);

            if Compilation_Needed and then Opt.Keep_Going then
               --  When in Keep__Going mode first check that we did not already
               --  tried to compile this source as part of another import of
               --  the corresponding project file.

               for Index in 1 .. Bad_Compilations.Last loop
                  if Source.Id.File = Bad_Compilations.Table (Index).File then
                     Compilation_Needed := False;
                     exit;
                  end if;
               end loop;
            end if;

            if Compilation_Needed or else Opt.Check_Switches then
               Set_Options_For_File (Source.Id);

               if Opt.Check_Switches and then not Compilation_Needed then
                  Compilation_Needed := Check_Switches_File (Source.Id);
               end if;
            end if;

            if Compilation_Needed then
               Update_Object_Path (Source.Id, Source_Project);
               Change_To_Object_Directory (Source_Project);

               --  Record the last recorded option index, to be able to
               --  write the switches file later.

               if Id.Language.Config.Object_Generated then
                  Last_Switches_For_File := Compilation_Options.Last;
               else
                  Last_Switches_For_File := -1;
               end if;

               Add_Dependency_Options (Id);
               Set_Env_For_Include_Dirs (Id);
               Add_Config_File_Switches (Id, Source_Project);
               Mapping_File := Add_Mapping_File_Switches
                 (Source, Source_Project);
               Add_Trailing_Switches (Id);
               Add_Name_Of_Source_Switches (Id);
               Add_Object_File_Switches (Id);
               Add_Multi_Unit_Switches (Id);

               Spawn_Compiler_And_Register
                 (Source         => Source,
                  Source_Project => Source_Project,
                  Compiler_Path  => Get_Compiler_Driver_Path
                    (Project_Tree, Id.Language).all,
                  Mapping_File_Path => Mapping_File,
                  Last_Switches_For_File => Last_Switches_For_File);

            elsif Builder_Data (Source.Tree).Closure_Needed
              and then Id.Language.Config.Dependency_Kind = ALI_File
            then
               Record_ALI_For (Source, The_ALI);

            else
               ALI.Initialize_ALI;
               ALI.Util.Initialize_ALI_Source;
            end if;
         end if;
      end Process_Project_Phase_1;

      --------------------------------
      -- Must_Exit_Because_Of_Error --
      --------------------------------

      function Must_Exit_Because_Of_Error return Boolean is
         Source_Identity : Queue.Source_Info;
         Compilation_OK  : Boolean;
      begin
         if Bad_Compilations.Last > 0 and then not Opt.Keep_Going then
            while Outstanding_Compiles > 0 loop
               Await_Compile (Source_Identity, Compilation_OK);

               if not Compilation_OK then
                  Record_Failure (Source_Identity.Id);
               end if;
            end loop;

            return True;
         end if;
         return False;
      end Must_Exit_Because_Of_Error;

      -------------------------------
      -- Start_Compile_If_Possible --
      -------------------------------

      procedure Start_Compile_If_Possible is
         Found  : Boolean;
         Source : Queue.Source_Info;
      begin
         if not Queue.Is_Empty
           and then Outstanding_Compiles < Opt.Maximum_Processes
         then
            Queue.Extract (Found, Source);
            if Found then
               Initialize_Source_Record (Source.Id);
               Process_Project_Phase_1 (Source);
            end if;
         end if;
      end Start_Compile_If_Possible;

      -----------------------------
      -- Wait_For_Available_Slot --
      -----------------------------

      procedure Wait_For_Available_Slot is
         Source_Identity : Queue.Source_Info;
         Compilation_OK  : Boolean;
         No_Check        : Boolean;
      begin
         if Outstanding_Compiles = Opt.Maximum_Processes
           or else (Queue.Is_Virtually_Empty and then Outstanding_Compiles > 0)
         then
            Await_Compile (Source_Identity, Compilation_OK);

            if Compilation_OK then
               --  Check if dependencies are on sources in Interfaces and,
               --  when --direct-import-only is used, the imported sources
               --  come from directly withed projects.

               Imports.Reset;
               Included_Sources.Set_Last (0);

               case Source_Identity.Id.Language.Config.Dependency_Kind is
                  when None     => null;
                  when Makefile =>
                     Compilation_OK := Phase_2_Makefile (Source_Identity);
                  when ALI_File =>
                     Compilation_OK := Phase_2_ALI (Source_Identity);
               end case;

               --  If the compilation was invalidated, delete the compilation
               --  artifacts.

               if not Compilation_OK then
                  if Source_Identity.Id.Dep_Path /= No_Path then
                     Delete_File
                       (Get_Name_String (Source_Identity.Id.Dep_Path),
                        No_Check);
                  end if;

                  if Source_Identity.Id.Object_Path /= No_Path then
                     Delete_File
                       (Get_Name_String (Source_Identity.Id.Object_Path),
                        No_Check);
                  end if;

                  if Source_Identity.Id.Switches_Path /= No_Path then
                     Delete_File
                       (Get_Name_String (Source_Identity.Id.Switches_Path),
                        No_Check);
                  end if;
               end if;
            end if;

            if not Compilation_OK then
               Record_Failure (Source_Identity.Id);
            end if;
         end if;
      end Wait_For_Available_Slot;

   --  Start of processing for Compilation_Phase

   begin
      Outstanding_Compiles := 0;

      --  Then process each files in the queue (new files might be added to
      --  the queue as a result)

      Compilation_Loop :
      while not Queue.Is_Empty or else Outstanding_Compiles > 0 loop
         exit Compilation_Loop when Must_Exit_Because_Of_Error;
         Start_Compile_If_Possible;
         Wait_For_Available_Slot;

         if Opt.Display_Compilation_Progress then
            Write_Str ("completed ");
            Write_Int (Int (Queue.Processed));
            Write_Str (" out of ");
            Write_Int (Int (Queue.Size));
            Write_Str (" (");
            Write_Int (Int (((Queue.Processed) * 100) / Queue.Size));
            Write_Str ("%)...");
            Write_Eol;
         end if;
      end loop Compilation_Loop;

      --  Release local memory

      declare
         Data : Local_Project_Data :=
                  Local_Projects_HT.Get_First (Local_Projects);
      begin
         while Data /= No_Local_Project_Data loop
            Free (Data.Include_Path);
            Free (Data.Imported_Dirs_Switches);
            Data := Local_Projects_HT.Get_Next (Local_Projects);
         end loop;

         Local_Projects_HT.Reset (Local_Projects);
      end;
   end Compilation_Phase;

   ---------------------
   -- Config_File_For --
   ---------------------

   function Config_File_For
     (Project        : Project_Id;
      Package_Name   : Name_Id;
      Attribute_Name : Name_Id;
      Language       : Name_Id)
      return Path_Name_Type
   is
      Config_Package      : constant Package_Id :=
        Value_Of
          (Name        => Package_Name,
           In_Packages => Project.Decl.Packages,
           Shared      => Project_Tree.Shared);
      Config_Variable     : Variable_Value :=
        Value_Of
          (Name                    => Language,
           Attribute_Or_Array_Name => Attribute_Name,
           In_Package              => Config_Package,
           Shared                  => Project_Tree.Shared);

   begin
      --  Get the config pragma attribute when the language is Ada and the
      --  config file attribute is not declared.

      if Config_Variable = Nil_Variable_Value and then
        Config_Package /= No_Package and then
        Language = Name_Ada
      then
         if Attribute_Name = Name_Global_Config_File then
            Config_Variable :=
              Value_Of
                (Variable_Name => Name_Global_Configuration_Pragmas,
                 In_Variables  => Project_Tree.Shared.Packages.Table
                   (Config_Package).Decl.Attributes,
                 Shared        => Project_Tree.Shared);

         elsif Attribute_Name = Name_Local_Config_File then
            Config_Variable :=
              Value_Of
                (Variable_Name => Name_Local_Configuration_Pragmas,
                 In_Variables  => Project_Tree.Shared.Packages.Table
                   (Config_Package).Decl.Attributes,
                 Shared        => Project_Tree.Shared);
         end if;
      end if;

      if Config_Variable = Nil_Variable_Value then
         return No_Path;

      else
         Get_Name_String (Config_Variable.Value);

         if Name_Len = 0 then
            return No_Path;

         else
            return
              Absolute_Path
                (Path_Name_Type (Config_Variable.Value),
                 Config_Variable.Project);
         end if;
      end if;
   end Config_File_For;

   ---------------
   -- Copyright --
   ---------------

   procedure Copyright is
   begin
      --  Only output the Copyright notice once

      if not Copyright_Output then
         Copyright_Output := True;
         Display_Version
           ("GPRBUILD", "2004", Version_String => Gpr_Version_String);
      end if;
   end Copyright;

   ------------------------
   -- Create_Config_File --
   ------------------------

   procedure Create_Config_File
     (For_Project : Project_Id;
      Config      : Language_Config;
      Language    : Name_Id)
   is

      File_Name : Path_Name_Type  := No_Path;
      File      : File_Descriptor := Invalid_FD;

      Source   : Source_Id;
      Iter     : Source_Iterator;

      procedure Check
        (Project : Project_Id;
         Tree    : Project_Tree_Ref;
         Dummy   : in out Boolean);
      --  Check the naming schemes of the different projects of the project
      --  tree. For each different naming scheme issue the pattern config
      --  declarations.

      procedure Check_Temp_File;
      --  Check if a temp file has been created. If not, create one

      procedure Copy_Config_File
        (Project        : Project_Id;
         Package_Name   : Name_Id;
         Attribute_Name : Name_Id;
         Language       : Name_Id);
      --  If a specified config file exists, copy it in the temporary config
      --  file.

      procedure Put_Line (File : File_Descriptor; S : String);
      --  Output procedure, analogous to normal Text_IO proc of same name

      -----------
      -- Check --
      -----------

      procedure Check
        (Project : Project_Id;
         Tree    : Project_Tree_Ref;
         Dummy   : in out Boolean)
      is
         pragma Unreferenced (Dummy, Tree);
         Lang_Id   : Language_Ptr := Project.Languages;

         Current_Naming : Positive := 1;

         procedure Replace;

         -------------
         -- Replace --
         -------------

         procedure Replace is
            Cur : Positive := 1;

            procedure Substitute (N : File_Name_Type);
            procedure Substitute (Name : String);

            ----------------
            -- Substitute --
            ----------------

            procedure Substitute (N : File_Name_Type) is
            begin
               if N = No_File then
                  Cur := Cur + 1;

               else
                  Substitute (Get_Name_String (N));
               end if;
            end Substitute;

            procedure Substitute (Name : String) is
            begin
               Name_Buffer
                 (Cur + Name'Length .. Name_Len - 2 + Name'Length) :=
                 Name_Buffer (Cur + 2 .. Name_Len);
               Name_Buffer (Cur .. Cur + Name'Length - 1) := Name;
               Name_Len := Name_Len - 2 + Name'Length;
               Cur := Cur + Name'Length;
            end Substitute;

         begin
            while Cur < Name_Len loop
               if Name_Buffer (Cur) = '%' then
                  case Name_Buffer (Cur + 1) is
                     when 'b' =>
                        Substitute (Lang_Id.Config.Naming_Data.Body_Suffix);

                     when 's' =>
                        Substitute (Lang_Id.Config.Naming_Data.Spec_Suffix);

                     when 'd' =>
                        Substitute
                          (Lang_Id.Config.Naming_Data.Dot_Replacement);

                     when 'c' =>
                        Substitute
                          (Image (Lang_Id.Config.Naming_Data.Casing));

                     when '%' =>
                        Name_Buffer (Cur .. Name_Len - 1) :=
                          Name_Buffer (Cur + 1 .. Name_Len);
                        Name_Len := Name_Len - 1;
                        Cur := Cur + 1;

                     when others =>
                        Cur := Cur + 1;
                  end case;

               else
                  Cur := Cur + 1;
               end if;
            end loop;
         end Replace;

      begin
         if Current_Verbosity = High then
            Write_Str ("Checking project file """);
            Write_Str (Namet.Get_Name_String (Project.Name));
            Write_Str (""".");
            Write_Eol;
         end if;

         while Lang_Id /= No_Language_Index loop
            exit when Lang_Id.Name = Language;
            Lang_Id := Lang_Id.Next;
         end loop;

         if Lang_Id /= No_Language_Index then
            Current_Naming := Naming_Datas.First;

            while Current_Naming <= Naming_Datas.Last loop
               exit when Naming_Datas.Table (Current_Naming) =
                 Lang_Id.Config.Naming_Data;
               Current_Naming := Current_Naming + 1;
            end loop;

            if Current_Naming > Naming_Datas.Last then
               Naming_Datas.Increment_Last;
               Naming_Datas.Table (Naming_Datas.Last) :=
                 Lang_Id.Config.Naming_Data;

               Check_Temp_File;

               if Lang_Id.Config.Config_Spec_Pattern /= No_Name then
                  Get_Name_String (Lang_Id.Config.Config_Spec_Pattern);
                  Replace;
                  Put_Line (File, Name_Buffer (1 .. Name_Len));
               end if;

               if Lang_Id.Config.Config_Body_Pattern /= No_Name then
                  Get_Name_String (Lang_Id.Config.Config_Body_Pattern);
                  Replace;
                  Put_Line (File, Name_Buffer (1 .. Name_Len));
               end if;
            end if;
         end if;
      end Check;

      ---------------------
      -- Check_Temp_File --
      ---------------------

      procedure Check_Temp_File is
      begin
         if File = Invalid_FD then
            Tempdir.Create_Temp_File (File, Name => File_Name);

            if File = Invalid_FD then
               Fail_Program
                 (Project_Tree,
                  "unable to create temporary configuration pragmas file");

            else
               Record_Temp_File (Project_Tree.Shared, File_Name);

               if Opt.Verbose_Mode and then Opt.Verbosity_Level > Opt.Low then
                  Write_Str ("Creating temp file """);
                  Write_Str (Get_Name_String (File_Name));
                  Write_Line ("""");
               end if;
            end if;
         end if;
      end Check_Temp_File;

      ----------------------
      -- Copy_Config_File --
      ----------------------

      procedure Copy_Config_File
        (Project        : Project_Id;
         Package_Name   : Name_Id;
         Attribute_Name : Name_Id;
         Language       : Name_Id)
      is
         Config_File_Path : constant Path_Name_Type :=
                              Config_File_For
                                (Project, Package_Name,
                                 Attribute_Name, Language);
         Config_File      : Ada.Text_IO.File_Type;
         Line             : String (1 .. 1_000);
         Last             : Natural;

      begin
         if Config_File_Path /= No_Path then
            begin
               Open (Config_File, In_File, Get_Name_String (Config_File_Path));

            exception
               when others =>
                  Fail_Program
                    (Project_Tree,
                     "unable to open config file " &
                     Get_Name_String (Config_File_Path));
            end;

            Check_Temp_File;

            while not End_Of_File (Config_File) loop
               Get_Line (Config_File, Line, Last);
               Put_Line (File, Line (1 .. Last));
            end loop;

            Close (Config_File);
         end if;
      end Copy_Config_File;

      --------------
      -- Put_Line --
      --------------

      procedure Put_Line (File : File_Descriptor; S : String) is
         S0   : String (1 .. S'Length + 1);
         Last : Natural;

      begin
         --  Add an ASCII.LF to the string. As this config file is supposed to
         --  be used only by the compiler, we don't care about the characters
         --  for the end of line. In fact we could have put a space, but
         --  it is more convenient to be able to read gnat.adc during
         --  development, for which the ASCII.LF is fine.

         S0 (1 .. S'Length) := S;
         S0 (S0'Last) := ASCII.LF;
         Last := Write (File, S0'Address, S0'Length);

         if Last /= S'Length + 1 then
            Fail_Program (Project_Tree, "Disk full");
         end if;

         if Current_Verbosity = High then
            Write_Line (S);
         end if;
      end Put_Line;

      procedure Check_All_Projects is
        new For_Every_Project_Imported (Boolean, Check);
      Dummy : Boolean := False;

      --  Start of processing for Create_Config_File

   begin
      --  Nothing to do if config has already been checked

      if For_Project.Config_Checked then
         return;
      end if;

      if Config.Config_File_Unique then
         --  Copy an eventual global config file

         Copy_Config_File
           (Main_Project, Name_Builder, Name_Global_Config_File, Language);

         --  Copy an eventual local config file

         Copy_Config_File
           (For_Project, Name_Compiler, Name_Local_Config_File, Language);
      end if;

      For_Project.Config_Checked := True;

      Naming_Datas.Init;

      Check_All_Projects (For_Project, Project_Tree, Dummy);

      --  Visit all the units and issue the config declarations for those that
      --  need one.

      Iter := For_Each_Source (Project_Tree);

      loop
         Source := Prj.Element (Iter);
         exit when Source = No_Source;

         if Source.Language.Name = Language
           and then Source.Naming_Exception /= No
           and then Source.Unit /= No_Unit_Index
           and then not Source.Locally_Removed
           and then Source.Replaced_By = No_Source
         then
            Name_Len := 0;

            if Source.Kind = Spec then
               if Source.Index = 0 and then Config.Config_Spec /= No_Name then
                  Get_Name_String (Config.Config_Spec);

               elsif
                 Source.Index /= 0 and then Config.Config_Spec_Index /= No_Name
               then
                  Get_Name_String (Config.Config_Spec_Index);
               end if;

            else
               if Source.Index = 0 and then Config.Config_Body /= No_Name then
                  Get_Name_String (Config.Config_Body);

               elsif
                 Source.Index /= 0 and then Config.Config_Body_Index /= No_Name
               then
                  Get_Name_String (Config.Config_Body_Index);
               end if;
            end if;

            if Name_Len /= 0 then
               declare
                  Cur       : Positive := 1;
                  Unit      : constant String :=
                    Get_Name_String (Source.Unit.Name);
                  File_Name : constant String :=
                    Get_Name_String (Source.Display_File);

               begin
                  while Cur < Name_Len loop
                     if Name_Buffer (Cur) /= '%' then
                        Cur := Cur + 1;

                     else
                        case Name_Buffer (Cur + 1) is
                           when 'u' =>
                              Name_Buffer
                                (Cur + Unit'Length ..
                                   Name_Len - 2 + Unit'Length) :=
                                  Name_Buffer (Cur + 2 .. Name_Len);
                              Name_Buffer (Cur .. Cur + Unit'Length - 1) :=
                                Unit;
                              Cur := Cur + Unit'Length;
                              Name_Len := Name_Len - 2 + Unit'Length;

                           when 'f' =>
                              Name_Buffer
                                (Cur + File_Name'Length ..
                                   Name_Len - 2 + File_Name'Length) :=
                                  Name_Buffer (Cur + 2 .. Name_Len);
                              Name_Buffer
                                (Cur .. Cur + File_Name'Length - 1) :=
                                File_Name;
                              Cur := Cur + File_Name'Length;
                              Name_Len := Name_Len - 2 + File_Name'Length;

                           when 'i' =>
                              declare
                                 Index_String : constant String :=
                                   Source.Index'Img;

                              begin
                                 Name_Buffer
                                   (Cur + Index_String'Length ..
                                      Name_Len - 2 + Index_String'Length) :=
                                     Name_Buffer (Cur + 2 .. Name_Len);
                                 Name_Buffer
                                   (Cur .. Cur + Index_String'Length - 1) :=
                                   Index_String;
                                 Cur := Cur + Index_String'Length;
                                 Name_Len :=
                                   Name_Len - 2 + Index_String'Length;
                              end;

                           when '%' =>
                              Name_Buffer (Cur .. Name_Len - 1) :=
                                Name_Buffer (Cur + 1 .. Name_Len);
                              Cur := Cur + 1;
                              Name_Len := Name_Len - 1;

                           when others =>
                              Cur := Cur + 1;
                        end case;
                     end if;
                  end loop;

                  Put_Line (File, Name_Buffer (1 .. Name_Len));
               end;
            end if;
         end if;

         Next (Iter);
      end loop;

      if File /= Invalid_FD then
         Close (File);
         For_Project.Config_File_Name := File_Name;
      end if;

   end Create_Config_File;

   ---------------------------
   -- Create_Path_From_Dirs --
   ---------------------------

   function Create_Path_From_Dirs return String_Access is
      Result    : String_Access;
      Tmp       : String_Access;
      Path_Last : Natural := 0;
   begin
      for Index in 1 .. Directories.Last loop
         Get_Name_String (Directories.Table (Index));

         while Name_Len > 1
           and then (Name_Buffer (Name_Len) = Directory_Separator
                     or else Name_Buffer (Name_Len) = '/')
         loop
            Name_Len := Name_Len - 1;
         end loop;

         if Result = null then
            Result := new String (1 .. Name_Len);
         else
            while Path_Last + Name_Len + 1 > Result'Last loop
               Tmp := new String (1 .. 2 * Result'Length);
               Tmp (1 .. Path_Last) := Result (1 .. Path_Last);
               Free (Result);
               Result := Tmp;
            end loop;

            Path_Last := Path_Last + 1;
            Result (Path_Last) := Path_Separator;
         end if;

         Result (Path_Last + 1 .. Path_Last + Name_Len) :=
           Name_Buffer (1 .. Name_Len);
         Path_Last := Path_Last + Name_Len;
      end loop;

      if Current_Verbosity = High and then Result /= null then
         Put_Line ("Path=" & Result (1 .. Path_Last));
      end if;

      Tmp := new String'(Result (1 .. Path_Last));
      Free (Result);
      return Tmp;
   end Create_Path_From_Dirs;

   ----------------------
   -- Directly_Imports --
   ----------------------

   function Directly_Imports
     (Project  : Project_Id;
      Imported : Project_Id) return Boolean
   is
      L : Project_List := Project.Imported_Projects;
      P : Project_Id;

   begin
      while L /= null loop
         P := L.Project;
         while P /= No_Project loop
            if Imported = P then
               return True;
            end if;

            P := P.Extends;
         end loop;

         L := L.Next;
      end loop;

      return False;
   end Directly_Imports;

   ---------------------
   -- Display_Command --
   ---------------------

   procedure Display_Command
     (Name    : String;
      Path    : String_Access;
      Ellipse : Boolean := False)
   is
      Display_Ellipse : Boolean := Ellipse;
   begin
      --  Only display the command in Verbose Mode (-v) or when
      --  not in Quiet Output (no -q).

      if not Opt.Quiet_Output then

         --  In Verbose Mode output the full path of the spawned process

         if Opt.Verbose_Mode then
            Write_Str (Path.all);

         elsif Executable_Suffix'Length > 0 and then
           Name'Length > Executable_Suffix'Length
         then
            Name_Len := Name'Length;
            Name_Buffer (1 .. Name_Len) := Name;

            if Name_Buffer
              (Name_Len - Executable_Suffix'Length + 1 .. Name_Len) =
              Executable_Suffix.all
            then
               Name_Len := Name_Len - Executable_Suffix'Length;
            end if;

            Put (Base_Name (Name_Buffer (1 .. Name_Len)));

         else
            Write_Str (Base_Name (Name));
         end if;

         --  Display only the arguments for which the display flag is set
         --  (in Verbose Mode, the display flag is set for all arguments)

         for Arg in 1 .. Last_Argument loop
            if Arguments_Displayed (Arg) then
               Write_Char (' ');

               if Arguments_Simple_Name (Arg) then
                  Write_Str (Base_Name (Arguments (Arg).all));

               else
                  Write_Str (Arguments (Arg).all);
               end if;

            elsif Display_Ellipse then
               Write_Str (" ...");
               Display_Ellipse := False;
            end if;
         end loop;

         Write_Eol;
      end if;
   end Display_Command;

   ------------------------
   -- Get_Linker_Options --
   ------------------------

   procedure Get_Linker_Options (For_Project : Project_Id) is
      Linker_Lib_Dir_Option  : String_Access;
      Linker_Lib_Name_Option : String_Access;

      procedure Recursive_Add
        (Proj  : Project_Id;
         Tree  : Project_Tree_Ref;
         Dummy : in out Boolean);
      --  The recursive routine used to add linker options

      -------------------
      -- Recursive_Add --
      -------------------

      procedure Recursive_Add
        (Proj  : Project_Id;
         Tree  : Project_Tree_Ref;
         Dummy : in out Boolean)
      is
         pragma Unreferenced (Dummy);
         Linker_Package : Package_Id;
         Options        : Variable_Value;

      begin
         if Proj /= For_Project then
            Linker_Package :=
              Prj.Util.Value_Of
                (Name        => Name_Linker,
                 In_Packages => Proj.Decl.Packages,
                 Shared      => Tree.Shared);
            Options :=
              Prj.Util.Value_Of
                (Name                    => Name_Ada,
                 Index                   => 0,
                 Attribute_Or_Array_Name => Name_Linker_Options,
                 In_Package              => Linker_Package,
                 Shared                  => Tree.Shared);

            --  If attribute is present, add the project with
            --  the attribute to table Linker_Opts.

            if Options /= Nil_Variable_Value then
               Linker_Opts.Increment_Last;
               Linker_Opts.Table (Linker_Opts.Last) :=
                 (Project => Proj, Options => Options.Values);
            end if;
         end if;
      end Recursive_Add;

      procedure For_All_Projects is
        new For_Every_Project_Imported (Boolean, Recursive_Add);
      Dummy                  : Boolean := False;

      --  Start of processing for Get_Linker_Options

   begin
      if For_Project.Config.Linker_Lib_Dir_Option = No_Name then
         Linker_Lib_Dir_Option := new String'("-L");

      else
         Linker_Lib_Dir_Option :=
           new String'
             (Get_Name_String (For_Project.Config.Linker_Lib_Dir_Option));
      end if;

      if For_Project.Config.Linker_Lib_Name_Option = No_Name then
         Linker_Lib_Name_Option := new String'("-l");

      else
         Linker_Lib_Name_Option :=
           new String'
             (Get_Name_String (For_Project.Config.Linker_Lib_Name_Option));
      end if;

      Linker_Opts.Init;

      For_All_Projects
        (For_Project, Project_Tree, Dummy, Imported_First => True);

      for Index in reverse 1 .. Linker_Opts.Last loop
         declare
            Options  : String_List_Id := Linker_Opts.Table (Index).Options;
            Proj     : constant Project_Id :=
              Linker_Opts.Table (Index).Project;
            Option   : Name_Id;
            Dir_Path : constant String :=
              Get_Name_String (Proj.Directory.Display_Name);

         begin
            while Options /= Nil_String loop
               Option :=
                 Project_Tree.Shared.String_Elements.Table (Options).Value;
               Get_Name_String (Option);

               --  Do not consider empty linker options

               if Name_Len /= 0 then
                  --  Object files and -L switches specified with relative
                  --  paths must be converted to absolute paths.

                  if Name_Len > Linker_Lib_Dir_Option'Length and then
                    Name_Buffer (1 .. Linker_Lib_Dir_Option'Length) =
                    Linker_Lib_Dir_Option.all
                  then
                     if Is_Absolute_Path
                       (Name_Buffer
                          (Linker_Lib_Dir_Option'Length + 1 .. Name_Len))
                     then
                        Add_Argument (Name_Buffer (1 .. Name_Len), True);

                     else
                        Add_Argument
                          (Linker_Lib_Dir_Option.all &
                           Dir_Path &
                           Directory_Separator &
                           Name_Buffer
                             (Linker_Lib_Dir_Option'Length + 1 .. Name_Len),
                           True);
                     end if;

                  elsif (Name_Len > Linker_Lib_Name_Option'Length and then
                           Name_Buffer (1 .. Linker_Lib_Name_Option'Length) =
                           Linker_Lib_Name_Option.all)
                    or else
                      Name_Buffer (1) = '-'
                    or else
                      Is_Absolute_Path (Name_Buffer (1 .. Name_Len))
                  then
                     Add_Argument (Name_Buffer (1 .. Name_Len), True);

                  else
                     Add_Argument
                       (Dir_Path &
                        Directory_Separator &
                        Name_Buffer (1 .. Name_Len),
                        True,
                        Simple_Name => True);
                  end if;
               end if;

               Options :=
                 Project_Tree.Shared.String_Elements.Table (Options).Next;
            end loop;
         end;
      end loop;
   end Get_Linker_Options;

   ----------------
   -- Get_Option --
   ----------------

   function Get_Option (Option : Name_Id) return String_Access is
      Option_Name : constant String := Get_Name_String (Option);
   begin
      --  Look in All_Options if this option is already cached

      for Index in 1 .. All_Options.Last loop
         if All_Options.Options (Index).all = Option_Name then
            return All_Options.Options (Index);
         end if;
      end loop;

      --  Add the option to the All_Options cache, so that it will be found
      --  next time.

      Add_Option_Internal
        (new String'(Option_Name),
         To      => All_Options,
         Display => False);

      return All_Options.Options (All_Options.Last);
   end Get_Option;

   -------------------------
   -- Global_Archive_Name --
   -------------------------

   function Global_Archive_Name (For_Project : Project_Id) return String is
   begin
      return
        "lib" &
        Get_Name_String (For_Project.Name) &
        Archive_Suffix (For_Project);
   end Global_Archive_Name;

   -------------------------
   -- Add_Global_Switches --
   -------------------------

   function Add_Global_Switches
     (Switch                          : String;
      For_Lang                        : Name_Id;
      For_Builder                     : Boolean;
      Has_Global_Compilation_Switches : Boolean) return Boolean
   is
      Success : Boolean;
   begin
      if For_Builder then
         if Has_Global_Compilation_Switches then
            Builder_Switches_Lang := No_Name;
         else
            Builder_Switches_Lang := For_Lang;
         end if;

         Scan_Arg
           (Switch,
            Command_Line => False,
            Language     => For_Lang,
            Success      => Success);
         return Success;

      else
         Current_Processor := Compiler;

         Current_Builder_Comp_Option_Table :=
           Builder_Compiling_Options_HTable.Get (For_Lang);

         if Current_Builder_Comp_Option_Table =
           No_Builder_Comp_Option_Table
         then
            Current_Builder_Comp_Option_Table :=
              new Builder_Compiling_Options.Instance;
            Builder_Compiling_Options_HTable.Set
              (For_Lang, Current_Builder_Comp_Option_Table);
            Builder_Compiling_Options.Init
              (Current_Builder_Comp_Option_Table.all);
         end if;

         Add_Option (Switch, Command_Line => False);

         Current_Processor := None;
         return True;
      end if;
   end Add_Global_Switches;

   --------------
   -- Gprbuild --
   --------------

   procedure Gprbuild is
      procedure Do_Compute_Builder_Switches
        is new Compute_Builder_Switches (Add_Global_Switches);

      User_Project_Node : Project_Node_Id;
   begin
      --  First initialize and read the command line arguments

      Buildgpr.Initialize;

      --  And install Ctrl-C handler

      Install_Int_Handler (Sigint_Intercepted'Access);

      --  Check command line arguments. These will be overridden when looking
      --  for the configuration file

      if Target_Name = null then
         Target_Name := new String'("");
      end if;

      if Config_Project_File_Name = null then
         Config_Project_File_Name := new String'("");
      end if;

      --  Then, parse the user's project and the configuration file. Apply the
      --  configuration file to the project so that its settings are
      --  automatically inherited by the project.
      --  If either the project or the configuration file contains errors, the
      --  following call with call Osint.Fail and never return

      begin
         Parse_Project_And_Apply_Config
           (Main_Project               => Main_Project,
            User_Project_Node          => User_Project_Node,
            Config_File_Name           => Config_Project_File_Name.all,
            Autoconf_Specified         => Autoconf_Specified,
            Project_File_Name          => Project_File_Name.all,
            Project_Tree               => Project_Tree,
            Env                        => Root_Environment,
            Project_Node_Tree          => Project_Node_Tree,
            Packages_To_Check          => Packages_To_Check,
            Allow_Automatic_Generation => Autoconfiguration,
            Automatically_Generated    => Delete_Autoconf_File,
            Config_File_Path           => Configuration_Project_Path,
            Target_Name                => Target_Name.all,
            Normalized_Hostname        => Normalized_Hostname);
      exception
         when E : Prj.Conf.Invalid_Config =>
            Osint.Fail (Exception_Message (E));
      end;

      if Main_Project = No_Project then
         --  Don't flush messages in case of parsing error. This has already
         --  been taken care when parsing the tree. Otherwise, it results in
         --  the same message being displayed twice.

         Fail_Program
           (Project_Tree,
            """" & Project_File_Name.all & """ processing failed",
            Flush_Messages => User_Project_Node /= Empty_Node);
      end if;

      if Configuration_Project_Path /= null then
         Free (Config_Project_File_Name);
         Config_Project_File_Name := new String'
           (Base_Name (Configuration_Project_Path.all));
      end if;

      if Total_Errors_Detected > 0 then
         Prj.Err.Finalize;
         Fail_Program
           (Project_Tree,
            "problems while getting the configuration",
            Flush_Messages => False);
      end if;

      Main_Project_Dir :=
        new String'(Get_Name_String (Main_Project.Directory.Display_Name));

      if Err_Vars.Warnings_Detected > 0 then
         Prj.Err.Finalize;
         Prj.Err.Initialize;
      end if;

      Compute_All_Imported_Projects (Main_Project, Project_Tree);

      Queue.Initialize (Opt.One_Compilation_Per_Obj_Dir);

      if Mains.Number_Of_Mains (Project_Tree) = 0
        and then not Unique_Compile
      then
         --  Register the Main units from the projects.
         --  No need to waste time when we are going to compile all files
         --  anyway (Unique_Compile).
         Mains.Fill_From_Project (Main_Project, Project_Tree);
      end if;

      Mains.Complete_Mains
        (Root_Environment.Flags, Main_Project, Project_Tree);

      if not Unique_Compile
        and then Output_File_Name /= null
        and then Mains.Number_Of_Mains (null) > 1
      then
         Fail_Program
           (Project_Tree, "cannot specify -o when there are several mains");
      end if;

      Compute_Compilation_Phases
        (Project_Tree,
         Main_Project,
         Option_Unique_Compile => Unique_Compile,
         Option_Compile_Only   => Opt.Compile_Only,
         Option_Bind_Only      => Opt.Bind_Only,
         Option_Link_Only      => Opt.Link_Only);

      if Mains.Number_Of_Mains (Project_Tree) > 0
        and then Main_Project.Library
        and then Builder_Data (Project_Tree).Need_Binding
      then
         Fail_Program
           (Project_Tree,
            "cannot specify a main program " &
            "on the command line for a library project file");
      end if;

      Add_Mains_To_Queue;

      --  If no sources to compile, then there is nothing to do

      if Queue.Size = 0 then
         if not Opt.Quiet_Output
           and then not Main_Project.Externally_Built
         then
            Osint.Write_Program_Name;
            Write_Line (": no sources to compile");
         end if;

         Finish_Program (Project_Tree, E_Success);
      end if;

      Do_Compute_Builder_Switches
        (Project_Tree     => Project_Tree,
         Root_Environment => Root_Environment,
         Main_Project     => Main_Project);

      Always_Compile :=
        Always_Compile
        and then Opt.Force_Compilations
        and then Unique_Compile
        and then not Unique_Compile_All_Projects;

      --  Reprocess recorded command line options that have priority over
      --  those in the main project file.

      Options.Process_Command_Line_Options;

      if Debug.Debug_Flag_M then
         Write_Line ("Maximum number of simultaneous compilations =" &
                     Opt.Maximum_Processes'Img);
      end if;

      --  Warn if --create-map-file is not supported

      if Map_File /= null and then
        Main_Project.Config.Map_File_Option = No_Name
      then
         Write_Str ("warning: option ");
         Write_Str (Create_Map_File_Switch);
         Write_Str (" is not supported in this configuration");
         Write_Eol;
      end if;

      --  Source file lookups should be cached for efficiency.
      --  Source files are not supposed to change.

      Osint.Source_File_Data (Cache => True);

      --  If switch --no-object-check is used, then there is no check for the
      --  switches.

      if not Object_Checked then
         Opt.Check_Switches := False;
      end if;

      Compilation_Phase;
      Post_Compilation_Phase;
      Linking_Phase;

      if Warnings_Detected /= 0 then
         Prj.Err.Finalize;
      end if;

      Namet.Finalize;

      Finish_Program (Project_Tree, E_Success);
   end Gprbuild;

   ----------
   -- Hash --
   ----------

   function Hash (Pid : Process_Id) return Header_Num is
      Modulo : constant Integer := Integer (Header_Num'Last) + 1;
   begin
      return Header_Num (Pid_To_Integer (Pid) mod Modulo);
   end Hash;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      --  Do some necessary package initializations

      Csets.Initialize;
      Namet.Initialize;
      Snames.Initialize;

      Prj.Tree.Initialize (Root_Environment, Gprbuild_Flags);
      Prj.Tree.Initialize (Project_Node_Tree);

      Prj.Initialize (Project_Tree);
      Mains.Delete;

      --  Get the name id for "-L";

      Name_Len := 0;
      Add_Str_To_Name_Buffer ("-L");
      Dash_L := Name_Find;

      --  Get the command line arguments, starting with --version and --help

      Check_Version_And_Help
        ("GPRBUILD",
         "2004",
         Version_String => Gpr_Version_String);

      --  Now process the other options

      Autoconfiguration := True;

      declare
         Do_Not_Care : Boolean;

      begin
         Scan_Args : for Next_Arg in 1 .. Argument_Count loop
            Scan_Arg
              (Argument (Next_Arg),
               Command_Line => True,
               Language     => No_Name,
               Success      => Do_Not_Care);
         end loop Scan_Args;
      end;

      Mains.Set_Multi_Unit_Index (Project_Tree, Main_Index);

      Current_Processor := None;

      --  Target_Name has potentially been set when calling Scan_Arg, so we can
      --  only initialize the project path after parsing the command line
      --  arguments.

      if Target_Name = null then
         Prj.Env.Initialize_Default_Project_Path
           (Root_Environment.Project_Path, Target_Name => "");
      else
         Prj.Env.Initialize_Default_Project_Path
           (Root_Environment.Project_Path, Target_Name.all);
      end if;

      --  If --display-paths was specified, display the config and the user
      --  project paths and exit.

      if Display_Paths then
         Write_Char ('.');

         declare
            Prefix_Path : constant String := Executable_Prefix_Path;
            P           : String_Access;

         begin
            if Prefix_Path'Length /= 0 then
               Write_Char (Path_Separator);
               Write_Str (Prefix_Path);
               Write_Str ("share");
               Write_Char (Directory_Separator);
               Write_Str ("gpr");
            end if;

            Write_Eol;

            Prj.Env.Get_Path (Root_Environment.Project_Path, Path => P);
            Write_Line (P.all);
            Exit_Program (E_Success);
         end;
      end if;

      if Opt.Verbose_Mode then
         Copyright;
      end if;

      if Usage_Needed then
         Usage;
         Usage_Needed := False;
      end if;

      --  Fail if command line ended with "-P"

      if Project_File_Name_Expected then
         Fail_Program
           (Project_Tree, "project file name missing after -P");

         --  Or if it ended with "-o"

      elsif Output_File_Name_Expected then
         Fail_Program
           (Project_Tree, "output file name missing after -o");

         --  Or if it ended with "-aP"

      elsif Search_Project_Dir_Expected then
         Fail_Program
           (Project_Tree, "directory name missing after -aP");

      elsif Db_Directory_Expected then
         Fail_Program
           (Project_Tree, "directory name missing after --db");
      end if;

      if Load_Standard_Base then
         --  We need to parse the knowledge base so that we are able to
         --  normalize the target names. Unfortunately, if we have to spawn
         --  gprconfig, it will also have to parse that knowledge base on
         --  its own.
         Parse_Knowledge_Base (Project_Tree);
      end if;

      --  If no project file was specified, look first for a default

      if Project_File_Name = null then
         Look_For_Default_Project;
      end if;

      if Project_File_Name = null then
         Copyright;
         Usage;
         Fail_Program
           (Project_Tree,
            "no project file specified and no default project file");
      end if;
   end Initialize;

   -----------------------------------
   -- Is_Included_In_Global_Archive --
   -----------------------------------

   function Is_Included_In_Global_Archive
     (Object_Name : File_Name_Type;
      Project     : Project_Id) return Boolean
   is
      Proj   : Project_Id;
      Source : Source_Id;
      Iter   : Source_Iterator;

   begin
      --  If a source is overriden in an extending project, then the object
      --  file is not included in the global archive.

      Proj := Project.Extended_By;
      while Proj /= No_Project loop
         Iter := For_Each_Source (Project_Tree, Proj);
         loop
            Source := Prj.Element (Iter);
            exit when Source = No_Source;

            if Object_To_Global_Archive (Source)
              and then Source.Object = Object_Name
            then
               return False;
            end if;

            Next (Iter);
         end loop;
         Proj := Proj.Extended_By;
      end loop;

      Iter := For_Each_Source (Project_Tree, Project);

      loop
         Source := Prj.Element (Iter);
         exit when Source = No_Source;

         if Object_To_Global_Archive (Source)
           and then Source.Object =  Object_Name
         then
            return Source.Language.Config.Objects_Linked;
         end if;

         Next (Iter);
      end loop;

      return True;
   end Is_Included_In_Global_Archive;

   ---------------------------
   -- Is_In_Library_Project --
   ---------------------------

   function Is_In_Library_Project (Object_Path : String) return Boolean is
      Path_Id : constant Path_Name_Type := Create_Name (Object_Path);
      Src     : Source_Id;
      Iter    : Source_Iterator;
   begin
      Iter := For_Each_Source (Project_Tree);
      loop
         Src := Prj.Element (Iter);
         exit when Src = No_Source;

         if Src.Object_Path = Path_Id then
            return Src.Project.Library;
         end if;

         Next (Iter);
      end loop;

      return False;
   end Is_In_Library_Project;

   -------------------
   -- Linking_Phase --
   -------------------

   procedure Linking_Phase is

      procedure Do_Link (Project : Project_Id; Tree : Project_Tree_Ref);

      -------------
      -- Do_Link --
      -------------

      procedure Do_Link (Project : Project_Id; Tree : Project_Tree_Ref) is
         pragma Unreferenced (Project);
         Main_File : Main_Info;
      begin
         if Builder_Data (Tree).Need_Linking then
            Mains.Reset;
            loop
               Main_File := Mains.Next_Main;
               exit when Main_File = No_Main_Info;

               if Main_File.Tree = Tree
                 and then not Project_Compilation_Failed (Main_File.Project)
               then
                  Link_Main (Main_File);
               end if;
            end loop;

            if Total_Errors_Detected > 0 then
               Fail_Program (Tree, "*** link failed");
            end if;
         end if;
      end Do_Link;

      procedure Link_All is new For_Project_And_Aggregated (Do_Link);

   begin
      Link_All (Main_Project, Project_Tree);
   end Linking_Phase;

   ---------------
   -- Link_Main --
   ---------------

   procedure Link_Main (Main_File  : Main_Info) is
      Linker_Name        : String_Access := null;
      Linker_Path        : String_Access;
      Min_Linker_Opts    : Name_List_Index;
      Exchange_File      : Ada.Text_IO.File_Type;
      Line               : String (1 .. 1_000);
      Last               : Natural;

      Success            : Boolean := False;

      Section            : Binding_Section := No_Binding_Section;

      Linker_Needs_To_Be_Called : Boolean;

      Executable_TS      : Time_Stamp_Type;
      Main_Object_TS     : Time_Stamp_Type;
      Binder_Exchange_TS : Time_Stamp_Type;
      Binder_Object_TS   : Time_Stamp_Type := Dummy_Time_Stamp;
      Global_Archive_TS  : Time_Stamp_Type;

      Global_Archive_Has_Been_Built : Boolean;
      Global_Archive_Exists         : Boolean;

      Disregard          : Boolean;

      B_Data : Binding_Data;

      --  Main already has the right canonical casing
      Main         : constant String := Get_Name_String (Main_File.File);
      Main_Source  : constant Source_Id := Main_File.Source;

      Main_Id        : File_Name_Type;

      Exec_Name      : File_Name_Type;
      Exec_Path_Name : Path_Name_Type;

      Main_Proj      : Project_Id;

      Main_Base_Name_Index : File_Name_Type;

      First_Object_Index : Natural := 0;
      Last_Object_Index  : Natural := 0;

      Index_Separator : Character;

      Response_File_Name : Path_Name_Type := No_Path;
      Response_2         : Path_Name_Type := No_Path;

   begin
      --  Make sure that the table Rpaths is emptied after each main, so
      --  that the same rpaths are not duplicated.

      Rpaths.Set_Last (0);

      Linker_Needs_To_Be_Called := Opt.Force_Compilations;

      Main_Id := Create_Name (Base_Name (Main));
      Main_Proj := Ultimate_Extending_Project_Of (Main_Source.Project);

      Change_To_Object_Directory (Main_Proj);

      --  Build the global archive for this project, if needed

      Build_Global_Archive
        (Main_Proj,
         Main_File.Tree,
         Global_Archive_Has_Been_Built,
         Global_Archive_Exists);

      --  Get the main base name

      Index_Separator :=
        Main_Source.Language.Config.Multi_Unit_Object_Separator;

      Main_Base_Name_Index :=
        Base_Name_Index_For (Main, Main_File.Index, Index_Separator);

      if (not Linker_Needs_To_Be_Called) and then Opt.Verbose_Mode then
         Write_Str ("   Checking executable for ");
         Write_Str (Get_Name_String (Main_Source.File));
         Write_Line (" ...");
      end if;

      if Output_File_Name /= null then
         Name_Len := 0;
         Add_Str_To_Name_Buffer (Output_File_Name.all);
         Exec_Name := Name_Find;

      else
         Exec_Name := Executable_Of
           (Project  => Main_Proj,
            Shared   => Main_File.Tree.Shared,
            Main     => Main_Id,
            Index    => Main_Source.Index,
            Ada_Main => False,
            Language => Get_Name_String (Main_Source.Language.Name));
      end if;

      if Main_Proj.Exec_Directory = Main_Proj.Object_Directory then
         Exec_Path_Name := Path_Name_Type (Exec_Name);

      else
         Get_Name_String (Main_Proj.Exec_Directory.Display_Name);
         Name_Len := Name_Len + 1;
         Name_Buffer (Name_Len) := Directory_Separator;
         Add_Str_To_Name_Buffer (Get_Name_String (Exec_Name));
         Exec_Path_Name := Name_Find;
      end if;

      Executable_TS := File_Stamp (Exec_Path_Name);

      if (not Linker_Needs_To_Be_Called) and then
        Executable_TS = Empty_Time_Stamp
      then
         Linker_Needs_To_Be_Called := True;

         if Opt.Verbose_Mode then
            Write_Line ("      -> executable does not exist");
         end if;
      end if;

      --  Get the path of the linker driver

      if Main_Proj.Config.Linker /= No_Path then
         Linker_Name :=
           new String'(Get_Name_String (Main_Proj.Config.Linker));

         Linker_Path := Locate_Exec_On_Path (Linker_Name.all);

         if Linker_Path = null then
            Fail_Program
              (Main_File.Tree,
               "unable to find linker " & Linker_Name.all);
         end if;

      else
         Fail_Program
           (Main_File.Tree,
            "no linker specified and " &
              "no default linker in the configuration");
      end if;

      Last_Argument := 0;

      Initialize_Source_Record (Main_Source);

      Main_Object_TS :=
        File_Stamp (File_Name_Type (Main_Source.Object_Path));

      if not Linker_Needs_To_Be_Called then
         if Main_Object_TS = Empty_Time_Stamp then
            if Opt.Verbose_Mode then
               Write_Line ("      -> main object does not exist");
            end if;

            Linker_Needs_To_Be_Called := True;

         elsif String (Main_Object_TS) > String (Executable_TS) then
            if Opt.Verbose_Mode then
               Write_Line
                 ("      -> main object more recent than executable");
            end if;

            Linker_Needs_To_Be_Called := True;
         end if;
      end if;

      if Main_Object_TS = Empty_Time_Stamp then
         if Opt.Keep_Going then
            --  Do not stop there, the compilation error has already been
            --  reported.
            return;

         else
            Fail_Program
              (Main_File.Tree,
               "main object for " &
                 Get_Name_String (Main_Source.File) &
                 " does not exist");
         end if;
      end if;

      if Main_Proj = Main_Source.Object_Project then
         Add_Argument (Get_Name_String (Main_Source.Object), True);
      else
         Add_Argument (Get_Name_String (Main_Source.Object_Path), True);
      end if;

      --  Add the Leading_Switches if there are any in package Linker

      declare
         The_Packages   : constant Package_Id :=
                            Main_Proj.Decl.Packages;
         Linker_Package : constant Prj.Package_Id :=
                            Prj.Util.Value_Of
                              (Name        => Name_Linker,
                               In_Packages => The_Packages,
                               Shared      => Main_File.Tree.Shared);

         Switches    : Variable_Value;
         Switch_List : String_List_Id;
         Element     : String_Element;

      begin
         if Linker_Package /= No_Package then
            declare
               Switches_Array : constant Array_Element_Id :=
                 Prj.Util.Value_Of
                   (Name      => Name_Leading_Switches,
                    In_Arrays =>
                      Main_File.Tree.Shared.Packages.Table
                        (Linker_Package).Decl.Arrays,
                    Shared    => Main_File.Tree.Shared);
               Option   : String_Access;

            begin
               Switches :=
                 Prj.Util.Value_Of
                   (Index     => Name_Id (Main_Id),
                    Src_Index => 0,
                    In_Array  => Switches_Array,
                    Shared    => Main_File.Tree.Shared);

               if Switches = Nil_Variable_Value then
                  Switches :=
                    Prj.Util.Value_Of
                      (Index                  =>
                           Main_Source.Language.Name,
                       Src_Index              => 0,
                       In_Array               => Switches_Array,
                       Shared                 => Main_File.Tree.Shared,
                       Force_Lower_Case_Index => True);
               end if;

               if Switches = Nil_Variable_Value then
                  Switches :=
                    Prj.Util.Value_Of
                      (Index                  => All_Other_Names,
                       Src_Index              => 0,
                       In_Array               => Switches_Array,
                       Shared                 => Main_File.Tree.Shared,
                       Force_Lower_Case_Index => True);
               end if;

               case Switches.Kind is
                  when Undefined | Single =>
                     null;

                  when Prj.List =>
                     Switch_List := Switches.Values;

                     while Switch_List /= Nil_String loop
                        Element :=
                          Main_File.Tree.Shared.String_Elements.Table
                            (Switch_List);
                        Get_Name_String (Element.Value);

                        if Name_Len > 0 then
                           Option :=
                             new String'(Name_Buffer (1 .. Name_Len));
                           Add_Argument (Option.all, True);
                        end if;

                        Switch_List := Element.Next;
                     end loop;
               end case;
            end;
         end if;
      end;

      Find_Binding_Languages (Main_File.Tree, Main_File.Project);

      if Builder_Data (Main_File.Tree).There_Are_Binder_Drivers then
         First_Object_Index := Last_Argument + 1;
         Binding_Options.Init;

         B_Data := Builder_Data (Main_File.Tree).Binding;

         while B_Data /= null loop
            declare
               Exchange_File_Name : constant String :=
                                      Binder_Exchange_File_Name
                                        (Main_Base_Name_Index,
                                         B_Data.Binder_Prefix).all;

            begin
               if Is_Regular_File (Exchange_File_Name) then

                  Binder_Exchange_TS :=
                    File_Stamp
                      (Path_Name_Type'(Create_Name
                       (Exchange_File_Name)));

                  if (not Linker_Needs_To_Be_Called) and then
                    String (Binder_Exchange_TS) > String (Executable_TS)
                  then
                     Linker_Needs_To_Be_Called := True;

                     if Opt.Verbose_Mode then
                        Write_Str ("      -> binder exchange file """);
                        Write_Str (Exchange_File_Name);
                        Write_Line (""" is more recent than executable");
                     end if;
                  end if;

                  Open (Exchange_File, In_File, Exchange_File_Name);

                  while not End_Of_File (Exchange_File) loop
                     Get_Line (Exchange_File, Line, Last);

                     if Last > 0 then
                        if Line (1) = '[' then
                           Section :=
                             Get_Binding_Section (Line (1 .. Last));

                        else
                           case Section is
                              when Generated_Object_File =>

                                 Binder_Object_TS :=
                                   File_Stamp
                                     (Path_Name_Type'
                                          (Create_Name
                                               (Line (1 .. Last))));

                                 Add_Argument
                                   (Line (1 .. Last), Opt.Verbose_Mode);

                              when Bound_Object_Files =>
                                 if Normalize_Pathname
                                   (Line (1 .. Last),
                                    Case_Sensitive => False) /=
                                   Normalize_Pathname
                                     (Get_Name_String
                                          (Main_Source.Object_Path),
                                      Case_Sensitive => False)
                                   and then
                                     not Is_In_Library_Project
                                       (Line (1 .. Last))
                                 then
                                    Add_Argument
                                      (Line (1 .. Last), Opt.Verbose_Mode);
                                 end if;

                              when Resulting_Options =>
                                 if Line (1 .. Last) /= "-static" and then
                                   Line (1 .. Last) /= "-shared"
                                 then
                                    Binding_Options.Append
                                      (new String'(Line (1 .. Last)));
                                 end if;

                              when Gprexch.Run_Path_Option =>
                                 if Opt.Run_Path_Option and then
                                   Main_Proj.Config.Run_Path_Option /=
                                     No_Name_List
                                 then
                                    Add_Rpath (Line (1 .. Last));
                                    Add_Rpath
                                      (Shared_Libgcc_Dir
                                         (Line (1 .. Last)));
                                 end if;

                              when others =>
                                 null;
                           end case;
                        end if;
                     end if;
                  end loop;

                  Close (Exchange_File);

                  if Binder_Object_TS = Empty_Time_Stamp then
                     if (not Linker_Needs_To_Be_Called)
                       and then Opt.Verbose_Mode
                     then
                        Write_Line
                          ("      -> no binder generated object file");
                     end if;

                     Fail_Program
                       (Main_File.Tree,
                        "no binder generated object file");

                  elsif (not Linker_Needs_To_Be_Called)
                    and then
                      String (Binder_Object_TS) > String (Executable_TS)
                  then
                     Linker_Needs_To_Be_Called := True;

                     if Opt.Verbose_Mode then
                        Write_Line
                          ("      -> binder generated object is more " &
                             "recent than executable");
                     end if;
                  end if;

               else
                  Fail_Program
                    (Main_File.Tree,
                     "binder exchange file " &
                       Exchange_File_Name & " does not exist");
               end if;
            end;

            B_Data := B_Data.Next;
         end loop;

         Last_Object_Index := Last_Argument;
      end if;

      --  Add the global archive, if there is one

      if Global_Archive_Exists then
         Global_Archive_TS :=
           File_Stamp
             (Path_Name_Type'
                  (Create_Name (Global_Archive_Name (Main_Proj))));

         if Global_Archive_TS = Empty_Time_Stamp then
            if not Linker_Needs_To_Be_Called
              and then Opt.Verbose_Mode
            then
               Write_Line ("      -> global archive does not exist");
            end if;

            Fail_Program
              (Main_File.Tree,
               "global archive for project file " &
                 Get_Name_String (Main_Proj.Name) &
                 " does not exist");
         end if;
      end if;

      if (not Linker_Needs_To_Be_Called)
        and then Global_Archive_Has_Been_Built
      then
         Linker_Needs_To_Be_Called := True;

         if Opt.Verbose_Mode then
            Write_Line ("      -> global archive has just been built");
         end if;
      end if;

      if (not Linker_Needs_To_Be_Called)
        and then Global_Archive_Exists
        and then String (Global_Archive_TS) > String (Executable_TS)
      then
         Linker_Needs_To_Be_Called := True;

         if Opt.Verbose_Mode then
            Write_Line ("      -> global archive is more recent than " &
                          "executable");
         end if;
      end if;

      --  Check if there are library files that are more recent than
      --  executable.

      declare
         List : Project_List := Main_Proj.All_Imported_Projects;
         Proj : Project_Id;

         Current_Dir : constant String := Get_Current_Dir;
      begin
         while List /= null loop
            Proj := List.Project;
            List := List.Next;

            if Proj.Extended_By = No_Project
              and then Proj.Library
              and then Proj.Object_Directory /= No_Path_Information
              and then not Proj.Externally_Built
              and then (Proj.Library_Kind = Static or else
                          not Proj.Standalone_Library)
            then
               Change_Dir
                 (Get_Name_String (Proj.Object_Directory.Display_Name));
               Get_Name_String (Proj.Library_Name);
               Add_Str_To_Name_Buffer (Library_Exchange_Suffix);

               declare
                  Exchange_File : Ada.Text_IO.File_Type;
                  Path_Name     : constant String :=
                    Name_Buffer (1 .. Name_Len);
                  Lib_TS        : Time_Stamp_Type;

               begin
                  begin
                     Open (Exchange_File, In_File, Path_Name);

                  exception
                     when others =>
                        if Opt.Verbose_Mode then
                           Write_Str
                             ("      -> library exchange file """);
                           Write_Str (Path_Name);
                           Write_Line (""" does not exist");
                        end if;

                        Linker_Needs_To_Be_Called := True;
                        exit;
                  end;

                  if End_Of_File (Exchange_File) then
                     if Opt.Verbose_Mode then
                        Write_Str
                          ("      -> library exchange file """);
                        Write_Str (Path_Name);
                        Write_Line (""" is empty");
                     end if;

                     Linker_Needs_To_Be_Called := True;
                     Close (Exchange_File);
                     exit;
                  end if;

                  Get_Line (Exchange_File, Name_Buffer, Name_Len);

                  if Name_Buffer (1 .. Name_Len) /=
                    Library_Label (Library_Path)
                  then
                     Linker_Needs_To_Be_Called := True;
                     Close (Exchange_File);

                     if Opt.Verbose_Mode then
                        Write_Str
                          ("      -> library exchange file """);
                        Write_Str (Path_Name);
                        Write_Line (""" has wrong format");
                     end if;

                     exit;
                  end if;

                  Get_Line (Exchange_File, Name_Buffer, Name_Len);
                  Close (Exchange_File);

                  Lib_TS := File_Stamp (File_Name_Type'(Name_Find));

                  if Lib_TS = Empty_Time_Stamp then
                     Linker_Needs_To_Be_Called := True;

                     if Opt.Verbose_Mode then
                        Write_Str ("      -> library file """);
                        Write_Str (Name_Buffer (1 .. Name_Len));
                        Write_Line (""" not found");
                     end if;

                     exit;

                  elsif String (Lib_TS) > String (Executable_TS) then
                     Linker_Needs_To_Be_Called := True;

                     if Opt.Verbose_Mode then
                        Write_Str ("      -> library file """);
                        Write_Str (Name_Buffer (1 .. Name_Len));
                        Write_Line
                          (""" is more recent than executable");
                     end if;

                     exit;
                  end if;
               end;
            end if;
         end loop;

         Change_Dir (Current_Dir);
      end;

      if not Linker_Needs_To_Be_Called then
         if Opt.Verbose_Mode then
            Write_Line ("      -> up to date");

         elsif not Opt.Quiet_Output then
            Inform (Exec_Name, "up to date");
         end if;

      else
         if Global_Archive_Exists then
            Add_Argument
              (Global_Archive_Name (Main_Proj), Opt.Verbose_Mode);
         end if;

         --  Add the library switches, if there are libraries

         Process_Imported_Libraries
           (Main_Proj, There_Are_SALs => Disregard);

         Library_Dirs.Reset;

         for J in reverse 1 .. Library_Projs.Last loop
            if Library_Projs.Table (J).Library_Kind = Static then
               Add_Argument
                 (Get_Name_String
                    (Library_Projs.Table (J).Library_Dir.Display_Name) &
                    Directory_Separator &
                    "lib" &
                    Get_Name_String
                    (Library_Projs.Table (J).Library_Name) &
                    Archive_Suffix (Library_Projs.Table (J)),
                  Opt.Verbose_Mode);

            else
               --  Do not issue several time the same -L switch if
               --  several library projects share the same library
               --  directory.

               if not Library_Dirs.Get
                 (Library_Projs.Table (J).Library_Dir.Name)
               then
                  Library_Dirs.Set
                    (Library_Projs.Table (J).Library_Dir.Name, True);

                  if
                    Main_Proj.Config.Linker_Lib_Dir_Option = No_Name
                  then
                     Add_Argument
                       ("-L" &
                          Get_Name_String
                          (Library_Projs.Table
                             (J).Library_Dir.Display_Name),
                        Opt.Verbose_Mode);

                  else
                     Add_Argument
                       (Get_Name_String
                          (Main_Proj.Config.Linker_Lib_Dir_Option) &
                          Get_Name_String
                          (Library_Projs.Table
                             (J).Library_Dir.Display_Name),
                        Opt.Verbose_Mode);
                  end if;

                  if Opt.Run_Path_Option
                    and then
                      Main_Proj.Config.Run_Path_Option /= No_Name_List
                      and then
                        Library_Projs.Table (J).Library_Kind /= Static
                  then
                     Add_Rpath
                       (Get_Name_String
                          (Library_Projs.Table
                             (J).Library_Dir.Display_Name));
                  end if;
               end if;

               if Main_Proj.Config.Linker_Lib_Name_Option = No_Name then
                  Add_Argument
                    ("-l" &
                       Get_Name_String
                       (Library_Projs.Table (J).Library_Name),
                     Opt.Verbose_Mode);

               else
                  Add_Argument
                    (Get_Name_String
                       (Main_Proj.Config.Linker_Lib_Name_Option) &
                       Get_Name_String
                       (Library_Projs.Table (J).Library_Name),
                     Opt.Verbose_Mode);
               end if;
            end if;
         end loop;

         --  Put the options in the project file, if any

         declare
            The_Packages : constant Package_Id :=
                             Main_Proj.Decl.Packages;

            Linker_Package : constant Prj.Package_Id :=
                               Prj.Util.Value_Of
                                 (Name        => Name_Linker,
                                  In_Packages => The_Packages,
                                  Shared      => Main_File.Tree.Shared);

            Switches    : Variable_Value;
            Switch_List : String_List_Id;
            Element     : String_Element;

         begin
            if Linker_Package /= No_Package then
               declare
                  Defaults : constant Array_Element_Id :=
                    Prj.Util.Value_Of
                      (Name      => Name_Default_Switches,
                       In_Arrays =>
                         Main_File.Tree.Shared.Packages.Table
                           (Linker_Package).Decl.Arrays,
                       Shared    => Main_File.Tree.Shared);

                  Switches_Array : constant Array_Element_Id :=
                    Prj.Util.Value_Of
                      (Name      => Name_Switches,
                       In_Arrays =>
                         Main_File.Tree.Shared.Packages.Table
                           (Linker_Package).Decl.Arrays,
                       Shared => Main_File.Tree.Shared);
                  Option   : String_Access;

               begin
                  Switches :=
                    Prj.Util.Value_Of
                      (Index           => Name_Id (Main_Id),
                       Src_Index       => 0,
                       In_Array        => Switches_Array,
                       Shared          => Main_File.Tree.Shared,
                       Allow_Wildcards => True);

                  if Switches = Nil_Variable_Value then
                     Switches :=
                       Prj.Util.Value_Of
                         (Index                  =>
                              Main_Source.Language.Name,
                          Src_Index              => 0,
                          In_Array               => Switches_Array,
                          Shared                 => Main_File.Tree.Shared,
                          Force_Lower_Case_Index => True);
                  end if;

                  if Switches = Nil_Variable_Value then
                     Switches :=
                       Prj.Util.Value_Of
                         (Index                  => All_Other_Names,
                          Src_Index              => 0,
                          In_Array               => Switches_Array,
                          Shared                 => Main_File.Tree.Shared,
                          Force_Lower_Case_Index => True);
                  end if;

                  if Switches = Nil_Variable_Value then
                     Switches :=
                       Prj.Util.Value_Of
                         (Index     =>
                              Main_Source.Language.Name,
                          Src_Index => 0,
                          In_Array  => Defaults,
                          Shared    => Main_File.Tree.Shared);
                  end if;

                  case Switches.Kind is
                     when Undefined | Single =>
                        null;

                     when Prj.List =>
                        Switch_List := Switches.Values;

                        while Switch_List /= Nil_String loop
                           Element :=
                             Main_File.Tree.Shared.String_Elements.Table
                               (Switch_List);
                           Get_Name_String (Element.Value);

                           if Name_Len > 0 then
                              Option :=
                                new String'(Name_Buffer (1 .. Name_Len));

                              Test_If_Relative_Path
                                (Option,
                                 Main_Project_Dir.all,
                                 Dash_L);

                              Add_Argument (Option.all, True);
                           end if;

                           Switch_List := Element.Next;
                        end loop;
                  end case;
               end;
            end if;
         end;

         --  Get the Linker_Options, if any

         Get_Linker_Options (For_Project => Main_Proj);

         --  Add the linker switches specified on the command line

         for J in 1 .. Command_Line_Linker_Options.Last loop
            Add_Argument
              (Command_Line_Linker_Options.Table (J), Opt.Verbose_Mode);
         end loop;

         --  Then the binding options

         for J in 1 .. Binding_Options.Last loop
            Add_Argument (Binding_Options.Table (J), Opt.Verbose_Mode);
         end loop;

         --  Finally, the required switches, if any. These are put at the
         --  end because, if they include -L switches for example, the
         --  link may fail because the wrong objects or libraries are
         --  linked in.

         Min_Linker_Opts :=
           Main_Proj.Config.Trailing_Linker_Required_Switches;
         while Min_Linker_Opts /= No_Name_List loop
            Add_Argument
              (Get_Name_String
                 (Main_File.Tree.Shared.Name_Lists.Table
                    (Min_Linker_Opts).Name),
               Opt.Verbose_Mode);
            Min_Linker_Opts   := Main_File.Tree.Shared.Name_Lists.Table
              (Min_Linker_Opts).Next;
         end loop;

         --  Look for the last switch -shared-libgcc or -static-libgcc.
         --  If -shared-libgcc was the last switch, then put in the
         --  run path option the shared libgcc dir.

         if Opt.Run_Path_Option and then
           Main_Proj.Config.Run_Path_Option /= No_Name_List
         then
            declare
               Add_Shared_Libgcc_Dir : Boolean := False;
            begin
               for J in reverse 1 .. Last_Argument loop
                  if Arguments (J).all = "-shared-libgcc" then
                     Add_Shared_Libgcc_Dir := True;
                     exit;

                  elsif Arguments (J).all = "-static-libgcc" then
                     exit;
                  end if;
               end loop;

               if Add_Shared_Libgcc_Dir then
                  --  Look for the adalib directory in -L switches.
                  --  If it is found, then add the shared libgcc
                  --  directory to the run path option.

                  for J in 1 .. Last_Argument loop
                     declare
                        Option : String (1 .. Arguments (J)'Length);
                        Last   : Natural := Option'Last;

                     begin
                        Option := Arguments (J).all;

                        if Last > 2 and then Option (1 .. 2) = "-L" then
                           if Option (Last) = '/' or else
                             Option (Last) = Directory_Separator
                           then
                              Last := Last - 1;
                           end if;

                           if Last > 10 and then
                             Option (Last - 5 .. Last) = "adalib"
                           then
                              Add_Rpath
                                (Shared_Libgcc_Dir
                                   (Option (3 .. Last)));
                              exit;
                           end if;
                        end if;
                     end;
                  end loop;
               end if;
            end;
         end if;

         --  Add the run path option, if necessary

         if Opt.Run_Path_Option
           and then
             Main_Proj.Config.Run_Path_Option /= No_Name_List
             and then
               Rpaths.Last > 0
         then
            declare
               Nam_Nod  : Name_Node :=
                 Main_File.Tree.Shared.Name_Lists.Table
                   (Main_Proj.Config.Run_Path_Option);
               Length   : Natural := 0;
               Arg      : String_Access := null;
            begin
               if Main_Proj.Config.Run_Path_Origin /= No_Name then
                  Rpaths_Relative_To
                    (Main_Proj.Exec_Directory.Display_Name,
                     Main_Proj.Config.Run_Path_Origin);
               end if;

               if Main_Proj.Config.Separate_Run_Path_Options then
                  for J in 1 .. Rpaths.Last loop
                     Nam_Nod := Main_File.Tree.Shared.Name_Lists.Table
                       (Main_Proj.Config.Run_Path_Option);
                     while Nam_Nod.Next /= No_Name_List loop
                        Add_Argument
                          (Get_Name_String (Nam_Nod.Name), True);
                        Nam_Nod := Main_File.Tree.Shared.Name_Lists.Table
                          (Nam_Nod.Next);
                     end loop;

                     Get_Name_String (Nam_Nod.Name);
                     Add_Str_To_Name_Buffer (Rpaths.Table (J).all);
                     Add_Argument
                       (Name_Buffer (1 .. Name_Len), Opt.Verbose_Mode);
                  end loop;

               else
                  while Nam_Nod.Next /= No_Name_List loop
                     Add_Argument (Get_Name_String (Nam_Nod.Name), True);
                     Nam_Nod := Main_File.Tree.Shared.Name_Lists.Table
                       (Nam_Nod.Next);
                  end loop;

                  --  Compute the length of the argument

                  Get_Name_String (Nam_Nod.Name);
                  Length := Name_Len;

                  for J in 1 .. Rpaths.Last loop
                     Length := Length + Rpaths.Table (J)'Length + 1;
                  end loop;

                  Length := Length - 1;

                  --  Create the argument

                  Arg := new String (1 .. Length);
                  Length := Name_Len;
                  Arg (1 .. Name_Len) := Name_Buffer (1 .. Name_Len);

                  for J in 1 .. Rpaths.Last loop
                     if J /= 1 then
                        Length := Length + 1;
                        Arg (Length) := Path_Separator;
                     end if;

                     Arg (Length + 1 .. Length + Rpaths.Table (J)'Length)
                       := Rpaths.Table (J).all;
                     Length := Length + Rpaths.Table (J)'Length;
                  end loop;

                  Add_Argument (Arg, Opt.Verbose_Mode);
               end if;
            end;
         end if;

         --  Add the map file option, if supported and requested

         if Map_File /= null and then
           Main_Proj.Config.Map_File_Option /= No_Name
         then
            Get_Name_String (Main_Proj.Config.Map_File_Option);

            if Map_File'Length > 0 then
               Add_Str_To_Name_Buffer (Map_File.all);

            else
               Add_Str_To_Name_Buffer
                 (Get_Name_String (Main_Base_Name_Index));
               Add_Str_To_Name_Buffer (".map");
            end if;

            Add_Argument (Name_Buffer (1 .. Name_Len), Opt.Verbose_Mode);
         end if;

         --  Add the switch(es) to specify the name of the executable

         declare
            List : Name_List_Index :=
              Main_Proj.Config.Linker_Executable_Option;
            Nam  : Name_Node;

            procedure Add_Executable_Name;
            --  Add the name of the executable to to current name buffer,
            --  then the content of the name buffer as the next argument.

            -------------------------
            -- Add_Executable_Name --
            -------------------------

            procedure Add_Executable_Name is
            begin
               Add_Str_To_Name_Buffer (Get_Name_String (Exec_Path_Name));
               Add_Argument
                 (Name_Buffer (1 .. Name_Len),
                  True,
                  Simple_Name => not Opt.Verbose_Mode);
            end Add_Executable_Name;

         begin
            if List /= No_Name_List then
               loop
                  Nam := Main_File.Tree.Shared.Name_Lists.Table (List);
                  Get_Name_String (Nam.Name);

                  if Nam.Next = No_Name_List then
                     Add_Executable_Name;
                     exit;

                  else
                     Add_Argument (Name_Buffer (1 .. Name_Len), True);
                  end if;

                  List := Nam.Next;
               end loop;

            else
               Add_Argument ("-o", True);
               Name_Len := 0;
               Add_Executable_Name;
            end if;
         end;

         --  If response file are supported, check the length of the
         --  command line and the number of object files, then create
         --  a response file if needed.

         if Main_Proj.Config.Max_Command_Line_Length > 0 and then
           Main_Proj.Config.Resp_File_Format /= Prj.None and then
           First_Object_Index > 0
         then
            declare
               Arg_Length : Natural := 0;
               Min_Number_Of_Objects : Natural := 0;
            begin
               for J in 1 .. Last_Argument loop
                  Arg_Length := Arg_Length + Arguments (J)'Length + 1;
               end loop;

               if
                 Arg_Length > Main_Proj.Config.Max_Command_Line_Length
               then
                  if Main_Proj.Config.Resp_File_Options =
                    No_Name_List
                  then
                     Min_Number_Of_Objects := 0;
                  else
                     Min_Number_Of_Objects := 1;
                  end if;

                  --  Don't create a project file if there would not be
                  --  a smaller number of arguments.

                  if Last_Object_Index - First_Object_Index + 1 >
                    Min_Number_Of_Objects
                  then
                     declare
                        Resp_File_Options : String_List_Access :=
                          new String_List (1 .. 0);
                        List             : Name_List_Index :=
                          Main_Proj.Config.Resp_File_Options;
                        Nam_Nod          : Name_Node;

                     begin
                        while List /= No_Name_List loop
                           Nam_Nod :=
                             Main_File.Tree.Shared.Name_Lists.Table (List);
                           Resp_File_Options :=
                             new String_List'
                               (Resp_File_Options.all &
                                  new String'
                                  (Get_Name_String (Nam_Nod.Name)));
                           List := Nam_Nod.Next;
                        end loop;

                        Create_Response_File
                          (Format            =>
                             Main_Proj.Config.Resp_File_Format,
                           Objects           => Arguments
                             (First_Object_Index .. Last_Object_Index),
                           Other_Arguments   =>
                             Arguments (Last_Object_Index + 1 ..
                                 Last_Argument),
                           Resp_File_Options => Resp_File_Options.all,
                           Name_1            => Response_File_Name,
                           Name_2            => Response_2);

                        if Main_Proj.Config.Resp_File_Format = GCC
                          or else
                            Main_Proj.Config.Resp_File_Format = GCC_GNU
                            or else
                              Main_Proj.Config.Resp_File_Format =
                                GCC_Object_List
                                or else
                                  Main_Proj.Config.Resp_File_Format =
                                    GCC_Option_List
                        then
                           Arguments (First_Object_Index) :=
                             new String'("@" &
                                           Get_Name_String
                                           (Response_File_Name));
                           Last_Argument := First_Object_Index;

                        else
                           --  Replace the first object file arguments
                           --  with the argument(s) specifying the
                           --  response file. No need to update
                           --  Arguments_Displayed, as the values are
                           --  already correct (= Verbose_Mode).

                           if Resp_File_Options'Length = 0 then
                              Arguments (First_Object_Index) :=
                                new String'(Get_Name_String
                                            (Response_File_Name));
                              First_Object_Index :=
                                First_Object_Index + 1;

                           else
                              for J in Resp_File_Options'First ..
                                Resp_File_Options'Last - 1
                              loop
                                 Arguments (First_Object_Index) :=
                                   Resp_File_Options (J);
                                 First_Object_Index :=
                                   First_Object_Index + 1;
                              end loop;

                              Arguments (First_Object_Index) :=
                                new String'(Resp_File_Options
                                            (Resp_File_Options'Last).all
                                            &
                                              Get_Name_String
                                              (Response_File_Name));
                              First_Object_Index :=
                                First_Object_Index + 1;
                           end if;

                           --  And put the arguments following the object
                           --  files immediately after the response file
                           --  argument(s). Update Arguments_Displayed
                           --  too.

                           Arguments (First_Object_Index ..
                                        Last_Argument -
                                          Last_Object_Index +
                                            First_Object_Index -
                                              1) :=
                                     Arguments (Last_Object_Index + 1 ..
                                                          Last_Argument);
                           Arguments_Displayed
                             (First_Object_Index ..
                                Last_Argument -
                                  Last_Object_Index +
                                    First_Object_Index -
                                      1) :=
                                     Arguments_Displayed
                                       (Last_Object_Index + 1 ..
                                                    Last_Argument);
                           Last_Argument :=
                             Last_Argument - Last_Object_Index +
                               First_Object_Index - 1;
                        end if;
                     end;
                  end if;
               end if;
            end;
         end if;

         --  Delete an eventual executable, in case it is a symbolic
         --  link as we don't want to modify the target of the link.

         declare
            Dummy : Boolean;
            pragma Unreferenced (Dummy);

         begin
            Delete_File (Get_Name_String (Exec_Path_Name), Dummy);
         end;

         Display_Command (Linker_Name.all, Linker_Path);

         Spawn
           (Linker_Path.all, Arguments (1 .. Last_Argument), Success);

         if Response_File_Name /= No_Path and then
           not Debug.Debug_Flag_N
         then
            declare
               Dont_Care : Boolean;
               pragma Warnings (Off, Dont_Care);
            begin
               Delete_File
                 (Get_Name_String (Response_File_Name), Dont_Care);

               if Response_2 /= No_Path then
                  Delete_File
                    (Get_Name_String (Response_2), Dont_Care);
               end if;
            end;
         end if;

         if not Success then
            Fail_Program
              (Main_File.Tree, "link of " & Main & " failed");
         end if;
      end if;
   end Link_Main;

   -------------
   -- Options --
   -------------

   package body Options is

      type Option_Data is record
         Option : Option_Type;
         Value  : Natural := 0;
      end record;

      package Command_Line_Options is new Table.Table
        (Table_Component_Type => Option_Data,
         Table_Index_Type     => Natural,
         Table_Low_Bound      => 1,
         Table_Initial        => 10,
         Table_Increment      => 100,
         Table_Name           => "Makegpr.Opt.Command_Line_Options");
      --  Table to store the command line options

      ----------------------------------
      -- Process_Command_Line_Options --
      ----------------------------------

      procedure Process_Command_Line_Options is
      begin
         for Index in 1 .. Command_Line_Options.Last loop
            case Command_Line_Options.Table (Index).Option is
               when Force_Compilations_Option =>
                  Opt.Force_Compilations := True;

               when Keep_Going_Option =>
                  Opt.Keep_Going := True;

               when Maximum_Processes_Option =>
                  Opt.Maximum_Processes :=
                    Command_Line_Options.Table (Index).Value;

               when Quiet_Output_Option =>
                  Opt.Quiet_Output := True;
                  Opt.Verbose_Mode := False;

               when Check_Switches_Option =>
                  Opt.Check_Switches := True;

               when Verbose_Mode_Option =>
                  Opt.Verbose_Mode    := True;
                  Opt.Verbosity_Level := Opt.High;
                  Opt.Quiet_Output    := False;

               when Verbose_Low_Mode_Option =>
                  Opt.Verbose_Mode    := True;
                  Opt.Verbosity_Level := Opt.Low;
                  Opt.Quiet_Output    := False;

               when Verbose_Medium_Mode_Option =>
                  Opt.Verbose_Mode    := True;
                  Opt.Verbosity_Level := Opt.Medium;
                  Opt.Quiet_Output    := False;

               when Warnings_Treat_As_Error =>
                  Opt.Warning_Mode := Opt.Treat_As_Error;

               when Warnings_Normal =>
                  Opt.Warning_Mode := Opt.Normal;

               when Warnings_Suppress =>
                  Opt.Warning_Mode := Opt.Suppress;

               when Indirect_Imports =>
                  Buildgpr.Indirect_Imports :=
                    Command_Line_Options.Table (Index).Value /= 0;
            end case;
         end loop;
      end Process_Command_Line_Options;

      ----------------------------------
      -- Register_Command_Line_Option --
      ----------------------------------

      procedure Register_Command_Line_Option
        (Option : Option_Type; Value : Natural := 0)
      is
      begin
         Command_Line_Options.Increment_Last;
         Command_Line_Options.Table (Command_Line_Options.Last) :=
           (Option => Option, Value => Value);
      end Register_Command_Line_Option;

   end Options;

   ----------------------------
   -- Post_Compilation_Phase --
   ----------------------------

   procedure Post_Compilation_Phase is

      procedure Do_Post (Project : Project_Id; Tree : Project_Tree_Ref);

      -------------
      -- Do_Post --
      -------------

      procedure Do_Post (Project : Project_Id; Tree : Project_Tree_Ref) is
      begin
         if Builder_Data (Tree).Need_Binding then
            Post_Compilation_Phase (Project, Tree);

            if Total_Errors_Detected > 0 then
               Fail_Program (Tree, "*** bind failed");
            end if;
         end if;
      end Do_Post;

      procedure Post_Compile_All is new For_Project_And_Aggregated (Do_Post);

   begin
      Post_Compile_All (Main_Project, Project_Tree);
   end Post_Compilation_Phase;

   ----------------------------
   -- Post_Compilation_Phase --
   ----------------------------

   procedure Post_Compilation_Phase
     (Main_Project : Project_Id; Project_Tree : Project_Tree_Ref)
   is
      Success : Boolean;

      Exchange_File : Ada.Text_IO.File_Type;
      Line          : String (1 .. 1_000);
      Last          : Natural;

      Proj_List : Project_List;

      Shared_Libs : Boolean := False;

      Bind_Exchange_TS                 : Time_Stamp_Type;
      Bind_Object_TS                   : Time_Stamp_Type;
      Binder_Driver_Needs_To_Be_Called : Boolean := False;

      Project_Path    : Name_Id;
      Project_File_TS : Time_Stamp_Type;

      There_Are_Stand_Alone_Libraries : Boolean := False;
      --  Set to True if there are SALS in the project tree.

      procedure Bind_Language
        (Main_Proj            : Project_Id;
         Main                 : String;
         Main_Base_Name_Index : File_Name_Type;
         Main_File            : Main_Info;
         Main_Id              : File_Name_Type;
         B_Data               : Binding_Data);
      --  Do the "binding" phase for the language describeb in B_Data

      procedure Add_Dependency_Files
        (For_Project : Project_Id;
         Language    : Language_Ptr;
         Main_Source : Source_Id;
         Dep_Files   : out Boolean);
      --  Put the dependency files of the project in the binder exchange file

      --------------------------
      -- Add_Dependency_Files --
      --------------------------

      procedure Add_Dependency_Files
        (For_Project : Project_Id;
         Language    : Language_Ptr;
         Main_Source : Source_Id;
         Dep_Files   : out Boolean)
      is
         Config : constant Language_Config := Language.Config;
         Roots  : Roots_Access;
         Iter   : Source_Iterator;

         procedure Put_Dependency_File (Source : Source_Id);
         --  Put in the exchange file the dependency file path name for source
         --  Source, if applicable.

         -------------------------
         -- Put_Dependency_File --
         -------------------------

         procedure Put_Dependency_File (Source : Source_Id) is
         begin
            if Source.Language.Name = Language.Name
              and then
                ((Config.Kind = File_Based and then Source.Kind = Impl)
                 or else
                   (Config.Kind = Unit_Based
                    and then
                      Source.Unit /= No_Unit_Index
                    and then
                      Source.Unit /= Main_Source.Unit
                    and then
                      (Source.Kind = Impl
                       or else
                         Other_Part (Source) = No_Source)
                    and then not Is_Subunit (Source)))
              and then Is_Included_In_Global_Archive
                (Source.Object, Source.Project)
            then
               if Source.Project = For_Project
                 or not Source.Project.Library
                 or Config.Kind = File_Based
               then
                  Put_Line
                    (Exchange_File, Get_Name_String (Source.Dep_Path));
                  Dep_Files := True;

               elsif not Source.Project.Standalone_Library then
                  Get_Name_String
                    (Source.Project.Library_ALI_Dir.Display_Name);
                  Add_Char_To_Name_Buffer (Directory_Separator);
                  Get_Name_String_And_Append (Source.Dep_Name);
                  Put_Line (Exchange_File, Name_Buffer (1 .. Name_Len));
                  Dep_Files := True;
               end if;
            end if;
         end Put_Dependency_File;

      begin
         Dep_Files := False;

         Roots := Main_Source.Roots;

         if Roots = null then
            if Main_Source.Unit = No_Unit_Index then
               Iter := For_Each_Source (Project_Tree);
               while Prj.Element (Iter) /= No_Source loop
                  Initialize_Source_Record (Prj.Element (Iter));

                  --  Do not bind the non compilable sources, such as those
                  --  that have been locally removed.

                  if Is_Compilable (Prj.Element (Iter)) then
                     Put_Dependency_File (Prj.Element (Iter));
                  end if;

                  Next (Iter);
               end loop;
            end if;

         else
            --  Put the Roots
            while Roots /= null loop
               Put_Dependency_File (Roots.Root);
               Roots := Roots.Next;
            end loop;
         end if;
      end Add_Dependency_Files;

      -------------------
      -- Bind_Language --
      -------------------

      procedure Bind_Language
        (Main_Proj            : Project_Id;
         Main                 : String;
         Main_Base_Name_Index : File_Name_Type;
         Main_File            : Main_Info;
         Main_Id              : File_Name_Type;
         B_Data               : Binding_Data)
      is
         Main_Source : constant Source_Id := Main_File.Source;

         Bind_Exchange                    : String_Access;
         Options_Instance                 : Bind_Option_Table_Ref :=
                                              No_Bind_Option_Table;
         Dep_Files                        : Boolean;
         Lang_Index                       : Language_Ptr;
         Object_File_Suffix_Label_Written : Boolean;

      begin
         Binder_Driver_Needs_To_Be_Called := Opt.Force_Compilations;

         --  First check if the binder driver needs to be called.
         --  It needs to be called if
         --    1) there is no existing binder exchange file
         --    2) there is no binder generated object file
         --    3) there is a dependency file of the language that
         --       is more recent than any of these two files

         if not Binder_Driver_Needs_To_Be_Called
           and then Opt.Verbose_Mode
         then
            Write_Line
              ("   Checking binder generated files for " &
               Main & "...");
         end if;

         Bind_Exchange :=
           Binder_Exchange_File_Name
             (Main_Base_Name_Index, B_Data.Binder_Prefix);
         Bind_Exchange_TS :=
           File_Stamp
             (Path_Name_Type'(Create_Name (Bind_Exchange.all)));

         if not Binder_Driver_Needs_To_Be_Called then
            if Bind_Exchange_TS = Empty_Time_Stamp then
               Binder_Driver_Needs_To_Be_Called := True;

               if Opt.Verbose_Mode then
                  Write_Line
                    ("      -> binder exchange file " &
                     Bind_Exchange.all &
                     " does not exist");
               end if;

            else
               begin
                  Open (Exchange_File, In_File, Bind_Exchange.all);

               exception
                  when others =>
                     Binder_Driver_Needs_To_Be_Called := True;

                     if Opt.Verbose_Mode then
                        Write_Line
                          ("      -> could not open " &
                           "binder exchange file" &
                           Bind_Exchange.all);
                     end if;
               end;
            end if;
         end if;

         if not Binder_Driver_Needs_To_Be_Called then
            begin
               Get_Line (Exchange_File, Line, Last);
            exception
               when others =>
                  Binder_Driver_Needs_To_Be_Called := True;

                  if Opt.Verbose_Mode then
                     Write_Line
                       ("      -> previous gprbind failed, or " &
                        Bind_Exchange.all &
                        " corrupted");
                  end if;
            end;
         end if;

         --  Check the generated object file

         if not Binder_Driver_Needs_To_Be_Called then
            if Line (1 .. Last) /=
              Binding_Label (Generated_Object_File)
              or else End_Of_File (Exchange_File)
            then
               Binder_Driver_Needs_To_Be_Called := True;

               if Opt.Verbose_Mode then
                  Write_Line
                    ("      -> previous gprbind failed, or " &
                     Bind_Exchange.all &
                     " corrupted");
               end if;

            else
               Get_Line (Exchange_File, Line, Last);
               Bind_Object_TS :=
                 File_Stamp
                   (Path_Name_Type'(Create_Name (Line (1 .. Last))));

               if Bind_Object_TS = Empty_Time_Stamp then
                  Binder_Driver_Needs_To_Be_Called := True;

                  if Opt.Verbose_Mode then
                     Write_Line
                       ("      -> binder generated object " &
                        Line (1 .. Last) &
                        " does not exist");
                  end if;
               end if;
            end if;
         end if;

         if not Binder_Driver_Needs_To_Be_Called then
            if End_Of_File (Exchange_File) then
               Binder_Driver_Needs_To_Be_Called := True;

            else
               Get_Line (Exchange_File, Line, Last);

               if Line (1 .. Last) /=
                 Binding_Label (Project_Files)
                 or else End_Of_File (Exchange_File)
               then
                  Binder_Driver_Needs_To_Be_Called := True;
               end if;
            end if;

            if Binder_Driver_Needs_To_Be_Called then
               if Opt.Verbose_Mode then
                  Write_Line
                    ("      -> previous gprbind failed, or " &
                     Bind_Exchange.all & " corrupted");
               end if;

            else
               --  Populate the hash table Project_File_Paths with
               --  the paths of all project files in the closure
               --  of the main project.

               Project_File_Paths.Reset;

               Project_File_Paths.Set
                 (Name_Id (Main_Proj.Path.Display_Name), True);

               Proj_List := Main_Proj.All_Imported_Projects;

               while Proj_List /= null loop
                  Project_File_Paths.Set
                    (Name_Id (Proj_List.Project.Path.Display_Name),
                     True);
                  Proj_List := Proj_List.Next;
               end loop;

               --  Get the project file paths from the exchange
               --  file and check if they are the expected project
               --  files with the same time stamps.

               while not End_Of_File (Exchange_File) loop
                  Get_Line (Exchange_File, Name_Buffer, Name_Len);
                  exit when Name_Len > 0 and then Name_Buffer (1) = '[';

                  if End_Of_File (Exchange_File) then
                     Binder_Driver_Needs_To_Be_Called := True;

                     if Opt.Verbose_Mode then
                        Write_Line
                          ("      -> previous gprbind failed, " &
                           "or " &
                           Bind_Exchange.all &
                           " corrupted");
                     end if;

                     exit;
                  end if;

                  Project_Path := Name_Find;

                  if Project_File_Paths.Get (Project_Path) then
                     Project_File_Paths.Remove (Project_Path);
                     Get_Line
                       (Exchange_File, Line, Last);

                     Project_File_TS :=
                       File_Stamp (Path_Name_Type (Project_Path));

                     if String (Project_File_TS) /= Line (1 .. Last) then
                        Binder_Driver_Needs_To_Be_Called := True;

                        if Opt.Verbose_Mode then
                           Write_Line
                             ("      -> project file " &
                              Get_Name_String (Project_Path) &
                              " has been modified");
                        end if;

                        exit;
                     end if;

                  else
                     Binder_Driver_Needs_To_Be_Called := True;

                     if Opt.Verbose_Mode then
                        Write_Line
                          ("      -> unknown project file " &
                           Get_Name_String (Project_Path));
                     end if;

                     exit;
                  end if;
               end loop;

               --  Check if there are still project file paths in
               --  the has table.

               if (not Binder_Driver_Needs_To_Be_Called)
                 and then Project_File_Paths.Get_First
               then
                  Binder_Driver_Needs_To_Be_Called := True;

                  if Opt.Verbose_Mode then
                     Write_Line
                       ("      -> more project files");
                  end if;
               end if;
            end if;
         end if;

         if Is_Open (Exchange_File) then
            Close (Exchange_File);
         end if;

         if not Binder_Driver_Needs_To_Be_Called then

            Queue.Initialize (Opt.One_Compilation_Per_Obj_Dir, Force => True);

            declare
               Config          : constant Language_Config :=
                                   B_Data.Language.Config;
               Source_Identity : Source_Id;
               Roots           : Roots_Access;
               Source          : Source_Id;
               Iter            : Source_Iterator;

            begin
               --  Put the root sources in the queue

               if Main_Source.Language.Name = B_Data.Language.Name then
                  Queue.Insert
                    (Source => (Format => Format_Gprbuild,
                                Tree   => Main_File.Tree,
                                Id     => Main_File.Source));
               end if;

               Roots := Main_Source.Roots;

               while Roots /= null loop
                  Queue.Insert
                    (Source => (Format => Format_Gprbuild,
                                Tree   => Main_File.Tree,
                                Id     => Roots.Root));
                  Roots := Roots.Next;
               end loop;

               --  If main is not unit base and there is no root,
               --  check all sources with the language name of the
               --  binder, except those that are not interfaces of
               --  their project.

               if Queue.Is_Empty then
                  Iter := For_Each_Source (Project_Tree);

                  Loop1 : loop
                     Source := Prj.Element (Iter);
                     exit Loop1 when Source = No_Source;

                     if Source.Language.Name =
                       B_Data.Language.Name
                       and then
                         not Source.Locally_Removed
                         and then Is_Compilable (Source)
                       and then
                         ((Config.Kind = File_Based
                           and then Source.Kind = Impl)
                          or else
                            (Config.Kind = Unit_Based
                             and then
                               Source.Unit /= No_Unit_Index
                             and then
                               Source.Unit /= Main_Source.Unit
                             and then
                               (Source.Kind = Impl
                                or else
                                  Other_Part (Source) = No_Source)
                             and then not Is_Subunit (Source)))
                       and then Source.In_Interfaces
                     then
                        declare
                           Proj  : Project_Id;
                           Src   : Source_Id;
                           Iter2 : Source_Iterator;

                        begin
                           --  If a source is overriden in an
                           --  extending project, then the object file
                           --  is not included in the global archive.

                           Proj := Source.Project.Extended_By;
                           Loop2 : while Proj /= No_Project loop
                              Iter2 := For_Each_Source
                                (Project_Tree, Proj);
                              loop
                                 Src := Prj.Element (Iter2);
                                 exit when Src = No_Source;

                                 exit Loop1 when
                                   Src.Object = Source.Object;

                                 Next (Iter2);
                              end loop;
                              Proj := Proj.Extended_By;
                           end loop Loop2;
                        end;

                        Queue.Insert
                          (Source => (Format => Format_Gprbuild,
                                      Tree   => Main_File.Tree,
                                      Id     => Source));
                     end if;

                     Next (Iter);
                  end loop Loop1;

               end if;

               --  Get each file from the queue and check its
               --  dependency file.

               declare
                  Dep_File : File_Name_Type;
                  Dep_Path : Path_Name_Type;
                  Dep_TS   : aliased File_Attributes :=
                               Unknown_Attributes;
                  Stamp    : Time_Stamp_Type;
                  The_ALI  : ALI.ALI_Id;
                  Text     : Text_Buffer_Ptr;
                  Found    : Boolean;
                  Source   : Queue.Source_Info;
               begin
                  while not Queue.Is_Empty loop
                     Queue.Extract (Found, Source);
                     Source_Identity := Source.Id;

                     Initialize_Source_Record (Source_Identity);

                     --  Get the dependency file for this source

                     Dep_File := Source_Identity.Dep_Name;
                     Dep_Path := Source_Identity.Dep_Path;
                     Dep_TS   := Source_Identity.Dep_TS;

                     --  For a library file, if there is no ALI file
                     --  in the object directory, check in the Library
                     --  ALI directory.

                     if (not Is_Regular_File
                         (Get_Name_String (Dep_Path)))
                       and then Source_Identity.Project.Library
                       and then
                         Source_Identity.Project.Library_ALI_Dir /=
                           No_Path_Information
                     then
                        Name_Len := 0;
                        Add_Str_To_Name_Buffer
                          (Get_Name_String
                             (Source_Identity.Project
                              .Library_ALI_Dir.Display_Name));
                        Add_Char_To_Name_Buffer
                          (Directory_Separator);
                        Add_Str_To_Name_Buffer
                          (Get_Name_String (Dep_File));
                        Name_Buffer (Name_Len + 1) := ASCII.NUL;

                        Dep_TS := Unknown_Attributes;
                        if Is_Regular_File
                          (Name_Buffer'Address, Dep_TS'Access)
                        then
                           Dep_Path := Name_Find;
                        end if;
                     end if;

                     declare
                        Proj : Project_Id :=
                                 Source_Identity.Project.Extended_By;
                     begin
                        while Proj /= No_Project loop
                           Name_Len := 0;

                           if Proj.Library
                             and then
                               Proj.Library_ALI_Dir /= No_Path_Information
                           then
                              Add_Str_To_Name_Buffer
                                (Get_Name_String
                                   (Proj.Library_ALI_Dir.Display_Name));

                           else
                              Add_Str_To_Name_Buffer
                                (Get_Name_String
                                   (Proj.Object_Directory.Display_Name));
                           end if;

                           Add_Char_To_Name_Buffer
                             (Directory_Separator);
                           Add_Str_To_Name_Buffer
                             (Get_Name_String (Dep_File));
                           Name_Buffer (Name_Len + 1) := ASCII.NUL;

                           --  Check if the dependency file exists in
                           --  the extended project, and if it does,
                           --  replace both Dep_Path and Dep_TS with
                           --  the information for it.

                           declare
                              NDT : aliased File_Attributes :=
                                      Unknown_Attributes;
                           begin
                              if Is_Regular_File
                                (Name_Buffer'Address, NDT'Access)
                              then
                                 Dep_Path := Name_Find;
                                 Dep_TS := NDT;
                              end if;
                           end;

                           Proj := Proj.Extended_By;
                        end loop;
                     end;

                     Stamp := File_Time_Stamp (Dep_Path, Dep_TS'Access);

                     --  Check the time stamp against the binder
                     --  exchange file time stamp.

                     if Stamp = Empty_Time_Stamp then
                        Binder_Driver_Needs_To_Be_Called := True;

                        if Opt.Verbose_Mode then
                           Write_Str ("      -> cannot find ");
                           Write_Line (Get_Name_String (Dep_Path));
                        end if;

                        exit;

                     elsif Stamp > Bind_Exchange_TS then
                        Binder_Driver_Needs_To_Be_Called := True;

                        if Opt.Verbose_Mode then
                           Write_Str ("      -> ");
                           Write_Str (Get_Name_String (Dep_Path));
                           Write_Line
                             (" is more recent that the binder " &
                              "exchange file");
                        end if;

                        exit;
                     else
                        Text := Read_Library_Info_From_Full
                          (File_Name_Type (Dep_Path),
                           Dep_TS'Access);

                        if Text /= null then
                           The_ALI :=
                             ALI.Scan_ALI
                               (File_Name_Type (Dep_Path),
                                Text,
                                Ignore_ED     => False,
                                Err           => True,
                                Ignore_Errors => True,
                                Read_Lines    => "W");
                           Free (Text);

                           Queue.Insert_Withed_Sources_For
                             (The_ALI,
                              Project_Tree,
                              Excluding_Shared_SALs => True);
                        end if;
                     end if;
                  end loop;
               end;
            end;
         end if;

         if not Binder_Driver_Needs_To_Be_Called then
            if Opt.Verbose_Mode then
               Write_Line ("      -> up to date");
            end if;

         else
            Create (Exchange_File, Out_File, Bind_Exchange.all);

            --  Optional line: Quiet or Verbose

            if Opt.Quiet_Output then
               Put_Line (Exchange_File, Binding_Label (Quiet));

            elsif Opt.Verbose_Mode then
               Put_Line (Exchange_File, Binding_Label (Verbose));
            end if;

            --  If -dn was used, indicate to gprbind that the
            --  temporary response file, if created, should not
            --  deleted.

            if Debug_Flag_N then
               Put_Line
                 (Exchange_File,
                  Binding_Label (Delete_Temp_Files));
               Put_Line (Exchange_File, "False");
            end if;

            --  If there are Stand-Alone Libraries, tell it to gprbind

            if There_Are_Stand_Alone_Libraries then
               Put_Line
                 (Exchange_File,
                  Binding_Label
                    (Gprexch.There_Are_Stand_Alone_Libraries));
            end if;

            --  If the language is Ada, create a binder mapping file
            --  and pass it to gprbind.

            if B_Data.Language_Name = Name_Ada then
               declare
                  Mapping_Path : constant Path_Name_Type :=
                    Create_Binder_Mapping_File (Project_Tree);

               begin
                  if Mapping_Path /= No_Path then
                     Put_Line
                       (Exchange_File,
                        Binding_Label (Gprexch.Mapping_File));
                     Put_Line
                       (Exchange_File,
                        Get_Name_String (Mapping_Path));
                  end if;
               end;
            end if;

            --  Send the Toolchain Version if there is one for the language

            if B_Data.Language.Config.Toolchain_Version /= No_Name then
               Put_Line (Exchange_File, Binding_Label (Toolchain_Version));
               Put_Line
                 (Exchange_File,
                  Get_Name_String (B_Data.Language.Name));
               Put_Line
                 (Exchange_File,
                  Get_Name_String (B_Data.Language.Config.Toolchain_Version));
            end if;

            --  Send the object file suffix for each language where it
            --  is declared.

            Lang_Index := Main_Proj.Languages;
            Object_File_Suffix_Label_Written := False;

            while Lang_Index /= No_Language_Index loop
               if Lang_Index.Config.Object_File_Suffix /= No_Name then
                  if not Object_File_Suffix_Label_Written then
                     Put_Line
                       (Exchange_File, Binding_Label
                          (Gprexch.Object_File_Suffix));
                     Object_File_Suffix_Label_Written := True;
                  end if;

                  Put_Line
                    (Exchange_File,
                     Get_Name_String (Lang_Index.Name));
                  Put_Line
                    (Exchange_File,
                     Get_Name_String
                       (Lang_Index.Config.Object_File_Suffix));
               end if;

               Lang_Index := Lang_Index.Next;
            end loop;

            --  Optional line: shared libs

            if Shared_Libs then
               Put_Line (Exchange_File, Binding_Label (Gprexch.Shared_Libs));
            end if;

            --  First, the main base name

            Put_Line
              (Exchange_File,
               Binding_Label (Gprexch.Main_Base_Name));
            Put_Line
              (Exchange_File, Get_Name_String (Main_Base_Name_Index));

            --  Then, the compiler path and required switches

            declare
               Config  : Language_Config renames
                           B_Data.Language.Config;
               List    : Name_List_Index;
               Nam_Nod : Name_Node;
            begin
               --  Compiler path

               Put_Line
                 (Exchange_File,
                  Binding_Label (Gprexch.Compiler_Path));
               Put_Line
                 (Exchange_File,
                  Get_Compiler_Driver_Path
                    (Project_Tree, B_Data.Language).all);

               --  Leading required switches, if any

               List := Config.Compiler_Leading_Required_Switches;
               if List /= No_Name_List then
                  Put_Line
                    (Exchange_File,
                     Binding_Label (Gprexch.Compiler_Leading_Switches));

                  while List /= No_Name_List loop
                     Nam_Nod := Project_Tree.Shared.Name_Lists.Table (List);
                     Put_Line
                       (Exchange_File,
                        Get_Name_String (Nam_Nod.Name));
                     List := Nam_Nod.Next;
                  end loop;
               end if;

               --  Trailing required switches, if any

               List := Config.Compiler_Trailing_Required_Switches;
               if List /= No_Name_List then
                  Put_Line
                    (Exchange_File,
                     Binding_Label
                       (Gprexch.Compiler_Trailing_Switches));

                  while List /= No_Name_List loop
                     Nam_Nod :=
                       Project_Tree.Shared.Name_Lists.Table (List);
                     Put_Line
                       (Exchange_File, Get_Name_String (Nam_Nod.Name));
                     List := Nam_Nod.Next;
                  end loop;
               end if;
            end;

            --  Then, the Dependency files

            if Main_Source.Unit /= No_Unit_Index then
               Initialize_Source_Record (Main_Source);
               Put_Line
                 (Exchange_File,
                  Binding_Label (Main_Dependency_File));
               Put_Line
                 (Exchange_File,
                  Get_Name_String (Main_Source.Dep_Path));
            end if;

            --  Add the relevant dependency files, either those in
            --  Roots (<main>) for the project, or all dependency
            --  files in the project tree, if Roots (<main>) is not
            --  specified .

            Put_Line
              (Exchange_File, Binding_Label (Dependency_Files));

            Add_Dependency_Files
              (Main_Proj,
               B_Data.Language,
               Main_Source,
               Dep_Files);

            --  Put the options, if any

            declare
               The_Packages : constant Package_Id :=
                                Main_Proj.Decl.Packages;

               Binder_Package : constant Prj.Package_Id :=
                                  Prj.Util.Value_Of
                                    (Name        => Name_Binder,
                                     In_Packages => The_Packages,
                                     Shared      => Project_Tree.Shared);
               Config         : constant Language_Config :=
                                  B_Data.Language.Config;

               Switches    : Variable_Value;
               Switch_List : String_List_Id;
               Element     : String_Element;

            begin
               --  First, check if there are binder options
               --  specified in the main project file.

               if Binder_Package /= No_Package then
                  declare
                     Defaults : constant Array_Element_Id :=
                                  Prj.Util.Value_Of
                                    (Name      => Name_Default_Switches,
                                     In_Arrays =>
                                       Project_Tree.Shared.Packages.Table
                                         (Binder_Package).Decl.Arrays,
                                     Shared    => Project_Tree.Shared);

                     Switches_Array : constant Array_Element_Id :=
                                        Prj.Util.Value_Of
                                          (Name      => Name_Switches,
                                           In_Arrays =>
                                             Project_Tree.Shared.Packages.Table
                                               (Binder_Package).Decl.Arrays,
                                           Shared    => Project_Tree.Shared);

                  begin
                     Switches :=
                       Prj.Util.Value_Of
                         (Index           => Name_Id (Main_Id),
                          Src_Index       => 0,
                          In_Array        => Switches_Array,
                          Shared          => Project_Tree.Shared,
                          Allow_Wildcards => True);

                     if Switches = Nil_Variable_Value then
                        Switches :=
                          Prj.Util.Value_Of
                            (Index                  =>
                                 B_Data.Language_Name,
                             Src_Index              => 0,
                             In_Array               => Switches_Array,
                             Shared                 => Project_Tree.Shared,
                             Force_Lower_Case_Index => True);
                     end if;

                     if Switches = Nil_Variable_Value then
                        Switches :=
                          Prj.Util.Value_Of
                            (Index                  => All_Other_Names,
                             Src_Index              => 0,
                             In_Array               => Switches_Array,
                             Shared                 => Project_Tree.Shared,
                             Force_Lower_Case_Index => True);
                     end if;

                     if Switches = Nil_Variable_Value then
                        Switches :=
                          Prj.Util.Value_Of
                            (Index     => B_Data.Language_Name,
                             Src_Index => 0,
                             In_Array  => Defaults,
                             Shared    => Project_Tree.Shared);
                     end if;
                  end;
               end if;

               --  If there are binder options, either minimum
               --  binder options, or in the main project file or
               --  on the command line, put them in the exchange
               --  file.

               Options_Instance :=
                 Binder_Options_HTable.Get (B_Data.Language_Name);

               if Config.Binder_Required_Switches /= No_Name_List
                 or else
                   Switches.Kind = Prj.List
                   or else
                     All_Language_Binder_Options.Last > 0
                     or else
                       Options_Instance /= No_Bind_Option_Table
               then
                  Put_Line
                    (Exchange_File,
                     Binding_Label (Gprexch.Binding_Options));

                  --  First, the required switches, if any

                  declare
                     List : Name_List_Index :=
                              Config.Binder_Required_Switches;
                     Elem : Name_Node;

                  begin
                     while List /= No_Name_List loop
                        Elem :=
                          Project_Tree.Shared.Name_Lists.Table (List);
                        Get_Name_String (Elem.Name);

                        if Name_Len > 0 then
                           Put_Line
                             (Exchange_File,
                              Name_Buffer (1 .. Name_Len));
                        end if;

                        List := Elem.Next;
                     end loop;
                  end;

                  --  Then, the eventual options in the main
                  --  project file.

                  if Switches.Kind = Prj.List then
                     declare
                        Option : String_Access;

                     begin
                        Switch_List := Switches.Values;

                        while Switch_List /= Nil_String loop
                           Element :=
                             Project_Tree.Shared.String_Elements.Table
                               (Switch_List);

                           Get_Name_String (Element.Value);

                           if Name_Len > 0 then
                              Option :=
                                new String'
                                  (Name_Buffer (1 .. Name_Len));
                              Test_If_Relative_Path
                                (Option,
                                 Main_Project_Dir.all,
                                 No_Name);
                              Put_Line (Exchange_File, Option.all);
                           end if;

                           Switch_List := Element.Next;
                        end loop;
                     end;
                  end if;

                  --  Then those on the command line, for all
                  --  binder drivers, if any.

                  for J in 1 .. All_Language_Binder_Options.Last loop
                     Put_Line
                       (Exchange_File,
                        All_Language_Binder_Options.Table (J).all);
                  end loop;

                  --  Finally those on the command line for the
                  --  binder driver of the language

                  if Options_Instance /= No_Bind_Option_Table then
                     for Index in 1 .. Binder_Options.Last
                       (Options_Instance.all)
                     loop
                        Put_Line
                          (Exchange_File,
                           Options_Instance.Table (Index).all);
                     end loop;
                  end if;

               end if;
            end;

            --  Finally, the list of the project paths with their
            --  time stamps.

            Put_Line
              (Exchange_File,
               Binding_Label (Project_Files));

            Put_Line
              (Exchange_File,
               Get_Name_String (Main_Proj.Path.Display_Name));

            Put_Line
              (Exchange_File,
               String (File_Stamp (Main_Proj.Path.Display_Name)));

            Proj_List := Main_Proj.All_Imported_Projects;

            while Proj_List /= null loop
               Put_Line
                 (Exchange_File,
                  Get_Name_String
                    (Proj_List.Project.Path.Display_Name));

               Put_Line
                 (Exchange_File,
                  String
                    (File_Stamp
                       (Proj_List.Project.Path.Display_Name)));

               Proj_List := Proj_List.Next;
            end loop;

            Close (Exchange_File);

            if Main_Source.Unit = No_Unit_Index and then (not Dep_Files) then
               if Opt.Verbose_Mode then
                  Write_Line ("      -> nothing to bind");
               end if;

            else
               if B_Data.Language.Config.Objects_Path /= No_Name then
                  declare
                     Env_Var   : constant String :=
                                   Get_Name_String
                                     (B_Data.Language.Config.
                                                      Objects_Path);
                     Path_Name : String_Access :=
                                   Main_Proj.Objects_Path;
                  begin
                     if Path_Name = null then
                        if Current_Verbosity = High then
                           Put_Line (Env_Var & " :");
                        end if;

                        Get_Directories
                          (Project_Tree => Project_Tree,
                           For_Project  => Main_Proj,
                           Activity     => Executable_Binding,
                           Languages    => No_Names);

                        Path_Name := Create_Path_From_Dirs;
                        Main_Proj.Objects_Path := Path_Name;
                     end if;

                     Setenv (Env_Var, Path_Name.all);

                     if Opt.Verbose_Mode then
                        Write_Str (Env_Var);
                        Write_Str (" = ");
                        Write_Line (Path_Name.all);
                     end if;
                  end;

               elsif B_Data.Language.Config.Objects_Path_File /=
                 No_Name
               then
                  declare
                     Env_Var   : constant String :=
                                   Get_Name_String
                                     (B_Data.Language.Config.
                                                      Objects_Path_File);
                     Path_Name : Path_Name_Type :=
                                   Main_Proj.Objects_Path_File_Without_Libs;
                  begin
                     if Path_Name = No_Path then
                        if Current_Verbosity = High then
                           Put_Line (Env_Var & " :");
                        end if;

                        Get_Directories
                          (Project_Tree => Project_Tree,
                           For_Project  => Main_Proj,
                           Activity     => Executable_Binding,
                           Languages    => No_Names);

                        declare
                           FD     : File_Descriptor;
                           Len    : Integer;
                           Status : Boolean;
                        begin
                           Prj.Env.Create_New_Path_File
                             (Shared    => Project_Tree.Shared,
                              Path_FD   => FD,
                              Path_Name =>
                                Main_Proj.
                                  Objects_Path_File_Without_Libs);

                           if FD = Invalid_FD then
                              Fail_Program
                                (Project_Tree,
                                 "could not create " &
                                 "temporary path file");
                           end if;

                           Path_Name :=
                             Main_Proj.
                               Objects_Path_File_Without_Libs;

                           for Index in 1 .. Directories.Last loop
                              Get_Name_String
                                (Directories.Table (Index));

                              if Current_Verbosity = High then
                                 Put_Line
                                   (Name_Buffer (1 .. Name_Len));
                              end if;

                              Name_Len := Name_Len + 1;
                              Name_Buffer (Name_Len) := ASCII.LF;

                              Len :=
                                Write
                                  (FD,
                                   Name_Buffer (1)'Address,
                                   Name_Len);

                              if Len /= Name_Len then
                                 Fail_Program
                                   (Project_Tree, "disk full");
                              end if;
                           end loop;

                           Close (FD, Status);

                           if not Status then
                              Fail_Program
                                (Project_Tree, "disk full");
                           end if;
                        end;
                     end if;

                     Setenv (Env_Var, Get_Name_String (Path_Name));

                     if Opt.Verbose_Mode then
                        Write_Str (Env_Var);
                        Write_Str (" = ");
                        Write_Line (Get_Name_String (Path_Name));
                     end if;
                  end;
               end if;

               if not Opt.Quiet_Output then
                  if Opt.Verbose_Mode then
                     Write_Str (B_Data.Binder_Driver_Path.all);

                  else
                     Name_Len := 0;
                     Add_Str_To_Name_Buffer
                       (Base_Name
                          (Get_Name_String
                             (B_Data.Binder_Driver_Name)));

                     if Executable_Suffix'Length /= 0 and then
                       Name_Len > Executable_Suffix'Length and then
                       Name_Buffer
                         (Name_Len - Executable_Suffix'Length + 1
                          .. Name_Len)
                       = Executable_Suffix.all
                     then
                        Name_Len :=
                          Name_Len - Executable_Suffix'Length;
                     end if;

                     Write_Str (Name_Buffer (1 .. Name_Len));
                  end if;

                  Write_Char (' ');
                  Write_Line (Bind_Exchange.all);
               end if;

               Spawn
                 (B_Data.Binder_Driver_Path.all,
                  (1 => Bind_Exchange),
                  Success);

               if not Success then
                  Fail_Program
                    (Project_Tree, "unable to bind " & Main);
               end if;
            end if;
         end if;
      end Bind_Language;

   --  Start of processing for Post_Compilation_Phase

   begin
      --  Build the libraries, if any

      --  First, get the libraries in building order in table Library_Projs

      Process_Imported_Libraries
        (Main_Project,
         There_Are_SALs     => There_Are_Stand_Alone_Libraries,
         And_Project_Itself => True);

      if Library_Projs.Last > 0 then
         declare
            Lib_Projs : array (1 .. Library_Projs.Last) of Project_Id;
            Proj      : Project_Id;

         begin
            --  Copy the list of library projects in local array Lib_Projs,
            --  as procedure Build_Library uses table Library_Projs.

            for J in 1 .. Library_Projs.Last loop
               Lib_Projs (J) := Library_Projs.Table (J);
            end loop;

            for J in Lib_Projs'Range loop
               Proj := Lib_Projs (J);

               --  Try building a library only if no errors occured in library
               --  project and projects it depends on.

               if not Project_Compilation_Failed (Proj) then
                  if Proj.Extended_By = No_Project then
                     if not Proj.Externally_Built then
                        Build_Library (Proj, Project_Tree);
                     end if;

                     if Proj.Library_Kind /= Static then
                        Shared_Libs := True;
                     end if;
                  end if;
               end if;
            end loop;
         end;
      end if;

      --  If no main is specified, there is nothing else to do

      if Mains.Number_Of_Mains (Project_Tree) = 0 then
         return;
      end if;

      --  Check if there is a need to call a binder driver

      Find_Binding_Languages (Project_Tree, Main_Project);

      --  Proceed to bind (or rebind if needed) for each main

      Mains.Reset;

      loop
         declare
            Main_File : Main_Info;
         begin
            Main_File := Mains.Next_Main;
            exit when Main_File = No_Main_Info;

            if Main_File.Tree /= Project_Tree
              or else Project_Compilation_Failed (Main_File.Project)
            then
               --  Will be processed later, or do not need any processing in
               --  the case of compilation errors in the project.
               null;

            elsif
               not Builder_Data (Main_File.Tree).There_Are_Binder_Drivers
            then
               if Current_Verbosity = High then
                  Debug_Output ("Post-compilation, no binding required for",
                                Debug_Name (Main_File.Tree));
               end if;

            else
               declare
                  Main                 : constant String :=
                                           Get_Name_String (Main_File.File);
                  Main_Id              : constant File_Name_Type :=
                                           Create_Name (Base_Name (Main));
                  Main_Index           : constant Int := Main_File.Index;
                  B_Data               : Binding_Data;
                  Main_Base_Name_Index : File_Name_Type;
                  Main_Proj            : Project_Id;
                  Index_Separator      : Character;

               begin
                  Main_Proj := Ultimate_Extending_Project_Of
                    (Main_File.Source.Project);

                  --  Get the main base name-index name

                  Index_Separator :=
                    Main_File.Source.Language
                      .Config.Multi_Unit_Object_Separator;

                  Main_Base_Name_Index :=
                    Base_Name_Index_For (Main, Main_Index, Index_Separator);

                  Change_To_Object_Directory (Main_Proj);

                  B_Data := Builder_Data (Main_File.Tree).Binding;
                  while B_Data /= null loop
                     Bind_Language
                       (Main_Proj, Main, Main_Base_Name_Index,
                        Main_File, Main_Id, B_Data);
                     B_Data := B_Data.Next;
                  end loop;
               end;
            end if;
         end;
      end loop;
   end Post_Compilation_Phase;

   --------------------------------
   -- Process_Imported_Libraries --
   --------------------------------

   procedure Process_Imported_Libraries
     (For_Project        : Project_Id;
      There_Are_SALs     : out Boolean;
      And_Project_Itself : Boolean := False)
   is

      procedure Process_Project (Project : Project_Id);
      --  Process Project and its imported projects recursively.
      --  Add any library projects to table Library_Projs.

      ---------------------
      -- Process_Project --
      ---------------------

      procedure Process_Project (Project : Project_Id) is
         Imported : Project_List := Project.Imported_Projects;

      begin
         --  Nothing to do if process has already been processed

         if not Processed_Projects.Get (Project.Name) then
            Processed_Projects.Set (Project.Name, True);

            --  Call Process_Project recursively for any imported project.
            --  We first process the imported projects to guarantee that
            --  we have a proper reverse order for the libraries.

            while Imported /= null loop
               if Imported.Project /= No_Project then
                  Process_Project (Imported.Project);
               end if;

               Imported := Imported.Next;
            end loop;

            --  For an extending project, process the project being extended

            if Project.Extends /= No_Project then
               Process_Project (Project.Extends);
            end if;

            --  If it is a library project, add it to Library_Projs

            if (And_Project_Itself or (Project /= For_Project))
              and then Project.Extended_By = No_Project
              and then Project.Library
            then
               if Project.Standalone_Library then
                  There_Are_SALs := True;
               end if;

               Library_Projs.Append (Project);
            end if;

         end if;
      end Process_Project;

      --  Start of processing for Process_Imported_Libraries

   begin
      Processed_Projects.Reset;
      Library_Projs.Init;
      There_Are_SALs := False;

      Process_Project (For_Project);
   end Process_Imported_Libraries;

   ------------------------------------
   -- Process_Imported_Non_Libraries --
   ------------------------------------

   procedure Process_Imported_Non_Libraries (For_Project : Project_Id) is

      procedure Process_Project (Project : Project_Id);
      --  Process Project and its imported projects recursively.
      --  Add any library projects to table Library_Projs.

      ---------------------
      -- Process_Project --
      ---------------------

      procedure Process_Project (Project : Project_Id) is
         Imported : Project_List := Project.Imported_Projects;

      begin
         --  Nothing to do if process has already been processed

         if not Processed_Projects.Get (Project.Name) then
            Processed_Projects.Set (Project.Name, True);

            --  Call Process_Project recursively for any imported project.
            --  We first process the imported projects to guarantee that
            --  we have a proper reverse order for the libraries.

            while Imported /= null loop
               if Imported.Project /= No_Project then
                  Process_Project (Imported.Project);
               end if;

               Imported := Imported.Next;
            end loop;

            --  For an extending project, process the project being extended

            if Project.Extends /= No_Project then
               Process_Project (Project.Extends);
            end if;

            --  If it is not a library project, add it to Non_Library_Projs

            if Project /= For_Project
              and then Project.Extended_By = No_Project
              and then not Project.Library
            then
               Non_Library_Projs.Append (Project);
            end if;

         end if;
      end Process_Project;

      --  Start of processing for Process_Imported_Libraries

   begin
      Processed_Projects.Reset;
      Non_Library_Projs.Init;

      Process_Project (For_Project);
   end Process_Imported_Non_Libraries;

   ---------------------
   -- Project_Extends --
   ---------------------

   function Project_Extends
     (Extending : Project_Id;
      Extended  : Project_Id) return Boolean
   is
      Current : Project_Id := Extending;
   begin
      loop
         if Current = No_Project then
            return False;

         elsif Current = Extended then
            return True;
         end if;

         Current := Current.Extends;
      end loop;
   end Project_Extends;

   ------------------------
   -- Rpaths_Relative_To --
   ------------------------

   procedure Rpaths_Relative_To
     (Exec_Dir : Path_Name_Type;
      Origin   : Name_Id)
   is
      Exec      : String := Get_Name_String (Exec_Dir);
      Last_Exec : Positive;
      Curr_Exec : Positive;
      Last_Path : Positive;
      Curr_Path : Positive;
      Nmb       : Natural;

      Origin_Name : constant String := Get_Name_String (Origin);

   begin
      --  Replace all directory separators with '/' to ease search

      if Directory_Separator /= '/' then
         for J in Exec'Range loop
            if Exec (J) = Directory_Separator then
               Exec (J) := '/';
            end if;
         end loop;
      end if;

      for Npath in 1 .. Rpaths.Last loop
         declare
            Path : String := Rpaths.Table (Npath).all;

         begin
            --  Replace all directory separators with '/' to ease search

            if Directory_Separator /= '/' then
               for J in Path'Range loop
                  if Path (J) = Directory_Separator then
                     Path (J) := '/';
                  end if;
               end loop;
            end if;

            --  Find the number of common directories between the path and the
            --  exec directory.

            Nmb := 0;
            Curr_Path := Path'First;
            Curr_Exec := Exec'First;
            loop
               exit when
                 Curr_Path > Path'Last or else
                 Curr_Exec > Exec'Last or else
                 Path (Curr_Path) /= Exec (Curr_Exec);

               if Path (Curr_Path) = '/' then
                  Nmb := Nmb + 1;
                  Last_Path := Curr_Path;
                  Last_Exec := Curr_Exec;

               elsif Curr_Exec = Exec'Last and then Curr_Path > Path'Last then
                  Nmb := Nmb + 1;
                  Last_Path := Curr_Path + 1;
                  Last_Exec := Curr_Exec + 1;
                  exit;
               end if;

               Curr_Path := Curr_Path + 1;
               Curr_Exec := Curr_Exec + 1;
            end loop;

            --  If there is more than one common directories (the root
            --  directory does not count), then change the absolute path to a
            --  relative path.

            if Nmb > 1 then
               Nmb := 0;

               for J in Last_Exec .. Exec'Last - 1 loop
                  if Exec (J) = '/' then
                     Nmb := Nmb + 1;
                  end if;
               end loop;

               if Nmb = 0 then
                  if Last_Path >= Path'Last then
                     --  Case of the path being the exec dir

                     Rpaths.Table (Npath) :=
                       new String'(Origin_Name & Directory_Separator & ".");

                  else
                     --  Case of the path being a subdir of the exec dir

                     Rpaths.Table (Npath) :=
                       new String'
                         (Origin_Name & Directory_Separator &
                          Rpaths.Table (Npath) (Last_Path + 1 .. Path'Last));
                  end if;

               else
                  if Last_Path >= Path'Last then
                     --  Case of the exec dir being a subdir of the path

                     Rpaths.Table (Npath) :=
                       new String'
                         (Origin_Name & Directory_Separator &
                          (Nmb - 1) * (".." & Directory_Separator) & "..");

                  else
                     --  General case of path and exec dir having a common root

                     Rpaths.Table (Npath) :=
                       new String'
                         (Origin_Name & Directory_Separator &
                          Nmb * (".." & Directory_Separator) &
                          Rpaths.Table (Npath) (Last_Path + 1 .. Path'Last));
                  end if;
               end if;
            end if;
         end;
      end loop;
   end Rpaths_Relative_To;

   --------------------
   -- Record_Failure --
   --------------------

   procedure Record_Failure (Source : Source_Id) is
   begin
      Bad_Compilations.Increment_Last;
      Bad_Compilations.Table (Bad_Compilations.Last) := Source;
   end Record_Failure;

   ----------------------
   -- Recursive_Import --
   ----------------------

   procedure Recursive_Import (Project : Project_Id) is
      Ext : constant Project_Id := Project.Extends;
      L   : Project_List := Project.Imported_Projects;

   begin
      if Ext /= No_Project and then
        not Imports.Get (Ext)
      then
         Imports.Set (Ext, True);
         Recursive_Import (Ext);
      end if;

      while L /= null loop
         if not Imports.Get (L.Project) then
            Imports.Set (L.Project, True);
            Recursive_Import (L.Project);
         end if;

         L := L.Next;
      end loop;
   end Recursive_Import;

   --------------
   -- Scan_Arg --
   --------------

   procedure Scan_Arg
     (Arg          : String;
      Command_Line : Boolean;
      Language     : Name_Id;
      Success      : out Boolean)
   is
      Processed : Boolean := True;

      procedure Forbidden_In_Package_Builder;
      --  Fail if switch Arg is found in package Builder

      ----------------------------------
      -- Forbidden_In_Package_Builder --
      ----------------------------------

      procedure Forbidden_In_Package_Builder is
      begin
         if not Command_Line then
            Fail_Program
              (Project_Tree,
               Arg & " can only be used on the command line");
         end if;
      end Forbidden_In_Package_Builder;

   begin
      pragma Assert (Arg'First = 1);

      Success := True;

      if Arg'Length = 0 then
         return;
      end if;

      --  If preceding switch was -P, a project file name need to be
      --  specified, not a switch.

      if Project_File_Name_Expected then
         if Arg (1) = '-' then
            Fail_Program
              (Project_Tree, "project file name missing after -P");
         else
            Project_File_Name_Expected := False;
            Project_File_Name := new String'(Arg);
         end if;

         --  If preceding switch was -o, an executable name need to be
         --  specified, not a switch.

      elsif Output_File_Name_Expected then
         if Arg (1) = '-' then
            Fail_Program
              (Project_Tree, "output file name missing after -o");
         else
            Output_File_Name_Expected := False;
            Output_File_Name := new String'(Arg);
         end if;

      elsif Search_Project_Dir_Expected then
         if Arg (1) = '-' then
            Fail_Program
              (Project_Tree, "directory name missing after -aP");
         else
            Search_Project_Dir_Expected := False;
            Prj.Env.Add_Directories (Root_Environment.Project_Path, Arg);
         end if;

      elsif Db_Directory_Expected then
            Db_Directory_Expected := False;
            Parse_Knowledge_Base (Project_Tree, Arg);

         --  Set the processor/language for the following switches

         --  -cargs         all compiler arguments

      elsif Arg = "-cargs" then
         Current_Processor := Compiler;

         if Command_Line then
            Current_Comp_Option_Table := No_Comp_Option_Table;

         else
            Current_Builder_Comp_Option_Table := No_Builder_Comp_Option_Table;
         end if;

         --  -cargs:lang    arguments for compiler of language lang

      elsif Arg'Length > 7 and then Arg (1 .. 7) = "-cargs:" then
         Current_Processor := Compiler;

         Name_Len := 0;
         Add_Str_To_Name_Buffer (Arg (8 .. Arg'Last));
         To_Lower (Name_Buffer (1 .. Name_Len));

         declare
            Lang : constant Name_Id := Name_Find;
         begin
            if Command_Line then
               Current_Comp_Option_Table :=
                 Compiling_Options_HTable.Get (Lang);

               if Current_Comp_Option_Table = No_Comp_Option_Table then
                  Current_Comp_Option_Table := new Compiling_Options.Instance;
                  Compiling_Options_HTable.Set
                    (Lang, Current_Comp_Option_Table);
                  Compiling_Options.Init (Current_Comp_Option_Table.all);
               end if;

            else
               Current_Builder_Comp_Option_Table :=
                 Builder_Compiling_Options_HTable.Get (Lang);

               if Current_Builder_Comp_Option_Table =
                 No_Builder_Comp_Option_Table
               then
                  Current_Builder_Comp_Option_Table :=
                    new Builder_Compiling_Options.Instance;
                  Builder_Compiling_Options_HTable.Set
                    (Lang, Current_Builder_Comp_Option_Table);
                  Builder_Compiling_Options.Init
                    (Current_Builder_Comp_Option_Table.all);
               end if;
            end if;
         end;

         --  -bargs     all binder arguments

      elsif Arg = "-bargs" then
         Current_Processor := Binder;
         Current_Bind_Option_Table := No_Bind_Option_Table;

         --  -bargs:lang    arguments for binder of language lang

      elsif Arg'Length > 7 and then Arg (1 .. 7) = "-bargs:" then
         Current_Processor := Binder;

         Name_Len := 0;
         Add_Str_To_Name_Buffer (Arg (8 .. Arg'Last));
         To_Lower (Name_Buffer (1 .. Name_Len));

         declare
            Lang : constant Name_Id := Name_Find;
         begin
            Current_Bind_Option_Table :=
              Binder_Options_HTable.Get (Lang);

            if Current_Bind_Option_Table = No_Bind_Option_Table then
               Current_Bind_Option_Table := new Binder_Options.Instance;
               Binder_Options_HTable.Set
                 (Lang, Current_Bind_Option_Table);
               Binder_Options.Init (Current_Bind_Option_Table.all);
            end if;
         end;

         --  -largs     linker arguments

      elsif Arg = "-largs" then
         Current_Processor := Linker;

         --  -gargs/margs     options directly for gprbuild
         --  support -margs for compatibility with gnatmake

      elsif Arg = "-gargs"
        or else Arg = "-margs"
      then
         Current_Processor := None;

         --  A special test is needed for the -o switch within a -largs since
         --  that is another way to specify the name of the final executable.

      elsif Command_Line
            and then Current_Processor = Linker
            and then Arg = "-o"
      then
            Fail_Program
           (Project_Tree,
            "switch -o not allowed within a -largs. Use -o directly.");

         --  If current processor is not gprbuild directly, store the option
         --  in the appropriate table.

      elsif Current_Processor /= None then
         Add_Option (Arg, Command_Line);

         --  Switches start with '-'

      elsif Arg (1) = '-' then

         if Arg = "--db-" then
            if Hostparm.OpenVMS then
               Fail_Program
                 (Project_Tree,
                  "--db- cannot be used on VMS");
            end if;

            Forbidden_In_Package_Builder;

            Load_Standard_Base := False;

         elsif Arg = "--db" then
            if Hostparm.OpenVMS then
               Fail_Program
                 (Project_Tree,
                  "--db cannot be used on VMS");
            end if;

            Forbidden_In_Package_Builder;

            Db_Directory_Expected := True;

         elsif Arg = "--display-paths" then
            Forbidden_In_Package_Builder;
            Display_Paths := True;

         elsif Arg = "--no-split-units" then
            Opt.No_Split_Units := True;

         elsif Arg = Single_Compile_Per_Obj_Dir_Switch then
            Opt.One_Compilation_Per_Obj_Dir := True;

         elsif Arg'Length > Source_Info_Option'Length and then
               Arg (1 .. Source_Info_Option'Length) = Source_Info_Option
         then
            Forbidden_In_Package_Builder;
            Project_Tree.Source_Info_File_Name :=
               new String'(Arg (Source_Info_Option'Length + 1 .. Arg'Last));

         elsif Arg'Length > Config_Project_Option'Length
           and then
               Arg (1 .. Config_Project_Option'Length) = Config_Project_Option
         then
            Forbidden_In_Package_Builder;

            if Config_Project_File_Name /= null and then
              (Autoconf_Specified or else
               Config_Project_File_Name.all /=
                 Arg (Config_Project_Option'Length + 1 .. Arg'Last))
            then
               Fail_Program
                 (Project_Tree,
                  "several different configuration switches " &
                  "cannot be specified");

            else
               Autoconfiguration := False;
               Autoconf_Specified := False;
               Config_Project_File_Name :=
                 new String'
                   (Arg (Config_Project_Option'Length + 1 .. Arg'Last));
            end if;

         elsif Arg'Length > Autoconf_Project_Option'Length
           and then
            Arg (1 .. Autoconf_Project_Option'Length) =
              Autoconf_Project_Option
         then
            if Hostparm.OpenVMS then
               Fail_Program
                 (Project_Tree,
                  Autoconf_Project_Option & " cannot be used on VMS");
            end if;

            Forbidden_In_Package_Builder;

            if Config_Project_File_Name /= null and then
              ((not Autoconf_Specified) or else
                Config_Project_File_Name.all /=
                  Arg (Autoconf_Project_Option'Length + 1 .. Arg'Last))
            then
               Fail_Program
                 (Project_Tree,
                  "several different configuration switches " &
                  "cannot be specified");

            else
               Config_Project_File_Name :=
                 new String'
                   (Arg (Autoconf_Project_Option'Length + 1 .. Arg'Last));
               Autoconf_Specified := True;
            end if;

         elsif Arg'Length > Target_Project_Option'Length
           and then
            Arg (1 .. Target_Project_Option'Length) = Target_Project_Option
         then
            if Hostparm.OpenVMS then
               Fail_Program
                 (Project_Tree,
                  Target_Project_Option & " cannot be used on VMS");
            end if;

            Forbidden_In_Package_Builder;

            if Target_Name /= null then
               if Target_Name.all /=
                 Arg (Target_Project_Option'Length + 1 .. Arg'Last)
               then
                  Fail_Program
                    (Project_Tree,
                     "several different target switches cannot be specified");
               end if;

            else
               Target_Name :=
                 new String'
                   (Arg (Target_Project_Option'Length + 1 .. Arg'Last));
            end if;

         elsif Arg'Length > RTS_Option'Length and then
           Arg (1 .. RTS_Option'Length) = RTS_Option
         then
            declare
               Set : constant Boolean := Runtime_Name_Set_For (Name_Ada);
               Old : constant String := Runtime_Name_For (Name_Ada);
               RTS : constant String :=
                        Arg (RTS_Option'Length + 1 .. Arg'Last);
            begin
               if Command_Line then
                  if Set and then Old /= RTS then
                     Fail_Program
                       (Project_Tree,
                        "several different run-times cannot be specified");
                  end if;

                  Set_Runtime_For (Name_Ada, RTS);
               end if;

               --  Ignore any --RTS= switch in package Builder. These are only
               --  taken into account to create the config file in
               --  auto-configuration.
            end;

         elsif Arg'Length > RTS_Language_Option'Length and then
           Arg (1 .. RTS_Language_Option'Length) = RTS_Language_Option
         then
            declare
               Language_Name : Name_Id := No_Name;
               RTS_Start : Natural := Arg'Last + 1;

            begin
               for J in RTS_Language_Option'Length + 2 .. Arg'Last loop
                  if Arg (J) = '=' then
                     Name_Len := 0;
                     Add_Str_To_Name_Buffer
                       (Arg (RTS_Language_Option'Length + 1 .. J - 1));
                     To_Lower (Name_Buffer (1 .. Name_Len));
                     Language_Name := Name_Find;
                     RTS_Start := J + 1;
                     exit;
                  end if;
               end loop;

               if Language_Name = No_Name then
                  Fail_Program (Project_Tree, "illegal switch: " & Arg);

               elsif Command_Line then
                  --  Ignore any --RTS:<lang>= switch in package Builder. These
                  --  are only taken into account to create the config file in
                  --  auto-configuration.

                  declare
                     RTS : constant String := Arg (RTS_Start .. Arg'Last);
                     Set : constant Boolean :=
                       Runtime_Name_Set_For (Language_Name);
                     Old : constant String := Runtime_Name_For (Language_Name);

                  begin
                     if Set and then Old /= RTS then
                        Fail_Program
                          (Project_Tree,
                           "several different run-times cannot be specified" &
                           " for the same language");

                     else
                        Set_Runtime_For (Language_Name, RTS);
                     end if;
                  end;
               end if;
            end;

         elsif Arg'Length > Subdirs_Option'Length and then
               Arg (1 .. Subdirs_Option'Length) = Subdirs_Option
         then
            Forbidden_In_Package_Builder;
            Subdirs :=
              new String'(Arg (Subdirs_Option'Length + 1 .. Arg'Last));

         elsif Arg = Indirect_Imports_Switch then
            Indirect_Imports := True;

            if Command_Line then
               Register_Command_Line_Option (Options.Indirect_Imports, 1);
            end if;

         elsif Arg = No_Indirect_Imports_Switch
               or else
               Arg = Direct_Import_Only_Switch
         then
            Indirect_Imports := False;

            if Command_Line then
               Register_Command_Line_Option (Options.Indirect_Imports, 0);
            end if;

         elsif Arg = Makeutl.Unchecked_Shared_Lib_Imports then
            Forbidden_In_Package_Builder;
            Opt.Unchecked_Shared_Lib_Imports := True;

         elsif Arg = No_Object_Check_Switch then
            Forbidden_In_Package_Builder;
            Object_Checked := False;
            Unique_Compile := True;

         elsif Arg = Create_Map_File_Switch then
            Map_File := new String'("");

         elsif Arg'Length > Create_Map_File_Switch'Length + 1
           and then
             Arg (1 .. Create_Map_File_Switch'Length) = Create_Map_File_Switch
           and then
             Arg (Create_Map_File_Switch'Length + 1) = '='
         then
            Map_File :=
              new String'(Arg (Create_Map_File_Switch'Length + 2 .. Arg'Last));

         elsif Arg'Length >= 3 and then Arg (1 .. 3) = "-aP" then
            Forbidden_In_Package_Builder;

            if Arg'Length = 3 then
               Search_Project_Dir_Expected := True;

            else
               Prj.Env.Add_Directories
                 (Root_Environment.Project_Path, Arg (4 .. Arg'Last));
            end if;

         elsif Arg = "-b" then
            Forbidden_In_Package_Builder;
            Opt.Bind_Only  := True;

         elsif Arg = "-c" then
            Forbidden_In_Package_Builder;
            Opt.Compile_Only := True;

            if Opt.Link_Only then
               Opt.Bind_Only := True;
            end if;

         elsif Arg = "-C" then
            --  This switch is only for upward compatibility

            null;

         elsif Arg = "-d" then
            Opt.Display_Compilation_Progress := True;

         elsif Arg'Length = 3 and then Arg (2) = 'd' then
            if Arg (3) in '1' .. '9' or else
              Arg (3) in 'a' .. 'z' or else
              Arg (3) in 'A' .. 'Z'
            then
               Set_Debug_Flag (Arg (3));

            else
               Fail_Program
                 (Project_Tree, "illegal debug switch " & Arg);
            end if;

         elsif Arg'Length > 3 and then Arg (1 .. 3) = "-eI" then
            Forbidden_In_Package_Builder;

            begin
               Main_Index := Int'Value (Arg (4 .. Arg'Last));

            exception
               when Constraint_Error =>
                  Fail_Program (Project_Tree, "invalid switch " & Arg);
            end;

         elsif Arg = "-eL" then
            Forbidden_In_Package_Builder;
            Opt.Follow_Links_For_Files := True;
            Opt.Follow_Links_For_Dirs  := True;

         elsif Arg = "-eS" then
            Forbidden_In_Package_Builder;

            --  Accept switch for compatibility with gnatmake

            Opt.Commands_To_Stdout := True;

         elsif Arg = "-f" then
            Opt.Force_Compilations := True;

            if Command_Line then
               Register_Command_Line_Option (Force_Compilations_Option);
            end if;

         elsif Arg = "-F" then
            Forbidden_In_Package_Builder;
            Opt.Full_Path_Name_For_Brief_Errors := True;

         elsif Arg = "-h" then
            Forbidden_In_Package_Builder;
            Usage_Needed := True;

         elsif Arg'Length > 2 and then Arg (2) = 'j' then
            declare
               Max_Proc : Natural := 0;
            begin
               for J in 3 .. Arg'Length loop
                  if Arg (J) in '0' .. '9' then
                     Max_Proc := (Max_Proc * 10) +
                       Character'Pos (Arg (J)) -
                       Character'Pos ('0');

                  else
                     Processed := False;
                  end if;
               end loop;

               if Processed then
                  if Max_Proc = 0 then
                     Max_Proc := Natural (Number_Of_CPUs);
                  end if;

                  if Max_Proc = 0 then
                     Max_Proc := 1;
                  end if;

                  Opt.Maximum_Processes := Max_Proc;
               end if;
            end;

            if Processed and then Command_Line then
               Register_Command_Line_Option
                 (Maximum_Processes_Option, Opt.Maximum_Processes);
            end if;

         elsif Arg = "-k" then
            Opt.Keep_Going := True;

            if Command_Line then
               Register_Command_Line_Option (Keep_Going_Option);
            end if;

         elsif Arg = "-l" then
            Forbidden_In_Package_Builder;
            Opt.Link_Only  := True;

            if Opt.Compile_Only then
               Opt.Bind_Only := True;
            end if;

         elsif Arg = "-m" then
            Opt.Minimal_Recompilation := True;

         elsif Arg = "-o" then
            Forbidden_In_Package_Builder;

            if Output_File_Name /= null then
               Fail_Program
                 (Project_Tree, "cannot specify several -o switches");

            else
               Output_File_Name_Expected := True;
            end if;

         elsif Arg = "-p" or else Arg = "--create-missing-dirs" then
            Forbidden_In_Package_Builder;
            Opt.Setup_Projects := True;

         elsif Arg'Length >= 2 and then Arg (2) = 'P' then
            Forbidden_In_Package_Builder;

            if Project_File_Name /= null then
               Fail_Program
                 (Project_Tree,
                  "cannot have several project files specified");

            elsif Arg'Length = 2 then
               Project_File_Name_Expected := True;

            else
               Project_File_Name := new String'(Arg (3 .. Arg'Last));
            end if;

         elsif Arg = "-q" then
            Opt.Quiet_Output := True;
            Opt.Verbose_Mode := False;

            if Command_Line then
               Register_Command_Line_Option (Quiet_Output_Option);
            end if;

         elsif Arg = "-r" then
            Forbidden_In_Package_Builder;
            Recursive := True;

         elsif Arg = "-R" then
            Opt.Run_Path_Option := False;

         elsif Arg = "-s" then
            Opt.Check_Switches := True;

            if Command_Line then
               Register_Command_Line_Option (Check_Switches_Option);
            end if;

         elsif Arg = "-u" then
            Forbidden_In_Package_Builder;
            Unique_Compile := True;

         elsif Arg = "-U" then
            Forbidden_In_Package_Builder;
            Unique_Compile_All_Projects := True;
            Unique_Compile := True;

         elsif Arg = "-v" or else Arg = "-vh" then
            Opt.Verbose_Mode    := True;
            Opt.Verbosity_Level := Opt.High;
            Opt.Quiet_Output    := False;

            if Command_Line then
               Register_Command_Line_Option (Verbose_Mode_Option);
            end if;

         elsif Arg = "-vl" then
            Opt.Verbose_Mode    := True;
            Opt.Verbosity_Level := Opt.Low;
            Opt.Quiet_Output    := False;

            if Command_Line then
               Register_Command_Line_Option (Verbose_Low_Mode_Option);
            end if;

         elsif Arg = "-vm" then
            Opt.Verbose_Mode    := True;
            Opt.Verbosity_Level := Opt.Medium;
            Opt.Quiet_Output    := False;

            if Command_Line then
               Register_Command_Line_Option (Verbose_Medium_Mode_Option);
            end if;

         elsif Arg'Length = 4 and then Arg (1 .. 3) = "-vP" and then
               Arg (4) in '0' .. '2'
         then
            Forbidden_In_Package_Builder;
            case Arg (4) is
               when '0' =>
                  Current_Verbosity := Prj.Default;
               when '1' =>
                  Current_Verbosity := Prj.Medium;
               when '2' =>
                  Current_Verbosity := Prj.High;
               when others =>
                  null;
            end case;

         elsif Arg = "-we" then
            Opt.Warning_Mode := Opt.Treat_As_Error;

            if Command_Line then
               Register_Command_Line_Option (Warnings_Treat_As_Error);
            end if;

         elsif Arg = "-wn" then
            Opt.Warning_Mode := Opt.Normal;

            if Command_Line then
               Register_Command_Line_Option (Warnings_Normal);
            end if;

         elsif Arg = "-ws" then
            Opt.Warning_Mode  := Opt.Suppress;

            if Command_Line then
               Register_Command_Line_Option (Warnings_Suppress);
            end if;

         elsif Arg = "-x" then
            Opt.Use_Include_Path_File := True;

         elsif Arg'Length >= 3
           and then Arg (2) = 'X'
           and then Is_External_Assignment (Root_Environment, Arg)
         then
            Forbidden_In_Package_Builder;

            --  Is_External_Assignment has side effects when it returns True

            null;

         elsif (Language = No_Name or else Language = Name_Ada)
           and then (not Command_Line)
           and then Arg = "-x"
         then
            --  For compatibility with gnatmake, ignore -x if found in the
            --  Builder switches.

            null;

         elsif (Language = No_Name or else Language = Name_Ada)
            and then
             (Arg = "-fstack-check"
              or else Arg = "-fno-inline"
              or else
                (Arg'Length >= 2 and then
                (Arg (2) = 'O' or else Arg (2) = 'g')))
         then
            --  For compatibility with gnatmake, use switch to compile Ada
            --  code.

            if Command_Line then
               Current_Comp_Option_Table :=
                 Compiling_Options_HTable.Get (Name_Ada);

               if Current_Comp_Option_Table = No_Comp_Option_Table then
                  Current_Comp_Option_Table := new Compiling_Options.Instance;
                  Compiling_Options_HTable.Set
                    (Name_Ada, Current_Comp_Option_Table);
                  Compiling_Options.Init (Current_Comp_Option_Table.all);
               end if;

            else
               Current_Builder_Comp_Option_Table :=
                 Builder_Compiling_Options_HTable.Get (Name_Ada);

               if Current_Builder_Comp_Option_Table =
                  No_Builder_Comp_Option_Table
               then
                  Current_Builder_Comp_Option_Table :=
                    new Builder_Compiling_Options.Instance;
                  Builder_Compiling_Options_HTable.Set
                    (Name_Ada, Current_Builder_Comp_Option_Table);
                  Builder_Compiling_Options.Init
                    (Current_Builder_Comp_Option_Table.all);
               end if;
            end if;

            Current_Processor := Compiler;
            Add_Option (Arg, Command_Line);
            Current_Processor := None;

         elsif (Language = No_Name or else Language = Name_Ada)
            and then
             (Arg = "-nostdlib" or else Arg = "-nostdinc")
         then
            --  For compatibility with gnatmake, use switch to bind Ada code
            --  code and for -nostdlib to link.

            Current_Bind_Option_Table :=
              Binder_Options_HTable.Get (Name_Ada);

            if Current_Bind_Option_Table = No_Bind_Option_Table then
               Current_Bind_Option_Table := new Binder_Options.Instance;
               Binder_Options_HTable.Set
                 (Name_Ada, Current_Bind_Option_Table);
               Binder_Options.Init (Current_Bind_Option_Table.all);
            end if;

            Current_Processor := Binder;
            Add_Option (Arg, Command_Line);

            --  For -nostdlib, use the switch to link too

            if Arg = "-nostdlib" then
               Current_Processor := Linker;
               Add_Option (Arg, Command_Line);
            end if;

            Current_Processor := None;

         else
            Processed := False;
         end if;

      elsif Command_Line then
         --  The file name of a main or a project file

         declare
            File_Name : String := Arg;

         begin
            Canonical_Case_File_Name (File_Name);

            if File_Name'Length > Project_File_Extension'Length and then
              File_Name
                (File_Name'Last - Project_File_Extension'Length + 1
                 .. File_Name'Last) = Project_File_Extension
            then
               if Project_File_Name /= null then
                  Fail_Program
                    (Project_Tree,
                     "cannot have several project files specified");

               else
                  Project_File_Name := new String'(File_Name);
               end if;

            else
               --  Not a project file, then it is a main

               Mains.Add_Main (Arg);
               Always_Compile := True;
            end if;
         end;

      else
         Processed := False;
      end if;

      if not Processed then
         if Command_Line then
            Fail_Program
              (Project_Tree,
               "illegal option """ & Arg & """ on the command line");

         else
            --  If we have a switch and there is a Builder Switches language
            --  set, pass this switch to the compiler of the language.

            if Arg (1) = '-' and then Builder_Switches_Lang /= No_Name then
               Current_Builder_Comp_Option_Table :=
                 Builder_Compiling_Options_HTable.Get (Builder_Switches_Lang);

               if Current_Builder_Comp_Option_Table =
                 No_Builder_Comp_Option_Table
               then
                  Current_Builder_Comp_Option_Table :=
                    new Builder_Compiling_Options.Instance;
                  Builder_Compiling_Options_HTable.Set
                    (Builder_Switches_Lang, Current_Builder_Comp_Option_Table);
                  Builder_Compiling_Options.Init
                    (Current_Builder_Comp_Option_Table.all);
               end if;

               Current_Processor := Compiler;
               Add_Option (Arg, False);
               Current_Processor := None;

            else
               Success := False;
            end if;
         end if;
      end if;
   end Scan_Arg;

   ------------------------
   -- Sigint_Intercepted --
   ------------------------

   procedure Sigint_Intercepted is
   begin
      Write_Line ("*** Interrupted ***");
      Delete_All_Temp_Files (Project_Tree.Shared);
      OS_Exit (1);
   end Sigint_Intercepted;

   ---------------------------
   -- Test_If_Relative_Path --
   ---------------------------

   procedure Test_If_Relative_Path
     (Switch           : in out String_Access;
      Parent           : String;
      Including_Switch : Name_Id)
   is
      Original : constant String (1 .. Switch'Length) := Switch.all;

   begin
      if Original (1) = '-' and then Including_Switch /= No_Name then
         declare
            Inc_Switch : constant String := Get_Name_String (Including_Switch);

         begin
            if Original'Last > Inc_Switch'Last and then
              Original (1 .. Inc_Switch'Last) = Inc_Switch and then
              not Is_Absolute_Path
                    (Original (Inc_Switch'Last + 1 .. Original'Last))
            then
                  Switch := new String'
                    (Inc_Switch & Parent & Directory_Separator &
                     Original (Inc_Switch'Last + 1 .. Original'Last));
            end if;
         end;
      end if;

      if Original (1) /= '-' and then not Is_Absolute_Path (Original) then
         Switch := new String'(Parent & Directory_Separator & Original);
      end if;
   end Test_If_Relative_Path;

   -----------
   -- Usage --
   -----------

   procedure Usage is
   begin
      if not Usage_Output then
         Usage_Output := True;

         Write_Str ("Usage: ");
         Osint.Write_Program_Name;
         Write_Str (" [-P<proj>] [<proj>.gpr] [opts] [name]");
         Write_Eol;
         Write_Str ("    {[-cargs opts] [-cargs:lang opts] [-largs opts]" &
                    " [-gargs opts]}");
         Write_Eol;
         Write_Eol;
         Write_Str ("  name is zero or more file names");
         Write_Eol;
         Write_Eol;

         --  GPRBUILD switches

         Write_Str ("gprbuild switches:");
         Write_Eol;

         --  Line for Config_Project_Option

         Write_Str ("  ");
         Write_Str (Config_Project_Option);
         Write_Str ("file.cgpr");
         Write_Eol;
         Write_Str ("           Specify the main config project file name");
         Write_Eol;

         --  Line for Autoconf_Project_Option

         if not Hostparm.OpenVMS then
            Write_Str ("  ");
            Write_Str (Autoconf_Project_Option);
            Write_Str ("file.cgpr");
            Write_Eol;
            Write_Str
              ("           Specify/create the main config project file name");
            Write_Eol;
         end if;

         --  Line for Target_Project_Option

         if not Hostparm.OpenVMS then
            Write_Str ("  ");
            Write_Str (Target_Project_Option);
            Write_Str ("targetname");
            Write_Eol;
            Write_Str
              ("           Specify a target for cross platforms");
            Write_Eol;
         end if;

         --  Line for --db

         if not Hostparm.OpenVMS then
            Write_Str ("  --db dir Parse dir as an additional knowledge base");
            Write_Eol;
         end if;

         --  Line for --db-

         if not Hostparm.OpenVMS then
            Write_Str ("  --db-    Do not load the standard knowledge base");
            Write_Eol;
         end if;

         --  Line for --subdirs=

         Write_Str ("  --subdirs=dir");
         Write_Eol;
         Write_Str ("           Real obj/lib/exec dirs are subdirs");
         Write_Eol;

         --  Line for --single-compile-per-obj-dir

         Write_Str ("  ");
         Write_Str (Single_Compile_Per_Obj_Dir_Switch);
         Write_Eol;
         Write_Str
           ("           No simultaneous compilations for the same obj dir");
         Write_Eol;

         Write_Str ("  ");
         Write_Str (No_Indirect_Imports_Switch);
         Write_Eol;
         Write_Str
           ("           Sources can import only from directly imported " &
            "projects");
         Write_Eol;

         Write_Str ("  ");
         Write_Str (Indirect_Imports_Switch);
         Write_Eol;
         Write_Str
           ("           Sources can import from directly and indirectly " &
            "imported projects");
         Write_Eol;

         Write_Str ("  --RTS=<runtime>");
         Write_Eol;
         Write_Str ("           Use runtime <runtime> for language Ada");
         Write_Eol;

         Write_Str ("  --RTS:<lang>=<runtime>");
         Write_Eol;
         Write_Str ("           Use runtime <runtime> for language <lang>");
         Write_Eol;

         Write_Str ("  ");
         Write_Str (Makeutl.Unchecked_Shared_Lib_Imports);
         Write_Eol;
         Write_Str ("           Shared lib projects may import any project");
         Write_Eol;

         Write_Str ("  ");
         Write_Str (No_Object_Check_Switch);
         Write_Eol;
         Write_Str ("           Do not check object files");
         Write_Eol;
         Write_Eol;

         Write_Str ("  ");
         Write_Str (Create_Map_File_Switch);
         Write_Eol;
         Write_Str ("           Create map file mainprog.map");
         Write_Eol;
         Write_Str ("  ");
         Write_Str (Create_Map_File_Switch);
         Write_Str ("=mapfile");
         Write_Eol;
         Write_Str ("           Create map file mapfile");
         Write_Eol;
         Write_Eol;

         --  Line for -aP

         Write_Str ("  -aP dir  Add directory dir to project search path");
         Write_Eol;

         --  Line for -b

         Write_Str ("  -b       Bind only");
         Write_Eol;

         --  Line for -c

         Write_Str ("  -c       Compile only");
         Write_Eol;

         --  Line for -d

         Write_Str ("  -d       Display progress");
         Write_Eol;

         --  Line for -eInn

         Write_Str ("  -eInn    Index of main unit in multi-unit source file");
         Write_Eol;

         --  Line for -eL

         Write_Str ("  -eL      " &
                    "Follow symbolic links when processing project files");
         Write_Eol;

         --  Line for -eS

         Write_Str ("  -eS      " &
                    "(no action, for compatibility with gnatmake only)");
         Write_Eol;

         --  Line for -f

         Write_Str ("  -f       Force recompilations");
         Write_Eol;

         --  Line for -F

         Write_Str
           ("  -F       Full project path name in brief error messages");
         Write_Eol;

         --  Line for -jnnn

         Write_Str ("  -jnum    Use num processes to compile");
         Write_Eol;

         --  Line for -k

         Write_Str ("  -k       Keep going after compilation errors");
         Write_Eol;

         --  Line for -l

         Write_Str ("  -l       Link only");
         Write_Eol;

         --  Line for -m

         Write_Str ("  -m       Minimum Ada recompilation");
         Write_Eol;

         --  Line for -o

         Write_Str ("  -o name  Choose an alternate executable name");
         Write_Eol;

         --  Line for -p

         Write_Str ("  -p       Create missing obj, lib and exec dirs");
         Write_Eol;

         --  Line for -P

         Write_Str ("  -P proj  Use Project File proj");
         Write_Eol;

         --  Line for -q

         Write_Str ("  -q       Be quiet/terse");
         Write_Eol;

         --  Line for -r

         Write_Str ("  -r       Recursive (default except when using -c)");
         Write_Eol;

         --  Line for -R

         Write_Str ("  -R       Do not use run path option");
         Write_Eol;

         --  Line for -s

         Write_Str ("  -s       Recompile if compiler switches have changed");
         Write_Eol;

         --  Line for -u

         Write_Str
           ("  -u       Unique compilation, only compile the given files");
         Write_Eol;

         --  Line for -U

         Write_Str
           ("  -U       Unique compilation for all sources of all projects");
         Write_Eol;

         --  Line for -v

         Write_Str ("  -v       Verbose output");
         Write_Eol;

         --  Line for -vl

         Write_Str ("  -vl      Verbose output (low verbosity)");
         Write_Eol;

         --  Line for -vm

         Write_Str ("  -vm      Verbose output (medium verbosity)");
         Write_Eol;

         --  Line for -vh

         Write_Str ("  -vh      Verbose output (high verbosity)");
         Write_Eol;

         --  Line for -vPx

         Write_Str ("  -vPx     Specify verbosity when parsing Project Files" &
                    " (x = 0/1/2)");
         Write_Eol;

         --  Line for -we

         Write_Str ("  -we      Treat all warnings as errors");
         Write_Eol;

         --  Line for -wn

         Write_Str ("  -wn      Treat warnings as warnings");
         Write_Eol;

         --  Line for -ws

         Write_Str ("  -ws      Suppress all warnings");
         Write_Eol;

         --  Line for -x

         Write_Str ("  -x       Always create include path file");
         Write_Eol;

         --  Line for -X

         Write_Str ("  -Xnm=val Specify an external reference for " &
                    "Project Files");
         Write_Eol;
         Write_Eol;

         --  Line for -cargs

         Write_Line ("  -cargs opts    opts are passed to all compilers");

         --  Line for -cargs:lang

         Write_Line ("  -cargs:<lang> opts");
         Write_Line ("                 opts are passed to the compiler " &
                     "for language <lang> ");

         --  Line for -bargs

         Write_Line ("  -bargs opts    opts are passed to all binders");

         --  Line for -cargs:lang

         Write_Line ("  -bargs:<lang> opts");
         Write_Line ("                 opts are passed to the binder " &
                     "for language <lang> ");

         --  Line for -largs

         Write_Str ("  -largs opts    opts are passed to the linker");
         Write_Eol;

         --  Line for -gargs

         Write_Str ("  -gargs opts    opts directly interpreted by gprbuild");
         Write_Eol;

         --  Line for -margs

         Write_Str ("  -margs opts    equivalent to -gargs opts");
         Write_Eol;

         Write_Eol;

         Write_Str
           ("For compatibility with gnatmake, these switches are passed " &
            "to the Ada compiler:");
         Write_Eol;

         Write_Str ("  -nostdlib");
         Write_Eol;

         Write_Str ("  -nostdinc");
         Write_Eol;

         Write_Str ("  -fstack-check");
         Write_Eol;

         Write_Str ("  -fno-inline");
         Write_Eol;

         Write_Str ("  -gxxx");
         Write_Eol;

         Write_Str ("  -Oxx");
         Write_Eol;

         Write_Eol;
      end if;
   end Usage;

end Buildgpr;
