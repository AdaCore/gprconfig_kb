------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             B U I L D G P R                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2004-2008, Free Software Foundation, Inc.         --
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
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with ALI;
with Csets;
with Confgpr;   use Confgpr;
with Debug;     use Debug;
with Errout;    use Errout;

with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.Dynamic_Tables;
with GNAT.HTable;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Regexp;               use GNAT.Regexp;

with Gpr_Util;         use Gpr_Util;
with GPR_Version;      use GPR_Version;
with Gprexch;          use Gprexch;
with Hostparm;         use Hostparm;
with Makeutl;          use Makeutl;
with Namet;            use Namet;
with Output;           use Output;
with Opt;              use Opt;
with Osint;            use Osint;
with Prj;              use Prj;
with Prj.Env;
with Prj.Err;
with Prj.Ext;          use Prj.Ext;
with Prj.Proc;         use Prj.Proc;
with Prj.Util;         use Prj.Util;
with Sinput.P;
with Snames;           use Snames;
with Switch;           use Switch;
with System;
with System.Case_Util; use System.Case_Util;
with Table;
with Tempdir;
with Types;            use Types;

package body Buildgpr is

   Object_Suffix : constant String := Get_Target_Object_Suffix.all;
   --  The suffix of object files on this platform

   Maximum_Size : Integer;
   pragma Import (C, Maximum_Size, "__gnat_link_max");
   --  Maximum number of bytes to put in an invocation of the
   --  Archive_Builder.

   Name_Star : Name_Id;
   --  The character '*', initialized in procedure Initialize

   Dash_L : Name_Id;
   --  "-L", initialized in procedure Initialize

   Current_Working_Dir : constant String := Get_Current_Dir;
   --  The current working directory

   Main_Project_Dir : String_Access;
   --  The absolute path of the project directory of the main project,
   --  initialized in procedure Initialize.

   Executable_Suffix : constant String_Access := Get_Executable_Suffix;
   --  The suffix of executables on this platforms

   On_Windows : constant Boolean := Directory_Separator = '\';
   --  True when on Windows. Used in Check_Compilation_Needed when processing
   --  C/C++ dependency files for backslash handling.

   Never : constant Time_Stamp_Type := (others => '9');
   --  A time stamp that is greater than any real one

   Copyright_Output : Boolean := False;
   Usage_Output     : Boolean := False;
   --  Flags to avoid multiple displays of Copyright notice and of Usage

   Usage_Needed : Boolean := False;
   --  Set by swith -h: usage will be displayed after all command line
   --  switches have been scanned.

   Display_Paths : Boolean := False;
   --  Set by switch --display_paths: config project path and user project path
   --  will be displayed af ter all command lineswitches have been scanned.

   Output_File_Name           : String_Access := null;
   --  The name given after a switch -o

   Output_File_Name_Expected  : Boolean := False;
   --  True when last switch was -o

   Project_File_Name_Expected : Boolean := False;
   --  True when last switch was -P

   Search_Project_Dir_Expected : Boolean := False;
   --  True when last switch was -aP

   Direct_Import_Only_Switch : constant String := "--direct-import-only";
   Direct_Import_Only : Boolean := False;
   --  True when switch --direct-import-only is used. Sources are only
   --  allowed to import from the projects that are directly withed.

   Recursive : Boolean := False;

   Closure_Needed : Boolean := False;

   Unique_Compile : Boolean := False;
   --  Set to True if -u or -U or a project file with no main is used

   Unique_Compile_All_Projects : Boolean := False;
   --  Set to True if -U is used

   Naming_String                : aliased String := "naming";
   Builder_String               : aliased String := "builder";
   Compiler_String              : aliased String := "compiler";
   Binder_String                : aliased String := "binder";
   Linker_String                : aliased String := "linker";
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

   package Bad_Compilations is new Table.Table (
     Table_Component_Type => Source_Id,
     Table_Index_Type     => Natural,
     Table_Low_Bound      => 1,
     Table_Initial        => 20,
     Table_Increment      => 100,
     Table_Name           => "Makegpr.Bad_Compilations");
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
         Warnings_Treat_As_Error,
         Warnings_Normal,
         Warnings_Suppress);

      All_Phases   : Boolean := True;
      --  True when all phases (compilation, binding and linking) are to be
      --  performed.

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
      Pid           : Process_Id         := Invalid_Pid;
      Source        : Source_Id          := No_Source;
      Mapping_File  : Path_Name_Type     := No_Path;
      Purpose       : Process_Purpose    := Compilation;
      Options       : String_List_Access := null;
   end record;
   --  Data recorded for each spawned jobs, compilation of dependency file
   --  building.

   No_Process_Data : constant Process_Data :=
                           (Pid          => Invalid_Pid,
                            Source       => No_Source,
                            Mapping_File => No_Path,
                            Purpose      => Compilation,
                            Options      => null);

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

   Initial_Number_Of_Options : constant := 10;

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

   Include_Options : Options_Data;
   --  The options to indicate the directories where to find sources or
   --  templates.

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

   package Main_Sources is new GNAT.HTable.Simple_HTable
     (Header_Num => Prj.Header_Num,
      Element    => Source_Id,
      No_Element => No_Source,
      Key        => File_Name_Type,
      Hash       => Prj.Hash,
      Equal      => "=");
   --  A hash table to store the Source_Id of the mains.

   type Root_Array is array (Positive range <>) of Source_Id;
   type Roots_Access is access Root_Array;
   No_Roots : constant Roots_Access := null;

   Roots_Buffer : Roots_Access := new Root_Array (1 .. 4);
   --  An extensible array to store the roots when they are computed

   package Root_Sources is new GNAT.HTable.Simple_HTable
     (Header_Num => Prj.Header_Num,
      Element    => Roots_Access,
      No_Element => No_Roots,
      Key        => File_Name_Type,
      Hash       => Prj.Hash,
      Equal      => "=");
   --  A hash table to store the Roots of the mains, if any.

   Project_Of_Current_Object_Directory : Project_Id := No_Project;
   --  The object directory of the project for the last binding. Avoid
   --  calling Change_Dir if the current working directory is already this
   --  directory.

   package Global_Archives_Built is new GNAT.HTable.Simple_HTable
     (Header_Num => Prj.Header_Num,
      Element    => Boolean,
      No_Element => False,
      Key        => Name_Id,
      Hash       => Prj.Hash,
      Equal      => "=");
   --  A hash table to record what global archives have been already built

   Need_To_Rebuild_Global_Archives : Boolean := False;

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
      Table_Name           => "Makegpr.Cache_Args");
   --  A table to cache arguments, to avoid multiple allocation of the same
   --  strings. It is not possible to use a hash table, because String is
   --  an unconstrained type.

   package Directories is new Table.Table
     (Table_Component_Type => Path_Name_Type,
      Table_Index_Type     => Integer,
      Table_Low_Bound      => 1,
      Table_Initial        => 200,
      Table_Increment      => 100,
      Table_Name           => "Makegpr.Directories");
   --  Table of all the source directories

   --  The buffer to build a path

   Path_Buffer : String_Access := null;
   Path_Buffer_Initial_Length : constant := 1_024;
   Path_Last   : Natural := 0;

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
      Table_Name           => "Make.Library_Objs");
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
     Table_Name           => "Make.Library_Projs");
   --  Library projects imported directly or indirectly

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

   -----------
   -- Queue --
   -----------

   package Queue is

      --  The queue of sources to be checked for compilation

      procedure Init;
      --  Initialize the queue

      procedure Insert
        (Source_File_Name : File_Name_Type;
         Source_Identity  : Source_Id;
         Source_Project   : Project_Id);
      --  Insert a new source in the the queue

      function Is_Empty return Boolean;
      --  Returns True if the queue is empty

      procedure Extract
        (Source_File_Name : out File_Name_Type;
         Source_Identity  : out Source_Id;
         Source_Project   : out Project_Id);
      --  Get the first source from the queue

      procedure Insert_Project_Sources
        (The_Project  : Project_Id;
         All_Projects : Boolean;
         Unit_Based   : Boolean);
      --  Insert the sources from The_Project and, if All_Projects is True,
      --  from all the projects it imports directly or indirectly.

      function Size return Natural;
      --  Return the total size of the queue, including the sources already
      --  extracted.

      function First return Natural;
      --  Return the rank in the queue of the first source not already
      --  extracted.

   end Queue;

   type Sigint_Handler is access procedure;
   pragma Convention (C, Sigint_Handler);

   procedure Install_Int_Handler (Handler : Sigint_Handler);
   pragma Import (C, Install_Int_Handler, "__gnat_install_int_handler");
   --  Called by Gnatmake to install the SIGINT handler below

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

   procedure Add_Option
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
   procedure Add_Options
     (Value         : String_List;
      To            : in out Options_Data;
      Display_All   : Boolean;
      Display_First : Boolean;
      Simple_Name   : Boolean := False);
   --  Add one or several options to a list of options. Increase the size
   --  of the list, if necessary.

   procedure Add_Process
     (Pid          : Process_Id;
      Source       : Source_Id;
      Mapping_File : Path_Name_Type;
      Purpose      : Process_Purpose;
      Options      : String_List_Access);
   --  Record a compiling process

   procedure Add_Rpath (Path : String);
   --  Add a path name to Rpath

   procedure Add_Source_Id (Project : Project_Id; Id : Source_Id);
   --  Add a source id to Source_Indexes, with Found set to False

   procedure Add_To_Path (C : Character);
   procedure Add_To_Path (S : String);
   --  Add to Path_Buffer, incrementing Path_Last

   procedure Await_Compile (Source : out Source_Id; OK : out Boolean);
   --  Wait for a compiling process to finish

   procedure Binding_Phase;
   --  Perform binding, if needed

   procedure Build_Global_Archive
     (For_Project    : Project_Id;
      Has_Been_Built : out Boolean);
   --  Build, if necessary, the global archive for a main project.
   --  Out parameter Has_Been_Built is True iff the global archive has been
   --  built/rebuilt.

   procedure Build_Library (For_Project : Project_Id);
   --  Build, if necessary, the library of a library project

   function Canonical_Cased_File_Name (Name : String) return String;
   --  Returns its parameter in canonical case

   procedure Change_To_Object_Directory (Project : Project_Id);
   --  Change to the object directory of project Project, if this is not
   --  already the current working directory.

   procedure Check_Archive_Builder;
   --  Check if the archive builder (ar) is there

   procedure Check_Mains;
   --  Check that each main is a single file name and that it is a source
   --  of a project from the tree.

   procedure Compilation_Phase;
   --  Perform compilations

   procedure Compute_All_Imported_Projects (Project : Project_Id);
   --  Compute, the list of the projects imported directly or indirectly by
   --  project Project.

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

   procedure Get_Mains;
   --  If no mains were specified on the command line, get the mains specified
   --  by attribute Mains in the main project and check if they are sources of
   --  the main project.

   function Get_Option (Option : Name_Id) return String_Access;
   --  Get a string access corresponding to Option. Either find the string
   --  access in the All_Options cache, or create a new entry in All_Options.

   type Name_Ids is array (Positive range <>) of Name_Id;
   No_Names : constant Name_Ids := (1 .. 0 => No_Name);
   --  Name_Ids is used for list of language names in procedure Get_Directories
   --  below.

   procedure Get_Directories
     (For_Project : Project_Id;
      Sources     : Boolean;
      Languages   : Name_Ids);
   --  Put in table Directories the source (when Sources is True) or
   --  object/library (when Sources is False) directories of project
   --  For_Project and of all the project it imports directly or indirectly.
   --  The source directories of imported projects are only included if one
   --  of the declared languages is in the list Languages.

   function Global_Archive_Name (For_Project : Project_Id) return String;
   --  Returns the name of the global archive for a project

   procedure Initialize;
   --  Do the necessary package initialization and process the command line
   --  arguments.

   procedure Initialize_Source_Record
     (Source      : Source_Id;
      Need_Object : Boolean := False);
   --  Get the different components of a source record: object and dependency
   --  file and path names and, if they exist, their time stamps.

   function Is_Included_In_Global_Archive
     (Object_Name : File_Name_Type;
      Project     : Project_Id) return Boolean;
   --  Return True if the object Object_Name is not overridden by a source
   --  in a project extending project Project.

   function Is_Subunit (Source : Source_Data) return Boolean;
   --  Return True if source is a subunit

   procedure Linking_Phase;
   --  Perform linking, if necessary

   function Need_To_Compile (Source : Source_Id) return Boolean;
   --  Check if a source need to be compiled.
   --  A source need to be compiled if:
   --    - Force_Compilations is True
   --    - No object file generated for the language
   --    - Object file does not exist
   --    - Dependency file does not exist
   --    - Switches file does not exist
   --    - Either of these 3 files are older than the source or any source it
   --      depends on.

   procedure Process_Imported_Libraries
     (For_Project        : Project_Id;
      And_Project_Itself : Boolean := False);
   --  Get the imported library project ids in table Library_Projs

   function Project_Extends
     (Extending : Project_Id;
      Extended  : Project_Id) return Boolean;
   --  Returns True if Extending is Extended or is extending Extended directly
   --  or indirectly.

   procedure Record_Failure (Source : Source_Id);
   --  Record that compilation of a source failed

   procedure Scan_Arg (Arg : String; Command_Line : Boolean);
   --  Process one command line argument

   procedure Recursive_Import (Project : Project_Id);
   --  Add to table Imports the projects imported by Project, recursively

   procedure Sigint_Intercepted;
   pragma Convention (C, Sigint_Intercepted);
   --  Called when the program is interrupted by Ctrl-C to delete the
   --  temporary mapping files and configuration pragmas files.

   function Source_Of (File : String) return Source_Data;
   --  Return the source data corresponding to a file name

   procedure Test_If_Relative_Path
     (Switch           : in out String_Access;
      Parent           : String;
      Including_Switch : Name_Id);

   function Ultimate_Extending_Project_Of (Proj : Project_Id)
                                           return Project_Id;
   --  Returns the ultimate extending project of project Proj. If project Proj
   --  is not extended, returns Proj.

   procedure Usage;
   --  Display the usage

   procedure Check_Version_And_Help is new
     Check_Version_And_Help_G (Usage);

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

      if Arg /= null or else Arg'Length = 0 then

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
      Src_Data : constant Source_Data := Project_Tree.Sources.Table (Source);
      Package_Compiler : constant Package_Id :=
                           Value_Of
                             (Name        => Name_Compiler,
                              In_Packages => Project_Tree.Projects.Table
                                (Src_Data.Project).Decl.Packages,
                              In_Tree     => Project_Tree);

      Options          : Variable_Value :=
                           Value_Of
                             (Name                    =>
                                                     Name_Id (Src_Data.File),
                              Attribute_Or_Array_Name => Name_Switches,
                              In_Package              => Package_Compiler,
                              In_Tree                 => Project_Tree);

   begin
      if Options = Nil_Variable_Value then
         Options :=
           Value_Of
             (Name                    => Src_Data.Language_Name,
              Attribute_Or_Array_Name => Name_Switches,
              In_Package              => Package_Compiler,
              In_Tree                 => Project_Tree,
              Force_Lower_Case_Index  => True);
      end if;

      if Options = Nil_Variable_Value then
         Options :=
           Value_Of
             (Name                    => Src_Data.Language_Name,
              Attribute_Or_Array_Name => Name_Default_Switches,
              In_Package              => Package_Compiler,
              In_Tree                 => Project_Tree);
      end if;

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
     (Value       : String_Access;
      To          : in out Options_Data;
      Display     : Boolean;
      Simple_Name : Boolean := False)
   is
   begin
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
   end Add_Option;

   procedure Add_Option
     (Value       : String;
      To          : in out Options_Data;
      Display     : Boolean;
      Simple_Name : Boolean := False)
   is
   begin
      Add_Option (new String'(Value), To, Display, Simple_Name);
   end Add_Option;

   procedure Add_Option
     (Value       : Name_Id;
      To          : in out Options_Data;
      Display     : Boolean;
      Simple_Name : Boolean := False)
   is
   begin
      Add_Option
        (new String'(Get_Name_String (Value)), To, Display, Simple_Name);
   end Add_Option;

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
         Element := Project_Tree.String_Elements.Table (List);
         Option := Get_Option (Element.Value);

         if Option'Length > 0 then
            Add_Option
              (Value       => Option,
               To          => To,
               Display     => Display_All or First_Display,
               Simple_Name => Simple_Name);
            First_Display := False;
         end if;

         List := Element.Next;
      end loop;
   end Add_Options;

   procedure Add_Options
     (Value         : String_List;
      To            : in out Options_Data;
      Display_All   : Boolean;
      Display_First : Boolean;
      Simple_Name   : Boolean := False)
   is
      First_Display : Boolean := Display_First;
   begin
      for J in Value'Range loop
         if Value (J)'Length > 0 then
            Add_Option
              (Value       => Value (J),
               To          => To,
               Display     => Display_All or else First_Display,
               Simple_Name => Simple_Name);
            First_Display := False;
         end if;
      end loop;
   end Add_Options;

   -----------------
   -- Add_Process --
   -----------------

   procedure Add_Process
     (Pid          : Process_Id;
      Source       : Source_Id;
      Mapping_File : Path_Name_Type;
      Purpose      : Process_Purpose;
      Options      : String_List_Access)
   is
   begin
      Compilation_Htable.Set
        (Pid, (Pid, Source, Mapping_File, Purpose, Options));
      Outstanding_Compiles := Outstanding_Compiles + 1;
   end Add_Process;

   ---------------
   -- Add_Rpath --
   ---------------

   procedure Add_Rpath (Path : String) is
   begin
      --  Nothing to do if the directory is already in the Rpaths table
      for J in 1 .. Rpaths.Last loop
         if Rpaths.Table (J).all = Path then
            return;
         end if;
      end loop;

      Rpaths.Append (new String'(Path));
   end Add_Rpath;

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

   -----------------
   -- Add_To_Path --
   -----------------

   procedure Add_To_Path (C : Character) is
   begin
      if Path_Last = Path_Buffer'Last then
         declare
            New_Path_Buffer : constant String_Access :=
                                new String (1 .. 2 * Path_Buffer'Last);
         begin
            New_Path_Buffer (Path_Buffer'Range) := Path_Buffer.all;
            Free (Path_Buffer);
            Path_Buffer := New_Path_Buffer;
         end;
      end if;

      Path_Last := Path_Last + 1;
      Path_Buffer (Path_Last) := C;
   end Add_To_Path;

   procedure Add_To_Path (S : String) is
   begin
      for J in S'Range loop
         Add_To_Path (S (J));
      end loop;
   end Add_To_Path;

   -------------------
   -- Await_Compile --
   -------------------

   procedure Await_Compile (Source : out Source_Id; OK : out Boolean) is
      Pid : Process_Id;
      Comp_Data : Process_Data;

      Language  : Language_Index;

      Config : Language_Config;

   begin
      loop
         Source := No_Source;

         Wait_Process (Pid, OK);

         if Pid = Invalid_Pid then
            return;
         end if;

         Comp_Data := Compilation_Htable.Get (Pid);

         if Comp_Data /= No_Process_Data then
            Source := Comp_Data.Source;

            if Comp_Data.Purpose = Compilation then

               --  Update the time stamps of the object file and of the
               --  dependency file if the compilation was successful.

               if OK then
                  Project_Tree.Sources.Table (Source).Object_TS :=
                    File_Stamp
                      (Project_Tree.Sources.Table (Source).Object_Path);
                  Project_Tree.Sources.Table (Source).Dep_TS :=
                    File_Stamp
                      (Project_Tree.Sources.Table (Source).Dep_Path);

                  if Comp_Data.Options /= null then
                     --  Write the switches file, now that we have the updated
                     --  time stamp for the object file.

                     declare
                        File : Ada.Text_IO.File_Type;

                     begin
                        Create
                          (File,
                           Out_File,
                           Get_Name_String
                             (Project_Tree.Sources.Table
                                (Source).Switches_Path));

                        Put_Line
                          (File,
                           String
                             (Project_Tree.Sources.Table (Source).Object_TS));

                        for J in Comp_Data.Options'Range loop
                           Put_Line (File, Comp_Data.Options (J).all);
                        end loop;

                        Close (File);
                     end;
                  end if;
               end if;

               Language := Project_Tree.Sources.Table (Source).Language;

               --  If there is a mapping file used, recycle it in the hash
               --  table of the language.

               if Comp_Data.Mapping_File /= No_Path and then
                 Language /= No_Language_Index
               then
                  Mapping_Files_Htable.Set
                    (T => Project_Tree.Languages_Data.Table
                       (Language).Mapping_Files,
                     K => Comp_Data.Mapping_File,
                     E => Comp_Data.Mapping_File);
               end if;

               Config := Project_Tree.Languages_Data.Table (Language).Config;

               if Config.Dependency_Kind = Makefile and then
                 Config.Compute_Dependency /= No_Name_List
               then
                  declare
                     List : Name_List_Index := Config.Compute_Dependency;
                     Nam : Name_Node :=
                              Project_Tree.Name_Lists.Table (List);
                     Exec_Name : constant String := Get_Name_String (Nam.Name);
                     Exec_Path : String_Access;
                  begin
                     Comp_Data.Mapping_File := No_Path;
                     Comp_Data.Purpose := Dependency;

                     Exec_Path := Locate_Exec_On_Path (Exec_Name);

                     if Exec_Path = null then
                        Fail_Program
                          ("unable to find dependency builder ",
                           Exec_Name);
                     end if;

                     List := Nam.Next;
                     Compilation_Options.Last := 0;

                     if List = No_Name_List then
                        Name_Len := 0;

                     else
                        loop
                           Nam := Project_Tree.Name_Lists.Table (List);
                           List := Nam.Next;

                           if List = No_Name_List then
                              Get_Name_String (Nam.Name);
                              exit;
                           end if;

                           Add_Option
                             (Nam.Name,
                              Compilation_Options,
                              Opt.Verbose_Mode);
                        end loop;
                     end if;

                     Add_Str_To_Name_Buffer
                       (Get_Name_String
                          (Project_Tree.Sources.Table (Source).Display_Path));
                     Add_Option
                       (Name_Buffer (1 .. Name_Len),
                        Compilation_Options,
                        Opt.Verbose_Mode,
                        Simple_Name => not Verbose_Mode);

                     if not Quiet_Output then
                        if Verbose_Mode then
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
                          Output_File  =>
                            Get_Name_String
                              (Project_Tree.Sources.Table (Source).Dep_Path),
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

   -------------------
   -- Binding_Phase --
   -------------------

   procedure Binding_Phase is
      Success              : Boolean;

      Exchange_File        : Ada.Text_IO.File_Type;
      Line                 : String (1 .. 1_000);
      Last                 : Natural;

      Proj_List            : Project_List;
      Proj_Element         : Project_Element;

      Shared_Libs          : Boolean := False;

      Bind_Exchange_TS     : Time_Stamp_Type;
      Bind_Object_TS       : Time_Stamp_Type;
      Binder_Driver_Needs_To_Be_Called : Boolean := False;

      Project_Path         : Name_Id;
      Project_File_TS      : Time_Stamp_Type;

      procedure Add_Dependency_Files
        (For_Project : Project_Id;
         Language    : Language_Index;
         Lang_Name   : Name_Id;
         Main_Source : Source_Data;
         Dep_Files   : out Boolean);
      --  Put the dependency files of the project in the binder exchange file

      procedure Check_Dependency_Files
        (For_Project  : Project_Id;
         For_Language : Name_Id);

      --------------------------
      -- Add_Dependency_Files --
      --------------------------

      procedure Add_Dependency_Files
        (For_Project : Project_Id;
         Language    : Language_Index;
         Lang_Name   : Name_Id;
         Main_Source : Source_Data;
         Dep_Files   : out Boolean)
      is
         Src_Id  : Source_Id;
         Source  : Source_Data;
         Config  : constant Language_Config :=
                     Project_Tree.Languages_Data.Table (Language).Config;
         Roots   : Roots_Access;

         procedure Put_Dependency_File;
         --  Put in the exchange file the dependency file path name for source
         --  Source, if applicable.

         -------------------------
         -- Put_Dependency_File --
         -------------------------

         procedure Put_Dependency_File is
            Local_Data : Project_Data;
         begin
            if Source.Language_Name = Lang_Name
              and then
                ((Config.Kind = File_Based and then Source.Kind = Impl)
                 or else
                   (Config.Kind = Unit_Based
                    and then
                      Source.Unit /= No_Name
                    and then
                      Source.Unit /= Main_Source.Unit
                    and then
                      (Source.Kind = Impl
                       or else
                         Source.Other_Part = No_Source)
                    and then
                      (not Is_Subunit (Source))))
              and then Is_Included_In_Global_Archive
                (Source.Object, Source.Project)
            then
               Initialize_Source_Record (Src_Id);
               Source := Project_Tree.Sources.Table (Src_Id);
               Local_Data := Project_Tree.Projects.Table (Source.Project);

               if (Source.Project = For_Project) or
                  (not Local_Data.Library) or
                  Config.Kind = File_Based
               then
                  Put_Line
                    (Exchange_File,
                     Get_Name_String (Source.Dep_Path));
                  Dep_Files := True;

               elsif not Local_Data.Standalone_Library then
                  Get_Name_String (Local_Data.Library_ALI_Dir);
                  Add_Char_To_Name_Buffer (Directory_Separator);
                  Get_Name_String_And_Append (Source.Dep_Name);
                  Put_Line (Exchange_File, Name_Buffer (1 .. Name_Len));
                  Dep_Files := True;
               end if;
            end if;
         end Put_Dependency_File;

      begin
         Dep_Files := False;

         Roots := Root_Sources.Get (Main_Source.File);

         if Roots = No_Roots then
            if Main_Source.Unit = No_Name then
               Src_Id := Project_Tree.First_Source;
               while Src_Id /= No_Source loop
                  Initialize_Source_Record (Src_Id);
                  Source := Project_Tree.Sources.Table (Src_Id);
                  Put_Dependency_File;
                  Src_Id := Source.Next_In_Sources;
               end loop;
            end if;

         else

            --  Put the Roots
            for J in Roots'Range loop
               Src_Id := Roots (J);
               Initialize_Source_Record (Src_Id);
               Source := Project_Tree.Sources.Table (Src_Id);
               Put_Dependency_File;
            end loop;
         end if;
      end Add_Dependency_Files;

      ----------------------------
      -- Check_Dependency_Files --
      ----------------------------

      procedure Check_Dependency_Files
        (For_Project  : Project_Id;
         For_Language : Name_Id)
      is
         Data    : Project_Data;
         Src_Id  : Source_Id;
         Source  : Source_Data;

         Dep_TS  : Time_Stamp_Type;

      begin
         if For_Project /= No_Project and then
           (not Binder_Driver_Needs_To_Be_Called) and then
           (not Project_Tree.Projects.Table (For_Project).Seen)
         then
            Project_Tree.Projects.Table (For_Project).Seen := True;
            Data := Project_Tree.Projects.Table (For_Project);

            Src_Id := Data.First_Source;

            while Src_Id /= No_Source loop
               Source := Project_Tree.Sources.Table (Src_Id);

               if Source.Language_Name = For_Language
                 and then
                   Source.Unit /= No_Name
                 and then
                   (Source.Kind = Impl
                    or else
                    Source.Other_Part = No_Source)
                 and then
                   (not Is_Subunit (Source))
                 and then
                   Is_Included_In_Global_Archive
                     (Source.Object, Source.Project)
               then
                  Initialize_Source_Record (Src_Id);
                  Source := Project_Tree.Sources.Table (Src_Id);

                  if (not Data.Library) or Source.Unit = No_Name then
                     Dep_TS := Source.Dep_TS;

                  else
                     Get_Name_String (Data.Library_ALI_Dir);
                     Add_Char_To_Name_Buffer (Directory_Separator);
                     Get_Name_String_And_Append (Source.Dep_Name);
                     Dep_TS := File_Stamp (Path_Name_Type'(Name_Find));
                  end if;

                  if Dep_TS = Empty_Time_Stamp
                    or else
                    String (Dep_TS) > String (Bind_Exchange_TS)
                    or else
                    String (Dep_TS) > String (Bind_Object_TS)
                  then
                     Binder_Driver_Needs_To_Be_Called := True;

                     if Verbose_Mode then
                        Write_Str ("      -> ");
                        Write_Str (Get_Name_String (Source.Dep_Name));

                        if Dep_TS = Empty_Time_Stamp then
                           Write_Line (" does not exist");

                        elsif String (Dep_TS) > String (Bind_Exchange_TS) then
                           Write_Line
                             (" is more recent than the binder exchange file");

                        else
                           Write_Line
                             (" is more recent that the binder generated " &
                              "object file");
                        end if;
                     end if;

                     exit;
                  end if;
               end if;

               Src_Id := Source.Next_In_Project;
            end loop;
         end if;
      end Check_Dependency_Files;

   begin
      --  Build the libraries, if any

      --  First, get the libraries in building order in table Library_Projs

      Process_Imported_Libraries (Main_Project, And_Project_Itself => True);

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

               if Project_Tree.Projects.Table
                    (Proj).Extended_By = No_Project
               then
                  if not Project_Tree.Projects.Table
                           (Proj).Externally_Built
                  then
                     Build_Library (Proj);
                  end if;

                  if Project_Tree.Projects.Table (Proj).Library_Kind /=
                     Static
                  then
                     Shared_Libs := True;
                  end if;
               end if;
            end loop;
         end;
      end if;

      --  Check if there is a need to call a binder driver

      There_Are_Binder_Drivers := False;
      Find_Binding_Languages;

      Mains.Reset;

      --  If no main is specified, there is nothing else to do

      if Mains.Number_Of_Mains = 0 then
         return;
      end if;

      loop
         declare
            Display_Main   : constant String := Mains.Next_Main;

         begin
            exit when Display_Main'Length = 0;

            declare
               Main           : constant String :=
                                  Canonical_Cased_File_Name (Display_Main);
               Main_Id        : constant File_Name_Type := Create_Name (Main);
               Main_Source_Id : constant Source_Id :=
                                  Main_Sources.Get (Main_Id);
               Main_Source    : Source_Data;

               Bind_Exchange  : String_Access;
               Main_Proj      : Project_Id;
               B_Data         : Binding_Data;
               Main_Base_Name : File_Name_Type;

               Options_Instance : Bind_Option_Table_Ref :=
                                    No_Bind_Option_Table;

               Dep_Files      : Boolean;

            begin
               Initialize_Source_Record (Main_Source_Id);
               Main_Source := Project_Tree.Sources.Table (Main_Source_Id);
               Main_Proj   :=
                 Ultimate_Extending_Project_Of (Main_Source.Project);

               --  Get the main base name

               Name_Len := 0;
               Add_Str_To_Name_Buffer (Main);

               for J in reverse 2 .. Name_Len loop
                  if Name_Buffer (J) = '.' then
                     Name_Len := J - 1;
                     exit;
                  end if;
               end loop;

               Main_Base_Name := Name_Find;

               Change_To_Object_Directory (Main_Proj);

               if There_Are_Binder_Drivers then
                  for B_Index in 1 .. Binding_Languages.Last loop
                     B_Data := Binding_Languages.Table (B_Index);

                     Binder_Driver_Needs_To_Be_Called := Force_Compilations;

                     --  First check if the binder driver needs to be called.
                     --  It needs to be called if
                     --    1) there is no existing binder exchange file
                     --    2) there is no binder generated object file
                     --    3) there is a dependency file of the language that
                     --       is more recent than any of these two files

                     if (not Binder_Driver_Needs_To_Be_Called) and then
                       Verbose_Mode
                     then
                        Write_Line
                          ("   Checking binder generated files for " &
                           Display_Main &
                           "...");
                     end if;

                     Bind_Exchange :=
                       Binder_Exchange_File_Name
                         (Main_Base_Name, B_Data.Binder_Prefix);
                     Bind_Exchange_TS :=
                       File_Stamp
                         (Path_Name_Type'(Create_Name (Bind_Exchange.all)));

                     if not Binder_Driver_Needs_To_Be_Called then
                        if Bind_Exchange_TS = Empty_Time_Stamp then
                           Binder_Driver_Needs_To_Be_Called := True;

                           if Verbose_Mode then
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

                                 if Verbose_Mode then
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

                              if Verbose_Mode then
                                 Write_Line
                                   ("      -> previous gprbind failed, or " &
                                    Bind_Exchange.all &
                                    " corrupted");
                              end if;
                        end;
                     end if;

                     if not Binder_Driver_Needs_To_Be_Called then
                        if Line (1 .. Last) /=
                          Binding_Label (Generated_Object_File)
                          or else End_Of_File (Exchange_File)
                        then
                           Binder_Driver_Needs_To_Be_Called := True;

                           if Verbose_Mode then
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

                              if Verbose_Mode then
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
                           if Verbose_Mode then
                              Write_Line
                                ("      -> previous gprbind failed, or " &
                                 Bind_Exchange.all &
                                 " corrupted");
                           end if;

                        else

                           --  Populate the hash table Project_File_Paths with
                           --  the paths of all project files in the closure
                           --  of the main project.

                           Project_File_Paths.Reset;

                           Project_File_Paths.Set
                             (Name_Id (Project_Tree.Projects.Table
                                         (Main_Proj).Path_Name),
                              True);

                           Proj_List :=
                             Project_Tree.Projects.Table
                               (Main_Proj).All_Imported_Projects;

                           while Proj_List /= Empty_Project_List loop
                              Proj_Element :=
                                Project_Tree.Project_Lists.Table (Proj_List);
                              Project_File_Paths.Set
                                (Name_Id (Project_Tree.Projects.Table
                                           (Proj_Element.Project).Path_Name),
                                 True);
                              Proj_List := Proj_Element.Next;
                           end loop;

                           --  Get the project file paths from the exchange
                           --  file and check if they are the expected project
                           --  files with the same time stamps.

                           while not End_Of_File (Exchange_File) loop
                              Get_Line (Exchange_File, Name_Buffer, Name_Len);
                              exit when
                                Name_Len > 0 and then Name_Buffer (1) = '[';

                              if End_Of_File (Exchange_File) then
                                 Binder_Driver_Needs_To_Be_Called := True;

                                 if Verbose_Mode then
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

                                 if String (Project_File_TS) /=
                                   Line (1 .. Last)
                                 then
                                    Binder_Driver_Needs_To_Be_Called := True;

                                    if Verbose_Mode then
                                       Write_Line
                                         ("      -> project file " &
                                          Get_Name_String (Project_Path) &
                                          " has been modified");
                                    end if;

                                    exit;
                                 end if;

                              else
                                 Binder_Driver_Needs_To_Be_Called := True;

                                 if Verbose_Mode then
                                    Write_Line
                                      ("      -> unknown project file " &
                                       Get_Name_String (Project_Path));
                                 end if;

                                 exit;
                              end if;
                           end loop;

                           --  Check if there are still project file paths in
                           --  the has table.

                           if (not Binder_Driver_Needs_To_Be_Called) and then
                             Project_File_Paths.Get_First
                           then
                              Binder_Driver_Needs_To_Be_Called := True;

                              if Verbose_Mode then
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

                        --  Reset the Seen flag for all projects, so that all
                        --  dependency files are check once.

                        for Index in
                          1 .. Project_Table.Last (Project_Tree.Projects)
                        loop
                           Project_Tree.Projects.Table (Index).Seen := False;
                        end loop;

                        Check_Dependency_Files
                          (Main_Proj, B_Data.Language_Name);

                        Proj_List :=
                          Project_Tree.Projects.Table
                            (Main_Proj).All_Imported_Projects;

                        while (not Binder_Driver_Needs_To_Be_Called) and then
                        Proj_List /= Empty_Project_List
                        loop
                           Proj_Element :=
                             Project_Tree.Project_Lists.Table (Proj_List);
                           Check_Dependency_Files
                             (Proj_Element.Project, B_Data.Language_Name);
                           Proj_List := Proj_Element.Next;
                        end loop;
                     end if;

                     if not Binder_Driver_Needs_To_Be_Called then
                        if Verbose_Mode then
                           Write_Line ("      -> up to date");
                        end if;

                     else
                        Create (Exchange_File, Out_File, Bind_Exchange.all);

                        --  Optional line: Quiet or Verbose

                        if Quiet_Output then
                           Put_Line (Exchange_File, Binding_Label (Quiet));

                        elsif Verbose_Mode then
                           Put_Line (Exchange_File, Binding_Label (Verbose));
                        end if;

                        --  Optional line: shared libs
                        if Shared_Libs then
                           Put_Line
                             (Exchange_File,
                              Binding_Label (Gprexch.Shared_Libs));
                        end if;

                        --  First, the main base name

                        Put_Line
                          (Exchange_File,
                           Binding_Label (Gprexch.Main_Base_Name));
                        Put_Line
                          (Exchange_File, Get_Name_String (Main_Base_Name));

                        --  Then, the compiler path

                        Put_Line
                          (Exchange_File,
                           Binding_Label (Gprexch.Compiler_Path));
                        Put_Line
                          (Exchange_File,
                           Project_Tree.Languages_Data.Table
                             (B_Data.Language).
                                Config.Compiler_Driver_Path.all);

                        --  Then, the Dependency files

                        if Main_Source.Unit /= No_Name then
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
                           B_Data.Language_Name,
                           Main_Source,
                           Dep_Files);

                        --  Put the options, if any

                        declare
                           The_Packages : constant Package_Id :=
                                            Project_Tree.Projects.Table
                                              (Main_Proj).Decl.Packages;

                           Binder_Package : constant Prj.Package_Id :=
                                              Prj.Util.Value_Of
                                                (Name        => Name_Binder,
                                                 In_Packages => The_Packages,
                                                 In_Tree     => Project_Tree);

                           Switches     : Variable_Value;
                           Switch_List  : String_List_Id;
                           Element      : String_Element;
                           Config  : constant Language_Config :=
                             Project_Tree.Languages_Data.Table
                               (B_Data.Language).Config;

                        begin
                           --  First, check if there are binder options
                           --  specified in the main project file.

                           if Binder_Package /= No_Package then
                              declare
                                 Defaults : constant Array_Element_Id :=
                                              Prj.Util.Value_Of
                                                (Name      =>
                                                   Name_Default_Switches,
                                                 In_Arrays =>
                                                   Project_Tree.Packages.Table
                                                  (Binder_Package).Decl.Arrays,
                                                 In_Tree   => Project_Tree);

                                 Switches_Array : constant Array_Element_Id :=
                                                    Prj.Util.Value_Of
                                                      (Name      =>
                                                         Name_Switches,
                                                       In_Arrays =>
                                                         Project_Tree.
                                                           Packages.Table
                                                         (Binder_Package)
                                                       .Decl.Arrays,
                                                       In_Tree   =>
                                                         Project_Tree);

                              begin
                                 Switches :=
                                   Prj.Util.Value_Of
                                     (Index     => Name_Id (Main_Id),
                                      Src_Index => 0,
                                      In_Array  => Switches_Array,
                                      In_Tree   => Project_Tree);

                                 if Switches = Nil_Variable_Value then
                                    Switches :=
                                      Prj.Util.Value_Of
                                        (Index     => B_Data.Language_Name,
                                         Src_Index => 0,
                                         In_Array  => Switches_Array,
                                         In_Tree   => Project_Tree,
                                         Force_Lower_Case_Index => True);
                                 end if;

                                 if Switches = Nil_Variable_Value then
                                    Switches :=
                                      Prj.Util.Value_Of
                                        (Index     => B_Data.Language_Name,
                                         Src_Index => 0,
                                         In_Array  => Defaults,
                                         In_Tree   => Project_Tree);
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
                                      Project_Tree.Name_Lists.Table (List);
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
                                         Project_Tree.String_Elements.Table
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

                              for
                                J in 1 .. All_Language_Binder_Options.Last
                              loop
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
                           Get_Name_String
                             (Project_Tree.Projects.Table
                                (Main_Proj).Path_Name));

                        Put_Line
                          (Exchange_File,
                           String
                             (File_Stamp
                                (Project_Tree.Projects.Table
                                   (Main_Proj).Path_Name)));

                        Proj_List :=
                          Project_Tree.Projects.Table
                            (Main_Proj).All_Imported_Projects;

                        while Proj_List /= Empty_Project_List loop
                           Proj_Element :=
                             Project_Tree.Project_Lists.Table (Proj_List);
                           Put_Line
                             (Exchange_File,
                              Get_Name_String
                                (Project_Tree.Projects.Table
                                   (Proj_Element.Project).Path_Name));

                           Put_Line
                             (Exchange_File,
                              String
                                (File_Stamp
                                   (Project_Tree.Projects.Table
                                      (Proj_Element.Project).Path_Name)));

                           Proj_List := Proj_Element.Next;
                        end loop;

                        Close (Exchange_File);

                        if Main_Source.Unit = No_Name and then
                          (not Dep_Files)
                        then
                           if Verbose_Mode then
                              Write_Line ("      -> nothing to bind");
                           end if;

                        else
                           if Project_Tree.Languages_Data.Table
                             (B_Data.Language).Config.Objects_Path /= No_Name
                           then
                              declare
                                 Env_Var   : constant String :=
                                        Get_Name_String
                                          (Project_Tree.Languages_Data.Table
                                            (B_Data.Language).Config.
                                               Objects_Path);
                                 Path_Name : String_Access :=
                                               Project_Tree.Projects.Table
                                                 (Main_Proj).Objects_Path;
                              begin
                                 if Path_Name = null then
                                    if Current_Verbosity = High then
                                       Put_Line (Env_Var & " :");
                                    end if;

                                    Get_Directories
                                      (Main_Proj,
                                       Sources   => False,
                                       Languages => No_Names);

                                    if Path_Buffer = null then
                                       Path_Buffer :=
                                         new String
                                           (1 .. Path_Buffer_Initial_Length);
                                    end if;

                                    Path_Last := 0;

                                    for Index in 1 .. Directories.Last loop
                                       if Path_Last /= 0 then
                                          Add_To_Path (Path_Separator);
                                       end if;

                                       Add_To_Path
                                         (Get_Name_String
                                            (Directories.Table (Index)));

                                       if Current_Verbosity = High then
                                          Put_Line
                                            (Get_Name_String
                                               (Directories.Table (Index)));
                                       end if;
                                    end loop;

                                    Path_Name :=
                                      new String'(Path_Buffer
                                                  (1 .. Path_Last));
                                    Project_Tree.Projects.Table
                                      (Main_Proj).Objects_Path :=
                                      Path_Name;
                                 end if;

                                 Setenv (Env_Var, Path_Name.all);

                                 if Verbose_Mode then
                                    Write_Str (Env_Var);
                                    Write_Str (" = ");
                                    Write_Line (Path_Name.all);
                                 end if;
                              end;

                           elsif Project_Tree.Languages_Data.Table
                             (B_Data.Language).Config.Objects_Path_File /=
                             No_Name
                           then
                              declare
                                 Env_Var   : constant String :=
                                        Get_Name_String
                                          (Project_Tree.Languages_Data.Table
                                             (B_Data.Language).Config.
                                                Objects_Path_File);
                                 Path_Name : Path_Name_Type :=
                                               Project_Tree.Projects.Table
                                                 (Main_Proj).
                                                Objects_Path_File_Without_Libs;
                              begin
                                 if Path_Name = No_Path then
                                    if Current_Verbosity = High then
                                       Put_Line (Env_Var & " :");
                                    end if;

                                    Get_Directories
                                      (Main_Proj,
                                       Sources   => False,
                                       Languages => No_Names);

                                    declare
                                       FD     : File_Descriptor;
                                       Len    : Integer;
                                       Status : Boolean;
                                    begin
                                       Prj.Env.Create_New_Path_File
                                         (In_Tree   => Project_Tree,
                                          Path_FD   => FD,
                                          Path_Name =>
                                            Project_Tree.Projects.Table
                                              (Main_Proj).
                                              Objects_Path_File_Without_Libs);

                                       Path_Name :=
                                         Project_Tree.Projects.Table
                                           (Main_Proj).
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
                                             Fail_Program ("disk full");
                                          end if;
                                       end loop;

                                       Close (FD, Status);

                                       if not Status then
                                          Fail_Program ("disk full");
                                       end if;
                                    end;
                                 end if;

                                 Setenv (Env_Var, Get_Name_String (Path_Name));

                                 if Verbose_Mode then
                                    Write_Str (Env_Var);
                                    Write_Str (" = ");
                                    Write_Line (Get_Name_String (Path_Name));
                                 end if;
                              end;
                           end if;

                           if not Quiet_Output then
                              if Verbose_Mode then
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
                              Fail_Program ("unable to bind ", Main);
                           end if;
                        end if;
                     end if;
                  end loop;
               end if;
            end;
         end;
      end loop;
   end Binding_Phase;

   --------------------------
   -- Build_Global_Archive --
   --------------------------

   procedure Build_Global_Archive
     (For_Project : Project_Id;
      Has_Been_Built : out Boolean)
   is
      Data : constant Project_Data :=
               Project_Tree.Projects.Table (For_Project);

      Archive_Name : constant String :=
        "lib" & Get_Name_String (Data.Name) & ".a";
      --  The name of the archive file for this project

      Archive_Dep_Name : constant String :=
        "lib" & Get_Name_String (Data.Name) & ".deps";
      --  The name of the archive dependency file for this project

      File : Prj.Util.Text_File;

      Object_Path  : Path_Name_Type;
      Time_Stamp   : Time_Stamp_Type;

      First_Object        : Natural;

      Discard : Boolean;

      Proj_Element : Project_Element;
      Proj_List    : Project_List;

      Src_Id       : Source_Id;
      S_Id         : Source_Id;
      Source       : Source_Data;

      Success      : Boolean;

      Real_Last_Argument : Positive;
      Current_Object_Pos : Positive;

      Size : Natural;

      procedure Add_Sources (Proj : Project_Id);
      --  Add all the sources of project Proj to Sources_Index

      procedure Add_Objects (Proj : Project_Id);
      --  Add all the object paths of project Proj to Arguments

      -----------------
      -- Add_Sources --
      -----------------

      procedure Add_Sources (Proj : Project_Id) is
         Project : Project_Id := Proj;
         Id : Source_Id;
         Source : Source_Data;

      begin
         loop
            Id := Project_Tree.Projects.Table (Project).First_Source;

            while Id /= No_Source loop
               Source := Project_Tree.Sources.Table (Id);

               if (not Source.Locally_Removed)
                 and then
                  Source.Compiled
                 and then
                 (Source.Kind = Impl
                  or else
                  (Source.Unit /= No_Name
                   and then
                   Source.Kind = Spec
                   and then
                   Source.Other_Part = No_Source))
               then
                  if Source.Unit = No_Name
                    or else Source.Kind = Spec
                    or else not Is_Subunit (Source)
                  then
                     Initialize_Source_Record (Id);
                     Source := Project_Tree.Sources.Table (Id);

                     --  Only include sources with object file names that have
                     --  not been overriden in extending projects.

                     if Source.Object /= No_File
                       and then
                        Is_Included_In_Global_Archive (Source.Object, Project)
                     then
                        Add_Source_Id (Proj, Id);
                     end if;
                  end if;
               end if;

               Id := Source.Next_In_Project;
            end loop;
            Project := Project_Tree.Projects.Table (Project).Extends;

            exit when Project = No_Project;
         end loop;
      end Add_Sources;

      -----------------
      -- Add_Objects --
      -----------------

      procedure Add_Objects (Proj : Project_Id) is
         Project : Project_Id := Proj;
         Id : Source_Id;
         Source : Source_Data;

      begin
         loop
            if Project_Tree.Projects.Table (Project).Externally_Built then
               --  If project is externally built, include all object files in
               --  the object directory in the global archive.

               declare
                  Obj_Dir : constant String :=
                              Get_Name_String
                                (Project_Tree.Projects.Table (Project).
                                   Object_Directory);
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
                           Verbose_Mode,
                           Simple_Name => not Verbose_Mode);
                     end if;
                  end loop;

                  Close (Dir_Obj);
               end;

            else
               Id := Project_Tree.Projects.Table (Project).First_Source;

               while Id /= No_Source loop
                  Source := Project_Tree.Sources.Table (Id);

                  if (not Source.Locally_Removed)
                    and then
                     Source.Compiled
                    and then
                      (Source.Kind = Impl
                       or else
                         (Source.Unit /= No_Name
                          and then
                            Source.Kind = Spec
                          and then
                            Source.Other_Part = No_Source))
                  then
                     if not Is_Subunit (Source) then
                        --  Only include object file name that have not been
                        --  overriden in extending projects.

                        if Source.Object /= No_File
                           and then
                           Is_Included_In_Global_Archive
                             (Source.Object, Project)
                        then
                           Add_Argument
                             (Get_Name_String (Source.Object_Path),
                              Verbose_Mode,
                              Simple_Name => not Verbose_Mode);
                        end if;
                     end if;
                  end if;

                  Id := Source.Next_In_Project;
               end loop;
            end if;

            Project := Project_Tree.Projects.Table (Project).Extends;

            exit when Project = No_Project;
         end loop;
      end Add_Objects;

   begin
      Has_Been_Built := Need_To_Rebuild_Global_Archives;

      --  No need to build the global archive, if it has already been done

      if Data.Object_Directory /= No_Path and then
        not Global_Archives_Built.Get (Data.Name)
      then
         Check_Archive_Builder;

         if Project_Of_Current_Object_Directory /= For_Project then
            Project_Of_Current_Object_Directory := For_Project;
            Change_Dir (Get_Name_String (Data.Object_Directory));

            if Verbose_Mode then
               Write_Str  ("Changing to object directory of """);
               Write_Name (Data.Name);
               Write_Str  (""": """);
               Write_Name (Data.Object_Directory);
               Write_Line ("""");
            end if;
         end if;

         --  Put all sources in the project tree in Source_Indexes

         Last_Source := 0;

         Add_Sources (For_Project);

         Proj_List := Data.All_Imported_Projects;

         while Proj_List /= Empty_Project_List loop
            Proj_Element :=
              Project_Tree.Project_Lists.Table (Proj_List);

            if not Project_Tree.Projects.Table
              (Proj_Element.Project).Library
            then
               Add_Sources (Proj_Element.Project);
            end if;

            Proj_List := Proj_Element.Next;
         end loop;

         if not Has_Been_Built then
            if Verbose_Mode then
               Write_Str  ("   Checking ");
               Write_Str  (Archive_Name);
               Write_Line (" ...");
            end if;

            --  If the archive does not exist, of course it needs to be built

            if not Is_Regular_File (Archive_Name) then
               Has_Been_Built := True;

               if Verbose_Mode then
                  Write_Line ("      -> archive does not exist");
               end if;

            else
               --  Archive does exist

               --  Check the archive dependency file

               Open (File, Archive_Dep_Name);

               --  If the archive dependency file does not exist, we need to
               --  to rebuild the archive and to create its dependency file.

               if not Is_Valid (File) then
                  Has_Been_Built := True;

                  if Verbose_Mode then
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
                        Source := Project_Tree.Sources.Table (S_Id);

                        if (not Source_Indexes (S).Found)
                          and then Source.Object_Path = Object_Path
                        then
                           --  We have found the object file: get the source
                           --  data, and mark it as found.

                           Src_Id := S_Id;
                           Source_Indexes (S).Found := True;
                           exit;
                        end if;
                     end loop;

                     --  If it is not for a source of this project, then the
                     --  archive needs to be rebuilt.

                     if Src_Id = No_Source then
                        Has_Been_Built := True;
                        if Verbose_Mode then
                           Write_Str  ("      -> ");
                           Write_Str  (Get_Name_String (Object_Path));
                           Write_Line (" is not an object of any project");
                        end if;

                        exit;
                     end if;

                     --  The second line is the time stamp of the object file.
                     --  If there is no next line, then the dependency file is
                     --  truncated, and the archive need to be rebuilt.

                     if End_Of_File (File) then
                        Has_Been_Built := True;

                        if Verbose_Mode then
                           Write_Str  ("      -> archive dependency file ");
                           Write_Line (" is truncated");
                        end if;

                        exit;
                     end if;

                     Get_Line (File, Name_Buffer, Name_Len);

                     --  If the line has the wrong number of characters, then
                     --  the dependency file is incorrectly formatted, and the
                     --  archive needs to be rebuilt.

                     if Name_Len /= Time_Stamp_Length then
                        Has_Been_Built := True;

                        if Verbose_Mode then
                           Write_Str  ("      -> archive dependency file ");
                           Write_Line
                             (" is incorrectly formatted (time stamp)");
                        end if;

                        exit;
                     end if;

                     Time_Stamp :=
                       Time_Stamp_Type (Name_Buffer (1 .. Name_Len));

                     --  If the time stamp in the dependency file is different
                     --  from the time stamp of the object file, then the
                     --  archive needs to be rebuilt. The comparaison is done
                     --  with String type values, because two values of type
                     --  Time_Stamp_Type are equal if they differ by 2 seconds
                     --  or less; here the check is for an exact match.

                     if String (Time_Stamp) /= String (Source.Object_TS) then
                        Has_Been_Built := True;

                        if Verbose_Mode then
                           Write_Str  ("      -> time stamp of ");
                           Write_Str  (Get_Name_String (Object_Path));
                           Write_Str  (" is incorrect in the archive");
                           Write_Line (" dependency file");
                           Write_Str  ("         recorded time stamp: ");
                           Write_Line (String (Time_Stamp));
                           Write_Str  ("           actual time stamp: ");
                           Write_Line (String (Source.Object_TS));
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
                           Write_Line (String (Source.Object_TS));
                     end if;
                  end loop;

                  Close (File);
               end if;
            end if;
         end if;

         if not Has_Been_Built then
            for S in 1 .. Last_Source loop
               if not Source_Indexes (S).Found then
                  Has_Been_Built := True;

                  if Verbose_Mode then
                     Source := Project_Tree.Sources.Table
                                  (Source_Indexes (S).Id);
                     Write_Str ("      -> object file ");
                     Write_Str (Get_Name_String (Source.Object_Path));
                     Write_Line (" is not in the dependency file");
                  end if;

                  exit;
               end if;
            end loop;
         end if;

         if not Has_Been_Built then
            if Verbose_Mode then
               Write_Line  ("      -> up to date");
            end if;

            --  Archive needs to be rebuilt

         else
            --  If archive already exists, first delete it, but if this is not
            --  possible, continue: if archive cannot be built, we will fail
            --  later on.

            if Is_Regular_File (Archive_Name) then
               Delete_File (Archive_Name, Discard);
            end if;

            Last_Argument := 0;

            --  Start with the minimal options

            Add_Arguments
              (Archive_Builder_Opts.Options (1 .. Archive_Builder_Opts.Last),
               True);

            --  Followed by the archive name

            Add_Argument (Archive_Name, True, Simple_Name => not Verbose_Mode);

            First_Object := Last_Argument + 1;

            --  Followed by all the object files of the non library projects

            Add_Objects (For_Project);

            Proj_List := Data.All_Imported_Projects;

            while Proj_List /= Empty_Project_List loop
               Proj_Element :=
                 Project_Tree.Project_Lists.Table (Proj_List);

               if not Project_Tree.Projects.Table
                 (Proj_Element.Project).Library
               then
                  Add_Objects (Proj_Element.Project);
               end if;

               Proj_List := Proj_Element.Next;
            end loop;

            --  No need to create a global archive, if there is no object
            --  file to put into.

            if Last_Argument >= First_Object then

               Real_Last_Argument := Last_Argument;

               --  If there is an Archive_Builder_Append_Option, we may have to
               --  build the archive in chuck.

               if Archive_Builder_Append_Opts.Last = 0 then
                  Current_Object_Pos := Real_Last_Argument + 1;

               else
                  Size := 0;
                  for J in 1 .. First_Object - 1 loop
                     Size := Size + Arguments (J)'Length + 1;
                  end loop;

                  Current_Object_Pos := First_Object;
                  while Current_Object_Pos <= Real_Last_Argument loop
                     Size := Size + Arguments (Current_Object_Pos)'Length + 1;
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

               if Success and then
                 Current_Object_Pos <= Real_Last_Argument
               then
                  Last_Argument := 0;
                  Add_Arguments
                    (Archive_Builder_Append_Opts.Options
                       (1 .. Archive_Builder_Append_Opts.Last),
                     True);
                  Add_Argument
                    (Archive_Name, True, Simple_Name => not Verbose_Mode);
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

               --  If the archive was built, run the archive indexer (ranlib)
               --  if there is one.

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
                        Simple_Name => not Verbose_Mode);

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

                        Fail_Program ("global archive could not be built");
                        return;
                     end if;
                  end if;

                  --  The archive was correctly built, create its dependency
                  --  file.

                  declare
                     Dep_File  : Ada.Text_IO.File_Type;

                  begin
                     --  Create the file in Append mode, to avoid automatic
                     --  insertion of an end of line if file is empty.

                     Create (Dep_File, Append_File, Archive_Dep_Name);

                     for S in 1 .. Last_Source loop
                        Src_Id := Source_Indexes (S).Id;
                        Source := Project_Tree.Sources.Table (Src_Id);
                        Put_Line
                          (Dep_File, Get_Name_String (Source.Object_Path));
                        Put_Line
                          (Dep_File, String (Source.Object_TS));
                     end loop;

                     Close (Dep_File);

                  exception
                     when others =>
                        if Is_Open (Dep_File) then
                           Close (Dep_File);
                        end if;
                  end;

               else
                  --  Building the archive failed, delete dependency file if
                  --  one exists.

                  if Is_Regular_File (Archive_Dep_Name) then
                     Delete_File (Archive_Dep_Name, Success);
                  end if;

                  Fail_Program ("global archive could not be built");
                  return;
               end if;
            end if;
         end if;

         Global_Archives_Built.Set (Data.Name, True);
      end if;
   end Build_Global_Archive;

   -------------------
   -- Build_Library --
   -------------------

   procedure Build_Library (For_Project : Project_Id) is
      Data : constant Project_Data :=
               Project_Tree.Projects.Table (For_Project);

      Object_Directory_Path : constant String :=
                                Get_Name_String (Data.Object_Directory);

      Project_Name          : constant String := Get_Name_String (Data.Name);

      Current_Dir           : constant String := Get_Current_Dir;

      Exchange_File : Ada.Text_IO.File_Type;

      Exchange_File_Name : String_Access;

      Latest_Object_TS : Time_Stamp_Type := Empty_Time_Stamp;

      Library_Builder_Name : String_Access;
      Library_Builder      : String_Access;

      Toolchain_Version_Label_Written : Boolean;
      Lang_Index : Language_Index;
      Lang_Data  : Language_Data;

      Library_Options : Variable_Value := Nil_Variable_Value;

      Library_Needs_To_Be_Built : Boolean := False;

      Object_Path : Path_Name_Type;
      Object_TS   : Time_Stamp_Type;

      Source      : Source_Id;
      Src_Data    : Source_Data;
      Project     : Project_Id;

      Unit_Based_Language_Name : Name_Id;

      procedure Get_Objects;
      --  Get the paths of the object files of the library in table
      --  Library_Objs.

      -----------------
      -- Get_Objects --
      -----------------

      procedure Get_Objects is
         Source   : Source_Id;
         Src_Data : Source_Data;

         Proj : Project_Id := For_Project;

      begin
         Library_Objs.Init;

         loop
            Source := Project_Tree.Projects.Table (Proj).First_Source;

            while Source /= No_Source loop
               Initialize_Source_Record (Source);

               Src_Data := Project_Tree.Sources.Table (Source);

               if (not Src_Data.Locally_Removed)
                 and then
                   Src_Data.Compiled
                 and then
                   Src_Data.Object_Linked
                 and then
                 ((Src_Data.Unit = No_Name and then Src_Data.Kind = Impl)
                 or else
                   (Src_Data.Unit /= No_Name
                    and then
                      (Src_Data.Kind = Impl
                       or else
                         Src_Data.Other_Part = No_Source)
                    and then
                      (not Is_Subunit (Src_Data))))
               then
                  Library_Objs.Append
                    ((Path  => Src_Data.Object_Path,
                      TS    => Src_Data.Object_TS,
                      Known => False));

                  if Src_Data.Object_TS = Empty_Time_Stamp then
                     Latest_Object_TS := Never;

                     if not Library_Needs_To_Be_Built then
                        Library_Needs_To_Be_Built := True;

                        if Verbose_Mode then
                           Write_Str ("      -> missing object file: ");
                           Get_Name_String (Src_Data.Object);
                           Write_Line (Name_Buffer (1 .. Name_Len));
                        end if;
                     end if;

                  elsif Src_Data.Object_TS > Latest_Object_TS then
                     Latest_Object_TS := Src_Data.Object_TS;
                  end if;
               end if;

               Source := Src_Data.Next_In_Project;
            end loop;

            Proj := Project_Tree.Projects.Table (Proj).Extends;
            exit when Proj = No_Project;
         end loop;
      end Get_Objects;

      --  Start of processing for Build_Library

   begin
      if Data.Config.Lib_Support = None then
         Fail_Program ("library projects not supported on this platform");

      elsif Data.Library_Kind /= Static and then
            Data.Config.Lib_Support /= Full
      then
         Fail_Program
           ("shared library projects not supported on this platform");
      end if;

      if Data.Config.Library_Builder = No_Path then
         Fail_Program ("no library builder specified");

      else
         Library_Builder :=
           Locate_Exec_On_Path
             (Get_Name_String (Data.Config.Library_Builder));

         if Library_Builder = null then
            Fail_Program
              ("could not locate library builder """,
               Get_Name_String (Data.Config.Library_Builder), """");

         else
            Library_Builder_Name :=
              new String'(Base_Name (Library_Builder.all));
         end if;
      end if;

      if Data.Library_Kind = Static then
         Check_Archive_Builder;
      end if;

      --  Work occurs in the object directory

      if Project_Of_Current_Object_Directory /= For_Project then
         Project_Of_Current_Object_Directory := For_Project;
         Change_Dir (Object_Directory_Path);

         if Verbose_Mode then
            Write_Str  ("Changing to object directory of """);
            Write_Name (Data.Name);
            Write_Str  (""": """);
            Write_Str  (Object_Directory_Path);
            Write_Line ("""");
         end if;
      end if;

      Library_Needs_To_Be_Built := Force_Compilations;

      if (not Library_Needs_To_Be_Built) and then Verbose_Mode then
         Write_Str ("   Checking library ");
         Get_Name_String (Data.Library_Name);
         Write_Str (Name_Buffer (1 .. Name_Len));
         Write_Line (" ...");
      end if;

      Get_Objects;

      --  Get the name of of the library exchange file

      Get_Name_String (Data.Library_Name);
      Add_Str_To_Name_Buffer (Library_Exchange_Suffix);
      Exchange_File_Name := new String'(Name_Buffer (1 .. Name_Len));

      if not Library_Needs_To_Be_Built then
         declare
            TS : constant Time_Stamp_Type :=
                   File_Stamp (File_Name_Type'(Name_Find));

         begin
            if String (TS) < String (Latest_Object_TS) then
               Library_Needs_To_Be_Built := True;

               if Verbose_Mode then
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

               exception
                  when others =>
                     Write_Str
                       ("      -> library exchange file """);
                     Write_Str (Exchange_File_Name.all);
                     Write_Line (""" does not exist");
                     Library_Needs_To_Be_Built := True;
               end;

               if End_Of_File (Exchange_File) then
                  Write_Str ("      -> library exchange file """);
                  Write_Str (Exchange_File_Name.all);
                  Write_Line (""" is empty");
                  Library_Needs_To_Be_Built := True;
               end if;

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

            if Verbose_Mode then
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

               if Verbose_Mode then
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

            if Verbose_Mode then
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
               if Verbose_Mode then
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

                  for Index in 1 .. Library_Objs.Last loop
                     if Object_Path = Library_Objs.Table (Index).Path then
                        Library_Needs_To_Be_Built :=
                          Object_TS /= Library_Objs.Table (Index).TS;
                        Library_Objs.Table (Index).Known := True;
                        exit;
                     end if;
                  end loop;

                  if Library_Needs_To_Be_Built and then Verbose_Mode then
                     Write_Str ("      -> object file ");
                     Write_Str (Get_Name_String (Object_Path));
                     Write_Line
                       (" does not exist or have wrong time stamp");
                  end if;

               else
                  if Verbose_Mode then
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

                  if Verbose_Mode then
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
         if Verbose_Mode then
            Write_Line ("      -> up to date");
         end if;

      else
         --  Create the library exchange file
         begin
            Create (Exchange_File, Out_File, Exchange_File_Name.all);

         exception
            when others =>
               Fail_Program
                 ("unable to create library exchange file ",
                  Exchange_File_Name.all);
         end;

         if Quiet_Output then
            Put_Line (Exchange_File, Library_Label (Quiet));

         elsif Verbose_Mode then
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
         Put_Line (Exchange_File, Get_Name_String (Data.Library_Name));

         if Data.Lib_Internal_Name /= No_Name then
            Put_Line (Exchange_File, Library_Label (Library_Version));
            Put_Line (Exchange_File, Get_Name_String (Data.Lib_Internal_Name));
         end if;

         Put_Line (Exchange_File, Library_Label (Library_Directory));
         Put_Line (Exchange_File, Get_Name_String (Data.Library_Dir));

         if Data.Library_ALI_Dir /= No_Path and then
           Data.Library_ALI_Dir /= Data.Library_Dir
         then
            Put_Line
              (Exchange_File, Library_Label (Library_Dependency_Directory));
            Put_Line (Exchange_File, Get_Name_String (Data.Library_ALI_Dir));
         end if;

         Put_Line (Exchange_File, Library_Label (Object_Directory));
         Put_Line (Exchange_File, Object_Directory_Path);

         --  Add object directory of project being extended, if any

         declare
            Proj : Project_Id := Data.Extends;

         begin
            while Proj /= No_Project loop
               if
                 Project_Tree.Projects.Table (Proj).Object_Directory /= No_Path
               then
                  Put_Line
                    (Exchange_File,
                     Get_Name_String
                       (Project_Tree.Projects.Table (Proj).Object_Directory));
               end if;
               Proj := Project_Tree.Projects.Table (Proj).Extends;
            end loop;
         end;

         --  Add ALI dir directories of imported projects

         declare
            List : Project_List := Data.All_Imported_Projects;
            Elem : Project_Element;
            Pdata : Project_Data;

         begin
            while List /= Empty_Project_List loop
               Elem := Project_Tree.Project_Lists.Table (List);
               Pdata := Project_Tree.Projects.Table (Elem.Project);

               if Pdata.Library_ALI_Dir /= No_Path then
                  Put_Line
                    (Exchange_File, Get_Name_String (Pdata.Library_ALI_Dir));

               elsif Pdata.Library_Dir /= No_Path then
                  Put_Line
                    (Exchange_File, Get_Name_String (Pdata.Library_Dir));
               end if;

               List := Elem.Next;
            end loop;
         end;

         Put_Line (Exchange_File, Library_Label (Compilers));

         declare
            Lang : Language_Index := Data.First_Language_Processing;
            Lang_Data : Language_Data;

         begin
            while Lang /= No_Language_Index loop
               Lang_Data := Project_Tree.Languages_Data.Table (Lang);

               if Lang_Data.Config.Compiler_Driver_Path /= null then
                  Put_Line (Exchange_File, Get_Name_String (Lang_Data.Name));
                  Put_Line
                    (Exchange_File, Lang_Data.Config.Compiler_Driver_Path.all);

               elsif Lang_Data.Config.Compiler_Driver /= No_File then
                  Put_Line (Exchange_File, Get_Name_String (Lang_Data.Name));
                  Put_Line
                    (Exchange_File,
                     Get_Name_String (Lang_Data.Config.Compiler_Driver));
               end if;

               Lang := Lang_Data.Next;
            end loop;
         end;

         if Data.Library_Kind = Static then
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

            if Data.Config.Archive_Suffix /= No_File then
               Put_Line (Exchange_File, Library_Label (Archive_Suffix));
               Put_Line
                 (Exchange_File,
                  Get_Name_String (Data.Config.Archive_Suffix));
            end if;

            if Archive_Indexer_Path /= null then
               Put_Line (Exchange_File, Library_Label (Archive_Indexer));
               Put_Line (Exchange_File, Archive_Indexer_Path.all);

               for J in 1 .. Archive_Indexer_Opts.Last loop
                  Put_Line
                    (Exchange_File, Archive_Indexer_Opts.Options (J).all);
               end loop;
            end if;

            if Data.Config.Lib_Partial_Linker /= No_Name_List then
               Put_Line (Exchange_File, Library_Label (Partial_Linker));

               declare
                  List : Name_List_Index :=
                           Data.Config.Lib_Partial_Linker;
                  Nam_Nod : Name_Node;

               begin
                  while List /= No_Name_List loop
                     Nam_Nod := Project_Tree.Name_Lists.Table (List);
                     Put_Line
                       (Exchange_File,
                        Get_Name_String (Nam_Nod.Name));
                     List := Nam_Nod.Next;
                  end loop;
               end;
            end if;

         else
            if Data.Config.Shared_Lib_Driver /= No_File then
               Put_Line (Exchange_File, Library_Label (Driver_Name));
               Put_Line
                 (Exchange_File,
                  Get_Name_String (Data.Config.Shared_Lib_Driver));
            end if;

            if Data.Config.Shared_Lib_Prefix /= No_File then
               Put_Line (Exchange_File, Library_Label (Shared_Lib_Prefix));
               Put_Line
                 (Exchange_File,
                  Get_Name_String (Data.Config.Shared_Lib_Prefix));
            end if;

            if Data.Config.Shared_Lib_Suffix /= No_File then
               Put_Line (Exchange_File, Library_Label (Shared_Lib_Suffix));
               Put_Line
                 (Exchange_File,
                  Get_Name_String (Data.Config.Shared_Lib_Suffix));
            end if;

            if Data.Config.Shared_Lib_Min_Options /= No_Name_List then
               Put_Line
                 (Exchange_File, Library_Label (Shared_Lib_Minimum_Options));
               declare
                  List : Name_List_Index :=
                           Data.Config.Shared_Lib_Min_Options;
                  Nam_Nod : Name_Node;

               begin
                  while List /= No_Name_List loop
                     Nam_Nod := Project_Tree.Name_Lists.Table (List);
                     Put_Line
                       (Exchange_File,
                        Get_Name_String (Nam_Nod.Name));
                     List := Nam_Nod.Next;
                  end loop;
               end;
            end if;

            if Data.Config.Lib_Version_Options /= No_Name_List then
               Put_Line
                 (Exchange_File, Library_Label (Library_Version_Options));
               declare
                  List : Name_List_Index :=
                           Data.Config.Lib_Version_Options;
                  Nam_Nod : Name_Node;

               begin
                  while List /= No_Name_List loop
                     Nam_Nod := Project_Tree.Name_Lists.Table (List);
                     Put_Line
                       (Exchange_File,
                        Get_Name_String (Nam_Nod.Name));
                     List := Nam_Nod.Next;
                  end loop;
               end;
            end if;

            if Data.Config.Symbolic_Link_Supported then
               Put_Line
                 (Exchange_File, Library_Label (Symbolic_Link_Supported));
            end if;

            if Data.Config.Lib_Maj_Min_Id_Supported then
               Put_Line
                 (Exchange_File, Library_Label (Major_Minor_Id_Supported));
            end if;

            Process_Imported_Libraries (For_Project);

            --  Check for runtime library dir

            declare
               List : Language_Index := Data.First_Language_Processing;
               Elem : Language_Data;
               First : Boolean := True;

            begin
               while List /= No_Language_Index loop
                  Elem := Project_Tree.Languages_Data.Table (List);

                  if Elem.Config.Runtime_Library_Dir /= No_Name then
                     if First then
                        Put_Line
                          (Exchange_File, Library_Label (Runtime_Library_Dir));
                        First := False;
                     end if;

                     Put_Line
                       (Exchange_File, Get_Name_String (Elem.Name));
                     Put_Line
                       (Exchange_File,
                        Get_Name_String (Elem.Config.Runtime_Library_Dir));
                  end if;

                  List := Elem.Next;
               end loop;
            end;

            if Data.Library_Kind /= Static then
               Put_Line (Exchange_File, Library_Label (Relocatable));
            end if;

            if Data.Standalone_Library then
               if Data.Lib_Auto_Init then
                  Put_Line (Exchange_File, Library_Label (Auto_Init));
               end if;

               declare
                  Binder_Package : constant Package_Id :=
                                     Value_Of
                                       (Name        => Name_Binder,
                                        In_Packages => Data.Decl.Packages,
                                        In_Tree     => Project_Tree);

               begin
                  if Binder_Package /= No_Package then
                     declare
                        Defaults : constant Array_Element_Id :=
                                     Value_Of
                                       (Name      => Name_Default_Switches,
                                        In_Arrays =>
                                          Project_Tree.Packages.Table
                                            (Binder_Package).Decl.Arrays,
                                        In_Tree   => Project_Tree);
                        Switches : Variable_Value := Nil_Variable_Value;

                        Switch   : String_List_Id := Nil_String;

                     begin
                        if Defaults /= No_Array_Element then
                           Switches :=
                             Value_Of
                               (Index     => Name_Ada,
                                Src_Index => 0,
                                In_Array  => Defaults,
                                In_Tree   => Project_Tree);

                           if not Switches.Default then
                              Put_Line
                                (Exchange_File,
                                 Library_Label (Gprexch.Binding_Options));
                              Switch := Switches.Values;

                              while Switch /= Nil_String loop
                                 Put_Line
                                   (Exchange_File,
                                    Get_Name_String
                                      (Project_Tree.String_Elements.Table
                                         (Switch).Value));
                                 Switch := Project_Tree.String_Elements.
                                   Table (Switch).Next;
                              end loop;
                           end if;
                        end if;
                     end;
                  end if;
               end;

            end if;

            if Data.Config.Run_Path_Option /= No_Name_List then
               Put_Line
                 (Exchange_File, Library_Label (Gprexch.Run_Path_Option));

               declare
                  List : Name_List_Index :=
                           Data.Config.Run_Path_Option;
                  Nam  : Name_Node;

               begin
                  while List /= No_Name_List loop
                     Nam := Project_Tree.Name_Lists.Table (List);
                     Put_Line (Exchange_File, Get_Name_String (Nam.Name));
                     List := Nam.Next;
                  end loop;
               end;
            end if;

            --  If attribute Library_Options was specified, add these
            --  additional options.

            Library_Options :=
              Value_Of
                (Name_Library_Options, Data.Decl.Attributes, Project_Tree);

            if not Library_Options.Default then
               declare
                  Current      : String_List_Id := Library_Options.Values;
                  Element      : String_Element;
                  Output_Label : Boolean := True;

               begin
                  while Current /= Nil_String loop
                     Element :=
                       Project_Tree.String_Elements.Table (Current);
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
                       (Project_Tree.Projects.Table
                          (Library_Projs.Table (J)).Library_Dir));
                  Put_Line
                    (Exchange_File,
                     Get_Name_String
                       (Project_Tree.Projects.Table
                          (Library_Projs.Table (J)).Library_Name));
               end loop;
            end if;
         end if;

         Put_Line (Exchange_File, Library_Label (Dependency_Files));

         declare
            Current_Proj : Project_Id := For_Project;
            Proj_Data    : Project_Data;
            Source       : Source_Id;
            Src_Data     : Source_Data;
         begin
            while Current_Proj /= No_Project loop
               Proj_Data := Project_Tree.Projects.Table (Current_Proj);
               Source := Proj_Data.First_Source;

               while Source /= No_Source loop
                  Src_Data := Project_Tree.Sources.Table (Source);

                  if not Src_Data.Locally_Removed
                    and then Src_Data.Dep_Name /= No_File
                  then
                     if Src_Data.Kind = Spec then
                        if Src_Data.Other_Part = No_Source then
                           Put_Line
                             (Exchange_File,
                              Get_Name_String (Src_Data.Dep_Name));
                        end if;

                     elsif not Is_Subunit (Src_Data) then
                        Put_Line
                          (Exchange_File,
                           Get_Name_String (Src_Data.Dep_Name));
                     end if;
                  end if;

                  Source := Src_Data.Next_In_Project;
               end loop;

               Current_Proj := Proj_Data.Extends;
            end loop;
         end;

         Lang_Index := Data.First_Language_Processing;
         Toolchain_Version_Label_Written := False;

         while Lang_Index /= No_Language_Index loop
            Lang_Data := Project_Tree.Languages_Data.Table (Lang_Index);

            if Lang_Data.Config.Toolchain_Version /= No_Name then
               if not Toolchain_Version_Label_Written then
                  Put_Line (Exchange_File, Library_Label (Toolchain_Version));
                  Toolchain_Version_Label_Written := True;
               end if;

               Put_Line (Exchange_File, Get_Name_String (Lang_Data.Name));
               Put_Line
                 (Exchange_File,
                  Get_Name_String (Lang_Data.Config.Toolchain_Version));
            end if;

            Lang_Index := Lang_Data.Next;
         end loop;

         if Data.Standalone_Library then
            if Data.Lib_Auto_Init then
               Put_Line (Exchange_File, Library_Label (Auto_Init));
            end if;

            declare
               Interface_ALIs : String_List_Id := Data.Lib_Interface_ALIs;
               Element        : String_Element;

            begin
               Put_Line (Exchange_File, Library_Label (Interface_Dep_Files));

               while Interface_ALIs /= Nil_String loop
                  Element :=
                    Project_Tree.String_Elements.Table (Interface_ALIs);
                  Put_Line (Exchange_File, Get_Name_String (Element.Value));
                  Interface_ALIs := Element.Next;
               end loop;
            end;

            if Data.Library_Src_Dir /= No_Path then
               Put_Line (Exchange_File, Library_Label (Copy_Source_Dir));
               Put_Line
                 (Exchange_File, Get_Name_String (Data.Library_Src_Dir));

               Put_Line (Exchange_File, Library_Label (Sources));

               --  Copy the path of the sources

               Unit_Based_Language_Name :=
                 Project_Tree.Projects.Table
                   (For_Project).Unit_Based_Language_Name;

               Project := For_Project;

               while Project /= No_Project loop

                  Source := Project_Tree.Projects.Table (Project).First_Source;

                  while Source /= No_Source loop
                     Src_Data := Project_Tree.Sources.Table (Source);

                     if  Src_Data.Language_Name = Unit_Based_Language_Name
                       and then
                         (not Src_Data.Locally_Removed)
                       and then
                          Src_Data.Replaced_By = No_Source
                     then
                        Put_Line
                          (Exchange_File,
                           Get_Name_String (Src_Data.Path));
                     end if;

                     Source := Src_Data.Next_In_Project;
                  end loop;

                  Project := Project_Tree.Projects.Table (Project).Extends;
               end loop;

            end if;

         end if;

         Close (Exchange_File);

         declare
            Arguments : constant Argument_List := (1 => Exchange_File_Name);
            Success   : Boolean;

         begin
            if not Quiet_Output then
               if Verbose_Mode then
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
                 ("could not build library for project ",
                  Project_Name);
            end if;
         end;
      end if;

      --  Restore the current working directory to its previous value

      Change_Dir (Current_Dir);
   end Build_Library;

   -------------------------------
   -- Canonical_Cased_File_Name --
   -------------------------------

   function Canonical_Cased_File_Name (Name : String) return String is
      Result : String := Name;
   begin
      Canonical_Case_File_Name (Result);
      return Result;
   end Canonical_Cased_File_Name;

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

         Change_Dir
           (Get_Name_String
              (Project_Tree.Projects.Table (Project).Object_Directory));

         if Verbose_Mode then
            Write_Str  ("Changing to object directory of """);
            Write_Name (Project_Tree.Projects.Table (Project).Name);
            Write_Str  (""": """);
            Write_Name
              (Project_Tree.Projects.Table (Project).Object_Directory);
            Write_Line ("""");
         end if;
      end if;

   exception
      --  Fail if unable to change to the object directory

      when Directory_Error =>
         Fail_Program
           ("unable to change to object directory """,
            Get_Name_String
              (Project_Tree.Projects.Table (Project).Object_Directory) &
            """ of project ",
            Get_Name_String
              (Project_Tree.Projects.Table (Project).Display_Name));
   end Change_To_Object_Directory;

   ---------------------------
   -- Check_Archive_Builder --
   ---------------------------

   procedure Check_Archive_Builder is
      Data : constant Project_Data :=
               Project_Tree.Projects.Table (Main_Project);
      List : Name_List_Index;
   begin
      --  First, make sure that the archive builder (ar) is on the path

      if Archive_Builder_Path = null then
         List := Data.Config.Archive_Builder;

         if List = No_Name_List then
            Fail_Program ("no archive builder in configuration");

         else
            Archive_Builder_Name :=
              new String'(Get_Name_String
                                     (Project_Tree.Name_Lists.Table
                                        (List).Name));
            Archive_Builder_Path :=
              Locate_Exec_On_Path (Archive_Builder_Name.all);

            if Archive_Builder_Path = null then
               Fail_Program
                 ("unable to locate archive builder """,
                  Archive_Builder_Name.all,
                  """");
            end if;

            loop
               List := Project_Tree.Name_Lists.Table (List).Next;
               exit when List = No_Name_List;
               Add_Option
                 (Value   => Project_Tree.Name_Lists.Table (List).Name,
                  To      => Archive_Builder_Opts,
                  Display => True);
            end loop;

            List := Data.Config.Archive_Builder_Append_Option;
            while List /= No_Name_List loop
               Add_Option
                 (Value   => Project_Tree.Name_Lists.Table (List).Name,
                  To      => Archive_Builder_Append_Opts,
                  Display => True);
               List := Project_Tree.Name_Lists.Table (List).Next;
            end loop;

            --  If there is an archive indexer (ranlib), try to locate it on
            --  the path. Don't fail if it is not found.

            List := Data.Config.Archive_Indexer;

            if List /= No_Name_List then
               Archive_Indexer_Name :=
                 new String'(Get_Name_String
                   (Project_Tree.Name_Lists.Table
                      (List).Name));
               Archive_Indexer_Path :=
                 Locate_Exec_On_Path (Archive_Indexer_Name.all);

               if Archive_Builder_Path /= null then
                  loop
                     List := Project_Tree.Name_Lists.Table (List).Next;
                     exit when List = No_Name_List;
                     Add_Option
                       (Value   => Project_Tree.Name_Lists.Table (List).Name,
                        To      => Archive_Indexer_Opts,
                        Display => True);
                  end loop;
               end if;
            end if;
         end if;
      end if;
   end Check_Archive_Builder;

   -----------------
   -- Check_Mains --
   -----------------

   procedure Check_Mains is
      Source       : Source_Id;
      Nmb          : Natural := 0;

   begin
      Mains.Reset;

      loop
         declare
            Display_Main : constant String := Mains.Next_Main;
            Main         : String := Display_Main;
            Main_Id      : File_Name_Type;

         begin
            exit when Display_Main'Length = 0;

            Canonical_Case_File_Name (Main);

            if Base_Name (Main) /= Main then
               Fail_Program
                 ("mains cannot include directory information (""",
                  Display_Main,
                  """)");
            end if;

            Main_Id := Create_Name (Main);

            Source := Project_Tree.First_Source;
            Nmb := 0;

            while Source /= No_Source loop
               if Project_Tree.Sources.Table (Source).File = Main_Id then
                  Main_Sources.Set (Main_Id, Source);

                  if
                    Project_Tree.Sources.Table (Source).Project = Main_Project
                  then
                     Nmb := 1;
                     exit;

                  else
                     Nmb := Nmb + 1;
                  end if;
               end if;

               Source := Project_Tree.Sources.Table (Source).Next_In_Sources;
            end loop;

            if Nmb = 0 then
               Fail_Program
                 ("""",
                  Display_Main,
                  """ is not a source of any project");

            elsif Nmb > 1 then
               Fail_Program
                 ("""",
                  Display_Main,
                  """ is a source of several projects, but not of " &
                  "the main project");
            end if;
         end;
      end loop;

      Mains.Reset;

      loop
         declare
            Display_Main : constant String := Mains.Next_Main;
            Main         : String := Display_Main;
            Main_Id      : File_Name_Type;
            Source       : Source_Id;

            Project      : Project_Id;
            Data         : Project_Data;
            Root_Arr     : Array_Element_Id;
            Roots        : Variable_Value;
            Root_List    : Roots_Access;
            Nmb_Root     : Natural;
            Pat_Root     : Boolean;
            Root_Pattern : Regexp;
            Root_Found   : Boolean;
            Roots_Found  : Boolean;
            List         : String_List_Id;
            Elem         : String_Element;
            Unit_Name    : Name_Id;
            Root_Source  : Source_Id;
            Src_Data     : Source_Data;
         begin
            exit when Display_Main'Length = 0;

            Canonical_Case_File_Name (Main);
            Main_Id := Create_Name (Main);
            Source := Main_Sources.Get (Main_Id);

            if Source /= No_Source then
               Src_Data := Project_Tree.Sources.Table (Source);
               Project := Src_Data.Project;
               Queue.Insert
                 (Source_File_Name => Main_Id,
                  Source_Identity  => Source,
                  Source_Project   => Project);

               --  Look for roots if closure is needed

               if Closure_Needed then
                  if Current_Verbosity = High then
                     Write_Str ("Looking for roots for ");
                     Write_Line (Display_Main);
                  end if;

                  Data := Project_Tree.Projects.Table (Project);
                  Root_Arr :=
                    Prj.Util.Value_Of
                      (Name      => Name_Roots,
                       In_Arrays => Data.Decl.Arrays,
                       In_Tree   => Project_Tree);
                  Roots :=
                    Prj.Util.Value_Of
                      (Index     => Name_Id (Main_Id),
                       Src_Index => 0,
                       In_Array  => Root_Arr,
                       In_Tree   => Project_Tree);

                  --  If there is no roots for the specific main, try the
                  --  language.

                  if Roots = Nil_Variable_Value then
                     Roots :=
                       Prj.Util.Value_Of
                         (Index                  => Src_Data.Language_Name,
                          Src_Index              => 0,
                          In_Array               => Root_Arr,
                          In_Tree                => Project_Tree,
                          Force_Lower_Case_Index => True);
                  end if;

                  --  Then try "*"

                  if Roots = Nil_Variable_Value then
                     Roots :=
                       Prj.Util.Value_Of
                         (Index                  => Name_Star,
                          Src_Index              => 0,
                          In_Array               => Root_Arr,
                          In_Tree                => Project_Tree,
                          Force_Lower_Case_Index => True);
                  end if;

                  if Roots = Nil_Variable_Value then
                     if Current_Verbosity = High then
                        Write_Line ("   -> no roots declared");
                     end if;

                     --  If a non Ada main has no roots, then all sources need
                     --  to be compiled, so no need to check for closure.

                     if Src_Data.Lang_Kind /= Unit_Based then
                        Closure_Needed := False;
                     end if;

                  else
                     List := Roots.Values;
                     Nmb_Root := 0;

                     Pattern_Loop :
                     while List /= Nil_String loop
                        Elem := Project_Tree.String_Elements.Table (List);
                        Get_Name_String (Elem.Value);
                        To_Lower (Name_Buffer (1 .. Name_Len));
                        Unit_Name := Name_Find;

                        --  Check if it is a unit name or a pattern

                        Pat_Root := False;

                        for J in 1 .. Name_Len loop
                           if (Name_Buffer (J) not in 'a' .. 'z') and then
                             (Name_Buffer (J) not in '0' .. '9') and then
                             (Name_Buffer (J) /= '_') and then
                             (Name_Buffer (J) /= '.')
                           then
                              Pat_Root := True;
                              exit;
                           end if;
                        end loop;

                        if Pat_Root then
                           begin
                              Root_Pattern :=
                                Compile
                                  (Pattern => Name_Buffer (1 .. Name_Len),
                                   Glob    => True);

                           exception
                              when Error_In_Regexp =>
                                 Error_Msg_Name_1 := Unit_Name;
                                 Error_Msg
                                   ("invalid pattern %", Roots.Location);
                                 exit Pattern_Loop;
                           end;
                        end if;

                        Roots_Found := False;

                        Root_Source := Project_Tree.First_Source;
                        Source_Loop :
                        while Root_Source /= No_Source loop
                           Root_Found := False;
                           Src_Data :=
                             Project_Tree.Sources.Table (Root_Source);

                           if Pat_Root then
                              Root_Found :=
                                Src_Data.Unit /= No_Name and then
                                Match
                                  (Get_Name_String (Src_Data.Unit),
                                   Root_Pattern);

                           else
                              Root_Found := Src_Data.Unit = Unit_Name;
                           end if;

                           if Root_Found then
                              case Src_Data.Kind is
                                 when Impl =>
                                    null;

                                 when Spec =>
                                    Root_Found :=
                                      Src_Data.Other_Part = No_Source;

                                 when Sep =>
                                    Root_Found := False;
                              end case;
                           end if;

                           if Root_Found then
                              Roots_Found := True;

                              if Current_Verbosity = High then
                                 Write_Str ("   -> ");
                                 Write_Line
                                   (Get_Name_String
                                      (Project_Tree.Sources.Table
                                         (Root_Source).Display_File));
                              end if;

                              Queue.Insert
                                (Source_File_Name => Src_Data.File,
                                 Source_Identity  => Root_Source,
                                 Source_Project   => Src_Data.Project);

                              if Nmb_Root = Roots_Buffer'Last then
                                 declare
                                    New_Roots : constant Roots_Access :=
                                      new Root_Array
                                            (1 .. 2 * Roots_Buffer'Last);

                                 begin
                                    New_Roots (Roots_Buffer'Range) :=
                                      Roots_Buffer.all;
                                    Roots_Buffer := New_Roots;
                                 end;
                              end if;

                              Nmb_Root := Nmb_Root + 1;
                              Roots_Buffer (Nmb_Root) := Root_Source;
                              exit Source_Loop when not Pat_Root;
                           end if;

                           Root_Source := Src_Data.Next_In_Sources;
                        end loop Source_Loop;

                        if not Roots_Found then
                           if Pat_Root then
                              if not Quiet_Output then
                                 Error_Msg_Name_1 := Unit_Name;
                                 Error_Msg
                                   ("?no unit matches pattern %",
                                    Roots.Location);
                              end if;

                           else
                              --  report error
                              Error_Msg
                                ("Unit " & Get_Name_String (Unit_Name) &
                                 " does not exist",
                                 Roots.Location);
                           end if;
                        end if;

                        List := Elem.Next;
                     end loop Pattern_Loop;

                     Root_List :=
                       new Root_Array'(Roots_Buffer (1 .. Nmb_Root));
                     Root_Sources.Set (Main_Id, Root_List);
                  end if;
               end if;
            end if;
         end;
      end loop;

      if Total_Errors_Detected /= 0 then
         Fail_Program ("cannot continue");
      end if;
   end Check_Mains;

   -----------------------
   -- Compilation_Phase --
   -----------------------

   procedure Compilation_Phase is
      Source_File_Name : File_Name_Type;
      Source_Identity  : Source_Id;
      Source_Project   : Project_Id;
      Language         : Language_Index;
      Language_Name    : Name_Id;
      Config           : Language_Config;
      List             : Name_List_Index;
      Node             : Name_Node;
      Compiler_Path    : String_Access;
      Compiler_Name_Id : File_Name_Type;
      Pid              : Process_Id;
      Compilation_OK   : Boolean;

      Compilation_Needed : Boolean;

      Options_Instance         : Comp_Option_Table_Ref;
      Builder_Options_Instance : Builder_Comp_Option_Table_Ref;

      Current_Project      : Project_Id := No_Project;
      Current_Language_Ind : Language_Index := No_Language_Index;
      --  The project for which the include path environment has been set last,
      --  to avoid computing it several times.

      Mapping_File_Path : Path_Name_Type;

      Src_Data         : Source_Data;
      Dep_File         : Text_File;

      Start            : Natural;
      Finish           : Natural;
      Last_Obj         : Natural;

      Last_Recorded_Option : Integer;

      package Good_ALI is new Table.Table
        (Table_Component_Type => ALI.ALI_Id,
         Table_Index_Type     => Natural,
         Table_Low_Bound      => 1,
         Table_Initial        => 50,
         Table_Increment      => 100,
         Table_Name           => "Buildgpr.Good_ALI");
      --  Contains the set of valid ALI files that have not yet been scanned

      procedure Add_Config_File_Switch
        (Config    : Language_Config;
         Path_Name : Path_Name_Type);

      function Get_Next_Good_ALI return ALI.ALI_Id;
      --  Returns the next good ALI_Id record

      function Good_ALI_Present return Boolean;
      --  Returns True if any ALI file was recorded in the previous set

      procedure Record_ALI_For (Source_Identity : Source_Id);
      --  Record the Id of an ALI file in Good_ALI table

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
            Nam := Project_Tree.Name_Lists.Table (List);
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

      -----------------------
      -- Get_Next_Good_ALI --
      -----------------------

      function Get_Next_Good_ALI return ALI.ALI_Id is
         Result : ALI.ALI_Id;

      begin
         pragma Assert (Good_ALI_Present);
         Result := Good_ALI.Table (Good_ALI.Last);
         Good_ALI.Decrement_Last;
         return Result;
      end Get_Next_Good_ALI;

      ----------------------
      -- Good_ALI_Present --
      ----------------------

      function Good_ALI_Present return Boolean is
      begin
         return Good_ALI.First <= Good_ALI.Last;
      end Good_ALI_Present;

      ---------------------
      -- Record_Good_ALI --
      ---------------------

      procedure Record_ALI_For (Source_Identity : Source_Id) is
         Src_Data : constant Source_Data :=
                      Project_Tree.Sources.Table (Source_Identity);
         The_ALI  : ALI.ALI_Id;
         Text     : Text_Buffer_Ptr :=
                      Read_Library_Info
                        (File_Name_Type (Src_Data.Dep_Path));

      begin
         if Text /= null then
            --  Read the ALI file but read only the necessary lines.

            The_ALI :=
              ALI.Scan_ALI
                (File_Name_Type (Src_Data.Dep_Path),
                 Text,
                 Ignore_ED     => False,
                 Err           => True,
                 Read_Lines    => "W");
            Free (Text);
            Good_ALI.Increment_Last;
            Good_ALI.Table (Good_ALI.Last) := The_ALI;
         end if;
      end Record_ALI_For;

   --  Start of processing for Compilation_Phase

   begin
      Outstanding_Compiles := 0;

      Compilation_Loop :
      while not Queue.Is_Empty or else Outstanding_Compiles > 0 loop

         --  If the user does not want to keep going in case of errors then
         --  wait for the remaining outstanding compiles and then exit.

         if Bad_Compilations.Last > 0 and then not Keep_Going then
            while Outstanding_Compiles > 0 loop
               Await_Compile
                  (Source_Identity, Compilation_OK);

               if not Compilation_OK then
                  Record_Failure (Source_Identity);
               end if;
            end loop;

            exit Compilation_Loop;
         end if;

         --  PHASE 1: Check if there is more work that we can do (ie the Queue
         --  is non empty). If there is, do it only if we have not yet used
         --  up all the available processes.

         if not Queue.Is_Empty and then
           Outstanding_Compiles < Maximum_Processes
         then
            Queue.Extract (Source_File_Name, Source_Identity, Source_Project);

            Source_Project := Ultimate_Extending_Project_Of (Source_Project);

            Change_To_Object_Directory (Source_Project);

            Initialize_Source_Record (Source_Identity, Need_Object => True);

            Compilation_Needed := Need_To_Compile (Source_Identity);

            if Compilation_Needed or Check_Switches then
               Language :=
                 Project_Tree.Sources.Table (Source_Identity).Language;
               Language_Name :=
                 Project_Tree.Sources.Table (Source_Identity).Language_Name;

               Config := Project_Tree.Languages_Data.Table (Language).Config;
               Compiler_Name_Id :=  Config.Compiler_Driver;
               Compiler_Path := Config.Compiler_Driver_Path;

               --  If this is the first time we try this compiler, then get its
               --  path name.

               if Compiler_Path = null then
                  declare
                     Compiler_Name     : constant String :=
                                           Get_Name_String (Compiler_Name_Id);
                  begin
                     Compiler_Path := Locate_Exec_On_Path (Compiler_Name);

                     if Compiler_Path = null then
                        Fail_Program
                          ("unable to locate """, Compiler_Name, """");

                     else
                        Project_Tree.Languages_Data.Table
                          (Language).Config.Compiler_Driver_Path :=
                          Compiler_Path;
                     end if;
                  end;
               end if;

               --  Compilation Switches

               Compilation_Options.Last := 0;

               --  1) The required switches

               declare
                  List : Name_List_Index := Config.Compiler_Required_Switches;
                  Nam_Nod : Name_Node;
                  First   : Boolean;

               begin
                  First := True;
                  while List /= No_Name_List loop
                     Nam_Nod := Project_Tree.Name_Lists.Table (List);
                     Add_Option
                       (Nam_Nod.Name,
                        To   => Compilation_Options,
                        Display => First or Verbose_Mode);
                     First := False;
                     List := Nam_Nod.Next;
                  end loop;
               end;

               --  2) the compilation switches specified in package Builder for
               --  all compilers, following "-cargs", if any.

               if All_Language_Builder_Compiling_Options.Last /= 0 then
                  declare
                     Options :
                       String_List
                         (1 .. All_Language_Builder_Compiling_Options.Last);

                  begin
                     for Index in Options'Range loop
                        Options (Index) :=
                          All_Language_Builder_Compiling_Options.Table (Index);
                     end loop;

                     Add_Options
                       (Options,
                        To            => Compilation_Options,
                        Display_All   => True,
                        Display_First => True);
                  end;
               end if;

               --  3) the compilation switches specified in package Builder for
               --  the compiler of the language, following -cargs:<language>.

               Builder_Options_Instance :=
                 Builder_Compiling_Options_HTable.Get (Language_Name);

               if Builder_Options_Instance /= No_Builder_Comp_Option_Table then
                  declare
                     Options : String_List
                                 (1 .. Builder_Compiling_Options.Last
                                         (Builder_Options_Instance.all));

                  begin
                     for Index in Options'Range loop
                        Options (Index) :=
                          Builder_Options_Instance.Table (Index);
                     end loop;

                     Add_Options
                       (Options,
                        To            => Compilation_Options,
                        Display_All   => True,
                        Display_First => True);
                  end;
               end if;

               --  4) The PIC option if it exists, for shared libraries

               if Project_Tree.Projects.Table (Source_Project).Library
                 and then
                   Project_Tree.Projects.Table (Source_Project).Library_Kind /=
                     Static
                 and then
                   Config.Compilation_PIC_Option /= No_Name_List
               then
                  declare
                     List : Name_List_Index := Config.Compilation_PIC_Option;
                     Nam_Nod : Name_Node;

                  begin
                     while List /= No_Name_List loop
                        Nam_Nod := Project_Tree.Name_Lists.Table (List);
                        Add_Option
                          (Nam_Nod.Name,
                           To   => Compilation_Options,
                           Display => True);
                        List := Nam_Nod.Next;
                     end loop;
                  end;
               end if;

               --  5) Compiler'Switches(<source file name>), if it is defined,
               --  otherwise Compiler'Switches (<language name>), if defined.

               Add_Compilation_Switches (Source_Identity);

               --  4) the switches specified on the gprbuild command line for
               --  all compilers, following "-cargs", if any.

               if All_Language_Compiling_Options.Last /= 0 then
                  declare
                     Options : String_List
                                 (1 .. All_Language_Compiling_Options.Last);

                  begin
                     for Index in Options'Range loop
                        Options (Index) :=
                          All_Language_Compiling_Options.Table (Index);
                     end loop;

                     Add_Options
                       (Options,
                        To            => Compilation_Options,
                        Display_All   => True,
                        Display_First => True);
                  end;
               end if;

               --  6) the switches specified on the gprbuild command line for
               --  the compiler of the language, following -cargs:<language>.

               Options_Instance :=
                 Compiling_Options_HTable.Get (Language_Name);

               if Options_Instance /= No_Comp_Option_Table then
                  declare
                     Options : String_List
                       (1 .. Compiling_Options.Last (Options_Instance.all));

                  begin
                     for Index in Options'Range loop
                        Options (Index) := Options_Instance.Table (Index);
                     end loop;

                     Add_Options
                       (Options,
                        To            => Compilation_Options,
                        Display_All   => True,
                        Display_First => True);
                  end;
               end if;

               if Check_Switches and then not Compilation_Needed then
                  --  Check switches

                  declare
                     File : Ada.Text_IO.File_Type;
                     Line : String (1 .. 1_000);
                     Last : Natural;
                  begin
                     Open
                       (File,
                        In_File,
                        Get_Name_String
                          (Project_Tree.Sources.Table (Source_Identity).
                                                         Switches_Path));

                     if End_Of_File (File) then
                        if Verbose_Mode then
                           Write_Line
                             ("    -> switches file is empty");
                        end if;

                        Compilation_Needed := True;

                     else
                        Get_Line (File, Line, Last);

                        if Line (1 .. Last) /=
                          String (Project_Tree.Sources.Table
                                  (Source_Identity).Object_TS)
                        then
                           if Verbose_Mode then
                              Write_Line
                                ("    -> different object file timestamp");
                              Write_Line
                                ("       " & Line (1 .. Last));
                              Write_Line
                                ("       " &
                                 String (Project_Tree.Sources.Table
                                   (Source_Identity).Object_TS));
                           end if;

                           Compilation_Needed := True;
                        end if;
                     end if;

                     if not Compilation_Needed then
                        for Index in 1 .. Compilation_Options.Last loop
                           if End_Of_File (File) then
                              if Verbose_Mode then
                                 Write_Line
                                   ("    -> more switches");
                              end if;

                              Compilation_Needed := True;
                              exit;
                           end if;

                           Get_Line (File, Line, Last);

                           if Line (1 .. Last) /=
                             Compilation_Options.Options (Index).all
                           then
                              if Verbose_Mode then
                                 Write_Line
                                   ("    -> different switches");
                              end if;

                              Compilation_Needed := True;
                              exit;
                           end if;
                        end loop;
                     end if;

                     if not Compilation_Needed then
                        if End_Of_File (File) then
                           if Verbose_Mode then
                              Write_Line ("    -> up to date");
                           end if;

                        else
                           if Verbose_Mode then
                              Write_Line ("    -> less switches");
                           end if;

                           Compilation_Needed := True;
                        end if;
                     end if;

                     Close (File);

                  exception
                     when others =>
                        if Verbose_Mode then
                           Write_Line ("    -> no switches file");
                        end if;

                        Compilation_Needed := True;
                  end;
               end if;
            end if;

            if Compilation_Needed then
               --  Update, if necessary, the path of the object file, of the
               --  dependency file and of the switches file, in the case of the
               --  compilation of a source in an extended project, when the
               --  source is in a project being extended.

               Project_Tree.Sources.Table (Source_Identity).Object_Project :=
                 Source_Project;

               if Source_Project /=
                 Project_Tree.Sources.Table (Source_Identity).Project
               then
                  Get_Name_String
                    (Project_Tree.Projects.Table
                       (Source_Project).Object_Directory);
                  Name_Len := Name_Len + 1;
                  Name_Buffer (Name_Len) := Directory_Separator;
                  Add_Str_To_Name_Buffer
                    (Get_Name_String
                       (Project_Tree.Sources.Table (Source_Identity).Object));
                  Project_Tree.Sources.Table (Source_Identity).Object_Path :=
                    Name_Find;

                  Get_Name_String
                    (Project_Tree.Projects.Table
                       (Source_Project).Object_Directory);
                  Name_Len := Name_Len + 1;
                  Name_Buffer (Name_Len) := Directory_Separator;
                  Add_Str_To_Name_Buffer
                    (Get_Name_String
                       (Project_Tree.Sources.Table
                          (Source_Identity).Dep_Name));
                  Project_Tree.Sources.Table (Source_Identity).Dep_Path :=
                    Name_Find;

                  Get_Name_String
                    (Project_Tree.Projects.Table
                       (Source_Project).Object_Directory);
                  Name_Len := Name_Len + 1;
                  Name_Buffer (Name_Len) := Directory_Separator;
                  Add_Str_To_Name_Buffer
                    (Get_Name_String
                       (Project_Tree.Sources.Table
                          (Source_Identity).Switches));
                  Project_Tree.Sources.Table (Source_Identity).Switches_Path :=
                    Name_Find;
               end if;

               --  Record the last recorded option index, to be able to write
               --  the switches file later.

               if Config.Object_Generated then
                  Last_Recorded_Option := Compilation_Options.Last;

               else
                  Last_Recorded_Option := -1;
               end if;

               --  Add dependency option, if there is one

               List := Project_Tree.Languages_Data.Table (Language).
                                                      Config.Dependency_Option;

               if List /= No_Name_List then
                  loop
                     Node := Project_Tree.Name_Lists.Table (List);
                     List := Node.Next;

                     if List = No_Name_List then
                        declare
                           Dep_Name : constant File_Name_Type :=
                                        Project_Tree.Sources.Table
                                          (Source_Identity).Dep_Name;
                           Switch : constant String :=
                                      Get_Name_String (Node.Name) &
                                      Get_Name_String (Dep_Name);

                        begin
                           Add_Option
                             (Switch,
                              To      => Compilation_Options,
                              Display => Opt.Verbose_Mode);
                           exit;
                        end;

                     else
                        Add_Option
                          (Value   => Node.Name,
                           To      => Compilation_Options,
                           Display => Opt.Verbose_Mode);
                     end if;
                  end loop;
               end if;

               Add_Option
                 (Get_Name_String
                    (Project_Tree.Sources.Table (Source_Identity).Path),
                  To   => Compilation_Options,
                  Display => True,
                  Simple_Name => not Verbose_Mode);

               --  Set the environment or additional switches for visibility
               --  on source directories.

               if Source_Project /= Current_Project or else
                  Language /= Current_Language_Ind
               then
                  Current_Project      := Source_Project;
                  Current_Language_Ind := Language;

                  if Project_Tree.Projects.Table
                    (Source_Project).Include_Language /= Language
                    and then
                      (Config.Include_Option /= No_Name_List
                       or else
                         Config.Include_Path /= No_Name
                       or else
                         Config.Include_Path_File /= No_Name)
                  then
                     Project_Tree.Projects.Table
                       (Source_Project).Include_Language := Language;

                     declare
                        Index : Positive;
                        NL    : Name_List_Index;

                     begin
                        Index := 1;
                        NL := Config.Include_Compatible_Languages;
                        while NL /= No_Name_List loop
                           Index := Index + 1;
                           NL := Project_Tree.Name_Lists.Table (NL).Next;
                        end loop;

                        declare
                           Languages : Name_Ids (1 .. Index);

                        begin
                           Index := 1;
                           Languages (Index) := Language_Name;

                           NL := Config.Include_Compatible_Languages;
                           while NL /= No_Name_List loop
                              Index := Index + 1;
                              Languages (Index) :=
                                Project_Tree.Name_Lists.Table (NL).Name;
                              NL := Project_Tree.Name_Lists.Table (NL).Next;
                           end loop;

                           Get_Directories
                             (Source_Project,
                              Sources   => True,
                              Languages => Languages);
                        end;
                     end;

                     if Config.Include_Option /= No_Name_List then
                        --  Get the value of Imported_Directories_Switches

                        declare
                           List : Name_List_Index;
                           Nam  : Name_Node;

                        begin
                           Include_Options.Last := 0;

                           for Index in 1 .. Directories.Last loop
                              List := Config.Include_Option;

                              loop
                                 Nam := Project_Tree.Name_Lists.Table (List);
                                 exit when Nam.Next = No_Name_List;

                                 Add_Option
                                   (Nam.Name,
                                    To => Include_Options,
                                    Display => Opt.Verbose_Mode);
                                 List := Nam.Next;
                              end loop;

                              Get_Name_String (Directories.Table (Index));

                              while Name_Len > 1 and then
                                (Name_Buffer (Name_Len) =
                                   Directory_Separator
                                 or else Name_Buffer (Name_Len) = '/')
                              loop
                                 Name_Len := Name_Len - 1;
                              end loop;

                              Add_Option
                                (Get_Name_String (Nam.Name) &
                                 Name_Buffer (1 .. Name_Len),
                                 To => Include_Options,
                                 Display => Opt.Verbose_Mode);
                           end loop;
                        end;

                        Project_Tree.Projects.Table
                          (Source_Project).Imported_Directories_Switches :=
                          new String_List'
                            (Include_Options.Options
                                 (1 .. Include_Options.Last));

                     elsif Config.Include_Path_File /= No_Name then
                        --  Create temp path file and store its name in
                        --  Include_Path_File.

                        declare
                           FD     : File_Descriptor;
                           Len    : Integer;
                           Status : Boolean;
                        begin
                           Prj.Env.Create_New_Path_File
                             (In_Tree   => Project_Tree,
                              Path_FD   => FD,
                              Path_Name =>
                                Project_Tree.Projects.Table
                                  (Source_Project).Include_Path_File);

                           for Index in 1 .. Directories.Last loop
                              Get_Name_String
                                (Directories.Table (Index));
                              Name_Len := Name_Len + 1;
                              Name_Buffer (Name_Len) := ASCII.LF;

                              Len :=
                                Write (FD, Name_Buffer (1)'Address, Name_Len);

                              if Len /= Name_Len then
                                 Fail_Program ("disk full");
                              end if;
                           end loop;

                           Close (FD, Status);

                           if not Status then
                              Fail_Program ("disk full");
                           end if;
                        end;

                     elsif Config.Include_Path /= No_Name then
                        --  Get the value of Include_Path

                        if Path_Buffer = null then
                           Path_Buffer :=
                             new String (1 .. Path_Buffer_Initial_Length);
                        end if;

                        Path_Last := 0;

                        for Index in 1 .. Directories.Last loop
                           if Path_Last /= 0 then
                              Add_To_Path (Path_Separator);
                           end if;

                           Get_Name_String (Directories.Table (Index));

                           while Name_Len > 1 and then
                                 (Name_Buffer (Name_Len) = Directory_Separator
                                  or else Name_Buffer (Name_Len) = '/')
                           loop
                              Name_Len := Name_Len - 1;
                           end loop;

                           Add_To_Path (Name_Buffer (1 .. Name_Len));
                        end loop;

                        Project_Tree.Projects.Table
                          (Source_Project).Include_Path :=
                          new String'(Path_Buffer (1 .. Path_Last));
                     end if;
                  end if;

                  if Config.Include_Path_File /= No_Name then
                     Setenv (Get_Name_String (Config.Include_Path_File),
                             Get_Name_String
                               (Project_Tree.Projects.Table
                               (Source_Project).Include_Path_File));

                  elsif Config.Include_Path /= No_Name then
                     Setenv (Get_Name_String (Config.Include_Path),
                             Project_Tree.Projects.Table
                               (Source_Project).Include_Path.all);

                     if Verbose_Mode then
                        Write_Str (Get_Name_String (Config.Include_Path));
                        Write_Str (" = ");
                        Write_Line
                          (Project_Tree.Projects.Table (Source_Project).
                                                          Include_Path.all);
                     end if;
                  end if;
               end if;

               --  If Include_Option is specified, add the options for the
               --  project.

               if Config.Include_Option /= No_Name_List then
                  Add_Options
                    (Project_Tree.Projects.Table
                       (Source_Project).Imported_Directories_Switches.all,
                     To   => Compilation_Options,
                     Display_All   => Opt.Verbose_Mode,
                     Display_First => False);
               end if;

               --  If Config_File_Switches is specified, check if a config
               --  file need to be specified.

               if Config.Config_File_Switches /= No_Name_List and then
                 (Config.Config_Body         /= No_Name or else
                  Config.Config_Spec         /= No_Name or else
                  Config.Config_Body_Pattern /= No_Name or else
                  Config.Config_Spec_Pattern /= No_Name)
               then
                  Create_Config_File
                    (For_Project => Source_Project,
                     Config      => Config,
                     Language    => Language_Name);

                  if Project_Tree.Projects.Table
                       (Source_Project).Config_File_Name /= No_Path
                  then
                     Add_Config_File_Switch
                       (Config => Config,
                        Path_Name =>
                          Project_Tree.Projects.Table (Source_Project).
                                                         Config_File_Name);
                  end if;

                  if not Config.Config_File_Unique then
                     declare
                        Config_File_Path : Path_Name_Type;

                     begin
                        Config_File_Path :=
                          Config_File_For
                            (Project        => Main_Project,
                             Package_Name   => Name_Builder,
                             Attribute_Name => Name_Global_Config_File,
                             Language       => Language_Name);

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
                             Language       => Language_Name);

                        if Config_File_Path /= No_Path then
                           Add_Config_File_Switch
                             (Config    => Config,
                              Path_Name => Config_File_Path);
                        end if;
                     end;
                  end if;
               end if;

               --  If the compiler supports mapping files, add the necessary
               --  switch.

               if Config.Mapping_File_Switches /= No_Name_List then
                  Mapping_File_Path :=
                    Mapping_Files_Htable.Get_First
                      (Project_Tree.Languages_Data.Table
                           (Language).Mapping_Files);

                  if Mapping_File_Path /= No_Path then
                     Mapping_Files_Htable.Remove
                       (Project_Tree.Languages_Data.Table
                          (Language).Mapping_Files,
                        Mapping_File_Path);

                  else
                     Prj.Env.Create_Mapping_File
                       (Project  => Source_Project,
                        Language => Language_Name,
                        In_Tree  => Project_Tree,
                        Name     => Mapping_File_Path);
                     Mapping_Files_Htable.Set
                       (Project_Tree.Languages_Data.Table
                          (Language).Mapping_Files,
                        Mapping_File_Path,
                        Mapping_File_Path);
                  end if;

                  declare
                     List : Name_List_Index := Config.Mapping_File_Switches;
                     Nam_Nod : Name_Node;
                  begin
                     while List /= No_Name_List loop
                        Nam_Nod := Project_Tree.Name_Lists.Table (List);
                        List := Nam_Nod.Next;

                        if List /= No_Name_List then
                           Add_Option
                             (Value   => Nam_Nod.Name,
                              To      => Compilation_Options,
                              Display => Opt.Verbose_Mode);

                        else
                           Get_Name_String (Nam_Nod.Name);
                           Add_Str_To_Name_Buffer
                             (Get_Name_String (Mapping_File_Path));
                           Add_Option
                             (Name_Buffer (1 .. Name_Len),
                              To => Compilation_Options,
                              Display => Opt.Verbose_Mode);
                        end if;
                     end loop;
                  end;

               else
                  Mapping_File_Path := No_Path;
               end if;

               --  Finally the specification of the object file

               if Config.Object_Generated then
                  Add_Option
                    ("-o", To => Compilation_Options, Display => True);
                  Add_Option
                    (Name_Id
                       (Project_Tree.Sources.Table (Source_Identity).Object),
                     To      => Compilation_Options,
                     Display => True);
               end if;

               --  Display the command invoked if not in quiet output mode

               if not Quiet_Output then
                  if Verbose_Mode then
                     Write_Str (Compiler_Path.all);

                  else
                     Name_Len := 0;
                     Add_Str_To_Name_Buffer (Base_Name (Compiler_Path.all));

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
                             (Base_Name
                                (Compilation_Options.Options (Option).all));

                        else
                           Write_Str
                             (Compilation_Options.Options (Option).all);
                        end if;
                     end if;
                  end loop;

                  Write_Eol;
               end if;

               Pid := GNAT.OS_Lib.Non_Blocking_Spawn
                 (Compiler_Path.all,
                  Compilation_Options.Options
                    (1 .. Compilation_Options.Last));

               declare
                  Options : String_List_Access := null;
               begin
                  if Last_Recorded_Option >= 0 then
                     Options :=
                       new String_List'
                         (Compilation_Options.Options
                              (1 .. Last_Recorded_Option));
                  end if;

                  Add_Process
                    (Pid,
                     Source_Identity,
                     Mapping_File_Path,
                     Compilation,
                     Options);
               end;

               Need_To_Rebuild_Global_Archives := True;

            elsif Closure_Needed and then
                  Project_Tree.Sources.Table
                     (Source_Identity).Dependency = ALI_File
            then
               Record_ALI_For (Source_Identity);
            end if;
         end if;

         --  PHASE 2: Now check if we should wait for a compilation to
         --  finish. This is the case if all the available processes are
         --  busy compiling sources or there is nothing else to do
         --  (that is the Q is empty and there are outstanding compilations).

         if Outstanding_Compiles = Maximum_Processes
           or else (Queue.Is_Empty and then Outstanding_Compiles > 0)
         then
            Await_Compile (Source_Identity, Compilation_OK);

            if Compilation_OK then
               --  Check if dependencies are on sources in Interfaces and,
               --  when --direct-import-only is used, the imported sources
               --  come from directly withed projects.

               Imports.Reset;
               Included_Sources.Set_Last (0);

               Src_Data := Project_Tree.Sources.Table (Source_Identity);

               case Src_Data.Dependency is
               when None =>
                  null;

               when Makefile =>
                  Open (Dep_File, Get_Name_String (Src_Data.Dep_Path));

                  if Is_Valid (Dep_File) then

                     Big_Loop :
                     loop
                        declare
                           End_Of_File_Reached : Boolean := False;

                        begin
                           loop
                              if End_Of_File (Dep_File) then
                                 End_Of_File_Reached := True;
                                 exit;
                              end if;

                              Get_Line (Dep_File, Name_Buffer, Name_Len);

                              exit when Name_Len > 0
                                and then Name_Buffer (1) /= '#';
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

                                 while Finish < Last and then
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

                                    Src_Data_2 : Source_Data;
                                 begin

                                    Name_Len := 0;
                                    Add_Str_To_Name_Buffer (Src_Name);
                                    Source_2 := Source_Paths_Htable.Get
                                      (Project_Tree.Source_Paths_HT,
                                       Name_Find);

                                    if Source_2 /= No_Source then
                                       Src_Data_2 :=
                                         Project_Tree.Sources.Table (Source_2);

                                       --  It is a source of a project

                                       if (not Project_Extends
                                           (Src_Data.Project,
                                            Src_Data_2.Project))
                                       then
                                          --  It is not a source of the same
                                          --  project as the source just
                                          --  compiled. Check if it can be
                                          --  imported.

                                          if Direct_Import_Only then
                                             if Directly_Imports
                                               (Src_Data.Project,
                                                Src_Data_2.Project)
                                             then
                                                --  It is a source of a
                                                --  directly imported project.
                                                --  Record its project, for
                                                --  later processing.

                                                Imports.Set
                                                  (Src_Data_2.Project, True);

                                             else
                                                --  It is a source of a project
                                                --  that is not directly
                                                --  imported. Record the source
                                                --  for later processing.

                                                Included_Sources.Append
                                                  (Source_2);
                                             end if;
                                          end if;

                                          if
                                            (not Src_Data_2.In_Interfaces)
                                          then
                                             --  It is not a source in the
                                             --  interfaces of its project.
                                             --  Report an error and invalidate
                                             --  the compilation.
                                             Write_Char ('"');
                                             Write_Str
                                               (Get_Name_String
                                                  (Src_Data.Path));
                                             Write_Str
                                               (""" cannot import """);
                                             Write_Str (Src_Name);
                                             Write_Line (""":");

                                             Write_Str
                                               ("  it is not part of the " &
                                                "interfaces of its " &
                                                "project """);
                                             Write_Str
                                               (Get_Name_String
                                                  (Project_Tree.Projects.Table
                                                     (Src_Data_2.Project).
                                                     Display_Name));
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
                           L : Project_List :=
                                 Project_Tree.Projects.Table
                                   (Src_Data.Project).Imported_Projects;
                           E : Project_Element;

                           Src_Data_2 : Source_Data;

                        begin

                           --  Put in hash table Imports the project trees
                           --  rooted at the projects that are already in
                           --  Imports.

                           while L /= Empty_Project_List loop
                              E := Project_Tree.Project_Lists.Table (L);

                              if Imports.Get (E.Project) then
                                 Recursive_Import (E.Project);
                              end if;

                              L := E.Next;
                           end loop;

                           --  For all the imported sources from project not
                           --  directly imported, check if their projects are
                           --  in has thable imports.

                           for J in 1 .. Included_Sources.Last loop
                              Src_Data_2 :=
                                Project_Tree.Sources.Table
                                  (Included_Sources.Table (J));

                              if not Imports.Get (Src_Data_2.Project) then
                                 --  This source is either directly imported or
                                 --  imported from another source that should
                                 --  not be imported. Report an error and
                                 --  invalidate the compilation.

                                 Write_Char ('"');
                                 Write_Str
                                   (Get_Name_String
                                      (Src_Data.Path));
                                 Write_Str
                                   (""" cannot import """);
                                 Write_Str (Get_Name_String (Src_Data_2.Path));
                                 Write_Line (""":");

                                 Write_Str ("  """);
                                 Write_Str
                                   (Get_Name_String
                                      (Project_Tree.Projects.Table
                                         (Src_Data.Project).
                                         Display_Name));
                                 Write_Str
                                   (""" does not directly " &
                                    "import project """);
                                 Write_Str
                                   (Get_Name_String
                                      (Project_Tree.Projects.Table
                                         (Src_Data_2.Project).
                                         Display_Name));
                                 Write_Line ("""");

                                 Compilation_OK := False;
                              end if;
                           end loop;
                        end;
                     end if;
                  end if;

               when ALI_File =>
                  declare
                     use type ALI.ALI_Id;
                     Text       : Text_Buffer_Ptr :=
                                    Read_Library_Info
                                      (File_Name_Type (Src_Data.Dep_Path));
                     The_ALI    : ALI.ALI_Id;
                     Sfile      : File_Name_Type;
                     Afile      : File_Name_Type;
                     Source_2   : Source_Id;
                     Src_Data_2 : Source_Data;

                  begin
                     if Text /= null then
                        --  Read the ALI file but read only the necessary lines

                        The_ALI :=
                          ALI.Scan_ALI
                            (File_Name_Type (Src_Data.Dep_Path),
                             Text,
                             Ignore_ED     => False,
                             Err           => True,
                             Read_Lines    => "W");
                        Free (Text);

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
                                    Source_2 := Project_Tree.First_Source;

                                    while Source_2 /= No_Source loop
                                       Initialize_Source_Record (Source_2);
                                       Src_Data_2 :=
                                         Project_Tree.Sources.Table (Source_2);

                                       if Src_Data_2.Compiled and then
                                         Src_Data_2.Dep_Name = Afile
                                       then
                                          case Src_Data_2.Kind is
                                          when Spec =>
                                             null;

                                          when Impl =>
                                             if Is_Subunit (Src_Data_2) then
                                                Source_2 := No_Source;
                                             end if;

                                          when Sep =>
                                             Source_2 := No_Source;
                                          end case;

                                          exit;
                                       end if;

                                       Source_2 := Src_Data_2.Next_In_Sources;
                                    end loop;

                                    --  If it is the source of a project that
                                    --  is not the project of the source just
                                    --  compiled, check if it is allowed to be
                                    --  imported.

                                    if Source_2 /= No_Source
                                      and then
                                        (not Project_Extends
                                             (Src_Data.Project,
                                              Src_Data_2.Project))
                                    then
                                       if Direct_Import_Only and then
                                         not Directly_Imports
                                           (Src_Data.Project,
                                            Src_Data_2.Project)
                                       then
                                          --  It is in a project that is not
                                          --  directly imported. Report an
                                          --  error and invalidate the
                                          --  compilation.

                                          Write_Str ("Unit """);
                                          Write_Str
                                            (Get_Name_String (Src_Data.Unit));
                                          Write_Str
                                            (""" cannot import unit """);
                                          Write_Str
                                            (Get_Name_String
                                               (Src_Data_2.Unit));
                                          Write_Line (""":");

                                          Write_Str ("  """);
                                          Write_Str
                                            (Get_Name_String
                                               (Project_Tree.Projects.Table
                                                  (Src_Data.Project).
                                                  Display_Name));
                                          Write_Str
                                            (""" does not directly " &
                                             "import project """);
                                          Write_Str
                                            (Get_Name_String
                                               (Project_Tree.Projects.Table
                                                  (Src_Data_2.Project).
                                                  Display_Name));
                                          Write_Line ("""");

                                          Compilation_OK := False;

                                       elsif not Src_Data_2.In_Interfaces then
                                          --  It is not an interface of its
                                          --  project. Report an error and
                                          --  invalidate the compilation.

                                          Write_Str ("Unit """);
                                          Write_Str
                                            (Get_Name_String (Src_Data.Unit));
                                          Write_Str
                                            (""" cannot import unit """);
                                          Write_Str
                                            (Get_Name_String
                                               (Src_Data_2.Unit));
                                          Write_Line (""":");

                                          Write_Str
                                            ("  it is not part of the " &
                                             "interfaces of its project """);
                                          Write_Str
                                            (Get_Name_String
                                               (Project_Tree.Projects.Table
                                                  (Src_Data_2.Project).
                                                  Display_Name));
                                          Write_Line ("""");
                                          Compilation_OK := False;
                                       end if;
                                    end if;
                                 end if;
                              end loop;
                           end loop;
                        end if;
                     end if;
                  end;
               end case;

               --  If the compilation was invalidated, delete the compilation
               --  artifacts.

               if not Compilation_OK then
                  declare
                     No_Check : Boolean;
                  begin
                     if Src_Data.Dep_Path /= No_Path then
                        Delete_File
                          (Get_Name_String (Src_Data.Dep_Path), No_Check);
                     end if;

                     if Src_Data.Object_Path /= No_Path then
                        Delete_File
                          (Get_Name_String (Src_Data.Object_Path), No_Check);
                     end if;

                     if Src_Data.Switches_Path /= No_Path then
                        Delete_File
                          (Get_Name_String (Src_Data.Switches_Path), No_Check);
                     end if;
                  end;
               end if;
            end if;

            if not Compilation_OK then
               Record_Failure (Source_Identity);

            elsif Closure_Needed and then
              Project_Tree.Sources.Table
                (Source_Identity).Dependency = ALI_File
            then
               Record_ALI_For (Source_Identity);
            end if;
         end if;

         --  Phase 3: Check if we recorded good ALI files. If yes process
         --  them now in the order in which they have been recorded. There
         --  are two occasions in which we record good ali files. The first is
         --  in phase 1 when, after scanning an existing ALI file we realize
         --  it is up-to-date, the second instance is after a successful
         --  compilation.

         declare
            The_ALI : ALI.ALI_Id;
            Sfile   : File_Name_Type;
            Afile   : File_Name_Type;
            Src_Id  : Source_Id;
            Source  : Source_Data;

         begin
            while Good_ALI_Present loop
               The_ALI := Get_Next_Good_ALI;

               --  Now insert in the Q the unmarked source files (i.e. those
               --  which have never been inserted in the Q and hence never
               --  considered).

               for J in ALI.ALIs.Table (The_ALI).First_Unit ..
                        ALI.ALIs.Table (The_ALI).Last_Unit
               loop
                  for K in ALI.Units.Table (J).First_With ..
                           ALI.Units.Table (J).Last_With
                  loop
                     Sfile := ALI.Withs.Table (K).Sfile;

                     --  Skip generics

                     if Sfile /= No_File then
                        Afile := ALI.Withs.Table (K).Afile;
                        Src_Id := Project_Tree.First_Source;

                        while Src_Id /= No_Source loop
                           Initialize_Source_Record (Src_Id);
                           Source := Project_Tree.Sources.Table (Src_Id);

                           if Source.Compiled and then
                              Source.Dep_Name = Afile
                           then
                              case Source.Kind is
                              when Spec =>
                                 if Source.Other_Part /= No_Source then
                                    Src_Id := Source.Other_Part;
                                 end if;

                              when Impl =>
                                 if Is_Subunit (Source) then
                                    Src_Id := No_Source;
                                 end if;

                              when Sep =>
                                 Src_Id := No_Source;
                              end case;

                              exit;
                           end if;

                           Src_Id := Source.Next_In_Sources;
                        end loop;

                        if Src_Id /= No_Source then
                           Queue.Insert
                             (Sfile,
                              Src_Id,
                              Source.Project);
                        end if;
                     end if;
                  end loop;
               end loop;
            end loop;
         end;

         if Display_Compilation_Progress then
            Write_Str ("completed ");
            Write_Int (Int (Queue.First - 1));
            Write_Str (" out of ");
            Write_Int (Int (Queue.Size));
            Write_Str (" (");
            Write_Int (Int (((Queue.First - 1) * 100) / Queue.Size));
            Write_Str ("%)...");
            Write_Eol;
         end if;
      end loop Compilation_Loop;
   end Compilation_Phase;

   -----------------------------------
   -- Compute_All_Imported_Projects --
   -----------------------------------

   procedure Compute_All_Imported_Projects (Project : Project_Id) is
      procedure Add_To_List (Prj : Project_Id);
      --  Add a project to the list All_Imported_Projects of project Project

      procedure Recursive_Add_Imported (Project : Project_Id);
      --  Recursively add the projects imported by project Project, but not
      --  those that are extended.

      -----------------
      -- Add_To_List --
      -----------------

      procedure Add_To_List (Prj : Project_Id) is
         Element : constant Project_Element :=
           (Prj, Project_Tree.Projects.Table (Project).All_Imported_Projects);
         List : Project_List;
      begin
         Project_List_Table.Increment_Last (Project_Tree.Project_Lists);
         List := Project_List_Table.Last (Project_Tree.Project_Lists);
         Project_Tree.Project_Lists.Table (List) := Element;
         Project_Tree.Projects.Table (Project).All_Imported_Projects := List;
      end Add_To_List;

      ----------------------------
      -- Recursive_Add_Imported --
      ----------------------------

      procedure Recursive_Add_Imported (Project : Project_Id) is
         List    : Project_List;
         Element : Project_Element;
         Prj     : Project_Id;

      begin
         if Project /= No_Project then

            --  For all the imported projects

            List := Project_Tree.Projects.Table (Project).Imported_Projects;
            while List /= Empty_Project_List loop
               Element := Project_Tree.Project_Lists.Table (List);
               Prj := Ultimate_Extending_Project_Of (Element.Project);

               --  If project has not yet been visited, add to list and recurse

               if not Project_Tree.Projects.Table (Prj).Seen then
                  Project_Tree.Projects.Table (Prj).Seen := True;
                  Add_To_List (Prj);
                  Recursive_Add_Imported (Prj);
               end if;

               List := Element.Next;
            end loop;

            --  Recurse on projects being imported, if any

            Recursive_Add_Imported
              (Project_Tree.Projects.Table (Project).Extends);
         end if;
      end Recursive_Add_Imported;

   begin
      --  Reset the Seen flag for all projects

      for Index in 1 .. Project_Table.Last (Project_Tree.Projects) loop
         Project_Tree.Projects.Table (Index).Seen := False;
      end loop;

      --  Make sure the list is empty

      Project_Tree.Projects.Table (Project).All_Imported_Projects :=
        Empty_Project_List;

      --  Make sure Project is not importing itself

      Project_Tree.Projects.Table (Project).Seen := True;

      --  Add to the list all projects imported directly or indirectly

      Recursive_Add_Imported (Project);
   end Compute_All_Imported_Projects;

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
      Config_Project_Data : constant Project_Data :=
                              Project_Tree.Projects.Table (Project);
      Config_Package      : constant Package_Id :=
                          Value_Of
                            (Name        => Package_Name,
                             In_Packages => Config_Project_Data.Decl.Packages,
                             In_Tree     => Project_Tree);
      Config_Variable     : Variable_Value :=
                              Value_Of
                                (Name                    => Language,
                                 Attribute_Or_Array_Name => Attribute_Name,
                                 In_Package              => Config_Package,
                                 In_Tree                 => Project_Tree);

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
                 In_Variables  => Project_Tree.Packages.Table
                                    (Config_Package).Decl.Attributes,
                 In_Tree       => Project_Tree);

         elsif Attribute_Name = Name_Local_Config_File then
            Config_Variable :=
              Value_Of
                (Variable_Name => Name_Local_Configuration_Pragmas,
                 In_Variables  => Project_Tree.Packages.Table
                                    (Config_Package).Decl.Attributes,
                 In_Tree       => Project_Tree);
         end if;
      end if;

      if Config_Variable = Nil_Variable_Value then
         return No_Path;

      else
         Get_Name_String (Config_Variable.Value);

         if Name_Len = 0 then
            return No_Path;

         elsif Is_Absolute_Path (Name_Buffer (1 .. Name_Len)) then
            return Path_Name_Type (Config_Variable.Value);

         else
            Get_Name_String (Config_Project_Data.Directory);
            Name_Len := Name_Len + 1;
            Name_Buffer (Name_Len) := Directory_Separator;
            Add_Str_To_Name_Buffer (Get_Name_String (Config_Variable.Value));
            return Name_Find;
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
     (For_Project  : Project_Id;
      Config       : Language_Config;
      Language     : Name_Id)
   is

      File_Name : Path_Name_Type  := No_Path;
      File      : File_Descriptor := Invalid_FD;

      Source   : Source_Id;
      Src_Data : Source_Data;

      procedure Check (Project : Project_Id);
      --  Check the naming schemes of the different projects of the project
      --  tree. For each different naming scheme issue the pattern config
      --  declarations.

      procedure Check_Temp_File;
      --  Check if a temp file has been created. If not, create one.

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

      procedure Check (Project : Project_Id) is
         Data : constant Project_Data := Project_Tree.Projects.Table (Project);

         Lang_Id   : Language_Index := Data.First_Language_Processing;
         Lang_Data : Language_Data;

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
                        Substitute (Lang_Data.Config.Naming_Data.Body_Suffix);

                     when 's' =>
                        Substitute (Lang_Data.Config.Naming_Data.Spec_Suffix);

                     when 'd' =>
                        Substitute
                          (Lang_Data.Config.Naming_Data.Dot_Replacement);

                     when 'c' =>
                        Substitute
                          (Image (Lang_Data.Config.Naming_Data.Casing));

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
         --  Do not check the same project several times to avoid endless loops
         --  with cycles that include limited withs.

         if Data.Seen then
            return;
         end if;

         Project_Tree.Projects.Table (Project).Seen := True;

         if Current_Verbosity = High then
            Write_Str ("Checking project file """);
            Write_Str (Namet.Get_Name_String (Data.Name));
            Write_Str (""".");
            Write_Eol;
         end if;

         while Lang_Id /= No_Language_Index loop
            Lang_Data := Project_Tree.Languages_Data.Table (Lang_Id);
            exit when Lang_Data.Name = Language;
            Lang_Id := Lang_Data.Next;
         end loop;

         if Lang_Id /= No_Language_Index then
            Current_Naming := Naming_Datas.First;

            while Current_Naming <= Naming_Datas.Last loop
               exit when Naming_Datas.Table (Current_Naming) =
                 Lang_Data.Config.Naming_Data;
               Current_Naming := Current_Naming + 1;
            end loop;

            if Current_Naming > Naming_Datas.Last then
               Naming_Datas.Increment_Last;
               Naming_Datas.Table (Naming_Datas.Last) :=
                 Lang_Data.Config.Naming_Data;

               Check_Temp_File;

               if Lang_Data.Config.Config_Spec_Pattern /= No_Name then
                  Get_Name_String (Lang_Data.Config.Config_Spec_Pattern);
                  Replace;
                  Put_Line (File, Name_Buffer (1 .. Name_Len));
               end if;

               if Lang_Data.Config.Config_Body_Pattern /= No_Name then
                  Get_Name_String (Lang_Data.Config.Config_Body_Pattern);
                  Replace;
                  Put_Line (File, Name_Buffer (1 .. Name_Len));
               end if;
            end if;
         end if;

         if Data.Extends /= No_Project then
            Check (Data.Extends);
         end if;

         declare
            Current : Project_List := Data.Imported_Projects;

         begin
            while Current /= Empty_Project_List loop
               Check (Project_Tree.Project_Lists.Table (Current).Project);
               Current := Project_Tree.Project_Lists.Table (Current).Next;
            end loop;
         end;
      end Check;

      ---------------------
      -- Check_Temp_File --
      ---------------------

      procedure Check_Temp_File is
      begin
         if File = Invalid_FD then
            Tempdir.Create_Temp_File (File, Name => File_Name);
            Record_Temp_File (Path => File_Name);

            if File = Invalid_FD then
               Fail_Program
                 ("unable to create temporary configuration pragmas file");

            else
               Record_Temp_File (File_Name);

               if Opt.Verbose_Mode then
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
                                (Project,
                                 Package_Name,
                                 Attribute_Name,
                                 Language);
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
                    ("unable to open config file ",
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
            Fail_Program ("Disk full");
         end if;

         if Current_Verbosity = High then
            Write_Line (S);
         end if;
      end Put_Line;

   --  Start of processing for Create_Config_File

   begin
      --  Nothing to do if config has already been checked

      if Project_Tree.Projects.Table (For_Project).Config_Checked then
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

      Project_Tree.Projects.Table (For_Project).Config_Checked := True;

      Naming_Datas.Init;

      for Proj in 1 .. Project_Table.Last (Project_Tree.Projects) loop
         Project_Tree.Projects.Table (Proj).Seen := False;
      end loop;

      Check (For_Project);

      --  Visit all the units and issue the config declarations for those that
      --  need one.

      Source := Project_Tree.First_Source;

      while Source /= No_Source loop
         Src_Data := Project_Tree.Sources.Table (Source);

         if Src_Data.Language_Name = Language  and then
           Src_Data.Naming_Exception and then
           Src_Data.Unit /= No_Name and then
           not Src_Data.Locally_Removed and then
           Src_Data.Replaced_By = No_Source
         then
            Name_Len := 0;

            if Src_Data.Kind = Spec and then Config.Config_Spec /= No_Name then
               Get_Name_String (Config.Config_Spec);

            elsif (Src_Data.Kind = Impl or else Src_Data.Kind = Sep) and then
                  Config.Config_Body /= No_Name
            then
               Get_Name_String (Config.Config_Body);
            end if;

            if Name_Len /= 0 then
               declare
                  Cur : Positive := 1;
                  Unit : constant String := Get_Name_String (Src_Data.Unit);
                  File_Name : constant String :=
                                Get_Name_String (Src_Data.Display_File);

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

         Source := Src_Data.Next_In_Sources;
      end loop;

      if File /= Invalid_FD then
         Close (File);
         Project_Tree.Projects.Table (For_Project).Config_File_Name :=
           File_Name;
      end if;

   end Create_Config_File;

   ----------------------
   -- Directly_Imports --
   ----------------------

   function Directly_Imports
     (Project  : Project_Id;
      Imported : Project_Id) return Boolean
   is
      L : Project_List :=
            Project_Tree.Projects.Table (Project).Imported_Projects;
      E : Project_Element;
      P : Project_Id;

   begin
      while L /= Empty_Project_List loop
         E := Project_Tree.Project_Lists.Table (L);

         P := E.Project;
         while P /= No_Project loop
            if Imported = P then
               return True;
            end if;

            P := Project_Tree.Projects.Table (P).Extends;
         end loop;

         L := E.Next;
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

      if not Quiet_Output then

         --  In Verbose Mode output the full path of the spawned process

         if Verbose_Mode then
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

      Data : constant Project_Data :=
               Project_Tree.Projects.Table (For_Project);

      procedure Recursive_Add_Linker_Options (Proj : Project_Id);
      --  The recursive routine used to add linker options

      ----------------------------------
      -- Recursive_Add_Linker_Options --
      ----------------------------------

      procedure Recursive_Add_Linker_Options (Proj : Project_Id) is
         Data           : Project_Data;
         Linker_Package : Package_Id;
         Options        : Variable_Value;
         Imported       : Project_List;

      begin
         if Proj /= No_Project then
            Data := Project_Tree.Projects.Table (Proj);

            if not Data.Seen then
               Project_Tree.Projects.Table (Proj).Seen := True;
               Imported := Data.Imported_Projects;

               while Imported /= Empty_Project_List loop
                  Recursive_Add_Linker_Options
                    (Project_Tree.Project_Lists.Table
                       (Imported).Project);
                  Imported := Project_Tree.Project_Lists.Table
                                (Imported).Next;
               end loop;

               if Proj /= For_Project then
                  Linker_Package :=
                    Prj.Util.Value_Of
                      (Name        => Name_Linker,
                       In_Packages => Data.Decl.Packages,
                       In_Tree     => Project_Tree);
                  Options :=
                    Prj.Util.Value_Of
                      (Name                    => Name_Ada,
                       Index                   => 0,
                       Attribute_Or_Array_Name => Name_Linker_Options,
                       In_Package              => Linker_Package,
                       In_Tree                 => Project_Tree);

                  --  If attribute is present, add the project with
                  --  the attribute to table Linker_Opts.

                  if Options /= Nil_Variable_Value then
                     Linker_Opts.Increment_Last;
                     Linker_Opts.Table (Linker_Opts.Last) :=
                       (Project => Proj, Options => Options.Values);
                  end if;
               end if;
            end if;
         end if;
      end Recursive_Add_Linker_Options;

   --  Start of processing for Linker_Options_Switches

   begin
      if Data.Config.Linker_Lib_Dir_Option = No_Name then
         Linker_Lib_Dir_Option := new String'("-L");

      else
         Linker_Lib_Dir_Option :=
           new String'
             (Get_Name_String (Data.Config.Linker_Lib_Dir_Option));
      end if;

      if Data.Config.Linker_Lib_Name_Option = No_Name then
         Linker_Lib_Name_Option := new String'("-l");

      else
         Linker_Lib_Name_Option :=
           new String'
             (Get_Name_String (Data.Config.Linker_Lib_Name_Option));
      end if;

      Linker_Opts.Init;

      for Index in Project_Table.First ..
                   Project_Table.Last (Project_Tree.Projects)
      loop
         Project_Tree.Projects.Table (Index).Seen := False;
      end loop;

      Recursive_Add_Linker_Options (For_Project);

      for Index in reverse 1 .. Linker_Opts.Last loop
         declare
            Options : String_List_Id := Linker_Opts.Table (Index).Options;
            Proj    : constant Project_Id :=
              Linker_Opts.Table (Index).Project;
            Option  : Name_Id;

         begin
            --  If Dir_Path has not been computed for this project, do it now

            if Project_Tree.Projects.Table (Proj).Dir_Path = null then
               Project_Tree.Projects.Table (Proj).Dir_Path :=
                 new String'
                   (Get_Name_String
                        (Project_Tree.Projects.Table
                             (Proj). Directory));
            end if;

            while Options /= Nil_String loop
               Option :=
                 Project_Tree.String_Elements.Table (Options).Value;
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
                        Add_Argument
                          (Name_Buffer (1 .. Name_Len),
                           True,
                           Simple_Name => not Verbose_Mode);

                     else
                        Add_Argument
                          (Linker_Lib_Dir_Option.all &
                           Project_Tree.Projects.Table (Proj).Dir_Path.all &
                           Directory_Separator &
                           Name_Buffer
                             (Linker_Lib_Dir_Option'Length + 1 .. Name_Len),
                           True,
                           Simple_Name => not Verbose_Mode);
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
                       (Project_Tree.Projects.Table (Proj).Dir_Path.all &
                        Directory_Separator &
                        Name_Buffer (1 .. Name_Len),
                        True,
                        Simple_Name => True);
                  end if;
               end if;

               Options :=
                 Project_Tree.String_Elements.Table (Options).Next;
            end loop;
         end;
      end loop;
   end Get_Linker_Options;

   ---------------
   -- Get_Mains --
   ---------------

   procedure Get_Mains is
   begin
      Gpr_Util.Get_Mains;

      if Mains.Number_Of_Mains = 0 then
         --  Do not link, as there is no main.

         if All_Phases then
            All_Phases   := False;
            Compile_Only := True;
            Bind_Only    := True;
         end if;

         Link_Only := False;
      end if;
   end Get_Mains;

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

      Add_Option
        (Option_Name,
         To => All_Options,
         Display => False);

      return All_Options.Options (All_Options.Last);
   end Get_Option;

   ---------------------
   -- Get_Directories --
   ---------------------

   procedure Get_Directories
     (For_Project : Project_Id;
      Sources     : Boolean;
      Languages   : Name_Ids)
   is

      procedure Recursive_Add (Project : Project_Id);
      --  Add all the source directories of a project to the path only if
      --  this project has not been visited. Calls itself recursively for
      --  projects being extended, and imported projects.

      -------------------
      -- Recursive_Add --
      -------------------

      procedure Recursive_Add (Project : Project_Id) is
         Current    : String_List_Id;
         Dir        : String_Element;

         procedure Add_Dir (Value : Path_Name_Type);
         --  Add directory Value in table Directories, if it is defined and not
         --  already there.

         -------------
         -- Add_Dir --
         -------------

         procedure Add_Dir (Value : Path_Name_Type) is
            Add_It : Boolean := True;

         begin
            if Value /= No_Path then
               for Index in 1 .. Directories.Last loop
                  if Directories.Table (Index) = Value then
                     Add_It := False;
                     exit;
                  end if;
               end loop;

               if Add_It then
                  Directories.Increment_Last;
                  Directories.Table (Directories.Last) := Value;
               end if;
            end if;
         end Add_Dir;

      begin
         --  If Seen is empty, then the project cannot have been visited

         if not Project_Tree.Projects.Table (Project).Seen then
            Project_Tree.Projects.Table (Project).Seen := True;

            declare
               Data : constant Project_Data :=
                        Project_Tree.Projects.Table (Project);
               List : Project_List := Data.Imported_Projects;

               Lang_Proc : Language_Index := Data.First_Language_Processing;
               Lang_Data : Language_Data;
               OK : Boolean;

            begin
               --  Add to path all directories of this project

               if Sources then
                  OK := False;

                  Lang_Loop :
                  while Lang_Proc /= No_Language_Index loop
                     Lang_Data :=
                       Project_Tree.Languages_Data.Table (Lang_Proc);

                     for J in Languages'Range loop
                        OK := Lang_Data.Name = Languages (J);
                        exit Lang_Loop when OK;
                     end loop;

                     Lang_Proc := Lang_Data.Next;
                  end loop Lang_Loop;

                  if OK then
                     Current := Data.Source_Dirs;

                     while Current /= Nil_String loop
                        Dir := Project_Tree.String_Elements.Table (Current);
                        Add_Dir (Path_Name_Type (Dir.Value));
                        Current := Dir.Next;
                     end loop;
                  end if;

               elsif Data.Library then
                  Add_Dir (Data.Library_ALI_Dir);

               else
                  Add_Dir (Data.Object_Directory);
               end if;

               --  Call Add to the project being extended, if any

               if Data.Extends /= No_Project then
                  Recursive_Add (Data.Extends);
               end if;

               --  Call Add for each imported project, if any

               while List /= Empty_Project_List loop
                  Recursive_Add
                    (Project_Tree.Project_Lists.Table (List).Project);
                  List := Project_Tree.Project_Lists.Table (List).Next;
               end loop;
            end;
         end if;
      end Recursive_Add;

   --  Start of processing for Get_Directories

   begin
      Directories.Init;

      for Index in Project_Table.First ..
        Project_Table.Last (Project_Tree.Projects)
      loop
         Project_Tree.Projects.Table (Index).Seen := False;
      end loop;

      Recursive_Add (For_Project);
   end Get_Directories;

   -------------------------
   -- Global_Archive_Name --
   -------------------------

   function Global_Archive_Name (For_Project : Project_Id) return String is
   begin
      return
        "lib" &
        Get_Name_String (Project_Tree.Projects.Table (For_Project).Name) &
        ".a";
   end Global_Archive_Name;

   --------------
   -- Gprbuild --
   --------------

   procedure Gprbuild is
   begin
      --  First initialize and read the command line arguments

      Buildgpr.Initialize;

      --  And install Ctrl-C handler

      Install_Int_Handler (Sigint_Intercepted'Access);

      --  Then, get the configuration

      Get_Configuration (Packages_To_Check);

      if Total_Errors_Detected > 0 then
         Prj.Err.Finalize;
         Fail_Program
           ("problems while getting the configuration",
            Flush_Messages => False);
      end if;

      --  Finish processing the main project

      Prj.Proc.Process_Project_Tree_Phase_2
        (In_Tree                => Project_Tree,
         Project                => Main_Project,
         Success                => Gpr_Util.Success,
         From_Project_Node      => User_Project_Node,
         From_Project_Node_Tree => Project_Node_Tree,
         Report_Error           => null,
         Current_Dir            => Get_Current_Dir,
         When_No_Sources        => Warning);

      if not Gpr_Util.Success then
         Prj.Err.Finalize;
         Fail_Program
           ("""",
            Project_File_Name.all,
            """ processing failed",
            Flush_Messages => False);
      end if;

      Main_Project_Dir :=
        new String'
          (Get_Name_String
             (Project_Tree.Projects.Table (Main_Project).Directory));

      if Warnings_Detected > 0 then
         Prj.Err.Finalize;
         Prj.Err.Initialize;
      end if;

      for Proj in 1 .. Project_Table.Last (Project_Tree.Projects) loop
         Compute_All_Imported_Projects (Proj);
      end loop;

      Queue.Init;

      --  If -u or -U is specified on the command line, disregard any -c, -b
      --  or -l switch: only perform compilation.

      if Unique_Compile then
         All_Phases := False;
         Compile_Only := True;
         Bind_Only := False;
         Link_Only := False;
         Closure_Needed := False;

         --  If there are mains specified on the command line, only compile
         --  these sources.

         if Mains.Number_Of_Mains > 0 then
            Check_Mains;

         --  Otherwise compile all the sources of the main project (-u) or
         --  all the sources of all projects (-U).

         else
            Queue.Insert_Project_Sources
              (Main_Project,
               All_Projects => Unique_Compile_All_Projects or Recursive,
               Unit_Based   => True);
         end if;

      elsif Mains.Number_Of_Mains = 0 and then
        (not All_Phases) and then
        Compile_Only and then
        (not Bind_Only)
      then
         Queue.Insert_Project_Sources
           (Main_Project, All_Projects => Recursive, Unit_Based => True);
         Closure_Needed := False;

      elsif Mains.Number_Of_Mains /= 0 and then
        (not All_Phases) and then
        Compile_Only
      then
         Closure_Needed := False;
         Check_Mains;

      else
         Get_Mains;

         if Mains.Number_Of_Mains = 0 then
            --  Compile everything if there is no main

            Closure_Needed := False;

         else
            Closure_Needed := True;
            Check_Mains;
         end if;

         --  If we need the closures of Ada mains and roots, then if we need
         --  to perform the binding phase, then we need first to execute the
         --  compilation phase, otherwise we will not know what the content of
         --  the global archive.

         if Closure_Needed and then (not All_Phases) and then Bind_Only then
            Compile_Only := True;
         end if;

         if All_Phases or Compile_Only then
            --  If the closure is needed, we put in the queue all file based
            --  sources and all sources in library projects.

            if Closure_Needed then
               Queue.Insert_Project_Sources
                 (Main_Project, All_Projects => True, Unit_Based => False);

            else
               Queue.Insert_Project_Sources
                 (Main_Project, All_Projects => True, Unit_Based => True);
            end if;

            --  If no sources to compile, then there is nothing to do

            if Queue.Size = 0 then
               if (not Quiet_Output) and then
                  (not Project_Tree.Projects.Table
                        (Main_Project).Externally_Built)
               then
                  Osint.Write_Program_Name;
                  Write_Line (": no sources to compile");
               end if;

               Finish_Program (Fatal => False);
            end if;
         end if;

      end if;

      --  Get the builder switches in the main project, if any

      if Mains.Number_Of_Mains > 0 then
         declare
            Builder_Package  : constant Package_Id :=
                                 Value_Of (Name_Builder,
                                           Project_Tree.Projects.Table
                                             (Main_Project).Decl.Packages,
                                           Project_Tree);

            Switches : Variable_Value;

            List             : String_List_Id;
            Element          : String_Element;

            Name             : Name_Id := No_Name;
            Lang             : Name_Id := No_Name;
            Source           : Source_Data;
         begin
            Mains.Reset;

            for Index in 1 .. Mains.Number_Of_Mains loop
               Source := Source_Of (Mains.Next_Main);
               if Source /= No_Source_Data then
                  if Name = No_Name and then Lang = No_Name then
                     --  First main

                     Name := Name_Id (Source.File);
                     Lang := Source.Language_Name;

                  elsif Lang = Source.Language_Name then
                     --  Other mains. If they are of the same language, use
                     --  the language as the name.

                     Name := Lang;

                  else
                     --  There are mains with different languages: no switches
                     --  from package Builder.

                     Name := No_Name;
                     exit;
                  end if;
               end if;
            end loop;

            if Name /= No_Name and then Builder_Package /= No_Package then
               --  Get the switches for the single main or the language of the
               --  mains.

               Switches := Value_Of
                 (Name                    => Name,
                  Attribute_Or_Array_Name => Name_Switches,
                  In_Package              => Builder_Package,
                  In_Tree                 => Project_Tree);

               if Name /= Lang then
                  --  If specific switches for the main have been found,
                  --  the switches that are not recognized by gprbuild will be
                  --  global switches for the language of the main.

                  if Switches /= Nil_Variable_Value and then
                    not Switches.Default
                  then
                     Builder_Switches_Lang := Lang;

                  else
                     --  If no specific switches for the main are declared,
                     --  check for Switches (<language>).

                     Switches := Value_Of
                       (Name                    => Lang,
                        Attribute_Or_Array_Name => Name_Switches,
                        In_Package              => Builder_Package,
                        In_Tree                 => Project_Tree,
                        Force_Lower_Case_Index  => True);
                  end if;
               end if;

               --  For backward compatibility with gnatmake, if no Switches
               --  are declared, check for Default_Switches (<language>).

               if Switches = Nil_Variable_Value or else Switches.Default then
                  Switches := Value_Of
                    (Name                    => Lang,
                     Attribute_Or_Array_Name => Name_Default_Switches,
                     In_Package              => Builder_Package,
                     In_Tree                 => Project_Tree);

                  --  Set the Builder Switches language, so that switches that
                  --  are not recognized by gprbuild are passed to the compiler
                  --  of the language.

                  if Switches /= Nil_Variable_Value and then
                    (not Switches.Default)
                  then
                     Builder_Switches_Lang := Lang;
                  end if;
               end if;

               --  If switches have been found, scan them

               if Switches /= Nil_Variable_Value and then
                 (not Switches.Default)
               then
                  List := Switches.Values;

                  while List /= Nil_String loop
                     Element := Project_Tree.String_Elements.Table (List);
                     Get_Name_String (Element.Value);

                     if Name_Len /= 0 then
                        Scan_Arg
                          (Name_Buffer (1 .. Name_Len),
                           Command_Line => False);
                     end if;

                     List := Element.Next;
                  end loop;

                  --  Reset the Builder Switches language

                  Builder_Switches_Lang := No_Name;
               end if;
            end if;
         end;
      end if;

      --  Reprocess recorded command line options that have priority over
      --  those in the main project file.

      Options.Process_Command_Line_Options;

      --  Compilation phase

      if All_Phases or Compile_Only then
         Compilation_Phase;

         if Total_Errors_Detected > 0
           or else Bad_Compilations.Last > 0
         then
            --  If switch -k is used, output a summary of the sources that
            --  could not be compiled.

            if Keep_Going and then Bad_Compilations.Last > 0 then
               declare
                  Source   : Source_Id;
                  Src_Data : Source_Data;

               begin
                  Write_Eol;

                  for Index in 1 .. Bad_Compilations.Last loop
                     Source := Bad_Compilations.Table (Index);

                     if Source /= No_Source then
                        Src_Data := Project_Tree.Sources.Table (Source);

                        Write_Str ("   compilation of ");
                        Write_Str (Get_Name_String (Src_Data.Display_File));
                        Write_Line (" failed");
                     end if;
                  end loop;

                  Write_Eol;
               end;
            end if;

            Fail_Program ("*** compilation phase failed");
         end if;
      end if;

      --  Binding phase

      if All_Phases or Bind_Only then
         Binding_Phase;

         if Total_Errors_Detected > 0 then
            Fail_Program ("*** bind failed");
         end if;
      end if;

      if All_Phases or Link_Only then
         Linking_Phase;

         if Total_Errors_Detected > 0 then
            Fail_Program ("*** link failed");
         end if;
      end if;

      if Warnings_Detected /= 0 then
         Errout.Finalize (Last_Call => True);
         Errout.Output_Messages;
      end if;

      Finish_Program (Fatal => False);
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

      Prj.Set_Mode (Multi_Language);
      Prj.Initialize (Project_Tree);
      Mains.Delete;

      --  Get the name id for '*'

      Name_Len := 1;
      Name_Buffer (1) := '*';
      Name_Star := Name_Find;

      --  Get the name id for "-L";

      Name_Len := 0;
      Add_Str_To_Name_Buffer ("-L");
      Dash_L := Name_Find;

      --  Add the directory where gprbuild is invoked in front of the path,
      --  if gprbuild is invoked from a bin directory or with directory
      --  information. Only do this if the platform is not VMS, where the
      --  notion of path does not really exist.

      if not OpenVMS then
         declare
            Prefix  : constant String := Executable_Prefix_Path;
            Command : constant String := Command_Name;

         begin
            if Prefix'Length > 0 then
               declare
                  PATH : constant String :=
                           Prefix & Directory_Separator & "bin" &
                           Path_Separator &
                           Getenv ("PATH").all;

               begin
                  Setenv ("PATH", PATH);
               end;

            else
               for Index in reverse Command'Range loop
                  if Command (Index) = Directory_Separator then
                     declare
                        Absolute_Dir : constant String :=
                                         Normalize_Pathname
                                           (Command (Command'First .. Index));

                        PATH         : constant String :=
                                         Absolute_Dir &
                                         Path_Separator &
                                         Getenv ("PATH").all;

                     begin
                        Setenv ("PATH", PATH);
                     end;

                     exit;
                  end if;
               end loop;
            end if;
         end;
      end if;

      --  Get the command line arguments

      All_Phases := True;

      --  First check for --version or --help

      Check_Version_And_Help
        ("GPRBUILD",
         "2004",
         Version_String => Gpr_Version_String);

      --  Now process the other options

      Autoconfiguration := True;

      Scan_Args : for Next_Arg in 1 .. Argument_Count loop
         Scan_Arg (Argument (Next_Arg), Command_Line => True);
      end loop Scan_Args;

      Current_Processor := None;

      --  If --display-paths was specified, display the config and the user
      --  project paths and exit.

      if Display_Paths then
         Write_Char ('.');

         declare
            Prefix_Path : constant String := Executable_Prefix_Path;

         begin
            if Prefix_Path'Length /= 0 then
               Write_Char (Path_Separator);
               Write_Str (Prefix_Path);
               Write_Char (Directory_Separator);
               Write_Str ("share");
               Write_Char (Directory_Separator);
               Write_Str ("gpr");
            end if;
         end;

         Write_Eol;

         Write_Line (Project_Path);

         Exit_Program (E_Success);
      end if;

      if Verbose_Mode then
         Copyright;
      end if;

      if Usage_Needed then
         Usage;
         Usage_Needed := False;
      end if;

      --  Fail if command line ended with "-P"

      if Project_File_Name_Expected then
         Fail_Program ("project file name missing after -P");

      --  Or if it ended with "-o"

      elsif Output_File_Name_Expected then
         Fail_Program ("output file name missing after -o");

      --  Or if it ended with "-aP"

      elsif Search_Project_Dir_Expected then
         Fail_Program ("directory name missing after -aP");
      end if;

      --  If no project file was specified, look first for a default

      if Project_File_Name = null then
         Look_For_Default_Project;
      end if;

      if Project_File_Name = null then
         Copyright;
         Usage;
         Fail_Program
           ("no project file specified and no default project file");
      end if;
   end Initialize;

   ------------------------------
   -- Initialize_Source_Record --
   ------------------------------

   procedure Initialize_Source_Record
     (Source      : Source_Id;
      Need_Object : Boolean := False)
   is
      Src_Data : Source_Data  := Project_Tree.Sources.Table (Source);
      Data     : Project_Data;
      Obj_Proj : Project_Id := Src_Data.Project;
      Found    : Boolean := False;
   begin
      if Src_Data.Source_TS = Empty_Time_Stamp or else Need_Object then
         Data := Project_Tree.Projects.Table (Src_Data.Project);
         Src_Data.Source_TS := File_Stamp (Src_Data.Path);
         Project_Tree.Sources.Table (Source) := Src_Data;

         if Need_Object then
            Src_Data.Get_Object := True;
            Project_Tree.Sources.Table (Source).Get_Object := True;
         end if;

         if Src_Data.Compiled then
            if Src_Data.Lang_Kind = Unit_Based
              and then
                Src_Data.Kind = Impl
                and then
                  Is_Subunit (Src_Data)
            then
               Src_Data.Kind := Sep;
               Src_Data.Get_Object := False;
               Project_Tree.Sources.Table (Source) := Src_Data;
               return;

            elsif (Src_Data.Lang_Kind = File_Based
                   and then Src_Data.Kind = Spec)
              or else
                (Src_Data.Lang_Kind = Unit_Based
                 and then
                   (Src_Data.Kind = Sep
                    or else
                      (Src_Data.Kind = Spec
                       and then
                         Src_Data.Other_Part /= No_Source)))
            then
               Src_Data.Get_Object := False;
               Project_Tree.Sources.Table (Source) := Src_Data;
               return;
            end if;

            loop
               declare
                  Object_Path     :
                    constant String :=
                      Normalize_Pathname
                        (Name      => Get_Name_String (Src_Data.Object),
                         Directory => Get_Name_String (Data.Object_Directory));
               begin
                  Src_Data.Object_Path := Create_Name (Object_Path);
               end;

               Src_Data.Object_TS := File_Stamp (Src_Data.Object_Path);

               if Src_Data.Dependency /= None then
                  declare
                     Dep_Path        :
                       constant String :=
                         Normalize_Pathname
                           (Name      => Get_Name_String (Src_Data.Dep_Name),
                            Directory =>
                                      Get_Name_String (Data.Object_Directory));

                  begin
                     Src_Data.Dep_Path := Create_Name (Dep_Path);
                  end;

                  Src_Data.Dep_TS := File_Stamp (Src_Data.Dep_Path);
               end if;

               declare
                  Switches_Path :
                    constant String :=
                      Normalize_Pathname
                        (Name      => Get_Name_String (Src_Data.Switches),
                         Directory => Get_Name_String (Data.Object_Directory));

               begin
                  Src_Data.Switches_Path := Create_Name (Switches_Path);
               end;

               Src_Data.Switches_TS := File_Stamp (Src_Data.Switches_Path);

               if Src_Data.Object_TS /= Empty_Time_Stamp then
                  Found := True;
                  Src_Data.Object_Project := Obj_Proj;
                  Project_Tree.Sources.Table (Source) := Src_Data;
               end if;

               Obj_Proj := Data.Extended_By;
               if Obj_Proj = No_Project then
                  if not Found then
                     Project_Tree.Sources.Table (Source) := Src_Data;
                  end if;

                  exit;
               end if;

               Data := Project_Tree.Projects.Table (Obj_Proj);
            end loop;
         end if;
      end if;
   end Initialize_Source_Record;

   -----------------------------------
   -- Is_Included_In_Global_Archive --
   -----------------------------------

   function Is_Included_In_Global_Archive
     (Object_Name : File_Name_Type;
      Project     : Project_Id)
      return Boolean
   is
      Data     : Project_Data := Project_Tree.Projects.Table (Project);
      Source   : Source_Id;
      Src_Data : Source_Data;

   begin
      --  If a source is overriden in an extending project, then the object
      --  file is not included in the global archive.

      while Data.Extended_By /= No_Project loop
         Data := Project_Tree.Projects.Table (Data.Extended_By);

         Source := Data.First_Source;
         while Source /= No_Source loop
            Src_Data := Project_Tree.Sources.Table (Source);

            if ((Src_Data.Lang_Kind = File_Based and then Src_Data.Kind = Impl)
                or else
                (Src_Data.Lang_Kind = Unit_Based
                 and then
                 (Src_Data.Kind = Impl
                  or else
                  (Src_Data.Kind = Spec
                   and then
                   Src_Data.Other_Part = No_Source))))
                and then
                Src_Data.Object =  Object_Name
            then
               return False;
            else
               Source := Project_Tree.Sources.Table (Source).Next_In_Project;
            end if;
         end loop;
      end loop;

      Data := Project_Tree.Projects.Table (Project);
      Source := Data.First_Source;
      while Source /= No_Source loop
         Src_Data := Project_Tree.Sources.Table (Source);

         if ((Src_Data.Lang_Kind = File_Based and then Src_Data.Kind = Impl)
             or else
             (Src_Data.Lang_Kind = Unit_Based
              and then
              (Src_Data.Kind = Impl
              or else
              (Src_Data.Kind = Spec
               and then
               Src_Data.Other_Part = No_Source))))
            and then
            Src_Data.Object =  Object_Name
         then
            return
              Src_Data.Object_Linked
              and then (Src_Data.Lang_Kind /= Unit_Based
                        or else (not Closure_Needed)
                        or else Src_Data.Get_Object);

         else
            Source :=
              Project_Tree.Sources.Table (Source).Next_In_Project;
         end if;
      end loop;

      return True;
   end Is_Included_In_Global_Archive;

   ----------------
   -- Is_Subunit --
   ----------------

   function Is_Subunit (Source : Source_Data) return Boolean is
      Src_Ind : Source_File_Index;
   begin
      if Source.Kind = Sep then
         return True;

      --  A Spec, a file based language source or a body with a spec cannot be
      --  a subunit.

      elsif Source.Kind = Spec or else
        Source.Unit = No_Name or else
        Source.Other_Part /= No_Source
      then
         return False;
      end if;

      --  Here, we are assuming that the language is Ada, as it is the only
      --  unit based language that we know.

      Src_Ind := Sinput.P.Load_Project_File (Get_Name_String (Source.Path));

      return Sinput.P.Source_File_Is_Subunit (Src_Ind);
   end Is_Subunit;

   -------------------
   -- Linking_Phase --
   -------------------

   procedure Linking_Phase is
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

      Binder_Object_TS   : Time_Stamp_Type;

      Global_Archive_TS  : Time_Stamp_Type;

      Global_Archive_Has_Been_Built : Boolean := False;

   begin
      Mains.Reset;

      loop
         declare
            Display_Main   : constant String := Mains.Next_Main;
            Main           : String := Display_Main;
            Main_Id        : File_Name_Type;
            Main_Source_Id : Source_Id;
            Main_Source    : Source_Data;

            Exec_Name      : File_Name_Type;
            Exec_Path_Name : Path_Name_Type;

            Data           : Project_Data;

            Main_Proj      : Project_Id;

            Main_Base_Name : File_Name_Type;

            Has_Been_Built : Boolean;

         begin
            exit when Display_Main'Length = 0;

            Linker_Needs_To_Be_Called := Force_Compilations;

            Canonical_Case_File_Name (Main);

            Main_Id := Create_Name (Main);
            Main_Source_Id := Main_Sources.Get (Main_Id);
            Initialize_Source_Record (Main_Source_Id);
            Main_Source := Project_Tree.Sources.Table (Main_Source_Id);
            Main_Proj  := Ultimate_Extending_Project_Of (Main_Source.Project);
            Data        := Project_Tree.Projects.Table (Main_Proj);

            Change_To_Object_Directory (Main_Proj);

            --  Build the global archive for this project, if needed

            Build_Global_Archive (Main_Proj, Has_Been_Built);

            Global_Archive_Has_Been_Built :=
              Global_Archive_Has_Been_Built or Has_Been_Built;

            --  Get the main base name

            Name_Len := 0;
            Add_Str_To_Name_Buffer (Main);

            for J in reverse 2 .. Name_Len loop
               if Name_Buffer (J) = '.' then
                  Name_Len := J - 1;
                  exit;
               end if;
            end loop;

            Main_Base_Name := Name_Find;

            if (not Linker_Needs_To_Be_Called) and then Verbose_Mode then
               Write_Str ("   Checking executable for ");
               Write_Str (Get_Name_String (Main_Source.File));
               Write_Line (" ...");
            end if;

            Exec_Name := Executable_Of
              (Project  => Main_Proj,
               In_Tree  => Project_Tree,
               Main     => Main_Id,
               Index    => 0,
               Ada_Main => False,
               Language => Get_Name_String (Main_Source.Language_Name));

            if Data.Exec_Directory = Data.Object_Directory then
               Exec_Path_Name := Path_Name_Type (Exec_Name);

            else
               Get_Name_String (Data.Exec_Directory);
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

               if Verbose_Mode then
                  Write_Line ("      -> executable does not exist");
               end if;
            end if;

            --  Get the path of the linker driver

            if Data.Linker_Path /= No_Path then
               Linker_Path := new String'(Get_Name_String (Data.Linker_Path));

            elsif Data.Linker_Name /= No_File then
               Linker_Name :=
                 new String'(Get_Name_String (Data.Linker_Name));

               Linker_Path := Locate_Exec_On_Path (Linker_Name.all);

               if Linker_Path = null then
                  Fail_Program ("unable to find linker ", Linker_Name.all);

               else
                  Data.Linker_Path :=
                    Path_Name_Type'(Create_Name (Linker_Path.all));
                  Project_Tree.Projects.Table (Main_Proj) := Data;
               end if;

            elsif Data.Config.Linker /= No_Path then
               Linker_Name :=
                 new String'(Get_Name_String
                             (Data.Config.Linker));
               Linker_Path := Locate_Exec_On_Path (Linker_Name.all);

               if Linker_Path = null then
                  Fail_Program ("unable to find linker ", Linker_Name.all);
               end if;

            else
               Fail_Program
                 ("no linker specified and " &
                  "no default linker in the configuration");
            end if;

            Last_Argument := 0;

            --  First, the minimum options, if any

            if Data.Linker_Name /= No_File then
               Min_Linker_Opts := Data.Minimum_Linker_Options;

            else
               Min_Linker_Opts := Data.Config.Minimum_Linker_Options;
            end if;

            while Min_Linker_Opts /= No_Name_List loop
               Add_Argument
                 (Get_Name_String
                    (Project_Tree.Name_Lists.Table (Min_Linker_Opts).Name),
                  Verbose_Mode);
               Min_Linker_Opts   :=
                 Project_Tree.Name_Lists.Table (Min_Linker_Opts).Next;
            end loop;

            Main_Object_TS :=
              File_Stamp (File_Name_Type (Main_Source.Object_Path));

            if not Linker_Needs_To_Be_Called then
               if Main_Object_TS = Empty_Time_Stamp then
                  if Verbose_Mode then
                     Write_Line ("      -> main object does not exist");
                  end if;

                  Linker_Needs_To_Be_Called := True;

               elsif String (Main_Object_TS) > String (Executable_TS) then
                  if Verbose_Mode then
                     Write_Line
                       ("      -> main object more recent than executable");
                  end if;

                  Linker_Needs_To_Be_Called := True;
               end if;
            end if;

            if Main_Object_TS = Empty_Time_Stamp then
               Fail_Program
                 ("main object for ",
                  Get_Name_String (Main_Source.File),
                  " does not exist");
            end if;

            if Main_Proj = Main_Source.Object_Project then
               Add_Argument (Get_Name_String (Main_Source.Object), True);

            else
               Add_Argument (Get_Name_String (Main_Source.Object_Path), True);
            end if;

            if There_Are_Binder_Drivers
              and then Binding_Languages.Last = 0
            then
               Find_Binding_Languages;
            end if;

            if There_Are_Binder_Drivers then
               Binding_Options.Init;

               for B_Index in 1 .. Binding_Languages.Last loop
                  declare
                     B_Data : constant Binding_Data :=
                                Binding_Languages.Table (B_Index);
                     Exchange_File_Name : constant String :=
                                            Binder_Exchange_File_Name
                                              (Main_Base_Name,
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

                           if Verbose_Mode then
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
                                        (Path_Name_Type
                                             (Path_Name_Type'
                                                  (Create_Name
                                                       (Line (1 .. Last)))));

                                    Add_Argument
                                      (Line (1 .. Last), Verbose_Mode);

                                 when Resulting_Options =>
                                    if Line (1 .. Last) /= "-static" and then
                                      Line (1 .. Last) /= "-shared"
                                    then
                                       Binding_Options.Append
                                         (new String'(Line (1 .. Last)));
                                    end if;

                                 when Gprexch.Run_Path_Option =>
                                    if Data.Config.Run_Path_Option /=
                                      No_Name_List
                                    then
                                       Add_Rpath (Line (1 .. Last));
                                    end if;

                                 when others =>
                                    null;
                                 end case;
                              end if;
                           end if;
                        end loop;

                        Close (Exchange_File);

                        if Binder_Object_TS = Empty_Time_Stamp then
                           if (not Linker_Needs_To_Be_Called) and then
                             Verbose_Mode
                           then
                              Write_Line
                                ("      -> no binder generated object file");
                           end if;

                           Fail_Program
                             ("no binder generated object file");

                        elsif (not Linker_Needs_To_Be_Called) and then
                        String (Binder_Object_TS) > String (Executable_TS)
                        then
                           Linker_Needs_To_Be_Called := True;

                           if Verbose_Mode then
                              Write_Line
                                ("      -> binder generated object is more " &
                                 "recent than executable");
                           end if;
                        end if;

                     else
                        Fail_Program
                          ("binder exchange file ",
                           Exchange_File_Name,
                           " does not exist");
                     end if;
                  end;
               end loop;
            end if;

            --  Add the global archive

            Global_Archive_TS :=
              File_Stamp
                (Path_Name_Type'
                   (Create_Name (Global_Archive_Name (Main_Proj))));

            if Global_Archive_TS = Empty_Time_Stamp then
               if (not Linker_Needs_To_Be_Called) and then Verbose_Mode then
                  Write_Line ("      -> global archive does not exist");
               end if;

               Fail_Program
                 ("global archive for project file ",
                  Get_Name_String
                    (Project_Tree.Projects.Table (Main_Proj).Name),
                  " does not exist");
            end if;

            if (not Linker_Needs_To_Be_Called) and then
              Global_Archive_Has_Been_Built
            then
               Linker_Needs_To_Be_Called := True;

               if Verbose_Mode then
                  Write_Line ("      -> global archive has just been built");
               end if;
            end if;

            if (not Linker_Needs_To_Be_Called) and then
              String (Global_Archive_TS) > String (Executable_TS)
            then
               Linker_Needs_To_Be_Called := True;

               if Verbose_Mode then
                  Write_Line ("      -> global archive is more recent than " &
                            "executable");
               end if;
            end if;

            Add_Argument
              (Global_Archive_Name (Main_Proj), Verbose_Mode);

            --  Add the library switches, if there are libraries

            Process_Imported_Libraries (Main_Proj);

            for J in reverse 1 .. Library_Projs.Last loop
               if Data.Config.Linker_Lib_Dir_Option = No_Name then
                  Add_Argument
                    ("-L" &
                     Get_Name_String
                       (Project_Tree.Projects.Table
                          (Library_Projs.Table (J)).Library_Dir),
                    Verbose_Mode);

               else
                  Add_Argument
                    (Get_Name_String
                       (Data.Config.Linker_Lib_Dir_Option) &
                     Get_Name_String
                       (Project_Tree.Projects.Table
                          (Library_Projs.Table (J)).Library_Dir),
                     Verbose_Mode);
               end if;

               if Data.Config.Run_Path_Option /= No_Name_List
                 and then
                   Project_Tree.Projects.Table
                     (Library_Projs.Table (J)).Library_Kind /= Static
               then
                  Add_Rpath
                    (Get_Name_String
                       (Project_Tree.Projects.Table
                          (Library_Projs.Table (J)).Library_Dir));
               end if;

               if Data.Config.Linker_Lib_Name_Option = No_Name then
                  Add_Argument
                    ("-l" &
                     Get_Name_String
                       (Project_Tree.Projects.Table
                          (Library_Projs.Table (J)).Library_Name),
                    Verbose_Mode);

               else
                  Add_Argument
                    (Get_Name_String
                       (Data.Config.Linker_Lib_Name_Option) &
                     Get_Name_String
                       (Project_Tree.Projects.Table
                          (Library_Projs.Table (J)).Library_Name),
                     Verbose_Mode);
               end if;
            end loop;

            --  Add the additional options, if any
            --  ??? Shouldn't this be after the command line options?

            for J in 1 .. Binding_Options.Last loop
               Add_Argument (Binding_Options.Table (J), Verbose_Mode);
            end loop;

            --  Add the run path option, if necessary

            if Data.Config.Run_Path_Option /= No_Name_List and then
              Rpaths.Last > 0
            then
               declare
                  Nam_Nod  : Name_Node :=
                               Project_Tree.Name_Lists.Table
                                 (Data.Config.Run_Path_Option);
                  Length   : Natural := 0;
                  Arg      : String_Access := null;
               begin
                  while Nam_Nod.Next /= No_Name_List loop
                     Add_Argument (Get_Name_String (Nam_Nod.Name), True);
                     Nam_Nod :=
                       Project_Tree.Name_Lists.Table (Nam_Nod.Next);
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

                     Arg (Length + 1 .. Length + Rpaths.Table (J)'Length) :=
                       Rpaths.Table (J).all;
                     Length := Length + Rpaths.Table (J)'Length;
                  end loop;

                  Add_Argument (Arg, Verbose_Mode);
               end;
            end if;

            --  Put the options in the project file, if any

            declare
               The_Packages : constant Package_Id :=
                                Project_Tree.Projects.Table
                                  (Main_Proj).Decl.Packages;

               Linker_Package : constant Prj.Package_Id :=
                                  Prj.Util.Value_Of
                                    (Name        => Name_Linker,
                                     In_Packages => The_Packages,
                                     In_Tree     => Project_Tree);

               Switches     : Variable_Value;
               Switch_List  : String_List_Id;
               Element      : String_Element;

            begin
               if Linker_Package /= No_Package then
                  declare
                     Defaults : constant Array_Element_Id :=
                                  Prj.Util.Value_Of
                                    (Name      => Name_Default_Switches,
                                     In_Arrays =>
                                       Project_Tree.Packages.Table
                                         (Linker_Package).Decl.Arrays,
                                     In_Tree   => Project_Tree);

                     Switches_Array : constant Array_Element_Id :=
                                        Prj.Util.Value_Of
                                          (Name      => Name_Switches,
                                           In_Arrays =>
                                             Project_Tree.Packages.Table
                                               (Linker_Package).Decl.Arrays,
                                           In_Tree   => Project_Tree);
                     Option   : String_Access;

                  begin
                     Switches :=
                       Prj.Util.Value_Of
                         (Index     => Name_Id (Main_Id),
                          Src_Index => 0,
                          In_Array  => Switches_Array,
                          In_Tree   => Project_Tree);

                     if Switches = Nil_Variable_Value then
                        Switches :=
                          Prj.Util.Value_Of
                            (Index     => Main_Source.Language_Name,
                             Src_Index => 0,
                             In_Array  => Switches_Array,
                             In_Tree   => Project_Tree,
                             Force_Lower_Case_Index => True);
                     end if;

                     if Switches = Nil_Variable_Value then
                        Switches :=
                          Prj.Util.Value_Of
                            (Index     => Main_Source.Language_Name,
                             Src_Index => 0,
                             In_Array  => Defaults,
                             In_Tree   => Project_Tree);
                     end if;

                     case Switches.Kind is
                        when Undefined | Single =>
                           null;

                        when Prj.List =>
                           Switch_List := Switches.Values;

                           while Switch_List /= Nil_String loop
                              Element :=
                                Project_Tree.String_Elements.Table
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

            --  Finally add the linker switches specified on the command line

            for J in 1 .. Command_Line_Linker_Options.Last loop
               Add_Argument
                 (Command_Line_Linker_Options.Table (J), Verbose_Mode);
            end loop;

            --  Add the switch(es) to specify the name of the executable

            declare
               List : Name_List_Index :=
                        Data.Config.Linker_Executable_Option;
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
                     Simple_Name => not Verbose_Mode);
               end Add_Executable_Name;

            begin
               if List /= No_Name_List then
                  loop
                     Nam := Project_Tree.Name_Lists.Table (List);
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

            if not Linker_Needs_To_Be_Called then
               if Verbose_Mode then
                     Write_Line ("      -> up to date");

               elsif not Quiet_Output then
                  Inform (Exec_Name, "up to date");
               end if;

            else
               Display_Command (Linker_Name.all, Linker_Path);

               Spawn
                 (Linker_Path.all, Arguments (1 .. Last_Argument), Success);

               if not Success then
                  Fail_Program ("link of ", Main, " failed");
               end if;
            end if;
         end;
      end loop;
   end Linking_Phase;

   ---------------------
   -- Need_To_Compile --
   ---------------------

   function Need_To_Compile (Source : Source_Id) return Boolean is
      Src_Data : constant Source_Data := Project_Tree.Sources.Table (Source);

      Source_Path   : constant String :=
                        Get_Name_String (Src_Data.Path);

      Object_Name   : constant String :=
                        Get_Name_String (Src_Data.Object);
      C_Object_Name : String := Object_Name;

      Object_Path   : constant String :=
                        Get_Name_String (Src_Data.Object_Path);
      Dep_Name      : String_Access;

      Switches_Name : constant String :=
                        Get_Name_String (Src_Data.Switches_Path);

      Dep_File : Prj.Util.Text_File;
      Start    : Natural;
      Finish   : Natural;
      Last_Obj : Natural;

      Looping : Boolean := False;
      --  Set to True at the end of the first Big_Loop for Makefile fragments

      Source_In_Dependencies : Boolean := False;
      --  Set True if source was found in dependency file of its object file

      Num_Ext  : Natural;
      --  Number of extending projects

      ALI_Project : Project_Id;
      --  If the ALI file is in the object directory of a project, this is
      --  the project id.

      Externally_Built : constant Boolean :=
                           Project_Tree.Projects.Table
                             (Src_Data.Project).Externally_Built;
      --  True if the project of the source is externally built

   begin
      if Force_Compilations then
         return not Externally_Built;
      end if;

      Canonical_Case_File_Name (C_Object_Name);

      if Verbose_Mode then
         Write_Str  ("   Checking ");
         Write_Str  (Source_Path);
         Write_Line (" ... ");
      end if;

      --  No need to compile if project is externally built

      if Externally_Built then
         if Verbose_Mode then
            Write_Line ("      project is externally built");
         end if;

         return False;
      end if;

      --  If no object file is generated, the "compiler" need to be invoked

      if not Project_Tree.Languages_Data.Table
        (Src_Data.Language).Config.Object_Generated
      then
         if Verbose_Mode then
            Write_Line ("      -> no object file generated");
         end if;

         return True;
      end if;

      --  If object file does not exist, of course source need to be compiled

      if Src_Data.Object_TS = Empty_Time_Stamp then
         if Verbose_Mode then
            Write_Str  ("      -> object file ");
            Write_Str  (Object_Path);
            Write_Line (" does not exist");
         end if;

         return True;
      end if;

      --  If the object file has been created before the last modification
      --  of the source, the source need to be recompiled.

      if Src_Data.Object_TS < Src_Data.Source_TS then
         if Verbose_Mode then
            Write_Str  ("      -> object file ");
            Write_Str  (Object_Path);
            Write_Line (" has time stamp earlier than source");
         end if;

         return True;
      end if;

      if Src_Data.Dependency /= None then

         Dep_Name := new String'(Get_Name_String (Src_Data.Dep_Path));

         --  If there is no dependency file, then the source needs to be
         --  recompiled and the dependency file need to be created.

         if Src_Data.Dep_TS = Empty_Time_Stamp then
            if Verbose_Mode then
               Write_Str  ("      -> dependency file ");
               Write_Str  (Dep_Name.all);
               Write_Line (" does not exist");
            end if;

            return True;
         end if;

         --  The source needs to be recompiled if the source has been modified
         --  after the dependency file has been created.

         if Src_Data.Dep_TS < Src_Data.Source_TS then
            if Verbose_Mode then
               Write_Str  ("      -> dependency file ");
               Write_Str  (Dep_Name.all);
               Write_Line (" has time stamp earlier than source");
            end if;

            return True;
         end if;
      end if;

      --  If we are checking the switches and there is no switches file, then
      --  the source needs to be recompiled and the switches file need to be
      --  created.

      if Check_Switches then
         if Src_Data.Switches_TS = Empty_Time_Stamp then
            if Verbose_Mode then
               Write_Str  ("      -> switches file ");
               Write_Str  (Switches_Name);
               Write_Line (" does not exist");
            end if;

            return True;
         end if;

         --  The source needs to be recompiled if the source has been modified
         --  after the switches file has been created.

         if  Src_Data.Switches_TS < Src_Data.Source_TS then
            if Verbose_Mode then
               Write_Str  ("      -> switches file ");
               Write_Str  (Switches_Name);
               Write_Line (" has time stamp earlier than source");
            end if;

            return True;
         end if;
      end if;

      case Src_Data.Dependency is
         when None =>
            null;

         when Makefile =>
            Open (Dep_File, Dep_Name.all);

            --  If dependency file cannot be open, we need to recompile
            --  the source.

            if not Is_Valid (Dep_File) then
               if Verbose_Mode then
                  Write_Str  ("      -> could not open dependency file ");
                  Write_Line (Dep_Name.all);
               end if;

               return True;
            end if;

            --  Loop Big_Loop is executed several times only when the
            --  dependency file contains several times
            --     <object file>: <source1> ...
            --  When there is only one of such occurence, Big_Loop is exited
            --  successfully at the beginning of the second loop.

            Big_Loop :
            loop
               declare
                  End_Of_File_Reached : Boolean := False;

               begin
                  loop
                     if End_Of_File (Dep_File) then
                        End_Of_File_Reached := True;
                        exit;
                     end if;

                     Get_Line (Dep_File, Name_Buffer, Name_Len);

                     exit when Name_Len > 0 and then Name_Buffer (1) /= '#';
                  end loop;

                  --  If dependency file contains only empty lines or comments,
                  --  then dependencies are unknown, and the source needs to be
                  --  recompiled.

                  if End_Of_File_Reached then
                     --  If we have reached the end of file after the first
                     --  loop, there is nothing else to do.

                     exit Big_Loop when Looping;

                     if Verbose_Mode then
                        Write_Str  ("      -> dependency file ");
                        Write_Str  (Dep_Name.all);
                        Write_Line (" is empty");
                     end if;

                     Close (Dep_File);
                     return True;
                  end if;
               end;

               Start  := 1;
               Finish := Index (Name_Buffer (1 .. Name_Len), ": ");

               if Finish /= 0 then
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

                  Canonical_Case_File_Name (Name_Buffer (Start .. Last_Obj));
               end if;

               --  First line must start with name of object file, followed by
               --  colon.

               if Finish = 0 or else
                 Name_Buffer (Start .. Last_Obj) /= C_Object_Name
               then
                  if Verbose_Mode then
                     Write_Str  ("      -> dependency file ");
                     Write_Str  (Dep_Name.all);
                     Write_Line (" has wrong format");

                     if Finish = 0 then
                        Write_Line ("         no colon");

                     else
                        Write_Str  ("         expected object file name ");
                        Write_Str  (C_Object_Name);
                        Write_Str  (", got ");
                        Write_Line (Name_Buffer (Start .. Last_Obj));
                     end if;
                  end if;

                  Close (Dep_File);
                  return True;

               else
                  Start := Finish + 2;

                  --  Process each line

                  Line_Loop : loop
                     declare
                        Line : String  := Name_Buffer (1 .. Name_Len);
                        Last : Natural := Name_Len;

                     begin
                        Name_Loop : loop

                           --  Find the beginning of the next source path name

                           while Finish < Last and then Line (Start) = ' ' loop
                              Start := Start + 1;
                           end loop;

                           --  Go to next line when there is a continuation
                           --  character \ at the end of the line.

                           exit Name_Loop when Start = Last
                             and then Line (Start) = '\';

                           --  We should not be at the end of the line, without
                           --  a continuation character \.

                           if Start = Last then
                              if Verbose_Mode then
                                 Write_Str  ("      -> dependency file ");
                                 Write_Str  (Dep_Name.all);
                                 Write_Line (" has wrong format");
                              end if;

                              Close (Dep_File);
                              return True;
                           end if;

                           --  Look for the end of the source path name

                           Finish := Start;

                           while Finish < Last loop
                              if Line (Finish) = '\' then
                                 --  On Windows, a '\' is part of the path
                                 --  name, except when it is not the first
                                 --  character followed by another '\' or by a
                                 --  space. On other platforms, when we are
                                 --  getting a '\' that is not the last
                                 --  character of the line, the next character
                                 --  is part of the path name, even if it is a
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
                              Src_TS   : Time_Stamp_Type;

                              Source   : Source_Id;

                           begin
                              --  If it is original source, set
                              --  Source_In_Dependencies.

                              if Src_Name = Source_Path then
                                 Source_In_Dependencies := True;
                              end if;

                              Name_Len := 0;
                              Add_Str_To_Name_Buffer (Src_Name);
                              Src_TS :=
                                File_Stamp (File_Name_Type'(Name_Find));

                              --  If the source does not exist, we need to
                              --  recompile.

                              if Src_TS = Empty_Time_Stamp then
                                 if Verbose_Mode then
                                    Write_Str  ("      -> source ");
                                    Write_Str  (Src_Name);
                                    Write_Line (" does not exist");
                                 end if;

                                 Close (Dep_File);
                                 return True;

                                 --  If the source has been modified after the
                                 --  object file, we need to recompile.

                              elsif Src_TS > Src_Data.Object_TS then
                                 if Verbose_Mode then
                                    Write_Str  ("      -> source ");
                                    Write_Str  (Src_Name);
                                    Write_Line
                                    (" has time stamp later than object file");
                                 end if;

                                 Close (Dep_File);
                                 return True;

                              else
                                 Name_Len := Src_Name'Length;
                                 Name_Buffer (1 .. Name_Len) := Src_Name;
                                 Source := Source_Paths_Htable.Get
                                   (Project_Tree.Source_Paths_HT, Name_Find);

                                 if Source /= No_Source and then
                                   Project_Tree.Sources.Table
                                     (Source).Replaced_By /= No_Source
                                 then
                                    if Verbose_Mode then
                                       Write_Str  ("      -> source ");
                                       Write_Str  (Src_Name);
                                       Write_Line (" has been replaced");
                                    end if;

                                    Close (Dep_File);
                                    return True;
                                 end if;
                              end if;
                           end;

                           --  If the source path name ends the line, we are
                           --  done.

                           exit Line_Loop when Finish = Last;

                           --  Go get the next source on the line

                           Start := Finish + 1;
                        end loop Name_Loop;
                     end;

                     --  If we are here, we had a continuation character \ at
                     --  the end of the line, so we continue with the next
                     --  line.

                     Get_Line (Dep_File, Name_Buffer, Name_Len);
                     Start  := 1;
                     Finish := 1;
                  end loop Line_Loop;
               end if;

               --  Set Looping at the end of the first loop
               Looping := True;
            end loop Big_Loop;

            Close (Dep_File);

            --  If the original sources were not in the dependency file, then
            --  we need to recompile. It may mean that we are using a different
            --  source (different variant) for this object file.

            if not Source_In_Dependencies then
               if Verbose_Mode then
                  Write_Str  ("      -> source ");
                  Write_Str  (Source_Path);
                  Write_Line (" is not in the dependencies");
               end if;

               return True;
            end if;

         when ALI_File =>
            declare
               use type ALI.ALI_Id;
               Text     : Text_Buffer_Ptr :=
                            Read_Library_Info
                              (File_Name_Type (Src_Data.Dep_Path));
               The_ALI  : ALI.ALI_Id;
               Sfile    : File_Name_Type;
               Stamp    : Time_Stamp_Type;
               Dep_Src  : Source_Id;
--               Found    : Boolean;
               Proj     : Project_Id;

            begin
               if Text = null then
                  if Verbose_Mode then
                     Write_Str ("    -> cannot read ");
                     Write_Line (Get_Name_String (Src_Data.Dep_Path));
                  end if;

                  return True;
               end if;

               --  Read the ALI file but read only the necessary lines.

               The_ALI :=
                 ALI.Scan_ALI
                   (File_Name_Type (Src_Data.Dep_Path),
                    Text,
                    Ignore_ED     => False,
                    Err           => True,
                    Read_Lines    => "PD");
               Free (Text);

               if The_ALI = ALI.No_ALI_Id then
                  if Verbose_Mode then
                     Write_Str ("    -> ");
                     Write_Str (Get_Name_String (Src_Data.Dep_Path));
                     Write_Line (" is incorrectly formatted");
                  end if;

                  return True;
               end if;

               if ALI.ALIs.Table (The_ALI).Compile_Errors then
                  if Verbose_Mode then
                     Write_Line ("    -> last compilation had errors");
                  end if;

                  return True;
               end if;

               --  We need to check that the ALI file is in the correct object
               --  directory. If it is in the object directory of a project
               --  that is extended and it depends on a source that is in one
               --  of its extending projects, then the ALI file is not in the
               --  correct object directory.

               ALI_Project := Src_Data.Object_Project;

               --  Count the extending projects

               Num_Ext := 0;
               Proj := ALI_Project;
               loop
                  Proj := Project_Tree.Projects.Table (Proj).Extended_By;
                  exit when Proj = No_Project;
                  Num_Ext := Num_Ext + 1;
               end loop;

               declare
                  Projects : array (1 .. Num_Ext) of Project_Id;
               begin
                  Proj := ALI_Project;
                  for J in Projects'Range loop
                     Proj := Project_Tree.Projects.Table (Proj).Extended_By;
                     Projects (J) := Proj;
                  end loop;

                  for D in ALI.ALIs.Table (The_ALI).First_Sdep ..
                    ALI.ALIs.Table (The_ALI).Last_Sdep
                  loop
                     Sfile := ALI.Sdep.Table (D).Sfile;
                     Stamp := ALI.Sdep.Table (D).Stamp;

                     if Stamp /= Empty_Time_Stamp then
                        Dep_Src := Project_Tree.First_Source;

                        while Dep_Src /= No_Source loop
                           Initialize_Source_Record (Dep_Src);

                           if (not Project_Tree.Sources.Table
                               (Dep_Src).Locally_Removed)
                             and then
                               Project_Tree.Sources.Table (Dep_Src).Unit /=
                               No_Name
                               and then
                                 Project_Tree.Sources.Table
                                   (Dep_Src).File = Sfile
                           then
                              if Stamp /=
                                Project_Tree.Sources.Table (Dep_Src).Source_TS
                              then
                                 if Verbose_Mode then
                                    Write_Str
                                      ("   -> different time stamp for ");
                                    Write_Line (Get_Name_String (Sfile));

                                    if Debug_Flag_T then
                                       Write_Str ("   in ALI file: ");
                                       Write_Line (String (Stamp));
                                       Write_Str ("   actual file: ");
                                       Write_Line
                                         (String (Project_Tree.Sources.Table
                                          (Dep_Src).Source_TS));
                                    end if;
                                 end if;

                                 return True;

                              else
                                 for J in Projects'Range loop
                                    if Project_Tree.Sources.Table
                                      (Dep_Src).Project = Projects (J)
                                    then
                                       if Verbose_Mode then
                                          Write_Line
                                            ("   -> wrong object directory");
                                       end if;

                                       return True;
                                    end if;
                                 end loop;

                                 exit;
                              end if;
                           end if;

                           Dep_Src :=
                             Project_Tree.Sources.Table
                               (Dep_Src).Next_In_Sources;
                        end loop;
                     end if;
                  end loop;
               end;
            end;
      end case;

      --  If we are here, then everything is OK, and we don't need
      --  to recompile.

      if (not Check_Switches) and then Verbose_Mode then
         Write_Line ("      -> up to date");
      end if;

      return False;

   end Need_To_Compile;

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
                  Force_Compilations := True;

               when Keep_Going_Option =>
                  Keep_Going := True;

               when Maximum_Processes_Option =>
                  Maximum_Processes :=
                    Command_Line_Options.Table (Index).Value;

               when Quiet_Output_Option =>
                  Quiet_Output := True;
                  Verbose_Mode := False;

               when Check_Switches_Option =>
                  Check_Switches := True;

               when Verbose_Mode_Option =>
                  Verbose_Mode := True;
                  Quiet_Output := False;

               when Warnings_Treat_As_Error =>
                  Warning_Mode := Treat_As_Error;

               when Warnings_Normal =>
                  Warning_Mode := Normal;

               when Warnings_Suppress =>
                  Warning_Mode := Suppress;
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

   --------------------------------
   -- Process_Imported_Libraries --
   --------------------------------

   procedure Process_Imported_Libraries
     (For_Project        : Project_Id;
      And_Project_Itself : Boolean := False)
   is

      procedure Process_Project (Project : Project_Id);
      --  Process Project and its imported projects recursively.
      --  Add any library projects to table Library_Projs.

      ---------------------
      -- Process_Project --
      ---------------------

      procedure Process_Project (Project : Project_Id) is
         Data     : constant Project_Data :=
                      Project_Tree.Projects.Table (Project);
         Imported : Project_List := Data.Imported_Projects;
         Element  : Project_Element;

      begin
         --  Nothing to do if process has already been processed

         if not Processed_Projects.Get (Data.Name) then
            Processed_Projects.Set (Data.Name, True);

            --  Call Process_Project recursively for any imported project.
            --  We first process the imported projects to guarantee that
            --  we have a proper reverse order for the libraries.

            while Imported /= Empty_Project_List loop
               Element :=
                 Project_Tree.Project_Lists.Table (Imported);

               if Element.Project /= No_Project then
                  Process_Project (Element.Project);
               end if;

               Imported := Element.Next;
            end loop;

            --  If it is a library project, add it to Library_Projs

            if (And_Project_Itself or (Project /= For_Project))
              and then Data.Library
            then
               Library_Projs.Increment_Last;
               Library_Projs.Table (Library_Projs.Last) := Project;
            end if;

         end if;
      end Process_Project;

      --  Start of processing for Process_Imported_Libraries

   begin
      Processed_Projects.Reset;
      Library_Projs.Init;

      Process_Project (For_Project);
   end Process_Imported_Libraries;

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

         Current := Project_Tree.Projects.Table (Current).Extends;
      end loop;
   end Project_Extends;

   -----------
   -- Queue --
   -----------

   package body Queue is
      type Q_Record is record
         Name : File_Name_Type;
         Id   : Source_Id;
         Proj : Project_Id;
      end record;

      package Q is new Table.Table
        (Table_Component_Type => Q_Record,
         Table_Index_Type     => Natural,
         Table_Low_Bound      => 1,
         Table_Initial        => 1000,
         Table_Increment      => 100,
         Table_Name           => "Makegpr.Queue.Q");
      --  This is the actual Q

      Q_Front : Natural := 0;

      -------------
      -- Extract --
      -------------

      procedure Extract
        (Source_File_Name : out File_Name_Type;
         Source_Identity  : out Source_Id;
         Source_Project   : out Project_Id)
      is
      begin
         Source_File_Name := Q.Table (Q_Front).Name;
         Source_Identity  := Q.Table (Q_Front).Id;
         Source_Project   := Q.Table (Q_Front).Proj;
         Q_Front := Q_Front + 1;
      end Extract;

      -----------
      -- First --
      -----------

      function First return Natural is
      begin
         return Q_Front;
      end First;

      ----------
      -- Init --
      ----------

      procedure Init is
      begin
         Q.Init;
         Q_Front := 1;
      end Init;

      ------------
      -- Insert --
      ------------

      procedure Insert
        (Source_File_Name : File_Name_Type;
         Source_Identity  : Source_Id;
         Source_Project   : Project_Id)
      is
      begin
         for Index in 1 .. Q.Last loop
            if Q.Table (Index).Id = Source_Identity then
               return;
            end if;
         end loop;

         if Current_Verbosity = High then
            Write_Str ("Adding """);
            Write_Str (Get_Name_String (Source_File_Name));
            Write_Line (""" to the queue");
         end if;

         Q.Append
           (New_Val =>
              (Name => Source_File_Name,
               Id   => Source_Identity,
               Proj => Source_Project));
      end Insert;

      ----------------------------
      -- Insert_Project_Sources --
      ----------------------------

      procedure Insert_Project_Sources
        (The_Project  : Project_Id;
         All_Projects : Boolean;
         Unit_Based   : Boolean)
      is
         Source : Source_Data;

      begin
         for Index in 1 .. Source_Data_Table.Last (Project_Tree.Sources) loop
            Source := Project_Tree.Sources.Table (Index);

            if Source.Compiled
               and then
                (All_Projects
                   or else
                 Is_Extending (The_Project, Source.Project, Project_Tree))
               and then
                not Source.Locally_Removed
               and then
                Source.Replaced_By = No_Source
               and then
                not Project_Tree.Projects.Table
                                            (Source.Project).Externally_Built
               and then
                Source.Kind /= Sep
               and then
                Source.Path /= No_Path
            then
               if Source.Kind = Impl or else
                 (Source.Unit /= No_Name and then
                  Source.Kind = Spec and then
                  Source.Other_Part = No_Source)
               then
                  if (Unit_Based or else
                      Source.Unit = No_Name or else
                      Project_Tree.Projects.Table (Source.Project).Library)
                     and then
                     not Is_Subunit (Source)
                  then
                     Insert
                       (Source_File_Name => Source.File,
                        Source_Identity  => Index,
                        Source_Project   =>
                          Ultimate_Extending_Project_Of (Source.Project));
                  end if;
               end if;
            end if;
         end loop;
      end Insert_Project_Sources;

      --------------
      -- Is_Empty --
      --------------

      function Is_Empty return Boolean is
      begin
         return Q_Front > Q.Last;
      end Is_Empty;

      ----------
      -- Size --
      ----------

      function Size return Natural is
      begin
         return Q.Last;
      end Size;

   end Queue;

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
      Ext : constant Project_Id :=
              Project_Tree.Projects.Table (Project).Extends;
      L   : Project_List :=
              Project_Tree.Projects.Table (Project).Imported_Projects;
      E   : Project_Element;

   begin
      if Ext /= No_Project and then
        not Imports.Get (Ext)
      then
         Imports.Set (Ext, True);
         Recursive_Import (Ext);
      end if;

      while L /= Empty_Project_List loop
         E := Project_Tree.Project_Lists.Table (L);

         if not Imports.Get (E.Project) then
            Imports.Set (E.Project, True);
            Recursive_Import (E.Project);
         end if;

         L := E.Next;
      end loop;
   end Recursive_Import;

   --------------
   -- Scan_Arg --
   --------------

   procedure Scan_Arg (Arg : String; Command_Line : Boolean) is
      Processed : Boolean := True;
   begin
      pragma Assert (Arg'First = 1);

      if Arg'Length = 0 then
         return;
      end if;

      --  If preceding switch was -P, a project file name need to be
      --  specified, not a switch.

      if Project_File_Name_Expected then
         if Arg (1) = '-' then
            Fail_Program ("project file name missing after -P");
         else
            Project_File_Name_Expected := False;
            Project_File_Name := new String'(Arg);
         end if;

         --  If preceding switch was -o, an executable name need to be
         --  specified, not a switch.

      elsif Output_File_Name_Expected then
         if Arg (1) = '-' then
            Fail_Program ("output file name missing after -o");
         else
            Output_File_Name_Expected := False;
            Output_File_Name := new String'(Arg);
         end if;

      elsif Search_Project_Dir_Expected then
         if Arg (1) = '-' then
            Fail_Program ("directory name missing after -aP");
         else
            Search_Project_Dir_Expected := False;
            Add_Search_Project_Directory (Arg);
         end if;

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

         --  -gargs     options directly for gprbuild

      elsif Arg = "-gargs" then
         Current_Processor := None;

         --  A special test is needed for the -o switch within a -largs since
         --  that is another way to specify the name of the final executable.

      elsif Command_Line
            and then Current_Processor = Linker
            and then Arg = "-o"
      then
            Fail_Program
              ("switch -o not allowed within a -largs. Use -o directly.");

         --  If current processor is not gprbuild directly, store the option
         --  in the appropriate table.

      elsif Current_Processor /= None then
         Add_Option (Arg, Command_Line);

         --  Switches start with '-'

      elsif Arg (1) = '-' then

         if Command_Line and then Arg = "--display-paths" then
            Display_Paths := True;

         elsif Command_Line
           and then
         Arg'Length > Config_Project_Option'Length
           and then
         Arg (1 .. Config_Project_Option'Length) = Config_Project_Option
         then
            if Config_Project_File_Name /= null then
               Fail_Program
                 ("several configuration switches cannot be specified");

            elsif Target_Name /= null then
               Fail_Program
                 ("configuration and target switches cannot be used together");

            else
               Autoconfiguration := False;
               Config_Project_File_Name :=
                 new String'
                   (Arg (Config_Project_Option'Length + 1 .. Arg'Last));
            end if;

         elsif Command_Line
           and then
            Arg'Length > Autoconf_Project_Option'Length
           and then
            Arg (1 .. Autoconf_Project_Option'Length) =
              Autoconf_Project_Option
         then
            if Config_Project_File_Name /= null then
               Fail_Program
                 ("several configuration switches cannot be specified");

            elsif Target_Name /= null then
               Fail_Program
                 ("configuration and target switches cannot be used together");

            else
               Config_Project_File_Name :=
                 new String'
                   (Arg (Autoconf_Project_Option'Length + 1 .. Arg'Last));
            end if;

         elsif Command_Line
           and then
            Arg'Length > Target_Project_Option'Length
           and then
            Arg (1 .. Target_Project_Option'Length) = Target_Project_Option
         then
            if Target_Name /= null then
               Fail_Program
                 ("several target switches cannot be specified");

            elsif Config_Project_File_Name /= null then
               Fail_Program
                 ("configuration and target switches cannot be used together");

            else
               Target_Name :=
                 new String'
                   (Arg (Target_Project_Option'Length + 1 .. Arg'Last));
            end if;

         elsif Command_Line and then
           Arg'Length > Subdirs_Option'Length and then
           Arg (1 .. Subdirs_Option'Length) =
           Subdirs_Option
         then
            Subdirs :=
              new String'(Arg (Subdirs_Option'Length + 1 .. Arg'Last));

         elsif Command_Line and then Arg = Direct_Import_Only_Switch then
            Direct_Import_Only := True;

         elsif Command_Line and then
           Arg'Length >= 3 and then
           Arg (1 .. 3) = "-aP"
         then
            if Arg'Length = 3 then
               Search_Project_Dir_Expected := True;

            else
               Add_Search_Project_Directory (Arg (4 .. Arg'Last));
            end if;

         elsif Command_Line and then Arg = "-b" then
            Bind_Only  := True;
            All_Phases := False;

         elsif Command_Line and then Arg = "-c" then
            Compile_Only := True;
            All_Phases   := False;

            if Link_Only then
               Bind_Only := True;
            end if;

         elsif Arg = "-C" then
            --  This switch is only for upward compatibility

            null;

         elsif Command_Line and then Arg = "-d" then
            Display_Compilation_Progress := True;

         elsif Command_Line and then
         Arg'Length = 3 and then
         Arg (2) = 'd'
         then
            if Arg (3) in '1' .. '9' or else
              Arg (3) in 'a' .. 'z' or else
              Arg (3) in 'A' .. 'Z'
            then
               Set_Debug_Flag (Arg (3));

            else
               Fail_Program ("illegal debug switch ", Arg);
            end if;

         elsif Command_Line and then Arg = "-eL" then
            Follow_Links_For_Files := True;

         elsif Arg = "-f" then
            Force_Compilations := True;
            Need_To_Rebuild_Global_Archives := True;

            if Command_Line then
               Register_Command_Line_Option (Force_Compilations_Option);
            end if;

         elsif Command_Line and then Arg = "-F" then
            Full_Path_Name_For_Brief_Errors := True;

         elsif Command_Line and then Arg = "-h" then
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

               if Max_Proc = 0 then
                  Processed := False;
               end if;

               if Processed then
                  Maximum_Processes := Max_Proc;
               end if;
            end;

            if Processed and then Command_Line then
               Register_Command_Line_Option
                 (Maximum_Processes_Option, Maximum_Processes);
            end if;

         elsif Arg = "-k" then
            Keep_Going := True;

            if Command_Line then
               Register_Command_Line_Option (Keep_Going_Option);
            end if;

         elsif Command_Line and then Arg = "-l" then
            Link_Only  := True;
            All_Phases := False;

            if Compile_Only then
               Bind_Only := True;
            end if;

         elsif Command_Line and then Arg = "-o" then
            if Output_File_Name /= null then
               Fail_Program ("cannot specify several -o switches");

            else
               Output_File_Name_Expected := True;
            end if;

         elsif Command_Line and then
           (Arg = "-p" or else Arg = "--create-missing-dirs")
         then
            Setup_Projects := True;

         elsif Command_Line and then
         Arg'Length >= 2 and then Arg (2) = 'P'
         then
            if Project_File_Name /= null then
               Fail_Program ("cannot have several project files specified");

            elsif Arg'Length = 2 then
               Project_File_Name_Expected := True;

            else
               Project_File_Name := new String'(Arg (3 .. Arg'Last));
            end if;

         elsif Arg = "-q" then
            Quiet_Output := True;
            Verbose_Mode := False;

            if Command_Line then
               Register_Command_Line_Option (Quiet_Output_Option);
            end if;

         elsif Command_Line and then Arg = "-r" then
            Recursive := True;

         elsif Arg = "-s" then
            Check_Switches := True;

            if Command_Line then
               Register_Command_Line_Option (Check_Switches_Option);
            end if;

         elsif Command_Line and then Arg = "-u" then
            Unique_Compile := True;

         elsif Command_Line and then Arg = "-U" then
            Unique_Compile_All_Projects := True;
            Unique_Compile := True;

         elsif Arg = "-v" then
            Verbose_Mode := True;
            Quiet_Output := False;

            if Command_Line then
               Register_Command_Line_Option (Verbose_Mode_Option);
            end if;

         elsif Command_Line
           and then Arg'Length = 4 and then Arg (1 .. 3) = "-vP"
           and then Arg (4) in '0' .. '2'
         then
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
            Warning_Mode := Treat_As_Error;

            if Command_Line then
               Register_Command_Line_Option (Warnings_Treat_As_Error);
            end if;

         elsif Arg = "-wn" then
            Warning_Mode := Normal;

            if Command_Line then
               Register_Command_Line_Option (Warnings_Normal);
            end if;

         elsif Arg = "-ws" then
            Warning_Mode  := Suppress;

            if Command_Line then
               Register_Command_Line_Option (Warnings_Suppress);
            end if;

         elsif Command_Line
           and then Arg'Length >= 3
           and then Arg (2) = 'X'
           and then Is_External_Assignment (Arg)
         then
            --  Is_External_Assignment has side effects when it returns True

            null;

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
                    ("cannot have several project files specified");

               else
                  Project_File_Name := new String'(File_Name);
               end if;

            else
               --  Not a project file, then it is a main

               Mains.Add_Main (Arg);
            end if;
         end;

      else
         Processed := False;
      end if;

      if not Processed then
         if Command_Line then
            Finish_Program
              (True, "illegal option """, Arg, """");

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
               Finish_Program
                 (True,
                  "illegal option """,
                  Arg,
                  """ in package Builder of project file """ &
                  Project_File_Name.all & '"');
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
      Delete_All_Temp_Files;
      OS_Exit (1);
   end Sigint_Intercepted;

   ---------------
   -- Source_Of --
   ---------------

   function Source_Of (File : String) return Source_Data is
      File_Id : File_Name_Type;
      Source : Source_Data;
   begin
      Name_Len := 0;
      Add_Str_To_Name_Buffer (File);
      Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));
      File_Id := Name_Find;

      for Index in 1 .. Source_Data_Table.Last (Project_Tree.Sources) loop
         Source := Project_Tree.Sources.Table (Index);

         if Source.File = File_Id then
            return Source;
         end if;
      end loop;

      return No_Source_Data;
   end Source_Of;

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

   -----------------------------------
   -- Ultimate_Extending_Project_Of --
   -----------------------------------

   function Ultimate_Extending_Project_Of (Proj : Project_Id)
                                           return Project_Id
   is
      Prj : Project_Id := Proj;

   begin
      while
        Project_Tree.Projects.Table (Prj).Extended_By /= No_Project
      loop
         Prj := Project_Tree.Projects.Table (Prj).Extended_By;
      end loop;

      return Prj;
   end Ultimate_Extending_Project_Of;

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

         Write_Str ("  ");
         Write_Str (Autoconf_Project_Option);
         Write_Str ("file.cgpr");
         Write_Eol;
         Write_Str
           ("           Specify/create the main config project file name");
         Write_Eol;

         --  Line for Target_Project_Option

         Write_Str ("  ");
         Write_Str (Target_Project_Option);
         Write_Str ("targetname");
         Write_Eol;
         Write_Str
           ("           Specify a target for cross platforms");
         Write_Eol;

         Write_Str ("  --subdirs=dir");
         Write_Eol;
         Write_Str ("           Real obj/lib/exec dirs are subdirs");
         Write_Eol;

         Write_Str ("  ");
         Write_Str (Direct_Import_Only_Switch);
         Write_Eol;
         Write_Str
           ("           Sources can import only from withed projects");
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

         --  Line for -eL

         Write_Str ("  -eL      " &
                    "Follow symbolic links when processing project files");
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
         Write_Eol;

      end if;
   end Usage;

end Buildgpr;
