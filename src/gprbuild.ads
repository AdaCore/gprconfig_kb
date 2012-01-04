------------------------------------------------------------------------------
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             G P R B U I L D                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2004-2012, Free Software Foundation, Inc.          --
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

--  The following package implements the facilities to compile, bind and/or
--  link a set of Ada and non Ada sources, specified in Project Files.

private with Ada.Unchecked_Deallocation;

private with GNAT.Dynamic_Tables;
private with GNAT.HTable;
private with GNAT.OS_Lib;

with ALI;
with Namet;       use Namet;
with Opt;
with Prj;         use Prj;
with Table;
with Types;       use Types;

package Gprbuild is

   --  Everyting private so only accessible to child packages

private

   use GNAT.OS_Lib;

   use type ALI.ALI_Id, Opt.Verbosity_Level_Type, Opt.Warning_Mode_Type;

   Object_Suffix : constant String := Get_Target_Object_Suffix.all;
   --  The suffix of object files on this platform

   Dash_L : Name_Id;
   --  "-L", initialized in procedure Initialize

   Main_Project_Dir : String_Access;
   --  The absolute path of the project directory of the main project,
   --  initialized in procedure Initialize.

   Executable_Suffix : constant String_Access := Get_Executable_Suffix;
   --  The suffix of executables on this platforms

   Main_Index : Int := 0;

   Project_Tree : constant Project_Tree_Ref :=
                    new Project_Tree_Data (Is_Root_Tree => True);
   --  The project tree

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

   Object_Checked : Boolean := True;
   --  False when switch --no-object-check is used. When True, presence of
   --  the object file and its time stamp are checked to decide if a file needs
   --  to be compiled.

   Map_File : String_Access := null;
   --  Value of switch --create-map-file

   Indirect_Imports : Boolean := True;
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

   Builder_Switches_Lang : Name_Id := No_Name;
   --  Used to decide to what compiler the Builder'Default_Switches that
   --  are not recognized by gprbuild should be given.

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

   --  Archive builder name, path and options

   Archive_Builder_Name        : String_Access := null;
   Archive_Builder_Path        : String_Access := null;
   Archive_Builder_Opts        : Options_Data;
   Archive_Builder_Append_Opts : Options_Data;

   --  Archive indexer name, path and options

   Archive_Indexer_Name : String_Access := null;
   Archive_Indexer_Path : String_Access := null;
   Archive_Indexer_Opts : Options_Data;

   --  Libraries

   type Library_Project is record
      Proj          : Project_Id;
      Is_Aggregated : Boolean;
   end record;

   package Library_Projs is new Table.Table (
     Table_Component_Type => Library_Project,
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

   function Get_Option (Option : Name_Id) return String_Access;
   --  Get a string access corresponding to Option. Either find the string
   --  access in the All_Options cache, or create a new entry in All_Options.

   procedure Test_If_Relative_Path
     (Switch           : in out String_Access;
      Parent           : String;
      Including_Switch : Name_Id);

   procedure Add_Option_Internal
     (Value       : String_Access;
      To          : in out Options_Data;
      Display     : Boolean;
      Simple_Name : Boolean := False);

   procedure Process_Imported_Libraries
     (For_Project        : Project_Id;
      There_Are_SALs     : out Boolean;
      And_Project_Itself : Boolean := False);
   --  Get the imported library project ids in table Library_Projs

   procedure Process_Imported_Non_Libraries (For_Project : Project_Id);
   --  Get the imported non library project ids in table Non_Library_Projs

   function Create_Path_From_Dirs return String_Access;
   --  Concatenate all directories in the Directories table into a path.
   --  Caller is responsible for freeing the result

   procedure Check_Archive_Builder;
   --  Check if the archive builder (ar) is there

   function Archive_Suffix (For_Project : Project_Id) return String;
   --  Return the archive suffix for the project, if defined, otherwise
   --  return ".a".

   procedure Change_To_Object_Directory (Project : Project_Id);
   --  Change to the object directory of project Project, if this is not
   --  already the current working directory.

end Gprbuild;
