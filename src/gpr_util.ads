------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                            G P R _ U T I L                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2007-2010, Free Software Foundation, Inc.       --
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

--  This package contains constants, variable and subprograms used by gprbuild
--  and gprclean.

with GNAT.OS_Lib; use GNAT.OS_Lib;

with Namet;    use Namet;
with Prj;      use Prj;
with Prj.Tree; use Prj.Tree;
with Table;

package Gpr_Util is

   Partial_Prefix : constant String := "p__";

   Begin_Info : constant String := "--  BEGIN Object file/option list";
   End_Info   : constant String := "--  END Object file/option list   ";

   Project_Node_Tree : constant Project_Node_Tree_Ref :=
     new Project_Node_Tree_Data;
   --  This is also used to hold project path and scenario variables

   Success : Boolean := False;

   There_Are_Binder_Drivers   : Boolean := False;
   --  True when there is a binder driver. Set by Get_Configuration when
   --  an attribute Language_Processing'Binder_Driver is declared.
   --  Reset to False if there are no sources of the languages with binder
   --  drivers.

   type Binding_Data is record
      Language           : Language_Ptr;
      Language_Name      : Name_Id;
      Binder_Driver_Name : File_Name_Type;
      Binder_Driver_Path : String_Access;
      Binder_Prefix      : Name_Id;
   end record;
   --  Data for a language that have a binder driver

   package Binding_Languages is new Table.Table
     (Table_Component_Type => Binding_Data,
      Table_Index_Type     => Integer,
      Table_Low_Bound      => 1,
      Table_Initial        => 4,
      Table_Increment      => 100,
      Table_Name           => "Gpr_Util.Binding_Languages");
   --  Table for binding data for languages that have a binder driver

   --  Config project

   Config_Project_Option : constant String := "--config=";

   Autoconf_Project_Option : constant String := "--autoconf=";

   Target_Project_Option : constant String := "--target=";

   No_Name_Map_File_Option : constant String := "--map-file-option";

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

   --  User projects

   Project_File_Name          : String_Access := null;
   --  The name of the project file specified with switch -P

   Main_Project : Project_Id;
   --  The project id of the main project

   RTS_Option : constant String := "--RTS=";

   RTS_Language_Option : constant String := "--RTS:";

--   Base : Knowledge_Base;
   --  The knowledge base used to find the standard name of the native target

   Db_Directory_Expected : Boolean := False;
   --  True when last switch was --db

   Load_Standard_Base : Boolean := True;
   --  False when switch --db- is used

   Unchecked_Shared_Lib_Imports : constant String :=
                                    "--unchecked-shared-lib-imports";
   --  Command line switch of gprbuild and gprclean to allow shared library
   --  projects to import projects that are not shared library projects.

   --  Local subprograms

   function Binder_Exchange_File_Name
     (Main_Base_Name : File_Name_Type; Prefix : Name_Id)
      return String_Access;
   --  Returns the name of the binder exchange file corresponding to an
   --  object file and a language.
   --  Main_Base_Name must have no extension specified

   procedure Create_Response_File
     (Format          : Response_File_Format;
      Objects         : String_List;
      Other_Arguments : String_List;
      Name_1          : out Path_Name_Type;
      Name_2          : out Path_Name_Type);
   --  Create a temporary file as a response file that contains either the list
   --  of Objects in the correct Format, or for Format GCC the list of all
   --  arguments. It is the responsibility of the caller to delete this
   --  temporary file if needed.

   procedure Find_Binding_Languages;
   --  Check if in the project tree there are sources of languages that have a
   --  binder driver. Populate table Binding_Languages and set variable
   --  There_Are_Binder_Drivers accordingly.

   function Get_Compiler_Driver_Path
     (Lang : Language_Ptr) return String_Access;
   --  Get, from the config, the path of the compiler driver. This is first
   --  looked for on the PATH if needed.
   --  Returns "null" if no compiler driver was specified for the language, and
   --  exit with an error if one was specified but not found.

   procedure Fail_Program (S : String; Flush_Messages : Boolean := True);
   --  Terminate program with a message and a fatal status code

   procedure Finish_Program (Fatal : Boolean; S : String := "");
   --  Terminate program, with or without a message, setting the status code
   --  according to Fatal.

   procedure Get_Mains;
   --  Get the mains. If no main is defined on the command line, take those
   --  in attribute Main of the main project. If there are mains, check that
   --  they are not truncated (no body suffix) and if they are put the complete
   --  file names in the table. Fail if a main specified in attribute Main is
   --  not a source of the main project.

   procedure Initialize_Source_Record (Source : Source_Id);
   --  Get information either about the source file, the object and
   --  dependency file. This includes timestamps.

   function Is_Subunit (Source : Source_Id) return Boolean;
   --  Return True if source is a subunit

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

      procedure Parse_Knowledge_Base (Directory : String := "");

   end Knowledge;

end Gpr_Util;
