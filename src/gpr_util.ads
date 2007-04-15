------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                            G P R _ U T I L                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2007, Free Software Foundation, Inc.            --
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

with GNAT.HTable;
with GNAT.OS_Lib; use GNAT.OS_Lib;

with Namet;   use Namet;
with Prj;     use Prj;
with Table;

package Gpr_Util is

   There_Are_Runtime_Projects : Boolean := False;
   --  True when there are runtime projects. Set by Get_Configuration when
   --  an attribute Language_Processing'Runtime_Project is declared.
   --  Reset to False if there are no sources of the languages with runtime
   --  projects.

   There_Are_Binder_Drivers   : Boolean := False;
   --  True when there is a binder driver. Set by Get_Configuration when
   --  an attribute Language_Processing'Binder_Driver is declared.
   --  Reset to False if there are no sources of the languages with binder
   --  drivers.

   type Binding_Data is record
      Language           : Language_Index;
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

   Config_Path : String_Access := null;

   Default_Config_Project_File_Name : constant String := "default.cgpr";

   Config_Project_Env_Var : constant String := "GPR_CONFIG";

   Config_Project_File_Name : String_Access := null;

   --  Default project

   Default_Project_File_Name : constant String := "default.gpr";

   type Name_Project is record
      Lang : Name_Id        := No_Name;
      Name : Path_Name_Type := No_Path;
      Proj : Project_Id     := No_Project;
   end record;
   --  Component of table Runtimes

   No_Name_Project : constant Name_Project :=
                       (Lang => No_Name, Name => No_Path, Proj => No_Project);

   package Runtimes is new GNAT.HTable.Simple_HTable
     (Header_Num => Prj.Header_Num,
      Element    => Name_Project,
      No_Element => No_Name_Project,
      Key        => Name_Id,
      Hash       => Prj.Hash,
      Equal      => "=");
   --  A hash table to store the path name and project id of the runtime
   --  projects.

   function Binder_Exchange_File_Name
     (Object_File : File_Name_Type; Prefix : Name_Id)
      return String_Access;
   --  Returns the name of the binder exchange file corresponding to an
   --  object file and a language.

   procedure Find_Binding_Languages;
   --  Check if in the project tree there are sources of languages that have a
   --  binder driver. Populate table Binding_Languages and set variable
   --  There_Are_Binder_Drivers accordingly.

   procedure Fail_Program (S1 : String; S2 : String := ""; S3 : String := "");
   --  Terminate program with a message and a fatal status code

   procedure Finish_Program
     (Fatal : Boolean;
      S1    : String := "";
      S2    : String := "";
      S3 : String := "");
   --  Terminate program, with or without a message, setting the status code
   --  according to Fatal.

end Gpr_Util;
