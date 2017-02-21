------------------------------------------------------------------------------
--                                                                          --
--                             GPR TECHNOLOGY                               --
--                                                                          --
--                       Copyright (C) 2016, AdaCore                        --
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

--  This package provide services to build a build script in gprbuild.

with Ada.Text_IO; use Ada.Text_IO;

with GNAT.OS_Lib; use GNAT.OS_Lib;

with GPR; use GPR;

package Gpr_Script is

   Build_Script_Option : constant String := "--build-script=";
   --  gprbuild switch to create a build script

   Build_Script_Name : String_Access := null;
   --  Path name of the build script

   Build_Script_File : File_Type;
   --  Build script file

   procedure Script_Write
     (Program_Name : String;
      Args         : Argument_List);
   --  If a build script is being built, append a line to invoke the
   --  program with its arguments.

   procedure Script_Change_Dir (New_Dir : Path_Name_Type);
   --  If a build script is being built, append a line to change the current
   --  working directory to New_Dir.

   procedure Script_Copy
     (File_Name   : String;
      Destination : String_Access);
   --  If a build script is being built, append a line to copy file File_Name
   --  to directory Destination.

   procedure Spawn_And_Script_Write
     (Program_Name : String;
      Args         : Argument_List;
      Success      : out Boolean);
   --  If a build script is being built, append a line to invoke the program
   --  with its arguments, then spawn the process.
end Gpr_Script;
