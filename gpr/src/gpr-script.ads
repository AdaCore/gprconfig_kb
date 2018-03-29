------------------------------------------------------------------------------
--                                                                          --
--                           GPR PROJECT MANAGER                            --
--                                                                          --
--          Copyright (C) 2016-2018, Free Software Foundation, Inc.         --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

--  This package provide services to build a build script in gprbuild.

with Ada.Text_IO; use Ada.Text_IO;

with GPR.Util;    use GPR.Util;

package GPR.Script is

   Build_Script_Option : constant String := "--build-script=";
   --  gprbuild switch to create a build script

   Build_Script_Name : String_Access := null;
   --  Path name of the build script

   Build_Script_File : File_Type;
   --  Build script file

   procedure Script_Write
     (Program_Name : String;
      Args         : String_Vectors.Vector);
   --  If a build script is being built, append a line to invoke the
   --  program with its arguments.

   procedure Script_Change_Dir (New_Dir : Path_Name_Type);
   --  If a build script is being built, append a line to change the current
   --  working directory to New_Dir.

   procedure Script_Copy
     (File_Name   : String;
      Destination : String);
   --  If a build script is being built, append a line to copy file File_Name
   --  to directory Destination.

   procedure Spawn_And_Script_Write
     (Program_Name : String;
      Args         : String_Vectors.Vector;
      Success      : out Boolean);
   --  If a build script is being built, append a line to invoke the program
   --  with its arguments, then spawn the process.

end GPR.Script;
