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

with GPR.Names; use GPR.Names;

package body Gpr_Script is

   -----------------------
   -- Script_Change_Dir --
   -----------------------

   procedure Script_Change_Dir (New_Dir : Path_Name_Type) is
   begin
      if Build_Script_Name /= null then
         declare
            Args : Argument_List :=
              (1 => new String'(Get_Name_String (New_Dir)));
         begin
            Script_Write ("cd", Args);
            Free (Args (1));
         end;
      end if;
   end Script_Change_Dir;

   -----------------
   -- Script_Copy --
   -----------------

   procedure Script_Copy
     (File_Name   : String;
      Destination : String_Access)
   is
   begin
      if Build_Script_Name /= null then
         declare
            Args : Argument_List :=
              (1 => new String'(File_Name),
               2 => Destination);

         begin
            Script_Write ("cp", Args);
            Free (Args (1));
         end;
      end if;
   end Script_Copy;

   ------------------
   -- Script_Write --
   ------------------

   procedure Script_Write
     (Program_Name : String;
      Args         : Argument_List)
   is
      Already_Open : constant Boolean :=  Is_Open (Build_Script_File);
   begin
      if Build_Script_Name /= null then
         if not Already_Open then
            Open (Build_Script_File, Append_File, Build_Script_Name.all);
         end if;

         Put (Build_Script_File, Program_Name);

         for J in Args'Range loop
            Put (Build_Script_File, " " & Args (J).all);
         end loop;

         New_Line (Build_Script_File);

         if not Already_Open then
            Close (Build_Script_File);
         end if;
      end if;
   end Script_Write;

   ----------------------------
   -- Spawn_And_Script_Write --
   ----------------------------

   procedure Spawn_And_Script_Write
     (Program_Name : String;
      Args         : Argument_List;
      Success      : out Boolean)
   is
   begin
      Script_Write (Program_Name, Args);
      Spawn (Program_Name, Args, Success);
   end Spawn_And_Script_Write;
end Gpr_Script;
