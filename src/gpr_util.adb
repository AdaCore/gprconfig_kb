------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                            G P R _ U T I L                               --
--                                                                          --
--                                 B o d y                                  --
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

with Debug;   use Debug;
with Errout;  use Errout;
with Makeutl; use Makeutl;
with Osint;   use Osint;
with Output;  use Output;

package body Gpr_Util is

   Object_Suffix : constant String := Get_Object_Suffix.all;
   --  The suffix of object file for this platform

   -------------------------------
   -- Binder_Exchange_File_Name --
   -------------------------------

   function Binder_Exchange_File_Name
     (Object_File : File_Name_Type; Prefix : Name_Id)
      return String_Access
   is
      File_Name : constant String := Get_Name_String (Object_File);
   begin
      Get_Name_String (Prefix);
      Add_Str_To_Name_Buffer (File_Name);
      Name_Len := Name_Len - Object_Suffix'Length;
      Name_Buffer (Name_Len + 1 .. Name_Len + Binder_Exchange_Suffix'Length) :=
        Binder_Exchange_Suffix;
      Name_Len := Name_Len + Binder_Exchange_Suffix'Length;
      return new String'(Name_Buffer (1 .. Name_Len));
   end Binder_Exchange_File_Name;

   ------------------
   -- Fail_Program --
   ------------------

   procedure Fail_Program (S1 : String; S2 : String := ""; S3 : String := "")
   is
   begin
      Errout.Finalize;
      Errout.Output_Messages;
      Finish_Program (Fatal => True, S1 => S1, S2 => S2, S3 => S3);
   end Fail_Program;

   ----------------------------
   -- Find_Binding_Languages --
   ----------------------------

   procedure Find_Binding_Languages is
      B_Data : Binding_Data;

      Language           : Language_Index;
      Language_Name      : Name_Id;
      Binder_Driver_Name : File_Name_Type := No_File;
      Binder_Driver_Path : String_Access;
      Binder_Prefix      : Name_Id;

      Config : Language_Config;

      Compiler_Path : String_Access;

      Found : Boolean;

   begin
      There_Are_Binder_Drivers := False;

      for Index in
        1 .. Language_Data_Table.Last (Project_Tree.Languages_Data)
      loop
         Config := Project_Tree.Languages_Data.Table (Index).Config;

         Binder_Driver_Name := Config.Binder_Driver;

         if Project_Tree.Languages_Data.Table (Index).First_Source /= No_Source
           and then Binder_Driver_Name /= No_File
         then
            There_Are_Binder_Drivers := True;
            Language := Index;
            Language_Name := Project_Tree.Languages_Data.Table (Index).Name;

            Found := False;
            for B_Index in 1 .. Binding_Languages.Last loop
               if Binding_Languages.Table (B_Index).Language_Name =
                 Language_Name
               then
                  Found := True;
                  exit;
               end if;
            end loop;

            if not Found then
               Get_Name_String (Binder_Driver_Name);
               Binder_Driver_Path :=
                 Locate_Exec_On_Path (Name_Buffer (1 .. Name_Len));

               if Binder_Driver_Path = null then
                  Fail_Program
                    ("unable to find binder driver ",
                     Name_Buffer (1 .. Name_Len));
               end if;

               Compiler_Path := Config.Compiler_Driver_Path;

               --  If this is the first time we try this compiler, then get its
               --  path name.

               if Compiler_Path = null then
                  declare
                     Compiler_Name : constant String :=
                                       Get_Name_String
                                         (Project_Tree.Languages_Data.Table
                                            (Index).Config.Compiler_Driver);

                  begin
                     Compiler_Path := Locate_Exec_On_Path (Compiler_Name);

                     if Compiler_Path = null then
                        Fail_Program
                          ("unable to locate """, Compiler_Name, """");

                     else
                        Project_Tree.Languages_Data.Table
                          (Index).Config.Compiler_Driver_Path :=
                          Compiler_Path;
                     end if;
                  end;
               end if;

               if Config.Binder_Prefix = No_Name then
                  Binder_Prefix := Empty_String;

               else
                  Binder_Prefix := Config.Binder_Prefix;
               end if;

               for B_Index in 1 .. Binding_Languages.Last loop
                  if Binder_Prefix =
                    Binding_Languages.Table (B_Index).Binder_Prefix
                  then
                     Fail_Program
                       ("binding prefix cannot be the same for two languages");
                  end if;
               end loop;

               B_Data :=
                 (Language           => Language,
                  Language_Name      => Language_Name,
                  Binder_Driver_Name => Binder_Driver_Name,
                  Binder_Driver_Path => Binder_Driver_Path,
                  Binder_Prefix      => Binder_Prefix);
               Binding_Languages.Append (B_Data);
            end if;
         end if;
      end loop;
   end Find_Binding_Languages;

   --------------------
   -- Finish_Program --
   --------------------

   procedure Finish_Program
     (Fatal : Boolean;
      S1    : String := "";
      S2    : String := "";
      S3    : String := "")
   is
   begin
      if not Debug_Flag_N then
         Delete_All_Temp_Files;
      end if;

      if S1'Length > 0 then
         if Fatal then
            Osint.Fail (S1, S2, S3);

         else
            Write_Str (S1);
            Write_Str (S2);
            Write_Line (S3);
         end if;
      end if;

      if Fatal then
         Exit_Program (E_Fatal);

      else
         Exit_Program (E_Success);
      end if;
   end Finish_Program;

end Gpr_Util;
