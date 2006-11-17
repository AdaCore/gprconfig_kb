------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--            G P R L I B . B U I L D _ S H A R E D _ L I B                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2006, Free Software Foundation, Inc.            --
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

--  This is the version of the body of procedure Build_Shared_Lib for most
--  non VMS platforms where shared libraries are supported.

with Ada.Directories; use Ada.Directories;

with Output; use Output;
with System;

separate (Gprlib)
procedure Build_Shared_Lib is

   Gcc_Name : constant String := "gcc";

   Ofiles : constant Argument_List :=
              Argument_List (Object_Files.Table (1 .. Object_Files.Last));

   Options : constant Argument_List :=
               Argument_List (Options_Table.Table (1 .. Options_Table.Last));

   Lib_File : constant String :=
            Shared_Lib_Prefix.all & Library_Name.all & Shared_Lib_Suffix.all;

   Lib_Path : constant String :=
                Library_Directory.all & Directory_Separator & Lib_File;

   Symbolic_Link_Needed : Boolean := False;

   Last_Maj : Positive;
   Last     : Positive;
   Ok_Maj   : Boolean := False;

   Result  : Integer;
   pragma Unreferenced (Result);

   function Symlink
     (Oldpath : System.Address;
      Newpath : System.Address) return Integer;
   pragma Import (C, Symlink, "__gnat_symlink");

   procedure Build (Output_File : String);

   function Lib_Version return String;

   procedure Remove_File (Filename : String);

   -----------
   -- Build --
   -----------

   --  Not one comment below in this body ???

   procedure Build (Output_File : String) is
      Arguments :
         Argument_List
          (1 .. 10 + Ofiles'Length + Options'Length);

      A        : Natural := 0;
      Success  : Boolean;

      Out_Opt : constant String_Access :=
                   new String'("-o");
      Out_V   : constant String_Access :=
                   new String'(Output_File);

      Driver : String_Access;

   begin
      if Driver_Name = No_Name then
         Driver := Locate_Exec_On_Path (Gcc_Name);

         if Driver = null then
            Osint.Fail (Gcc_Name, " not found in path");
         end if;

      else
         Driver := Locate_Exec_On_Path (Get_Name_String (Driver_Name));

         if Driver = null then
            Osint.Fail (Get_Name_String (Driver_Name), " not found in path");
         end if;
      end if;

      for J in 1 .. Shared_Lib_Minimum_Options.Last loop
         A := A + 1;
         Arguments (A) := Shared_Lib_Minimum_Options.Table (J);
      end loop;

      A := A + 1;
      Arguments (A) := Out_Opt;

      A := A + 1;
      Arguments (A) := Out_V;

      for J in Options'Range loop
         if Options (J) /= null and then Options (J).all /= "" then
            A := A + 1;
            Arguments (A) := Options (J);
         end if;
      end loop;

      for J in 1 .. Library_Version_Options.Last loop
         if Library_Version_Options.Table (J).all /= "" then
            A := A + 1;
            Arguments (A) := Library_Version_Options.Table (J);
         end if;
      end loop;

      for J in 1 .. Library_Options_Table.Last loop
         A := A + 1;
         Arguments (A) := Library_Options_Table.Table (J);
      end loop;

      for J in Ofiles'Range loop
         A := A + 1;
         Arguments (A) := Ofiles (J);
      end loop;

      if not Opt.Quiet_Output then
         Write_Str (Driver.all);

         for J in 1 .. A loop
            Write_Char (' ');
            Write_Str  (Arguments (J).all);
         end loop;

         Write_Eol;
      end if;

      Spawn (Driver.all, Arguments (1 .. A), Success);

      if not Success then
         if Driver_Name = No_Name then
            Osint.Fail (Gcc_Name, " execution error");

         else
            Osint.Fail (Get_Name_String (Driver_Name), " execution error");
         end if;
      end if;
   end Build;

   -----------------
   -- Lib_Version --
   -----------------

   function Lib_Version return String is
   begin
      if Ok_Maj then
         return Library_Version (1 .. Last_Maj);
      else
         return Library_Version.all;
      end if;
   end Lib_Version;

   -----------------
   -- Remove_File --
   -----------------

   procedure Remove_File (Filename : String) is
      File    : constant String := Filename & ASCII.Nul;
      Success : Boolean;

   begin
      Delete_File (File'Address, Success);

      if Opt.Verbose_Mode then
         if Success then
            Write_Str ("deleted ");

         else
            Write_Str ("could not delete ");
         end if;

         Write_Line (Filename);
      end if;
   end Remove_File;

--  Start of processing for Build_Shared_Lib

--  This body needs comments ???

begin
   if Opt.Verbose_Mode then
      Write_Str ("building relocatable shared library ");
      Write_Line (Lib_File);
   end if;

   if Library_Version.all = "" then
      Library_Version_Options.Set_Last (0);
      Build (Lib_Path);

   else
      if Symbolic_Link_Supported then
         Last_Maj := Library_Version'Last;

         if Major_Minor_Id_Supported then
            while Last_Maj > 1 loop
               if Library_Version (Last_Maj) in '0' .. '9' then
                  Last_Maj := Last_Maj - 1;

               else
                  Ok_Maj := Last_Maj /= Library_Version'Last and then
                            Library_Version (Last_Maj) = '.';

                  if Ok_Maj then
                     Last_Maj := Last_Maj - 1;
                  end if;

                  exit;
               end if;
            end loop;

            if Ok_Maj then
               Last := Last_Maj;
               while Last > 1 loop
                  if Library_Version (Last) in '0' .. '9' then
                     Last := Last - 1;

                  else
                     Ok_Maj := Last /= Last_Maj and then
                               Library_Version (Last) = '.';

                     if Ok_Maj then
                        Last := Last - 1;
                        Ok_Maj :=
                          Simple_Name (Library_Version (1 .. Last)) = Lib_File;
                     end if;

                     exit;
                  end if;
               end loop;
            end if;
         end if;
      end if;

      if Library_Version_Options.Last > 0 then
         Library_Version_Options.Table (Library_Version_Options.Last)  :=
           new String'
             (Library_Version_Options.Table
                  (Library_Version_Options.Last).all &
              Lib_Version);
      end if;

      if not Is_Absolute_Path (Library_Version.all) then
         Library_Version :=
           new String'
             (Library_Directory.all &
              Directory_Separator &
              Library_Version.all);

         if Ok_Maj then
            Last_Maj := Last_Maj + Library_Directory'Length + 1;
         end if;
      end if;

      Build (Library_Version.all);
      Symbolic_Link_Needed :=
        Symbolic_Link_Supported and then Library_Version.all /= Lib_File;

      if Symbolic_Link_Needed then
         declare
            Oldpath : String (1 .. Library_Version.all'Length + 1);
            Newpath : String (1 .. Lib_Path'Length + 1);

         begin
            Oldpath (1 .. Library_Version'Length) :=
              Library_Version.all;
            Oldpath (Oldpath'Last)            := ASCII.NUL;
            Newpath (1 .. Lib_Path'Length)    := Lib_Path;
            Newpath (Newpath'Last)            := ASCII.NUL;

            Remove_File (Lib_Path);

            Result := Symlink (Oldpath'Address, Newpath'Address);
         end;
      end if;

      if Symbolic_Link_Supported and then Ok_Maj then
         declare
            Oldpath : String (1 .. Library_Version.all'Length + 1);
            Newpath : String (1 .. Last_Maj + 1);

         begin
            Oldpath (1 .. Library_Version'Length) :=
              Library_Version.all;
            Oldpath (Oldpath'Last)  := ASCII.NUL;
            Newpath (1 .. Last_Maj) := Library_Version (1 .. Last_Maj);
            Newpath (Newpath'Last)  := ASCII.NUL;

            Remove_File (Library_Version (1 .. Last_Maj));

            Result := Symlink (Oldpath'Address, Newpath'Address);
         end;
      end if;

   end if;
end Build_Shared_Lib;
