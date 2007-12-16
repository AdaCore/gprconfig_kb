------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--            G P R L I B . B U I L D _ S H A R E D _ L I B                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2006-2007, Free Software Foundation, Inc.       --
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

with MLib;   use MLib;
with Output; use Output;

separate (Gprlib)
procedure Build_Shared_Lib is

   Ofiles : constant Argument_List :=
              Argument_List (Object_Files.Table (1 .. Object_Files.Last));

   Options : constant Argument_List :=
               Argument_List (Options_Table.Table (1 .. Options_Table.Last));

   Lib_File : constant String :=
            Shared_Lib_Prefix.all & Library_Name.all & Shared_Lib_Suffix.all;

   Lib_Path : constant String :=
                Library_Directory.all & Directory_Separator & Lib_File;

   Symbolic_Link_Needed : Boolean := False;

   Maj_Version : String_Access := new String'("");

   Result  : Integer;
   pragma Unreferenced (Result);

   procedure Build (Output_File : String);

   -----------
   -- Build --
   -----------

   --  Not one comment below in this body ???

   procedure Build (Output_File : String) is
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

      Last_Arg := 0;

      for J in 1 .. Shared_Lib_Minimum_Options.Last loop
         Add_Arg (Shared_Lib_Minimum_Options.Table (J));
      end loop;

      Add_Arg (Out_Opt);

      Add_Arg (Out_V);

      for J in Options'Range loop
         if Options (J) /= null and then Options (J).all /= "" then
            Add_Arg (Options (J));
         end if;
      end loop;

      for J in 1 .. Library_Version_Options.Last loop
         if Library_Version_Options.Table (J).all /= "" then
            Add_Arg (Library_Version_Options.Table (J));
         end if;
      end loop;

      for J in 1 .. Library_Options_Table.Last loop
         Add_Arg (Library_Options_Table.Table (J));
      end loop;

      for J in Ofiles'Range loop
         Add_Arg (Ofiles (J));
      end loop;

      if not Opt.Quiet_Output then
         Write_Str (Driver.all);

         for J in 1 .. Last_Arg loop
            Write_Char (' ');
            Write_Str  (Arguments (J).all);
         end loop;

         Write_Eol;
      end if;

      Spawn (Driver.all, Arguments (1 .. Last_Arg), Success);

      if not Success then
         if Driver_Name = No_Name then
            Osint.Fail (Gcc_Name, " execution error");

         else
            Osint.Fail (Get_Name_String (Driver_Name), " execution error");
         end if;
      end if;
   end Build;

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
         if Major_Minor_Id_Supported then
            Maj_Version :=
              new String'(Major_Id_Name (Lib_File, Library_Version.all));
         end if;
      end if;

      if Library_Version_Options.Last > 0 then
         if Maj_Version.all /= "" then
            Library_Version_Options.Table (Library_Version_Options.Last)  :=
              new String'
                (Library_Version_Options.Table
                     (Library_Version_Options.Last).all & Maj_Version.all);

         else
            Library_Version_Options.Table (Library_Version_Options.Last)  :=
              new String'
                (Library_Version_Options.Table
                     (Library_Version_Options.Last).all &
                 Library_Version.all);
         end if;
      end if;

      if not Is_Absolute_Path (Library_Version.all) then
         Library_Version :=
           new String'
             (Library_Directory.all &
              Directory_Separator &
              Library_Version.all);
      end if;

      Build (Library_Version.all);
      Symbolic_Link_Needed :=
        Symbolic_Link_Supported and then Library_Version.all /= Lib_Path;

      if Symbolic_Link_Needed then
         Create_Sym_Links
           (Lib_Path,
            Library_Version.all,
            Library_Directory.all,
            Maj_Version.all);
      end if;

   end if;
end Build_Shared_Lib;
