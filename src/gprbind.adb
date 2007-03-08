------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              G P R B I N D                               --
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

--  gprbind is the executable called by gprmake to bind Ada sources. It is
--  the driver for gnatbind. It gets its input from gprmake through the
--  binding exchange file and gives back its results through the same file.

with Ada.Text_IO; use Ada.Text_IO;

with Ada.Command_Line; use Ada.Command_Line;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;      use GNAT.OS_Lib;

with Gprexch; use Gprexch;
with Makeutl; use Makeutl;
with Namet;   use Namet;
with Osint;
with Table;

procedure Gprbind is

   Executable_Suffix : constant String_Access := Get_Executable_Suffix;
   --  The suffix of executables on this platforms

   GNATBIND : constant String := Osint.Program_Name ("gnatbind").all;

   Quiet_Output : Boolean := False;
   Verbose_Mode : Boolean := False;

   No_Main_Option : constant String := "-n";
   Dash_o         : constant String := "-o";
   Dash_shared    : constant String := "-shared";

   GCC_Version : Character := '0';
   Gcc_Version_String : constant String := "gcc version ";

   Begin_Info : constant String := "--  BEGIN Object file/option list";
   End_Info   : constant String := "--  END Object file/option list   ";

   Shared_Libgcc : constant String := "-shared-libgcc";

   IO_File : File_Type;
   --  The file to get the inputs and to put the results of the binding

   Line : String (1 .. 1_000);
   Last : Natural;

   Exchange_File_Name : String_Access;
   Ada_Compiler_Path  : String_Access;
   Gnatbind_Path      : String_Access;

   Compiler_Options     : String_List_Access := new String_List (1 .. 100);
   Last_Compiler_Option : Natural := 0;

   Gnatbind_Options     : String_List_Access := new String_List (1 .. 100);
   Last_Gnatbind_Option : Natural := 0;

   Main_ALI : String_Access := null;

   Binder_Generated_File : String_Access := null;
   BG_File               : File_Type;

   Success     : Boolean := False;
   Return_Code : Integer;

   Adalib_Dir  : String_Access;
   Static_Libs : Boolean := True;

   Current_Section : Binding_Section := No_Binding_Section;

   package Binding_Options_Table is new Table.Table
     (Table_Component_Type => String_Access,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 10,
      Table_Increment      => 100,
      Table_Name           => "Gprbind.Binding_Options_Table");

   package ALI_Files_Table is new Table.Table
     (Table_Component_Type => String_Access,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 10,
      Table_Increment      => 100,
      Table_Name           => "Gprbind.ALI_File_Table");

begin
   if Argument_Count /= 1 then
      Osint.Fail ("incorrect invocation");
   end if;

   Exchange_File_Name := new String'(Argument (1));

   --  DEBUG: save a copy of the exchange file

   if Getenv ("GPRBIND_DEBUG").all = "TRUE" then
      Copy_File
        (Exchange_File_Name.all,
         Exchange_File_Name.all & "__saved",
         Success);
   end if;

   --  Open the binding exchange file

   begin
      Open (IO_File, In_File, Exchange_File_Name.all);
   exception
      when others =>
         Osint.Fail ("could not read ", Exchange_File_Name.all);
   end;

   --  Get the information from the binding exchange file

   while not End_Of_File (IO_File) loop
      Get_Line (IO_File, Line, Last);

      if Last > 0 then
         if Line (1) = '[' then
            Current_Section := Get_Binding_Section (Line (1 .. Last));

            case Current_Section is
               when No_Binding_Section =>
                  Osint.Fail ("unknown section: ", Line (1 .. Last));

               when Quiet =>
                  Quiet_Output := True;
                  Verbose_Mode := False;

               when Verbose =>
                  Quiet_Output := False;
                  Verbose_Mode := True;

               when Shared_Libs =>
                  Static_Libs := False;

               when others =>
                  null;
            end case;

         else
            case Current_Section is
               when No_Binding_Section =>
                  Osint.Fail ("no section specified: ", Line (1 .. Last));

               when Quiet =>
                  Osint.Fail ("quiet section should be empty");

               when Verbose =>
                  Osint.Fail ("verbose section should be empty");

               when Shared_Libs =>
                  Osint.Fail ("shared libs section should be empty");

               when Generated_Source_File =>
                  if Binder_Generated_File /= null then
                     Osint.Fail
                       ("binder generated file specified multiple times");
                  end if;

                  Binder_Generated_File := new String'(Line (1 .. Last));

               when Compiler_Path =>
                  if Ada_Compiler_Path /= null then
                     Osint.Fail
                       ("compiler path specified multiple times");
                  end if;

                  Ada_Compiler_Path := new String'(Line (1 .. Last));

               when Gprexch.Compiler_Options =>
                  Add
                    (Line (1 .. Last),
                     Compiler_Options, Last_Compiler_Option);

               when Main_Dependency_File =>
                  if Main_ALI /= null then
                     Osint.Fail ("main ALI file specified multiple times");
                  end if;

                  Main_ALI := new String'(Line (1 .. Last));

               when Dependency_Files =>
                  ALI_Files_Table.Append (new String'(Line (1 .. Last)));

               when Binding_Options =>
                  Binding_Options_Table.Append (new String'(Line (1 .. Last)));

               when Generated_Object_File |
                    Resulting_Options |
                    Run_Path_Option =>
                  null;
            end case;
         end if;
      end if;
   end loop;

   Close (IO_File);

   if not Static_Libs then
      Add (Dash_shared, Gnatbind_Options, Last_Gnatbind_Option);
   end if;

   --  Specify the name of the generated file to gnatbind

   Add (Dash_o, Gnatbind_Options, Last_Gnatbind_Option);
   Add (Binder_Generated_File, Gnatbind_Options, Last_Gnatbind_Option);

   if not Is_Regular_File (Ada_Compiler_Path.all) then
      Osint.Fail ("could not find the Ada compiler");
   end if;

   if Main_ALI /= null then
      Add (Main_ALI.all, Gnatbind_Options, Last_Gnatbind_Option);
   end if;

   for J in 1 .. ALI_Files_Table.Last loop
      Add (ALI_Files_Table.Table (J), Gnatbind_Options, Last_Gnatbind_Option);
   end loop;

   for J in 1 .. Binding_Options_Table.Last loop
      Add
        (Binding_Options_Table.Table (J),
         Gnatbind_Options,
         Last_Gnatbind_Option);
   end loop;

   Gnatbind_Path := Locate_Exec_On_Path (GNATBIND);

   if Gnatbind_Path = null then
      Osint.Fail ("could not locate ", GNATBIND);
   end if;

   if Main_ALI = null then
      Add (No_Main_Option, Gnatbind_Options, Last_Gnatbind_Option);
   end if;

   if not Quiet_Output then
      if Verbose_Mode then
         Put (Gnatbind_Path.all);
      else
         Put (GNATBIND);
      end if;

      if Verbose_Mode then
         for Option in 1 .. Last_Gnatbind_Option loop
            Put (' ');
            Put (Gnatbind_Options (Option).all);
         end loop;

      else
         Put (' ');

         if Main_ALI /= null then
            Put (Base_Name (Main_ALI.all));

            if ALI_Files_Table.Last > 0 then
               Put (" ...");
            end if;

         elsif ALI_Files_Table.Last > 0 then
            Put (Base_Name (ALI_Files_Table.Table (1).all));

            if ALI_Files_Table.Last > 1 then
               Put (" ...");
            end if;

            Put (' ');
            Put (No_Main_Option);
         end if;
      end if;

      New_Line;
   end if;

   Spawn
     (Gnatbind_Path.all,
      Gnatbind_Options (1 .. Last_Gnatbind_Option),
      Success);

   if not Success then
      Osint.Fail ("invocation of gnatbind failed");
   end if;

   Add (Binder_Generated_File, Compiler_Options, Last_Compiler_Option);

   declare
      Object      : String := Binder_Generated_File.all;
      Object_Last : constant Positive := Object'Last - 2;

   begin
      Object (Object_Last) := 'o';

      Add
        (Dash_o,
         Compiler_Options,
         Last_Compiler_Option);
      Add
        (Object (Object'First .. Object_Last),
         Compiler_Options,
         Last_Compiler_Option);

      if not Quiet_Output then
         Name_Len := 0;

         if Verbose_Mode then
            Add_Str_To_Name_Buffer (Ada_Compiler_Path.all);
         else
            Add_Str_To_Name_Buffer (Base_Name (Ada_Compiler_Path.all));
         end if;

         --  Remove the executable suffix, if present

         if Executable_Suffix'Length > 0
           and then
             Name_Len > Executable_Suffix'Length
           and then
               Name_Buffer
                 (Name_Len - Executable_Suffix'Length + 1 .. Name_Len) =
               Executable_Suffix.all
         then
            Name_Len := Name_Len - Executable_Suffix'Length;
         end if;

         Put (Name_Buffer (1 .. Name_Len));

         if Verbose_Mode then
            for Option in 1 .. Last_Compiler_Option loop
               Put (' ');
               Put (Compiler_Options (Option).all);
            end loop;

         else
            Put (' ');
            Put (Compiler_Options (1).all);

            if Compiler_Options (1) /= Binder_Generated_File then
               Put (' ');
               Put (Binder_Generated_File.all);
            end if;
         end if;

         New_Line;
      end if;

      Spawn
        (Ada_Compiler_Path.all,
         Compiler_Options (1 .. Last_Compiler_Option),
         Success);

      if not Success then
         Osint.Fail ("compilation of binder generated file failed");
      end if;

      --  Find the GCC version

      Spawn
        (Program_Name => Ada_Compiler_Path.all,
         Args         => (1 => new String'("-v")),
         Output_File  => Exchange_File_Name.all,
         Success      => Success,
         Return_Code  => Return_Code,
         Err_To_Out   => True);

      if Success then
         Open (IO_File, In_File, Exchange_File_Name.all);
         while not End_Of_File (IO_File) loop
            Get_Line (IO_File, Line, Last);

            if Last > Gcc_Version_String'Length and then
              Line (1 .. Gcc_Version_String'Length) = Gcc_Version_String
            then
               GCC_Version := Line (Gcc_Version_String'Length + 1);
               exit;
            end if;
         end loop;

         Close (IO_File);
      end if;

      Open (BG_File, In_File, Binder_Generated_File.all);

      Create (IO_File, Out_File, Exchange_File_Name.all);

      Put_Line (IO_File, Generated_Object_File_Label);
      Put_Line (IO_File, Object (Object'First .. Object_Last));

      --  Get the options from the binder generated file

      while not End_Of_File (BG_File) loop
         Get_Line (BG_File, Line, Last);
         exit when Line (1 .. Last) = Begin_Info;
      end loop;

      if not End_Of_File (BG_File) then
         Put_Line (IO_File, Resulting_Options_Label);

         loop
            Get_Line (BG_File, Line, Last);
            exit when Line (1 .. Last) = End_Info;
            Line (1 .. Last - 8) := Line (9 .. Last);
            Last := Last - 8;

            if Line (1) = '-' then
               if Last >= 3 and then Line (2) = 'L' then
                  Adalib_Dir := new String'(Line (3 .. Last));
                  Put_Line (IO_File, Line (1 .. Last));

               elsif Line (1 .. Last) = "-static" then
                  Static_Libs := True;
                  Put_Line (IO_File, Line (1 .. Last));

               elsif Line (1 .. Last) = "-shared" then
                  Static_Libs := False;
                  Put_Line (IO_File, Line (1 .. Last));

                  if GCC_Version >= '3' then
                     Put_Line (IO_File, Shared_Libgcc);
                  end if;

               elsif Line (1 .. Last) = "-lgnat" then
                  if Static_Libs then
                     Put_Line (IO_File,
                               Adalib_Dir.all & "libgnat.a");

                  else
                     Put_Line (IO_File, Line (1 .. Last));
                  end if;

               elsif Line (1 .. Last) = "-lgnarl" then
                  if Static_Libs then
                     Put_Line (IO_File,
                               Adalib_Dir.all & "libgnarl.a");

                  else
                     Put_Line (IO_File, Line (1 .. Last));
                  end if;

               else
                  Put_Line (IO_File, Line (1 .. Last));
               end if;
            end if;
         end loop;
      end if;

      if not Static_Libs then
         Put_Line (IO_File, Run_Path_Option_Label);
         Put_Line (IO_File, Adalib_Dir.all);
         Name_Len := Adalib_Dir'Length;
         Name_Buffer (1 .. Name_Len) := Adalib_Dir.all;

         for J in reverse 2 .. Name_Len - 4 loop
            if Name_Buffer (J) = Directory_Separator and then
              Name_Buffer (J + 4) = Directory_Separator and then
              Name_Buffer (J + 1 .. J + 3) = "lib"
            then
               Name_Len := J + 3;
               Put_Line (IO_File, Name_Buffer (1 .. Name_Len));
               exit;
            end if;
         end loop;
      end if;

      Close (IO_File);
   end;
end Gprbind;
