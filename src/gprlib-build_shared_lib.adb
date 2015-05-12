------------------------------------------------------------------------------
--                                                                          --
--                             GPR TECHNOLOGY                               --
--                                                                          --
--                     Copyright (C) 2006-2015, AdaCore                     --
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

--  This is the version of the body of procedure Build_Shared_Lib for most
--  where shared libraries are supported.

separate (Gprlib)
procedure Build_Shared_Lib is

   Ofiles : constant Argument_List :=
              Argument_List (Object_Files.Table (1 .. Object_Files.Last));

   Options : constant Argument_List :=
               Argument_List (Options_Table.Table (1 .. Options_Table.Last));

   Lib_File : constant String :=
                Shared_Lib_Prefix.all &
                Library_Name.all & Shared_Lib_Suffix.all;

   Lib_Path : constant String :=
                Library_Directory.all & Lib_File;

   Maj_Version : String_Access := new String'("");

   Result  : Integer;
   pragma Unreferenced (Result);

   procedure Build (Output_File : String);
   --  Find the library builder executable and invoke it with the correct
   --  options to build the shared library.

   -----------
   -- Build --
   -----------

   procedure Build (Output_File : String) is
      Success  : Boolean;

      Out_Opt : constant String_Access := new String'("-o");
      Out_V   : constant String_Access := new String'(Output_File);

      Driver   : String_Access;

      Lib_Index : Natural := 0;

      Response_File_Name : Path_Name_Type := No_Path;
      Response_2         : Path_Name_Type := No_Path;

      procedure Display_Linking_Command;
      --  Display the linking command, depending on verbosity and quiet output

      -----------------------------
      -- Display_Linking_Command --
      -----------------------------

      procedure Display_Linking_Command is
      begin
         if not Opt.Quiet_Output then

            if Opt.Verbose_Mode then
               Put (Driver.all);

            else
               Put (Base_Name (Driver.all));
            end if;

            for J in 1 .. Last_Arg loop
               if Opt.Verbose_Mode or else
                 J <= Lib_Index or else
                 J = First_Object
               then
                  Put (' ');
                  Put  (Arguments (J).all);

               elsif J > First_Object then
                  Put (" ...");
                  exit;

               elsif J = Lib_Index + 1 then
                  Put (" ...");
               end if;
            end loop;

            New_Line;
         end if;
      end Display_Linking_Command;

   begin
      --  Get the executable to use, either the specified Driver, or "gcc"

      if Driver_Name = No_Name then
         Driver := Locate_Exec_On_Path (Gcc_Name);

         if Driver = null then
            Fail_Program (null, Gcc_Name & " not found in path");
         end if;

      else
         Driver := Locate_Exec_On_Path (Get_Name_String (Driver_Name));

         if Driver = null then
            Fail_Program
              (null, Get_Name_String (Driver_Name) & " not found in path");
         end if;
      end if;

      Last_Arg := 0;
      Argument_Length := Driver'Length;

      --  The minimum arguments

      for J in 1 .. Shared_Lib_Minimum_Options.Last loop
         Add_Arg (Shared_Lib_Minimum_Options.Table (J));
      end loop;

      --  The leading library options, if any

      for J in 1 .. Leading_Library_Options_Table.Last loop
         Add_Arg (Leading_Library_Options_Table.Table (J));
      end loop;

      --  -o <library file name>

      Add_Arg (Out_Opt);
      Add_Arg (Out_V);
      Lib_Index := Last_Arg;

      --  The options

      for J in Options'Range loop
         if Options (J) /= null and then Options (J).all /= "" then
            Add_Arg (Options (J));
         end if;
      end loop;

      --  Other options

      for J in 1 .. Library_Version_Options.Last loop
         if Library_Version_Options.Table (J).all /= "" then
            Add_Arg (Library_Version_Options.Table (J));
         end if;
      end loop;

      --  The object files

      if Partial_Linker /= null then
         Partial_Linker_Path := Locate_Exec_On_Path (Partial_Linker.all);

         if Partial_Linker_Path = null then
            Fail_Program
              (null, "unable to locate linker " & Partial_Linker.all);
         end if;
      end if;

      if Resp_File_Format = GPR.None
        and then Partial_Linker_Path /= null
      then
         --  If partial linker is used, do a partial link first

         Partial_Number := 0;
         First_Object := Ofiles'First;

         loop
            declare
               Partial : constant String_Access :=
                           new String'
                             (Partial_Name
                                (Library_Name.all,
                                 Partial_Number,
                                 Object_Suffix));
               Size    : Natural := 0;

               Saved_Last_PL_Option : Natural;
            begin
               Saved_Last_PL_Option := Last_PL_Option;

               Add (Partial, PL_Options, Last_PL_Option);
               Size := Size + 1 + Partial'Length;

               if Partial_Number > 0 then
                  Add
                    (Partial_Name
                       (Library_Name.all,
                        Partial_Number - 1,
                        Object_Suffix),
                     PL_Options,
                     Last_PL_Option);
               end if;

               for J in 1 .. Last_PL_Option loop
                  Size := Size + 1 + PL_Options (J)'Length;
               end loop;

               loop
                  Add
                    (Ofiles (First_Object),
                     PL_Options,
                     Last_PL_Option);
                  Size := Size + 1 + PL_Options (Last_PL_Option)'Length;

                  First_Object := First_Object + 1;
                  exit when
                    First_Object > Ofiles'Last or else
                    Size >= Maximum_Size;
               end loop;

               if not Quiet_Output then
                  if Verbose_Mode then
                     Put (Partial_Linker_Path.all);
                  else
                     Put (Base_Name (Partial_Linker_Path.all));
                  end if;

                  for J in 1 .. Last_PL_Option loop
                     if (not Verbose_Mode) and then J >= 5 then
                        Put (" ...");
                        exit;
                     end if;

                     Put (' ');
                     Put (PL_Options (J).all);
                  end loop;

                  New_Line;
               end if;

               Spawn
                 (Partial_Linker_Path.all,
                  PL_Options (1 .. Last_PL_Option),
                  Success);

               if not Success then
                  Fail_Program
                    (null,
                     "call to linker driver " &
                     Partial_Linker.all & " failed");
               end if;

               if First_Object > Ofiles'Last then
                  Add_Arg (Partial);
                  exit;
               end if;

               Last_PL_Option := Saved_Last_PL_Option;
               Partial_Number := Partial_Number + 1;
            end;
         end loop;

      else
         First_Object := Last_Arg + 1;

         for J in Ofiles'Range loop
            Add_Arg (Ofiles (J));
         end loop;
      end if;

      Last_Object := Last_Arg;

      --  In Ofiles we can have at the end some libraries -lname, so ensure
      --  that the object are only taken up to Last_Object_File_Index.

      if Last_Object_File_Index > First_Object
        and then Last_Object_File_Index < Last_Object
      then
         Last_Object := Last_Object_File_Index;
      end if;

      --  Finally the library switches and the library options

      for J in 1 .. Library_Switches_Table.Last loop
         Add_Arg (Library_Switches_Table.Table (J));
      end loop;

      for J in 1 .. Library_Options_Table.Last loop
         Add_Arg (Library_Options_Table.Table (J));
      end loop;

      Display_Linking_Command;

      --  Check if a response file is needed

      if Max_Command_Line_Length > 0
        and then Argument_Length > Max_Command_Line_Length
        and then Resp_File_Format /= GPR.None
      then
         declare
            --  Preserve the options, if any

            Options : constant String_List :=
                        Arguments (Last_Object + 1 .. Last_Arg);

         begin
            Create_Response_File
              (Format            => Resp_File_Format,
               Objects           => Arguments (First_Object .. Last_Object),
               Other_Arguments   => Options,
               Resp_File_Options => Response_File_Switches.all,
               Name_1            => Response_File_Name,
               Name_2            => Response_2);

            Last_Arg := First_Object - 1;

            if Resp_File_Format = GCC
              or else Resp_File_Format = GCC_GNU
              or else Resp_File_Format = GCC_Object_List
              or else Resp_File_Format = GCC_Option_List
            then
               Add_Arg
                 (new String'("@" & Get_Name_String (Response_File_Name)));
            else
               if Response_File_Switches'Length /= 0 then
                  for J in Response_File_Switches'First ..
                    Response_File_Switches'Last - 1
                  loop
                     Add_Arg (Response_File_Switches (J));
                  end loop;

                  Add_Arg
                    (new String'
                       (Response_File_Switches
                          (Response_File_Switches'Last).all &
                        Get_Name_String (Response_File_Name)));

               else
                  Add_Arg
                    (new String'(Get_Name_String (Response_File_Name)));
               end if;

               --  Put back the options

               for J in Options'Range loop
                  Add_Arg (Options (J));
               end loop;
            end if;
         end;

         Display_Linking_Command;
      end if;

      --  Finally spawn the library builder driver

      Spawn (Driver.all, Arguments (1 .. Last_Arg), Success);

      --  Delete response file, if any, except when asked not to

      if Response_File_Name /= No_Path and then Delete_Response_File then
         declare
            Dont_Care : Boolean;
            pragma Warnings (Off, Dont_Care);
         begin
            Delete_File (Get_Name_String (Response_File_Name), Dont_Care);

            if Response_2 /= No_Path then
               Delete_File (Get_Name_String (Response_2), Dont_Care);
            end if;
         end;
      end if;

      if not Success then
         if Driver_Name = No_Name then
            Fail_Program (null, Gcc_Name & " execution error");

         else
            Fail_Program
              (null, Get_Name_String (Driver_Name) & " execution error");
         end if;
      end if;
   end Build;

--  Start of processing for Build_Shared_Lib

begin
   if Opt.Verbose_Mode then
      Put ("building relocatable shared library ");
      Put_Line (Lib_File);
   end if;

   if Library_Version.all = "" or else not Symbolic_Link_Supported then
      --  If no Library_Version specified, make sure the table is empty and
      --  call Build.

      Library_Version_Options.Set_Last (0);
      Build (Lib_Path);

   else
      --  Put the necessary options corresponding to the Library_Version in the
      --  table.

      if Major_Minor_Id_Supported then
         Maj_Version :=
           new String'(Major_Id_Name (Lib_File, Library_Version.all));
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

      if Is_Absolute_Path (Library_Version.all) then
         Library_Version_Path := Library_Version;

      else
         Library_Version_Path :=
           new String'
             (Library_Directory.all & Library_Version.all);
      end if;

      --  Now that the table has been filled, call Build

      Build (Library_Version_Path.all);

      --  Create symbolic link, if appropriate

      if Library_Version.all /= Lib_Path then
         Create_Sym_Links
           (Lib_Path,
            Library_Version.all,
            Library_Directory.all,
            Maj_Version.all);
      end if;

   end if;
end Build_Shared_Lib;
