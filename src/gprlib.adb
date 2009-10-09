------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               G P R L I B                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2006-2009, Free Software Foundation, Inc.       --
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

--  gprlib is called by gprmake to build the library for a library project
--  file. gprlib gets it parameters from a text file and give back results
--  through the same text file.

with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Command_Line;  use Ada.Command_Line;

with ALI;
with Csets;

with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;

with Gprexch;          use Gprexch;
with Gpr_Util;         use Gpr_Util;
with Hostparm;
with Makeutl;          use Makeutl;
with Namet;            use Namet;
with Opt;              use Opt;
with Osint;
with Prj;              use Prj;
with Snames;
with Switch;           use Switch;
with System.Case_Util; use System.Case_Util;
with Table;
with Tempdir;
with Types;            use Types;

procedure Gprlib is

   Shared_Libgnat_Separator : Character := '-';
   --  Character between "-lgnat" or "-lgnarl" and the toolchain version.
   --  It is not a constant because it is changed to '_' on VMS.

   Maximum_Size : Integer;
   pragma Import (C, Maximum_Size, "__gnat_link_max");
   --  Maximum number of bytes to put in an invocation of the
   --  Archive_Builder.

   Size : Natural;

   Partial_Number : Natural;

   First_Object : Natural;
   Last_Object  : Natural;

   Gcc_Name : constant String := "gcc";

   Preserve : Attribute := Time_Stamps;
   --  Used by Copy_ALI_Files. Changed to None for OpenVMS, because
   --  Copy_Attributes always fails on VMS.

   Object_Suffix : constant String := Get_Target_Object_Suffix.all;
   --  The suffix of object files on this platform

   --  Switches used when spawning processes

   No_Main_String : constant String := "-n";
   No_Main        : constant String_Access := new String'(No_Main_String);

   Output_Switch_String : constant String := "-o";
   Output_Switch        : constant String_Access :=
                            new String'(Output_Switch_String);

   Compile_Switch_String : constant String := "-c";
   Compile_Switch        : constant String_Access :=
                             new String'(Compile_Switch_String);

   Auto_Initialize_String : constant String := "-a";
   Auto_Initialize        : constant String_Access :=
                              new String'(Auto_Initialize_String);

   IO_File : File_Type;
   --  The file to get the inputs and to put the results

   Line : String (1 .. 1_000);
   Last : Natural;

   Exchange_File_Name : String_Access;
   --  Name of the exchange file

   GNAT_Version : String_Access := new String'("000");
   --  The version of GNAT, coming from the Toolchain_Version for Ada

   S_Osinte_Ads : File_Name_Type := No_File;
   --  Name_Id for "s-osinte.ads"

   S_Dec_Ads : File_Name_Type := No_File;
   --  Name_Id for "dec.ads"

   G_Trasym_Ads : File_Name_Type := No_File;
   --  Name_Id for "g-trasym.ads"

   Libgnat : String_Access := new String'("-lgnat");

   Libgnarl : String_Access := new String'("-lgnarl");

   Libgnarl_Needed : Boolean := False;
   --  True if libgnarl is needed

   Runtime_Library_Dir : String_Access := null;
   --  Full path name of tha Ada runtime library

   Current_Section : Library_Section := No_Library_Section;
   --  The current section when reading the exchange file

   Standalone : Boolean := False;
   --  True when building a stand-alone library

   Library_Path_Name : String_Access;
   --  Path name of the library file

   package Object_Files is new Table.Table
     (Table_Component_Type => String_Access,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 10,
      Table_Increment      => 100,
      Table_Name           => "Gprlib.Object_Files");
   --  A table to store the object files of the library

   Last_Object_File_Index : Natural := 0;
   --  Index of the last object file in the Object_Files table. When building
   --  a Stand Alone Library, the binder generated object file will be added
   --  in the Object_Files table.

   package Options_Table is new Table.Table
     (Table_Component_Type => String_Access,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 10,
      Table_Increment      => 100,
      Table_Name           => "Gprlib.Options_Table");
   --  A table to store the options from the exchange file

   package Imported_Library_Directories is new Table.Table
     (Table_Component_Type => String_Access,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 10,
      Table_Increment      => 100,
      Table_Name           => "Gprlib.Imported_Librar_Directories");
   --  A table to store the directories of the imported libraries

   package Imported_Library_Names is new Table.Table
     (Table_Component_Type => String_Access,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 10,
      Table_Increment      => 100,
      Table_Name           => "Gprlib.Imported_Library_Names");
   --  A table to store the names of the imported libraries

   package ALIs is new Table.Table
     (Table_Component_Type => String_Access,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 50,
      Table_Increment      => 100,
      Table_Name           => "Gprlib.Alis");
   --  A table to store all of the ALI files

   package Interface_ALIs is new Table.Table
     (Table_Component_Type => String_Access,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 50,
      Table_Increment      => 100,
      Table_Name           => "Gprlib.Interface_Alis");
   --  A table to store the ALI files of the interfaces of an SAL

   package Binding_Options_Table is new Table.Table
     (Table_Component_Type => String_Access,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 5,
      Table_Increment      => 100,
      Table_Name           => "Gprlib.Binding_Options_Table");
   --  A table to store the binding options

   package Library_Options_Table is new Table.Table
     (Table_Component_Type => String_Access,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 5,
      Table_Increment      => 100,
      Table_Name           => "Gprlib.Library_Options_Table");
   --  A table to store the library options

   package Library_Switches_Table is new Table.Table
     (Table_Component_Type => String_Access,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 5,
      Table_Increment      => 100,
      Table_Name           => "Gprlib.Library_Switches_Table");
   --  A table to store the switches for the imported libraries

   package Object_Directories is new Table.Table
     (Table_Component_Type => String_Access,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 2,
      Table_Increment      => 100,
      Table_Name           => "Gprlib.Object_Directories");
   --  A table to store the object directories of the project and of all
   --  the projects it extends.

   package Sources is new Table.Table
     (Table_Component_Type => String_Access,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 2,
      Table_Increment      => 100,
      Table_Name           => "Gprlib.Sources");

   package Generated_Sources is new Table.Table
     (Table_Component_Type => String_Access,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 2,
      Table_Increment      => 100,
      Table_Name           => "Gprlib.Generated_Sources");

   package Generated_Objects is new Table.Table
     (Table_Component_Type => String_Access,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 2,
      Table_Increment      => 100,
      Table_Name           => "Gprlib.Generated_Objects");

   Auto_Init : Boolean := False;
   --  True when a SAL is auto initializable

   Static : Boolean := False;
   --  True if the library is an archive

   Archive_Builder : String_Access := null;
   --  Name of the archive builder

   AB_Options     : String_List_Access := new String_List (1 .. 10);
   Last_AB_Option : Natural := 0;
   --  Options of the archive builder

   First_AB_Object_Pos : Natural;
   Next_AB_Object_Pos  : Natural;
   Object_Pos          : Natural;
   --  Various indexes in AB_Options used when building an archive in chunks

   AB_Append_Options : String_List_Access := new String_List (1 .. 10);
   Last_AB_Append_Option : Natural := 0;
   --  Options for appending to an archive

   Archive_Indexer : String_Access := null;
   --  Name of the archive indexer

   AI_Options     : String_List_Access := new String_List (1 .. 10);
   Last_AI_Option : Natural := 0;
   --  Options of the archive indexer

   Partial_Linker : String_Access := null;
   --  Name of the library partial linker

   PL_Options     : String_List_Access := new String_List (1 .. 10);
   Last_PL_Option : Natural := 0;
   --  Options of the library partial linker

   Partial_Linker_Path : String_Access;
   --  The path to the partial linker driver

   Archive_Suffix : String_Access := new String'(".a");

   Bind_Options     : String_List_Access := new String_List (1 .. 10);
   Last_Bind_Option : Natural := 0;

   Success : Boolean;

   Relocatable : Boolean := False;

   Library_Name : String_Access := null;

   Library_Directory : String_Access := null;

   Library_Dependency_Directory : String_Access := null;

   Library_Version      : String_Access := new String'("");
   Library_Version_Path : String_Access := new String'("");

   Symbolic_Link_Supported  : Boolean := False;

   Major_Minor_Id_Supported : Boolean := False;

   PIC_Option : String_Access := null;

   package Library_Version_Options is new Table.Table
     (Table_Component_Type => String_Access,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 2,
      Table_Increment      => 100,
      Table_Name           => "Gprlib.Library_Version_Options");

   Shared_Lib_Prefix : String_Access := new String'("lib");

   Shared_Lib_Suffix : String_Access := new String'(".so");

   package Shared_Lib_Minimum_Options is new Table.Table
     (Table_Component_Type => String_Access,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 2,
      Table_Increment      => 100,
      Table_Name           => "Gprlib.Shared_Lib_Minimum_Options");

   Copy_Source_Directory : String_Access := null;

   Driver_Name : Name_Id := No_Name;

   Gnatbind_Name : String_Access := Osint.Program_Name ("gnatbind", "gprlib");

   Gnatbind_Path : String_Access;

   Compiler_Name : String_Access := Osint.Program_Name ("gcc", "gprlib");

   Compiler_Path : String_Access;

   Path_Option : String_Access := null;

   Separate_Run_Path_Options : Boolean := False;

   Rpath : String_List_Access := null;
   --  Allocated only if Path Option is supported

   Initial_Rpath_Length : constant := 4;
   --  Initial size of Rpath, when first allocated

   Rpath_Last : Natural := 0;
   --  Index of last directory in Rpath

   Rpath_Length : Natural := 0;
   --  Length of the full run path option

   Arguments : String_List_Access := new String_List (1 .. 20);
   Last_Arg  : Natural := 0;
   Argument_Length : Natural := 0;

   --  Response Files

   Max_Command_Line_Length : Natural := 0;

   Resp_File_Format : Prj.Response_File_Format := Prj.None;

   Response_File_Switches : String_List_Access := null;

   Delete_Response_File : Boolean := True;

   procedure Add_Arg (Arg : String_Access);
   --  Add one argument to the Arguments list. Increase the size of the list
   --  if necessary.

   procedure Add_Rpath (Path : String_Access);
   --  Add a path name to Rpath

   procedure Copy_ALI_Files;
   --  Copy the ALI files. For not SALs, copy all the ALI files. For SALs,
   --  only copy the interface ALI files, marking them with the special
   --  indicator "SL" on the P line.

   procedure Copy_Sources;
   --  Copy to the Copy_Source_Directory the sources of the interfaces of
   --  a Stand-Alone Library.

   function SALs_Use_Constructors return Boolean;
   --  Indicate if Stand-Alone Libraries are automatically initialized using
   --  the constructor mechanism.

   procedure Build_Shared_Lib;

   procedure Build_Shared_Lib is separate;

   -------------
   -- Add_Arg --
   -------------

   procedure Add_Arg (Arg : String_Access) is
   begin
      if Last_Arg = Arguments'Last then
         --  Double the size of Arguments

         declare
            New_Args : constant String_List_Access :=
                         new String_List (1 .. 2 * Last_Arg);

         begin
            New_Args (Arguments'Range) := Arguments.all;
            Arguments := New_Args;
         end;
      end if;

      Last_Arg := Last_Arg + 1;
      Arguments (Last_Arg) := Arg;
      Argument_Length := Argument_Length + Arg'Length + 1;
   end Add_Arg;

   ---------------
   -- Add_Rpath --
   ---------------

   procedure Add_Rpath (Path : String_Access) is

      procedure Double;
      --  Double Rpath size

      ------------
      -- Double --
      ------------

      procedure Double is
         New_Rpath : constant String_List_Access :=
                       new String_List (1 .. 2 * Rpath'Length);
      begin
         New_Rpath (1 .. Rpath_Last) := Rpath (1 .. Rpath_Last);

         for J in 1 .. Rpath_Last loop
            Rpath (J) := null;
         end loop;

         Free (Rpath);
         Rpath := New_Rpath;
      end Double;

   --  Start of processing for Add_Rpath

   begin
      --  If firt path, allocate initial Rpath

      if Rpath = null then
         Rpath := new String_List (1 .. Initial_Rpath_Length);
         Rpath_Last := 1;
         Rpath_Length := 0;

      else
         --  Otherwise, double Rpath if it is full

         if Rpath_Last = Rpath'Last then
            Double;
         end if;

         Rpath_Last := Rpath_Last + 1;
         Rpath_Length := Rpath_Length + 1;
      end if;

      --  Add the path name

      Rpath (Rpath_Last) := Path;
      Rpath_Length := Rpath_Length + Path'Length;
   end Add_Rpath;

   --------------------
   -- Copy_ALI_Files --
   --------------------

   procedure Copy_ALI_Files is
      Success      : Boolean := False;
      FD           : File_Descriptor;
      Len          : Integer;
      Actual_Len   : Integer;
      S            : String_Access;
      Curr         : Natural;
      P_Line_Found : Boolean;
      Status       : Boolean;

   begin
      if not Standalone then
         for Index in 1 .. ALIs.Last loop
            declare
               Destination : constant String :=
                               Library_Dependency_Directory.all &
                               Directory_Separator &
                               Base_Name (ALIs.Table (Index).all);
               Disregard : Boolean;
            begin
               if Is_Regular_File (Destination) then
                  Set_Writable (Destination);
                  Delete_File (Destination, Disregard);
               end if;
            end;

            if Verbose_Mode then
               Put ("Copying ");
               Put (ALIs.Table (Index).all);
               Put_Line (" to library dependency directory");
            end if;

            Success := False;

            --  Look in the object directories for the ALI files, starting
            --  with the object directory of the projects, then those of the
            --  projects it extends, if any.

            for J in 1 .. Object_Directories.Last loop
               declare
                  ALI_File : constant String :=
                    Object_Directories.Table (J).all &
                    Directory_Separator &
                   ALIs.Table (Index).all;
               begin
                  if Is_Regular_File (ALI_File) then
                     Copy_File
                       (ALI_File,
                        Library_Dependency_Directory.all,
                        Success,
                        Mode     => Overwrite,
                        Preserve => Preserve);
                     exit;
                  end if;
               end;
            end loop;

            exit when not Success;
         end loop;

      else
         for Index in 1 .. Interface_ALIs.Last loop
            declare
               File_Name : String :=
                             Base_Name (Interface_ALIs.Table (Index).all);
               Destination : constant String :=
                               Library_Dependency_Directory.all &
                               Directory_Separator &
                               File_Name;
               Disregard : Boolean;

            begin
               if Is_Regular_File (Destination) then
                  Set_Writable (Destination);
                  Delete_File (Destination, Disregard);
               end if;

               if Verbose_Mode then
                  Put ("Copying ");
                  Put (Interface_ALIs.Table (Index).all);
                  Put_Line (" to library dependency directory");
               end if;

               Osint.Canonical_Case_File_Name (File_Name);

               --  Open the file

               Name_Len := Interface_ALIs.Table (Index)'Length;
               Name_Buffer (1 .. Name_Len) :=
                  Interface_ALIs.Table (Index).all;
               Name_Len := Name_Len + 1;
               Name_Buffer (Name_Len) := ASCII.NUL;

               FD := Open_Read (Name_Buffer'Address, Binary);

               if FD /= Invalid_FD then
                  Len := Integer (File_Length (FD));

                  S := new String (1 .. Len + 3);

                  --  Read the file. Note that the loop is not necessary
                  --  since the whole file is read at once except on VMS.

                  Curr := 1;
                  Actual_Len := Len;

                  while Actual_Len /= 0 loop
                     Actual_Len := Read (FD, S (Curr)'Address, Len);
                     Curr := Curr + Actual_Len;
                  end loop;

                  --  We are done with the input file, so we close it
                  --  (we simply ignore any bad status on the close)

                  Close (FD, Status);

                  P_Line_Found := False;

                  --  Look for the P line. When found, add marker SL at the
                  --  beginning of the P line.

                  for Index in 1 .. Len - 3 loop
                     if (S (Index) = ASCII.LF or else S (Index) = ASCII.CR)
                       and then S (Index + 1) = 'P'
                     then
                        S (Index + 5 .. Len + 3) := S (Index + 2 .. Len);
                        S (Index + 2 .. Index + 4) := " SL";
                        P_Line_Found := True;
                        exit;
                     end if;
                  end loop;

                  if P_Line_Found then

                     --  Create new modified ALI file

                     Name_Len := Library_Dependency_Directory'Length;
                     Name_Buffer (1 .. Name_Len) :=
                       Library_Dependency_Directory.all;
                     Name_Len := Name_Len + 1;
                     Name_Buffer (Name_Len) := Directory_Separator;
                     Name_Buffer
                       (Name_Len + 1 .. Name_Len + File_Name'Length) :=
                       File_Name;
                     Name_Len := Name_Len + File_Name'Length + 1;
                     Name_Buffer (Name_Len) := ASCII.NUL;

                     FD := Create_File (Name_Buffer'Address, Binary);

                     --  Write the modified text and close the newly
                     --  created file.

                     if FD /= Invalid_FD then
                        Actual_Len := Write (FD, S (1)'Address, Len + 3);

                        Close (FD, Status);

                        --  Set Success to True only if the newly
                        --  created file has been correctly written.

                        Success := Status and Actual_Len = Len + 3;

                        if Success then
                           Set_Read_Only
                             (Name_Buffer (1 .. Name_Len - 1));
                        end if;
                     end if;
                  end if;
               end if;
            end;
         end loop;
      end if;

      if not Success then
         Osint.Fail ("could not copy ALI files to library directory");
      end if;

   end Copy_ALI_Files;

   ------------------
   -- Copy_Sources --
   ------------------

   procedure Copy_Sources is
      Text     : Text_Buffer_Ptr;
      The_ALI  : ALI.ALI_Id;
      Lib_File : File_Name_Type;

      First_Unit  : ALI.Unit_Id;
      Second_Unit : ALI.Unit_Id;

      Copy_Subunits : Boolean := False;

      use ALI;

      procedure Copy (File_Name : File_Name_Type);
      --  Copy one source of the project to the copy source directory

      ----------
      -- Copy --
      ----------

      procedure Copy (File_Name : File_Name_Type) is
         Success : Boolean := False;

         Fname   : constant String := Get_Name_String (File_Name);

      begin
         for Index in 1 .. Sources.Last loop
            if Base_Name (Sources.Table (Index).all) = Fname then

               Copy_File
                 (Sources.Table (Index).all,
                  Copy_Source_Directory.all,
                  Success,
                  Mode     => Overwrite,
                  Preserve => Preserve);
               exit;
            end if;
         end loop;
      end Copy;

   begin
      for Index in 1 .. Interface_ALIs.Last loop

         --  First, load the ALI file

         Name_Len := 0;
         Add_Str_To_Name_Buffer (Interface_ALIs.Table (Index).all);
         Lib_File := Name_Find;
         Text := Osint.Read_Library_Info (Lib_File);
         The_ALI :=
           ALI.Scan_ALI
             (Lib_File,
              Text,
              Ignore_ED  => False,
              Err        => True,
              Read_Lines => "U");
         Free (Text);

         Second_Unit := ALI.No_Unit_Id;
         First_Unit := ALI.ALIs.Table (The_ALI).First_Unit;
         Copy_Subunits := True;

         --  If there is both a spec and a body, check if they are both needed

         if ALI.Units.Table (First_Unit).Utype = ALI.Is_Body then
            Second_Unit := ALI.ALIs.Table (The_ALI).Last_Unit;

            --  If the body is not needed, then reset First_Unit

            if not ALI.Units.Table (Second_Unit).Body_Needed_For_SAL then
               First_Unit := ALI.No_Unit_Id;
               Copy_Subunits := False;
            end if;

         elsif ALI.Units.Table (First_Unit).Utype = ALI.Is_Spec_Only then
            Copy_Subunits := False;
         end if;

         --  Copy the file(s) that need to be copied

         if First_Unit /= No_Unit_Id then
            Copy (File_Name => ALI.Units.Table (First_Unit).Sfile);
         end if;

         if Second_Unit /= No_Unit_Id then
            Copy (File_Name => ALI.Units.Table (Second_Unit).Sfile);
         end if;

         --  Copy all the separates, if any

         if Copy_Subunits then
            for Dep in ALI.ALIs.Table (The_ALI).First_Sdep ..
              ALI.ALIs.Table (The_ALI).Last_Sdep
            loop
               if ALI.Sdep.Table (Dep).Subunit_Name /= No_Name then
                  Copy (File_Name => Sdep.Table (Dep).Sfile);
               end if;
            end loop;
         end if;
      end loop;
   end Copy_Sources;

   ---------------------------
   -- SALs_Use_Constructors --
   ---------------------------

   function SALs_Use_Constructors return Boolean is
      function C_SALs_Init_Using_Constructors return Integer;
      pragma Import (C, C_SALs_Init_Using_Constructors,
                     "__gnat_sals_init_using_constructors");
   begin
      return C_SALs_Init_Using_Constructors /= 0;
   end SALs_Use_Constructors;

begin
   --  Initialize some packages

   Csets.Initialize;
   Namet.Initialize;
   Snames.Initialize;

   --  Copy_Attributes always fails on VMS

   if Hostparm.OpenVMS then
      Preserve := None;
      Shared_Libgnat_Separator := '_';
   end if;

   if Argument_Count /= 1 then
      Put_Line ("usage: gprlib <input file>");

      if Argument_Count /= 0 then
         Osint.Fail ("incorrect invocation");
      end if;

      return;
   end if;

   Exchange_File_Name := new String'(Argument (1));

   --  DEBUG: save a copy of the exchange file

   if Getenv ("GPRLIB_DEBUG").all = "TRUE" then
      Copy_File
        (Exchange_File_Name.all,
         Exchange_File_Name.all & "__saved",
         Success,
         Mode => Overwrite,
         Preserve => Preserve);
   end if;

   begin
      Open (IO_File, In_File, Exchange_File_Name.all);
   exception
      when others =>
         Osint.Fail ("could not read " & Exchange_File_Name.all);
   end;

   while not End_Of_File (IO_File) loop
      Get_Line (IO_File, Line, Last);

      if Last > 0 and then Line (1) = '[' then
         Current_Section := Get_Library_Section (Line (1 .. Last));

         case Current_Section is
            when No_Library_Section =>
               Osint.Fail ("unknown section: " & Line (1 .. Last));

            when Quiet =>
               Quiet_Output := True;
               Verbose_Mode := False;

            when Verbose =>
               Quiet_Output := False;
               Verbose_Mode := True;

            when Gprexch.Relocatable =>
               Relocatable := True;
               Static      := False;

            when Gprexch.Static =>
               Static      := True;
               Relocatable := False;

            when Gprexch.Archive_Builder =>
               Archive_Builder := null;
               Last_AB_Option  := 0;

            when Gprexch.Archive_Builder_Append_Option =>
               Last_AB_Append_Option := 0;

            when Gprexch.Archive_Indexer =>
               Archive_Indexer := null;
               Last_AI_Option  := 0;

            when Gprexch.Partial_Linker =>
               Partial_Linker := null;
               Last_PL_Option := 0;

            when Gprexch.Auto_Init =>
               Auto_Init := True;

            when Gprexch.Symbolic_Link_Supported =>
               Symbolic_Link_Supported  := True;

            when Gprexch.Major_Minor_Id_Supported =>
               Major_Minor_Id_Supported := True;

            when Gprexch.Keep_Response_File =>
               Delete_Response_File := False;

            when Gprexch.Separate_Run_Path_Options =>
               Separate_Run_Path_Options := True;

            when others =>
               null;
         end case;

      elsif Last > 0
        or else Current_Section = Gprexch.Shared_Lib_Prefix
        or else Current_Section = Gprexch.Response_File_Switches
      then
         case Current_Section is
            when No_Library_Section =>
               Osint.Fail ("no section specified: " & Line (1 .. Last));

            when Quiet =>
               Osint.Fail ("quiet section should be empty");

            when Verbose =>
               Osint.Fail ("verbose section should be empty");

            when Gprexch.Relocatable =>
               Osint.Fail ("relocatable section should be empty");

            when Gprexch.Static =>
               Osint.Fail ("static section should be empty");

            when Gprexch.Keep_Response_File =>
               Osint.Fail ("keep response file section should be empty");

            when Gprexch.Separate_Run_Path_Options =>
               Osint.Fail ("separate run path options should be empty");

            when Gprexch.Object_Files =>
               Object_Files.Append (new String'(Line (1 .. Last)));

            when Gprexch.Options =>
               Options_Table.Append (new String'(Line (1 .. Last)));

            when Gprexch.Object_Directory =>
               Object_Directories.Append (new String'(Line (1 .. Last)));

            when Gprexch.Library_Name =>
               Library_Name := new String'(Line (1 .. Last));

            when Gprexch.Library_Directory =>
               Library_Directory := new String'(Line (1 .. Last));

            when Gprexch.Library_Dependency_Directory =>
               Library_Dependency_Directory :=
                 new String'(Line (1 .. Last));

            when Gprexch.Library_Version =>
               Library_Version := new String'(Line (1 .. Last));

            when Gprexch.Library_Options =>
               Library_Options_Table.Append (new String'(Line (1 .. Last)));

            when Library_Path =>
               Osint.Fail ("library path should not be specified");

            when Gprexch.Library_Version_Options =>
               Library_Version_Options.Append
                 (new String'(Line (1 .. Last)));

            when Gprexch.Shared_Lib_Prefix =>
               Shared_Lib_Prefix := new String'(Line (1 .. Last));

            when Gprexch.Shared_Lib_Suffix =>
               Shared_Lib_Suffix := new String'(Line (1 .. Last));

            when Gprexch.Shared_Lib_Minimum_Options =>
               Shared_Lib_Minimum_Options.Append
                 (new String'(Line (1 .. Last)));

            when Gprexch.Symbolic_Link_Supported =>
               Osint.Fail
                 ("symbolic link supported section should be empty");

            when Gprexch.Major_Minor_Id_Supported =>
               Osint.Fail
                 ("major minor id supported section should be empty");

            when Gprexch.PIC_Option =>
               PIC_Option := new String'(Line (1 .. Last));

            when Gprexch.Imported_Libraries =>
               if End_Of_File (IO_File) then
                  Osint.Fail
                    ("no library name for imported library " &
                     Line (1 .. Last));

               else
                  Imported_Library_Directories.Append
                    (new String'(Line (1 .. Last)));
                  Get_Line (IO_File, Line, Last);
                  Imported_Library_Names.Append
                    (new String'(Line (1 .. Last)));
               end if;

            when Gprexch.Driver_Name =>
               Name_Len := Last;
               Name_Buffer (1 .. Name_Len) := Line (1 .. Last);
               Driver_Name := Name_Find;

            when Gprexch.Compilers =>
               if End_Of_File (IO_File) then
                  Osint.Fail
                    ("no compiler specified for language " &
                     Line (1 .. Last));

               else
                  To_Lower (Line (1 .. Last));

                  if Line (1 .. Last) = "ada" then
                     Get_Line (IO_File, Line, Last);

                     if Last = 0 then
                        Osint.Fail
                          ("Ada compiler name cannot be empty");

                     else
                        Compiler_Name :=
                          new String'(Line (1 .. Last));

                        if Last > 3
                          and then Line (Last - 2 .. Last) = "gcc"
                        then
                           Gnatbind_Name :=
                             new String'(Line (1 .. Last - 3) &
                                         "gnatbind");
                        elsif Last > 7
                          and then Line (Last - 6 .. Last) = "gcc.exe"
                        then
                           Gnatbind_Name :=
                             new String'(Line (1 .. Last - 7) &
                                         "gnatbind");
                        end if;
                     end if;

                  else
                     Skip_Line (IO_File);
                  end if;
               end if;

            when Toolchain_Version =>
               if End_Of_File (IO_File) then
                  Osint.Fail
                    ("no toolchain version for language " &
                     Line (1 .. Last));

               elsif Line (1 .. Last) = "ada" then
                  Get_Line (IO_File, Line, Last);

                  if Last > 5 and then Line (1 .. 5) = "GNAT " then
                     GNAT_Version := new String'(Line (6 .. Last));

                     --  On VMS, replace all '.' with '_', to avoid names with
                     --  several dots.

                     if Hostparm.OpenVMS then
                        for J in 6 .. Last loop
                           if Line (J) = '.' then
                              Line (J) := '_';
                           end if;
                        end loop;
                     end if;

                     Libgnat :=
                       new String'
                         ("-lgnat" &
                          Shared_Libgnat_Separator &
                          Line (6 .. Last));
                     Libgnarl :=
                       new String'
                         ("-lgnarl" &
                          Shared_Libgnat_Separator &
                          Line (6 .. Last));
                  end if;

               else
                  Skip_Line (IO_File);
               end if;

            when Gprexch.Archive_Builder =>
               if Archive_Builder = null then
                  Archive_Builder := new String'(Line (1 .. Last));

               else
                  Add
                    (new String'(Line (1 .. Last)),
                     AB_Options,
                     Last_AB_Option);
               end if;

            when Gprexch.Archive_Builder_Append_Option =>
               Add
                 (new String'(Line (1 .. Last)),
                  AB_Append_Options,
                  Last_AB_Append_Option);

            when Gprexch.Archive_Indexer =>
               if Archive_Indexer = null then
                  Archive_Indexer := new String'(Line (1 .. Last));

               else
                  Add
                    (new String'(Line (1 .. Last)),
                     AI_Options,
                     Last_AI_Option);
               end if;

            when Gprexch.Partial_Linker =>
               if Partial_Linker = null then
                  Partial_Linker := new String'(Line (1 .. Last));

               else
                  Add
                    (new String'(Line (1 .. Last)),
                     PL_Options,
                     Last_PL_Option);
               end if;

            when Gprexch.Archive_Suffix =>
               Archive_Suffix := new String'(Line (1 .. Last));

            when Gprexch.Run_Path_Option =>
               Path_Option := new String'(Line (1 .. Last));

            when Gprexch.Auto_Init =>
               Osint.Fail ("auto init section should be empty");

            when Interface_Dep_Files =>
               Interface_ALIs.Append (new String'(Line (1 .. Last)));
               Standalone := True;

            when Dependency_Files =>
               if Last > 4 and then Line (Last - 3 .. Last) = ".ali" then
                  ALIs.Append (new String'(Line (1 .. Last)));
               end if;

            when Binding_Options =>
               Binding_Options_Table.Append (new String'(Line (1 .. Last)));

            when Copy_Source_Dir =>
               Copy_Source_Directory := new String'(Line (1 .. Last));

            when Gprexch.Sources =>
               Sources.Append (new String'(Line (1 .. Last)));

            when Gprexch.Runtime_Library_Dir =>
               if End_Of_File (IO_File) then
                  Osint.Fail
                    ("no runtime library dir for language " &
                     Line (1 .. Last));

               elsif Line (1 .. Last) = "ada" then
                  Get_Line (IO_File, Line, Last);
                  Runtime_Library_Dir := new String'(Line (1 .. Last));

               else
                  Skip_Line (IO_File);
               end if;

            when Gprexch.Generated_Object_Files |
                 Gprexch.Generated_Source_Files =>
               null;

            when Gprexch.Max_Command_Line_Length =>
               begin
                  Max_Command_Line_Length := Natural'Value (Line (1 .. Last));

               exception
                  when Constraint_Error =>
                     Osint.Fail
                       ("incorrect value for max command line length: " &
                        Line (1 .. Last));
               end;

            when Gprexch.Response_File_Format =>
               begin
                  Resp_File_Format :=
                    Prj.Response_File_Format'Value (Line (1 .. Last));

               exception
                  when Constraint_Error =>
                     Osint.Fail
                       ("incorrect value for response file format: " &
                        Line (1 .. Last));
               end;

            when Gprexch.Response_File_Switches =>
               if Response_File_Switches = null then
                  Response_File_Switches := new String_List (1 .. 1);

               else
                  declare
                     New_Switches : constant String_List_Access :=
                                      new String_List
                                        (1 .. Response_File_Switches'Last + 1);

                  begin
                     New_Switches (Response_File_Switches'Range) :=
                       Response_File_Switches.all;
                     Free (Response_File_Switches);
                     Response_File_Switches := New_Switches;
                  end;
               end if;

               Response_File_Switches (Response_File_Switches'Last) :=
                 new String'(Line (1 .. Last));
         end case;
      end if;
   end loop;

   Close (IO_File);

   if Object_Files.Last = 0 then
      Osint.Fail ("no object files specified");
   end if;

   Last_Object_File_Index := Object_Files.Last;

   if Library_Name = null then
      Osint.Fail ("no library name specified");
   end if;

   if Library_Directory = null then
      Osint.Fail ("no library directory specified");
   end if;

   if Object_Directories.Last = 0 then
      Osint.Fail ("no object directory specified");
   end if;

   if Library_Directory.all = Object_Directories.Table (1).all then
      Osint.Fail ("object directory and library directory cannot be the same");
   end if;

   if Library_Dependency_Directory = null then
      Library_Dependency_Directory := Library_Directory;
   end if;

   --  We work in the object directory

   begin
      Change_Dir (Object_Directories.Table (1).all);

   exception
      when others =>
         Osint.Fail
           ("cannot change to object directory " &
            Object_Directories.Table (1).all);
   end;

   if Standalone then
      declare
         Binder_Generated_File   :  String :=
                                     "b__" & Library_Name.all & ".adb";
         Binder_Generated_Object :  String :=
                                     "b__" & Library_Name.all & Object_Suffix;
         ALI_First_Index         : Positive;
         First_ALI               : File_Name_Type;
         T                       : Text_Buffer_Ptr;
         A                       : ALI.ALI_Id;
         use ALI;

      begin
         Osint.Canonical_Case_File_Name (Binder_Generated_File);
         Osint.Canonical_Case_File_Name (Binder_Generated_Object);

         Gnatbind_Path := Locate_Exec_On_Path (Gnatbind_Name.all);

         if Gnatbind_Path = null then
            Osint.Fail ("unable to locate binder " & Gnatbind_Name.all);
         end if;

         Last_Bind_Option := 0;
         Add (No_Main, Bind_Options, Last_Bind_Option);
         Add (Output_Switch, Bind_Options, Last_Bind_Option);
         Add
           ("b__" & Library_Name.all & ".adb", Bind_Options, Last_Bind_Option);
         Add ("-L" & Library_Name.all, Bind_Options, Last_Bind_Option);

         if Auto_Init and then
           SALs_Use_Constructors and then
           GNAT_Version.all >= "5.04"
         then
            Add (Auto_Initialize, Bind_Options, Last_Bind_Option);
         end if;

         for J in 1 .. Binding_Options_Table.Last loop
            Add
              (Binding_Options_Table.Table (J).all,
               Bind_Options,
               Last_Bind_Option);
         end loop;

         --  Get an eventual --RTS from the ALI file

         Name_Len := 0;
         Add_Str_To_Name_Buffer (ALIs.Table (1).all);
         First_ALI := Name_Find;

         --  Load the ALI file

         T := Osint.Read_Library_Info (First_ALI, True);

         --  Read it

         A := Scan_ALI
           (First_ALI,
            T,
            Ignore_ED  => False,
            Err        => False,
            Read_Lines => "A");

         if A /= No_ALI_Id then
            for Index in
              ALI.Units.Table (ALI.ALIs.Table (A).First_Unit).First_Arg ..
              ALI.Units.Table (ALI.ALIs.Table (A).First_Unit).Last_Arg
            loop
               --  Look for --RTS. If found, add the switch to call gnatbind

               declare
                  Arg : Types.String_Ptr renames Args.Table (Index);
               begin
                  if Arg'Length >= 6 and then
                    Arg (Arg'First + 2 .. Arg'First + 5) = "RTS="
                  then
                     Add (Arg.all, Bind_Options, Last_Bind_Option);
                     exit;
                  end if;
               end;
            end loop;
         end if;

         ALI_First_Index := Last_Bind_Option + 1;

         for J in 1 .. ALIs.Last loop
            Add (ALIs.Table (J), Bind_Options, Last_Bind_Option);
         end loop;

         if not Quiet_Output then
            if Verbose_Mode then
               Put (Gnatbind_Path.all);
            else
               Put (Gnatbind_Name.all);
            end if;

            for J in 1 .. Last_Bind_Option loop
               if (not Verbose_Mode) and then J > ALI_First_Index then
                  Put (" ...");
                  exit;
               end if;

               Put (" ");
               Put (Bind_Options (J).all);
            end loop;

            New_Line;
         end if;

         --  If there is more than one object directory, set ADA_OBJECTS_PATH
         --  for the additional object libraries, so that gnatbind may find
         --  all the ALI files.

         if Object_Directories.Last > 1 then
            declare
               Object_Path : String_Access :=
                               new String'(Object_Directories.Table (2).all);

            begin
               for J in 3 .. Object_Directories.Last loop
                  Object_Path :=
                    new String'
                      (Object_Path.all &
                       Path_Separator &
                       Object_Directories.Table (J).all);
               end loop;

               Setenv ("ADA_OBJECTS_PATH", Object_Path.all);
            end;
         end if;

         declare
            Size         : Natural := 0;
            Maximum_Size : Integer;
            pragma Import (C, Maximum_Size, "__gnat_link_max");
            --  Maximum number of bytes to put in an invocation of the
            --  gnatbind.

         begin
            for J in 1 .. Last_Bind_Option loop
               Size := Size + Bind_Options (J)'Length + 1;
            end loop;

            --  Invoke gnatbind with the arguments if the size is not too large
            --  or if the version of GNAT is not recent enough.

            if GNAT_Version.all < "6" or else Size <= Maximum_Size then
               Spawn
                 (Gnatbind_Path.all,
                  Bind_Options (1 .. Last_Bind_Option),
                  Success);

            else
               --  Otherwise create a temporary response file

               declare
                  FD            : File_Descriptor;
                  Path          : Path_Name_Type;
                  Args          : Argument_List (1 .. 1);
                  EOL           : constant String (1 .. 1) := (1 => ASCII.LF);
                  Status        : Integer;
                  Succ          : Boolean;
                  Quotes_Needed : Boolean;
                  Last_Char     : Natural;
                  Ch            : Character;

               begin
                  Tempdir.Create_Temp_File (FD, Path);
                  Args (1) := new String'("@" & Get_Name_String (Path));

                  for J in 1 .. Last_Bind_Option loop

                     --  Check if the argument should be quoted

                     Quotes_Needed := False;
                     Last_Char     := Bind_Options (J)'Length;

                     for K in Bind_Options (J)'Range loop
                        Ch := Bind_Options (J) (K);

                        if Ch = ' ' or else Ch = ASCII.HT or else Ch = '"' then
                           Quotes_Needed := True;
                           exit;
                        end if;
                     end loop;

                     if Quotes_Needed then

                        --  Quote the argument, doubling '"'

                        declare
                           Arg : String (1 .. Bind_Options (J)'Length * 2 + 2);

                        begin
                           Arg (1) := '"';
                           Last_Char := 1;

                           for K in Bind_Options (J)'Range loop
                              Ch := Bind_Options (J) (K);
                              Last_Char := Last_Char + 1;
                              Arg (Last_Char) := Ch;

                              if Ch = '"' then
                                 Last_Char := Last_Char + 1;
                                 Arg (Last_Char) := '"';
                              end if;
                           end loop;

                           Last_Char := Last_Char + 1;
                           Arg (Last_Char) := '"';

                           Status := Write (FD, Arg'Address, Last_Char);
                        end;

                     else
                        Status := Write
                          (FD,
                           Bind_Options (J) (Bind_Options (J)'First)'Address,
                           Last_Char);
                     end if;

                     if Status /= Last_Char then
                        Osint.Fail ("disk full");
                     end if;

                     Status := Write (FD, EOL (1)'Address, 1);

                     if Status /= 1 then
                        Osint.Fail ("disk full");
                     end if;
                  end loop;

                  Close (FD);

                  --  And invoke gnatbind with this this response file

                  Spawn (Gnatbind_Path.all, Args, Success);

                  if Delete_Response_File then
                     Delete_File (Get_Name_String (Path), Succ);

                     if not Succ then
                        null;
                     end if;
                  end if;
               end;
            end if;
         end;

         if not Success then
            Osint.Fail ("invocation of " & Gnatbind_Name.all & " failed");
         end if;

         Generated_Sources.Append
           (new String'("b__" & Library_Name.all & ".ads"));
         Generated_Sources.Append
           (new String'("b__" & Library_Name.all & ".adb"));
         Generated_Sources.Append
           (new String'("b__" & Library_Name.all & ".ali"));

         Compiler_Path := Locate_Exec_On_Path (Compiler_Name.all);

         if Compiler_Path = null then
            Osint.Fail ("unable to locate compiler " & Compiler_Name.all);
         end if;

         Last_Bind_Option := 0;

         Add (Compile_Switch, Bind_Options, Last_Bind_Option);
         Add (Binder_Generated_File, Bind_Options, Last_Bind_Option);
         Add (Output_Switch, Bind_Options, Last_Bind_Option);
         Add (Binder_Generated_Object, Bind_Options, Last_Bind_Option);

         if Relocatable and then PIC_Option /= null then
            Add (PIC_Option, Bind_Options, Last_Bind_Option);
         end if;

         --  Get the back-end switches and --RTS from the ALI file

         --  Load the ALI file

         T := Osint.Read_Library_Info (First_ALI, True);

         --  Read it

         A := Scan_ALI
           (First_ALI,
            T,
            Ignore_ED  => False,
            Err        => False,
            Read_Lines => "A");

         if A /= No_ALI_Id then
            for Index in
              ALI.Units.Table
                (ALI.ALIs.Table (A).First_Unit).First_Arg ..
              ALI.Units.Table
                (ALI.ALIs.Table (A).First_Unit).Last_Arg
            loop
               --  Do not compile with the front end switches except
               --  for --RTS.

               declare
                  Arg : Types.String_Ptr renames Args.Table (Index);
               begin
                  if not Is_Front_End_Switch (Arg.all)
                    or else
                      (Arg'Length > 6 and then
                       Arg (Arg'First + 2 .. Arg'First + 5) = "RTS=")
                  then
                     Add (Arg.all, Bind_Options, Last_Bind_Option);
                  end if;
               end;
            end loop;
         end if;

         if not Quiet_Output then
            if Verbose_Mode then
               Put (Compiler_Path.all);
            else
               Put (Compiler_Name.all);
            end if;

            for J in 1 .. Last_Bind_Option loop
               Put (" ");
               Put (Bind_Options (J).all);
            end loop;

            New_Line;
         end if;

         Spawn
           (Compiler_Path.all, Bind_Options (1 .. Last_Bind_Option), Success);

         if not Success then
            Osint.Fail ("invocation of " & Compiler_Name.all & " failed");
         end if;

         Generated_Objects.Append (new String'(Binder_Generated_Object));

         Object_Files.Append (new String'(Binder_Generated_Object));

         --  For shared libraries, check if libgnarl is needed

         if Relocatable and then Runtime_Library_Dir /= null then
            declare
               BG_File : File_Type;
               Line : String (1 .. 1_000);
               Last : Natural;

            begin
               Open (BG_File, In_File, Binder_Generated_File);

               while not End_Of_File (BG_File) loop
                  Get_Line (BG_File, Line, Last);
                  exit when Line (1 .. Last) = Begin_Info;
               end loop;

               while not End_Of_File (BG_File) loop
                  Get_Line (BG_File, Line, Last);
                  exit when Line (1 .. Last) = End_Info;

                  if Line (9 .. Last) = "-lgnarl" then
                     Libgnarl_Needed := True;
                     exit;
                  end if;

               end loop;
            end;
         end if;
      end;
   end if;

   --  Archives

   if Static then
      if Standalone and then Partial_Linker /= null then
         Partial_Linker_Path := Locate_Exec_On_Path (Partial_Linker.all);

         if Partial_Linker_Path = null then
            Osint.Fail ("unable to locate linker " & Partial_Linker.all);
         end if;
      end if;

      if Archive_Builder = null then
         Osint.Fail ("no archive builder specified");
      end if;

      Library_Path_Name :=
        new String'
          (Library_Directory.all &
           Directory_Separator &
           "lib" &
           Library_Name.all &
           Archive_Suffix.all);

      Add (Library_Path_Name, AB_Options, Last_AB_Option);

      First_AB_Object_Pos := Last_AB_Option + 1;

      if Standalone and then Partial_Linker_Path /= null then
         --  If partial linker is used, do a partial link and put the resulting
         --  object file in the archive.

         Partial_Number := 0;
         First_Object := 1;

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
                       (Library_Name.all, Partial_Number - 1, Object_Suffix),
                     PL_Options,
                     Last_PL_Option);
               end if;

               for J in 1 .. Last_PL_Option loop
                  Size := Size + 1 + PL_Options (J)'Length;
               end loop;

               loop
                  Add
                    (Object_Files.Table (First_Object),
                     PL_Options,
                     Last_PL_Option);
                  Size := Size + 1 + PL_Options (Last_PL_Option)'Length;

                  First_Object := First_Object + 1;
                  exit when
                    First_Object > Object_Files.Last or else
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
                  Osint.Fail
                    ("call to linker driver " &
                     Partial_Linker.all & " failed");
               end if;

               if First_Object > Object_Files.Last then
                  Add (Partial, AB_Options, Last_AB_Option);
                  exit;
               end if;

               Last_PL_Option := Saved_Last_PL_Option;
               Partial_Number := Partial_Number + 1;
            end;
         end loop;

      else
         --  Not a standalone library, or Partial linker is not specified.
         --  Put all objects in the archive.

         for J in 1 .. Object_Files.Last loop
            Add (Object_Files.Table (J), AB_Options, Last_AB_Option);
         end loop;
      end if;

      if Last_AB_Append_Option = 0 then
         --  If there is no Archive_Builder_Append_Option, always build the
         --  archive in one chunk.

         Next_AB_Object_Pos := Last_AB_Option + 1;

      else
         --  If Archive_Builder_Append_Option is specified, for the creation of
         --  the archive, only put on the command line a number of character
         --  lower that Maximum_Size.

         Size := 0;
         for J in 1 .. First_AB_Object_Pos - 1 loop
            Size := Size + AB_Options (J)'Length + 1;
         end loop;

         Next_AB_Object_Pos := First_AB_Object_Pos;

         while Next_AB_Object_Pos <= Last_AB_Option loop
            Size := Size + AB_Options (Next_AB_Object_Pos)'Length + 1;
            exit when Size > Maximum_Size;
            Next_AB_Object_Pos := Next_AB_Object_Pos + 1;
         end loop;

         --  Display the invocation of the archive builder for the creation of
         --  the archive.

         if not Quiet_Output then
            if Verbose_Mode then
               Put (Archive_Builder.all);
            else
               Put (Base_Name (Archive_Builder.all));
            end if;

            for J in 1 .. Next_AB_Object_Pos - 1 loop
               if (not Verbose_Mode) and then J >= 5 then
                  Put (" ...");
                  exit;
               end if;

               Put (' ');
               Put (AB_Options (J).all);
            end loop;

            New_Line;
         end if;

         Spawn
           (Archive_Builder.all,
            AB_Options (1 .. Next_AB_Object_Pos - 1),
            Success);

         if not Success then
            Osint.Fail
              ("call to archive builder " & Archive_Builder.all & " failed");
         end if;
      end if;

      --  If the archive has not been created complete, add the remaining
      --  chunks

      if Next_AB_Object_Pos <= Last_AB_Option then
         First_AB_Object_Pos := Last_AB_Append_Option + 2;
         AB_Options (1 .. Last_AB_Append_Option) :=
           AB_Append_Options (1 .. Last_AB_Append_Option);
         AB_Options (Last_AB_Append_Option + 1) := Library_Path_Name;

         loop
            Size := 0;
            for J in 1 .. First_AB_Object_Pos - 1 loop
               Size := Size + AB_Options (J)'Length + 1;
            end loop;

            Object_Pos := First_AB_Object_Pos;
            while Next_AB_Object_Pos <= Last_AB_Option loop
               Size := Size + AB_Options (Next_AB_Object_Pos)'Length + 1;
               exit when Size > Maximum_Size;
               AB_Options (Object_Pos) := AB_Options (Next_AB_Object_Pos);
               Object_Pos := Object_Pos + 1;
               Next_AB_Object_Pos := Next_AB_Object_Pos + 1;
            end loop;

            --  Display the invocation of the Archive Builder for this chunk

            if not Quiet_Output then
               if Verbose_Mode then
                  Put (Archive_Builder.all);
               else
                  Put (Base_Name (Archive_Builder.all));
               end if;

               for J in 1 .. Object_Pos - 1 loop
                  if (not Verbose_Mode) and then J >= 5 then
                     Put (" ...");
                     exit;
                  end if;

                  Put (' ');
                  Put (AB_Options (J).all);
               end loop;

               New_Line;
            end if;

            Spawn
              (Archive_Builder.all,
               AB_Options (1 .. Object_Pos - 1),
               Success);

            if not Success then
               Osint.Fail
                 ("call to archive builder " &
                  Archive_Builder.all & " failed");
            end if;

            exit when Next_AB_Object_Pos > Last_AB_Option;
         end loop;
      end if;

      --  If there is an Archive Indexer, invoke it

      if Archive_Indexer /= null then
         Add (Library_Path_Name, AI_Options, Last_AI_Option);

         if not Quiet_Output then
            if Verbose_Mode then
               Put (Archive_Indexer.all);
            else
               Put (File_Name (Archive_Indexer.all));
            end if;

            for J in 1 .. Last_AI_Option loop
               Put (' ');

               if J = Last_AI_Option and then (not Verbose_Mode) then
                  Put (File_Name (AI_Options (J).all));

               else
                  Put (AI_Options (J).all);
               end if;
            end loop;

            New_Line;
         end if;

         Spawn
           (Archive_Indexer.all,
            AI_Options (1 .. Last_AI_Option),
            Success);

         if not Success then
            Osint.Fail
              ("call to archive indexer " & Archive_Indexer.all & " failed");
         end if;
      end if;

   else
      --  Shared libraries

      Library_Path_Name :=
        new String'
          (Library_Directory.all &
           Directory_Separator &
           Shared_Lib_Prefix.all &
           Library_Name.all &
           Shared_Lib_Suffix.all);

      if Relocatable and then
         PIC_Option /= null and then
         PIC_Option.all /= ""
      then
         Options_Table.Append (new String'(PIC_Option.all));
      end if;

      --  Get default search directories to locate system.ads when calling
      --  Targparm.Get_Target_Parameters.

      --  Osint.Add_Default_Search_Dirs;

      --  Check if the platform is VMS and, if it is, change some variables

      --  Targparm.Get_Target_Parameters;

      Prj.Initialize (Prj.No_Project_Tree);

      if S_Osinte_Ads = No_File then
         Name_Len := 0;
         Add_Str_To_Name_Buffer ("s-osinte.ads");
         S_Osinte_Ads := Name_Find;
      end if;

      if S_Dec_Ads = No_File then
         Name_Len := 0;
         Add_Str_To_Name_Buffer ("dec.ads");
         S_Dec_Ads := Name_Find;
      end if;

      if G_Trasym_Ads = No_File then
         Name_Len := 0;
         Add_Str_To_Name_Buffer ("g-trasym.ads");
         G_Trasym_Ads := Name_Find;
      end if;

      for J in 1 .. Imported_Library_Directories.Last loop
         Library_Switches_Table.Append
           (new String'
              ("-L" & Imported_Library_Directories.Table (J).all));

         if Path_Option /= null then
            Add_Rpath (Imported_Library_Directories.Table (J));
         end if;

         Library_Switches_Table.Append
           (new String'
              ("-l" & Imported_Library_Names.Table (J).all));
      end loop;

      --  If Ada is used and we don't already know yet that libgnarl is needed,
      --  look for s-osinte.ads in all the ALI files. If found in at least one,
      --  then libgnarl is needed.

      if Runtime_Library_Dir /= null and then not Libgnarl_Needed then
         declare
            Lib_File : File_Name_Type;
            Text     : Text_Buffer_Ptr;
            Id       : ALI.ALI_Id;

         begin
            ALI_Loop :
            for Index in 1 .. ALIs.Last loop
               Name_Len := 0;
               Add_Str_To_Name_Buffer (ALIs.Table (Index).all);
               Lib_File := Name_Find;
               Text := Osint.Read_Library_Info (Lib_File, True);

               Id  := ALI.Scan_ALI
                 (F          => Lib_File,
                  T          => Text,
                  Ignore_ED  => False,
                  Err        => True,
                  Read_Lines => "D");
               Free (Text);

               --  Look for s-osinte.ads in the dependencies

               for Index in ALI.ALIs.Table (Id).First_Sdep ..
                 ALI.ALIs.Table (Id).Last_Sdep
               loop
                  if ALI.Sdep.Table (Index).Sfile = S_Osinte_Ads then
                     Libgnarl_Needed := True;
                     exit ALI_Loop;
                  end if;
               end loop;
            end loop ALI_Loop;
         end;
      end if;

      if Runtime_Library_Dir /= null then
         Options_Table.Append
           (new String'("-L" & Runtime_Library_Dir.all));

         if Path_Option /= null then
            Add_Rpath (Runtime_Library_Dir);
         end if;

         if Libgnarl_Needed then
            Options_Table.Append (Libgnarl);
         end if;

         Options_Table.Append (Libgnat);
      end if;

      if Path_Option /= null and then Rpath /= null then
         if Separate_Run_Path_Options then
            for J in 1 .. Rpath_Last loop
               Options_Table.Append
                 (new String'(Path_Option.all & Rpath (J).all));
            end loop;

         else
            declare
               Option : constant String_Access :=
                 new String (1 .. Path_Option'Length + Rpath_Length);
               Cur    : Natural := 0;

            begin
               Option (Cur + 1 .. Cur + Path_Option'Length) := Path_Option.all;
               Cur := Cur + Path_Option'Length;
               Option (Cur + 1 .. Cur + Rpath (1)'Length) := Rpath (1).all;
               Cur := Cur + Rpath (1)'Length;

               for J in 2 .. Rpath_Last loop
                  Cur := Cur + 1;
                  Option (Cur) := Path_Separator;
                  Option (Cur + 1 .. Cur + Rpath (J)'Length) := Rpath (J).all;
                  Cur := Cur + Rpath (J)'Length;
               end loop;

               Options_Table.Append (Option);
            end;
         end if;
      end if;

      Build_Shared_Lib;
   end if;

   if ALIs.Last /= 0 then
      Copy_ALI_Files;
   end if;

   if Copy_Source_Directory /= null then
      Copy_Sources;
   end if;

   --  Create new exchange files with the path of the library file and the
   --  paths of the object files with their time stamps.

   begin
      Create (IO_File, Out_File, Exchange_File_Name.all);
   exception
      when others =>
         Osint.Fail ("could not create " & Exchange_File_Name.all);
   end;

   Put_Line (IO_File, Library_Label (Library_Path));
   Put_Line (IO_File, Library_Path_Name.all);

   Put_Line (IO_File, Library_Label (Gprexch.Object_Files));

   for Index in 1 .. Last_Object_File_Index loop
      Put_Line (IO_File, Object_Files.Table (Index).all);

      Name_Len := Object_Files.Table (Index)'Length;
      Name_Buffer (1 .. Name_Len) := Object_Files.Table (Index).all;
      Put_Line
        (IO_File,
         String (Osint.File_Stamp (Path_Name_Type'(Name_Find))));
   end loop;

   if Generated_Sources.Last > 0 then
      Put_Line (IO_File, Library_Label (Gprexch.Generated_Source_Files));

      for Index in 1 .. Generated_Sources.Last loop
         Put_Line (IO_File, Generated_Sources.Table (Index).all);
      end loop;
   end if;

   if Generated_Objects.Last > 0 then
      Put_Line (IO_File, Library_Label (Gprexch.Generated_Object_Files));

      for Index in 1 .. Generated_Objects.Last loop
         Put_Line (IO_File, Generated_Objects.Table (Index).all);
      end loop;
   end if;

   Close (IO_File);
end Gprlib;
