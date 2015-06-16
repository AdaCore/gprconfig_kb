------------------------------------------------------------------------------
--                                                                          --
--                           GPR PROJECT MANAGER                            --
--                                                                          --
--                     Copyright (C) 2001-2015, AdaCore                     --
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

with Ada.Command_Line;          use Ada.Command_Line;
with Ada.Text_IO;               use Ada.Text_IO;

with GNAT.Command_Line;         use GNAT.Command_Line;
with GNAT.Dynamic_Tables;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Table;

with GPR.Opt;
with GPR.Osint;   use GPR.Osint;
with GPR.Util;    use GPR.Util;

with Gpr_Util;    use Gpr_Util;
with GPR_Version; use GPR_Version;

with System.Regexp;    use System.Regexp;

procedure GPRName.Main is

   Flags   : constant Processing_Flags :=
                Create_Flags
                  (Report_Error               => null,
                   When_No_Sources            => Error,
                  Require_Sources_Other_Lang => False,
                  Allow_Duplicate_Basenames  => False,
                  Compiler_Driver_Mandatory  => False,
                  Error_On_Unknown_Language  => False,
                  Require_Obj_Dirs           => Error,
                  Allow_Invalid_External     => Error,
                  Missing_Source_Files       => Error,
                  Ignore_Missing_With        => False);

   Subdirs_Switch : constant String := "--subdirs=";

   Usage_Output : Boolean := False;
   --  Set to True when usage is output, to avoid multiple output

   Usage_Needed : Boolean := False;
   --  Set to True by -h switch

   Version_Output : Boolean := False;
   --  Set to True when version is output, to avoid multiple output

   Very_Verbose : Boolean := False;
   --  Set to True with -v -v

   File_Path : String_Access := null;
   --  Path name of the file specified -P switch

   File_Set : Boolean := False;
   --  Set to True by -P switch.
   --  Used to detect multiple -P switches.

   package Patterns is new GNAT.Dynamic_Tables
     (Table_Component_Type => String_Access,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 0,
      Table_Initial        => 10,
      Table_Increment      => 100);
   --  Table to accumulate the patterns

   type Argument_Data is record
      Directories       : Patterns.Instance;
      Name_Patterns     : Patterns.Instance;
      Excluded_Patterns : Patterns.Instance;
      Foreign_Patterns  : Patterns.Instance;
   end record;

   package Arguments is new GNAT.Table
     (Table_Component_Type => Argument_Data,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 0,
      Table_Initial        => 10,
      Table_Increment      => 100);
   --  Table to accumulate directories and patterns

   package Preprocessor_Switches is new GNAT.Table
     (Table_Component_Type => String_Access,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 0,
      Table_Initial        => 10,
      Table_Increment      => 100);
   --  Table to store the preprocessor switches to be used in the call
   --  to the compiler.

   procedure Output_Version;
   --  Print name and version

   procedure Usage;
   --  Print usage

   procedure Scan_Args;
   --  Scan the command line arguments

   procedure Add_Source_Directory (S : String);
   --  Add S in the Source_Directories table

   procedure Get_Directories (From_File : String);
   --  Read a source directory text file

   --------------------------
   -- Add_Source_Directory --
   --------------------------

   procedure Add_Source_Directory (S : String) is
   begin
      Patterns.Append
        (Arguments.Table (Arguments.Last).Directories, new String'(S));
   end Add_Source_Directory;

   ---------------------
   -- Get_Directories --
   ---------------------

   procedure Get_Directories (From_File : String) is
      File : Ada.Text_IO.File_Type;
      Line : String (1 .. 2_000);
      Last : Natural;

   begin
      Open (File, In_File, From_File);

      while not End_Of_File (File) loop
         Get_Line (File, Line, Last);

         if Last /= 0 then
            Add_Source_Directory (Line (1 .. Last));
         end if;
      end loop;

      Close (File);

   exception
      when Name_Error =>
         Fail ("cannot open source directory file """ & From_File & '"');
   end Get_Directories;

   --------------------
   -- Output_Version --
   --------------------

   procedure Output_Version is
   begin
      if not Version_Output then
         Version_Output := True;
         New_Line;
         Display_Version
           ("GPRNAME", "2001", Version_String => Gpr_Version_String);
      end if;
   end Output_Version;

   ---------------
   -- Scan_Args --
   ---------------

   procedure Scan_Args is

      procedure Check_Version_And_Help is new Check_Version_And_Help_G (Usage);

      Project_File_Name_Expected : Boolean;

      Directory_Expected : Boolean;

      Dir_File_Name_Expected : Boolean;

      Foreign_Pattern_Expected : Boolean;

      Excluded_Pattern_Expected : Boolean;

      procedure Check_Regular_Expression (S : String);
      --  Compile string S into a Regexp, fail if any error

      -----------------------------
      -- Check_Regular_Expression--
      -----------------------------

      procedure Check_Regular_Expression (S : String) is
         Dummy : Regexp;
         pragma Warnings (Off, Dummy);
      begin
         Dummy := Compile (S, Glob => True);
      exception
         when Error_In_Regexp =>
            Fail ("invalid regular expression """ & S & """");
      end Check_Regular_Expression;

   --  Start of processing for Scan_Args

   begin
      --  First check for --version or --help

      Check_Version_And_Help
        ("GPRNAME", "2001", Version_String => Gpr_Version_String);

      --  Now scan the other switches

      Project_File_Name_Expected := False;
      Directory_Expected         := False;
      Dir_File_Name_Expected     := False;
      Foreign_Pattern_Expected   := False;
      Excluded_Pattern_Expected  := False;

      for Next_Arg in 1 .. Argument_Count loop
         declare
            Next_Argv : constant String := Argument (Next_Arg);
            Arg       : String (1 .. Next_Argv'Length) := Next_Argv;

         begin
            if Arg'Length > 0 then

               --  -P xxx

               if Project_File_Name_Expected then
                  if Arg (1) = '-' then
                     Fail ("project file name missing");

                  else
                     File_Set       := True;
                     File_Path      := new String'(Arg);
                     Project_File_Name_Expected := False;
                  end if;

               --  -d xxx

               elsif Directory_Expected then
                  Add_Source_Directory (Arg);
                  Directory_Expected := False;

               --  -D xxx

               elsif Dir_File_Name_Expected then
                  Get_Directories (Arg);
                  Dir_File_Name_Expected := False;

               --  -f xxx

               elsif Foreign_Pattern_Expected then
                  Patterns.Append
                    (Arguments.Table (Arguments.Last).Foreign_Patterns,
                     new String'(Arg));
                  Check_Regular_Expression (Arg);
                  Foreign_Pattern_Expected := False;

               --  -x xxx

               elsif Excluded_Pattern_Expected then
                  Patterns.Append
                    (Arguments.Table (Arguments.Last).Excluded_Patterns,
                     new String'(Arg));
                  Check_Regular_Expression (Arg);
                  Excluded_Pattern_Expected := False;

               --  There must be at least one Ada pattern or one foreign
               --  pattern for the previous section.

               --  --and

               elsif Arg = "--and" then

                  if Patterns.Last
                    (Arguments.Table (Arguments.Last).Name_Patterns) = 0
                    and then
                      Patterns.Last
                        (Arguments.Table (Arguments.Last).Foreign_Patterns) = 0
                  then
                     Try_Help;
                     return;
                  end if;

                  --  If no directory were specified for the previous section,
                  --  then the directory is the project directory.

                  if Patterns.Last
                    (Arguments.Table (Arguments.Last).Directories) = 0
                  then
                     Patterns.Append
                       (Arguments.Table (Arguments.Last).Directories,
                        new String'("."));
                  end if;

                  --  Add and initialize another component to Arguments table

                  declare
                     New_Arguments : Argument_Data;
                     pragma Warnings (Off, New_Arguments);
                     --  Declaring this defaulted initialized object ensures
                     --  that the new allocated component of table Arguments
                     --  is correctly initialized.

                     --  This is VERY ugly, Table should never be used with
                     --  data requiring default initialization. We should
                     --  find a way to avoid violating this rule ???

                  begin
                     Arguments.Append (New_Arguments);
                  end;

                  Patterns.Init
                    (Arguments.Table (Arguments.Last).Directories);
                  Patterns.Set_Last
                    (Arguments.Table (Arguments.Last).Directories, 0);
                  Patterns.Init
                    (Arguments.Table (Arguments.Last).Name_Patterns);
                  Patterns.Set_Last
                    (Arguments.Table (Arguments.Last).Name_Patterns, 0);
                  Patterns.Init
                    (Arguments.Table (Arguments.Last).Excluded_Patterns);
                  Patterns.Set_Last
                    (Arguments.Table (Arguments.Last).Excluded_Patterns, 0);
                  Patterns.Init
                    (Arguments.Table (Arguments.Last).Foreign_Patterns);
                  Patterns.Set_Last
                    (Arguments.Table (Arguments.Last).Foreign_Patterns, 0);

               --  Subdirectory switch

               elsif Arg'Length > Subdirs_Switch'Length
                 and then Arg (1 .. Subdirs_Switch'Length) = Subdirs_Switch
               then
                  Subdirs :=
                    new String'(Arg (Subdirs_Switch'Length + 1 .. Arg'Last));

               --  --no-backup

               elsif Arg = "--no-backup" then
                  Opt.No_Backup := True;

               --  -d

               elsif Arg'Length >= 2 and then Arg (1 .. 2) = "-d" then
                  if Arg'Length = 2 then
                     Directory_Expected := True;

                     if Next_Arg = Argument_Count then
                        Fail ("directory name missing");
                     end if;

                  else
                     Add_Source_Directory (Arg (3 .. Arg'Last));
                  end if;

               --  -D

               elsif Arg'Length >= 2 and then Arg (1 .. 2) = "-D" then
                  if Arg'Length = 2 then
                     Dir_File_Name_Expected := True;

                     if Next_Arg = Argument_Count then
                        Fail ("directory list file name missing");
                     end if;

                  else
                     Get_Directories (Arg (3 .. Arg'Last));
                  end if;

               --  -eL

               elsif Arg = "-eL" then
                  Opt.Follow_Links_For_Files := True;
                  Opt.Follow_Links_For_Dirs  := True;

               --  -f

               elsif Arg'Length >= 2 and then Arg (1 .. 2) = "-f" then
                  if Arg'Length = 2 then
                     Foreign_Pattern_Expected := True;

                     if Next_Arg = Argument_Count then
                        Fail ("foreign pattern missing");
                     end if;

                  else
                     Patterns.Append
                       (Arguments.Table (Arguments.Last).Foreign_Patterns,
                        new String'(Arg (3 .. Arg'Last)));
                     Check_Regular_Expression (Arg (3 .. Arg'Last));
                  end if;

               --  -gnatep or -gnateD

               elsif Arg'Length > 7 and then
                 (Arg  (1 .. 7) = "-gnatep" or else Arg (1 .. 7) = "-gnateD")
               then
                  Preprocessor_Switches.Append (new String'(Arg));

               --  -h

               elsif Arg = "-h" then
                  Usage_Needed := True;

               --  -p

               elsif Arg'Length >= 2 and then Arg (1 .. 2) = "-P" then
                  if File_Set then
                     Fail ("only one -c or -P switch may be specified");
                  end if;

                  if Arg'Length = 2 then
                     if Next_Arg = Argument_Count then
                        Fail ("project file name missing");

                     else
                        Project_File_Name_Expected := True;
                     end if;

                  else
                     File_Set       := True;
                     File_Path      := new String'(Arg (3 .. Arg'Last));
                  end if;

               --  -v

               elsif Arg = "-v" then
                  if Opt.Verbose_Mode then
                     Very_Verbose := True;
                  else
                     Opt.Verbose_Mode := True;
                  end if;

               --  -x

               elsif Arg'Length >= 2 and then Arg (1 .. 2) = "-x" then
                  if Arg'Length = 2 then
                     Excluded_Pattern_Expected := True;

                     if Next_Arg = Argument_Count then
                        Fail ("excluded pattern missing");
                     end if;

                  else
                     Patterns.Append
                       (Arguments.Table (Arguments.Last).Excluded_Patterns,
                        new String'(Arg (3 .. Arg'Last)));
                     Check_Regular_Expression (Arg (3 .. Arg'Last));
                  end if;

               --  Junk switch starting with minus

               elsif Arg (1) = '-' then
                  Fail ("wrong switch: " & Arg);

               --  Not a recognized switch, assume file name

               else
                  Canonical_Case_File_Name (Arg);
                  Patterns.Append
                    (Arguments.Table (Arguments.Last).Name_Patterns,
                     new String'(Arg));
                  Check_Regular_Expression (Arg);
               end if;
            end if;
         end;
      end loop;
   end Scan_Args;

   -----------
   -- Usage --
   -----------

   procedure Usage is
   begin
      if not Usage_Output then
         Usage_Needed := False;
         Usage_Output := True;
         Put_Line
           ("Usage: gprname  [switches] naming-pattern [naming-patterns]");
         Put_Line ("   {--and [switches] naming-pattern [naming-patterns]}");
         New_Line;
         Put_Line ("switches:");

         Display_Usage_Version_And_Help;

         Put_Line ("  --subdirs=dir real obj/lib/exec dirs are subdirs");
         Put_Line ("  --no-backup   do not create backup of project file");
         New_Line;

         Put_Line ("  --and        use different patterns");
         New_Line;

         Put_Line ("  -ddir        use dir as one of the source " &
                     "directories");
         Put_Line ("  -Dfile       get source directories from file");
         Put_Line ("  -eL          follow symbolic links when processing " &
                     "project files");
         Put_Line ("  -fpat        foreign pattern");
         Put_Line ("  -gnateDsym=v preprocess with symbol definition");
         Put_Line ("  -gnatep=data preprocess files with data file");
         Put_Line ("  -h           output this help message");
         Put_Line ("  -Pproj       update or create project file proj");
         Put_Line ("  -v           verbose output");
         Put_Line ("  -v -v        very verbose output");
         Put_Line ("  -xpat        exclude pattern pat");
      end if;
   end Usage;

--  Start of processing for Gnatname

begin
   Set_Program_Name ("gprname");

   --  Initialize tables

   Arguments.Set_Last (0);
   declare
      New_Arguments : Argument_Data;
      pragma Warnings (Off, New_Arguments);
      --  Declaring this defaulted initialized object ensures that the new
      --  allocated component of table Arguments is correctly initialized.
   begin
      Arguments.Append (New_Arguments);
   end;

   Patterns.Init (Arguments.Table (1).Directories);
   Patterns.Set_Last (Arguments.Table (1).Directories, 0);
   Patterns.Init (Arguments.Table (1).Name_Patterns);
   Patterns.Set_Last (Arguments.Table (1).Name_Patterns, 0);
   Patterns.Init (Arguments.Table (1).Excluded_Patterns);
   Patterns.Set_Last (Arguments.Table (1).Excluded_Patterns, 0);
   Patterns.Init (Arguments.Table (1).Foreign_Patterns);
   Patterns.Set_Last (Arguments.Table (1).Foreign_Patterns, 0);

   Preprocessor_Switches.Set_Last (0);

   --  Get the arguments

   Scan_Args;

   if File_Path = null then
      Try_Help;
      Fail_Program (null, "no project file specified");
   end if;

   if Opt.Verbose_Mode then
      Output_Version;
   end if;

   if Usage_Needed then
      Usage;
   end if;

   --  If no Ada or foreign pattern was specified, print the usage and return

   if Patterns.Last (Arguments.Table (Arguments.Last).Name_Patterns) = 0
      and then
      Patterns.Last (Arguments.Table (Arguments.Last).Foreign_Patterns) = 0
   then
      if Argument_Count = 0 then
         Usage;
      elsif not Usage_Output then
         Try_Help;
      end if;

      return;
   end if;

   --  If no source directory was specified, use the current directory as the
   --  unique directory. Note that if a file was specified with directory
   --  information, the current directory is the directory of the specified
   --  file.

   if Patterns.Last
     (Arguments.Table (Arguments.Last).Directories) = 0
   then
      Patterns.Append
        (Arguments.Table (Arguments.Last).Directories, new String'("."));
   end if;

   --  Initialize

   declare
      Prep_Switches : Argument_List
                        (1 .. Integer (Preprocessor_Switches.Last));

   begin
      for Index in Prep_Switches'Range loop
         Prep_Switches (Index) := Preprocessor_Switches.Table (Index);
      end loop;

      Initialize
        (File_Path         => File_Path.all,
         Preproc_Switches  => Prep_Switches,
         Very_Verbose      => Very_Verbose,
         Flags             => Flags);
   end;

   --  Process each section successively

   for J in 1 .. Arguments.Last loop
      declare
         Directories   : Argument_List
           (1 .. Integer
                   (Patterns.Last (Arguments.Table (J).Directories)));
         Name_Patterns : Regexp_List
           (1 .. Integer
                   (Patterns.Last (Arguments.Table (J).Name_Patterns)));
         Excl_Patterns : Regexp_List
           (1 .. Integer
                   (Patterns.Last (Arguments.Table (J).Excluded_Patterns)));
         Frgn_Patterns : Regexp_List
           (1 .. Integer
                   (Patterns.Last (Arguments.Table (J).Foreign_Patterns)));

      begin
         --  Build the Directories and Patterns arguments

         for Index in Directories'Range loop
            Directories (Index) :=
              Arguments.Table (J).Directories.Table (Index);
         end loop;

         for Index in Name_Patterns'Range loop
            Name_Patterns (Index) :=
              Compile
                (Arguments.Table (J).Name_Patterns.Table (Index).all,
                 Glob => True);
         end loop;

         for Index in Excl_Patterns'Range loop
            Excl_Patterns (Index) :=
              Compile
                (Arguments.Table (J).Excluded_Patterns.Table (Index).all,
                 Glob => True);
         end loop;

         for Index in Frgn_Patterns'Range loop
            Frgn_Patterns (Index) :=
              Compile
                (Arguments.Table (J).Foreign_Patterns.Table (Index).all,
                 Glob => True);
         end loop;

         --  Call Process where the real work is done

         Process
           (Directories       => Directories,
            Name_Patterns     => Name_Patterns,
            Excluded_Patterns => Excl_Patterns,
            Foreign_Patterns  => Frgn_Patterns);
      end;
   end loop;

   --  Finalize

   Finalize;

   if Opt.Verbose_Mode then
      New_Line;
   end if;
end GPRName.Main;
