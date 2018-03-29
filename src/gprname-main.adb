------------------------------------------------------------------------------
--                                                                          --
--                           GPR PROJECT MANAGER                            --
--                                                                          --
--                     Copyright (C) 2001-2018, AdaCore                     --
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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Command_Line;        use Ada.Command_Line;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Vectors;
with Ada.Exceptions;          use Ada.Exceptions;
with Ada.Text_IO;             use Ada.Text_IO;

with GNAT.Command_Line;   use GNAT.Command_Line;
with GNAT.OS_Lib;         use GNAT.OS_Lib;

with GPR;
with GPR.Conf;
with GPR.Env;
with GPR.Names;          use GPR.Names;
with GPR.Opt;
with GPR.Osint;          use GPR.Osint;
with GPR.Snames;         use GPR.Snames;
with GPR.Tree;           use GPR.Tree;
with GPR.Util;           use GPR.Util;
with GPR.Version;        use GPR.Version;

with Gpr_Build_Util; use Gpr_Build_Util;

with System.Regexp; use System.Regexp;

procedure GPRName.Main is

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

   Project_File_Name_Expected : Boolean := False;
   --  True when switch "-P" has just been scanned

   Directory_Expected : Boolean := False;
   --  True when switch "-d" has just been scanned

   Dir_File_Name_Expected : Boolean := False;
   --  True when switch "-D" has just been scanned

   Foreign_Pattern_Expected : Boolean := False;
   --  True when switch "-f" has just been scanned

   Foreign_Language : Name_Id := No_Name;

   Excluded_Pattern_Expected : Boolean := False;
   --  True when switch "-x" has just been scanned

   type Foreign_Pattern (Ptrn_Len : Natural) is record
      Language : Name_Id := No_Name;
      Pattern  : String (1 .. Ptrn_Len);
   end record;

   package Foreign_Patterns is new Ada.Containers.Indefinite_Vectors
     (Positive, Foreign_Pattern);
   --  Table to accumulate the patterns for non Ada sources

   type Argument_Data is record
      Directories               : String_Vectors.Vector;
      Name_Patterns             : String_Vectors.Vector;
      Excluded_Patterns         : String_Vectors.Vector;
      Foreign_Sources_Patterns  : Foreign_Patterns.Vector;
   end record;

   package Argument_Data_Vectors is new Ada.Containers.Vectors
     (Positive, Argument_Data);

   Arguments : Argument_Data_Vectors.Vector;
   --  Table to accumulate directories and patterns

   Preprocessor_Switches : String_Vectors.Vector;
   --  Table to store the preprocessor switches to be used in the call
   --  to the compiler.

   procedure Add_Source_Directory (S : String);
   --  Add S in the Source_Directories table

   procedure Check_Regular_Expression (S : String);
   --  Compile string S into a Regexp, fail if any error

   procedure Get_Directories (From_File : String);
   --  Read a source directory text file

   procedure Initialize;
   --  Do the necessary package intialization and process the command line
   --  arguments.

   procedure Output_Version;
   --  Print name and version

   procedure Scan_Arg (Arg : String);
   --  Process on of the command line argument

   procedure Usage;
   --  Print usage

   --------------------------
   -- Add_Source_Directory --
   --------------------------

   procedure Add_Source_Directory (S : String)
   is
      procedure Update (List : in out Argument_Data);

      procedure Update (List : in out Argument_Data)
      is
      begin
         List.Directories.Append (S);
      end Update;

   begin
      Argument_Data_Vectors.Update_Element
        (Arguments, Arguments.Last_Index, Update'Access);
   end Add_Source_Directory;

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

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is

      procedure Check_Version_And_Help is new Check_Version_And_Help_G (Usage);

      User_Project_Node : Project_Node_Id;
      --  Used to call Parse_Project_And_Apply_Config

   begin
      --  Do some necessary package initializations

      GPR.Snames.Initialize;

      Set_Program_Name ("gprname");

      GPR.Tree.Initialize (Root_Environment, Gprname_Flags);
      GPR.Tree.Initialize (Project_Node_Tree);

      GPR.Initialize (Project_Tree);

      --  Initialize tables

      Arguments.Clear;
      declare
         New_Arguments : Argument_Data;
         pragma Warnings (Off, New_Arguments);
         --  Declaring this defaulted initialized object ensures that the new
         --  allocated component of table Arguments is correctly initialized.
      begin
         Arguments.Append (New_Arguments);
      end;

      Preprocessor_Switches.Clear;

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
            Arg       : constant String (1 .. Next_Argv'Length) := Next_Argv;

         begin
            Scan_Arg (Arg);
         end;
      end loop;

      if Project_File_Name_Expected or else not File_Set then
         Fail ("project file name missing");

      elsif File_Path = null then
         Try_Help;
         Fail_Program (null, "no project file specified");

      elsif Directory_Expected then
         Fail ("directory name missing");

      elsif Dir_File_Name_Expected then
         Fail ("directory list file name missing");

      elsif Foreign_Pattern_Expected then
         Fail ("foreign pattern missing");

      elsif Excluded_Pattern_Expected then
         Fail ("excluded pattern missing");
      end if;

      GPR.Env.Initialize_Default_Project_Path
        (Root_Environment.Project_Path, Target_Name => "-");

      if Load_Standard_Base then
         --  We need to parse the knowledge base so that we are able to
         --  normalize the target names. Unfortunately, if we have to spawn
         --  gprconfig, it will also have to parse that knowledge base on
         --  its own.
         Knowledge.Parse_Knowledge_Base (Project_Tree);
      end if;

      if Target_Name = null then
         Target_Name := new String'("");
      end if;

      if Config_Project_File_Name = null then
         Config_Project_File_Name := new String'("");
      end if;

      --  Check if the project file already exists

      declare
         Path_Name : String
                       (1 .. File_Path'Length + Project_File_Extension'Length);
         Path_Last : Positive := File_Path'Length;

      begin
         if File_Names_Case_Sensitive then
            Path_Name (1 .. Path_Last) := File_Path.all;
         else
            Path_Name (1 .. Path_Last) := To_Lower (File_Path.all);
         end if;

         Path_Name (Path_Last + 1 .. Path_Name'Last) :=
           Project_File_Extension;

         if Path_Last < Project_File_Extension'Length + 1
           or else Path_Name
           (Path_Last - Project_File_Extension'Length + 1 .. Path_Last)
           /= Project_File_Extension
         then
            Path_Last := Path_Name'Last;
         end if;

         File_Path := new String'(Path_Name (1 .. Path_Last));
      end;

      if Is_Regular_File (File_Path.all) then
         if Opt.Verbose_Mode then
            Put_Line
              ("Parsing already existing project file """ &
               File_Path.all & "");
         end if;

      else
         --  The project file does not exist; create an empty one

         declare
            File : File_Type;
            File_Name_Start : Positive := File_Path'First;
            File_Name_Last : constant Positive :=
                               File_Path'Last - Project_File_Extension'Length;

         begin
            for J in reverse File_Path'Range loop
               if File_Path (J) = Directory_Separator then
                  File_Name_Start := J + 1;
                  exit;
               end if;
            end loop;

            Create (File, Out_File, File_Path.all);
            Put (File, "project ");
            Put (File, File_Path (File_Name_Start .. File_Name_Last));
            Put_Line (File, " is");
            Put (File, "end ");
            Put (File, File_Path (File_Name_Start .. File_Name_Last));
            Put_Line (File, ";");
            Close (File);

         exception
            when others =>
               Fail ("could not create project file " & File_Path.all);
         end;
      end if;

      begin
         GPR.Opt.Warning_Mode  := GPR.Opt.Suppress;
         GPR.Conf.Parse_Project_And_Apply_Config
           (Main_Project               => Main_Project,
            User_Project_Node          => User_Project_Node,
            Config_File_Name           => Config_Project_File_Name.all,
            Autoconf_Specified         => Autoconf_Specified,
            Project_File_Name          => File_Path.all,
            Project_Tree               => Project_Tree,
            Env                        => Root_Environment,
            Project_Node_Tree          => Project_Node_Tree,
            Packages_To_Check          => Packages_To_Check,
            Allow_Automatic_Generation => Autoconfiguration,
            Automatically_Generated    => Delete_Autoconf_File,
            Config_File_Path           => Configuration_Project_Path,
            Target_Name                => Target_Name.all,
            Normalized_Hostname        => Knowledge.Normalized_Hostname);
      exception
         when E : GPR.Conf.Invalid_Config =>
            Fail_Program (Project_Tree, Exception_Message (E));
      end;

      if Main_Project = No_Project then
         --  Don't flush messages in case of parsing error. This has already
         --  been taken care when parsing the tree. Otherwise, it results in
         --  the same message being displayed twice.

         Fail_Program
           (Project_Tree,
            """" & File_Path.all & """ processing failed",
            Flush_Messages => User_Project_Node /= Empty_Project_Node);

      else
         declare
            Ada_Lang : constant Language_Ptr :=
              Get_Language_From_Name (Main_Project, "ada");
         begin
            if Ada_Lang /= No_Language_Index then
               Gcc_Path := Get_Compiler_Driver_Path (Project_Tree, Ada_Lang);
            end if;
         end;
      end if;
   end Initialize;

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

   --------------
   -- Scan_Arg --
   --------------

   procedure Scan_Arg (Arg : String)
   is
      pragma Assert (Arg'First = 1);
      procedure Add_Foreign_Source   (Argument : in out Argument_Data);

      ------------------------
      -- Add_Foreign_Source --
      ------------------------

      procedure Add_Foreign_Source (Argument : in out Argument_Data)
      is
      begin
         Argument.Foreign_Sources_Patterns.Append
           (Foreign_Pattern'
              (Ptrn_Len => Arg'Length,
               Language => Foreign_Language,
               Pattern  => Arg));
      end Add_Foreign_Source;

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
            Arguments.Update_Element
              (Arguments.Last_Index,
               Add_Foreign_Source'Access);
            Check_Regular_Expression (Arg);
            Foreign_Pattern_Expected := False;

         --  -x xxx

         elsif Excluded_Pattern_Expected then
            Arguments.Reference
              (Arguments.Last).Element.Excluded_Patterns.Append (Arg);
            Check_Regular_Expression (Arg);
            Excluded_Pattern_Expected := False;

         --  There must be at least one Ada pattern or one foreign pattern for
         --  the previous section.

         --  --and

         elsif Arg = "--and" then

            if Arguments.Last_Element.Name_Patterns.Is_Empty
              and then Arguments.Last_Element.Foreign_Sources_Patterns.Is_Empty
            then
               Try_Help;
               return;
            end if;

            --  If no directory were specified for the previous section, then
            --  the directory is the project directory.

            if Arguments.Last_Element.Directories.Is_Empty then
               Arguments.Reference
                 (Arguments.Last).Element.Directories.Append (".");
            end if;

            --  Add and initialize another component to Arguments table

            declare
               New_Arguments : Argument_Data;
               pragma Warnings (Off, New_Arguments);
               --  Declaring this defaulted initialized object ensures that
               --  the new allocated component of table Arguments is correctly
               --  initialized.

            begin
               Arguments.Append (New_Arguments);
            end;

         --  --subdir=

         elsif Arg'Length > Subdirs_Switch'Length
           and then Arg (1 .. Subdirs_Switch'Length) = Subdirs_Switch
         then
            Subdirs :=
              new String'(Arg (Subdirs_Switch'Length + 1 .. Arg'Last));

         --  --ignore-predefined-units

         elsif Arg = "--ignore-predefined-units" then
            Opt.Ignore_Predefined_Units := True;

         --  --ignore-duplicate-files

         elsif Arg = "--ignore-duplicate-files" then
            Opt.Ignore_Duplicate_Files := True;

         --  --no-backup

         elsif Arg = "--no-backup" then
            Opt.No_Backup := True;

         --  --target=

         elsif Arg'Length > Target_Project_Option'Length and then
               Arg (1 .. Target_Project_Option'Length) = Target_Project_Option
         then
            if Target_Name = null then
               Target_Name :=
                 new String'
                   (Arg (Target_Project_Option'Length + 1 .. Arg'Last));

            elsif Target_Name.all /=
                  Arg (Target_Project_Option'Length + 1 .. Arg'Last)
            then
               Fail ("multiple targets");
            end if;

         --  --RTS=path

         elsif Arg'Length >= 5 and then Arg (1 .. 5) = "--RTS" then
            if Arg'Length <= 6 or else Arg (6) /= '='then
               Osint.Fail ("missing path for --RTS");

            else
               --  Check that it is the first time we see this switch or, if
               --  it is not the first time, the same path is specified.

               if RTS_Specified = null then
                  RTS_Specified := new String'(Arg (7 .. Arg'Last));
                  GPR.Conf.Set_Runtime_For
                    (Snames.Name_Ada, Arg (7 .. Arg'Last));

               elsif RTS_Specified.all /= Arg (7 .. Arg'Last) then
                  Osint.Fail ("--RTS cannot be specified multiple times");
               end if;
            end if;

         --  -d

         elsif Arg'Length >= 2 and then Arg (1 .. 2) = "-d" then
            if Arg'Length = 2 then
               Directory_Expected := True;

            else
               Add_Source_Directory (Arg (3 .. Arg'Last));
            end if;

         --  -D

         elsif Arg'Length >= 2 and then Arg (1 .. 2) = "-D" then
            if Arg'Length = 2 then
               Dir_File_Name_Expected := True;

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
               Foreign_Language := Name_C;

            elsif Arg (3) = ':' then
               if Arg'Length = 3 then
                  Fail ("wrong switch: " & Arg);

               else
                  Name_Len := Arg'Length - 3;
                  Name_Buffer (1 .. Name_Len) :=
                    To_Lower (Arg (4 .. Arg'Last));
                  Foreign_Language := Name_Find;
                  Foreign_Pattern_Expected := True;
               end if;

            else
               Arguments.Reference
                 (Arguments.Last).Element.Foreign_Sources_Patterns.Append
                 (Foreign_Pattern'
                    (Ptrn_Len => Arg'Length - 2,
                     Language => Name_C,
                     Pattern  => Arg (3 .. Arg'Last)));
               Check_Regular_Expression (Arg (3 .. Arg'Last));
            end if;

         --  -gnatep or -gnateD

         elsif Arg'Length > 7 and then
           (Arg  (1 .. 7) = "-gnatep" or else Arg (1 .. 7) = "-gnateD")
         then
            Preprocessor_Switches.Append (Arg);

         --  -h

         elsif Arg = "-h" then
            Usage_Needed := True;

         --  -P

         elsif Arg'Length >= 2 and then Arg (1 .. 2) = "-P" then
            if File_Set then
               Fail ("only one -P switch may be specified");
            end if;

            if Arg'Length = 2 then
               Project_File_Name_Expected := True;

            else
               File_Set       := True;
               File_Path      := new String'(Arg (3 .. Arg'Last));
            end if;

            --  -v

         elsif Arg = "-v" then
            if Opt.Verbose_Mode then
               Very_Verbose := True;
            else
               Opt.Verbose_Mode    := True;
               Opt.Verbosity_Level := Opt.High;
            end if;

            --  -vP?

         elsif Arg'Length = 4 and then
               Arg (1 .. 3) = "-vP" and then
               Arg (4) in '0' .. '2'
         then
            case Arg (4) is
               when '0' =>
                  Current_Verbosity := Default;

               when '1' =>
                  Current_Verbosity := Medium;

               when '2' =>
                  Current_Verbosity := High;

               when others =>
                  null;
            end case;
            --  -x

         elsif Arg'Length >= 2 and then Arg (1 .. 2) = "-x" then
            if Arg'Length = 2 then
               Excluded_Pattern_Expected := True;

            else
               Arguments.Reference
                 (Arguments.Last).Element.Excluded_Patterns.Append
                 (Arg (3 .. Arg'Last));
               Check_Regular_Expression (Arg (3 .. Arg'Last));
            end if;

            --  Junk switch starting with minus

         elsif Arg (1) = '-' then
            Fail ("wrong switch: " & Arg);

            --  Not a recognized switch, assume file name

         else
            declare
               File_Name : String := Arg;
            begin
               Canonical_Case_File_Name (File_Name);
               Arguments.Reference
                 (Arguments.Last).Element.Name_Patterns.Append (File_Name);
               Check_Regular_Expression (File_Name);
            end;
         end if;
      end if;
   end Scan_Arg;

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

         Put_Line
           ("  --target=<targ> indicates the target of the GNAT compiler");
         New_Line;
         Put_Line ("  --RTS=dir     specify the Ada runtime");
         Put_Line ("  --subdirs=dir real obj/lib/exec dirs are subdirs");
         Put_Line ("  --no-backup   do not create backup of project file");
         New_Line;
         Put_Line ("  --ignore-duplicate-files  ignore duplicate basenames");
         Put_Line ("  --ignore-predefined-units ignore predefined units");
         New_Line;

         Put_Line ("  --and        use different patterns");
         New_Line;

         Put_Line ("  -ddir        use dir as one of the source " &
                     "directories");
         Put_Line ("  -Dfile       get source directories from file");
         Put_Line ("  -eL          follow symbolic links when processing " &
                     "project files");
         Put_Line ("  -fpat        pattern for C source");
         Put_Line ("  -f:lang pat  pattern for source of language lang");
         Put_Line ("  -gnateDsym=v preprocess with symbol definition");
         Put_Line ("  -gnatep=data preprocess files with data file");
         Put_Line ("  -h           output this help message");
         Put_Line ("  -Pproj       update or create project file proj");
         Put_Line ("  -v           verbose output");
         Put_Line ("  -v -v        very verbose output");
         Put_Line ("  -vPx         " &
                   "Specify verbosity when parsing Project Files (x = 0/1/2)");
         Put_Line ("  -xpat        exclude pattern pat");
      end if;
   end Usage;

--  Start of processing for Gnatname

begin
   Initialize;

   if Opt.Verbose_Mode then
      Output_Version;
   end if;

   if Usage_Needed then
      Usage;
   end if;

   --  If no Ada or foreign pattern was specified, print the usage and return

   if Arguments.Last_Element.Name_Patterns.Is_Empty
     and then Arguments.Last_Element.Foreign_Sources_Patterns.Is_Empty
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

   if Arguments.Last_Element.Directories.Is_Empty then
      Arguments.Reference (Arguments.Last).Element.Directories.Append (".");
   end if;

   --  Initialize

   Initialize
     (File_Path         => File_Path.all,
      Preproc_Switches  => Preprocessor_Switches,
      Very_Verbose      => Very_Verbose,
      Flags             => Gprname_Flags);

   --  Process each section successively

   for Arg of Arguments loop
      declare
         Name_Patterns : Regexp_List;
         Excl_Patterns : Regexp_List;
         Frgn_Patterns : Foreign_Regexp_List;
      begin
         for Name of Arg.Name_Patterns loop
            Name_Patterns.Append (Compile (Name, Glob => True));
         end loop;
         for Excl of Arg.Excluded_Patterns loop
            Excl_Patterns.Append (Compile (Excl, Glob => True));
         end loop;
         for Frgn of Arg.Foreign_Sources_Patterns loop
            Frgn_Patterns.Append
              (Foreign_Regexp'
                 (Language => Frgn.Language,
                  Pattern  => Compile (Frgn.Pattern, Glob => True)));
         end loop;

         Process
           (Directories       => Arg.Directories,
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

   Finish_Program (Project_Tree);
end GPRName.Main;
