------------------------------------------------------------------------------
--                                                                          --
--                             GPR TECHNOLOGY                               --
--                                                                          --
--                     Copyright (C) 2012-2015, AdaCore                     --
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

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Directories;
with Ada.Exceptions;   use Ada.Exceptions;
with Ada.Text_IO;      use Ada.Text_IO;

with GNAT.Case_Util;            use GNAT.Case_Util;
with GNAT.Command_Line;         use GNAT.Command_Line;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;

with Gpr_Build_Util; use Gpr_Build_Util;
with Gpr_Util;       use Gpr_Util;
with GPR_Version;    use GPR_Version;
with GPR.Conf;       use GPR.Conf;
with GPR.Env;
with GPR.Err;
with GPR.Names;      use GPR.Names;
with GPR.Opt;        use GPR.Opt;
with GPR.Osint;      use GPR.Osint;
with GPR.Proc;       use GPR.Proc;
with GPR.Tree;       use GPR.Tree;
with GPR.Snames;     use GPR.Snames;
with GPR.Util;       use GPR.Util;
with Gprinstall.DB;
with Gprinstall.Install;
with Gprinstall.Uninstall;

procedure Gprinstall.Main is

   use Gpr_Util.Knowledge;

   --  Options specific to gprinstall

   Build_Var_Option       : constant String := "--build-var";
   No_Build_Var_Option    : constant String := "--no-build-var";
   Build_Name_Option      : constant String := "--build-name";
   Install_Name_Option    : constant String := "--install-name";
   Uninstall_Option       : constant String := "--uninstall";
   Mode_Option            : constant String := "--mode";
   Lib_Subdir_Option      : constant String := "--lib-subdir";
   Link_Lib_Subdir_Option : constant String := "--link-lib-subdir";
   Exec_Subdir_Option     : constant String := "--exec-subdir";
   Sources_Subdir_Option  : constant String := "--sources-subdir";
   Project_Subdir_Option  : constant String := "--project-subdir";
   No_Lib_Link_Option     : constant String := "--no-lib-link";
   List_Option            : constant String := "--list";
   Stat_Option            : constant String := "--stat";
   Sources_Only_Option    : constant String := "--sources-only";

   Opt_A_Set : Boolean := False; -- to detect if -a and -m are used together
   Opt_M_Set : Boolean := False; -- likewise

   procedure Initialize;
   --  Do the necessary package intialization and process the command line
   --  arguments.

   procedure Usage;
   --  Display the usage

   procedure Scan_Arg
     (Arg          : String;
      Command_Line : Boolean;
      Success      : out Boolean);
   --  Process one gprinstall argument Arg. Command_Line is True if the
   --  argument is specified on the command line. Optional parameter Additional
   --  gives additional information about the origin of the argument if it is
   --  found illegal.

   procedure Copyright;
   --  Output the Copyright notice

   type Sigint_Handler is access procedure;
   pragma Convention (C, Sigint_Handler);

   procedure Install_Int_Handler (Handler : Sigint_Handler);
   pragma Import (C, Install_Int_Handler, "__gnat_install_int_handler");
   --  Called by Gnatmake to install the SIGINT handler below

   procedure Sigint_Intercepted;
   pragma Convention (C, Sigint_Intercepted);
   --  Called when the program is interrupted by Ctrl-C to delete the
   --  temporary mapping files and configuration pragmas files.

   ---------------
   -- Copyright --
   ---------------

   procedure Copyright is
   begin
      --  Only output the Copyright notice once

      if not Copyright_Output then
         Copyright_Output := True;
         Display_Version
           ("GPRINSTALL", "2012", Version_String => Gpr_Version_String);
      end if;
   end Copyright;

   --------------
   -- Scan_Arg --
   --------------

   procedure Scan_Arg
     (Arg          : String;
      Command_Line : Boolean;
      Success      : out Boolean)
   is

      function Has_Prefix (Name : String) return Boolean;
      --  Returns True if Arg start with Name

      procedure Set_Param
        (P : in out Param; Name : String; Is_Dir : Boolean := True);
      --  Set P with value for option Name

      ----------------
      -- Has_Prefix --
      ----------------

      function Has_Prefix (Name : String) return Boolean is
      begin
         pragma Assert (Arg'First = 1);
         return Arg'Length >= Name'Length
           and then Arg (1 .. Name'Length) = Name;
      end Has_Prefix;

      ---------------
      -- Set_Param --
      ---------------

      procedure Set_Param
        (P : in out Param; Name : String; Is_Dir : Boolean := True)
      is
         Value : constant String := Arg (Name'Length + 2 .. Arg'Last);
      begin
         P := (new String'
                 ((if Is_Dir then Ensure_Directory (Value) else Value)),
               False);
      end Set_Param;

      Processed : Boolean := True;

   begin
      pragma Assert (Arg'First = 1);

      Success := True;

      if Arg'Length = 0 then
         return;
      end if;

      --  If preceding switch was -P, a project file name need to be
      --  specified, not a switch.

      if Project_File_Name_Expected then
         if Arg (1) = '-' then
            Fail_Program
              (Project_Tree, "project file name missing after -P");
         else
            Project_File_Name_Expected := False;
            Project_File_Name := new String'(Arg);
         end if;

         --  If preceding switch was -o, an executable name need to be
         --  specified, not a switch.

      elsif Search_Project_Dir_Expected then
         if Arg (1) = '-' then
            Fail_Program
              (Project_Tree, "directory name missing after -aP");
         else
            Search_Project_Dir_Expected := False;
            GPR.Env.Add_Directories (Root_Environment.Project_Path, Arg);
         end if;

      elsif Db_Directory_Expected then
            Db_Directory_Expected := False;
            Parse_Knowledge_Base (Project_Tree, Arg);

         --  Set the processor/language for the following switches

         --  Switches start with '-'

      elsif Arg (1) = '-' then

         if Has_Prefix (Source_Info_Option) then
            Project_Tree.Source_Info_File_Name :=
               new String'(Arg (Source_Info_Option'Length + 1 .. Arg'Last));

         elsif Has_Prefix (Config_Project_Option) then
            if Config_Project_File_Name /= null
              and then (Autoconf_Specified
                        or else Config_Project_File_Name.all /=
                          Arg (Config_Project_Option'Length + 1 .. Arg'Last))
            then
               Fail_Program
                 (Project_Tree,
                  "several different configuration switches "
                  & "cannot be specified");

            else
               Autoconfiguration := False;
               Autoconf_Specified := False;
               Config_Project_File_Name :=
                 new String'
                   (Arg (Config_Project_Option'Length + 1 .. Arg'Last));
            end if;

         elsif Has_Prefix (Autoconf_Project_Option) then
            if Config_Project_File_Name /= null
              and then (not Autoconf_Specified
                        or else Config_Project_File_Name.all /=
                          Arg (Autoconf_Project_Option'Length + 1 .. Arg'Last))
            then
               Fail_Program
                 (Project_Tree,
                  "several different configuration switches "
                  & "cannot be specified");

            else
               Config_Project_File_Name :=
                 new String'
                   (Arg (Autoconf_Project_Option'Length + 1 .. Arg'Last));
               Autoconf_Specified := True;
            end if;

         elsif Has_Prefix (RTS_Option) then
            declare
               Set : constant Boolean := Runtime_Name_Set_For (Name_Ada);
               Old : constant String := Runtime_Name_For (Name_Ada);
               RTS : constant String :=
                       Arg (RTS_Option'Length + 1 .. Arg'Last);
            begin
               if Command_Line then
                  if Set and then Old /= RTS then
                     Fail_Program
                       (Project_Tree,
                        "several different run-times cannot be specified");
                  end if;

                  Set_Runtime_For (Name_Ada, RTS);
                  Set_Default_Runtime_For (Name_Ada, RTS);
               end if;

               --  Ignore any --RTS= switch in package Builder. These are only
               --  taken into account to create the config file in
               --  auto-configuration.
            end;

         elsif Arg = "-h" then
            Usage_Needed := True;

         elsif Arg = "-p" or else Arg = "--create-missing-dirs" then
            Create_Dest_Dir := True;

         elsif Arg'Length >= 2 and then Arg (2) = 'P' then
            if Project_File_Name /= null then
               Fail_Program
                 (Project_Tree,
                  "cannot have several project files specified");

            elsif Arg'Length = 2 then
               Project_File_Name_Expected := True;

            else
               Project_File_Name := new String'(Arg (3 .. Arg'Last));
            end if;

         elsif Arg'Length >= 3 and then Arg (1 .. 3) = "-aP" then
            if Arg'Length = 3 then
               Search_Project_Dir_Expected := True;

            else
               GPR.Env.Add_Directories
                 (Root_Environment.Project_Path, Arg (4 .. Arg'Last));
            end if;

         elsif Arg = "-q" then
            Opt.Quiet_Output := True;
            Opt.Verbose_Mode := False;

         elsif Arg = "-r" then
            Recursive := True;

         elsif Arg = "-v" then
            Opt.Verbose_Mode := True;
            Opt.Quiet_Output := False;

         elsif Arg = "-f" then
            Force_Installations := True;

         elsif Arg = "-a" then
            if Opt_M_Set then
               Fail_Program
                 (Project_Tree, "cannot use -a and -m together");
            else
               All_Sources := True;
               Opt_A_Set := True;
            end if;

         elsif Arg = "-m" then
            if Opt_A_Set then
               Fail_Program
                 (Project_Tree, "cannot use -m and -a together");
            else
               All_Sources := False;
               Opt_M_Set := True;
            end if;

         elsif Arg = "-d" then
            Dry_Run := True;

         elsif Arg'Length >= 3
           and then Arg (2) = 'X'
           and then Is_External_Assignment (Root_Environment, Arg)
         then
            --  Is_External_Assignment has side effects when it returns True

            null;

         elsif Arg'Length > 1 and then Arg (2) = '-' then

            if Has_Prefix (Prefix_Project_Option) then
               Set_Param (Global_Prefix_Dir, Prefix_Project_Option);

            elsif Has_Prefix (Exec_Subdir_Option) then
               Set_Param (Global_Exec_Subdir, Exec_Subdir_Option);

            elsif Has_Prefix (Lib_Subdir_Option) then
               Set_Param (Global_Lib_Subdir, Lib_Subdir_Option);

            elsif Has_Prefix (Link_Lib_Subdir_Option) then
               Set_Param (Global_Link_Lib_Subdir, Link_Lib_Subdir_Option);

            elsif Has_Prefix (Sources_Subdir_Option) then
               Set_Param (Global_Sources_Subdir, Sources_Subdir_Option);

            elsif Has_Prefix (Project_Subdir_Option) then
               Set_Param (Global_Project_Subdir, Project_Subdir_Option);

            elsif Has_Prefix (Build_Var_Option) then
               Build_Var := new String'
                 (Arg (Build_Var_Option'Length + 2 .. Arg'Last));

            elsif Has_Prefix (No_Build_Var_Option) then
               No_Build_Var := True;

            elsif Has_Prefix (Build_Name_Option) then
               Free (Build_Name);
               Build_Name := new String'
                 (Arg (Build_Name_Option'Length + 2 .. Arg'Last));

            elsif Has_Prefix (Install_Name_Option) then
               Set_Param
                 (Global_Install_Name, Install_Name_Option, Is_Dir => False);

            elsif Has_Prefix (Sources_Only_Option) then
               Sources_Only := True;

            elsif Has_Prefix (Uninstall_Option) then
               Usage_Mode := Uninstall_Mode;

            elsif Has_Prefix (List_Option) then
               Usage_Mode := List_Mode;

            elsif Has_Prefix (Stat_Option) then
               Output_Stats := True;

            elsif Has_Prefix (Mode_Option) then
               declare
                  Mode : String := Arg (Mode_Option'Length + 2 .. Arg'Last);
               begin
                  To_Lower (Mode);

                  if Mode in "dev" | "usage" then
                     Set_Param
                       (Global_Install_Mode, Mode_Option, Is_Dir => False);
                  else
                     Processed := False;
                  end if;
               end;

            elsif Has_Prefix (Dry_Run_Option) then
               Dry_Run := True;

            elsif Has_Prefix (No_Lib_Link_Option) then
               Add_Lib_Link := False;

            elsif Has_Prefix (Subdirs_Option) then
               Subdirs :=
                 new String'(Arg (Subdirs_Option'Length + 1 .. Arg'Last));

            elsif Arg'Length >= Relocate_Build_Tree_Option'Length
              and then Arg (1 .. Relocate_Build_Tree_Option'Length)
              = Relocate_Build_Tree_Option
            then
               if Arg'Length = Relocate_Build_Tree_Option'Length then
                  Build_Tree_Dir := new String'(Get_Current_Dir);

               else
                  declare
                     Dir : constant String :=
                             Ensure_Directory
                               (Arg (Relocate_Build_Tree_Option'Length + 2
                                     .. Arg'Last));
                  begin
                     if Is_Absolute_Path (Dir) then
                        Build_Tree_Dir := new String'(Dir);
                     else
                        Build_Tree_Dir := new String'(Get_Current_Dir & Dir);
                     end if;
                  end;
               end if;

               --  Out-of-tree compilation also imply -p (create missing dirs)

               Opt.Setup_Projects := True;

            elsif Arg'Length >= Root_Dir_Option'Length
              and then Arg (1 .. Root_Dir_Option'Length) = Root_Dir_Option
            then
               Root_Dir :=
                 new String'
                   (Normalize_Pathname
                      (Arg (Root_Dir_Option'Length + 2 .. Arg'Last),
                       Get_Current_Dir)
                    & Dir_Separator);

            elsif Has_Prefix (Target_Project_Option) then
               if Target_Name /= null then
                  if Target_Name.all /=
                    Arg (Target_Project_Option'Length + 1 .. Arg'Last)
                  then
                     Fail_Program
                       (Project_Tree,
                        "several different target switches "
                        & "cannot be specified");
                  end if;

               else
                  Target_Name :=
                    new String'
                      (Arg (Target_Project_Option'Length + 1 .. Arg'Last));
               end if;

            else
               Processed := False;
            end if;

         else
            Processed := False;
         end if;

      elsif Command_Line then
         --  The file name of a main or a project file

         declare
            File_Name : String := Arg;

         begin
            Canonical_Case_File_Name (File_Name);

            if Usage_Mode = Uninstall_Mode
              or else
                (File_Name'Length > Project_File_Extension'Length
                 and then File_Name
                   (File_Name'Last - Project_File_Extension'Length + 1
                    .. File_Name'Last) = Project_File_Extension)
            then
               if Project_File_Name /= null then
                  Fail_Program
                    (Project_Tree,
                     "cannot have several project files specified");

               else
                  Project_File_Name := new String'(File_Name);
               end if;

            else
               --  Not a project file, then it is a main

               Fail_Program (Project_Tree, "only project files expected");
            end if;
         end;

      else
         Processed := False;
      end if;

      if not Processed then
         if Command_Line then
            Fail_Program
              (Project_Tree,
               "illegal option """ & Arg & """ on the command line");
         end if;
      end if;
   end Scan_Arg;

   ------------------------
   -- Sigint_Intercepted --
   ------------------------

   procedure Sigint_Intercepted is
   begin
      Put_Line ("*** Interrupted ***");
      Delete_All_Temp_Files (Project_Tree.Shared);
      OS_Exit (1);
   end Sigint_Intercepted;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      procedure Check_Version_And_Help is new Check_Version_And_Help_G (Usage);
   begin
      --  Do some necessary package initializations

      Snames.Initialize;

      Set_Program_Name ("gprinstall");

      GPR.Tree.Initialize (Root_Environment, Gprinstall_Flags);
      GPR.Tree.Initialize (Project_Node_Tree);

      GPR.Initialize (Project_Tree);
      Mains.Delete;

      --  Get the command line arguments, starting with --version and --help

      Check_Version_And_Help
        ("GPRINSTALL", "2012", Version_String => Gpr_Version_String);

      --  Now process the other options

      Autoconfiguration := True;

      declare
         Do_Not_Care : Boolean;

      begin
         Scan_Args : for Next_Arg in 1 .. Argument_Count loop
            Scan_Arg
              (Argument (Next_Arg),
               Command_Line => True,
               Success      => Do_Not_Care);
         end loop Scan_Args;
      end;

      GPR.Env.Initialize_Default_Project_Path
        (Root_Environment.Project_Path, Target_Name => "-");

      if Opt.Verbose_Mode then
         Copyright;
      end if;

      if Usage_Needed then
         Usage;
         Usage_Needed := False;
      end if;

      --  Fail if command line ended with "-P"

      if Project_File_Name_Expected then
         Fail_Program
           (Project_Tree, "project file name missing after -P");

      elsif Search_Project_Dir_Expected then
         Fail_Program
           (Project_Tree, "directory name missing after -aP");
      end if;

      if Build_Name.all /= "default" and then Usage_Mode = Uninstall_Mode then
         Fail_Program
           (Project_Tree, "cannot specify --build-name in uninstall mode");
      end if;

      if Build_Var /= null and then Usage_Mode = Uninstall_Mode then
         Fail_Program
           (Project_Tree, "cannot specify --build-var in uninstall mode");
      end if;

      if Build_Var /= null and then No_Build_Var then
         Fail_Program
           (Project_Tree, "cannot specify --build-var and --no-build-var");
      end if;

      if Output_Stats and then Usage_Mode /= List_Mode then
         Fail_Program
           (Project_Tree, "cannot specify --stat in install/uninstall mode");
      end if;

      --  Makes the Ada RTS absolute if it is not a base name

      if Runtime_Name_Set_For (Name_Ada) then
         Locate_Runtime (Project_Tree, Name_Ada);
      end if;

      if Load_Standard_Base then
         --  We need to parse the knowledge base so that we are able to
         --  normalize the target names. Unfortunately, if we have to spawn
         --  gprconfig, it will also have to parse that knowledge base on
         --  its own.
         Parse_Knowledge_Base (Project_Tree);
      end if;

      --  If no project file was specified, look first for a default

      if Project_File_Name = null
        and then Usage_Mode /= List_Mode
      then
         Try_Help;
         Fail_Program (Project_Tree, "no project file specified");
      end if;

      --  Check prefix, if not specified set to default toolchain

      if Global_Prefix_Dir.V = null then
         --  Set to default for current toolchain
         Global_Prefix_Dir := (new String'(Executable_Prefix_Path), True);
      end if;

      --  Do not require directory to be present in Sources_Only mode

      Opt.Directories_Must_Exist_In_Projects := not Sources_Only;

      --  Check consistency of out-of-tree build options.

      if Root_Dir /= null and then Build_Tree_Dir = null then
         Fail_Program
           (Project_Tree,
            "cannot use --root-dir without --relocate-build-tree option");
      end if;

      --  Set default Root_Dir

      if Build_Tree_Dir /= null and then Root_Dir = null then
         Root_Dir := new String'
           (Ada.Directories.Containing_Directory
              (Normalize_Pathname (Project_File_Name.all))
            & Dir_Separator);
      end if;
   end Initialize;

   -----------
   -- Usage --
   -----------

   procedure Usage is
   begin
      if not Usage_Output then
         Usage_Output := True;

         Put ("Usage: ");
         Write_Program_Name;
         Put (" [-P<proj>] [<proj>.gpr] [opts]");
         New_Line;
         New_Line;

         --  GPRINSTALL switches

         Put ("gprinstall switches:");
         New_Line;

         Display_Usage_Version_And_Help;

         --  Line for Config_Project_Option

         Put ("  ");
         Put (Config_Project_Option);
         Put ("file.cgpr");
         New_Line;
         Put ("           Specify the main config project file name");
         New_Line;

         --  Line for Autoconf_Project_Option

         Put ("  ");
         Put (Autoconf_Project_Option);
         Put ("file.cgpr");
         New_Line;
         Put
           ("           Specify/create the main config project file name");
         New_Line;

         Put ("  --RTS=<runtime>");
         New_Line;
         Put ("           Use runtime <runtime> for language Ada");
         New_Line;

         --  Line for --prefix

         Put_Line ("  --prefix=<dir>");
         Put_Line ("           Install destination directory");
         Put_Line ("  --install-name=<name>");
         Put_Line ("           The name of the installation");
         Put_Line ("  --sources-subdir=<dir>");
         Put_Line ("           The sources directory/sub-directory");
         Put_Line ("  --lib-subdir=<dir>");
         Put_Line ("           The library directory/sub-directory");
         Put_Line ("  --link-lib-subdir=<dir>");
         Put_Line
           ("           The symlib directory/sub-directory to libraries");
         Put_Line ("  --exec-subdir=<dir>");
         Put_Line ("           The executbales directory/sub-directory");
         Put_Line ("  --project-subdir=<dir>");
         Put_Line ("           The project directory/sub-directory");
         Put_Line ("  --no-lib-link");
         Put_Line
           ("           Do not copy shared lib in exec/lib directory");

         Put_Line ("  --sources-only");
         Put_Line ("           Copy project sources only");

         --  Line for --relocate-build-tree=

         Put ("  --relocate-build-tree[=dir]");
         New_Line;
         Put ("           Root obj/lib/exec dirs are current-directory" &
                    " or dir");
         New_Line;

         --  Line for --root-dir=

         Put ("  --root-dir=dir");
         New_Line;
         Put ("           Root directory of obj/lib/exec to relocate");
         New_Line;

         --  Line for --subdirs=

         Put_Line ("  --subdirs=dir");
         Put_Line ("           Real obj/lib/exec dirs are subdirs");

         --  Line for Target_Project_Option

         Put ("  ");
         Put (Target_Project_Option);
         Put ("targetname");
         New_Line;
         Put
           ("           Specify a target for cross platforms");
         New_Line;

         --  Line for --dry-run

         Put_Line ("  -d, --dry-run");
         Put_Line ("           Execute nothing, display commands");

         --  Line for --build-var

         Put_Line ("  --build-var=<name>");
         Put_Line ("           Name of the variable which identify a build");

         --  Line for --no-build-var

         Put_Line ("  --no-build-var");
         Put_Line ("           Do not generate external build variable");

         --  Line for --build-name

         Put_Line ("  --build-name=<name>");
         Put_Line ("           Build name value (default is ""Default"")");

         --  Line for --mode

         Put_Line ("  --mode=[dev|usage]");
         Put_Line
           ("           Kind of installation (default is ""dev"")");

         --  Line for --uninstall

         Put_Line ("  --uninstall");
         Put_Line
           ("           Remove all previously installed files");

         --  Lines for --list/--stat

         Put_Line ("  --list");
         Put_Line
           ("           List all installed projects");

         Put_Line ("  --stat");
         Put_Line
           ("           Display stats about installed projects, must be "
            & "used with --list");

         --  Line for -aP

         Put_Line ("  -aP dir  Add directory dir to project search path");

         --  Line for -eL

         Put_Line ("  -eL      "
                     & "Follow symbolic links when processing project files");

         --  Line for -P

         Put_Line ("  -P proj  Use Project File proj");

         --  Line for -p

         Put_Line ("  -p, --create-missing-dirs");
         Put_Line ("           Create missing directories");

         --  Line for -q

         Put_Line ("  -q       Be quiet/terse");

         --  Line for -r

         Put_Line ("  -r       Recursive");

         --  Line for -m

         Put_Line ("  -m       Minimal copy of sources (only those needed)");

         --  Line for -f

         Put_Line ("  -f       Force installaion, overwrite files");

         --  Line for -v

         Put_Line ("  -v       Verbose output");

         --  Line for -X

         Put_Line ("  -Xnm=val Specify an external reference for "
                     & "Project Files");
         New_Line;
      end if;
   end Usage;

   User_Project_Node : Project_Node_Id;

begin
   --  First initialize and read the command line arguments

   Initialize;

   --  And install Ctrl-C handler

   Install_Int_Handler (Sigint_Intercepted'Unrestricted_Access);

   --  Check command line arguments. These will be overridden when looking
   --  for the configuration file

   if Target_Name = null then
      Target_Name := new String'("");
   end if;

   if Config_Project_File_Name = null then
      Config_Project_File_Name :=
        new String'((if Sources_Only then "auto.cgpr" else ""));
   end if;

   --  Then, parse the user's project and the configuration file. Apply the
   --  configuration file to the project so that its settings are
   --  automatically inherited by the project.
   --  If either the project or the configuration file contains errors, the
   --  following call with call Fail_Program and never return

   if Usage_Mode = Install_Mode then
      begin
         Main_Project := No_Project;
         Parse_Project_And_Apply_Config
           (Main_Project               => Main_Project,
            User_Project_Node          => User_Project_Node,
            Config_File_Name           => Config_Project_File_Name.all,
            Autoconf_Specified         => Autoconf_Specified,
            Project_File_Name          => Project_File_Name.all,
            Project_Tree               => Project_Tree,
            Env                        => Root_Environment,
            Project_Node_Tree          => Project_Node_Tree,
            Packages_To_Check          => Packages_To_Check,
            Allow_Automatic_Generation => Autoconfiguration,
            Automatically_Generated    => Delete_Autoconf_File,
            Config_File_Path           => Configuration_Project_Path,
            Target_Name                => Target_Name.all,
            Normalized_Hostname        => Normalized_Hostname,
            Implicit_Project           => No_Project_File_Found);
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
            """" & Project_File_Name.all & """ processing failed",
            Flush_Messages => User_Project_Node /= Empty_Project_Node);
      end if;

      if Configuration_Project_Path /= null then
         Free (Config_Project_File_Name);
         Config_Project_File_Name := new String'
           (Base_Name (Configuration_Project_Path.all));
      end if;

      if Total_Errors_Detected > 0 then
         GPR.Err.Finalize;
         Fail_Program
           (Project_Tree,
            "problems while getting the configuration",
            Flush_Messages => False);
      end if;

      Main_Project_Dir :=
        new String'(Get_Name_String (Main_Project.Directory.Display_Name));

      if Warnings_Detected > 0 then
         GPR.Err.Finalize;
         GPR.Err.Initialize;
      end if;

      Mains.Fill_From_Project (Main_Project, Project_Tree);

      Compute_All_Imported_Projects (Main_Project, Project_Tree);

      Install.Process (Project_Tree, Main_Project);

      if Warnings_Detected /= 0 then
         GPR.Err.Finalize;
      end if;

   elsif Usage_Mode = List_Mode then
      DB.List;

   else
      if Global_Install_Name.Default then
         Uninstall.Process (Ada.Directories.Base_Name (Project_File_Name.all));
      else
         Uninstall.Process (Global_Install_Name.V.all);
      end if;
   end if;

   if Usage_Mode = Install_Mode then
      Finish_Program (Project_Tree);
   end if;
end Gprinstall.Main;
