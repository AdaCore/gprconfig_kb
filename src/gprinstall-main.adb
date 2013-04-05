------------------------------------------------------------------------------
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                      G P R I N S T A L L . M A I N                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2012-2013, Free Software Foundation, Inc.         --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Exceptions;   use Ada.Exceptions;

with GNAT.Case_Util;            use GNAT.Case_Util;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;

with Atree;       use Atree;
with Csets;
with Gpr_Util;    use Gpr_Util;
with GPR_Version; use GPR_Version;
with Hostparm;
with Makeutl;     use Makeutl;
with Namet;       use Namet;
with Osint;       use Osint;
with Output;      use Output;
with Prj.Conf;    use Prj.Conf;
with Prj.Env;
with Prj.Err;
with Prj.Tree;    use Prj.Tree;
with Snames;      use Snames;
with Switch;      use Switch;

with Opt;         use Opt;
with Types;       use Types;

with Gprinstall.Install;
with Gprinstall.Uninstall;

procedure Gprinstall.Main is

   use Gpr_Util.Knowledge;

   --  Options specific to gprinstall

   Build_Var_Option       : constant String := "--build-var";
   Build_Name_Option      : constant String := "--build-name";
   Uninstall_Option       : constant String := "--uninstall";
   Mode_Option            : constant String := "--mode=";
   Lib_Subdir_Option      : constant String := "--lib-subdir";
   Link_Lib_Subdir_Option : constant String := "--link-lib-subdir";
   Exec_Subdir_Option     : constant String := "--exec-subdir";
   Sources_Subdir_Option  : constant String := "--sources-subdir";
   Project_Subdir_Option  : constant String := "--project-subdir";
   No_Lib_Link_Option     : constant String := "--no-lib-link";

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
           ("GPRBUILD", "2012", Version_String => Gpr_Version_String);
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

      procedure Set_Param (P : in out Param; Name : String);
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

      procedure Set_Param (P : in out Param; Name : String) is
      begin
         P := (new String'
                 (Ensure_Directory (Arg (Name'Length + 2 .. Arg'Last))),
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
            Prj.Env.Add_Directories (Root_Environment.Project_Path, Arg);
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
            if Hostparm.OpenVMS then
               Fail_Program
                 (Project_Tree,
                  Autoconf_Project_Option & " cannot be used on VMS");
            end if;

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

         elsif Arg = "-h" then
            Usage_Needed := True;

         elsif Arg = "-p" or else Arg = "--create-missing-dirs" then
            Opt.Setup_Projects := True;

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
               Prj.Env.Add_Directories
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
            All_Sources := True;

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

            elsif Has_Prefix (Build_Name_Option) then
               Free (Build_Name);
               Build_Name := new String'
                 (Arg (Build_Name_Option'Length + 2 .. Arg'Last));

            elsif Has_Prefix (Uninstall_Option) then
               Uninstall_Mode := True;

            elsif Has_Prefix (Mode_Option) then
               declare
                  Mode : String := Arg (Mode_Option'Length + 1 .. Arg'Last);
               begin
                  To_Lower (Mode);

                  if Mode = "dev" then
                     For_Dev := True;
                  elsif Mode = "usage" then
                     For_Dev := False;
                  else
                     Processed := False;
                  end if;
               end;

            elsif Has_Prefix (Dry_Run_Option) then
               Dry_Run := True;

            elsif Has_Prefix (No_Lib_Link_Option) then
               Add_Lib_Link := False;

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

            if File_Name'Length > Project_File_Extension'Length
              and then File_Name
                (File_Name'Last - Project_File_Extension'Length + 1
                 .. File_Name'Last) = Project_File_Extension
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
      Write_Line ("*** Interrupted ***");
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

      Csets.Initialize;
      Namet.Initialize;
      Snames.Initialize;

      Prj.Tree.Initialize (Root_Environment, Gprbuild_Flags);
      Prj.Tree.Initialize (Project_Node_Tree);

      Prj.Initialize (Project_Tree);
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

      Mains.Set_Multi_Unit_Index (Project_Tree, Main_Index);

      --  Target_Name has potentially been set when calling Scan_Arg, so we can
      --  only initialize the project path after parsing the command line
      --  arguments.

      if Target_Name = null then
         Prj.Env.Initialize_Default_Project_Path
           (Root_Environment.Project_Path, Target_Name => "");
      else
         Prj.Env.Initialize_Default_Project_Path
           (Root_Environment.Project_Path, Target_Name.all);
      end if;

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

      if Build_Name.all /= "default" and then Uninstall_Mode then
         Fail_Program
           (Project_Tree, "cannot specify --build-name in uninstall mode");
      end if;

      if Build_Var /= null and then Uninstall_Mode then
         Fail_Program
           (Project_Tree, "cannot specify --build-var in uninstall mode");
      end if;

      if Load_Standard_Base then
         --  We need to parse the knowledge base so that we are able to
         --  normalize the target names. Unfortunately, if we have to spawn
         --  gprconfig, it will also have to parse that knowledge base on
         --  its own.
         Parse_Knowledge_Base (Project_Tree);
      end if;

      --  If no project file was specified, look first for a default

      if Project_File_Name = null then
         Copyright;
         Usage;
         Fail_Program (Project_Tree, "no project file specified");
      end if;

      --  Check prefix, if not specified set to default toolchain

      if Global_Prefix_Dir.V = null then
         --  Set to default for current toolchain
         Global_Prefix_Dir := (new String'(Executable_Prefix_Path), True);
      end if;
   end Initialize;

   -----------
   -- Usage --
   -----------

   procedure Usage is
   begin
      if not Usage_Output then
         Usage_Output := True;

         Write_Str ("Usage: ");
         Osint.Write_Program_Name;
         Write_Str (" [-P<proj>] [<proj>.gpr] [opts]");
         Write_Eol;
         Write_Eol;

         --  GPRINSTALL switches

         Write_Str ("gprinstall switches:");
         Write_Eol;

         Display_Usage_Version_And_Help;

         --  Line for Config_Project_Option

         Write_Str ("  ");
         Write_Str (Config_Project_Option);
         Write_Str ("file.cgpr");
         Write_Eol;
         Write_Str ("           Specify the main config project file name");
         Write_Eol;

         --  Line for Autoconf_Project_Option

         if not Hostparm.OpenVMS then
            Write_Str ("  ");
            Write_Str (Autoconf_Project_Option);
            Write_Str ("file.cgpr");
            Write_Eol;
            Write_Str
              ("           Specify/create the main config project file name");
            Write_Eol;
         end if;

         --  Line for --prefix

         Write_Line ("  --prefix=<dir>");
         Write_Line ("           Install destination directory");
         Write_Line ("  --sources-subdir=<dir>");
         Write_Line ("           The sources directory/sub-directory");
         Write_Line ("  --lib-subdir=<dir>");
         Write_Line ("           The library directory/sub-directory");
         Write_Line ("  --link-lib-subdir=<dir>");
         Write_Line
           ("           The symlib directory/sub-directory to libraries");
         Write_Line ("  --exec-subdir=<dir>");
         Write_Line ("           The executbales directory/sub-directory");
         Write_Line ("  --project-subdir=<dir>");
         Write_Line ("           The project directory/sub-directory");
         Write_Line ("  --no-lib-link");
         Write_Line
           ("           Do not copy shared lib in exec/lib directory");

         --  Line for Target_Project_Option

         Write_Str ("  ");
         Write_Str (Target_Project_Option);
         Write_Str ("targetname");
         Write_Eol;
         Write_Str
           ("           Specify a target for cross platforms");
         Write_Eol;

         --  Line for --dry-run

         Write_Line ("  -d, --dry-run");
         Write_Line ("           Execute nothing, display commands");

         --  Line for --build-var

         Write_Line ("  --build-var=<name>");
         Write_Line ("           Name of the variable which identify a build");

         --  Line for --build-name

         Write_Line ("  --build-name=<name>");
         Write_Line ("           Build name value (default is ""Default"")");

         --  Line for --mode

         Write_Line ("  --mode=[dev|usage]");
         Write_Line
           ("           Kind of installation (default is ""dev"")");

         --  Line for --uninstall

         Write_Line ("  --uninstall");
         Write_Line
           ("           Remove all previously installed files");

         --  Line for -aP

         Write_Line ("  -aP dir  Add directory dir to project search path");

         --  Line for -eL

         Write_Line ("  -eL      "
                     & "Follow symbolic links when processing project files");

         --  Line for -P

         Write_Line ("  -P proj  Use Project File proj");

         --  Line for -p

         Write_Line ("  -p, --create-missing-dirs");
         Write_Line ("           Create missing directories");

         --  Line for -q

         Write_Line ("  -q       Be quiet/terse");

         --  Line for -r

         Write_Line ("  -r       Recursive (default except when using -c)");

         --  Line for -a

         Write_Line ("  -a       Force copy of all sources");

         --  Line for -f

         Write_Line ("  -f       Force installaion, overwrite files");

         --  Line for -v

         Write_Line ("  -v       Verbose output");

         --  Line for -X

         Write_Line ("  -Xnm=val Specify an external reference for "
                     & "Project Files");
         Write_Eol;
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
      Config_Project_File_Name := new String'("");
   end if;

   --  Then, parse the user's project and the configuration file. Apply the
   --  configuration file to the project so that its settings are
   --  automatically inherited by the project.
   --  If either the project or the configuration file contains errors, the
   --  following call with call Osint.Fail and never return

   if not Uninstall_Mode then
      begin
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
            Normalized_Hostname        => Normalized_Hostname);
      exception
         when E : Prj.Conf.Invalid_Config =>
            Osint.Fail (Exception_Message (E));
      end;

      if Main_Project = No_Project then
         --  Don't flush messages in case of parsing error. This has already
         --  been taken care when parsing the tree. Otherwise, it results in
         --  the same message being displayed twice.

         Fail_Program
           (Project_Tree,
            """" & Project_File_Name.all & """ processing failed",
            Flush_Messages => User_Project_Node /= Empty_Node);
      end if;

      if Configuration_Project_Path /= null then
         Free (Config_Project_File_Name);
         Config_Project_File_Name := new String'
           (Base_Name (Configuration_Project_Path.all));
      end if;

      if Total_Errors_Detected > 0 then
         Prj.Err.Finalize;
         Fail_Program
           (Project_Tree,
            "problems while getting the configuration",
            Flush_Messages => False);
      end if;

      Main_Project_Dir :=
        new String'(Get_Name_String (Main_Project.Directory.Display_Name));

      if Warnings_Detected > 0 then
         Prj.Err.Finalize;
         Prj.Err.Initialize;
      end if;

      Mains.Fill_From_Project (Main_Project, Project_Tree);

      Compute_All_Imported_Projects (Main_Project, Project_Tree);

      --  Source file lookups should be cached for efficiency.
      --  Source files are not supposed to change.

      Osint.Source_File_Data (Cache => True);

      Install.Process (Project_Tree, Main_Project);

      if Warnings_Detected /= 0 then
         Prj.Err.Finalize;
      end if;

   else
      Uninstall.Process (Base_Name (Project_File_Name.all, ".gpr"));
   end if;

   Namet.Finalize;

   if not Uninstall_Mode then
      Finish_Program (Project_Tree, E_Success);
   end if;
end Gprinstall.Main;
