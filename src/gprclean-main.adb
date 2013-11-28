------------------------------------------------------------------------------
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                        G P R C L E A N . M A I N                         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2011-2013, Free Software Foundation, Inc.          --
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

--  This package contains the implementation of gprclean.
--  See gprclean.adb

with Ada.Command_Line;           use Ada.Command_Line;
with Ada.Exceptions;             use Ada.Exceptions;

with System.Case_Util;           use System.Case_Util;

with GNAT.Directory_Operations;  use GNAT.Directory_Operations;
with GNAT.IO;                    use GNAT.IO;
with GNAT.OS_Lib;                use GNAT.OS_Lib;

with Gprbuild.Compilation.Slave; use Gprbuild.Compilation.Slave;

with Csets;
with Gpr_Util;    use Gpr_Util;
with GPR_Version; use GPR_Version;
with Hostparm;
with Makeutl;     use Makeutl;
with Namet;       use Namet;
with Opt;         use Opt;
with Osint;
with Prj;         use Prj;
with Prj.Conf;    use Prj.Conf;
with Prj.Env;
with Prj.Err;
with Prj.Ext;
with Prj.Tree;    use Prj.Tree;
with Snames;
with Stringt;
with Switch;      use Switch;

procedure Gprclean.Main is

   use Knowledge;

   procedure Usage;
   --  Display the usage.
   --  If called several times, the usage is displayed only the first time.

   procedure Parse_Cmd_Line;
   --  Parse the command line

   procedure Display_Copyright;
   --  Display the Copyright notice. If called several times, display the
   --  Copyright notice only the first time.

   procedure Initialize;
   --  Call the necessary package initializations

   procedure Check_Version_And_Help is new Check_Version_And_Help_G (Usage);

   -----------------------
   -- Display_Copyright --
   -----------------------

   procedure Display_Copyright is
   begin
      if not Copyright_Displayed then
         Copyright_Displayed := True;
         Display_Version
           ("GPRCLEAN", "2006", Version_String => Gpr_Version_String);
      end if;
   end Display_Copyright;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      if not Initialized then
         Initialized := True;

         --  Initialize some packages

         Csets.Initialize;
         Namet.Initialize;
         Snames.Initialize;
         Stringt.Initialize;

         Prj.Tree.Initialize (Root_Environment, Gprclean_Flags);
         Prj.Tree.Initialize (Project_Node_Tree);
      end if;

      --  Reset global variables

      Do_Nothing := False;
      File_Deleted := False;
      Copyright_Displayed := False;
      Usage_Displayed := False;
      Free (Project_File_Name);
      Main_Project := Prj.No_Project;
      All_Projects := False;
      Mains.Delete;
   end Initialize;

   --------------------
   -- Parse_Cmd_Line --
   --------------------

   procedure Parse_Cmd_Line is
      Last  : constant Natural := Argument_Count;
      Index : Positive := 1;

   begin
      --  First deal with --version and --help

      Check_Version_And_Help
        ("GPRCLEAN",
         "2006",
         Version_String => Gpr_Version_String);

      --  Now deal with the other options

      while Index <= Last loop
         declare
            Arg : constant String := Argument (Index);

            procedure Bad_Argument;
            --  Signal bad argument

            ------------------
            -- Bad_Argument --
            ------------------

            procedure Bad_Argument is
            begin
               Osint.Fail ("invalid argument """ & Arg & '"');
            end Bad_Argument;

         begin
            if Db_Directory_Expected then
               Db_Directory_Expected := False;
               Parse_Knowledge_Base (Project_Tree, Arg);

               Name_Len := 0;
               Add_Str_To_Name_Buffer (Arg);
               Db_Switch_Args.Append (Name_Find);

            elsif Arg'Length /= 0 then
               if Arg (1) = '-' then
                  if Arg'Length = 1 then
                     Bad_Argument;
                  end if;

                  case Arg (2) is
                     when '-' =>
                        if not Hostparm.OpenVMS and then Arg = "--db-" then
                           Load_Standard_Base := False;

                        elsif not Hostparm.OpenVMS and then Arg = "--db" then
                           Db_Directory_Expected := True;

                        elsif Arg'Length > Config_Project_Option'Length
                          and then Arg (1 .. Config_Project_Option'Length) =
                                   Config_Project_Option
                        then
                           if Config_Project_File_Name /= null
                             and then
                               (Autoconf_Specified
                                or else Config_Project_File_Name.all /=
                                  Arg (Config_Project_Option'Length + 1
                                       .. Arg'Last))
                           then
                              Fail_Program
                                (Project_Tree,
                                 "several configuration switches cannot "
                                 & "be specified");

                           else

                              Autoconfiguration := False;
                              Config_Project_File_Name :=
                                new String'
                                  (Arg (Config_Project_Option'Length + 1
                                        .. Arg'Last));
                           end if;

                        elsif Arg (1 .. Distributed_Option'Length)
                          = Distributed_Option
                        then
                           Distributed_Mode := True;

                           Gprbuild.Compilation.Slave.Record_Slaves
                             (Arg (Distributed_Option'Length + 1 .. Arg'Last));

                        elsif Arg'Length >= Slave_Env_Option'Length
                          and then
                            Arg (1 .. Slave_Env_Option'Length)
                          = Slave_Env_Option
                        then
                           if Arg = Slave_Env_Option then
                              --  Just --slave-env, it is up to gprbuild to
                              --  build a sensible slave environment value.
                              Slave_Env_Auto := True;
                           else
                              Slave_Env := new String'
                                (Arg
                                   (Slave_Env_Option'Length + 2 .. Arg'Last));
                           end if;

                        elsif not Hostparm.OpenVMS
                              and then
                                Arg'Length > Autoconf_Project_Option'Length
                              and then
                                Arg (1 .. Autoconf_Project_Option'Length) =
                                Autoconf_Project_Option
                        then
                           if Config_Project_File_Name /= null
                             and then
                               (not Autoconf_Specified
                                or else
                                  Config_Project_File_Name.all /=
                                    Arg (Autoconf_Project_Option'Length + 1
                                         .. Arg'Last))
                           then
                              Fail_Program
                                (Project_Tree,
                                 "several configuration switches cannot "
                                 & "be specified");

                           else
                              Config_Project_File_Name :=
                                new String'
                                  (Arg (Autoconf_Project_Option'Length + 1
                                        .. Arg'Last));
                              Autoconf_Specified := True;
                           end if;

                        elsif not Hostparm.OpenVMS
                          and then
                            Arg'Length > Target_Project_Option'Length
                          and then
                            Arg (1 .. Target_Project_Option'Length) =
                               Target_Project_Option
                        then
                           if Target_Name /= null then
                              if Target_Name.all /=
                                Arg (Target_Project_Option'Length + 1
                                     .. Arg'Last)
                              then
                                 Fail_Program
                                   (Project_Tree,
                                    "several target switches "
                                    & "cannot be specified");
                              end if;

                           else
                              Target_Name :=
                                new String'
                                  (Arg (Target_Project_Option'Length + 1
                                        .. Arg'Last));
                           end if;

                        elsif Arg'Length > RTS_Option'Length
                          and then Arg (1 .. RTS_Option'Length) = RTS_Option
                        then
                           declare
                              Set : constant Boolean :=
                                Runtime_Name_Set_For (Snames.Name_Ada);
                              Old : constant String :=
                                Runtime_Name_For (Snames.Name_Ada);
                              RTS : constant String :=
                                Arg (RTS_Option'Length + 1 .. Arg'Last);

                           begin
                              if Set and then Old /= RTS then
                                 Fail_Program
                                   (Project_Tree,
                                    "several different run-times " &
                                      "cannot be specified");
                              end if;

                              Set_Runtime_For (Snames.Name_Ada, RTS);
                           end;

                        elsif Arg'Length > RTS_Language_Option'Length
                          and then Arg (1 .. RTS_Language_Option'Length) =
                                   RTS_Language_Option
                        then
                           declare
                              Language_Name : Name_Id := No_Name;
                              RTS_Start : Natural := Arg'Last + 1;

                           begin
                              for J in RTS_Language_Option'Length + 2 ..
                                       Arg'Last
                              loop
                                 if Arg (J) = '=' then
                                    Name_Len := 0;
                                    Add_Str_To_Name_Buffer
                                      (Arg
                                         (RTS_Language_Option'Length + 1 ..
                                          J - 1));
                                    To_Lower (Name_Buffer (1 .. Name_Len));
                                    Language_Name := Name_Find;
                                    RTS_Start := J + 1;
                                    exit;
                                 end if;
                              end loop;

                              if Language_Name = No_Name then
                                 Fail_Program
                                   (Project_Tree, "illegal switch: " & Arg);

                              else

                                 declare
                                    RTS : constant String :=
                                      Arg (RTS_Start .. Arg'Last);
                                    Set : constant Boolean :=
                                      Runtime_Name_Set_For (Language_Name);
                                    Old : constant String :=
                                      Runtime_Name_For (Language_Name);

                                 begin
                                    if Set and then Old /= RTS then
                                       Fail_Program
                                       (Project_Tree,
                                        "several different run-times cannot" &
                                        " be specified for the same language");

                                    else
                                       Set_Runtime_For (Language_Name, RTS);
                                    end if;
                                 end;
                              end if;
                           end;

                        elsif Arg'Length > Subdirs_Option'Length
                          and then
                            Arg (1 .. Subdirs_Option'Length) = Subdirs_Option
                        then
                           Subdirs :=
                             new String'
                               (Arg (Subdirs_Option'Length + 1 .. Arg'Last));

                        elsif Arg = Makeutl.Unchecked_Shared_Lib_Imports then
                           Opt.Unchecked_Shared_Lib_Imports := True;

                        else
                           Bad_Argument;
                        end if;

                     when 'a' =>
                        if Arg'Length < 4 then
                           Bad_Argument;
                        end if;

                        if Arg (3) = 'P' then
                           Prj.Env.Add_Directories
                             (Root_Environment.Project_Path,
                              Arg (4 .. Arg'Last));

                        else
                           Bad_Argument;
                        end if;

                     when 'c'    =>
                        Compile_Only := True;

                     when 'e' =>
                        if Arg = "-eL" then
                           Follow_Links_For_Files := True;
                           Follow_Links_For_Dirs  := True;

                        else
                           Bad_Argument;
                        end if;

                     when 'f' =>
                        Force_Deletions := True;
                        Opt.Directories_Must_Exist_In_Projects := False;

                     when 'F' =>
                        Full_Path_Name_For_Brief_Errors := True;

                     when 'h' =>
                        Display_Copyright;
                        Usage;

                     when 'n' =>
                        Do_Nothing := True;

                     when 'P' =>
                        if Project_File_Name /= null then
                           Osint.Fail ("multiple -P switches");
                        end if;

                        if Arg'Length > 2 then
                           declare
                              Prj : constant String := Arg (3 .. Arg'Last);
                           begin
                              if Prj'Length > 1
                                and then Prj (Prj'First) = '='
                              then
                                 Project_File_Name :=
                                   new String'
                                     (Prj (Prj'First + 1 ..  Prj'Last));
                              else
                                 Project_File_Name := new String'(Prj);
                              end if;
                           end;

                        else
                           if Index = Last then
                              Osint.Fail ("no project specified after -P");
                           end if;

                           Index := Index + 1;
                           Project_File_Name := new String'(Argument (Index));
                        end if;

                     when 'q' =>
                        Quiet_Output := True;

                     when 'r' =>
                        All_Projects := True;

                     when 'v' =>
                        if Arg = "-v" then
                           Verbose_Mode := True;

                        elsif Arg = "-vP0" then
                           Current_Verbosity := Prj.Default;

                        elsif Arg = "-vP1" then
                           Current_Verbosity := Prj.Medium;

                        elsif Arg = "-vP2" then
                           Current_Verbosity := Prj.High;

                        else
                           Bad_Argument;
                        end if;

                     when 'X' =>
                        if Arg'Length = 2 then
                           Bad_Argument;
                        end if;

                        declare
                           Ext_Asgn : constant String := Arg (3 .. Arg'Last);
                           Start    : Positive := Ext_Asgn'First;
                           Stop     : Natural  := Ext_Asgn'Last;
                           OK       : Boolean  := True;

                        begin
                           if Ext_Asgn (Start) = '"' then
                              if Ext_Asgn (Stop) = '"' then
                                 Start := Start + 1;
                                 Stop  := Stop - 1;
                              else
                                 OK := False;
                              end if;
                           end if;

                           if not OK
                             or else not Prj.Ext.Check
                               (Root_Environment.External,
                                Declaration => Ext_Asgn (Start .. Stop))
                           then
                              Osint.Fail
                                ("illegal external assignment '"
                                 & Ext_Asgn & ''');
                           end if;
                        end;

                     when others =>
                        Bad_Argument;
                  end case;

               else
                  --  The file name of a main or a project file

                  declare
                     File_Name : String := Arg;

                  begin
                     Osint.Canonical_Case_File_Name (File_Name);

                     if File_Name'Length > Project_File_Extension'Length
                       and then
                        File_Name
                          (File_Name'Last - Project_File_Extension'Length + 1
                           .. File_Name'Last) = Project_File_Extension
                     then
                        if Project_File_Name /= null then
                           Osint.Fail
                             ("cannot have several project files specified");

                        else
                           Project_File_Name := new String'(File_Name);
                        end if;

                     else
                        --  Not a project file, then it is a main

                        Mains.Add_Main (Arg);
                     end if;
                  end;
               end if;
            end if;
         end;

         Index := Index + 1;
      end loop;
   end Parse_Cmd_Line;

   -----------
   -- Usage --
   -----------

   procedure Usage is
   begin
      if not Usage_Displayed then
         Usage_Displayed := True;

         Put_Line ("Usage: gprclean [switches] -P<project> {name}");

         New_Line;

         Put_Line ("  {name} is zero or more file names");
         New_Line;

         Display_Usage_Version_And_Help;

         Put_Line ("  --distributed=slave1[,slave2]");
         Put_Line ("           Activate the remote clean-up");

         Put_Line ("  --slave-env[=name]");
         Put_Line ("           Use a specific slave's environment");

         Put_Line ("  --config=file.cgpr");
         Put_Line ("           Specify the configuration project file name");

         if not Hostparm.OpenVMS then
            Put_Line ("  --autoconf=file.cgpr");
            Put_Line
              ("           Specify/create the main config project file name");
         end if;

         if not Hostparm.OpenVMS then
            Put_Line ("  --target=targetname");
            Put_Line ("           Specify a target for cross platforms");
         end if;

         if not Hostparm.OpenVMS then
            Put_Line ("  --db dir Parse dir as an additional knowledge base");
         end if;

         if not Hostparm.OpenVMS then
            Put_Line ("  --db-    Do not load the standard knowledge base");
         end if;

         Put_Line ("  --RTS=<runtime>");
         Put_Line ("           Use runtime <runtime> for language Ada");
         Put_Line ("  --RTS:<lang>=<runtime>");
         Put_Line ("           Use runtime <runtime> for language <lang>");

         Put_Line ("  --subdirs=dir");
         Put_Line ("           Real obj/lib/exec dirs are subdirs");
         Put_Line ("  " & Makeutl.Unchecked_Shared_Lib_Imports);
         Put_Line ("           Shared lib projects may import any project");
         New_Line;

         Put_Line ("  -aPdir   Add directory dir to project search path");
         Put_Line ("  -c       Only delete compiler generated files");
         Put_Line ("  -eL      Follow symbolic links when processing "
                   & "project files");
         Put_Line ("  -f       Force deletions of unwritable files");
         Put_Line ("  -F       Full project path name "
                   & "in brief error messages");
         Put_Line ("  -h       Display this message");
         Put_Line ("  -n       Nothing to do: only list files to delete");
         Put_Line ("  -P<proj> Use Project File <proj>");
         Put_Line ("  -q       Be quiet/terse");
         Put_Line ("  -r       Clean all projects recursively");
         Put_Line ("  -v       Verbose mode");
         Put_Line ("  -vPx     Specify verbosity when parsing Project Files");
         Put_Line ("  -Xnm=val Specify an external reference "
                   & "for Project Files");
         New_Line;
      end if;
   end Usage;

   User_Project_Node : Project_Node_Id;

begin
   --  Do the necessary initializations

   Initialize;

   --  Parse the command line, getting the switches and the executable names

   Parse_Cmd_Line;

   --  Once we have parsed the command line, we might know the target, and
   --  thus can initialize the default project path.

   if Target_Name = null then
      Prj.Env.Initialize_Default_Project_Path
        (Root_Environment.Project_Path, Target_Name => "");
   else
      Prj.Env.Initialize_Default_Project_Path
        (Root_Environment.Project_Path, Target_Name.all);
   end if;

   if Load_Standard_Base then
      Parse_Knowledge_Base (Project_Tree);
   end if;

   --  If no project file was specified, look first for a default

   if Project_File_Name = null then
      Look_For_Default_Project;
   end if;

   --  Check that a project file was specified and get the configuration

   if Project_File_Name = null then
      Display_Copyright;
      Usage;
      return;
   end if;

   if Verbose_Mode then
      Display_Copyright;
   end if;

   if Opt.Verbose_Mode then
      New_Line;
      Put ("Parsing Project File """);
      Put (Project_File_Name.all);
      Put_Line (""".");
      New_Line;
   end if;

   --  Makes the Ada RTS is absolute if it is not a base name

   if Runtime_Name_Set_For (Snames.Name_Ada) then
      Locate_Runtime (Project_Tree, Snames.Name_Ada);
   end if;

   --  Check command line arguments. These will be overridden when looking
   --  for the configuration file

   if Target_Name = null then
      Target_Name := new String'("");
   end if;

   if Config_Project_File_Name = null then
      Config_Project_File_Name := new String'("");
   end if;

   begin
      Parse_Project_And_Apply_Config
        (Main_Project               => Main_Project,
         User_Project_Node          => User_Project_Node,
         Config_File_Name           => Config_Project_File_Name.all,
         Autoconf_Specified         => Autoconf_Specified,
         Project_File_Name          => Project_File_Name.all,
         Project_Tree               => Project_Tree,
         Project_Node_Tree          => Project_Node_Tree,
         Packages_To_Check          => Packages_To_Check,
         Env                        => Root_Environment,
         Allow_Automatic_Generation => Autoconfiguration,
         Automatically_Generated    => Delete_Autoconf_File,
         Config_File_Path           => Configuration_Project_Path,
         Target_Name                => Target_Name.all,
         Normalized_Hostname        => Normalized_Hostname,
         Implicit_Project           => No_Project_File_Found);

      --  Print warnings that might have occurred while parsing the project
      Prj.Err.Finalize;

      --  But avoid duplicate warnings later on
      Prj.Err.Initialize;

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

   --  Update info on all sources

   declare
      Iter : Source_Iterator;
   begin
      Iter := For_Each_Source (Project_Tree);
      while Prj.Element (Iter) /= No_Source loop
         Initialize_Source_Record (Prj.Element (Iter));
         Next (Iter);
      end loop;
   end;

   --  Even if the config project file has not been automatically
   --  generated, gprclean will delete it if it was specified using
   --  --autoconf=.

   Delete_Autoconf_File := Delete_Autoconf_File or Autoconf_Specified;

   if Configuration_Project_Path /= null then
      Free (Config_Project_File_Name);
      Config_Project_File_Name := new String'
        (Base_Name (Configuration_Project_Path.all));
   end if;

   if Opt.Verbose_Mode then
      New_Line;
      Put ("Parsing of Project File """);
      Put (Project_File_Name.all);
      Put (""" is finished.");
      New_Line;
   end if;

   Mains.Fill_From_Project (Main_Project, Project_Tree);
   Mains.Complete_Mains
     (Root_Environment.Flags, Main_Project, Project_Tree);

   if Verbose_Mode then
      New_Line;
   end if;

   Processed_Projects.Init;

   if Slave_Env = null and then Distributed_Mode then
      Slave_Env :=
        new String'(Compute_Slave_Env (Project_Tree, Slave_Env_Auto));

      if Slave_Env_Auto and not Opt.Quiet_Output then
         Put_Line ("slave environment is " & Slave_Env.all);
      end if;
   end if;

   --  Clean-up local build

   declare
      procedure Do_Clean (Prj : Project_Id; Tree : Project_Tree_Ref);

      --------------
      -- Do_Clean --
      --------------

      procedure Do_Clean (Prj : Project_Id; Tree : Project_Tree_Ref) is
      begin
         --  For the main project and all aggregated projects, remove the
         --  binder and linker generated files.
         Clean_Project
           (Prj, Tree, Main => True, Remove_Executables => not Compile_Only);

         --  Clean-up remote slaves

         if Distributed_Mode then
            Clean_Up_Remote_Slaves (Tree, Prj);
         end if;
      end Do_Clean;

      procedure For_All is new For_Project_And_Aggregated (Do_Clean);
   begin
      --  For an aggregate project, we always cleanup all aggregated
      --  projects, whether "-r" was specified or not. But for those
      --  projects, we might not clean their imported projects.
      For_All (Main_Project, Project_Tree);
   end;

   if Delete_Autoconf_File then
      Delete ("", Configuration_Project_Path.all);
   end if;

   --  In verbose mode, if Delete has not been called, indicate that
   --  no file needs to be deleted.

   if Verbose_Mode and (not File_Deleted) then
      New_Line;

      if Do_Nothing then
         Put_Line ("No file needs to be deleted");
      else
         Put_Line ("No file has been deleted");
      end if;
   end if;
end Gprclean.Main;
