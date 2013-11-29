------------------------------------------------------------------------------
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                        G P R B U I L D . M A I N                         --
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

with Ada.Command_Line;       use Ada.Command_Line;
with Ada.Directories;
with Ada.Exceptions;         use Ada.Exceptions;

with System;
with System.Case_Util;          use System.Case_Util;
with System.Multiprocessors;    use System.Multiprocessors;

with GNAT.Directory_Operations; use GNAT.Directory_Operations;

with Atree;       use Atree;
with Csets;
with Debug;       use Debug;
with Gpr_Util;    use Gpr_Util;
with GPR_Version; use GPR_Version;
with Hostparm;
with Makeutl;     use Makeutl;
with Opt;         use Opt;
with Osint;       use Osint;
with Output;      use Output;
with Prj.Conf;    use Prj.Conf;
with Prj.Env;
with Prj.Err;
with Prj.Tree;    use Prj.Tree;
with Snames;      use Snames;
with Stringt;
with Switch;      use Switch;
with Tempdir;     use Tempdir;

with Gprbuild.Compile;
with Gprbuild.Link;
with Gprbuild.Post_Compile;
with Gprbuild.Compilation.Slave;

procedure Gprbuild.Main is

   use Gpr_Util.Knowledge;

   There_Are_Restricted_Languages : Boolean := False;

   procedure Initialize;
   --  Do the necessary package intialization and process the command line
   --  arguments.

   procedure Usage;
   --  Display the usage

   function Add_Global_Switches
     (Switch                          : String;
      For_Lang                        : Name_Id;
      For_Builder                     : Boolean;
      Has_Global_Compilation_Switches : Boolean) return Boolean;
   --  Take into account a global switch (builder or global compilation switch)
   --  read from the project file.

   procedure Add_Mains_To_Queue;
   --  Check that each main is a single file name and that it is a source
   --  of a project from the tree.

   procedure Scan_Arg
     (Arg          : String;
      Command_Line : Boolean;
      Language     : Name_Id;
      Success      : out Boolean);
   --  Process one gprbuild argument Arg. Command_Line is True if the argument
   --  is specified on the command line. Optional parameter Additional gives
   --  additional information about the origin of the argument if it is found
   --  illegal.

   procedure Add_Option (Arg : String; Command_Line : Boolean);
   --  Add a switch for a compiler or all compilers, or for the binder or for
   --  the linker. The table where this option is stored depends on the value
   --  of Current_Processor and other global variables.

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

   No_Object_Check_Switch     : constant String := "--no-object-check";
   Direct_Import_Only_Switch  : constant String := "--direct-import-only";
   Indirect_Imports_Switch    : constant String := "--indirect-imports";
   No_Indirect_Imports_Switch : constant String := "--no-indirect-imports";

   Current_Working_Dir : constant String := Get_Current_Dir;
   --  The current working directory

   type Processor is (None, Linker, Binder, Compiler);
   Current_Processor : Processor := None;
   --  This variable changes when switches -*args are used

   Current_Builder_Comp_Option_Table : Builder_Comp_Option_Table_Ref :=
                                         No_Builder_Comp_Option_Table;

   -------------------------------------------
   -- Options specified on the command line --
   -------------------------------------------

   package Options is

      type Option_Type is
        (Force_Compilations_Option,
         Keep_Going_Option,
         Maximum_Processes_Option,
         Quiet_Output_Option,
         Check_Switches_Option,
         Verbose_Mode_Option,
         Verbose_Low_Mode_Option,
         Verbose_Medium_Mode_Option,
         Warnings_Treat_As_Error,
         Warnings_Normal,
         Warnings_Suppress,
         Indirect_Imports);

      procedure Register_Command_Line_Option
        (Option : Option_Type; Value : Natural := 0);
      --  Record a command line option

      procedure Process_Command_Line_Options;
      --  Reprocess the recorded command line options that have priority over
      --  the options in package Builder of the main project.

   end Options;

   use Options;

   ------------------------
   -- Add_Mains_To_Queue --
   ------------------------

   procedure Add_Mains_To_Queue is
      Main_Id : Main_Info;

   begin
      Mains.Reset;

      loop
         Main_Id := Mains.Next_Main;
         exit when Main_Id = No_Main_Info;

         if Main_Id.Source /= No_Source then
            --  Fail if any main is declared as an excluded source file

            if Main_Id.Source.Locally_Removed then
               Fail_Program
                 (Project_Tree,
                  "main """ &
                  Get_Name_String (Main_Id.Source.File) &
                  """ cannot also be an excluded file");
            end if;

            if Is_Allowed_Language (Main_Id.Source.Language.Name) then
               Queue.Insert
                 (Source     => (Format => Format_Gprbuild,
                                 Tree   => Main_Id.Tree,
                                 Id     => Main_Id.Source),
                  With_Roots => Builder_Data (Main_Id.Tree).Closure_Needed);

               --  If a non Ada main has no roots, then all sources need to be
               --  compiled, so no need to check for closure.

               if Main_Id.Source.Language.Config.Kind /= Unit_Based
                 and then Main_Id.Source.Roots = null
               then
                  Builder_Data (Main_Id.Tree).Closure_Needed := False;
               end if;
            end if;
         end if;
      end loop;

      if Total_Errors_Detected /= 0 then
         Fail_Program (Project_Tree, "cannot continue");
      end if;

      Queue.Insert_Project_Sources
        (Project        => Main_Project,
         Project_Tree   => Project_Tree,
         Unique_Compile => Unique_Compile,
         All_Projects =>
            not Unique_Compile
            or else (Unique_Compile_All_Projects or Recursive));
   end Add_Mains_To_Queue;

   -------------------------
   -- Add_Global_Switches --
   -------------------------

   function Add_Global_Switches
     (Switch                          : String;
      For_Lang                        : Name_Id;
      For_Builder                     : Boolean;
      Has_Global_Compilation_Switches : Boolean) return Boolean
   is
      Success : Boolean;
   begin
      if For_Builder then
         if Has_Global_Compilation_Switches then
            Builder_Switches_Lang := No_Name;
         else
            Builder_Switches_Lang := For_Lang;
         end if;

         Scan_Arg
           (Switch,
            Command_Line => False,
            Language     => For_Lang,
            Success      => Success);
         return Success;

      else
         Current_Processor := Compiler;

         Current_Builder_Comp_Option_Table :=
           Builder_Compiling_Options_HTable.Get (For_Lang);

         if Current_Builder_Comp_Option_Table =
           No_Builder_Comp_Option_Table
         then
            Current_Builder_Comp_Option_Table :=
              new Builder_Compiling_Options.Instance;
            Builder_Compiling_Options_HTable.Set
              (For_Lang, Current_Builder_Comp_Option_Table);
            Builder_Compiling_Options.Init
              (Current_Builder_Comp_Option_Table.all);
         end if;

         Add_Option (Switch, Command_Line => False);

         Current_Processor := None;
         return True;
      end if;
   end Add_Global_Switches;

   ----------------
   -- Add_Option --
   ----------------

   procedure Add_Option (Arg : String; Command_Line : Boolean) is
      Option : String_Access := new String'(Arg);

   begin
      case Current_Processor is
         when None =>
            null;

         when Linker =>

            --  Add option to the linker table

            if Command_Line then
               Test_If_Relative_Path
                 (Switch           => Option,
                  Parent           => Current_Working_Dir,
                  Including_Switch => Dash_L);

            else
               Test_If_Relative_Path
                 (Switch           => Option,
                  Parent           => Main_Project_Dir.all,
                  Including_Switch => Dash_L);
            end if;

            Command_Line_Linker_Options.Append (Option);

         when Binder =>

            if Command_Line then
               Test_If_Relative_Path
                 (Switch           => Option,
                  Parent           => Current_Working_Dir,
                  Including_Switch => No_Name);

            else
               Test_If_Relative_Path
                 (Switch           => Option,
                  Parent           => Main_Project_Dir.all,
                  Including_Switch => No_Name);
            end if;

            if Current_Bind_Option_Table = No_Bind_Option_Table then
               --  Option for all binder

               All_Language_Binder_Options.Append (Option);

            else
               --  Option for a single binder

               Binder_Options.Append
                 (Current_Bind_Option_Table.all, Option);
            end if;

         when Compiler =>

            if Command_Line then
               if Current_Comp_Option_Table = No_Comp_Option_Table then
                  --  Option for all compilers

                  All_Language_Compiling_Options.Append (Option);

               else
                  --  Option for a single compiler

                  Compiling_Options.Append
                    (Current_Comp_Option_Table.all, Option);
               end if;

            else
               if Current_Builder_Comp_Option_Table =
                    No_Builder_Comp_Option_Table
               then
                  --  Option for all compilers

                  All_Language_Builder_Compiling_Options.Append (Option);

               else
                  --  Option for a single compiler

                  Builder_Compiling_Options.Append
                    (Current_Builder_Comp_Option_Table.all, Option);
               end if;
            end if;
      end case;
   end Add_Option;

   ---------------
   -- Copyright --
   ---------------

   procedure Copyright is
   begin
      --  Only output the Copyright notice once

      if not Copyright_Output then
         Copyright_Output := True;
         Display_Version
           ("GPRBUILD", "2004", Version_String => Gpr_Version_String);
      end if;
   end Copyright;

   -------------
   -- Options --
   -------------

   package body Options is

      type Option_Data is record
         Option : Option_Type;
         Value  : Natural := 0;
      end record;

      package Command_Line_Options is new Table.Table
        (Table_Component_Type => Option_Data,
         Table_Index_Type     => Natural,
         Table_Low_Bound      => 1,
         Table_Initial        => 10,
         Table_Increment      => 100,
         Table_Name           => "Makegpr.Opt.Command_Line_Options");
      --  Table to store the command line options

      ----------------------------------
      -- Process_Command_Line_Options --
      ----------------------------------

      procedure Process_Command_Line_Options is
      begin
         for Index in 1 .. Command_Line_Options.Last loop
            case Command_Line_Options.Table (Index).Option is
               when Force_Compilations_Option =>
                  Opt.Force_Compilations := True;

               when Keep_Going_Option =>
                  Opt.Keep_Going := True;

               when Maximum_Processes_Option =>
                  Opt.Maximum_Processes :=
                    Command_Line_Options.Table (Index).Value;

               when Quiet_Output_Option =>
                  Opt.Quiet_Output := True;
                  Opt.Verbose_Mode := False;

               when Check_Switches_Option =>
                  Opt.Check_Switches := True;

               when Verbose_Mode_Option =>
                  Opt.Verbose_Mode    := True;
                  Opt.Verbosity_Level := Opt.High;
                  Opt.Quiet_Output    := False;

               when Verbose_Low_Mode_Option =>
                  Opt.Verbose_Mode    := True;
                  Opt.Verbosity_Level := Opt.Low;
                  Opt.Quiet_Output    := False;

               when Verbose_Medium_Mode_Option =>
                  Opt.Verbose_Mode    := True;
                  Opt.Verbosity_Level := Opt.Medium;
                  Opt.Quiet_Output    := False;

               when Warnings_Treat_As_Error =>
                  Opt.Warning_Mode := Opt.Treat_As_Error;

               when Warnings_Normal =>
                  Opt.Warning_Mode := Opt.Normal;

               when Warnings_Suppress =>
                  Opt.Warning_Mode := Opt.Suppress;

               when Indirect_Imports =>
                  Gprbuild.Indirect_Imports :=
                    Command_Line_Options.Table (Index).Value /= 0;
            end case;
         end loop;
      end Process_Command_Line_Options;

      ----------------------------------
      -- Register_Command_Line_Option --
      ----------------------------------

      procedure Register_Command_Line_Option
        (Option : Option_Type; Value : Natural := 0)
      is
      begin
         Command_Line_Options.Increment_Last;
         Command_Line_Options.Table (Command_Line_Options.Last) :=
           (Option => Option, Value => Value);
      end Register_Command_Line_Option;

   end Options;

   --------------
   -- Scan_Arg --
   --------------

   procedure Scan_Arg
     (Arg          : String;
      Command_Line : Boolean;
      Language     : Name_Id;
      Success      : out Boolean)
   is
      Processed : Boolean := True;

      procedure Forbidden_In_Package_Builder;
      --  Fail if switch Arg is found in package Builder

      ----------------------------------
      -- Forbidden_In_Package_Builder --
      ----------------------------------

      procedure Forbidden_In_Package_Builder is
      begin
         if not Command_Line then
            Fail_Program
              (Project_Tree,
               Arg & " can only be used on the command line");
         end if;
      end Forbidden_In_Package_Builder;

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

      elsif Output_File_Name_Expected then
         if Arg (1) = '-' then
            Fail_Program
              (Project_Tree, "output file name missing after -o");
         else
            Output_File_Name_Expected := False;
            Output_File_Name := new String'(Arg);
         end if;

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

         Name_Len := 0;
         Add_Str_To_Name_Buffer (Arg);
         Db_Switch_Args.Append (Name_Find);

         --  Set the processor/language for the following switches

         --  -cargs         all compiler arguments

      elsif Arg = "-cargs" then
         Current_Processor := Compiler;

         if Command_Line then
            Current_Comp_Option_Table := No_Comp_Option_Table;

         else
            Current_Builder_Comp_Option_Table := No_Builder_Comp_Option_Table;
         end if;

         --  -cargs:lang    arguments for compiler of language lang

      elsif Arg'Length > 7 and then Arg (1 .. 7) = "-cargs:" then
         Current_Processor := Compiler;

         Name_Len := 0;
         Add_Str_To_Name_Buffer (Arg (8 .. Arg'Last));
         To_Lower (Name_Buffer (1 .. Name_Len));

         declare
            Lang : constant Name_Id := Name_Find;
         begin
            if Command_Line then
               Current_Comp_Option_Table :=
                 Compiling_Options_HTable.Get (Lang);

               if Current_Comp_Option_Table = No_Comp_Option_Table then
                  Current_Comp_Option_Table := new Compiling_Options.Instance;
                  Compiling_Options_HTable.Set
                    (Lang, Current_Comp_Option_Table);
                  Compiling_Options.Init (Current_Comp_Option_Table.all);
               end if;

            else
               Current_Builder_Comp_Option_Table :=
                 Builder_Compiling_Options_HTable.Get (Lang);

               if Current_Builder_Comp_Option_Table =
                 No_Builder_Comp_Option_Table
               then
                  Current_Builder_Comp_Option_Table :=
                    new Builder_Compiling_Options.Instance;
                  Builder_Compiling_Options_HTable.Set
                    (Lang, Current_Builder_Comp_Option_Table);
                  Builder_Compiling_Options.Init
                    (Current_Builder_Comp_Option_Table.all);
               end if;
            end if;
         end;

         --  -bargs     all binder arguments

      elsif Arg = "-bargs" then
         Current_Processor := Binder;
         Current_Bind_Option_Table := No_Bind_Option_Table;

         --  -bargs:lang    arguments for binder of language lang

      elsif Arg'Length > 7 and then Arg (1 .. 7) = "-bargs:" then
         Current_Processor := Binder;

         Name_Len := 0;
         Add_Str_To_Name_Buffer (Arg (8 .. Arg'Last));
         To_Lower (Name_Buffer (1 .. Name_Len));

         declare
            Lang : constant Name_Id := Name_Find;
         begin
            Current_Bind_Option_Table :=
              Binder_Options_HTable.Get (Lang);

            if Current_Bind_Option_Table = No_Bind_Option_Table then
               Current_Bind_Option_Table := new Binder_Options.Instance;
               Binder_Options_HTable.Set
                 (Lang, Current_Bind_Option_Table);
               Binder_Options.Init (Current_Bind_Option_Table.all);
            end if;
         end;

         --  -largs     linker arguments

      elsif Arg = "-largs" then
         Current_Processor := Linker;

         --  -gargs/margs     options directly for gprbuild
         --  support -margs for compatibility with gnatmake

      elsif Arg = "-gargs"
        or else Arg = "-margs"
      then
         Current_Processor := None;

         --  A special test is needed for the -o switch within a -largs since
         --  that is another way to specify the name of the final executable.

      elsif Command_Line
        and then Current_Processor = Linker
        and then Arg = "-o"
      then
            Fail_Program
           (Project_Tree,
            "switch -o not allowed within a -largs. Use -o directly.");

         --  If current processor is not gprbuild directly, store the option
         --  in the appropriate table.

      elsif Current_Processor /= None then
         Add_Option (Arg, Command_Line);

         --  Switches start with '-'

      elsif Arg (1) = '-' then

         if Arg'Length > Distributed_Option'Length
            and then
            Arg (1 .. Distributed_Option'Length) = Distributed_Option
         then
            Distributed_Mode := True;

            --  In distributed mode we do not want to use temp directories

            Use_Temp_Dir (Status => False);

            Compilation.Slave.Record_Slaves
              (Arg (Distributed_Option'Length + 1 .. Arg'Last));

         elsif Arg'Length >= Slave_Env_Option'Length
            and then
            Arg (1 .. Slave_Env_Option'Length) = Slave_Env_Option
         then
            if Arg = Slave_Env_Option then
               --  Just --slave-env, it is up to gprbuild to build a sensible
               --  slave environment value.
               Slave_Env_Auto := True;
            else
               Slave_Env :=
                 new String'(Arg (Slave_Env_Option'Length + 2 .. Arg'Last));
            end if;

         elsif Arg = "--db-" then
            if Hostparm.OpenVMS then
               Fail_Program
                 (Project_Tree,
                  "--db- cannot be used on VMS");
            end if;

            Forbidden_In_Package_Builder;

            Load_Standard_Base := False;

         elsif Arg = "--db" then
            if Hostparm.OpenVMS then
               Fail_Program
                 (Project_Tree,
                  "--db cannot be used on VMS");
            end if;

            Forbidden_In_Package_Builder;

            Db_Directory_Expected := True;

         elsif Arg = "--display-paths" then
            Forbidden_In_Package_Builder;
            Display_Paths := True;

         elsif Arg = "--no-split-units" then
            Opt.No_Split_Units := True;

         elsif Arg = Single_Compile_Per_Obj_Dir_Switch then
            Opt.One_Compilation_Per_Obj_Dir := True;

         elsif Arg'Length > Source_Info_Option'Length
           and then Arg (1 .. Source_Info_Option'Length) = Source_Info_Option
         then
            Forbidden_In_Package_Builder;
            Project_Tree.Source_Info_File_Name :=
               new String'(Arg (Source_Info_Option'Length + 1 .. Arg'Last));

         elsif Arg'Length > Config_Project_Option'Length
           and then
               Arg (1 .. Config_Project_Option'Length) = Config_Project_Option
         then
            if Config_Project_File_Name /= null
              and then (Autoconf_Specified
                        or else Config_Project_File_Name.all /=
                          Arg (Config_Project_Option'Length + 1 .. Arg'Last))
            then
               Fail_Program
                 (Project_Tree,
                  "several different configuration switches " &
                  "cannot be specified");

            else
               Autoconfiguration := False;
               Autoconf_Specified := False;
               Config_Project_File_Name :=
                 new String'
                   (Arg (Config_Project_Option'Length + 1 .. Arg'Last));
            end if;

         elsif Arg'Length > Autoconf_Project_Option'Length
           and then
            Arg (1 .. Autoconf_Project_Option'Length) =
              Autoconf_Project_Option
         then
            if Hostparm.OpenVMS then
               Fail_Program
                 (Project_Tree,
                  Autoconf_Project_Option & " cannot be used on VMS");
            end if;

            Forbidden_In_Package_Builder;

            if Config_Project_File_Name /= null
              and then (not Autoconf_Specified
                        or else Config_Project_File_Name.all /=
                          Arg (Autoconf_Project_Option'Length + 1 .. Arg'Last))
            then
               Fail_Program
                 (Project_Tree,
                  "several different configuration switches " &
                  "cannot be specified");

            else
               Config_Project_File_Name :=
                 new String'
                   (Arg (Autoconf_Project_Option'Length + 1 .. Arg'Last));
               Autoconf_Specified := True;
            end if;

         elsif Arg'Length > Target_Project_Option'Length
           and then
            Arg (1 .. Target_Project_Option'Length) = Target_Project_Option
         then
            if Hostparm.OpenVMS then
               Fail_Program
                 (Project_Tree,
                  Target_Project_Option & " cannot be used on VMS");
            end if;

            Forbidden_In_Package_Builder;

            if Target_Name /= null then
               if Target_Name.all /=
                 Arg (Target_Project_Option'Length + 1 .. Arg'Last)
               then
                  Fail_Program
                    (Project_Tree,
                     "several different target switches cannot be specified");
               end if;

            else
               Target_Name :=
                 new String'
                   (Arg (Target_Project_Option'Length + 1 .. Arg'Last));
            end if;

         elsif Arg'Length > RTS_Option'Length
           and then Arg (1 .. RTS_Option'Length) = RTS_Option
         then
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
               end if;

               --  Ignore any --RTS= switch in package Builder. These are only
               --  taken into account to create the config file in
               --  auto-configuration.
            end;

         elsif Arg'Length > RTS_Language_Option'Length
           and then Arg (1 .. RTS_Language_Option'Length) = RTS_Language_Option
         then
            declare
               Language_Name : Name_Id := No_Name;
               RTS_Start : Natural := Arg'Last + 1;

            begin
               for J in RTS_Language_Option'Length + 2 .. Arg'Last loop
                  if Arg (J) = '=' then
                     Name_Len := 0;
                     Add_Str_To_Name_Buffer
                       (Arg (RTS_Language_Option'Length + 1 .. J - 1));
                     To_Lower (Name_Buffer (1 .. Name_Len));
                     Language_Name := Name_Find;
                     RTS_Start := J + 1;
                     exit;
                  end if;
               end loop;

               if Language_Name = No_Name then
                  Fail_Program (Project_Tree, "illegal switch: " & Arg);

               elsif Command_Line then
                  --  Ignore any --RTS:<lang>= switch in package Builder. These
                  --  are only taken into account to create the config file in
                  --  auto-configuration.

                  declare
                     RTS : constant String := Arg (RTS_Start .. Arg'Last);
                     Set : constant Boolean :=
                       Runtime_Name_Set_For (Language_Name);
                     Old : constant String := Runtime_Name_For (Language_Name);

                  begin
                     if Set and then Old /= RTS then
                        Fail_Program
                          (Project_Tree,
                           "several different run-times cannot be specified" &
                           " for the same language");

                     else
                        Set_Runtime_For (Language_Name, RTS);
                     end if;
                  end;
               end if;
            end;

         elsif Arg'Length > Subdirs_Option'Length
           and then Arg (1 .. Subdirs_Option'Length) = Subdirs_Option
         then
            Forbidden_In_Package_Builder;
            Subdirs :=
              new String'(Arg (Subdirs_Option'Length + 1 .. Arg'Last));

         elsif Command_Line
           and then Arg'Length > Restricted_To_Languages_Option'Length
           and then Arg (1 .. Restricted_To_Languages_Option'Length) =
                      Restricted_To_Languages_Option
         then
            declare
               Start  : Positive := Restricted_To_Languages_Option'Length + 1;
               Finish : Positive;

            begin
               Processed := False;

               while Start <= Arg'Last loop
                  Finish := Start;
                  loop
                     exit when Finish > Arg'Last or else Arg (Finish) = ',';
                     Finish := Finish + 1;
                  end loop;

                  if Finish > Start then
                     Add_Restricted_Language (Arg (Start .. Finish - 1));
                     Processed := True;
                     There_Are_Restricted_Languages := True;
                  end if;

                  Start := Finish + 1;
               end loop;
            end;

         elsif Arg = Indirect_Imports_Switch then
            Indirect_Imports := True;

            if Command_Line then
               Register_Command_Line_Option (Options.Indirect_Imports, 1);
            end if;

         elsif Arg = No_Indirect_Imports_Switch
               or else
               Arg = Direct_Import_Only_Switch
         then
            Indirect_Imports := False;

            if Command_Line then
               Register_Command_Line_Option (Options.Indirect_Imports, 0);
            end if;

         elsif Arg = Makeutl.Unchecked_Shared_Lib_Imports then
            Forbidden_In_Package_Builder;
            Opt.Unchecked_Shared_Lib_Imports := True;

         elsif Arg = No_Object_Check_Switch then
            Object_Checked := False;

         elsif Arg = "--codepeer" then
            Forbidden_In_Package_Builder;

            if not CodePeer_Mode then
               CodePeer_Mode := True;
               Object_Checked := False;
               Target_Name := new String'("codepeer");

               if Subdirs = null then
                  Subdirs := new String'("codepeer");
               end if;
            end if;

         elsif Arg = Create_Map_File_Switch then
            Map_File := new String'("");

         elsif Arg'Length > Create_Map_File_Switch'Length + 1
           and then
             Arg (1 .. Create_Map_File_Switch'Length) = Create_Map_File_Switch
           and then
             Arg (Create_Map_File_Switch'Length + 1) = '='
         then
            Map_File :=
              new String'(Arg (Create_Map_File_Switch'Length + 2 .. Arg'Last));

         elsif Arg'Length >= 3 and then Arg (1 .. 3) = "-aP" then
            Forbidden_In_Package_Builder;

            if Arg'Length = 3 then
               Search_Project_Dir_Expected := True;

            else
               Prj.Env.Add_Directories
                 (Root_Environment.Project_Path, Arg (4 .. Arg'Last));
            end if;

         elsif Arg = "-b" then
            Opt.Bind_Only  := True;

         elsif Arg = "-c" then
            Opt.Compile_Only := True;

            if Opt.Link_Only then
               Opt.Bind_Only  := True;
            end if;

         elsif Arg = "-C" then
            --  This switch is only for upward compatibility

            null;

         elsif Arg = "-d" then
            Opt.Display_Compilation_Progress := True;

         elsif Arg'Length = 3 and then Arg (2) = 'd' then
            if Arg (3) in '1' .. '9'
              or else Arg (3) in 'a' .. 'z'
              or else Arg (3) in 'A' .. 'Z'
            then
               Set_Debug_Flag (Arg (3));

            else
               Fail_Program
                 (Project_Tree, "illegal debug switch " & Arg);
            end if;

         elsif Arg'Length > 3 and then Arg (1 .. 3) = "-eI" then
            Forbidden_In_Package_Builder;

            begin
               Main_Index := Int'Value (Arg (4 .. Arg'Last));

            exception
               when Constraint_Error =>
                  Fail_Program (Project_Tree, "invalid switch " & Arg);
            end;

         elsif Arg = "-eL" then
            Forbidden_In_Package_Builder;
            Opt.Follow_Links_For_Files := True;
            Opt.Follow_Links_For_Dirs  := True;

         elsif Arg = "-eS" then
            Forbidden_In_Package_Builder;

            --  Accept switch for compatibility with gnatmake

            Opt.Commands_To_Stdout := True;

         elsif Arg = "-f" then
            Opt.Force_Compilations := True;

            if Command_Line then
               Register_Command_Line_Option (Force_Compilations_Option);
            end if;

         elsif Arg = "-F" then
            Forbidden_In_Package_Builder;
            Opt.Full_Path_Name_For_Brief_Errors := True;

         elsif Arg = "-h" then
            Forbidden_In_Package_Builder;
            Usage_Needed := True;

         elsif Arg'Length > 2 and then Arg (2) = 'j' then
            declare
               Max_Proc : Natural := 0;
            begin
               for J in 3 .. Arg'Length loop
                  if Arg (J) in '0' .. '9' then
                     Max_Proc := (Max_Proc * 10) +
                       Character'Pos (Arg (J)) -
                       Character'Pos ('0');

                  else
                     Processed := False;
                  end if;
               end loop;

               if Processed then
                  if Max_Proc = 0 then
                     Max_Proc := Natural (Number_Of_CPUs);
                  end if;

                  if Max_Proc = 0 then
                     Max_Proc := 1;
                  end if;

                  Opt.Maximum_Processes := Max_Proc;
               end if;
            end;

            if Processed and then Command_Line then
               Register_Command_Line_Option
                 (Maximum_Processes_Option, Opt.Maximum_Processes);
            end if;

         elsif Arg = "-k" then
            Opt.Keep_Going := True;

            if Command_Line then
               Register_Command_Line_Option (Keep_Going_Option);
            end if;

         elsif Arg = "-l" then
            Opt.Link_Only  := True;

            if Opt.Compile_Only then
               Opt.Bind_Only := True;
            end if;

         elsif Arg = "-m" then
            Opt.Minimal_Recompilation := True;

         elsif Arg = "-o" then
            Forbidden_In_Package_Builder;

            if Output_File_Name /= null then
               Fail_Program
                 (Project_Tree, "cannot specify several -o switches");

            else
               Output_File_Name_Expected := True;
            end if;

         elsif Arg = "-p" or else Arg = "--create-missing-dirs" then
            Forbidden_In_Package_Builder;
            Opt.Setup_Projects := True;

         elsif Arg'Length >= 2 and then Arg (2) = 'P' then
            Forbidden_In_Package_Builder;

            if Project_File_Name /= null then
               Fail_Program
                 (Project_Tree,
                  "cannot have several project files specified");

            elsif Arg'Length = 2 then
               Project_File_Name_Expected := True;

            else
               Project_File_Name := new String'(Arg (3 .. Arg'Last));
            end if;

         elsif Arg = "-q" then
            Opt.Quiet_Output := True;
            Opt.Verbose_Mode := False;

            if Command_Line then
               Register_Command_Line_Option (Quiet_Output_Option);
            end if;

         elsif Arg = "-r" then
            Forbidden_In_Package_Builder;
            Recursive := True;

         elsif Arg = "-R" then
            Opt.Run_Path_Option := False;

         elsif Arg = "-s" then
            Opt.Check_Switches := True;

            if Command_Line then
               Register_Command_Line_Option (Check_Switches_Option);
            end if;

         elsif Arg = "-u" then
            Forbidden_In_Package_Builder;
            Unique_Compile := True;

         elsif Arg = "-U" then
            Forbidden_In_Package_Builder;
            Unique_Compile_All_Projects := True;
            Unique_Compile := True;

         elsif Arg = "-v" or else Arg = "-vh" then
            Opt.Verbose_Mode    := True;
            Opt.Verbosity_Level := Opt.High;
            Opt.Quiet_Output    := False;

            if Command_Line then
               Register_Command_Line_Option (Verbose_Mode_Option);
            end if;

         elsif Arg = "-vl" then
            Opt.Verbose_Mode    := True;
            Opt.Verbosity_Level := Opt.Low;
            Opt.Quiet_Output    := False;

            if Command_Line then
               Register_Command_Line_Option (Verbose_Low_Mode_Option);
            end if;

         elsif Arg = "-vm" then
            Opt.Verbose_Mode    := True;
            Opt.Verbosity_Level := Opt.Medium;
            Opt.Quiet_Output    := False;

            if Command_Line then
               Register_Command_Line_Option (Verbose_Medium_Mode_Option);
            end if;

         elsif Arg'Length >= 3 and then Arg (1 .. 3) = "-vP" then
            Forbidden_In_Package_Builder;

            if Arg'Length = 4 and then  Arg (4) in '0' .. '2' then
               case Arg (4) is
               when '0' =>
                  Current_Verbosity := Prj.Default;
               when '1' =>
                  Current_Verbosity := Prj.Medium;
               when '2' =>
                  Current_Verbosity := Prj.High;
               when others =>
                  null;
               end case;

            else
               Fail_Program
                 (Project_Tree,
                  "invalid verbosity level " & Arg (4 .. Arg'Last));
            end if;

         elsif Arg = "-we" then
            Opt.Warning_Mode := Opt.Treat_As_Error;

            if Command_Line then
               Register_Command_Line_Option (Warnings_Treat_As_Error);
            end if;

         elsif Arg = "-wn" then
            Opt.Warning_Mode := Opt.Normal;

            if Command_Line then
               Register_Command_Line_Option (Warnings_Normal);
            end if;

         elsif Arg = "-ws" then
            Opt.Warning_Mode  := Opt.Suppress;

            if Command_Line then
               Register_Command_Line_Option (Warnings_Suppress);
            end if;

         elsif Arg = "-x" then
            Opt.Use_Include_Path_File := True;

         elsif Arg'Length >= 3
           and then Arg (2) = 'X'
           and then Is_External_Assignment (Root_Environment, Arg)
         then
            Forbidden_In_Package_Builder;

            --  Is_External_Assignment has side effects when it returns True

            null;

         elsif (Language = No_Name or else Language = Name_Ada)
           and then (not Command_Line)
           and then Arg = "-x"
         then
            --  For compatibility with gnatmake, ignore -x if found in the
            --  Builder switches.

            null;

         elsif (Language = No_Name or else Language = Name_Ada)
            and then
             (Arg = "-fstack-check"
              or else Arg = "-fno-inline"
              or else
                (Arg'Length >= 2
                 and then (Arg (2) = 'O' or else Arg (2) = 'g')))
         then
            --  For compatibility with gnatmake, use switch to compile Ada
            --  code.

            if Command_Line then
               Current_Comp_Option_Table :=
                 Compiling_Options_HTable.Get (Name_Ada);

               if Current_Comp_Option_Table = No_Comp_Option_Table then
                  Current_Comp_Option_Table := new Compiling_Options.Instance;
                  Compiling_Options_HTable.Set
                    (Name_Ada, Current_Comp_Option_Table);
                  Compiling_Options.Init (Current_Comp_Option_Table.all);
               end if;

            else
               Current_Builder_Comp_Option_Table :=
                 Builder_Compiling_Options_HTable.Get (Name_Ada);

               if Current_Builder_Comp_Option_Table =
                  No_Builder_Comp_Option_Table
               then
                  Current_Builder_Comp_Option_Table :=
                    new Builder_Compiling_Options.Instance;
                  Builder_Compiling_Options_HTable.Set
                    (Name_Ada, Current_Builder_Comp_Option_Table);
                  Builder_Compiling_Options.Init
                    (Current_Builder_Comp_Option_Table.all);
               end if;
            end if;

            Current_Processor := Compiler;
            Add_Option (Arg, Command_Line);
            Current_Processor := None;

         elsif (Language = No_Name or else Language = Name_Ada)
            and then
             (Arg = "-nostdlib" or else Arg = "-nostdinc")
         then
            --  For compatibility with gnatmake, use switch to bind Ada code
            --  code and for -nostdlib to link.

            Current_Bind_Option_Table :=
              Binder_Options_HTable.Get (Name_Ada);

            if Current_Bind_Option_Table = No_Bind_Option_Table then
               Current_Bind_Option_Table := new Binder_Options.Instance;
               Binder_Options_HTable.Set
                 (Name_Ada, Current_Bind_Option_Table);
               Binder_Options.Init (Current_Bind_Option_Table.all);
            end if;

            Current_Processor := Binder;
            Add_Option (Arg, Command_Line);

            --  For -nostdlib, use the switch to link too

            if Arg = "-nostdlib" then
               Current_Processor := Linker;
               Add_Option (Arg, Command_Line);
            end if;

            Current_Processor := None;

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

               Mains.Add_Main (Arg);
               Always_Compile := True;
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

         else
            --  If we have a switch and there is a Builder Switches language
            --  set, pass this switch to the compiler of the language.

            if Arg (1) = '-' and then Builder_Switches_Lang /= No_Name then
               Current_Builder_Comp_Option_Table :=
                 Builder_Compiling_Options_HTable.Get (Builder_Switches_Lang);

               if Current_Builder_Comp_Option_Table =
                 No_Builder_Comp_Option_Table
               then
                  Current_Builder_Comp_Option_Table :=
                    new Builder_Compiling_Options.Instance;
                  Builder_Compiling_Options_HTable.Set
                    (Builder_Switches_Lang, Current_Builder_Comp_Option_Table);
                  Builder_Compiling_Options.Init
                    (Current_Builder_Comp_Option_Table.all);
               end if;

               Current_Processor := Compiler;
               Add_Option (Arg, False);
               Current_Processor := None;

            else
               Success := False;
            end if;
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

      if Distributed_Mode then
         Compilation.Slave.Unregister_Remote_Slaves;
      end if;

      OS_Exit (1);
   end Sigint_Intercepted;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      procedure Check_Version_And_Help is new
        Check_Version_And_Help_G (Usage);
   begin
      --  Do some necessary package initializations

      Csets.Initialize;
      Namet.Initialize;
      Snames.Initialize;
      Stringt.Initialize;

      Prj.Tree.Initialize (Root_Environment, Gprbuild_Flags);
      Prj.Tree.Initialize (Project_Node_Tree);

      Prj.Initialize (Project_Tree);
      Mains.Delete;

      --  Get the name id for "-L";

      Name_Len := 0;
      Add_Str_To_Name_Buffer ("-L");
      Dash_L := Name_Find;

      --  Get the command line arguments, starting with --version and --help

      Check_Version_And_Help
        ("GPRBUILD",
         "2004",
         Version_String => Gpr_Version_String);

      --  Now process the other options

      Autoconfiguration := True;

      declare
         Do_Not_Care : Boolean;

      begin
         Scan_Args : for Next_Arg in 1 .. Argument_Count loop
            Scan_Arg
              (Argument (Next_Arg),
               Command_Line => True,
               Language     => No_Name,
               Success      => Do_Not_Care);
         end loop Scan_Args;
      end;

      if CodePeer_Mode then
         if There_Are_Restricted_Languages then
            Remove_All_Restricted_Languages;
         end if;

         Add_Restricted_Language ("ada");

         Opt.Link_Only := False;

         if not Opt.Compile_Only and not Opt.Bind_Only then
            Opt.Compile_Only := True;
            Opt.Bind_Only    := True;
         end if;

      elsif There_Are_Restricted_Languages then
         Opt.Compile_Only := True;
         Opt.Bind_Only    := False;
         Opt.Link_Only    := False;
      end if;

      Mains.Set_Multi_Unit_Index (Project_Tree, Main_Index);

      Current_Processor := None;

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

      --  If --display-paths was specified, display the config and the user
      --  project paths and exit.

      if Display_Paths then
         Write_Char ('.');

         declare
            Prefix_Path : constant String := Executable_Prefix_Path;
            P           : String_Access;

         begin
            if Prefix_Path'Length /= 0 then
               Write_Char (Path_Separator);
               Write_Str (Prefix_Path);
               Write_Str ("share");
               Write_Char (Directory_Separator);
               Write_Str ("gpr");
            end if;

            Write_Eol;

            Prj.Env.Get_Path (Root_Environment.Project_Path, Path => P);
            Write_Line (P.all);
            Exit_Program (E_Success);
         end;
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

         --  Or if it ended with "-o"

      elsif Output_File_Name_Expected then
         Fail_Program
           (Project_Tree, "output file name missing after -o");

         --  Or if it ended with "-aP"

      elsif Search_Project_Dir_Expected then
         Fail_Program
           (Project_Tree, "directory name missing after -aP");

      elsif Db_Directory_Expected then
         Fail_Program
           (Project_Tree, "directory name missing after --db");

      elsif Slave_Env /= null and then not Distributed_Mode then
         Fail_Program
           (Project_Tree, "cannot use --slave-env in non distributed mode");
      end if;

      --  Makes the Ada RTS is absolute if it is not a base name

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

      --  If no project file is specified, look for a default

      if Project_File_Name = null then
         Look_For_Default_Project;

      else
         No_Project_File_Found := False;
      end if;

      if Project_File_Name = null then
         Copyright;
         Usage;
         Fail_Program
           (Project_Tree,
            "no project file specified and no default project file");
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
         Write_Str (" [-P<proj>] [<proj>.gpr] [opts] [name]");
         Write_Eol;
         Write_Str ("    {[-cargs opts] [-cargs:lang opts] [-largs opts]" &
                    " [-gargs opts]}");
         Write_Eol;
         Write_Eol;
         Write_Str ("  name is zero or more file names");
         Write_Eol;
         Write_Eol;

         --  GPRBUILD switches

         Write_Str ("gprbuild switches:");
         Write_Eol;

         Display_Usage_Version_And_Help;

         --  Line for --distributed

         Write_Str ("  --distributed=slave1[,slave2]");
         Write_Eol;
         Write_Str ("           Activate the remote/distributed compilations");
         Write_Eol;

         --  Line for --slave-env

         Write_Str ("  --slave-env[=name]");
         Write_Eol;
         Write_Str ("           Use a specific slave's environment");
         Write_Eol;

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

         --  Line for Target_Project_Option

         if not Hostparm.OpenVMS then
            Write_Str ("  ");
            Write_Str (Target_Project_Option);
            Write_Str ("targetname");
            Write_Eol;
            Write_Str
              ("           Specify a target for cross platforms");
            Write_Eol;
         end if;

         --  Line for --db

         if not Hostparm.OpenVMS then
            Write_Str ("  --db dir Parse dir as an additional knowledge base");
            Write_Eol;
         end if;

         --  Line for --db-

         if not Hostparm.OpenVMS then
            Write_Str ("  --db-    Do not load the standard knowledge base");
            Write_Eol;
         end if;

         --  Line for --subdirs=

         Write_Str ("  --subdirs=dir");
         Write_Eol;
         Write_Str ("           Real obj/lib/exec dirs are subdirs");
         Write_Eol;

         --  Line for --single-compile-per-obj-dir

         Write_Str ("  ");
         Write_Str (Single_Compile_Per_Obj_Dir_Switch);
         Write_Eol;
         Write_Str
           ("           No simultaneous compilations for the same obj dir");
         Write_Eol;

         Write_Str ("  ");
         Write_Str (No_Indirect_Imports_Switch);
         Write_Eol;
         Write_Str
           ("           Sources can import only from directly imported " &
            "projects");
         Write_Eol;

         Write_Str ("  ");
         Write_Str (Indirect_Imports_Switch);
         Write_Eol;
         Write_Str
           ("           Sources can import from directly and indirectly " &
            "imported projects");
         Write_Eol;

         Write_Str ("  --RTS=<runtime>");
         Write_Eol;
         Write_Str ("           Use runtime <runtime> for language Ada");
         Write_Eol;

         Write_Str ("  --RTS:<lang>=<runtime>");
         Write_Eol;
         Write_Str ("           Use runtime <runtime> for language <lang>");
         Write_Eol;

         Write_Str ("  ");
         Write_Str (Makeutl.Unchecked_Shared_Lib_Imports);
         Write_Eol;
         Write_Str ("           Shared lib projects may import any project");
         Write_Eol;

         Write_Str ("  ");
         Write_Str (No_Object_Check_Switch);
         Write_Eol;
         Write_Str ("           Do not check object files");
         Write_Eol;

         Write_Str ("  ");
         Write_Str (Restricted_To_Languages_Option);
         Write_Str ("<list of languages>");
         Write_Eol;
         Write_Str ("           Restrict the languages of the sources");
         Write_Eol;
         Write_Eol;

         Write_Str ("  ");
         Write_Str (Create_Map_File_Switch);
         Write_Eol;
         Write_Str ("           Create map file mainprog.map");
         Write_Eol;
         Write_Str ("  ");
         Write_Str (Create_Map_File_Switch);
         Write_Str ("=mapfile");
         Write_Eol;
         Write_Str ("           Create map file mapfile");
         Write_Eol;
         Write_Eol;

         --  Line for -aP

         Write_Str ("  -aP dir  Add directory dir to project search path");
         Write_Eol;

         --  Line for -b

         Write_Str ("  -b       Bind only");
         Write_Eol;

         --  Line for -c

         Write_Str ("  -c       Compile only");
         Write_Eol;

         --  Line for -d

         Write_Str ("  -d       Display progress");
         Write_Eol;

         --  Line for -eInn

         Write_Str ("  -eInn    Index of main unit in multi-unit source file");
         Write_Eol;

         --  Line for -eL

         Write_Str ("  -eL      " &
                    "Follow symbolic links when processing project files");
         Write_Eol;

         --  Line for -eS

         Write_Str ("  -eS      " &
                    "(no action, for compatibility with gnatmake only)");
         Write_Eol;

         --  Line for -f

         Write_Str ("  -f       Force recompilations");
         Write_Eol;

         --  Line for -F

         Write_Str
           ("  -F       Full project path name in brief error messages");
         Write_Eol;

         --  Line for -jnnn

         Write_Str ("  -jnum    Use num processes to compile");
         Write_Eol;

         --  Line for -k

         Write_Str ("  -k       Keep going after compilation errors");
         Write_Eol;

         --  Line for -l

         Write_Str ("  -l       Link only");
         Write_Eol;

         --  Line for -m

         Write_Str ("  -m       Minimum Ada recompilation");
         Write_Eol;

         --  Line for -o

         Write_Str ("  -o name  Choose an alternate executable name");
         Write_Eol;

         --  Line for -p

         Write_Str ("  -p       Create missing obj, lib and exec dirs");
         Write_Eol;

         --  Line for -P

         Write_Str ("  -P proj  Use Project File proj");
         Write_Eol;

         --  Line for -q

         Write_Str ("  -q       Be quiet/terse");
         Write_Eol;

         --  Line for -r

         Write_Str ("  -r       Recursive (default except when using -c)");
         Write_Eol;

         --  Line for -R

         Write_Str ("  -R       Do not use run path option");
         Write_Eol;

         --  Line for -s

         Write_Str ("  -s       Recompile if compiler switches have changed");
         Write_Eol;

         --  Line for -u

         Write_Str
           ("  -u       Unique compilation, only compile the given files");
         Write_Eol;

         --  Line for -U

         Write_Str
           ("  -U       Unique compilation for all sources of all projects");
         Write_Eol;

         --  Line for -v

         Write_Str ("  -v       Verbose output");
         Write_Eol;

         --  Line for -vl

         Write_Str ("  -vl      Verbose output (low verbosity)");
         Write_Eol;

         --  Line for -vm

         Write_Str ("  -vm      Verbose output (medium verbosity)");
         Write_Eol;

         --  Line for -vh

         Write_Str ("  -vh      Verbose output (high verbosity)");
         Write_Eol;

         --  Line for -vPx

         Write_Str ("  -vPx     Specify verbosity when parsing Project Files" &
                    " (x = 0/1/2)");
         Write_Eol;

         --  Line for -we

         Write_Str ("  -we      Treat all warnings as errors");
         Write_Eol;

         --  Line for -wn

         Write_Str ("  -wn      Treat warnings as warnings");
         Write_Eol;

         --  Line for -ws

         Write_Str ("  -ws      Suppress all warnings");
         Write_Eol;

         --  Line for -x

         Write_Str ("  -x       Always create include path file");
         Write_Eol;

         --  Line for -X

         Write_Str ("  -Xnm=val Specify an external reference for " &
                    "Project Files");
         Write_Eol;
         Write_Eol;

         --  Line for -cargs

         Write_Line ("  -cargs opts    opts are passed to all compilers");

         --  Line for -cargs:lang

         Write_Line ("  -cargs:<lang> opts");
         Write_Line ("                 opts are passed to the compiler " &
                     "for language <lang> ");

         --  Line for -bargs

         Write_Line ("  -bargs opts    opts are passed to all binders");

         --  Line for -cargs:lang

         Write_Line ("  -bargs:<lang> opts");
         Write_Line ("                 opts are passed to the binder " &
                     "for language <lang> ");

         --  Line for -largs

         Write_Str ("  -largs opts    opts are passed to the linker");
         Write_Eol;

         --  Line for -gargs

         Write_Str ("  -gargs opts    opts directly interpreted by gprbuild");
         Write_Eol;

         --  Line for -margs

         Write_Str ("  -margs opts    equivalent to -gargs opts");
         Write_Eol;

         Write_Eol;

         Write_Str
           ("For compatibility with gnatmake, these switches are passed " &
            "to the Ada compiler:");
         Write_Eol;

         Write_Str ("  -nostdlib");
         Write_Eol;

         Write_Str ("  -nostdinc");
         Write_Eol;

         Write_Str ("  -fstack-check");
         Write_Eol;

         Write_Str ("  -fno-inline");
         Write_Eol;

         Write_Str ("  -gxxx");
         Write_Eol;

         Write_Str ("  -Oxx");
         Write_Eol;

         Write_Eol;
      end if;
   end Usage;

   User_Project_Node : Project_Node_Id;

   procedure Do_Compute_Builder_Switches is
      new Compute_Builder_Switches (Add_Global_Switches);

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

   elsif Autoconf_Specified then
      --  Check if path needs to be created

      declare
         Config_Path : constant String :=
                         Ada.Directories.Containing_Directory
                           (Config_Project_File_Name.all);
      begin
         if not Ada.Directories.Exists (Config_Path) then
            Ada.Directories.Create_Path (Config_Path);
         end if;
      end;
   end if;

   --  Then, parse the user's project and the configuration file. Apply the
   --  configuration file to the project so that its settings are
   --  automatically inherited by the project.
   --  If either the project or the configuration file contains errors, the
   --  following call with call Osint.Fail and never return

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
         Normalized_Hostname        => Normalized_Hostname,
         Implicit_Project           => No_Project_File_Found);
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

   Compute_All_Imported_Projects (Main_Project, Project_Tree);

   if Mains.Number_Of_Mains (Project_Tree) = 0
     and then not Unique_Compile
   then
      --  Register the Main units from the projects.
      --  No need to waste time when we are going to compile all files
      --  anyway (Unique_Compile).
      Mains.Fill_From_Project (Main_Project, Project_Tree);
   end if;

   Mains.Complete_Mains
     (Root_Environment.Flags, Main_Project, Project_Tree);

   if not Unique_Compile
     and then Output_File_Name /= null
     and then Mains.Number_Of_Mains (null) > 1
   then
      Fail_Program
        (Project_Tree, "cannot specify -o when there are several mains");
   end if;

   Do_Compute_Builder_Switches
     (Project_Tree     => Project_Tree,
      Root_Environment => Root_Environment,
      Main_Project     => Main_Project);

   Queue.Initialize (Opt.One_Compilation_Per_Obj_Dir);

   Compute_Compilation_Phases
     (Project_Tree,
      Main_Project,
      Option_Unique_Compile => Unique_Compile,
      Option_Compile_Only   => Opt.Compile_Only,
      Option_Bind_Only      => Opt.Bind_Only,
      Option_Link_Only      => Opt.Link_Only);

   if Mains.Number_Of_Mains (Project_Tree) > 0
     and then Main_Project.Library
     and then Builder_Data (Project_Tree).Need_Binding
   then
      Fail_Program
        (Project_Tree,
         "cannot specify a main program " &
           "on the command line for a library project file");
   end if;

   Add_Mains_To_Queue;

   --  If no sources to compile, then there is nothing to do

   if Queue.Size = 0 then
      if not Opt.Quiet_Output
        and then not Main_Project.Externally_Built
      then
         Osint.Write_Program_Name;
         Write_Line (": no sources to compile");
      end if;

      Finish_Program (Project_Tree, E_Success);
   end if;

   Always_Compile :=
     Always_Compile
     and then Opt.Force_Compilations
     and then Unique_Compile
     and then not Unique_Compile_All_Projects;

   --  Reprocess recorded command line options that have priority over
   --  those in the main project file.

   Options.Process_Command_Line_Options;

   if Debug.Debug_Flag_M then
      Write_Line ("Maximum number of simultaneous compilations =" &
                    Opt.Maximum_Processes'Img);
   end if;

   --  Warn if --create-map-file is not supported

   if Map_File /= null
     and then Main_Project.Config.Map_File_Option = No_Name
   then
      Write_Str ("warning: option ");
      Write_Str (Create_Map_File_Switch);
      Write_Str (" is not supported in this configuration");
      Write_Eol;
   end if;

   --  Source file lookups should be cached for efficiency.
   --  Source files are not supposed to change.

   Osint.Source_File_Data (Cache => True);

   --  If switch --no-object-check is used, then there is no check for the
   --  switches.

   if not Object_Checked then
      Opt.Check_Switches := False;
   end if;

   --  Set slave-env

   if Slave_Env = null and then Distributed_Mode then
      Slave_Env :=
        new String'(Compute_Slave_Env (Project_Tree, Slave_Env_Auto));

      if Slave_Env_Auto and not Opt.Quiet_Output then
         Write_Str ("slave environment is ");
         Write_Str (Slave_Env.all);
         Write_Eol;
      end if;
   end if;

   Compile.Run;
   Post_Compile.Run;
   Link.Run;

   if Warnings_Detected /= 0 then
      Prj.Err.Finalize;
   end if;

   Namet.Finalize;

   Finish_Program (Project_Tree, E_Success);

exception
   when E : others =>
      Osint.Fail (Exception_Information (E));
end Gprbuild.Main;
