------------------------------------------------------------------------------
--                                                                          --
--                             GPR TECHNOLOGY                               --
--                                                                          --
--                       Copyright (C) 2015, AdaCore                        --
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
with Ada.Text_IO;             use Ada.Text_IO;

with GNAT.Command_Line; use GNAT.Command_Line;

with GPR.Conf;  use GPR.Conf;
with GPR.Env;   use GPR.Env;
with GPR.Names; use GPR.Names;
with GPR.Opt;   use GPR.Opt;
with GPR.Osint;
with GPR.Snames;
with GPR.Tree;
with GPR.Util;  use GPR.Util;

with Gpr_Build_Util; use Gpr_Build_Util;
with Gpr_Util;       use Gpr_Util;
with GprConfig.Sdefault;
with GPR_Version;    use GPR_Version;

procedure Gprls.Main is

   use GPR;

   File_Set : Boolean := False;
   --  Set to True by -P switch.
   --  Used to detect multiple -P switches.

   Print_Usage : Boolean := False;
   --  Set to True with switch -h

   Project_File_Name_Expected : Boolean := False;
   --  True when switch "-P" has just been scanned

   Path_Name : String_Access;

   Path_Last : Natural;

   Output_Name      : String_Access;

   User_Project_Node : Project_Node_Id;

   No_Project_File_Specified : Boolean := False;

   All_Projects : Boolean := False;

   procedure Initialize;

   procedure Scan_Arg (Argv : String);
   --  Scan and process user specific arguments (Argv is a single argument)

   procedure Usage;
   --  Print usage message

   type Path_Record;
   type Path_Access is access Path_Record;
   type Path_record is record
      Path : String_Access := null;
      Next : Path_Access   := null;
   end record;

   type Paths is record
      First : Path_Access := null;
      Last  : Path_Access := null;
   end record;
   No_Paths : constant Paths := (null, null);

   procedure Add (Path : String; To : in out Paths);

   procedure Get_Source_Dirs
     (Project    : Project_Id;
      Tree       : Project_Tree_Ref;
      With_State : in out Paths);

   procedure Get_All_Source_Dirs is
     new For_Every_Project_Imported (Paths, Get_Source_Dirs);

   procedure Get_Runtime_Source_Dirs
     (Project    : Project_Id;
      Tree       : Project_Tree_Ref;
      With_State : in out Paths);

   procedure Get_All_Runtime_Source_Dirs is
      new For_Every_Project_Imported (Paths, Get_Runtime_Source_Dirs);

   procedure Get_Object_Dirs
     (Project    : Project_Id;
      Tree       : Project_Tree_Ref;
      With_State : in out Paths);

   procedure Get_All_Object_Dirs is
        new For_Every_Project_Imported (Paths, Get_Object_Dirs);

   procedure Get_Runtime_Object_Dirs
     (Project    : Project_Id;
      Tree       : Project_Tree_Ref;
      With_State : in out Paths);

   procedure Get_All_Runtime_Object_Dirs is
      new For_Every_Project_Imported (Paths, Get_Runtime_Object_Dirs);

   procedure Display_Paths;
   --  Display source, object and project paths

   ---------
   -- Add --
   ---------

   procedure Add (Path : String; To : in out Paths) is
      Cur : Path_Access := To.First;
   begin
      while Cur /= null loop
         if Cur.Path.all = Path then
            return;
         end if;

         Cur := Cur.Next;
      end loop;

      declare
         New_Path : constant Path_Access :=
           new Path_Record'(Path => new String'(Path), Next => null);
      begin
         if To = No_Paths then
            To := (New_Path, New_Path);

         else
            To.Last.Next := New_Path;
            To.Last := New_Path;
         end if;
      end;
   end Add;

   -------------------
   -- Display_Paths --
   -------------------

   procedure Display_Paths is
      Source_Paths : Paths := No_Paths;
      Object_Paths : Paths := No_Paths;

      Path : Path_Access;
   begin
      New_Line;
      Display_Version
        ("GPRLS", "2015", Version_String => Gpr_Version_String);

      New_Line;
      Put_Line ("Source Search Path:");

      --  First the source directories

      Get_All_Source_Dirs (Main_Project, Project_Tree, Source_Paths);

      --  Then the runtime source directories, if any

      Get_All_Runtime_Source_Dirs (Main_Project, Project_Tree, Source_Paths);

      Path := Source_Paths.First;
      while Path /= null loop
         Put_Line ("   " & Path.Path.all);
         Path := Path.Next;
      end loop;

      New_Line;
      Put_Line ("Object Search Path:");

      --  First the object directories

      Get_All_Object_Dirs (Main_Project, Project_Tree, Object_Paths);

      --  Then the runtime library directories, if any

      Get_All_Runtime_Object_Dirs (Main_Project, Project_Tree, Object_Paths);

      Path := Object_Paths.First;
      while Path /= null loop
         Put_Line ("   " & Path.Path.all);
         Path := Path.Next;
      end loop;

      New_Line;
      Put_Line ("Project Search Path:");

      declare
         Path : String_Access;
         First : Positive;
         Last  : Natural;
      begin
         Put_Line ("   <Current_Directory>");

         GPR.Env.Get_Path (Root_Environment.Project_Path, Path);

         if Path /= null then
            First := Path'First;
            while First < Path'Last loop
               Last := First;
               while Last < Path'Last and then
                 Path (Last + 1) /= Path_Separator
               loop
                  Last := Last + 1;
               end loop;

               if Path (First .. Last) /= "." then
                  Put_Line ("   " & Path (First .. Last));
               end if;

               First := Last + 1;
               while First < Path'Last and then
                 Path (First) = Path_Separator
               loop
                  First := First + 1;
               end loop;
            end loop;
         end if;
      end;

      New_Line;
   end Display_Paths;

   ---------------------
   -- Get_Object_Dirs --
   ---------------------

   procedure Get_Object_Dirs
     (Project    : Project_Id;
      Tree       : Project_Tree_Ref;
      With_State : in out Paths)
   is
      pragma Unreferenced (Tree);
      Name : constant Path_Name_Type := Project.Object_Directory.Display_Name;
   begin
      if Name /= No_Path then
         Add (Get_Name_String (Name),  With_State);
      end if;
   end Get_Object_Dirs;

   -----------------------------
   -- Get_Runtime_Object_Dirs --
   -----------------------------

   procedure Get_Runtime_Object_Dirs
     (Project    : Project_Id;
      Tree       : Project_Tree_Ref;
      With_State : in out Paths)
   is
      pragma Unreferenced (Tree);
      List : Language_Ptr := Project.Languages;
   begin
      while List /= No_Language_Index loop
         if List.Config.Runtime_Source_Dir /= No_Name then
            Add
              (Get_Name_String (List.Config.Runtime_Library_Dir),
               With_State);
         end if;

         List := List.Next;
      end loop;
   end Get_Runtime_Object_Dirs;

   -----------------------------
   -- Get_Runtime_Source_Dirs --
   -----------------------------

   procedure Get_Runtime_Source_Dirs
     (Project    : Project_Id;
      Tree       : Project_Tree_Ref;
      With_State : in out Paths)
   is
      pragma Unreferenced (Tree);
      List : Language_Ptr := Project.Languages;
   begin
      while List /= No_Language_Index loop
         if List.Config.Runtime_Source_Dir /= No_Name then
            Add (Get_Name_String (List.Config.Runtime_Source_Dir), With_State);
         end if;

         List := List.Next;
      end loop;
   end Get_Runtime_Source_Dirs;

   ---------------------
   -- Get_Source_Dirs --
   ---------------------

   procedure Get_Source_Dirs
     (Project    : Project_Id;
      Tree       : Project_Tree_Ref;
      With_State : in out Paths)
   is
      Source_Dirs : String_List_Id := Project.Source_Dirs;
   begin
      while Source_Dirs /= Nil_String loop
         Add
           (Get_Name_String
              (Tree.Shared.String_Elements.Table (Source_Dirs).Display_Value),
            With_State);
         Source_Dirs := Tree.Shared.String_Elements.Table (Source_Dirs).Next;
      end loop;
   end Get_Source_Dirs;

   --------------
   -- Scan_Arg --
   --------------

   procedure Scan_Arg (Argv : String) is
      FD  : File_Descriptor;
      Len : Integer;
      OK  : Boolean;

   begin
      pragma Assert (Argv'First = 1);

      if Argv'Length = 0 then
         return;
      end if;

      OK := True;

      --  -P xxx

      if Project_File_Name_Expected then
         if Argv (1) = '-' then
            Fail ("project file name missing");

         else
            File_Set                   := True;
            Project_File_Name          := new String'(Argv);
            Project_File_Name_Expected := False;
         end if;

      elsif Argv (1) = '-' then
         if Argv'Length = 1 then
            Fail ("switch character '-' cannot be followed by a blank");

         --  Forbid -?- or -??- where ? is any character

         elsif (Argv'Length = 3 and then Argv (3) = '-')
           or else (Argv'Length = 4 and then Argv (4) = '-')
         then
            Fail ("Trailing ""-"" at the end of " & Argv & " forbidden.");

         --  Processing for -aP<dir>

         elsif Argv'Length > 3 and then Argv (1 .. 3) = "-aP" then
            Add_Directories
              (Root_Environment.Project_Path,
               Argv (4 .. Argv'Last),
               Prepend => True);

         --  Processing for --unchecked-shared-lib-imports

         elsif Argv = "--unchecked-shared-lib-imports" then
            Opt.Unchecked_Shared_Lib_Imports := True;

         --  Processing for one character switches

         elsif Argv'Length = 2 then
            case Argv (2) is
               when 'a' => null; -- ??? To be implemented
               when 'h' => Print_Usage               := True;
               when 'u' => Reset_Print; Print_Unit   := True;
               when 'U' => All_Projects              := True;
               when 's' => Reset_Print; Print_Source := True;
               when 'o' => Reset_Print; Print_Object := True;
               when 'v' => Verbose_Mode              := True;
               when 'd' => Dependable                := True;

               when 'P' =>
                  if File_Set then
                     Fail ("only one -P switch may be specified");
                  end if;

                  Project_File_Name_Expected := True;

               when others => OK := False;
            end case;

         elsif Argv'Length = 4 and then Argv (2 .. 3) = "vP" then
            case Argv (4) is
               when '0' => Current_Verbosity := Default;
               when '1' => Current_Verbosity := Medium;
               when '2' => Current_Verbosity := High;
               when others => OK := False;
            end case;

         --  -Pxxx

         elsif Argv'Length > 2 and then Argv (2) = 'P' then
            if File_Set then
               Fail ("only one -P switch may be specified");
            end if;

            File_Set          := True;
            Project_File_Name := new String'(Argv (3 .. Argv'Last));

         --  Processing for -files=file

         elsif Argv'Length > 7 and then Argv (1 .. 7) = "-files=" then
            FD := Open_Read (Argv (8 .. Argv'Last), GNAT.OS_Lib.Text);

            if FD = Invalid_FD then
               Osint.Fail ("could not find text file """ &
                           Argv (8 .. Argv'Last) & '"');
            end if;

            Len := Integer (File_Length (FD));

            declare
               Buffer : String (1 .. Len + 1);
               Index  : Positive := 1;
               Last   : Positive;

            begin
               --  Read the file

               Len := Read (FD, Buffer (1)'Address, Len);
               Buffer (Buffer'Last) := ASCII.NUL;
               Close (FD);

               --  Scan the file line by line

               while Index < Buffer'Last loop

                  --  Find the end of line

                  Last := Index;
                  while Last <= Buffer'Last
                    and then Buffer (Last) /= ASCII.LF
                    and then Buffer (Last) /= ASCII.CR
                  loop
                     Last := Last + 1;
                  end loop;

                  --  Ignore empty lines

                  if Last > Index then
                     Add_File (Buffer (Index .. Last - 1));
                  end if;

                  --  Find the beginning of the next line

                  Index := Last;
                  while Buffer (Index) = ASCII.CR or else
                        Buffer (Index) = ASCII.LF
                  loop
                     Index := Index + 1;
                  end loop;
               end loop;
            end;

         elsif Argv'Length > Target_Project_Option'Length
           and then
             Argv (1 .. Target_Project_Option'Length) =
           Target_Project_Option
         then
            if Target_Name /= null then
               if Target_Name.all /=
                 Argv (Target_Project_Option'Length + 1
                      .. Argv'Last)
               then
                  Fail_Program
                    (Project_Tree,
                     "several target switches "
                     & "cannot be specified");
               end if;

            else
               Target_Name :=
                 new String'
                   (Argv (Target_Project_Option'Length + 1
                    .. Argv'Last));
            end if;

         --  Processing for --RTS=path

         elsif Argv'Length >= 5 and then Argv (1 .. 5) = "--RTS" then
            if Argv'Length <= 6 or else Argv (6) /= '='then
               Osint.Fail ("missing path for --RTS");

            else
               --  Check that it is the first time we see this switch or, if
               --  it is not the first time, the same path is specified.

               if RTS_Specified = null then
                  RTS_Specified := new String'(Argv (7 .. Argv'Last));

               elsif RTS_Specified.all /= Argv (7 .. Argv'Last) then
                  Osint.Fail ("--RTS cannot be specified multiple times");
               end if;
            end if;

         elsif Argv'Length >= 3
           and then Argv (2) = 'X'
           and then Is_External_Assignment (Root_Environment, Argv)
         then
            --  Is_External_Assignment has side effects when it returns True

            null;

         else
            OK := False;
         end if;

      --  If not a switch, it must be a file name

      else
         Add_File (Argv);
      end if;

      if not OK then
         Put ("warning: unknown switch """);
         Put (Argv);
         Put_Line ("""");
      end if;

   end Scan_Arg;

   -----------
   -- Usage --
   -----------

   procedure Usage is
   begin
      --  Usage line

      Put_Line ("Usage: gprls switches  [list of object files]");
      New_Line;

      --  GPRLS switches

      Put_Line ("switches:");

      Display_Usage_Version_And_Help;

      --  Line for -Pproj

      Put_Line ("  -Pproj       use project file proj");

      --  Line for -a

      Put_Line ("  -a           also output relevant predefined units");

      --  Line for -u

      Put_Line ("  -u           output only relevant unit names");

      --  Line for -U

      Put_Line ("  -U           list sources for all projects");

      --  Line for -h

      Put_Line ("  -h           output this help message");

      --  Line for -s

      Put_Line ("  -s           output only relevant source names");

      --  Line for -o

      Put_Line ("  -o           output only relevant object names");

      --  Line for -d

      Put_Line ("  -d           output sources on which specified units " &
                               "depend");

      --  Line for -v

      Put_Line ("  -v           verbose output, full path and unit " &
                               "information");

      --  Line for -vPx

      Put_Line ("  -vPx         specify verbosity when parsing project " &
                               "files (x = 0/1/2)");

      New_Line;
      --  Line for -files=

      Put_Line ("  -files=fil   files are listed in text file 'fil'");

      --  Line for -aP switch

      Put_Line ("  -aPdir       specify project search path");

      --  Line for --target=

      Put_Line ("  --target=xxx specify target xxx");

      --  Line for --RTS

      Put_Line ("  --RTS=dir    specify the Ada runtime");

      --  Line for --unchecked-shared-lib-imports

      Put_Line ("  --unchecked-shared-lib-imports");
      Put_Line
        ("               shared library projects may import any project");

      --  Line for -X

      Put_Line ("  -Xnm=val     specify an external reference for " &
                  "project files");

      --  File Status explanation

      New_Line;
      Put_Line (" file status can be:");

      for ST in File_Status loop
         Put ("   ");
         Output_Status (ST, Verbose => False);
         Put (" ==> ");
         Output_Status (ST, Verbose => True);
         New_Line;
      end loop;
   end Usage;

   procedure Check_Version_And_Help is new Check_Version_And_Help_G (Usage);

   procedure Initialize is
   begin
      if not Initialized then
         Initialized := True;

         --  Initialize some packages

         Snames.Initialize;

         Set_Program_Name ("gprls");

         GPR.Tree.Initialize (Root_Environment, Gprls_Flags);
         GPR.Tree.Initialize (Project_Node_Tree);

         GPR.Initialize (Project_Tree);

         if Target_Name = null then
            GPR.Env.Initialize_Default_Project_Path
              (Root_Environment.Project_Path,
               Target_Name => GprConfig.Sdefault.Hostname);

         else
            GPR.Env.Initialize_Default_Project_Path
           (Root_Environment.Project_Path, Target_Name.all);
         end if;

         GPR.Tree.Initialize (Tree);
      end if;
   end Initialize;

begin
   Initialize;

   Check_Version_And_Help
     ("GPRLS", "2015", Version_String => Gpr_Version_String);

   Project_File_Name_Expected := False;

   --  Loop to scan out arguments

   Next_Arg := 1;
   Scan_Args : while Next_Arg <= Argument_Count loop
      declare
         Next_Argv : constant String := Argument (Next_Arg);
      begin
         Scan_Arg (Next_Argv);
      end;

      Next_Arg := Next_Arg + 1;
   end loop Scan_Args;

   if Project_File_Name_Expected then
      Fail ("project file name missing");
   end if;

   --  Output usage information when requested

   if Print_Usage then
      Usage;
   end if;

   if Project_File_Name = null and then
     Number_File_Names = 0 and then
     not Verbose_Mode
   then
      if Argument_Count = 0 then
         Usage;

      else
         Try_Help;
         Exit_Status := E_Fatal;
      end if;

      Exit_Program (Exit_Status);
   end if;

   Save_Verbose := Verbose_Mode;

   No_Project_File_Specified := Project_File_Name = null;

   if Verbose_Mode and then
     No_Project_File_Specified and then
     Number_File_Names = 0
   then
      Verbose_Mode := False;
      Quiet_Output := True;
   end if;

   if Load_Standard_Base then
      Gpr_Util.Knowledge.Parse_Knowledge_Base (Project_Tree);
   end if;

   if Project_File_Name = null then
      Quiet_Output := False;
      Look_For_Default_Project;
      Quiet_Output := True;
   end if;

   if Project_File_Name = null then
      Try_Help;
      Fail_Program (null, "no project file specified");
   end if;

   Path_Name := new
     String (1 .. Project_File_Name'Length + Project_File_Extension'Length);
   Path_Last := Project_File_Name'Length;

   if File_Names_Case_Sensitive then
      Path_Name (1 .. Path_Last) := Project_File_Name.all;
   else
      Path_Name (1 .. Path_Last) := To_Lower (Project_File_Name.all);
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

   Output_Name := new String'(To_Lower (Path_Name (1 .. Path_Last)));

   if not Is_Regular_File (Output_Name (1 .. Path_Last)) then
      Fail ("cannot find project file " & Output_Name (1 .. Path_Last));
   end if;

   --  Check command line arguments. These will be overridden when looking
   --  for the configuration file

   if Target_Name = null then
      Target_Name := new String'("");
   end if;

   if Config_Project_File_Name = null then
      Config_Project_File_Name := new String'("");
   end if;

   Parse_Project_And_Apply_Config
     (Main_Project               => Main_Project,
      User_Project_Node          => User_Project_Node,
      Config_File_Name           => Config_Project_File_Name.all,
      Autoconf_Specified         => False,
      Project_File_Name          => Output_Name.all,
      Project_Tree               => Project_Tree,
      Project_Node_Tree          => Project_Node_Tree,
      Packages_To_Check          => Packages_To_Check,
      Env                        => Root_Environment,
      Allow_Automatic_Generation => True,
      Automatically_Generated    => Delete_Autoconf_File,
      Config_File_Path           => Configuration_Project_Path,
      Target_Name                => Target_Name.all,
      Normalized_Hostname        => Gpr_Util.Knowledge.Normalized_Hostname,
      Implicit_Project           => No_Project_File_Found);

   if Main_Project = No_Project then
      Fail_Program (Project_Tree, "unable to process project file");
   end if;

   Verbose_Mode := Save_Verbose;
   Quiet_Output := False;

   if Verbose_Mode then
      Display_Paths;

      if No_Project_File_Specified and then Number_File_Names = 0 then
         Exit_Program (Exit_Code => E_Success);
      end if;
   end if;

   declare
      Iter : Source_Iterator := For_Each_Source (Project_Tree);
      Source : GPR.Source_Id;
   begin
      loop
         Source := Element (Iter);
         exit when Source = No_Source;
         Initialize_Source_Record (Source);
         Next (Iter);
      end loop;
   end;

   if Number_File_Names = 0 then
      --  Get all the compilable sources of the project
      declare
         Unit    : GPR.Unit_Index;
         Subunit : Boolean := False;
      begin
         Unit := Units_Htable.Get_First (Project_Tree.Units_HT);
         while Unit /= No_Unit_Index loop

            --  We only need to put the library units, body or spec, but not
            --  the subunits.

            if Unit.File_Names (Impl) /= null
              and then not Unit.File_Names (Impl).Locally_Removed
            then
               --  There is a body, check if it is for this project

               if All_Projects
                 or else Unit.File_Names (Impl).Project = Main_Project
               then
                  Subunit := False;

                  if Unit.File_Names (Spec) = null
                    or else Unit.File_Names (Spec).Locally_Removed
                  then
                     --  We have a body with no spec: we need to check if
                     --  this is a subunit, because gnatls will complain
                     --  about subunits.

                     Subunit := Is_Subunit (Unit.File_Names (Impl));
                  end if;

                  if not Subunit then
                     Add_File
                       (Get_Name_String (Unit.File_Names (Impl).Object),
                        Source => Unit.File_Names (Impl));
                  end if;
               end if;

            elsif Unit.File_Names (Spec) /= null
              and then not Unit.File_Names (Spec).Locally_Removed
            then
               --  We have a spec with no body. Check if it is for this project

               if All_Projects
                 or else Unit.File_Names (Spec).Project = Main_Project
               then
                  Add_File
                    (Get_Name_String (Unit.File_Names (Spec).Object),
                     Source => Unit.File_Names (Spec));
               end if;
            end if;

            Unit := Units_Htable.Get_Next (Project_Tree.Units_HT);
         end loop;
      end;

   else
      --  Find the sources in the project files

      for J in 1 .. Number_File_Names loop
         declare
            File_Name : constant String := File_Names (J).File_Name.all;
            Unit      : GPR.Unit_Index;
            Subunit   : Boolean := False;
         begin
            Unit := Units_Htable.Get_First (Project_Tree.Units_HT);

            Unit_Loop :
            while Unit /= No_Unit_Index loop

               --  We only need to put the library units, body or spec, but not
               --  the subunits.

               if Unit.File_Names (Impl) /= null
                 and then not Unit.File_Names (Impl).Locally_Removed
               then
                  --  There is a body, check if it is for this project

                  if All_Projects
                    or else Unit.File_Names (Impl).Project = Main_Project
                  then
                     Subunit := False;

                     if Unit.File_Names (Spec) = null
                       or else Unit.File_Names (Spec).Locally_Removed
                     then
                        --  We have a body with no spec: we need to check if
                        --  this is a subunit, because gnatls will complain
                        --  about subunits.

                        Subunit := Is_Subunit (Unit.File_Names (Impl));
                     end if;

                     if not Subunit and then
                       (Get_Name_String (Unit.File_Names (Impl).Object) =
                          File_Name
                       or else
                         (Get_Name_String (Unit.File_Names (Impl).Dep_Name) =
                            File_Name))
                     then
                        File_Names (J).Source := Unit.File_Names (Impl);
                        exit Unit_Loop;
                     end if;
                  end if;

               elsif Unit.File_Names (Spec) /= null
                 and then not Unit.File_Names (Spec).Locally_Removed
               then
                  --  We have a spec with no body. Check if it is for this
                  --  project.

                  if (All_Projects
                      or else Unit.File_Names (Spec).Project = Main_Project)
                    and then
                      (Get_Name_String (Unit.File_Names (Spec).Object) =
                         File_Name
                       or else
                       Get_Name_String (Unit.File_Names (Spec).Dep_Name) =
                         File_Name)
                  then
                     File_Names (J).Source := Unit.File_Names (Spec);
                  end if;
               end if;

               Unit := Units_Htable.Get_Next (Project_Tree.Units_HT);
            end loop Unit_Loop;
         end;
      end loop;
   end if;

   --  Create mapping of ALI files to Source_Id

   --  Get all the compilable sources of the projects
   declare
      Unit    : GPR.Unit_Index;
      Subunit : Boolean := False;
   begin
      Unit := Units_Htable.Get_First (Project_Tree.Units_HT);
      while Unit /= No_Unit_Index loop

         --  We only need to put the library units, body or spec, but not
         --  the subunits.

         if Unit.File_Names (Impl) /= null
           and then not Unit.File_Names (Impl).Locally_Removed
         then
            Subunit := False;

            if Unit.File_Names (Spec) = null
              or else Unit.File_Names (Spec).Locally_Removed
            then
               --  We have a body with no spec: we need to check if this is
               --  a subunit.

               Subunit := Is_Subunit (Unit.File_Names (Impl));
            end if;

            if not Subunit then
               Add_ALI
                 (Unit.File_Names (Impl).File,
                  Spec   => False,
                  Source => Unit.File_Names (Impl));
            end if;
         end if;

         if Unit.File_Names (Spec) /= null
           and then not Unit.File_Names (Spec).Locally_Removed
         then
            Add_ALI
              (Unit.File_Names (Spec).File,
               Spec   => True,
               Source => Unit.File_Names (Spec));
         end if;

         Unit := Units_Htable.Get_Next (Project_Tree.Units_HT);
      end loop;
   end;

   for J in 1 .. Number_File_Names loop
      if File_Names (J).Source = No_Source then
         Put_Line
           (Standard_Error,
            "Can't find source for " & File_Names (J).File_Name.all);

      else
         declare
            Text    : Text_Buffer_Ptr;
         begin
            Text := Osint.Read_Library_Info
              (File_Name_Type (File_Names (J).Source.Dep_Path));

            if Text /= null then
               File_Names (J).The_ALI := Scan_ALI
                 (F          => File_Name_Type
                                  (File_Names (J).Source.Dep_Path),
                  T          => Text,
                  Ignore_ED  => False,
                  Err        => True,
                  Read_Lines => "WD",
                  Object_Path => File_Name_Type
                                  (File_Names (J).Source.Object_Path));
               Free (Text);

            else
               File_Names (J).The_ALI := No_ALI_Id;
               Put_Line
                 (Standard_Error,
                  "Can't find ALI file for " &
                    Get_Name_String (File_Names (J).Source.Path.Display_Name));

            end if;
         end;
      end if;
   end loop;

   for J in 1 .. Number_File_Names loop
      declare
         FN_Source : File_Name_Source renames File_Names (J);
         Id        : ALI_Id;
         Last_U    : Unit_Id;

      begin
         if FN_Source.Source /= No_Source then
            Id := FN_Source.The_ALI;

            if Id = No_ALI_Id then
               null;

               --  Output_Object (No_File);

            else
               Get_Name_String
                 (Units.Table (ALIs.Table (Id).First_Unit).Uname);

               if Print_Object then
                  if ALIs.Table (Id).No_Object then
                     Output_Object (No_File);
                  else
                     Output_Object (ALIs.Table (Id).Ofile_Full_Name);
                  end if;
               end if;

               --  In verbose mode print all main units in the ALI file,
               --  otherwise just print the first one to ease columnwise
               --  printout.

               if Verbose_Mode then
                  Last_U := ALIs.Table (Id).Last_Unit;
               else
                  Last_U := ALIs.Table (Id).First_Unit;
               end if;

               for U in ALIs.Table (Id).First_Unit .. Last_U loop
                  if Print_Unit then
                     Output_Unit (U);
                  end if;

                  --  Output source now, unless if it will be done as part of
                  --  outputing dependencies.

                  if not (Dependable and then Print_Source) then
                     Output_Source (Corresponding_Sdep_Entry (Id, U));
                  end if;
               end loop;

               --  Print out list of units on which this unit depends (D lines)

               if Dependable and then Print_Source then
                  if Verbose_Mode then
                     Put_Line ("   depends upon");
                  end if;

                  for D in
                    ALIs.Table (Id).First_Sdep .. ALIs.Table (Id).Last_Sdep
                  loop
                     if not Is_Ada_Predefined_File_Name (Sdep.Table (D).Sfile)
                     then
                        Put ("   ");
                        Output_Source (D);

                        if not Verbose_Mode then
                           New_Line;
                        end if;
                     end if;
                  end loop;
               end if;
            end if;
         end if;
      end;
   end loop;

end Gprls.Main;
