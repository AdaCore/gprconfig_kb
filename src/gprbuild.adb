------------------------------------------------------------------------------
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             G P R B U I L D                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2013, Free Software Foundation, Inc.          --
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

with Ada.Text_IO;       use Ada.Text_IO;

with GNAT.Directory_Operations; use GNAT.Directory_Operations;

with Output;      use Output;
with Gpr_Util;    use Gpr_Util;

package body Gprbuild is

   package Processed_Projects is new GNAT.HTable.Simple_HTable
     (Header_Num => Prj.Header_Num,
      Element    => Boolean,
      No_Element => False,
      Key        => Name_Id,
      Hash       => Hash,
      Equal      => "=");
   --  Projects that have already been processed

   ----------------
   -- Add_Option --
   ----------------

   procedure Add_Option
     (Value       : String;
      To          : in out Options_Data;
      Display     : Boolean;
      Simple_Name : Boolean := False)
   is
   begin
      Name_Len := Value'Length;
      Name_Buffer (1 .. Name_Len) := Value;
      Add_Option_Internal (Get_Option (Name_Find), To, Display, Simple_Name);
   end Add_Option;

   procedure Add_Option
     (Value       : Name_Id;
      To          : in out Options_Data;
      Display     : Boolean;
      Simple_Name : Boolean := False)
   is
   begin
      Add_Option_Internal (Get_Option (Value), To, Display, Simple_Name);
   end Add_Option;

   -------------------------
   -- Add_Option_Internal --
   -------------------------

   procedure Add_Option_Internal
     (Value       : String_Access;
      To          : in out Options_Data;
      Display     : Boolean;
      Simple_Name : Boolean := False)
   is
   begin
      --  For compatibility with gnatmake, do not consider empty options

      if Value'Length = 0 then
         return;
      end if;

      To.Last := To.Last + 1;

      if To.Last > To.Options'Last then
         declare
            New_Options     : constant String_List_Access :=
                                new String_List (1 .. 2 * To.Options'Last);
            New_Visible     : constant Booleans :=
                                new Boolean_Array (1 .. 2 * To.Visible'Last);
            New_Simple_Name : constant Booleans :=
                                new Boolean_Array (1 .. 2 * To.Visible'Last);

         begin
            New_Options (To.Options'Range) := To.Options.all;
            To.Options.all := (others => null);
            Free (To.Options);
            To.Options := New_Options;
            New_Visible (To.Visible'Range) := To.Visible.all;
            Free (To.Visible);
            To.Visible := New_Visible;
            New_Simple_Name (To.Simple_Name'Range) := To.Simple_Name.all;
            Free (To.Simple_Name);
            To.Simple_Name := New_Simple_Name;
         end;
      end if;

      To.Options (To.Last)     := Value;
      To.Visible (To.Last)     := Display;
      To.Simple_Name (To.Last) := Simple_Name;
   end Add_Option_Internal;

   -----------------
   -- Add_Options --
   -----------------

   procedure Add_Options
     (Value         : String_List_Id;
      To            : in out Options_Data;
      Display_All   : Boolean;
      Display_First : Boolean;
      Simple_Name   : Boolean := False)
   is
      List            : String_List_Id := Value;
      Element         : String_Element;
      Option          : String_Access;
      First_Display   : Boolean := Display_First;
   begin
      while List /= Nil_String loop
         Element := Project_Tree.Shared.String_Elements.Table (List);

         --  Ignore empty options

         if Element.Value /= Empty_String then
            Option := Get_Option (Element.Value);

            Add_Option_Internal
              (Value       => Option,
               To          => To,
               Display     => Display_All or First_Display,
               Simple_Name => Simple_Name);
            First_Display := False;
         end if;

         List := Element.Next;
      end loop;
   end Add_Options;

   -----------------
   -- Add_Process --
   -----------------

   procedure Add_Process (Process : Process_Id; Data : Process_Data) is
   begin
      Process_Htable.Set (Process, Data);
      Outstanding_Processes := Outstanding_Processes + 1;
   end Add_Process;

   --------------------
   -- Archive_Suffix --
   --------------------

   function Archive_Suffix (For_Project : Project_Id) return String is
   begin
      if For_Project.Config.Archive_Suffix = No_File then
         return ".a";

      else
         return Get_Name_String (For_Project.Config.Archive_Suffix);
      end if;
   end Archive_Suffix;

   -------------------
   -- Await_Process --
   -------------------

   procedure Await_Process (Data : out Process_Data; OK : out Boolean) is
      Pid  : Process_Id;
   begin
      loop
         Data := No_Process_Data;

         Wait_Process (Pid, OK);

         if Pid = Invalid_Pid then
            return;
         end if;

         Data := Process_Htable.Get (Pid);

         if Data /= No_Process_Data then
            Process_Htable.Set (Pid, No_Process_Data);
            Outstanding_Processes := Outstanding_Processes - 1;
            return;
         end if;
      end loop;
   end Await_Process;

   --------------------------------
   -- Change_To_Object_Directory --
   --------------------------------

   procedure Change_To_Object_Directory (Project : Project_Id) is
   begin
      --  Nothing to do if the current working directory is already the correct
      --  object directory.

      if Project_Of_Current_Object_Directory /= Project then
         Project_Of_Current_Object_Directory := Project;

         --  Set the working directory to the object directory of the actual
         --  project.

         Change_Dir (Get_Name_String (Project.Object_Directory.Display_Name));

         if Opt.Verbose_Mode and then Opt.Verbosity_Level > Opt.Low then
            Write_Str  ("Changing to object directory of """);
            Write_Name (Project.Display_Name);
            Write_Str  (""": """);
            Write_Name (Project.Object_Directory.Display_Name);
            Write_Line ("""");
         end if;
      end if;

   exception
      --  Fail if unable to change to the object directory

      when Directory_Error =>
         Fail_Program
           (Project_Tree,
            "unable to change to object directory """ &
            Get_Name_String (Project.Object_Directory.Display_Name) &
            """ of project " &
            Get_Name_String (Project.Display_Name));
   end Change_To_Object_Directory;

   ---------------------------
   -- Check_Archive_Builder --
   ---------------------------

   procedure Check_Archive_Builder is
      List : Name_List_Index;
   begin
      --  First, make sure that the archive builder (ar) is on the path

      if Archive_Builder_Path = null then
         List := Main_Project.Config.Archive_Builder;

         if List = No_Name_List then
            Fail_Program
              (Project_Tree, "no archive builder in configuration");

         else
            Archive_Builder_Name :=
              new String'(Get_Name_String
                                     (Project_Tree.Shared.Name_Lists.Table
                                        (List).Name));
            Archive_Builder_Path :=
              Locate_Exec_On_Path (Archive_Builder_Name.all);

            if Archive_Builder_Path = null then
               Fail_Program
                 (Project_Tree,
                  "unable to locate archive builder """ &
                  Archive_Builder_Name.all & '"');
            end if;

            loop
               List := Project_Tree.Shared.Name_Lists.Table (List).Next;
               exit when List = No_Name_List;
               Add_Option
                 (Value   => Project_Tree.Shared.Name_Lists.Table (List).Name,
                  To      => Archive_Builder_Opts,
                  Display => True);
            end loop;

            List := Main_Project.Config.Archive_Builder_Append_Option;
            while List /= No_Name_List loop
               Add_Option
                 (Value   => Project_Tree.Shared.Name_Lists.Table (List).Name,
                  To      => Archive_Builder_Append_Opts,
                  Display => True);
               List := Project_Tree.Shared.Name_Lists.Table (List).Next;
            end loop;

            --  If there is an archive indexer (ranlib), try to locate it on
            --  the path. Don't fail if it is not found.

            List := Main_Project.Config.Archive_Indexer;

            if List /= No_Name_List then
               Archive_Indexer_Name :=
                 new String'(Get_Name_String
                   (Project_Tree.Shared.Name_Lists.Table (List).Name));
               Archive_Indexer_Path :=
                 Locate_Exec_On_Path (Archive_Indexer_Name.all);

               if Archive_Builder_Path /= null then
                  loop
                     List := Project_Tree.Shared.Name_Lists.Table (List).Next;
                     exit when List = No_Name_List;
                     Add_Option
                       (Value   =>
                          Project_Tree.Shared.Name_Lists.Table (List).Name,
                        To      => Archive_Indexer_Opts,
                        Display => True);
                  end loop;
               end if;
            end if;
         end if;
      end if;
   end Check_Archive_Builder;

   ---------------------------
   -- Create_Path_From_Dirs --
   ---------------------------

   function Create_Path_From_Dirs return String_Access is
      Result    : String_Access;
      Tmp       : String_Access;
      Path_Last : Natural := 0;
   begin
      for Index in 1 .. Directories.Last loop
         Get_Name_String (Directories.Table (Index));

         while Name_Len > 1
           and then (Name_Buffer (Name_Len) = Directory_Separator
                     or else Name_Buffer (Name_Len) = '/')
         loop
            Name_Len := Name_Len - 1;
         end loop;

         if Result = null then
            Result := new String (1 .. Name_Len);
         else
            while Path_Last + Name_Len + 1 > Result'Last loop
               Tmp := new String (1 .. 2 * Result'Length);
               Tmp (1 .. Path_Last) := Result (1 .. Path_Last);
               Free (Result);
               Result := Tmp;
            end loop;

            Path_Last := Path_Last + 1;
            Result (Path_Last) := Path_Separator;
         end if;

         Result (Path_Last + 1 .. Path_Last + Name_Len) :=
           Name_Buffer (1 .. Name_Len);
         Path_Last := Path_Last + Name_Len;
      end loop;

      if Current_Verbosity = High and then Result /= null then
         Put_Line ("Path=" & Result (1 .. Path_Last));
      end if;

      Tmp := new String'(Result (1 .. Path_Last));
      Free (Result);
      return Tmp;
   end Create_Path_From_Dirs;

   -----------------------
   -- Display_Processes --
   -----------------------

   procedure Display_Processes (Name : String) is
   begin
      if Opt.Maximum_Processes > 1
        and then Opt.Verbose_Mode
        and then Current_Verbosity = High
      then
         Write_Str ("   ");
         Write_Str (Outstanding_Processes'Img);
         Write_Char (' ');
         Write_Str (Name);

         if Outstanding_Processes <= 1 then
            Write_Line (" process");
         else
            Write_Line (" processes");
         end if;
      end if;
   end Display_Processes;

   ----------------
   -- Get_Option --
   ----------------

   function Get_Option (Option : Name_Id) return String_Access is
      Option_Name : constant String := Get_Name_String (Option);
   begin
      --  Look in All_Options if this option is already cached

      for Index in 1 .. All_Options.Last loop
         if All_Options.Options (Index).all = Option_Name then
            return All_Options.Options (Index);
         end if;
      end loop;

      --  Add the option to the All_Options cache, so that it will be found
      --  next time.

      Add_Option_Internal
        (new String'(Option_Name),
         To      => All_Options,
         Display => False);

      return All_Options.Options (All_Options.Last);
   end Get_Option;

   ----------
   -- Hash --
   ----------

   function Hash (Pid : Process_Id) return Header_Num is
      Modulo : constant Integer := Integer (Header_Num'Last) + 1;
   begin
      return Header_Num (Pid_To_Integer (Pid) mod Modulo);
   end Hash;

   --------------------------------
   -- Process_Imported_Libraries --
   --------------------------------

   procedure Process_Imported_Libraries
     (For_Project        : Project_Id;
      There_Are_SALs     : out Boolean;
      And_Project_Itself : Boolean := False)
   is

      procedure Process_Project (Project : Project_Id; Is_Aggregate : Boolean);
      --  Process Project and its imported projects recursively.
      --  Add any library projects to table Library_Projs.

      ---------------------
      -- Process_Project --
      ---------------------

      procedure Process_Project
        (Project : Project_Id; Is_Aggregate : Boolean)
      is
         Imported : Project_List := Project.Imported_Projects;

      begin
         --  Nothing to do if project has already been processed

         if not Processed_Projects.Get (Project.Name) then
            Processed_Projects.Set (Project.Name, True);

            --  We first process the imported projects to guarantee that
            --  We have a proper reverse order for the libraries. Do not add
            --  library for encapsulated libraries dependencies except when
            --  building the encapsulated library itself.

            if For_Project.Standalone_Library = Encapsulated
              or else Project.Standalone_Library /= Encapsulated
            then
               while Imported /= null loop
                  if Imported.Project /= No_Project then
                     Process_Project
                       (Imported.Project,
                        Is_Aggregate =>
                          (Project.Qualifier = Aggregate_Library)
                           or else Is_Aggregate);
                  end if;

                  Imported := Imported.Next;
               end loop;
            end if;

            --  For an extending project, process the project being extended

            if Project.Extends /= No_Project then
               Process_Project (Project.Extends, Is_Aggregate => False);
            end if;

            --  If it is a library project, add it to Library_Projs

            if (And_Project_Itself or else Project /= For_Project)
              and then Project.Extended_By = No_Project
              and then Project.Library
            then
               if Project.Standalone_Library /= No then
                  There_Are_SALs := True;
               end if;

               Library_Projs.Append
                 (Library_Project'
                    (Project,
                     Is_Aggregate and then not Project.Externally_Built));
            end if;
         end if;
      end Process_Project;

      --  Start of processing for Process_Imported_Libraries

   begin
      Processed_Projects.Reset;
      Library_Projs.Init;
      There_Are_SALs := False;

      Process_Project (For_Project, Is_Aggregate => False);
   end Process_Imported_Libraries;

   ------------------------------------
   -- Process_Imported_Non_Libraries --
   ------------------------------------

   procedure Process_Imported_Non_Libraries (For_Project : Project_Id) is

      procedure Process_Project (Project : Project_Id);
      --  Process Project and its imported projects recursively.
      --  Add any non library project to table Non_Library_Projs.

      ---------------------
      -- Process_Project --
      ---------------------

      procedure Process_Project (Project : Project_Id) is
         Imported : Project_List := Project.Imported_Projects;

      begin
         --  Nothing to do if project has already been processed

         if not Processed_Projects.Get (Project.Name) then
            Processed_Projects.Set (Project.Name, True);

            --  Call Process_Project recursively for any imported project.
            --  We first process the imported projects to guarantee that
            --  we have a proper reverse order for the libraries.

            while Imported /= null loop
               if Imported.Project /= No_Project then
                  Process_Project (Imported.Project);
               end if;

               Imported := Imported.Next;
            end loop;

            --  For an extending project, process the project being extended

            if Project.Extends /= No_Project then
               Process_Project (Project.Extends);
            end if;

            --  If it is not a library project, add it to Non_Library_Projs

            if Project /= For_Project
              and then Project.Extended_By = No_Project
              and then not Project.Library
            then
               Non_Library_Projs.Append (Project);
            end if;
         end if;
      end Process_Project;

      --  Start of processing for Process_Imported_Non_Libraries

   begin
      Processed_Projects.Reset;
      Non_Library_Projs.Init;

      Process_Project (For_Project);
   end Process_Imported_Non_Libraries;

   --------------------
   -- Record_Failure --
   --------------------

   procedure Record_Failure (Main : Main_Info) is
   begin
      Bad_Processes.Append (Main);

      if not Opt.Keep_Going then
         Stop_Spawning := True;
      end if;
   end Record_Failure;

   ---------------------------
   -- Test_If_Relative_Path --
   ---------------------------

   procedure Test_If_Relative_Path
     (Switch           : in out String_Access;
      Parent           : String;
      Including_Switch : Name_Id)
   is
      Original : constant String (1 .. Switch'Length) := Switch.all;

   begin
      if Original (1) = '-' and then Including_Switch /= No_Name then
         declare
            Inc_Switch : constant String := Get_Name_String (Including_Switch);

         begin
            if Original'Last > Inc_Switch'Last
              and then Original (1 .. Inc_Switch'Last) = Inc_Switch
              and then not Is_Absolute_Path
                (Original (Inc_Switch'Last + 1 .. Original'Last))
            then
                  Switch := new String'
                    (Inc_Switch & Parent & Directory_Separator &
                     Original (Inc_Switch'Last + 1 .. Original'Last));
            end if;
         end;
      end if;

      if Original (1) /= '-' and then not Is_Absolute_Path (Original) then
         Switch := new String'(Parent & Directory_Separator & Original);
      end if;
   end Test_If_Relative_Path;

end Gprbuild;
