------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             C L E A N G P R                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2006-2008, Free Software Foundation, Inc.         --
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

with Csets;
with Confgpr;     use Confgpr;
with Gprexch;     use Gprexch;
with GPR_Version; use GPR_Version;
with Gpr_Util;    use Gpr_Util;
with Makeutl;     use Makeutl;
with MLib;        use MLib;
with Namet;       use Namet;
with Opt;         use Opt;
with Osint;
with Prj;         use Prj;
with Prj.Err;
with Prj.Ext;
with Prj.Proc;    use Prj.Proc;
with Prj.Util;    use Prj.Util;
with Sinput.P;
with Snames;
with Switch;      use Switch;
with Table;

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO;

with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.IO;                   use GNAT.IO;
with GNAT.OS_Lib;               use GNAT.OS_Lib;

package body Cleangpr is

   Object_Suffix : constant String := Get_Target_Object_Suffix.all;
   --  The suffix of object files on this platform

   Initialized : Boolean := False;
   --  Set to True by the first call to Initialize.
   --  To avoid reinitialization of some packages.

   Force_Deletions : Boolean := False;
   --  Set to True by switch -f. When True, attempts to delete non writable
   --  files will be done.

   Do_Nothing : Boolean := False;
   --  Set to True when switch -n is specified. When True, no file is deleted.
   --  gnatclean only lists the files that would have been deleted if the
   --  switch -n had not been specified.

   File_Deleted : Boolean := False;
   --  Set to True if at least one file has been deleted

   Copyright_Displayed : Boolean := False;
   Usage_Displayed     : Boolean := False;
   --  Flags set to True when the action is performed, to avoid duplicate
   --  displays.

   All_Projects : Boolean := False;
   --  Set to True when option -r is used, so that all projects in the project
   --  tree are cleaned.

   --  Packages of project files where unknown attributes are errors

   Naming_String   : aliased String := "naming";
   Builder_String  : aliased String := "builder";
   Compiler_String : aliased String := "compiler";
   Binder_String   : aliased String := "binder";
   Linker_String   : aliased String := "linker";

   Package_Names : aliased String_List :=
     (Naming_String   'Access,
      Builder_String  'Access,
      Compiler_String 'Access,
      Binder_String   'Access,
      Linker_String   'Access);

   Packages_To_Check : constant String_List_Access := Package_Names'Access;

   package Processed_Projects is new Table.Table
     (Table_Component_Type => Project_Id,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 0,
      Table_Initial        => 10,
      Table_Increment      => 100,
      Table_Name           => "Cleangpr.Processed_Projects");
   --  Table to keep track of what project files have been processed, when
   --  switch -r is specified.

   -----------------------------
   -- Other local subprograms --
   -----------------------------

   procedure Clean_Archive (Project : Project_Id);
   --  Delete a global archive and its dependency file, if they exist

   procedure Clean_Interface_Copy_Directory (Project : Project_Id);
   --  Delete files in an interface copy directory: any file that is a copy of
   --  a source of the project.

   procedure Clean_Library_Directory (Project : Project_Id);
   --  Delete the library file in a library directory and any ALI file
   --  of a source of the project in a library ALI directory.

   procedure Clean_Project (Project : Project_Id);
   --  Do the cleaning work when a project file is specified.
   --  This procedure calls itself recursively when there are several
   --  project files in the tree rooted at the main project file and switch -r
   --  has been specified.

   procedure Delete (In_Directory : String; File : String);
   --  Delete one file, or list the file name if switch -n is specified

   procedure Delete_Binder_Generated_Files
     (Dir    : String;
      Source : File_Name_Type);
   --  Delete the binder generated file in directory Dir for Source

   procedure Display_Copyright;
   --  Display the Copyright notice. If called several times, display the
   --  Copyright notice only the first time.

   procedure Initialize;
   --  Call the necessary package initializations

   procedure Parse_Cmd_Line;
   --  Parse the command line

   function Ultimate_Extension_Of (Project : Project_Id) return Project_Id;
   --  Returns either Project, if it is not extended by another project, or
   --  the project that extends Project, directly or indirectly, and that is
   --  not itself extended. Returns No_Project if Project is No_Project.

   procedure Usage;
   --  Display the usage.
   --  If called several times, the usage is displayed only the first time.

   procedure Check_Version_And_Help is new
     Check_Version_And_Help_G (Usage);

   -------------------
   -- Clean_Archive --
   -------------------

   procedure Clean_Archive (Project : Project_Id) is
      Current_Dir : constant Dir_Name_Str := Get_Current_Dir;
      Data        : constant Project_Data :=
                      Project_Tree.Projects.Table (Project);

      Archive_Name : constant String :=
                       "lib" & Get_Name_String (Data.Name) &
                       Get_Name_String (Data.Config.Archive_Suffix);
      --  The name of the archive file for this project

      Archive_Dep_Name : constant String :=
                           "lib" & Get_Name_String (Data.Name) & ".deps";
      --  The name of the archive dependency file for this project

      Obj_Dir : constant String := Get_Name_String (Data.Object_Directory);

   begin
      Change_Dir (Obj_Dir);

      if Is_Regular_File (Archive_Name) then
         Delete (Obj_Dir, Archive_Name);
      end if;

      if Is_Regular_File (Archive_Dep_Name) then
         Delete (Obj_Dir, Archive_Dep_Name);
      end if;

      Change_Dir (Current_Dir);
   end Clean_Archive;

   ------------------------------------
   -- Clean_Interface_Copy_Directory --
   ------------------------------------

   procedure Clean_Interface_Copy_Directory (Project : Project_Id) is
      Current : constant String := Get_Current_Dir;
      Data    : constant Project_Data := Project_Tree.Projects.Table (Project);

      Direc : Dir_Type;

      Name : String (1 .. 200);
      Last : Natural;

      Delete_File : Boolean;

      Source      : Prj.Source_Id;
      Src_Data    : Source_Data;

      File_Name   : File_Name_Type;

   begin
      if Data.Library and then Data.Library_Src_Dir /= No_Path then
         declare
            Directory : constant String :=
                          Get_Name_String (Data.Library_Src_Dir);

         begin
            Change_Dir (Get_Name_String (Data.Library_Src_Dir));
            Open (Direc, ".");

            --  For each regular file in the directory, if switch -n has not
            --  been specified, make it writable and delete the file if it is
            --  a copy of a source of the project.

            loop
               Read (Direc, Name, Last);
               exit when Last = 0;

               if Is_Regular_File (Name (1 .. Last)) then
                  Osint.Canonical_Case_File_Name (Name (1 .. Last));

                  Name_Len := Last;
                  Name_Buffer (1 .. Name_Len) := Name (1 .. Last);
                  File_Name := Name_Find;

                  Delete_File := False;

                  Source := Project_Tree.First_Source;

                  loop
                     Src_Data := Project_Tree.Sources.Table (Source);

                     if Src_Data.Unit /= No_Name
                       and then
                         Ultimate_Extension_Of (Src_Data.Project) = Project
                       and then
                         Src_Data.File = File_Name
                     then
                        Delete_File := True;
                        exit;
                     end if;

                     Source := Src_Data.Next_In_Sources;
                     exit when Source = No_Source;
                  end loop;

                  if Delete_File then
                     if not Do_Nothing then
                        Set_Writable (Name (1 .. Last));
                     end if;

                     Delete (Directory, Name (1 .. Last));
                  end if;
               end if;
            end loop;

            Close (Direc);

            --  Restore the initial working directory

            Change_Dir (Current);
         end;
      end if;
   end Clean_Interface_Copy_Directory;

   -----------------------------
   -- Clean_Library_Directory --
   -----------------------------

   procedure Clean_Library_Directory (Project : Project_Id) is
      Current : constant String := Get_Current_Dir;
      Data    : Project_Data := Project_Tree.Projects.Table (Project);

      Lib_Filename : constant String := Get_Name_String (Data.Library_Name);
      DLL_Name     : String :=
                       Get_Name_String
                         (Data.Config.Shared_Lib_Prefix) &
                       Lib_Filename &
                       Get_Name_String (Data.Config.Shared_Lib_Suffix);
      Archive_Name : String :=
                       "lib" & Lib_Filename &
                       Get_Name_String (Data.Config.Archive_Suffix);
      Library_Exchange_File_Name : constant String :=
                                     Lib_Filename & Library_Exchange_Suffix;

      Direc        : Dir_Type;

      Name : String (1 .. 200);
      Last : Natural;

      Delete_File : Boolean;

   begin
      if Data.Library then
         Osint.Canonical_Case_File_Name (DLL_Name);
         Osint.Canonical_Case_File_Name (Archive_Name);

         declare
            Obj_Directory     : constant String :=
                                  Get_Name_String (Data.Object_Directory);
            Lib_Directory     : constant String :=
                                  Get_Name_String (Data.Library_Dir);
            Lib_ALI_Directory : constant String :=
                                  Get_Name_String (Data.Library_ALI_Dir);

            Exchange_File : Ada.Text_IO.File_Type;

            In_Generated : Boolean;

         begin
            Change_Dir (Obj_Directory);

            Open (Direc, ".");

            --  Look for the library exchange file in the object directory.

            loop
               Read (Direc, Name, Last);
               exit when Last = 0;

               if Is_Regular_File (Name (1 .. Last)) then
                  Osint.Canonical_Case_File_Name (Name (1 .. Last));
                  exit when Name (1 .. Last) = Library_Exchange_File_Name;
               end if;
            end loop;

            Close (Direc);

            --  If there is a library exchange file then get the generated
            --  file names and delete them, then delete the library exchange
            --  file.

            if Last > 0 then
               Ada.Text_IO.Open
                 (Exchange_File,
                  Ada.Text_IO.In_File,
                  Library_Exchange_File_Name);

               In_Generated := False;
               while not Ada.Text_IO.End_Of_File (Exchange_File) loop
                  Ada.Text_IO.Get_Line (Exchange_File, Name, Last);

                  if Last > 0 then
                     if Name (1) = '[' then
                        In_Generated :=
                          Name (1 .. Last) =
                            Library_Label (Generated_Object_Files)
                          or else
                          Name (1 .. Last) =
                            Library_Label (Generated_Source_Files);

                     elsif In_Generated then
                        if Is_Regular_File (Name (1 .. Last)) then
                           if not Do_Nothing then
                              Set_Writable (Name (1 .. Last));
                           end if;

                           Delete (Obj_Directory, Name (1 .. Last));
                        end if;
                     end if;
                  end if;
               end loop;

               Ada.Text_IO.Close (Exchange_File);

               if not Do_Nothing then
                  Set_Writable (Library_Exchange_File_Name);
               end if;

               Delete (Obj_Directory, Library_Exchange_File_Name);
            end if;

            Change_Dir (Lib_Directory);
            Open (Direc, ".");

            --  For each regular file in the directory, if switch -n has not
            --  been specified, make it writable and delete the file if it is
            --  the library file.

            loop
               Read (Direc, Name, Last);
               exit when Last = 0;

               if Is_Regular_File (Name (1 .. Last))
                 or else Is_Symbolic_Link (Name (1 .. Last))
               then
                  Osint.Canonical_Case_File_Name (Name (1 .. Last));

                  if (Data.Library_Kind = Static and then
                        Name (1 .. Last) =  Archive_Name)
                    or else
                      ((Data.Library_Kind = Dynamic or else
                          Data.Library_Kind = Relocatable)
                       and then
                         Name (1 .. Last) = DLL_Name)
                  then
                     if not Do_Nothing then
                        Set_Writable (Name (1 .. Last));
                     end if;

                     Delete (Lib_Directory, Name (1 .. Last));
                  end if;
               end if;
            end loop;

            Close (Direc);

            if Data.Config.Symbolic_Link_Supported then
               if (Data.Library_Kind = Dynamic
                   or else Data.Library_Kind = Relocatable)
                 and then Data.Lib_Internal_Name /= No_Name
               then
                  declare
                     Lib_Version : String :=
                                     Get_Name_String (Data.Lib_Internal_Name);

                  begin
                     Osint.Canonical_Case_File_Name (Lib_Version);

                     if Data.Config.Lib_Maj_Min_Id_Supported then
                        declare
                           Maj_Version : String :=
                                         Major_Id_Name (DLL_Name, Lib_Version);
                        begin
                           if Maj_Version /= "" then
                              Osint.Canonical_Case_File_Name (Maj_Version);

                              Open (Direc, ".");

                              --  For each regular file in the directory, if
                              --  switch -n has not been specified, make it
                              --  writable and delete the file if it is the
                              --  library major version file.

                              loop
                                 Read (Direc, Name, Last);
                                 exit when Last = 0;

                                 if (Is_Regular_File (Name (1 .. Last))
                                     or else
                                     Is_Symbolic_Link (Name (1 .. Last)))
                                   and then Name (1 .. Last) = Maj_Version
                                 then
                                    if not Do_Nothing then
                                       Set_Writable (Name (1 .. Last));
                                    end if;

                                    Delete (Lib_Directory, Name (1 .. Last));
                                 end if;
                              end loop;

                              Close (Direc);
                           end if;
                        end;
                     end if;

                     Open (Direc, ".");

                     --  For each regular file in the directory, if switch -n
                     --  has not been specified, make it writable and delete
                     --  the file if it is the library version file.

                     loop
                        Read (Direc, Name, Last);
                        exit when Last = 0;

                        if Is_Regular_File (Name (1 .. Last))
                          and then Name (1 .. Last) = Lib_Version
                        then
                           if not Do_Nothing then
                              Set_Writable (Name (1 .. Last));
                           end if;

                           Delete (Lib_Directory, Name (1 .. Last));
                        end if;
                     end loop;

                     Close (Direc);
                  end;
               end if;
            end if;

            Change_Dir (Lib_ALI_Directory);
            Open (Direc, ".");

            --  For each regular file in the directory, if switch -n has not
            --  been specified, make it writable and delete the file if it is
            --  any dependency file of a source of the project.

            loop
               Read (Direc, Name, Last);
               exit when Last = 0;

               if Is_Regular_File (Name (1 .. Last)) then
                  Osint.Canonical_Case_File_Name (Name (1 .. Last));
                  Delete_File := False;

                  if Last > 4 and then Name (Last - 3 .. Last) = ".ali" then
                     declare
                        Source   : Prj.Source_Id;
                        Src_Data : Source_Data;
                     begin
                        Data := Project_Tree.Projects.Table (Project);

                        Project_Loop : loop
                           Source := Data.First_Source;

                           while Source /= No_Source loop
                              Src_Data :=
                                Project_Tree.Sources.Table (Source);

                              if Src_Data.Dep_Name /= No_File
                                and then
                                  Get_Name_String (Src_Data.Dep_Name) =
                                  Name (1 .. Last)
                              then
                                 Delete_File := True;
                                 exit Project_Loop;
                              end if;

                              Source := Src_Data.Next_In_Project;
                           end loop;

                           exit Project_Loop when Data.Extends = No_Project;

                           Data :=
                             Project_Tree.Projects.Table (Data.Extends);
                        end loop Project_Loop;
                     end;
                  end if;

                  if Delete_File then
                     if not Do_Nothing then
                        Set_Writable (Name (1 .. Last));
                     end if;

                     Delete (Lib_ALI_Directory, Name (1 .. Last));
                  end if;

               end if;
            end loop;

            Close (Direc);

            --  Restore the initial working directory

            Change_Dir (Current);
         end;
      end if;
   end Clean_Library_Directory;

   -------------------
   -- Clean_Project --
   -------------------

   procedure Clean_Project (Project : Project_Id) is
      Main_Source_File : File_Name_Type;
      --  Name of executable on the command line without directory info

      Executable : File_Name_Type;
      --  Name of the executable file

      Current_Dir : constant Dir_Name_Str := Get_Current_Dir;
      Data        : Project_Data := Project_Tree.Projects.Table (Project);

      Source_Id   : Prj.Source_Id;
      Source      : Source_Data;

   begin
      --  Check that we don't specify executable on the command line for
      --  a main library project.

      if Project = Main_Project
        and then Mains.Number_Of_Mains /= 0
        and then Data.Library
      then
         Osint.Fail
           ("Cannot specify executable(s) for a Library Project File");
      end if;

      --  Nothing to clean in an externally built project

      if Data.Externally_Built then
         if Verbose_Mode then
            Put ("Nothing to do to clean externally built project """);
            Put (Get_Name_String (Data.Name));
            Put_Line ("""");
         end if;

      else
         if Verbose_Mode then
            Put ("Cleaning project """);
            Put (Get_Name_String (Data.Name));
            Put_Line ("""");
         end if;

         --  Add project to the list of processed projects

         Processed_Projects.Increment_Last;
         Processed_Projects.Table (Processed_Projects.Last) := Project;

         if Data.Object_Directory /= No_Path then
            declare
               Obj_Dir : constant String :=
                           Get_Name_String (Data.Object_Directory);

            begin
               Change_Dir (Obj_Dir);

               --  For non library project, clean the global archive and its
               --  dependency file if they exist.

               if not Data.Library then
                  Clean_Archive (Project);
               end if;

               --  For a static library project, clean the partially link
               --  object, if there is one.

               if Data.Library and then Data.Library_Kind = Static then
                  declare
                     Partial : constant String :=
                                 Partial_Prefix &
                                 Get_Name_String (Data.Library_Name) &
                                 Object_Suffix;

                  begin
                     if Is_Regular_File (Partial) then
                        Delete (Obj_Dir, Partial);
                     end if;
                  end;
               end if;

               --  Check all the object file for the sources of the current
               --  project and all the projects it extends.

               loop
                  --  Delete the object files and the dependency files if they
                  --  exist.

                  Source_Id := Data.First_Source;
                  while Source_Id /= No_Source loop
                     Source :=
                       Project_Tree.Sources.Table (Source_Id);

                     if Source.Object /= No_File and then
                       Is_Regular_File
                         (Get_Name_String (Source.Object))
                     then
                        Delete (Obj_Dir, Get_Name_String (Source.Object));
                     end if;

                     if Source.Dep_Name /= No_File and then
                       Is_Regular_File (Get_Name_String (Source.Dep_Name))
                     then
                        Delete (Obj_Dir, Get_Name_String (Source.Dep_Name));
                     end if;

                     if Source.Switches /= No_File and then
                       Is_Regular_File (Get_Name_String (Source.Switches))
                     then
                        Delete (Obj_Dir, Get_Name_String (Source.Switches));
                     end if;

                     Source_Id := Source.Next_In_Project;
                  end loop;

                  exit when Data.Extends = No_Project;

                  Data := Project_Tree.Projects.Table (Data.Extends);
               end loop;

               --  Restore Data for the original project

               Data := Project_Tree.Projects.Table (Project);
            end;
         end if;

         --  If this is a library project, clean the library directory, the
         --  interface copy dir and, for a Stand-Alone Library, the binder
         --  generated files of the library.

         --  The directories are cleaned only if switch -c is not specified

         if Data.Library then
            if not Compile_Only then
               Clean_Library_Directory (Project);

               if Data.Library_Src_Dir /= No_Path then
                  Clean_Interface_Copy_Directory (Project);
               end if;
            end if;
         end if;

         if Verbose_Mode then
            New_Line;
         end if;
      end if;

      --  If switch -r is specified, call Clean_Project recursively for the
      --  imported projects and the project being extended.

      if All_Projects then
         declare
            Imported : Project_List := Data.Imported_Projects;
            Element  : Project_Element;
            Process  : Boolean;

         begin
            --  For each imported project, call Clean_Project if the project
            --  has not been processed already.

            while Imported /= Empty_Project_List loop
               Element := Project_Tree.Project_Lists.Table (Imported);
               Imported := Element.Next;
               Process := True;

               for
                 J in Processed_Projects.First .. Processed_Projects.Last
               loop
                  if Element.Project = Processed_Projects.Table (J) then
                     Process := False;
                     exit;
                  end if;
               end loop;

               if Process then
                  Clean_Project (Element.Project);
               end if;
            end loop;

            --  If this project extends another project, call Clean_Project for
            --  the project being extended. It is guaranteed that it has not
            --  called before, because no other project may import or extend
            --  this project.

            if Data.Extends /= No_Project then
               Clean_Project (Data.Extends);
            end if;
         end;
      end if;

         --  For the main project, delete the executables and the binder
         --  generated files.

         --  The executables are deleted only if switch -c is not specified

      if Project = Main_Project and then Data.Exec_Directory /= No_Path then
         declare
            Exec_Dir : constant String :=
                         Get_Name_String (Data.Exec_Directory);
            Source   : Prj.Source_Id;

         begin
            Change_Dir (Exec_Dir);

            for N_File in 1 .. Mains.Number_Of_Mains loop
               declare
                  Display_Main : constant String := Mains.Next_Main;
                  Main         : String := Display_Main;
               begin
                  Osint.Canonical_Case_File_Name (Main);
                  Main_Source_File := Create_Name (Main);
               end;

               Source := Project_Tree.First_Source;

               while Source /= No_Source loop
                  exit when Project_Tree.Sources.Table (Source).File =
                              Main_Source_File;

                  Source :=
                    Project_Tree.Sources.Table (Source).Next_In_Sources;
               end loop;

               if not Compile_Only
                 and then Source /= No_Source
               then
                  Executable :=
                    Executable_Of
                      (Project  => Main_Project,
                       In_Tree  => Project_Tree,
                       Main     => Main_Source_File,
                       Index    => 0,
                       Ada_Main =>
                         Project_Tree.Sources.Table (Source).Language_Name =
                         Snames.Name_Ada);

                  declare
                     Exec_File_Name : constant String :=
                                        Get_Name_String (Executable);

                  begin
                     if Is_Absolute_Path (Name => Exec_File_Name) then
                        if Is_Regular_File (Exec_File_Name) then
                           Delete ("", Exec_File_Name);
                        end if;

                     else
                        if Is_Regular_File (Exec_File_Name) then
                           Delete (Exec_Dir, Exec_File_Name);
                        end if;
                     end if;
                  end;
               end if;

               if Data.Object_Directory /= No_Path then
                  Delete_Binder_Generated_Files
                    (Get_Name_String
                       (Data.Object_Directory),
                     Main_Source_File);
               end if;
            end loop;
         end;
      end if;

      --  Change back to previous directory

      Change_Dir (Current_Dir);
   end Clean_Project;

   ------------
   -- Delete --
   ------------

   procedure Delete (In_Directory : String; File : String) is
      Full_Name : String (1 .. In_Directory'Length + File'Length + 1);
      Last : Natural := 0;
      Success : Boolean;

   begin
      --  Indicate that at least one file is deleted or is to be deleted

      File_Deleted := True;

      --  Build the path name of the file to delete

      Last := In_Directory'Length;
      Full_Name (1 .. Last) := In_Directory;

      if Last > 0 and then Full_Name (Last) /= Directory_Separator then
         Last := Last + 1;
         Full_Name (Last) := Directory_Separator;
      end if;

      Full_Name (Last + 1 .. Last + File'Length) := File;
      Last := Last + File'Length;

      --  If switch -n was used, simply output the path name

      if Do_Nothing then
         Put_Line (Full_Name (1 .. Last));

      --  Otherwise, delete the file if it is writable

      else
         if Force_Deletions
           or else Is_Writable_File (Full_Name (1 .. Last))
         then
            Delete_File (Full_Name (1 .. Last), Success);
         else
            Success := False;
         end if;

         if Verbose_Mode or else not Quiet_Output then
            if not Success then
               Put ("Warning: """);
               Put (Full_Name (1 .. Last));
               Put_Line (""" could not be deleted");

            else
               Put ("""");
               Put (Full_Name (1 .. Last));
               Put_Line (""" has been deleted");
            end if;
         end if;
      end if;
   end Delete;

   -----------------------------------
   -- Delete_Binder_Generated_Files --
   -----------------------------------

   procedure Delete_Binder_Generated_Files
     (Dir    : String;
      Source : File_Name_Type)
   is
      Current     : constant String := Get_Current_Dir;
      B_Data      : Binding_Data;
      Base_Name   : File_Name_Type;

   begin
      There_Are_Binder_Drivers := False;
      Find_Binding_Languages;

      if There_Are_Binder_Drivers then
         --  Get the main base name

         Get_Name_String (Source);
         Osint.Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));

         for J in reverse 4 .. Name_Len loop
            if Name_Buffer (J) = '.' then
               Name_Len := J - 1;
               exit;
            end if;
         end loop;

         Base_Name := Name_Find;

         --  Work in the object directory

         Change_Dir (Dir);

         for B_Index in 1 .. Binding_Languages.Last loop
            B_Data := Binding_Languages.Table (B_Index);

            declare
               File_Name : constant String :=
                             Binder_Exchange_File_Name
                               (Base_Name, B_Data.Binder_Prefix).all;
               File      : Ada.Text_IO.File_Type;
               Line      : String (1 .. 1_000);
               Last      : Natural;
               Section   : Binding_Section := No_Binding_Section;
            begin
               if Is_Regular_File (File_Name) then
                  Ada.Text_IO.Open (File, Ada.Text_IO.In_File, File_Name);

                  while not Ada.Text_IO.End_Of_File (File) loop
                     Ada.Text_IO.Get_Line (File, Line, Last);

                     if Last > 0 then
                        if Line (1) = '[' then
                           Section :=
                             Get_Binding_Section (Line (1 .. Last));

                        else
                           case Section is
                              when Generated_Object_File |
                                   Generated_Source_Files =>

                                 if Is_Regular_File (Line (1 .. Last)) then
                                    Delete (Dir, Line (1 .. Last));
                                 end if;

                              when others =>
                                 null;
                           end case;
                        end if;
                     end if;
                  end loop;

                  Ada.Text_IO.Close (File);

                  Delete (Dir, File_Name);
               end if;
            end;
         end loop;

         --  Change back to previous directory

         Change_Dir (Current);
      end if;
   end Delete_Binder_Generated_Files;

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

   --------------
   -- Gprclean --
   --------------

   procedure Gprclean is
   begin
      --  Do the necessary initializations

      Cleangpr.Initialize;

      --  Parse the command line, getting the switches and the executable names

      Parse_Cmd_Line;

      --  If no project file was specified, look first for a default

      if Project_File_Name = null then
         Look_For_Default_Project;
      end if;

      --  Check that a project file was specified and get the configuration.

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

      Get_Configuration (Packages_To_Check);

      --  Finish processing the project file

      Sinput.P.Reset_First;

      Prj.Proc.Process_Project_Tree_Phase_2
        (In_Tree                => Project_Tree,
         Project                => Main_Project,
         Success                => Gpr_Util.Success,
         From_Project_Node      => User_Project_Node,
         From_Project_Node_Tree => Project_Node_Tree,
         Report_Error           => null,
         Current_Dir            => Get_Current_Dir,
         When_No_Sources        => Silent);

      if not Gpr_Util.Success then
         Prj.Err.Finalize;
         Osint.Fail ("""" & Project_File_Name.all & """ processing failed");
      end if;

      if Opt.Verbose_Mode then
         New_Line;
         Put ("Parsing of Project File """);
         Put (Project_File_Name.all);
         Put (""" is finished.");
         New_Line;
      end if;

      --  If no executable name was specified, put all the mains of the project
      --  file (if any) as if there were on the command line.

      if Mains.Number_Of_Mains = 0 then
         declare
            Value : String_List_Id :=
                      Project_Tree.Projects.Table (Main_Project).Mains;
            Main  : String_Element;
         begin
            while Value /= Prj.Nil_String loop
               Main := Project_Tree.String_Elements.Table (Value);
               Mains.Add_Main
                 (Name => Get_Name_String (Main.Value));
               Value := Main.Next;
            end loop;
         end;
      end if;

      if Verbose_Mode then
         New_Line;
      end if;

      Processed_Projects.Init;
      Clean_Project (Main_Project);

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
   end Gprclean;

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
      Prj.Set_Mode (Multi_Language);
   end Initialize;

   --------------------
   -- Parse_Cmd_Line --
   --------------------

   procedure Parse_Cmd_Line is
      Index : Positive := 1;
      Last         : constant Natural := Argument_Count;

   begin
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
               Osint.Fail ("invalid argument """, Arg, """");
            end Bad_Argument;

         begin
            --  First deal with --version and --help

            Check_Version_And_Help
              ("GPRCLEAN",
               "2006",
               Version_String => Gpr_Version_String);

            --  Now deal with the other options

            if Arg'Length /= 0 then
               if Arg (1) = '-' then
                  if Arg'Length = 1 then
                     Bad_Argument;
                  end if;

                  case Arg (2) is
                     when '-' =>
                        if Arg'Length > Config_Project_Option'Length and then
                          Arg (1 .. Config_Project_Option'Length) =
                          Config_Project_Option
                        then
                           if Config_Project_File_Name /= null then
                              Fail_Program
                                ("several configuration switches cannot " &
                                 "be specified");

                           elsif Target_Name /= null then
                              Fail_Program
                                ("configuration and target switches cannot " &
                                 "be used together");

                           else

                              Autoconfiguration := False;
                              Config_Project_File_Name :=
                                new String'
                                  (Arg (Config_Project_Option'Length + 1 ..
                                     Arg'Last));
                           end if;

                        elsif Arg'Length > Autoconf_Project_Option'Length
                              and then
                              Arg (1 .. Autoconf_Project_Option'Length) =
                                Autoconf_Project_Option
                        then
                           if Config_Project_File_Name /= null then
                              Fail_Program
                                ("several configuration switches cannot " &
                                 "be specified");

                           elsif Target_Name /= null then
                              Fail_Program
                                ("configuration and target switches cannot " &
                                 "be used together");

                           else
                              Config_Project_File_Name :=
                                new String'
                                  (Arg (Autoconf_Project_Option'Length + 1 ..
                                        Arg'Last));
                           end if;

                        elsif
                          Arg'Length > Target_Project_Option'Length
                          and then
                          Arg (1 .. Target_Project_Option'Length) =
                             Target_Project_Option
                        then
                           if Target_Name /= null then
                              Fail_Program
                              ("several target switches cannot be specified");

                           elsif Config_Project_File_Name /= null then
                              Fail_Program
                                ("configuration and target switches cannot " &
                                 "be used together");

                           else
                              Target_Name :=
                                new String'
                                  (Arg (Target_Project_Option'Length + 1 ..
                                        Arg'Last));
                           end if;

                        elsif Arg'Length > Subdirs_Option'Length and then
                          Arg (1 .. Subdirs_Option'Length) =
                            Subdirs_Option
                        then
                           Subdirs :=
                             new String'
                               (Arg (Subdirs_Option'Length + 1 .. Arg'Last));

                        else
                           Bad_Argument;
                        end if;

                     when 'a' =>
                        if Arg'Length < 4 then
                           Bad_Argument;
                        end if;

                        if Arg (3) = 'P' then
                           Prj.Ext.Add_Search_Project_Directory
                             (Arg (4 .. Arg'Last));

                        else
                           Bad_Argument;
                        end if;

                     when 'c'    =>
                        Compile_Only := True;

                     when 'e' =>
                        if Arg = "-eL" then
                           Follow_Links_For_Files := True;

                        else
                           Bad_Argument;
                        end if;

                     when 'f' =>
                        Force_Deletions := True;

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
                              if Prj'Length > 1 and then
                                Prj (Prj'First) = '='
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
                           Ext_Asgn  : constant String := Arg (3 .. Arg'Last);
                           Start     : Positive := Ext_Asgn'First;
                           Stop      : Natural  := Ext_Asgn'Last;
                           Equal_Pos : Natural;
                           OK        : Boolean  := True;

                        begin
                           if Ext_Asgn (Start) = '"' then
                              if Ext_Asgn (Stop) = '"' then
                                 Start := Start + 1;
                                 Stop  := Stop - 1;

                              else
                                 OK := False;
                              end if;
                           end if;

                           Equal_Pos := Start;

                           while Equal_Pos <= Stop
                             and then Ext_Asgn (Equal_Pos) /= '='
                           loop
                              Equal_Pos := Equal_Pos + 1;
                           end loop;

                           if Equal_Pos = Start or else Equal_Pos > Stop then
                              OK := False;
                           end if;

                           if OK then
                              Prj.Ext.Add
                                (External_Name =>
                                   Ext_Asgn (Start .. Equal_Pos - 1),
                                 Value         =>
                                   Ext_Asgn (Equal_Pos + 1 .. Stop));

                           else
                              Osint.Fail
                                ("illegal external assignment '",
                                 Ext_Asgn, "'");
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

   ---------------------------
   -- Ultimate_Extension_Of --
   ---------------------------

   function Ultimate_Extension_Of (Project : Project_Id) return Project_Id is
      Result : Project_Id := Project;
      Data   : Project_Data;

   begin
      if Project /= No_Project then
         loop
            Data := Project_Tree.Projects.Table (Result);
            exit when Data.Extended_By = No_Project;
            Result := Data.Extended_By;
         end loop;
      end if;

      return Result;
   end Ultimate_Extension_Of;

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

         Put_Line ("  --config=file.cgpr");
         Put_Line ("           Specify the configuration project file name");
         Put_Line ("  --autoconf=file.cgpr");
         Put_Line
           ("           Specify/create the main config project file name");
         Put_Line ("  --target=targetname");
         Put_Line ("           Specify a target for cross polatforms");
         Put_Line ("  --subdirs=dir");
         Put_Line ("           Real obj/lib/exec dirs are subdirs");
         New_Line;

         Put_Line ("  -aPdir   Add directory dir to project search path");
         Put_Line ("  -c       Only delete compiler generated files");
         Put_Line ("  -eL      Follow symbolic links when processing " &
                   "project files");
         Put_Line ("  -f       Force deletions of unwritable files");
         Put_Line ("  -F       Full project path name " &
                   "in brief error messages");
         Put_Line ("  -h       Display this message");
         Put_Line ("  -n       Nothing to do: only list files to delete");
         Put_Line ("  -P<proj> Use Project File <proj>");
         Put_Line ("  -q       Be quiet/terse");
         Put_Line ("  -r       Clean all projects recursively");
         Put_Line ("  -v       Verbose mode");
         Put_Line ("  -vPx     Specify verbosity when parsing Project Files");
         Put_Line ("  -Xnm=val Specify an external reference " &
                   "for Project Files");
         New_Line;
      end if;
   end Usage;
end Cleangpr;
