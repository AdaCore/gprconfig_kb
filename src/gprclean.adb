------------------------------------------------------------------------------
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             G P R C L E A N                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2006-2013, Free Software Foundation, Inc.          --
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

with Ada.Text_IO;

with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.IO;                   use GNAT.IO;
with GNAT.Regexp;               use GNAT.Regexp;

with Gpr_Util;    use Gpr_Util;
with Gprexch;     use Gprexch;
with MLib;        use MLib;
with Makeutl;     use Makeutl;
with Namet;       use Namet;
with Opt;         use Opt;
with Osint;
with Prj.Util;    use Prj.Util;
with Snames;
with Types;       use Types;

package body Gprclean is

   use Knowledge;

   -----------------------------
   -- Other local subprograms --
   -----------------------------

   function Object_Artifact
     (Object_File_Name   : File_Name_Type;
      Artifact_Extension : Name_Id)
      return String;

   procedure Clean_Archive (Project : Project_Id);
   --  Delete a global archive and its dependency file, if they exist

   procedure Clean_Interface_Copy_Directory
     (Project : Project_Id; Project_Tree : Project_Tree_Ref);
   --  Delete files in an interface copy directory: any file that is a copy of
   --  a source of the project.

   procedure Clean_Library_Directory
     (Project      : Project_Id;
      Project_Tree : Project_Tree_Ref);
   --  Delete the library file in a library directory and any ALI file
   --  of a source of the project in a library ALI directory.

   procedure Delete_Binder_Generated_Files
     (Main_Project : Project_Id;
      Project_Tree : Project_Tree_Ref;
      Dir          : String;
      Source       : Source_Id);
   --  Delete the binder generated file in directory Dir for Source

   function Ultimate_Extension_Of (Project : Project_Id) return Project_Id;
   --  Returns either Project, if it is not extended by another project, or
   --  the project that extends Project, directly or indirectly, and that is
   --  not itself extended. Returns No_Project if Project is No_Project.

   function Object_Artifact
     (Object_File_Name   : File_Name_Type;
      Artifact_Extension : Name_Id)
     return String
   is
      Object : constant String := Get_Name_String (Object_File_Name);
      Last   : Natural := Object'Last;

   begin
      while Last > 0 and then Object (Last) /= '.' loop
         Last := Last - 1;
      end loop;

      if Last = 0 then
         Last := Object'Last + 1;
      end if;

      return Object (1 .. Last - 1) & Get_Name_String (Artifact_Extension);
   end Object_Artifact;

   -------------------
   -- Clean_Archive --
   -------------------

   procedure Clean_Archive (Project : Project_Id) is
      Current_Dir  : constant Dir_Name_Str := Get_Current_Dir;
      Archive_Name : constant String :=
                       "lib" & Get_Name_String (Project.Name)
                       & Get_Name_String (Project.Config.Archive_Suffix);
      --  The name of the archive file for this project

      Archive_Dep_Name : constant String :=
                           "lib" & Get_Name_String (Project.Name) & ".deps";
      --  The name of the archive dependency file for this project

      Obj_Dir     : constant String :=
                      Get_Name_String (Project.Object_Directory.Display_Name);

   begin
      if Is_Directory (Obj_Dir) then
         Change_Dir (Obj_Dir);

         if Is_Regular_File (Archive_Name) then
            Delete (Obj_Dir, Archive_Name);
         end if;

         if Is_Regular_File (Archive_Dep_Name) then
            Delete (Obj_Dir, Archive_Dep_Name);
         end if;

         Change_Dir (Current_Dir);
      end if;
   end Clean_Archive;

   ------------------------------------
   -- Clean_Interface_Copy_Directory --
   ------------------------------------

   procedure Clean_Interface_Copy_Directory
     (Project : Project_Id; Project_Tree : Project_Tree_Ref)
   is
      Current : constant String := Get_Current_Dir;

      Direc : Dir_Type;

      Name : String (1 .. 200);
      Last : Natural;

      Delete_File : Boolean;

      Source      : Prj.Source_Id;

      File_Name   : File_Name_Type;

   begin
      if Project.Library
        and then Project.Library_Src_Dir /= No_Path_Information
      then
         declare
            Directory : constant String :=
                          Get_Name_String (Project.Library_Src_Dir.Name);
            Iter      : Source_Iterator;

         begin
            if Is_Directory (Directory) then
               Change_Dir (Directory);
               Open (Direc, ".");

               --  For each regular file in the directory, if switch -n has not
               --  been specified, make it writable and delete the file if it
               --  is a copy of a source of the project.

               loop
                  Read (Direc, Name, Last);
                  exit when Last = 0;

                  if Is_Regular_File (Name (1 .. Last)) then
                     Osint.Canonical_Case_File_Name (Name (1 .. Last));

                     Name_Len := Last;
                     Name_Buffer (1 .. Name_Len) := Name (1 .. Last);
                     File_Name := Name_Find;

                     Delete_File := False;

                     Iter := For_Each_Source (Project_Tree);

                     loop
                        Source := Prj.Element (Iter);
                        exit when Source = No_Source;

                        if Ultimate_Extension_Of (Source.Project) = Project
                          and then Source.File = File_Name
                        then
                           Delete_File := True;
                           exit;
                        end if;

                        Next (Iter);
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
            end if;
         end;
      end if;
   end Clean_Interface_Copy_Directory;

   -----------------------------
   -- Clean_Library_Directory --
   -----------------------------

   procedure Clean_Library_Directory
     (Project      : Project_Id;
      Project_Tree : Project_Tree_Ref)
   is
      Current : constant String := Get_Current_Dir;

      Lib_Filename               : constant String :=
                                     Get_Name_String (Project.Library_Name);
      DLL_Name                   : String :=
                                     Get_Name_String
                                       (Project.Config.Shared_Lib_Prefix)
                                     & Lib_Filename
                                     & Get_Name_String
                                       (Project.Config.Shared_Lib_Suffix);
      Archive_Name               : String :=
                                     "lib" & Lib_Filename
                                     & Get_Name_String
                                       (Project.Config.Archive_Suffix);
      Library_Exchange_File_Name : constant String :=
                                     Lib_Filename & Library_Exchange_Suffix;

      Direc        : Dir_Type;

      Name : String (1 .. 200);
      Last : Natural;

      Delete_File : Boolean;

   begin
      if Project.Library then
         Osint.Canonical_Case_File_Name (DLL_Name);
         Osint.Canonical_Case_File_Name (Archive_Name);

         declare
            Obj_Directory     : String_Access := null;
            Lib_Directory     : constant String :=
                                  Get_Name_String
                                    (Project.Library_Dir.Display_Name);
            Lib_ALI_Directory : constant String :=
                                  Get_Name_String
                                    (Project.Library_ALI_Dir.Display_Name);

            Exchange_File : Ada.Text_IO.File_Type;

            In_Generated : Boolean;

         begin
            if Project.Object_Directory.Display_Name /= No_Path then
               Obj_Directory :=
                 new String'
                   (Get_Name_String (Project.Object_Directory.Display_Name));

               if Is_Directory (Obj_Directory.all) then
                  Change_Dir (Obj_Directory.all);

                  Open (Direc, ".");

                  --  Look for the library exchange file in the object
                  --  directory.

                  loop
                     Read (Direc, Name, Last);
                     exit when Last = 0;

                     if Is_Regular_File (Name (1 .. Last)) then
                        Osint.Canonical_Case_File_Name (Name (1 .. Last));
                        exit when
                          Name (1 .. Last) = Library_Exchange_File_Name;
                     end if;
                  end loop;

                  Close (Direc);

                  --  If there is a library exchange file then get the
                  --  generated file names and delete them, then delete
                  --  the library exchange file.

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

                                 Delete (Obj_Directory.all, Name (1 .. Last));
                              end if;
                           end if;
                        end if;
                     end loop;

                     Ada.Text_IO.Close (Exchange_File);

                     if not Do_Nothing then
                        Set_Writable (Library_Exchange_File_Name);
                     end if;

                     Delete (Obj_Directory.all, Library_Exchange_File_Name);
                  end if;

                  Change_Dir (Current);
               end if;
            end if;

            if Is_Directory (Lib_Directory) then
               Change_Dir (Lib_Directory);
               Open (Direc, ".");

               --  For each regular file in the directory, if switch -n has not
               --  been specified, make it writable and delete the file if it
               --  is the library file.

               loop
                  Read (Direc, Name, Last);
                  exit when Last = 0;

                  if Is_Regular_File (Name (1 .. Last))
                    or else Is_Symbolic_Link (Name (1 .. Last))
                  then
                     Osint.Canonical_Case_File_Name (Name (1 .. Last));

                     if (Project.Library_Kind = Static
                         and then Name (1 .. Last) =  Archive_Name)
                       or else
                         ((Project.Library_Kind = Dynamic
                           or else Project.Library_Kind = Relocatable)
                          and then Name (1 .. Last) = DLL_Name)
                     then
                        if not Do_Nothing then
                           Set_Writable (Name (1 .. Last));
                        end if;

                        Delete (Lib_Directory, Name (1 .. Last));
                     end if;
                  end if;
               end loop;

               Close (Direc);

               if Project.Config.Symbolic_Link_Supported then
                  if (Project.Library_Kind = Dynamic
                      or else Project.Library_Kind = Relocatable)
                    and then Project.Lib_Internal_Name /= No_Name
                  then
                     declare
                        Lib_Version : String :=
                          Get_Name_String (Project.Lib_Internal_Name);

                     begin
                        Osint.Canonical_Case_File_Name (Lib_Version);

                        if Project.Config.Lib_Maj_Min_Id_Supported then
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

                                       Delete
                                         (Lib_Directory, Name (1 .. Last));
                                    end if;
                                 end loop;

                                 Close (Direc);
                              end if;
                           end;
                        end if;

                        Open (Direc, ".");

                        --  For each regular file in the directory, if switch
                        --  -n has not been specified, make it writable and
                        --  delete the file if it is the library version file.

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

               Change_Dir (Current);
            end if;

            if Is_Directory (Lib_ALI_Directory) then
               Change_Dir (Lib_ALI_Directory);
               Open (Direc, ".");

               --  For each regular file in the directory, if switch -n has not
               --  been specified, make it writable and delete the file if it
               --  is any dependency file of a source of the project.

               loop
                  Read (Direc, Name, Last);
                  exit when Last = 0;

                  if Is_Regular_File (Name (1 .. Last)) then
                     Osint.Canonical_Case_File_Name (Name (1 .. Last));
                     Delete_File := False;

                     if Last > 4 and then Name (Last - 3 .. Last) = ".ali" then
                        declare
                           Source   : Prj.Source_Id;
                           Iter     : Source_Iterator;
                           Proj     : Project_Id := Project;
                        begin
                           Project_Loop : loop
                              Iter := For_Each_Source (Project_Tree, Proj);

                              loop
                                 Source := Prj.Element (Iter);
                                 exit when Source = No_Source;

                                 if Source.Dep_Name /= No_File
                                   and then
                                     Get_Name_String (Source.Dep_Name) =
                                   Name (1 .. Last)
                                 then
                                    Delete_File := True;
                                    exit Project_Loop;
                                 end if;

                                 Next (Iter);
                              end loop;

                              exit Project_Loop when Proj.Extends = No_Project;

                              Proj := Proj.Extends;
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
            end if;
         end;
      end if;
   end Clean_Library_Directory;

   --  Artifacts

   type Artifact_Array_Type is array (Positive range <>) of GNAT.Regexp.Regexp;
   type Artifact_Array_Ptr is access Artifact_Array_Type;

   Artifacts : Artifact_Array_Ptr := new Artifact_Array_Type (1 .. 4);
   --  List of regular expression file names to be deleted in procedure
   --  Clean_Artifacts below. Size 4 is arbitrary.

   Artifact_Last : Natural := 0;
   --  Last index of the valid artifacts in array Artifacts.

   -------------------
   -- Clean_Project --
   -------------------

   procedure Clean_Project
     (Project            : Project_Id;
      Project_Tree       : Project_Tree_Ref;
      Main               : Boolean;
      Remove_Executables : Boolean)
   is
      Executable : File_Name_Type;
      --  Name of the executable file

      Current_Dir : constant Dir_Name_Str := Get_Current_Dir;
      Project2    : Project_Id;

      Source_Id   : Prj.Source_Id;

      Partial_Number : Natural;

      List : Name_List_Index := No_Name_List;
      Node : Name_Node;

      procedure Clean_Artifacts (Dir : String; List : Name_List_Index);
      --  Clean the artifacts specified by List in directory Dir.
      --  The current directory is Dir.

      function Is_Regexp (Name : String) return Boolean;
      --  Return True iff Name is a glob regexp.

      ---------------------
      -- Clean_Artifacts --
      ---------------------

      procedure Clean_Artifacts (Dir : String; List : Name_List_Index) is
         Lst : Name_List_Index := List;
         Nod : Name_Node;

      begin
         Artifact_Last := 0;

         while Lst /= No_Name_List loop
            Nod := Project_Tree.Shared.Name_Lists.Table (Lst);

            declare
               Name : constant String := Get_Name_String (Nod.Name);
            begin

               if Is_Regexp (Name) then
                  if Artifact_Last = Artifacts'Length then
                     declare
                        New_Artifacts : constant Artifact_Array_Ptr :=
                          new Artifact_Array_Type (1 .. 2 * Artifact_Last);
                     begin
                        New_Artifacts (1 .. Artifact_Last) :=
                          Artifacts (1 .. Artifact_Last);
                        Artifacts := New_Artifacts;
                     end;
                  end if;

                  Artifact_Last := Artifact_Last + 1;
                  Artifacts (Artifact_Last) :=
                    Compile (Name, Glob => True);

               elsif Is_Regular_File (Name) then
                  Delete (Dir, Name);
               end if;
            end;

            Lst := Nod.Next;
         end loop;

         if Artifact_Last > 0 then
            declare
               Directory : Dir_Type;
               File_Name : Dir_Name_Str (1 .. 1_000);
               Last      : Natural;

            begin
               Open (Directory, Dir);

               Directory_Loop :
               loop
                  Read (Directory, File_Name, Last);
                  exit Directory_Loop when Last = 0;

                  if Is_Regular_File (File_Name (1 .. Last)) then
                     Artifact_Loop :
                     for J in 1 .. Artifact_Last loop
                        if Match
                          (File_Name (1 .. Last), Artifacts (J))
                        then
                           Delete (Dir, File_Name (1 .. Last));
                           exit Artifact_Loop;
                        end if;
                     end loop Artifact_Loop;
                  end if;
               end loop Directory_Loop;

               Close (Directory);
            end;
         end if;

      end Clean_Artifacts;

      ---------------
      -- Is_Regexp --
      ---------------

      function Is_Regexp (Name : String) return Boolean is
      begin
         for J in Name'Range loop
            case Name (J) is
               when '?' | '*' | '[' | '{' =>
                  return True;

               when others =>
                  null;
            end case;
         end loop;

         return False;
      end Is_Regexp;

   begin
      --  Check that we don't specify executable on the command line for
      --  a main library project.

      if Project = Main_Project
        and then Mains.Number_Of_Mains (null) /= 0
        and then Project.Library
      then
         Osint.Fail
           ("Cannot specify executable(s) for a Library Project File");
      end if;

      --  Nothing to clean in an externally built project

      if Project.Externally_Built then
         if Verbose_Mode then
            Put ("Nothing to do to clean externally built project """);
            Put (Get_Name_String (Project.Name));
            Put_Line ("""");
         end if;

      else
         if Verbose_Mode then
            Put ("Cleaning project """);
            Put (Get_Name_String (Project.Name));
            Put_Line ("""");
         end if;

         --  Add project to the list of processed projects

         Processed_Projects.Increment_Last;
         Processed_Projects.Table (Processed_Projects.Last) := Project;

         if Project.Object_Directory /= No_Path_Information
           and then Is_Directory
             (Get_Name_String (Project.Object_Directory.Display_Name))
         then
            declare
               Obj_Dir : constant String :=
                 Get_Name_String (Project.Object_Directory.Display_Name);
               Iter    : Source_Iterator;

            begin
               Change_Dir (Obj_Dir);

               --  For non library project, clean the global archive and its
               --  dependency file if they exist.

               if not Project.Library then
                  Clean_Archive (Project);
               end if;

               --  For a library project, clean the partially link objects, if
               --  there are some.

               if Project.Library then
                  Partial_Number := 0;
                  loop
                     declare
                        Partial : constant String :=
                                    Partial_Name
                                      (Get_Name_String (Project.Library_Name),
                                       Partial_Number,
                                       Object_Suffix);

                     begin
                        if Is_Regular_File (Partial) then
                           Delete (Obj_Dir, Partial);
                           Partial_Number := Partial_Number + 1;

                        else
                           exit;
                        end if;
                     end;
                  end loop;
               end if;

               --  Check all the object file for the sources of the current
               --  project and all the projects it extends.

               Project2 := Project;
               while Project2 /= No_Project loop
                  --  Delete the object files, the dependency files, the
                  --  switches files if they exist. Also additional artifacts
                  --  if they are any.

                  Iter := For_Each_Source (Project_Tree, Project2);
                  loop
                     Source_Id := Prj.Element (Iter);
                     exit when Source_Id = No_Source;

                     if Source_Id.Object /= No_File
                       and then Is_Regular_File
                         (Get_Name_String (Source_Id.Object))
                     then
                        Delete (Obj_Dir, Get_Name_String (Source_Id.Object));

                        --  Clean object artifacts, if any

                        List :=
                          Source_Id.Language.Config.Clean_Object_Artifacts;
                        while List /= No_Name_List loop
                           Node := Project_Tree.Shared.Name_Lists.Table (List);

                           declare
                              Artifact : constant String :=
                                Object_Artifact (Source_Id.Object, Node.Name);

                           begin
                              if Is_Regular_File (Artifact) then
                                 Delete (Obj_Dir, Artifact);
                              end if;
                           end;

                           List := Node.Next;
                        end loop;
                     end if;

                     if Source_Id.Dep_Name /= No_File
                       and then Is_Regular_File
                         (Get_Name_String (Source_Id.Dep_Name))
                     then
                        Delete (Obj_Dir, Get_Name_String (Source_Id.Dep_Name));
                     end if;

                     if Source_Id.Switches /= No_File
                       and then Is_Regular_File
                         (Get_Name_String (Source_Id.Switches))
                     then
                        Delete (Obj_Dir, Get_Name_String (Source_Id.Switches));
                     end if;

                     --  Clean source artifacts, if any

                     List := Source_Id.Language.Config.Clean_Source_Artifacts;
                     while List /= No_Name_List loop
                        Node := Project_Tree.Shared.Name_Lists.Table (List);

                        declare
                           Artifact : constant String :=
                             Get_Name_String (Source_Id.File) &
                             Get_Name_String (Node.Name);
                        begin
                           if Is_Regular_File (Artifact) then
                              Delete (Obj_Dir, Artifact);
                           end if;
                        end;

                        List := Node.Next;
                     end loop;

                     Next (Iter);
                  end loop;

                  Project2 := Project2.Extends;
               end loop;

               --  Clean the artifacts in object directory, if any. Do this
               --  after cleaning the object files, to avoid checking every
               --  object file when the artifacts are given as a regexp.

               Clean_Artifacts
                    (Obj_Dir, Project.Config.Artifacts_In_Object_Dir);
            end;
         end if;

         --  If this is a library project, clean the library directory, the
         --  interface copy dir and, for a Stand-Alone Library, the binder
         --  generated files of the library.

         --  The directories are cleaned only if switch -c is not specified

         if Project.Library then
            if not Compile_Only then
               Clean_Library_Directory (Project, Project_Tree);

               if Project.Library_Src_Dir /= No_Path_Information then
                  Clean_Interface_Copy_Directory (Project, Project_Tree);
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
            Imported : Project_List := Project.Imported_Projects;
            Process  : Boolean;

         begin
            --  For each imported project, call Clean_Project if the project
            --  has not been processed already.

            while Imported /= null loop
               Process := True;

               for
                 J in Processed_Projects.First .. Processed_Projects.Last
               loop
                  if Imported.Project = Processed_Projects.Table (J) then
                     Process := False;
                     exit;
                  end if;
               end loop;

               if Process then
                  Clean_Project (Imported.Project, Project_Tree, False, False);
               end if;
               Imported := Imported.Next;
            end loop;

            --  If this project extends another project, call Clean_Project for
            --  the project being extended. It is guaranteed that it has not
            --  called before, because no other project may import or extend
            --  this project.

            if Project.Extends /= No_Project then
               Clean_Project (Project.Extends, Project_Tree, False, False);
            end if;
         end;
      end if;

      --  For the main project, delete the executables and the binder generated
      --  files.

      --  The executables are deleted only if switch -c is not specified

      if Main
        and then Project.Exec_Directory /= No_Path_Information
        and then Is_Directory
                   (Get_Name_String (Project.Exec_Directory.Display_Name))
      then
         declare
            Exec_Dir  : constant String :=
                          Get_Name_String
                            (Project.Exec_Directory.Display_Name);
            Main_File : Main_Info;

         begin
            Change_Dir (Exec_Dir);

            --  Clean the artifacts in the exec dir, if any

            Clean_Artifacts (Exec_Dir, Project.Config.Artifacts_In_Exec_Dir);

            Mains.Reset;
            loop
               Main_File := Mains.Next_Main;
               exit when Main_File = No_Main_Info;

               if Main_File.Tree = Project_Tree then
                  if Remove_Executables
                    and then Main_File.Source /= No_Source
                  then
                     Executable :=
                       Executable_Of
                         (Project  => Project,
                          Shared   => Project_Tree.Shared,
                          Main     => Main_File.File,
                          Index    => Main_File.Index,
                          Ada_Main =>
                            Main_File.Source.Language.Name = Snames.Name_Ada);

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

                  --  Delete the binder generated files only if the main source
                  --  has been found and if there is an object directory.

                  if Main_File.Source /= No_Source
                    and then Project.Object_Directory /= No_Path_Information
                    and then Is_Directory
                               (Get_Name_String
                                  (Project.Object_Directory.Display_Name))
                  then
                     Delete_Binder_Generated_Files
                       (Project, Project_Tree,
                        Get_Name_String
                          (Project.Object_Directory.Display_Name),
                        Main_File.Source);
                  end if;
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
      Last      : Natural := 0;
      Success   : Boolean;

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
     (Main_Project : Project_Id;
      Project_Tree : Project_Tree_Ref;
      Dir          : String;
      Source       : Source_Id)
   is
      Data      : constant Builder_Data_Access := Builder_Data (Project_Tree);
      Current   : constant String := Get_Current_Dir;
      B_Data    : Binding_Data;
      Base_Name : File_Name_Type;

   begin
      Find_Binding_Languages (Project_Tree, Main_Project);

      if Data.There_Are_Binder_Drivers then
         --  Get the main base name

         Base_Name := Base_Name_Index_For
           (Get_Name_String (Source.File), Source.Index, '~');

         --  Work in the object directory

         Change_Dir (Dir);

         B_Data := Data.Binding;
         while B_Data /= null loop
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

            B_Data := B_Data.Next;
         end loop;

         --  Change back to previous directory

         Change_Dir (Current);
      end if;
   end Delete_Binder_Generated_Files;

   ---------------------------
   -- Ultimate_Extension_Of --
   ---------------------------

   function Ultimate_Extension_Of (Project : Project_Id) return Project_Id is
      Result : Project_Id := Project;

   begin
      if Project /= No_Project then
         loop
            exit when Result.Extended_By = No_Project;
            Result := Result.Extended_By;
         end loop;
      end if;

      return Result;
   end Ultimate_Extension_Of;

end Gprclean;