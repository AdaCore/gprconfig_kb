------------------------------------------------------------------------------
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                      G P R I N S T A L L . M A I N                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2012-2013, Free Software Foundation, Inc.          --
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

with Ada.Characters.Handling;                use Ada.Characters.Handling;
with Ada.Containers.Indefinite_Vectors;      use Ada;
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Containers.Ordered_Sets;
with Ada.Directories;                        use Ada.Directories;
with Ada.Strings.Equal_Case_Insensitive;
with Ada.Strings.Less_Case_Insensitive;
with Ada.Strings.Fixed;                      use Ada.Strings;
with Ada.Strings.Unbounded;
with Ada.Text_IO;                            use Ada.Text_IO;

with Gpr_Util;    use Gpr_Util;
with GPR_Version; use GPR_Version;

with Makeutl;
with Namet;    use Namet;
with Opt;
with Output;   use Output;
with Prj.Util; use Prj.Util;
with Snames;   use Snames;

package body Gprinstall.Install is

   package Name_Id_Set is new Containers.Ordered_Sets (Name_Id);

   Installed : Name_Id_Set.Set;

   -------------
   -- Process --
   -------------

   procedure Process
     (Tree    : Project_Tree_Ref;
      Project : Project_Id)
   is

      Pcks : Package_Table.Table_Ptr renames Tree.Shared.Packages.Table;
      Strs : String_Element_Table.Table_Ptr renames
               Tree.Shared.String_Elements.Table;

      --  Local values for the given project, these are initially set with the
      --  default values. It is updated using the Install package found in the
      --  project if any.

      Active         : Boolean := True;
      --  Whether installation is active or not (Install package's attribute)

      Prefix_Dir      : Param := Dup (Global_Prefix_Dir);
      Exec_Subdir     : Param := Dup (Global_Exec_Subdir);
      Lib_Subdir      : Param := Dup (Global_Lib_Subdir);
      Link_Lib_Subdir : Param := Dup (Global_Link_Lib_Subdir);
      Sources_Subdir  : Param := Dup (Global_Sources_Subdir);
      Project_Subdir  : Param := Dup (Global_Project_Subdir);

      type Items is (Source, Object, Dependency, Library, Executable);

      Copy : array (Items) of Boolean := (others => False);
      --  What should be copied from a project, this depends on the actual
      --  project kind and the mode (usage, dev) set for the install.

      Man : Text_IO.File_Type;
      --  File where manifest for this project is kept

      procedure Copy_File
        (From, To, File : String;
         Sym_Link       : Boolean := False;
         Executable     : Boolean := False);
      --  Copy file From into To, if Sym_Link is set a symbolic link is
      --  created. If Executable is set, the destination file exec attribute
      --  is set.

      function Dir_Name (Suffix : Boolean := True) return String;
      --  Returns the name of directory where project files are to be
      --  installed. This name is the name of the project. If Suffix is
      --  True then the build name is also returned.

      function Cat
        (Dir : Path_Name_Type; File : File_Name_Type) return String;
      pragma Inline (Cat);
      --  Returns the string which is the catenation of Dir and File

      function Sources_Dir (Build_Name : Boolean := True) return String;
      --  Returns the full pathname to the sources destination directory

      function Exec_Dir return String;
      --  Returns the full pathname to the executable destination directory

      function Lib_Dir (Build_Name : Boolean := True) return String;
      --  Returns the full pathname to the library destination directory

      function Link_Lib_Dir return String;
      --  Returns the full pathname to the lib symlib directory

      function Project_Dir return String;
      --  Returns the full pathname to the project destination directory

      procedure Check_Install_Package;
      --  Check Project's install package and overwrite the default values of
      --  the corresponding variables above.

      procedure Copy_Files;
      --  Do the file copues for the project's sources, object, library,
      --  executable.

      procedure Create_Project (Project : Project_Id);
      --  Create install project for the given project

      procedure Add_To_Manifest (Pathname : String);
      --  Add filename to manifest

      function Get_Library_Filename return File_Name_Type;
      --  Returns the actual file name for the library

      function Has_Sources (Project : Project_Id) return Boolean;
      pragma Inline (Has_Sources);
      --  Returns True if the project contains sources

      function Bring_Sources (Project : Project_Id) return Boolean;
      --  Returns True if Project gives visibility to some sources directly or
      --  indirectly via the with clauses.

      function Main_Binary (Source : Name_Id) return String;
      --  Give the source name found in the Main attribute, returns the actual
      --  binary as built by gprbuild. This routine looks into the Builder
      --  switches for a the Executable attribute.

      ---------------------
      -- Add_To_Manifest --
      ---------------------

      procedure Add_To_Manifest (Pathname : String) is
         Prefix_Len : constant Natural := Prefix_Dir.V'Length;
      begin
         if not Is_Open (Man) then
            --  Create manifest file

            declare
               Dir  : constant String := Project_Dir & "manifests";
               Name : constant String :=
                        Dir & DS
                        & Base_Name (Get_Name_String (Project.Path.Name));
            begin
               if not Exists (Dir) then
                  Create_Path (Dir);
               end if;

               if Exists (Name) then
                  Open (Man, Append_File, Name);
               else
                  Create (Man, Out_File, Name);
               end if;
            end;
         end if;

         --  Append entry into manifest

         Put_Line
           (Man,
            File_MD5 (Pathname) & " "
            --  Remove the prefix, we want to store the pathname relative to
            --  the prefix of installation.
            & Pathname (Pathname'First + Prefix_Len .. Pathname'Last));
      end Add_To_Manifest;

      -------------------
      -- Bring_Sources --
      -------------------

      function Bring_Sources (Project : Project_Id) return Boolean is
      begin
         if Has_Sources (Project) then
            return True;

         else
            declare
               List : Project_List := Project.All_Imported_Projects;
            begin
               while List /= null loop
                  if Has_Sources (List.Project) then
                     return True;
                  end if;
                  List := List.Next;
               end loop;
            end;
         end if;

         return False;
      end Bring_Sources;

      ---------------------------
      -- Check_Install_Package --
      ---------------------------

      procedure Check_Install_Package is
         Pck : Package_Id := Project.Decl.Packages;

         procedure Replace (P : in out Param; Val : Name_Id);
         pragma Inline (Replace);
         --  Set Var with Value, free previous pointer

         -------------
         -- Replace --
         -------------

         procedure Replace (P : in out Param; Val : Name_Id) is
            V : constant String := Ensure_Directory (Get_Name_String (Val));
         begin
            if V /= "" then
               Free (P.V);
               P := (new String'(V), Default => False);
            end if;
         end Replace;

      begin
         Look_Install_Package : while Pck /= No_Package loop
            if Pcks (Pck).Decl /= No_Declarations
              and then Pcks (Pck).Name = Name_Install
            then
               --  Found Install package, check attributes

               declare
                  Id : Variable_Id := Pcks (Pck).Decl.Attributes;
               begin
                  while Id /= No_Variable loop
                     declare
                        V : constant Variable :=
                              Tree.Shared.Variable_Elements.Table (Id);
                     begin
                        if V.Name = Name_Prefix
                          and then Global_Prefix_Dir.Default
                        then
                           Replace (Prefix_Dir, V.Value.Value);

                        elsif  V.Name = Name_Exec_Subdir
                          and then Global_Exec_Subdir.Default
                        then
                           Replace (Exec_Subdir, V.Value.Value);

                        elsif V.Name = Name_Lib_Subdir
                          and then Global_Lib_Subdir.Default
                        then
                           Replace (Lib_Subdir, V.Value.Value);

                        elsif V.Name = Name_Link_Lib_Subdir
                          and then Global_Link_Lib_Subdir.Default
                        then
                           Replace (Link_Lib_Subdir, V.Value.Value);

                        elsif V.Name = Name_Sources_Subdir
                          and then Global_Sources_Subdir.Default
                        then
                           Replace (Sources_Subdir, V.Value.Value);

                        elsif V.Name = Name_Project_Subdir
                          and then Global_Project_Subdir.Default
                        then
                           Replace (Project_Subdir, V.Value.Value);

                        elsif V.Name = Name_Active then
                           declare
                              Val : constant String :=
                                      To_Lower
                                        (Get_Name_String (V.Value.Value));
                           begin
                              if Val = "false" then
                                 Active := False;
                              else
                                 Active := True;
                              end if;
                           end;
                        end if;
                     end;
                     Id := Tree.Shared.Variable_Elements.Table (Id).Next;
                  end loop;
               end;

               exit Look_Install_Package;
            end if;

            Pck := Pcks (Pck).Next;
         end loop Look_Install_Package;
      end Check_Install_Package;

      --------------
      -- Dir_Name --
      --------------

      function Dir_Name (Suffix : Boolean := True) return String is

         function Get_Suffix return String;
         --  Returns a suffix if needed

         ----------------
         -- Get_Suffix --
         ----------------

         function Get_Suffix return String is
         begin
            --  .default is always ommitted from the directory name

            if Suffix and then Build_Name.all /= "default" then
               return '.' & Build_Name.all;
            else
               return "";
            end if;
         end Get_Suffix;

      begin
         return Get_Name_String (Project.Name) & Get_Suffix;
      end Dir_Name;

      ---------------------------
      -- Get_Library_Filenaame --
      ---------------------------

      function Get_Library_Filename return File_Name_Type is
      begin
         --  Library prefix

         Name_Len := 0;

         if Project.Library_Kind /= Static
           and then Project.Config.Shared_Lib_Prefix /= No_File
         then
            Add_Str_To_Name_Buffer
              (Get_Name_String (Project.Config.Shared_Lib_Prefix));
         else
            Add_Str_To_Name_Buffer ("lib");
         end if;

         --  Library name

         Add_Str_To_Name_Buffer (Get_Name_String (Project.Library_Name));

         --  Library suffix

         if Project.Library_Kind = Static
           and then Project.Config.Archive_Suffix /= No_File
         then
            Add_Str_To_Name_Buffer
              (Get_Name_String (Project.Config.Archive_Suffix));

         elsif Project.Library_Kind /= Static
           and then Project.Config.Shared_Lib_Suffix /= No_File
         then
            Add_Str_To_Name_Buffer
              (Get_Name_String (Project.Config.Shared_Lib_Suffix));

         else
            Add_Str_To_Name_Buffer (".so");
         end if;

         return Name_Find;
      end Get_Library_Filename;

      -----------------
      -- Main_Binary --
      -----------------

      function Main_Binary (Source : Name_Id) return String is

         function Get_Exec_Suffix return String;
         --  Return the target executable suffix

         ---------------------
         -- Get_Exec_Suffix --
         ---------------------

         function Get_Exec_Suffix return String is
         begin
            if Project.Config.Executable_Suffix = No_Name then
               return "";
            else
               return Get_Name_String (Project.Config.Executable_Suffix);
            end if;
         end Get_Exec_Suffix;

         Builder_Package  : constant Package_Id :=
                              Value_Of
                                (Name_Builder, Main_Project.Decl.Packages,
                                 Project_Tree.Shared);
         Value            : Variable_Value;
      begin
         if Builder_Package /= No_Package then
            Value := Value_Of
              (Name                    => Source,
               Attribute_Or_Array_Name => Name_Executable,
               In_Package              => Builder_Package,
               Shared                  => Project_Tree.Shared);

            if Value = Nil_Variable_Value then

               --  If not found and name has an extension, try without

               declare
                  Name : constant String := Get_Name_String (Source);
                  S    : Name_Id;
               begin
                  if Name /= Base_Name (Name) then
                     Name_Len := 0;
                     Add_Str_To_Name_Buffer (Base_Name (Name));
                     S := Name_Find;

                     Value := Value_Of
                       (Name                    => S,
                        Attribute_Or_Array_Name => Name_Executable,
                        In_Package              => Builder_Package,
                        Shared                  => Project_Tree.Shared);
                  end if;
               end;
            end if;
         end if;

         if Value = Nil_Variable_Value then
            return Base_Name (Get_Name_String (Source)) & Get_Exec_Suffix;
         else
            return Get_Name_String (Value.Value) & Get_Exec_Suffix;
         end if;
      end Main_Binary;

      -----------------
      -- Has_Sources --
      -----------------

      function Has_Sources (Project : Project_Id) return Boolean is
      begin
         return Project.Source_Dirs /= Nil_String
           or else Project.Qualifier = Aggregate_Library;
      end Has_Sources;

      --------------
      -- Exec_Dir --
      --------------

      function Exec_Dir return String is
      begin
         if Is_Absolute_Path (Exec_Subdir.V.all) then
            return Exec_Subdir.V.all;
         else
            return Prefix_Dir.V.all & Exec_Subdir.V.all;
         end if;
      end Exec_Dir;

      -------------
      -- Lib_Dir --
      -------------

      function Lib_Dir (Build_Name : Boolean := True) return String is
      begin
         if Is_Absolute_Path (Lib_Subdir.V.all) then
            return Lib_Subdir.V.all;

         elsif not Lib_Subdir.Default or else not Build_Name then
            return Prefix_Dir.V.all & Lib_Subdir.V.all;

         else
            return Ensure_Directory
              (Prefix_Dir.V.all & Lib_Subdir.V.all & Dir_Name);
         end if;
      end Lib_Dir;

      ------------------
      -- Link_Lib_Dir --
      ------------------

      function Link_Lib_Dir return String is
      begin
         if Is_Absolute_Path (Link_Lib_Subdir.V.all) then
            return Link_Lib_Subdir.V.all;
         else
            return Prefix_Dir.V.all & Link_Lib_Subdir.V.all;
         end if;
      end Link_Lib_Dir;

      -----------------
      -- Sources_Dir --
      -----------------

      function Sources_Dir (Build_Name : Boolean := True) return String is
      begin
         if Is_Absolute_Path (Sources_Subdir.V.all) then
            return Sources_Subdir.V.all;

         elsif not Sources_Subdir.Default or else not Build_Name then
            return Prefix_Dir.V.all & Sources_Subdir.V.all;

         else
            return Ensure_Directory
              (Prefix_Dir.V.all & Sources_Subdir.V.all & Dir_Name);
         end if;
      end Sources_Dir;

      -----------------
      -- Project_Dir --
      -----------------

      function Project_Dir return String is
      begin
         if Is_Absolute_Path (Project_Subdir.V.all) then
            return Project_Subdir.V.all;
         else
            return Prefix_Dir.V.all & Project_Subdir.V.all;
         end if;
      end Project_Dir;

      ---------
      -- Cat --
      ---------

      function Cat
        (Dir : Path_Name_Type; File : File_Name_Type) return String is
      begin
         return Get_Name_String (Dir) & Get_Name_String (File);
      end Cat;

      ---------------
      -- Copy_File --
      ---------------

      procedure Copy_File
        (From, To, File : String;
         Sym_Link       : Boolean := False;
         Executable     : Boolean := False)
      is
         Dest_Filename : constant String := To & File;
      begin
         if not Sym_Link
           and then Exists (Dest_Filename)
           and then not Force_Installations
         then
            Write_Str ("file ");
            Write_Str (File);
            Write_Str (" exists, use -f to overwrite");
            Write_Eol;
            OS_Exit (1);
         end if;

         if Dry_Run or else Opt.Verbose_Mode then
            if Sym_Link then
               Write_Str ("ln -s ");
            else
               Write_Str ("cp ");
            end if;

            Write_Str (From);
            Write_Str (" ");
            Write_Str (Dest_Filename);
            Write_Eol;
         end if;

         if not Dry_Run then
            if not Sym_Link and then not Exists (From) then
               Write_Str ("file ");
               Write_Str (From);
               Write_Str (" does not exist, build may not be complete");
               Write_Eol;
               OS_Exit (1);
            end if;

            if (not Sym_Link and then not Exists (To))
              or else (Sym_Link and then not Exists (From))
            then
               if Create_Dest_Dir then
                  if Sym_Link then
                     Create_Path (Containing_Directory (From));
                  else
                     Create_Path (To);
                  end if;

               else
                  Set_Standard_Error;
                  Write_Line ("directory does not exist, use -p to create");
                  OS_Exit (1);
               end if;
            end if;

            --  Do copy

            if Sym_Link then
               Create_Sym_Link (From, To & File);

               --  Add file to manifest

               Add_To_Manifest (From);

            else
               begin
                  Directories.Copy_File
                    (Source_Name => From,
                     Target_Name => Dest_Filename,
                     Form        => "preserve=timestamps");
               exception
                  when Text_IO.Use_Error =>
                     Write_Line
                       ("cannot overwrite file " & Dest_Filename
                        & " check permissions.");
                     OS_Exit (1);
               end;

               if Executable then
                  Set_Executable (Dest_Filename);
               end if;

               --  Add file to manifest

               Add_To_Manifest (Dest_Filename);
            end if;
         end if;
      end Copy_File;

      ----------------
      -- Copy_Files --
      ----------------

      procedure Copy_Files is

         procedure Copy_Project_Sources
           (Project : Project_Id; Tree : Project_Tree_Ref);
         --  Copy sources from the given project

         procedure Copy_Source (Sid : Source_Id);

         --------------------------
         -- Copy_Project_Sources --
         --------------------------

         procedure Copy_Project_Sources
           (Project : Project_Id; Tree : Project_Tree_Ref)
         is
            function Is_Ada (Sid : Source_Id) return Boolean;
            pragma Inline (Is_Ada);
            --  Returns True if Sid is an Ada source

            ------------
            -- Is_Ada --
            ------------

            function Is_Ada (Sid : Source_Id) return Boolean is
            begin
               return Sid.Language /= null
                 and then Get_Name_String (Sid.Language.Name) = "ada";
            end Is_Ada;

            Iter : Source_Iterator;
            Sid  : Source_Id;

         begin
            if Project.Qualifier = Aggregate_Library then
               Iter := For_Each_Source (Tree);
            else
               Iter := For_Each_Source (Tree, Project);
            end if;

            loop
               Sid := Element (Iter);
               exit when Sid = No_Source;

               --  Skip sources that are removed/excluded and sources not
               --  part of the interface for standalone libraries.

               if not Sid.Locally_Removed
                 and then not Sid.Project.Externally_Built
                 and then (Project.Standalone_Library = No
                           or else Sid.Declared_In_Interfaces)
               then
                  --  If the unit has a naming exception we install it
                  --  regardless of the fact that it is part of the interface
                  --  or not. This is because the installed project will have
                  --  a Namig package referencing this file. The .ali is looked
                  --  based on the name of the renamed body.

                  if All_Sources or else Sid.Naming_Exception = Yes then
                     Copy_Source (Sid);
                  end if;

                  --  Objects / Deps

                  if Other_Part (Sid) = null or else Sid.Kind /= Spec then
                     if Copy (Object) then
                        Copy_File
                          (From => Cat
                             (Get_Object_Directory
                                (Sid.Project, False), Sid.Object),
                           To   => Lib_Dir,
                           File => Get_Name_String (Sid.Object));
                     end if;

                     --  Only install Ada .ali files

                     if Copy (Dependency)
                       and then Sid.Kind /= Sep
                       and then Is_Ada (Sid)
                     then
                        Copy_File
                          (From => Cat
                             (Get_Object_Directory (Sid.Project, False),
                              Sid.Dep_Name),
                           To   => Lib_Dir,
                           File => Get_Name_String (Sid.Dep_Name));
                     end if;
                  end if;
               end if;

               Next (Iter);
            end loop;
         end Copy_Project_Sources;

         -----------------
         -- Copy_Source --
         -----------------

         procedure Copy_Source (Sid : Source_Id) is
         begin
            if Copy (Source) then
               Copy_File
                 (From => Get_Name_String (Sid.Path.Name),
                  To   => Sources_Dir,
                  File => Get_Name_String (Sid.File));
            end if;
         end Copy_Source;

         procedure Copy_Interfaces is new For_Interface_Sources (Copy_Source);

      begin
         if not All_Sources then
            Copy_Interfaces (Tree, Project);
         end if;

         Copy_Project_Sources (Project, Tree);

         --  Copy library

         if Copy (Library) then
            if Project.Library_Kind /= Static
              and then Project.Lib_Internal_Name /= No_Name
              and then Project.Library_Name /= Project.Lib_Internal_Name
            then
               Copy_File
                 (From       => Cat
                    (Project.Library_Dir.Display_Name,
                     File_Name_Type (Project.Lib_Internal_Name)),
                  To         => Lib_Dir,
                  File       => Get_Name_String (Project.Lib_Internal_Name),
                  Executable => Project.Library_Kind /= Static);

               Copy_File
                 (From     => Lib_Dir
                                & Get_Name_String (Get_Library_Filename),
                  To       => Lib_Dir,
                  File     => Get_Name_String (Project.Lib_Internal_Name),
                  Sym_Link => True);

            else
               Copy_File
                 (From       => Cat
                    (Project.Library_Dir.Display_Name,
                     Get_Library_Filename),
                  To         => Lib_Dir,
                  File       => Get_Name_String (Get_Library_Filename),
                  Executable => Project.Library_Kind /= Static);
            end if;

            --  On Windows copy the shared libraries into the bin directory
            --  for it to be found in the PATH when running executable. On non
            --  Windows platforms add a symlink into the lib directory.

            if Project.Library_Kind /= Static and then Add_Lib_Link then
               if Makeutl.On_Windows then
                  Copy_File
                    (From       => Lib_Dir
                                     & Get_Name_String (Get_Library_Filename),
                     To         => Exec_Dir,
                     File       => Get_Name_String (Get_Library_Filename),
                     Executable => True);

               else
                  Copy_File
                    (From       => Link_Lib_Dir
                                     & Get_Name_String (Get_Library_Filename),
                     To         => Lib_Dir,
                     File       => Get_Name_String (Get_Library_Filename),
                     Sym_Link   => True);
               end if;
            end if;
         end if;

         --  Copy executable(s)

         if Copy (Executable) then
            declare
               M : String_List_Id := Project.Mains;
            begin
               while M /= Nil_String loop
                  declare
                     Bin : constant String := Main_Binary (Strs (M).Value);
                  begin
                     Copy_File
                       (From       =>
                          Get_Name_String
                            (Project.Exec_Directory.Display_Name) & Bin,
                        To         => Exec_Dir,
                        File       => Bin,
                        Executable => True);
                  end;

                  M := Strs (M).Next;
               end loop;
            end;
         end if;
      end Copy_Files;

      --------------------
      -- Create_Project --
      --------------------

      procedure Create_Project (Project : Project_Id) is

         use Ada.Strings.Unbounded;

         Filename : constant String :=
                      Project_Dir
                      & Base_Name (Get_Name_String (Project.Path.Display_Name))
                      & ".gpr";

         Gprinstall_Tag : constant String :=
                            "This project has been generated by GPRINSTALL";

         package String_Vector is
           new Containers.Indefinite_Vectors (Positive, String);

         package Seen_Set is new Containers.Indefinite_Ordered_Sets (String);

         Content : String_Vector.Vector;
         --  The content of the project, this is used when creating the project
         --  and is needed to ease the project section merging when installing
         --  multiple builds.

         Line    : Unbounded_String;

         function "+"
           (Item : String) return Unbounded_String renames To_Unbounded_String;
         function "-"
           (Item : Unbounded_String) return String renames To_String;

         procedure Create_Packages;
         --  Create packages that are needed, currently Naming and part of
         --  Linker is generated for the installed project.

         function Image
           (Name : Name_Id;
            Id   : Array_Element_Id) return String;
         --  Returns Id image

         function Image (Id : Variable_Id) return String;
         --  Returns Id image

         function Image (Var : Variable_Value) return String;
         --  Returns Id image

         procedure Read_Project;
         --  Read project and set Content accordingly

         procedure Write_Project;
         --  Write content into project

         procedure Add_Empty_Line;
         pragma Inline (Add_Empty_Line);

         function Naming_Case_Alternative
           (Proj : Project_Id) return String_Vector.Vector;
         --  Returns the naming case alternative for this project configuration

         function Linker_Case_Alternative
           (Proj : Project_Id) return String_Vector.Vector;
         --  Returns the linker case alternative for this project configuration

         function Data_Attributes return String_Vector.Vector;
         --  Returns the attributes for the sources, objects and library

         function Get_Languages return String;
         --  Returns the list of languages

         function Get_Package
           (Project : Project_Id; Pkg_Name : Name_Id) return Package_Id;
         --  Returns the package Name for the given project

         --------------------
         -- Add_Empty_Line --
         --------------------

         procedure Add_Empty_Line is
         begin
            if Content.Element (Content.Last_Index) /= "" then
               Content.Append ("");
            end if;
         end Add_Empty_Line;

         ---------------------
         -- Create_Packages --
         ---------------------

         procedure Create_Packages is

            procedure Create_Naming (Proj : Project_Id);
            --  Create the naming package

            procedure Create_Linker (Proj : Project_Id);
            --  Create the linker package if needed

            -------------------
            -- Create_Naming --
            -------------------

            procedure Create_Naming (Proj : Project_Id) is
            begin
               Content.Append ("   package Naming is");

               --  Attributes

               declare
                  V : Variable_Id := Pcks
                        (Get_Package (Proj, Name_Naming)).Decl.Attributes;
               begin
                  while V /= No_Variable loop
                     Content.Append ("      " & Image (V));
                     V := Tree.Shared.Variable_Elements.Table (V).Next;
                  end loop;
               end;

               Content.Append ("      case BUILD is");

               Content.Append (Naming_Case_Alternative (Proj));

               Content.Append ("      end case;");
               Content.Append ("   end Naming;");
               Add_Empty_Line;
            end Create_Naming;

            -------------------
            -- Create_Linker --
            -------------------

            procedure Create_Linker (Proj : Project_Id) is
            begin
               Content.Append ("   package Linker is");

               Content.Append ("      case BUILD is");
               --  Attribute Linker_Options only if set

               Content.Append (Linker_Case_Alternative (Proj));

               Content.Append ("      end case;");
               Content.Append ("   end Linker;");
               Add_Empty_Line;
            end Create_Linker;

         begin
            Create_Naming (Project);
            Create_Linker (Project);
         end Create_Packages;

         ---------------------
         -- Data_Attributes --
         ---------------------

         function Data_Attributes return String_Vector.Vector is

            procedure Gen_Dir_Name
              (P : Param; Line : in out Unbounded_String);
            --  Generate dir name

            ------------------
            -- Gen_Dir_Name --
            ------------------

            procedure Gen_Dir_Name
              (P : Param; Line : in out Unbounded_String) is
            begin
               if P.Default then
                  --  This is the default value, add Dir_Name
                  Line := Line & Dir_Name (Suffix => False);

                  --  Furthermore, if the build name is "default" do not output

                  if Build_Name.all /= "default" then
                     Line := Line & "." & Build_Name.all;
                  end if;
               end if;
            end Gen_Dir_Name;

            V    : String_Vector.Vector;
            Line : Unbounded_String;
         begin
            V.Append ("      when """ & Build_Name.all & """ =>");

            --  Project sources

            Line := +"         for Source_Dirs use (""";
            Line := Line
              & Relative_Path
              (Sources_Dir (Build_Name => False), To => Project_Dir);

            Gen_Dir_Name (Sources_Subdir, Line);
            Line := Line & """);";

            V.Append (-Line);

            --  Project objects and/or library

            if Project.Library then
               Line := +"         for Library_Dir use """;
            else
               Line := +"         for Object_Dir use """;
            end if;

            Line := Line
              & Relative_Path
                  (Lib_Dir (Build_Name => False), To => Project_Dir);

            Gen_Dir_Name (Lib_Subdir, Line);
            Line := Line & """;";

            V.Append (-Line);

            if Project.Library then
               Line := +"         for Library_Kind use """;
               Line := Line & To_Lower (Lib_Kind'Image (Project.Library_Kind));
               Line := Line & """;";
               V.Append (-Line);
            end if;

            return V;
         end Data_Attributes;

         -------------------
         -- Get_Languages --
         -------------------

         function Get_Languages return String is

            package Lang_Set is new Containers.Indefinite_Ordered_Sets
              (String,
               Strings.Less_Case_Insensitive, Strings.Equal_Case_Insensitive);

            Langs : Lang_Set.Set;

            procedure For_Project (Project : Project_Id);
            --  Add languages for the given project

            -----------------
            -- For_Project --
            -----------------

            procedure For_Project (Project : Project_Id) is
               L : Language_Ptr := Project.Languages;
            begin
               while L /= null loop
                  Langs.Include (Get_Name_String (L.Display_Name));
                  L := L.Next;
               end loop;
            end For_Project;

         begin
            --  First adds language for the main project

            For_Project (Project);

            --  If we are dealing with an aggregate library, adds the languages
            --  from all aggregated projects.

            if Project.Qualifier = Aggregate_Library then
               declare
                  Agg : Aggregated_Project_List := Project.Aggregated_Projects;
               begin
                  while Agg /= null loop
                     For_Project (Agg.Project);
                     Agg := Agg.Next;
                  end loop;
               end;
            end if;

            declare
               Res   : Unbounded_String;
               First : Boolean := True;
            begin
               for V of Langs loop
                  if not First then
                     Res := Res & ", ";
                  end if;

                  Res := Res & '"' & V & '"';

                  First := False;
               end loop;

               return To_String (Res);
            end;
         end Get_Languages;

         -----------------
         -- Get_Package --
         -----------------

         function Get_Package
           (Project : Project_Id; Pkg_Name : Name_Id) return Package_Id
         is
            Pck : Package_Id := Project.Decl.Packages;
         begin
            while Pck /= No_Package loop
               if Pcks (Pck).Decl /= No_Declarations
                 and then Pcks (Pck).Name = Pkg_Name
               then
                  return Pck;
               end if;

               Pck := Pcks (Pck).Next;
            end loop;
            return No_Package;
         end Get_Package;

         -----------
         -- Image --
         -----------

         function Image
           (Name : Name_Id;
            Id   : Array_Element_Id) return String
         is
            E : constant Array_Element :=
                  Tree.Shared.Array_Elements.Table (Id);
         begin
            return "for "
              & Get_Name_String (Name)
              & " ("""
              & Get_Name_String (E.Index)
              & """) use "
              & Image (E.Value);
         end Image;

         function Image (Id : Variable_Id) return String is
            V : constant Variable_Value :=
                  Tree.Shared.Variable_Elements.Table (Id).Value;
         begin
            if V.Default then
               return "";
            else
               return "for "
                 & Get_Name_String
                     (Tree.Shared.Variable_Elements.Table (Id).Name)
                 & " use "
                 & Image (V);
            end if;
         end Image;

         function Image (Var : Variable_Value) return String is
         begin
            case Var.Kind is
               when Single =>
                  return """" & Get_Name_String (Var.Value) & """;";

               when List =>
                  declare
                     V     : Unbounded_String;
                     L     : String_List_Id := Var.Values;
                     First : Boolean := True;
                  begin
                     Append (V, "(");

                     while L /= Nil_String loop
                        if not First then
                           Append (V, ", ");
                        else
                           First := False;
                        end if;
                        Append (V, """");
                        Append (V, Get_Name_String (Strs (L).Value));
                        Append (V, """");
                        L := Strs (L).Next;
                     end loop;
                     Append (V, ");");

                     return To_String (V);
                  end;

               when Undefined =>
                  return "";
            end case;
         end Image;

         -----------------------------
         -- Linker_Case_Alternative --
         -----------------------------

         function Linker_Case_Alternative
           (Proj : Project_Id) return String_Vector.Vector
         is
            use type Ada.Containers.Count_Type;

            procedure Linker_For (Pck : Package_Id);
            --  Handle the linker options for this package

            Seen : Seen_Set.Set;
            --  Records the attribute generated to avoid duplicate when
            --  handling aggregated projects.

            R    : String_Vector.Vector;

            ----------------
            -- Linker_For --
            ----------------

            procedure Linker_For (Pck : Package_Id) is
               V : Variable_Id := Pcks (Pck).Decl.Attributes;
            begin
               while V /= No_Variable loop
                  if Tree.Shared.Variable_Elements.Table (V).Name =
                    Name_Linker_Options
                  then
                     declare
                        Img : constant String := Image (V);
                     begin
                        if Img'Length /= 0
                          and then not Seen.Contains (Img)
                        then
                           R.Append ("            " & Img);
                           Seen.Include (Img);
                        end if;
                     end;
                  end if;
                  V := Tree.Shared.Variable_Elements.Table (V).Next;
               end loop;
            end Linker_For;

         begin
            R.Append ("         when """ & Build_Name.all & """ =>");

            Linker_For (Get_Package (Proj, Name_Linker));

            if Proj.Qualifier = Aggregate_Library then
               declare
                  Agg : Aggregated_Project_List :=
                          Project.Aggregated_Projects;
               begin
                  while Agg /= null loop
                     Linker_For (Get_Package (Agg.Project, Name_Linker));
                     Agg := Agg.Next;
                  end loop;
               end;
            end if;

            if R.Length = 1 then
               --  No linker alternative found, add null statement
               R.Append ("            null;");
            end if;

            return R;
         end Linker_Case_Alternative;

         -----------------------------
         -- Naming_Case_Alternative --
         -----------------------------

         function Naming_Case_Alternative
           (Proj : Project_Id) return String_Vector.Vector
         is
            procedure Naming_For (Pck : Package_Id);
            --  Handle the naming scheme for this package

            Seen : Seen_Set.Set;
            --  Records the attribute generated to avoid duplicate when
            --  handling aggregated projects.

            V    : String_Vector.Vector;

            ----------------
            -- Naming_For --
            ----------------

            procedure Naming_For (Pck : Package_Id) is
               A : Array_Id := Pcks (Pck).Decl.Arrays;
               N : Name_Id;
               E : Array_Element_Id;
            begin
               --  Arrays

               while A /= No_Array loop
                  N := Tree.Shared.Arrays.Table (A).Name;
                  E := Tree.Shared.Arrays.Table (A).Value;

                  while E /= No_Array_Element loop
                     declare
                        Decl : constant String := Image (N, E);
                     begin
                        if not Seen.Contains (Decl) then
                           V.Append ("            " & Image (N, E));
                           Seen.Include (Decl);
                        end if;
                     end;
                     E := Tree.Shared.Array_Elements.Table (E).Next;
                  end loop;

                  A := Tree.Shared.Arrays.Table (A).Next;
               end loop;
            end Naming_For;

         begin
            V.Append ("         when """ & Build_Name.all & """ =>");

            Naming_For (Get_Package (Proj, Name_Naming));

            if Proj.Qualifier = Aggregate_Library then
               declare
                  Agg : Aggregated_Project_List :=
                          Project.Aggregated_Projects;
               begin
                  while Agg /= null loop
                     Naming_For (Get_Package (Agg.Project, Name_Naming));
                     Agg := Agg.Next;
                  end loop;
               end;
            end if;

            return V;
         end Naming_Case_Alternative;

         ------------------
         -- Read_Project --
         ------------------

         procedure Read_Project is
            File   : File_Type;
            Buffer : String (1 .. 1_024);
            Last   : Natural;
         begin
            Open (File, In_File, Filename);

            while not End_Of_File (File) loop
               Get_Line (File, Buffer, Last);
               Content.Append (Buffer (1 .. Last));
            end loop;

            Close (File);
         end Read_Project;

         -------------------
         -- Write_Project --
         -------------------

         procedure Write_Project is
            F    : File_Access := Standard_Output;
            File : aliased File_Type;
         begin
            if not Dry_Run then
               Create (File, Out_File, Filename);
               F := File'Unchecked_Access;
            end if;

            for K in Content.First_Index .. Content.Last_Index loop
               Put_Line (F.all, Content.Element (K));
            end loop;

            if not Dry_Run then
               Close (File);
            end if;
         end Write_Project;

         type Section_Kind is (Top, Naming, Linker);

         Project_Exists  : constant Boolean := Exists (Filename);
         Current_Section : Section_Kind := Top;
         Pos             : String_Vector.Cursor;
         Generated       : Boolean := False;

      begin
         if Dry_Run or else Opt.Verbose_Mode then
            Write_Eol;
            Write_Str ("Project ");
            Write_Str (Filename);

            if Dry_Run then
               Write_Line (" would be installed");
            else
               Write_Line (" installed");
            end if;

            Write_Eol;
         end if;

         --  If project exists, read it and check the generated status

         if Project_Exists then
            Read_Project;

            --  First check that this project has been generated by gprbuild,
            --  if not exit with an error as we cannot modify a project created
            --  manually and we do not want to overwrite it.

            Pos := Content.First;

            Check_Generated_Status :
            while String_Vector.Has_Element (Pos) loop
               if Fixed.Index
                 (String_Vector.Element (Pos), Gprinstall_Tag) /= 0
               then
                  Generated := True;
                  exit Check_Generated_Status;
               end if;
               String_Vector.Next (Pos);
            end loop Check_Generated_Status;

            if not Generated and then not Force_Installations then
               Write_Str ("non gprinstall project file ");
               Write_Str (Filename);
               Write_Str (" exists, use -f to overwrite");
               Write_Eol;
               OS_Exit (1);
            end if;
         end if;

         if Project_Exists and then Generated then
            if not Has_Sources (Project) then
               --  Nothing else to do in this case
               return;
            end if;

            if Opt.Verbose_Mode then
               Write_Line ("project file exists, merging new build");
            end if;

            --  Do merging for new build, we need to add an entry into the
            --  BUILD_KIND type and a corresponding case entry in the naming
            --  and Linker package.

            Parse_Content :
            while String_Vector.Has_Element (Pos) loop
               declare
                  BN   : constant String := Build_Name.all;
                  Line : constant String := String_Vector.Element (Pos);
                  P, L : Natural;
               begin
                  if Fixed.Index (Line, "type BUILD_KIND is (") /= 0 then
                     --  This is the "type BUILD_KIND" line, add new build name

                     --  First check if the current build name already exists

                     if Fixed.Index (Line, """" & BN & """") = 0 then
                        --  Get end of line

                        P := Fixed.Index (Line, ");");

                        if P = 0 then
                           Write_Line ("cannot parse the BUILD_KIND line");
                           OS_Exit (1);

                        else
                           Content.Replace_Element
                             (Pos,
                              Line (Line'First .. P - 1)
                              & ", """ & BN & """);");
                        end if;
                     end if;

                  elsif Fixed.Index (Line, ":= external") /= 0
                    and then Build_Var /= null
                  then
                     --  Replace build-var with new one

                     P := Fixed.Index (Line, """");

                     if P = 0 then
                        Write_Line ("cannot parse the BUILD line");
                        OS_Exit (1);

                     else
                        L := P + 1;
                        while L <= Line'Last and then Line (L) /= '"' loop
                           L := L + 1;
                        end loop;

                        if Line (L) /= '"' then
                           Write_Line ("cannot parse the BUILD line");
                           OS_Exit (1);

                        else
                           Content.Replace_Element
                             (Pos,
                              Line (Line'First .. P)
                              & Build_Var.all & Line (L .. Line'Last));
                        end if;
                     end if;

                  elsif Fixed.Index (Line, "package Naming is") /= 0 then
                     Current_Section := Naming;

                  elsif Fixed.Index (Line, "package Linker is") /= 0 then
                     Current_Section := Linker;

                  elsif Fixed.Index (Line, "case BUILD is") /= 0 then

                     --  Add new case section for the new build name

                     case Current_Section is
                        when Naming =>
                           String_Vector.Next (Pos);
                           Content.Insert
                             (Pos, Naming_Case_Alternative (Project));

                        when Linker =>
                           String_Vector.Next (Pos);
                           Content.Insert
                             (Pos, Linker_Case_Alternative (Project));

                        when Top =>
                           --  For the Sources/Lib attributes
                           String_Vector.Next (Pos);
                           Content.Insert (Pos, Data_Attributes);
                     end case;

                  elsif Fixed.Index (Line, "when """ & BN & """ =>") /= 0 then
                     --  Found a when with the current build name, this is a
                     --  previous install overwritten by this one. Remove this
                     --  section. Note that this removes sections from all
                     --  packages Naming and Linker, and from project level
                     --  case alternative.

                     Count_And_Delete : declare

                        use type Containers.Count_Type;

                        function End_When (L : String) return Boolean;
                        --  Return True if L is the end of a when alternative

                        --------------
                        -- End_When --
                        --------------

                        function End_When (L : String) return Boolean is
                           P   : constant Natural :=
                                   Strings.Fixed.Index_Non_Blank (L);
                           Len : constant Natural := L'Length;
                        begin
                           return P > 0
                             and then
                               ((P + 4 <= Len
                                 and then L (P .. P + 4) = "when ")
                                or else
                                  (P + 8 <= Len
                                   and then L (P .. P + 8) = "end case;"));
                        end End_When;

                        N : Containers.Count_Type := 0;
                        P : String_Vector.Cursor := Pos;
                     begin
                        --  The number of line to delete are from Pos to the
                        --  first line starting with a "when".

                        loop
                           String_Vector.Next (P);
                           N := N + 1;

                           exit when End_When (String_Vector.Element (P));
                        end loop;

                        Content.Delete (Pos, N);
                     end Count_And_Delete;
                  end if;
               end;

               String_Vector.Next (Pos);
            end loop Parse_Content;

         else
            --  Project does not exists, or it exists, was not generated by
            --  gprinstall and -f used. In this case it will be overwriten by
            --  a generated project.

            Content.Clear;

            --  Tag project as generated by gprbuild

            Content.Append
              ("--  " & Gprinstall_Tag & ' ' & Gpr_Version_String);
            Add_Empty_Line;

            --  Handle with clauses, generate a with clauses only for project
            --  bringing some visibility to sources. No need for doing this for
            --  aggregate projects.

            if Project.Qualifier /= Aggregate_Library then
               declare
                  L : Project_List := Project.Imported_Projects;
               begin
                  while L /= null loop
                     if Bring_Sources (L.Project) then
                        Content.Append
                          ("with """
                          & Base_Name
                            (Get_Name_String (L.Project.Path.Display_Name))
                          & """;");
                     end if;

                     L := L.Next;
                  end loop;
               end;
            end if;

            --  Project name

            if Has_Sources (Project) then
               if Project.Library then
                  Line := +"library ";
               else
                  Line := +"standard ";
               end if;
            else
               Line := +"abstract ";
            end if;

            Line := Line & "project ";
            Line := Line & Get_Name_String (Project.Display_Name);
            Line := Line & " is";
            Content.Append (-Line);

            if Has_Sources (Project) then
               --  BUILD variable

               Content.Append
                 ("   type BUILD_KIND is (""" & Build_Name.all & """);");

               Line := +"   BUILD : BUILD_KIND := external(""";

               if Build_Var /= null then
                  Line := Line & Build_Var.all;
               else
                  Line := Line & To_Upper (Dir_Name (Suffix => False));
                  Line := Line & "_BUILD";
               end if;

               Line := Line & """, """ & Build_Name.all & """);";
               Content.Append (-Line);

               --  Add languages, for an aggregate library we want all unique
               --  languages from all aggregated libraries.

               Add_Empty_Line;

               Content.Append
                 ("   for Languages use (" & Get_Languages & ");");

               --  Build_Suffix used to avoid .default as suffix

               Add_Empty_Line;

               Content.Append ("   case BUILD is");
               Content.Append (Data_Attributes);
               Content.Append ("   end case;");

               Add_Empty_Line;

               --  Library Name

               if Project.Library then
                  Content.Append
                    ("   for Library_Name use """
                     & Get_Name_String (Project.Library_Name)
                     & """;");

                  --  Issue the Library_Version only if needed

                  if Project.Library_Kind /= Static
                    and then Project.Lib_Internal_Name /= No_Name
                    and then Project.Library_Name /= Project.Lib_Internal_Name
                  then
                     Content.Append
                       ("   for Library_Version use """
                        & Get_Name_String (Project.Lib_Internal_Name)
                        & """;");
                  end if;
               end if;

               Add_Empty_Line;

               --  Packages

               Create_Packages;

               --  Externally Built

               Content.Append ("   for Externally_Built use ""True"";");

            else
               --  This is an abstract project

               Content.Append ("   for Source_Dirs use ();");
            end if;

            --  Close project

            Content.Append
              ("end " & Get_Name_String (Project.Display_Name) & ";");
         end if;

         --  Write new project if needed

         Write_Project;

         if not Dry_Run then
            --  Add project file to manifest

            Add_To_Manifest (Filename);
         end if;
      end Create_Project;

      Install_Project : Boolean;
      --  Whether the project is to be installed

   begin
      --  If we have an aggregate project we just install separately all
      --  aggregated projects.

      if Project.Qualifier = Aggregate then
         declare
            L : Aggregated_Project_List := Project.Aggregated_Projects;
         begin
            while L /= null loop
               Process (L.Tree, L.Project);
               L := L.Next;
            end loop;
         end;

         --  Nothing more to do for an aggegate project

         return;
      end if;

      if not Installed.Contains (Project.Name) then
         Installed.Include (Project.Name);

         --  First look for the Install package and set up the local values
         --  accordingly.

         Check_Install_Package;

         --  Skip non active project and externally built ones

         Install_Project := Active
           and Bring_Sources (Project)
           and not Project.Externally_Built;

         if not Opt.Quiet_Output then
            if Install_Project then
               Write_Str ("Install");
            elsif Opt.Verbose_Mode then
               Write_Str ("Skip");
            end if;

            if Install_Project  or Opt.Verbose_Mode then
               Write_Str (" project ");
               Write_Str (Get_Name_String (Project.Display_Name));
            end if;

            if not Install_Project and Opt.Verbose_Mode then
               if not Active then
                  Write_Str (" (not active)");
               elsif Project.Externally_Built then
                  Write_Str (" (externally build)");
               elsif Project.Source_Dirs = Nil_String then
                  Write_Str (" (no sources)");
               end if;
            end if;

            if Install_Project  or Opt.Verbose_Mode then
               Write_Eol;
            end if;
         end if;

         --  If this is not an active project, just return now

         if not Install_Project then
            return;
         end if;

         --  What should be copied

         Copy :=
           (Source     => For_Dev,
            Object     => For_Dev
                            and then Project.Mains = Nil_String
                            and then Project.Qualifier /= Library
                            and then Project.Qualifier /= Aggregate_Library
                            and then not Project.Library,
            Dependency => For_Dev and then Project.Mains = Nil_String,
            Library    => Project.Library
                            and then
                              ((For_Dev and then Project.Library_Kind = Static)
                                or else Project.Library_Kind /= Static),
            Executable => Project.Mains /= Nil_String);

         --  Copy all files from the project

         if Has_Sources (Project) then
            Copy_Files;
         end if;

         --  A project file is only needed in developer mode

         if For_Dev then
            Create_Project (Project);
         end if;

         --  Close manifest file if needed

         if Is_Open (Man) then
            Close (Man);
         end if;

         --  Handle all projects recursivelly if needed

         if Recursive then
            declare
               L : Project_List := Project.Imported_Projects;
            begin
               while L /= null loop
                  Process (Tree, L.Project);

                  L := L.Next;
               end loop;
            end;
         end if;
      end if;

      Free (Prefix_Dir);
      Free (Sources_Subdir);
      Free (Lib_Subdir);
      Free (Exec_Subdir);
      Free (Project_Subdir);
   end Process;

end Gprinstall.Install;
