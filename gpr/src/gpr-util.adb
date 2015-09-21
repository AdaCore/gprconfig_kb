------------------------------------------------------------------------------
--                                                                          --
--                           GPR PROJECT MANAGER                            --
--                                                                          --
--          Copyright (C) 2001-2015, Free Software Foundation, Inc.         --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Command_Line;                       use Ada.Command_Line;
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Directories;
with Ada.Unchecked_Deallocation;
with Ada.Strings.Fixed;                      use Ada.Strings.Fixed;
with Ada.Strings.Maps;                       use Ada.Strings.Maps;

with GNAT.Case_Util; use GNAT.Case_Util;
with GNAT.HTable;
with GNAT.Regexp;    use GNAT.Regexp;
with GNAT.Table;

with GPR.ALI;    use GPR.ALI;
with GPR.Debug;
with GPR.Err;
with GPR.Names;  use GPR.Names;
with GPR.Output; use GPR.Output;
with GPR.Opt;
with GPR.Com;
with GPR.Sinput;
with GPR.Snames; use GPR.Snames;

package body GPR.Util is

   Program_Name : String_Access := null;

   package Source_Info_Table is new GNAT.Table
     (Table_Component_Type => Source_Info_Iterator,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 10,
      Table_Increment      => 100);

   package Source_Info_Project_HTable is new GNAT.HTable.Simple_HTable
     (Header_Num => GPR.Header_Num,
      Element    => Natural,
      No_Element => 0,
      Key        => Name_Id,
      Hash       => GPR.Hash,
      Equal      => "=");

   procedure Free is new Ada.Unchecked_Deallocation
     (Text_File_Data, Text_File);

   -----------
   -- Close --
   -----------

   procedure Close (File : in out Text_File) is
      Len : Integer;
      Status : Boolean;

   begin
      if File = null then
         GPR.Com.Fail ("Close attempted on an invalid Text_File");
      end if;

      if File.Out_File then
         if File.Buffer_Len > 0 then
            Len := Write (File.FD, File.Buffer'Address, File.Buffer_Len);

            if Len /= File.Buffer_Len then
               GPR.Com.Fail ("Unable to write to an out Text_File");
            end if;
         end if;

         Close (File.FD, Status);

         if not Status then
            GPR.Com.Fail ("Unable to close an out Text_File");
         end if;

      else

         --  Close in file, no need to test status, since this is a file that
         --  we read, and the file was read successfully before we closed it.

         Close (File.FD);
      end if;

      Free (File);
   end Close;

   ------------
   -- Create --
   ------------

   procedure Create (File : out Text_File; Name : String) is
      FD        : File_Descriptor;
      File_Name : String (1 .. Name'Length + 1);

   begin
      File_Name (1 .. Name'Length) := Name;
      File_Name (File_Name'Last) := ASCII.NUL;
      FD := Create_File (Name => File_Name'Address,
                         Fmode => System.OS_Lib.Text);

      if FD = Invalid_FD then
         File := null;

      else
         File := new Text_File_Data;
         File.FD := FD;
         File.Out_File := True;
         File.End_Of_File_Reached := True;
      end if;
   end Create;

   -----------------
   -- Create_Name --
   -----------------

   function Create_Name (Name : String) return File_Name_Type is
   begin
      Name_Len := 0;
      Add_Str_To_Name_Buffer (Name);
      return Name_Find;
   end Create_Name;

   function Create_Name (Name : String) return Name_Id is
   begin
      Name_Len := 0;
      Add_Str_To_Name_Buffer (Name);
      return Name_Find;
   end Create_Name;

   function Create_Name (Name : String) return Path_Name_Type is
   begin
      Name_Len := 0;
      Add_Str_To_Name_Buffer (Name);
      return Name_Find;
   end Create_Name;

   ---------------
   -- Duplicate --
   ---------------

   procedure Duplicate
     (This   : in out Name_List_Index;
      Shared : Shared_Project_Tree_Data_Access)
   is
      Old_Current : Name_List_Index;
      New_Current : Name_List_Index;

   begin
      if This /= No_Name_List then
         Old_Current := This;
         Name_List_Table.Increment_Last (Shared.Name_Lists);
         New_Current := Name_List_Table.Last (Shared.Name_Lists);
         This := New_Current;
         Shared.Name_Lists.Table (New_Current) :=
           (Shared.Name_Lists.Table (Old_Current).Name, No_Name_List);

         loop
            Old_Current := Shared.Name_Lists.Table (Old_Current).Next;
            exit when Old_Current = No_Name_List;
            Shared.Name_Lists.Table (New_Current).Next := New_Current + 1;
            Name_List_Table.Increment_Last (Shared.Name_Lists);
            New_Current := New_Current + 1;
            Shared.Name_Lists.Table (New_Current) :=
              (Shared.Name_Lists.Table (Old_Current).Name, No_Name_List);
         end loop;
      end if;
   end Duplicate;

   -----------------
   -- End_Of_File --
   -----------------

   function End_Of_File (File : Text_File) return Boolean is
   begin
      if File = null then
         GPR.Com.Fail ("End_Of_File attempted on an invalid Text_File");
      end if;

      return File.End_Of_File_Reached;
   end End_Of_File;

   -------------------
   -- Executable_Of --
   -------------------

   function Executable_Of
     (Project  : Project_Id;
      Shared   : Shared_Project_Tree_Data_Access;
      Main     : File_Name_Type;
      Index    : Int;
      Language : String := "";
      Include_Suffix : Boolean := True) return File_Name_Type
   is
      pragma Assert (Project /= No_Project);

      The_Packages : constant Package_Id := Project.Decl.Packages;

      Builder_Package : constant GPR.Package_Id :=
                          GPR.Util.Value_Of
                            (Name        => Name_Builder,
                             In_Packages => The_Packages,
                             Shared      => Shared);

      Executable : Variable_Value :=
                     GPR.Util.Value_Of
                       (Name                    => Name_Id (Main),
                        Index                   => Index,
                        Attribute_Or_Array_Name => Name_Executable,
                        In_Package              => Builder_Package,
                        Shared                  => Shared);

      Lang   : Language_Ptr;

      Spec_Suffix : Name_Id := No_Name;
      Body_Suffix : Name_Id := No_Name;

      Spec_Suffix_Length : Natural := 0;
      Body_Suffix_Length : Natural := 0;

      procedure Get_Suffixes
        (B_Suffix : File_Name_Type;
         S_Suffix : File_Name_Type);
      --  Get the non empty suffixes in variables Spec_Suffix and Body_Suffix

      function Add_Suffix (File : File_Name_Type) return File_Name_Type;
      --  Return the name of the executable, based on File, and adding the
      --  executable suffix if needed

      ------------------
      -- Get_Suffixes --
      ------------------

      procedure Get_Suffixes
        (B_Suffix : File_Name_Type;
         S_Suffix : File_Name_Type)
      is
      begin
         if B_Suffix /= No_File then
            Body_Suffix := Name_Id (B_Suffix);
            Body_Suffix_Length := Natural (Length_Of_Name (Body_Suffix));
         end if;

         if S_Suffix /= No_File then
            Spec_Suffix := Name_Id (S_Suffix);
            Spec_Suffix_Length := Natural (Length_Of_Name (Spec_Suffix));
         end if;
      end Get_Suffixes;

      ----------------
      -- Add_Suffix --
      ----------------

      function Add_Suffix (File : File_Name_Type) return File_Name_Type is
         Saved_EEOT : constant Name_Id := Executable_Extension_On_Target;
         Result     : File_Name_Type;
         Suffix_From_Project : Variable_Value;
      begin
         if Include_Suffix then
            if Project.Config.Executable_Suffix /= No_Name then
               Executable_Extension_On_Target :=
                 Project.Config.Executable_Suffix;
            end if;

            Result :=  Executable_Name (File);
            Executable_Extension_On_Target := Saved_EEOT;
            return Result;

         elsif Builder_Package /= No_Package then

            --  If the suffix is specified in the project itself, as opposed to
            --  the config file, it needs to be taken into account. However,
            --  when the project was processed, in both cases the suffix was
            --  stored in Project.Config, so get it from the project again.

            Suffix_From_Project :=
              GPR.Util.Value_Of
                (Variable_Name => Name_Executable_Suffix,
                 In_Variables  =>
                   Shared.Packages.Table (Builder_Package).Decl.Attributes,
                 Shared        => Shared);

            if Suffix_From_Project /= Nil_Variable_Value
              and then Suffix_From_Project.Value /= No_Name
            then
               Executable_Extension_On_Target := Suffix_From_Project.Value;
               Result :=  Executable_Name (File);
               Executable_Extension_On_Target := Saved_EEOT;
               return Result;
            end if;
         end if;

         return File;
      end Add_Suffix;

   --  Start of processing for Executable_Of

   begin
      if Language /= "" then
         Lang := Get_Language_From_Name (Project, Language);
      end if;

      if Lang /= null then
         Get_Suffixes
           (B_Suffix => Lang.Config.Naming_Data.Body_Suffix,
            S_Suffix => Lang.Config.Naming_Data.Spec_Suffix);
      end if;

      if Builder_Package /= No_Package then
         if Executable = Nil_Variable_Value then
            Get_Name_String (Main);

            --  Try as index the name minus the implementation suffix or minus
            --  the specification suffix.

            declare
               Name : constant String (1 .. Name_Len) :=
                        Name_Buffer (1 .. Name_Len);
               Last : Positive := Name_Len;

               Truncated : Boolean := False;

            begin
               if Body_Suffix /= No_Name
                 and then Last > Natural (Length_Of_Name (Body_Suffix))
                 and then Name (Last - Body_Suffix_Length + 1 .. Last) =
                            Get_Name_String (Body_Suffix)
               then
                  Truncated := True;
                  Last := Last - Body_Suffix_Length;
               end if;

               if Spec_Suffix /= No_Name
                 and then not Truncated
                 and then Last > Spec_Suffix_Length
                 and then Name (Last - Spec_Suffix_Length + 1 .. Last) =
                            Get_Name_String (Spec_Suffix)
               then
                  Truncated := True;
                  Last := Last - Spec_Suffix_Length;
               end if;

               if Truncated then
                  Name_Len := Last;
                  Name_Buffer (1 .. Name_Len) := Name (1 .. Last);
                  Executable :=
                    GPR.Util.Value_Of
                      (Name                    => Name_Find,
                       Index                   => 0,
                       Attribute_Or_Array_Name => Name_Executable,
                       In_Package              => Builder_Package,
                       Shared                  => Shared);
               end if;
            end;
         end if;

         --  If we have found an Executable attribute, return its value,
         --  possibly suffixed by the executable suffix.

         if Executable /= Nil_Variable_Value
           and then Executable.Value /= No_Name
           and then Length_Of_Name (Executable.Value) /= 0
         then
            return Add_Suffix (File_Name_Type (Executable.Value));
         end if;
      end if;

      Get_Name_String (Main);

      --  If there is a body suffix or a spec suffix, remove this suffix,
      --  otherwise remove any suffix ('.' followed by other characters), if
      --  there is one.

      if Body_Suffix /= No_Name
         and then Name_Len > Body_Suffix_Length
         and then Name_Buffer (Name_Len - Body_Suffix_Length + 1 .. Name_Len) =
                    Get_Name_String (Body_Suffix)
      then
         --  Found the body termination, remove it

         Name_Len := Name_Len - Body_Suffix_Length;

      elsif Spec_Suffix /= No_Name
            and then Name_Len > Spec_Suffix_Length
            and then
              Name_Buffer (Name_Len - Spec_Suffix_Length + 1 .. Name_Len) =
                Get_Name_String (Spec_Suffix)
      then
         --  Found the spec termination, remove it

         Name_Len := Name_Len - Spec_Suffix_Length;

      else
         --  Remove any suffix, if there is one

         Get_Name_String (Strip_Suffix (Main));
      end if;

      return Add_Suffix (Name_Find);
   end Executable_Of;

   ----------------------------
   -- Executable_Prefix_Path --
   ----------------------------

   function Executable_Prefix_Path return String is
      Exec_Name : constant String := Command_Name;

      function Get_Install_Dir (S : String) return String;
      --  S is the executable name preceded by the absolute or relative path,
      --  e.g. "c:\usr\bin\gcc.exe". Returns the absolute directory where "bin"
      --  lies (in the example "C:\usr"). If the executable is not in a "bin"
      --  directory, return "".

      ---------------------
      -- Get_Install_Dir --
      ---------------------

      function Get_Install_Dir (S : String) return String is
         Exec      : String  := S;
         Path_Last : Integer := 0;

      begin
         for J in reverse Exec'Range loop
            if Exec (J) = Directory_Separator then
               Path_Last := J - 1;
               exit;
            end if;
         end loop;

         if Path_Last >= Exec'First + 2 then
            To_Lower (Exec (Path_Last - 2 .. Path_Last));
         end if;

         if Path_Last < Exec'First + 2
           or else Exec (Path_Last - 2 .. Path_Last) /= "bin"
           or else (Path_Last - 3 >= Exec'First
                     and then Exec (Path_Last - 3) /= Directory_Separator)
         then
            return "";
         end if;

         return Normalize_Pathname
                  (Exec (Exec'First .. Path_Last - 4),
                   Resolve_Links => Opt.Follow_Links_For_Dirs)
           & Directory_Separator;
      end Get_Install_Dir;

   --  Beginning of Executable_Prefix_Path

   begin
      --  First determine if a path prefix was placed in front of the
      --  executable name.

      for J in reverse Exec_Name'Range loop
         if Exec_Name (J) = Directory_Separator then
            return Get_Install_Dir (Exec_Name);
         end if;
      end loop;

      --  If we get here, the user has typed the executable name with no
      --  directory prefix.

      declare
         Path : String_Access := Locate_Exec_On_Path (Exec_Name);
      begin
         if Path = null then
            return "";
         else
            declare
               Dir : constant String := Get_Install_Dir (Path.all);
            begin
               Free (Path);
               return Dir;
            end;
         end if;
      end;
   end Executable_Prefix_Path;

   ------------
   -- Expect --
   ------------

   procedure Expect (The_Token : Token_Type; Token_Image : String) is
   begin
      if Token /= The_Token then

         --  ??? Should pass user flags here instead

         Err.Error_Msg (Gprbuild_Flags, Token_Image & " expected", Token_Ptr);
      end if;
   end Expect;

   ------------------
   -- Fail_Program --
   ------------------

   procedure Fail_Program
     (Project_Tree   : Project_Tree_Ref;
      S              : String;
      Flush_Messages : Boolean := True)
   is
   begin
      if Flush_Messages and not Opt.No_Exit_Message then
         if Total_Errors_Detected /= 0 or else Warnings_Detected /= 0 then
            Err.Finalize;
         end if;
      end if;

      Finish_Program (Project_Tree, E_Fatal, S => S);
   end Fail_Program;

   --------------------
   -- Finish_Program --
   --------------------

   procedure Finish_Program
     (Project_Tree : Project_Tree_Ref;
      Exit_Code    : Exit_Code_Type := E_Success;
      S            : String := "")
   is
   begin
      if not Debug.Debug_Flag_N then
         Delete_Temp_Config_Files (Project_Tree);

         if Project_Tree /= null then
            Delete_All_Temp_Files (Project_Tree.Shared);
         end if;
      end if;

      if S'Length > 0 then
         if Exit_Code /= E_Success then
            if not Opt.No_Exit_Message then
               Set_Standard_Error;
               Write_Program_Name;
               Write_Line (S);
            end if;

            Exit_Program (E_Fatal);

         elsif not Opt.No_Exit_Message then
            Write_Str (S);
         end if;
      end if;

      Exit_Program (Exit_Code);
   end Finish_Program;

   ---------------------------
   -- For_Interface_Sources --
   ---------------------------

   procedure For_Interface_Sources
     (Tree    : Project_Tree_Ref;
      Project : Project_Id)
   is
      use Ada;
      use type Ada.Containers.Count_Type;

      package Dep_Names is new Containers.Indefinite_Ordered_Sets (String);

      function Load_ALI (Filename : String) return ALI_Id;
      --  Load an ALI file and return its id

      --------------
      -- Load_ALI --
      --------------

      function Load_ALI (Filename : String) return ALI_Id is
         Result   : ALI_Id := No_ALI_Id;
         Text     : Text_Buffer_Ptr;
         Lib_File : File_Name_Type;

      begin
         if Directories.Exists (Filename) then
            Name_Len := 0;
            Add_Str_To_Name_Buffer (Filename);
            Lib_File := Name_Find;
            Text := Osint.Read_Library_Info (Lib_File);
            Result :=
              ALI.Scan_ALI
                (Lib_File,
                 Text,
                 Ignore_ED  => False,
                 Err        => True,
                 Read_Lines => "UD");
            Free (Text);
         end if;

         return Result;
      end Load_ALI;

      --  Local declarations

      Iter : Source_Iterator;
      Sid  : Source_Id;
      ALI  : ALI_Id;

      First_Unit  : Unit_Id;
      Second_Unit : Unit_Id;
      Body_Needed : Boolean;
      Deps        : Dep_Names.Set;

   --  Start of processing for For_Interface_Sources

   begin
      if Project.Qualifier = Aggregate_Library then
         Iter := For_Each_Source (Tree);
      else
         Iter := For_Each_Source (Tree, Project);
      end if;

      --  First look at each spec, check if the body is needed

      loop
         Sid := Element (Iter);
         exit when Sid = No_Source;

         --  Skip sources that are removed/excluded and sources not part of
         --  the interface for standalone libraries.

         if Sid.Kind = Spec
           and then (not Sid.Project.Externally_Built
                      or else Sid.Project = Project)
           and then not Sid.Locally_Removed
           and then (Project.Standalone_Library = No
                      or else Sid.Declared_In_Interfaces)

           --  Handle case of non-compilable languages

           and then Sid.Dep_Name /= No_File
         then
            Action (Sid);

            --  Check ALI for dependencies on body and sep

            ALI :=
              Load_ALI
                (Get_Name_String (Get_Object_Directory (Sid.Project, True))
                 & Get_Name_String (Sid.Dep_Name));

            if ALI /= No_ALI_Id then
               First_Unit := ALIs.Table (ALI).First_Unit;
               Second_Unit := No_Unit_Id;
               Body_Needed := True;

               --  If there is both a spec and a body, check if both needed

               if Units.Table (First_Unit).Utype = Is_Body then
                  Second_Unit := ALIs.Table (ALI).Last_Unit;

                  --  If the body is not needed, then reset First_Unit

                  if not Units.Table (Second_Unit).Body_Needed_For_SAL then
                     Body_Needed := False;
                  end if;

               elsif Units.Table (First_Unit).Utype = Is_Spec_Only then
                  Body_Needed := False;
               end if;

               --  Handle all the separates, if any

               if Body_Needed then
                  if Other_Part (Sid) /= null then
                     Deps.Include (Get_Name_String (Other_Part (Sid).File));
                  end if;

                  for Dep in ALIs.Table (ALI).First_Sdep ..
                    ALIs.Table (ALI).Last_Sdep
                  loop
                     if Sdep.Table (Dep).Subunit_Name /= No_Name then
                        Deps.Include
                          (Get_Name_String (Sdep.Table (Dep).Sfile));
                     end if;
                  end loop;
               end if;
            end if;
         end if;

         Next (Iter);
      end loop;

      --  Now handle the bodies and separates if needed

      if Deps.Length /= 0 then
         if Project.Qualifier = Aggregate_Library then
            Iter := For_Each_Source (Tree);
         else
            Iter := For_Each_Source (Tree, Project);
         end if;

         loop
            Sid := Element (Iter);
            exit when Sid = No_Source;

            if Sid.Kind /= Spec
              and then Deps.Contains (Get_Name_String (Sid.File))
            then
               Action (Sid);
            end if;

            Next (Iter);
         end loop;
      end if;
   end For_Interface_Sources;

   --------------
   -- Get_Line --
   --------------

   procedure Get_Line
     (File : Text_File;
      Line : out String;
      Last : out Natural)
   is
      C : Character;

      procedure Advance;

      -------------
      -- Advance --
      -------------

      procedure Advance is
      begin
         if File.Cursor = File.Buffer_Len then
            File.Buffer_Len :=
              Read
               (FD => File.FD,
                A  => File.Buffer'Address,
                N  => File.Buffer'Length);

            if File.Buffer_Len = 0 then
               File.End_Of_File_Reached := True;
               return;
            else
               File.Cursor := 1;
            end if;

         else
            File.Cursor := File.Cursor + 1;
         end if;
      end Advance;

   --  Start of processing for Get_Line

   begin
      if File = null then
         GPR.Com.Fail ("Get_Line attempted on an invalid Text_File");

      elsif File.Out_File then
         GPR.Com.Fail ("Get_Line attempted on an out file");
      end if;

      Last := Line'First - 1;

      if not File.End_Of_File_Reached then
         loop
            C := File.Buffer (File.Cursor);
            exit when C = ASCII.CR or else C = ASCII.LF;
            Last := Last + 1;
            Line (Last) := C;
            Advance;

            if File.End_Of_File_Reached then
               return;
            end if;

            exit when Last = Line'Last;
         end loop;

         if C = ASCII.CR or else C = ASCII.LF then
            Advance;

            if File.End_Of_File_Reached then
               return;
            end if;
         end if;

         if C = ASCII.CR
           and then File.Buffer (File.Cursor) = ASCII.LF
         then
            Advance;
         end if;
      end if;
   end Get_Line;

   ------------------
   -- Get_Switches --
   ------------------

   procedure Get_Switches
     (Source       : GPR.Source_Id;
      Pkg_Name     : Name_Id;
      Project_Tree : Project_Tree_Ref;
      Value        : out Variable_Value;
      Is_Default   : out Boolean)
   is
   begin
      Get_Switches
        (Source_File  => Source.File,
         Source_Lang  => Source.Language.Name,
         Source_Prj   => Source.Project,
         Pkg_Name     => Pkg_Name,
         Project_Tree => Project_Tree,
         Value        => Value,
         Is_Default   => Is_Default);
   end Get_Switches;

   procedure Get_Switches
     (Source_File         : File_Name_Type;
      Source_Lang         : Name_Id;
      Source_Prj          : Project_Id;
      Pkg_Name            : Name_Id;
      Project_Tree        : Project_Tree_Ref;
      Value               : out Variable_Value;
      Is_Default          : out Boolean;
      Test_Without_Suffix : Boolean := False;
      Check_ALI_Suffix    : Boolean := False)
   is
      Project : constant Project_Id :=
                  Ultimate_Extending_Project_Of (Source_Prj);
      Pkg     : constant Package_Id :=
                  GPR.Util.Value_Of
                    (Name        => Pkg_Name,
                     In_Packages => Project.Decl.Packages,
                     Shared      => Project_Tree.Shared);
      Lang : Language_Ptr;

   begin
      Is_Default := False;

      if Source_File /= No_File then
         Value := GPR.Util.Value_Of
           (Name                    => Name_Id (Source_File),
            Attribute_Or_Array_Name => Name_Switches,
            In_Package              => Pkg,
            Shared                  => Project_Tree.Shared,
            Allow_Wildcards         => True);
      end if;

      if Value = Nil_Variable_Value and then Test_Without_Suffix then
         Lang :=
           Get_Language_From_Name (Project, Get_Name_String (Source_Lang));

         if Lang /= null then
            declare
               Naming      : Lang_Naming_Data renames Lang.Config.Naming_Data;
               SF_Name     : constant String := Get_Name_String (Source_File);
               Last        : Positive := SF_Name'Length;
               Name        : String (1 .. Last + 3);
               Spec_Suffix : String   := Get_Name_String (Naming.Spec_Suffix);
               Body_Suffix : String   := Get_Name_String (Naming.Body_Suffix);
               Truncated   : Boolean  := False;

            begin
               Canonical_Case_File_Name (Spec_Suffix);
               Canonical_Case_File_Name (Body_Suffix);
               Name (1 .. Last) := SF_Name;

               if Last > Body_Suffix'Length
                 and then
                   Name (Last - Body_Suffix'Length + 1 .. Last) = Body_Suffix
               then
                  Truncated := True;
                  Last := Last - Body_Suffix'Length;
               end if;

               if not Truncated
                 and then Last > Spec_Suffix'Length
                 and then
                   Name (Last - Spec_Suffix'Length + 1 .. Last) = Spec_Suffix
               then
                  Truncated := True;
                  Last := Last - Spec_Suffix'Length;
               end if;

               if Truncated then
                  Name_Len := 0;
                  Add_Str_To_Name_Buffer (Name (1 .. Last));

                  Value := GPR.Util.Value_Of
                    (Name                    => Name_Find,
                     Attribute_Or_Array_Name => Name_Switches,
                     In_Package              => Pkg,
                     Shared                  => Project_Tree.Shared,
                     Allow_Wildcards         => True);
               end if;

               if Value = Nil_Variable_Value and then Check_ALI_Suffix then
                  Last := SF_Name'Length;
                  while Name (Last) /= '.' loop
                     Last := Last - 1;
                  end loop;

                  Name_Len := 0;
                  Add_Str_To_Name_Buffer (Name (1 .. Last));
                  Add_Str_To_Name_Buffer ("ali");

                  Value := GPR.Util.Value_Of
                    (Name                    => Name_Find,
                     Attribute_Or_Array_Name => Name_Switches,
                     In_Package              => Pkg,
                     Shared                  => Project_Tree.Shared,
                     Allow_Wildcards         => True);
               end if;
            end;
         end if;
      end if;

      if Value = Nil_Variable_Value then
         Is_Default := True;
         Value :=
           GPR.Util.Value_Of
             (Name                    => Source_Lang,
              Attribute_Or_Array_Name => Name_Switches,
              In_Package              => Pkg,
              Shared                  => Project_Tree.Shared,
              Force_Lower_Case_Index  => True);
      end if;

      if Value = Nil_Variable_Value then
         Value :=
           GPR.Util.Value_Of
             (Name                    => All_Other_Names,
              Attribute_Or_Array_Name => Name_Switches,
              In_Package              => Pkg,
              Shared                  => Project_Tree.Shared,
              Force_Lower_Case_Index  => True);
      end if;

      if Value = Nil_Variable_Value then
         Value :=
           GPR.Util.Value_Of
             (Name                    => Source_Lang,
              Attribute_Or_Array_Name => Name_Default_Switches,
              In_Package              => Pkg,
              Shared                  => Project_Tree.Shared);
      end if;
   end Get_Switches;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Iter        : out Source_Info_Iterator;
      For_Project : Name_Id)
   is
      Ind : constant Natural := Source_Info_Project_HTable.Get (For_Project);
   begin
      if Ind = 0 then
         Iter := (No_Source_Info, 0);
      else
         Iter := Source_Info_Table.Table (Ind);
      end if;
   end Initialize;

   ------------------------------
   -- Initialize_Source_Record --
   ------------------------------

   procedure Initialize_Source_Record
     (Source : GPR.Source_Id;
      Always : Boolean := False)
   is

      procedure Set_Object_Project
        (Obj_Dir  : String;
         Obj_Proj : Project_Id;
         Obj_Path : Path_Name_Type;
         Stamp    : Time_Stamp_Type);
      --  Update information about object file, switches file,...

      ------------------------
      -- Set_Object_Project --
      ------------------------

      procedure Set_Object_Project
        (Obj_Dir  : String;
         Obj_Proj : Project_Id;
         Obj_Path : Path_Name_Type;
         Stamp    : Time_Stamp_Type) is
      begin
         Source.Object_Project := Obj_Proj;
         Source.Object_Path    := Obj_Path;
         Source.Object_TS      := Stamp;

         if Source.Language.Config.Dependency_Kind /= None then
            declare
               Dep_Path : constant String :=
                            Normalize_Pathname
                              (Name          =>
                                 Get_Name_String (Source.Dep_Name),
                               Resolve_Links => Opt.Follow_Links_For_Files,
                               Directory     => Obj_Dir);
            begin
               Source.Dep_Path := Create_Name (Dep_Path);
               Source.Dep_TS   := Unknown_Attributes;
            end;
         end if;

         --  Get the path of the switches file, even if Opt.Check_Switches is
         --  not set, as switch -s may be in the Builder switches that have not
         --  been scanned yet.

         declare
            Switches_Path : constant String :=
                              Normalize_Pathname
                                (Name          =>
                                   Get_Name_String (Source.Switches),
                                 Resolve_Links => Opt.Follow_Links_For_Files,
                                 Directory     => Obj_Dir);
         begin
            Source.Switches_Path := Create_Name (Switches_Path);

            if Stamp /= Empty_Time_Stamp then
               Source.Switches_TS := File_Stamp (Source.Switches_Path);
            end if;
         end;
      end Set_Object_Project;

      Obj_Proj : Project_Id;

   begin
      --  Nothing to do if source record has already been fully initialized

      if Source.Initialized and not Always then
         return;
      end if;

      --  Systematically recompute the time stamp

      Source.Source_TS := File_Stamp (Source.Path.Display_Name);

      --  Parse the source file to check whether we have a subunit

      if Source.Language.Config.Kind = Unit_Based
        and then Source.Kind = Impl
        and then Is_Subunit (Source)
      then
         Source.Kind := Sep;
      end if;

      if Source.Language.Config.Object_Generated
        and then Is_Compilable (Source)
      then
         --  First, get the correct object file name and dependency file name
         --  if the source is in a multi-unit file.

         if Source.Index /= 0 then
            Source.Object :=
              Object_Name
                (Source_File_Name   => Source.File,
                 Source_Index       => Source.Index,
                 Index_Separator    =>
                   Source.Language.Config.Multi_Unit_Object_Separator,
                 Object_File_Suffix =>
                   Source.Language.Config.Object_File_Suffix);

            Source.Dep_Name :=
              Dependency_Name
                (Source.Object, Source.Language.Config.Dependency_Kind);
         end if;

         --  Find the object file for that source. It could be either in the
         --  current project or in an extended project (it might actually not
         --  exist yet in the ultimate extending project, but if not found
         --  elsewhere that's where we'll expect to find it).

         Obj_Proj := Source.Project;

         while Obj_Proj /= No_Project loop
            if Obj_Proj.Object_Directory /= No_Path_Information then
               declare
                  Dir : constant String :=
                    Get_Name_String (Obj_Proj.Object_Directory.Display_Name);

                  Object_Path : constant String :=
                    Normalize_Pathname
                      (Name          => Get_Name_String (Source.Object),
                       Resolve_Links => Opt.Follow_Links_For_Files,
                       Directory     => Dir);

                  Obj_Path : constant Path_Name_Type :=
                    Create_Name (Object_Path);

                  Stamp : Time_Stamp_Type := Empty_Time_Stamp;

               begin
                  --  For specs, we do not check object files if there is a
                  --  body. This saves a system call. On the other hand, we do
                  --  need to know the object_path, in case the user has passed
                  --  the .ads on the command line to compile the spec only.

                  if Source.Kind /= Spec
                    or else Source.Unit = No_Unit_Index
                    or else Source.Unit.File_Names (Impl) = No_Source
                  then
                     Stamp := File_Stamp (Obj_Path);
                  end if;

                  if Stamp /= Empty_Time_Stamp
                    or else (Obj_Proj.Extended_By = No_Project
                              and then Source.Object_Project = No_Project)
                  then
                     Set_Object_Project (Dir, Obj_Proj, Obj_Path, Stamp);
                  end if;
               end;
            end if;

            Obj_Proj := Obj_Proj.Extended_By;
         end loop;

      elsif Source.Language.Config.Dependency_Kind = Makefile then
         declare
            Object_Dir : constant String :=
              Get_Name_String (Source.Project.Object_Directory.Display_Name);
            Dep_Path   : constant String :=
              Normalize_Pathname
                (Name          => Get_Name_String (Source.Dep_Name),
                 Resolve_Links => Opt.Follow_Links_For_Files,
                 Directory     => Object_Dir);
         begin
            Source.Dep_Path := Create_Name (Dep_Path);
            Source.Dep_TS   := Unknown_Attributes;
         end;
      end if;

      Source.Initialized := True;
   end Initialize_Source_Record;

   function Is_Ada_Predefined_File_Name (Fname : File_Name_Type) return Boolean
   is
      subtype Str8 is String (1 .. 8);

      Predef_Names : constant array (1 .. 12) of Str8 :=
        ("ada     ",       -- Ada
         "interfac",       -- Interfaces
         "system  ",       -- System
         "gnat    ",       -- GNAT
         "calendar",       -- Calendar
         "machcode",       -- Machine_Code
         "unchconv",       -- Unchecked_Conversion
         "unchdeal",       -- Unchecked_Deallocation
         "directio",       -- Direct_IO
         "ioexcept",       -- IO_Exceptions
         "sequenio",       -- Sequential_IO
         "text_io ");      -- Text_IO
   begin
      Get_Name_String (Fname);

      --  Remove extension (if present)

      if Name_Len > 4 and then Name_Buffer (Name_Len - 3) = '.' then
         Name_Len := Name_Len - 4;
      end if;

      --  Definitely false if longer than 12 characters (8.3)

      if Name_Len > 8 then
         return False;

      --  Definitely predefined if prefix is a- i- or s- followed by letter

      elsif Name_Len >=  3
        and then Name_Buffer (2) = '-'
        and then (Name_Buffer (1) = 'a'
                    or else
                  Name_Buffer (1) = 'g'
                    or else
                  Name_Buffer (1) = 'i'
                    or else
                  Name_Buffer (1) = 's')
        and then (Name_Buffer (3) in 'a' .. 'z'
                    or else
                  Name_Buffer (3) in 'A' .. 'Z')
      then
         return True;
      end if;

      --  Otherwise check against special list, first padding to 8 characters

      while Name_Len < 8 loop
         Name_Len := Name_Len + 1;
         Name_Buffer (Name_Len) := ' ';
      end loop;

      for J in Predef_Names'Range loop
         if Name_Buffer (1 .. 8) = Predef_Names (J) then
            return True;
         end if;
      end loop;

      return False;
   end Is_Ada_Predefined_File_Name;

   ----------------
   -- Is_Subunit --
   ----------------

   function Is_Subunit (Source : GPR.Source_Id) return Boolean is
      Src_Ind : Source_File_Index;

   begin
      if Source.Kind = Sep then
         return True;

      --  A Spec, a file based language source or a body with a spec cannot be
      --  a subunit.

      elsif Source.Kind = Spec
        or else Source.Unit = No_Unit_Index
        or else Other_Part (Source) /= No_Source
      then
         return False;
      end if;

      --  Here, we are assuming that the language is Ada, as it is the only
      --  unit based language that we know.

      Src_Ind :=
        Sinput.Load_File
          (Get_Name_String (Source.Path.Display_Name));

      return Sinput.Source_File_Is_Subunit (Src_Ind);
   end Is_Subunit;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (File : Text_File) return Boolean is
   begin
      return File /= null;
   end Is_Valid;

   ----------
   -- Next --
   ----------

   procedure Next (Iter : in out Source_Info_Iterator) is
   begin
      if Iter.Next = 0 then
         Iter.Info := No_Source_Info;

      else
         Iter := Source_Info_Table.Table (Iter.Next);
      end if;
   end Next;

   ----------
   -- Open --
   ----------

   procedure Open (File : out Text_File; Name : String) is
      FD        : File_Descriptor;
      File_Name : String (1 .. Name'Length + 1);

   begin
      File_Name (1 .. Name'Length) := Name;
      File_Name (File_Name'Last) := ASCII.NUL;
      FD := Open_Read (Name => File_Name'Address,
                       Fmode => System.OS_Lib.Text);

      if FD = Invalid_FD then
         File := null;

      else
         File := new Text_File_Data;
         File.FD := FD;
         File.Buffer_Len :=
           Read (FD => FD,
                 A  => File.Buffer'Address,
                 N  => File.Buffer'Length);

         if File.Buffer_Len = 0 then
            File.End_Of_File_Reached := True;
         else
            File.Cursor := 1;
         end if;
      end if;
   end Open;

   ---------
   -- Put --
   ---------

   procedure Put
     (Into_List  : in out Name_List_Index;
      From_List  : String_List_Id;
      In_Tree    : Project_Tree_Ref;
      Lower_Case : Boolean := False)
   is
      Shared  : constant Shared_Project_Tree_Data_Access := In_Tree.Shared;

      Current_Name : Name_List_Index;
      List         : String_List_Id;
      Element      : String_Element;
      Last         : Name_List_Index :=
                       Name_List_Table.Last (Shared.Name_Lists);
      Value        : Name_Id;

   begin
      Current_Name := Into_List;
      while Current_Name /= No_Name_List
        and then Shared.Name_Lists.Table (Current_Name).Next /= No_Name_List
      loop
         Current_Name := Shared.Name_Lists.Table (Current_Name).Next;
      end loop;

      List := From_List;
      while List /= Nil_String loop
         Element := Shared.String_Elements.Table (List);
         Value := Element.Value;

         if Lower_Case then
            Get_Name_String (Value);
            To_Lower (Name_Buffer (1 .. Name_Len));
            Value := Name_Find;
         end if;

         Name_List_Table.Append
           (Shared.Name_Lists, (Name => Value, Next => No_Name_List));

         Last := Last + 1;

         if Current_Name = No_Name_List then
            Into_List := Last;
         else
            Shared.Name_Lists.Table (Current_Name).Next := Last;
         end if;

         Current_Name := Last;

         List := Element.Next;
      end loop;
   end Put;

   procedure Put (File : Text_File; S : String) is
      Len : Integer;
   begin
      if File = null then
         GPR.Com.Fail ("Attempted to write on an invalid Text_File");

      elsif not File.Out_File then
         GPR.Com.Fail ("Attempted to write an in Text_File");
      end if;

      if File.Buffer_Len + S'Length > File.Buffer'Last then
         --  Write buffer
         Len := Write (File.FD, File.Buffer'Address, File.Buffer_Len);

         if Len /= File.Buffer_Len then
            GPR.Com.Fail ("Failed to write to an out Text_File");
         end if;

         File.Buffer_Len := 0;
      end if;

      File.Buffer (File.Buffer_Len + 1 .. File.Buffer_Len + S'Length) := S;
      File.Buffer_Len := File.Buffer_Len + S'Length;
   end Put;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (File : Text_File; Line : String) is
      L : String (1 .. Line'Length + 1);
   begin
      L (1 .. Line'Length) := Line;
      L (L'Last) := ASCII.LF;
      Put (File, L);
   end Put_Line;

   ---------------------------
   -- Read_Source_Info_File --
   ---------------------------

   procedure Read_Source_Info_File (Tree : Project_Tree_Ref) is
      File : Text_File;
      Info : Source_Info_Iterator;
      Proj : Name_Id;

      procedure Report_Error;

      ------------------
      -- Report_Error --
      ------------------

      procedure Report_Error is
      begin
         Write_Line ("errors in source info file """ &
                     Tree.Source_Info_File_Name.all & '"');
         Tree.Source_Info_File_Exists := False;
      end Report_Error;

   begin
      Source_Info_Project_HTable.Reset;
      Source_Info_Table.Init;

      if Tree.Source_Info_File_Name = null then
         Tree.Source_Info_File_Exists := False;
         return;
      end if;

      Open (File, Tree.Source_Info_File_Name.all);

      if not Is_Valid (File) then
         if Opt.Verbose_Mode then
            Write_Line ("source info file " & Tree.Source_Info_File_Name.all &
                        " does not exist");
         end if;

         Tree.Source_Info_File_Exists := False;
         return;
      end if;

      Tree.Source_Info_File_Exists := True;

      if Opt.Verbose_Mode then
         Write_Line ("Reading source info file " &
                     Tree.Source_Info_File_Name.all);
      end if;

      Source_Loop :
      while not End_Of_File (File) loop
         Info := (new Source_Info_Data, 0);
         Source_Info_Table.Increment_Last;

         --  project name
         Get_Line (File, Name_Buffer, Name_Len);
         Proj := Name_Find;
         Info.Info.Project := Proj;
         Info.Next := Source_Info_Project_HTable.Get (Proj);
         Source_Info_Project_HTable.Set (Proj, Source_Info_Table.Last);

         if End_Of_File (File) then
            Report_Error;
            exit Source_Loop;
         end if;

         --  language name
         Get_Line (File, Name_Buffer, Name_Len);
         Info.Info.Language := Name_Find;

         if End_Of_File (File) then
            Report_Error;
            exit Source_Loop;
         end if;

         --  kind
         Get_Line (File, Name_Buffer, Name_Len);
         Info.Info.Kind := Source_Kind'Value (Name_Buffer (1 .. Name_Len));

         if End_Of_File (File) then
            Report_Error;
            exit Source_Loop;
         end if;

         --  display path name
         Get_Line (File, Name_Buffer, Name_Len);
         Info.Info.Display_Path_Name := Name_Find;
         Info.Info.Path_Name := Info.Info.Display_Path_Name;

         if End_Of_File (File) then
            Report_Error;
            exit Source_Loop;
         end if;

         --  optional fields
         Option_Loop :
         loop
            Get_Line (File, Name_Buffer, Name_Len);
            exit Option_Loop when Name_Len = 0;

            if Name_Len <= 2 then
               Report_Error;
               exit Source_Loop;

            else
               if Name_Buffer (1 .. 2) = "P=" then
                  Name_Buffer (1 .. Name_Len - 2) :=
                    Name_Buffer (3 .. Name_Len);
                  Name_Len := Name_Len - 2;
                  Info.Info.Path_Name := Name_Find;

               elsif Name_Buffer (1 .. 2) = "U=" then
                  Name_Buffer (1 .. Name_Len - 2) :=
                    Name_Buffer (3 .. Name_Len);
                  Name_Len := Name_Len - 2;
                  Info.Info.Unit_Name := Name_Find;

               elsif Name_Buffer (1 .. 2) = "I=" then
                  Info.Info.Index := Int'Value (Name_Buffer (3 .. Name_Len));

               elsif Name_Buffer (1 .. Name_Len) = "N=Y" then
                  Info.Info.Naming_Exception := Yes;

               elsif Name_Buffer (1 .. Name_Len) = "N=I" then
                  Info.Info.Naming_Exception := Inherited;

               else
                  Report_Error;
                  exit Source_Loop;
               end if;
            end if;
         end loop Option_Loop;

         Source_Info_Table.Table (Source_Info_Table.Last) := Info;
      end loop Source_Loop;

      Close (File);

   exception
      when others =>
         Close (File);
         Report_Error;
   end Read_Source_Info_File;

   -------------------
   -- Relative_Path --
   -------------------

   function Relative_Path (Pathname : String; To : String) return String is
      function Ensure_Directory (Path : String) return String;
      --  Returns Path with an added directory separator if needed

      ----------------------
      -- Ensure_Directory --
      ----------------------

      function Ensure_Directory (Path : String) return String is
      begin
         if Path'Length = 0
           or else Path (Path'Last) = Directory_Separator
           or else Path (Path'Last) = '/' -- on Windows check also for /
         then
            return Path;
         else
            return Path & Directory_Separator;
         end if;
      end Ensure_Directory;

      --  Local variables

      Dir_Sep_Map : constant Character_Mapping := To_Mapping ("\", "/");

      P  : String (1 .. Pathname'Length) := Pathname;
      T  : String (1 .. To'Length) := To;

      Pi : Natural; -- common prefix ending
      N  : Natural := 0;

   --  Start of processing for Relative_Path

   begin
      pragma Assert (Is_Absolute_Path (Pathname));
      pragma Assert (Is_Absolute_Path (To));

      --  Use canonical directory separator

      Translate (Source => P, Mapping => Dir_Sep_Map);
      Translate (Source => T, Mapping => Dir_Sep_Map);

      --  First check for common prefix

      Pi := 1;
      while Pi < P'Last and then Pi < T'Last and then P (Pi) = T (Pi) loop
         Pi := Pi + 1;
      end loop;

      --  Cut common prefix at a directory separator

      while Pi > P'First and then P (Pi) /= '/' loop
         Pi := Pi - 1;
      end loop;

      --  Count directory under prefix in P, these will be replaced by the
      --  corresponding number of "..".

      N := Ada.Strings.Fixed.Count (T (Pi + 1 .. T'Last), "/");

      if T (T'Last) /= '/' then
         N := N + 1;
      end if;

      return N * "../" & Ensure_Directory (P (Pi + 1 .. P'Last));
   end Relative_Path;

   ----------------------
   -- Set_Program_Name --
   ----------------------

   procedure Set_Program_Name (N : String) is
   begin
      Program_Name := new String'(N);
   end Set_Program_Name;

   --------------------
   -- Source_Info_Of --
   --------------------

   function Source_Info_Of (Iter : Source_Info_Iterator) return Source_Info is
   begin
      return Iter.Info;
   end Source_Info_Of;

   --------------
   -- Value_Of --
   --------------

   function Value_Of
     (Variable : Variable_Value;
      Default  : String) return String
   is
   begin
      if Variable.Kind /= Single
        or else Variable.Default
        or else Variable.Value = No_Name
      then
         return Default;
      else
         return Get_Name_String (Variable.Value);
      end if;
   end Value_Of;

   function Value_Of
     (Index    : Name_Id;
      In_Array : Array_Element_Id;
      Shared   : Shared_Project_Tree_Data_Access) return Name_Id
   is

      Current    : Array_Element_Id;
      Element    : Array_Element;
      Real_Index : Name_Id := Index;

   begin
      Current := In_Array;

      if Current = No_Array_Element then
         return No_Name;
      end if;

      Element := Shared.Array_Elements.Table (Current);

      if not Element.Index_Case_Sensitive then
         Get_Name_String (Index);
         To_Lower (Name_Buffer (1 .. Name_Len));
         Real_Index := Name_Find;
      end if;

      while Current /= No_Array_Element loop
         Element := Shared.Array_Elements.Table (Current);

         if Real_Index = Element.Index then
            exit when Element.Value.Kind /= Single;
            exit when Element.Value.Value = Empty_String;
            return Element.Value.Value;
         else
            Current := Element.Next;
         end if;
      end loop;

      return No_Name;
   end Value_Of;

   function Value_Of
     (Index                  : Name_Id;
      Src_Index              : Int := 0;
      In_Array               : Array_Element_Id;
      Shared                 : Shared_Project_Tree_Data_Access;
      Force_Lower_Case_Index : Boolean := False;
      Allow_Wildcards        : Boolean := False) return Variable_Value
   is
      Current      : Array_Element_Id;
      Element      : Array_Element;
      Real_Index_1 : Name_Id;
      Real_Index_2 : Name_Id;

   begin
      Current := In_Array;

      if Current = No_Array_Element then
         return Nil_Variable_Value;
      end if;

      Element := Shared.Array_Elements.Table (Current);

      Real_Index_1 := Index;

      if not Element.Index_Case_Sensitive or else Force_Lower_Case_Index then
         if Index /= All_Other_Names then
            Get_Name_String (Index);
            To_Lower (Name_Buffer (1 .. Name_Len));
            Real_Index_1 := Name_Find;
         end if;
      end if;

      while Current /= No_Array_Element loop
         Element := Shared.Array_Elements.Table (Current);
         Real_Index_2 := Element.Index;

         if not Element.Index_Case_Sensitive
           or else Force_Lower_Case_Index
         then
            if Element.Index /= All_Other_Names then
               Get_Name_String (Element.Index);
               To_Lower (Name_Buffer (1 .. Name_Len));
               Real_Index_2 := Name_Find;
            end if;
         end if;

         if Src_Index = Element.Src_Index and then
           (Real_Index_1 = Real_Index_2 or else
              (Real_Index_2 /= All_Other_Names and then
               Allow_Wildcards and then
                 Match (Get_Name_String (Real_Index_1),
                        Compile (Get_Name_String (Real_Index_2),
                                 Glob => True))))
         then
            return Element.Value;
         else
            Current := Element.Next;
         end if;
      end loop;

      return Nil_Variable_Value;
   end Value_Of;

   function Value_Of
     (Name                    : Name_Id;
      Index                   : Int := 0;
      Attribute_Or_Array_Name : Name_Id;
      In_Package              : Package_Id;
      Shared                  : Shared_Project_Tree_Data_Access;
      Force_Lower_Case_Index  : Boolean := False;
      Allow_Wildcards         : Boolean := False) return Variable_Value
   is
      The_Array     : Array_Element_Id;
      The_Attribute : Variable_Value := Nil_Variable_Value;

   begin
      if In_Package /= No_Package then

         --  First, look if there is an array element that fits

         The_Array :=
           Value_Of
             (Name      => Attribute_Or_Array_Name,
              In_Arrays => Shared.Packages.Table (In_Package).Decl.Arrays,
              Shared    => Shared);
         The_Attribute :=
           Value_Of
             (Index                  => Name,
              Src_Index              => Index,
              In_Array               => The_Array,
              Shared                 => Shared,
              Force_Lower_Case_Index => Force_Lower_Case_Index,
              Allow_Wildcards        => Allow_Wildcards);

         --  If there is no array element, look for a variable

         if The_Attribute = Nil_Variable_Value then
            The_Attribute :=
              Value_Of
                (Variable_Name => Attribute_Or_Array_Name,
                 In_Variables  => Shared.Packages.Table
                   (In_Package).Decl.Attributes,
                 Shared        => Shared);
         end if;
      end if;

      return The_Attribute;
   end Value_Of;

   function Value_Of
     (Index     : Name_Id;
      In_Array  : Name_Id;
      In_Arrays : Array_Id;
      Shared    : Shared_Project_Tree_Data_Access) return Name_Id
   is
      Current   : Array_Id;
      The_Array : Array_Data;

   begin
      Current := In_Arrays;
      while Current /= No_Array loop
         The_Array := Shared.Arrays.Table (Current);
         if The_Array.Name = In_Array then
            return Value_Of
              (Index, In_Array => The_Array.Value, Shared => Shared);
         else
            Current := The_Array.Next;
         end if;
      end loop;

      return No_Name;
   end Value_Of;

   function Value_Of
     (Name      : Name_Id;
      In_Arrays : Array_Id;
      Shared    : Shared_Project_Tree_Data_Access) return Array_Element_Id
   is
      Current   : Array_Id;
      The_Array : Array_Data;

   begin
      Current := In_Arrays;
      while Current /= No_Array loop
         The_Array := Shared.Arrays.Table (Current);

         if The_Array.Name = Name then
            return The_Array.Value;
         else
            Current := The_Array.Next;
         end if;
      end loop;

      return No_Array_Element;
   end Value_Of;

   function Value_Of
     (Name        : Name_Id;
      In_Packages : Package_Id;
      Shared      : Shared_Project_Tree_Data_Access) return Package_Id
   is
      Current     : Package_Id;
      The_Package : Package_Element;

   begin
      Current := In_Packages;
      while Current /= No_Package loop
         The_Package := Shared.Packages.Table (Current);
         exit when The_Package.Name /= No_Name
           and then The_Package.Name = Name;
         Current := The_Package.Next;
      end loop;

      return Current;
   end Value_Of;

   function Value_Of
     (Variable_Name : Name_Id;
      In_Variables  : Variable_Id;
      Shared        : Shared_Project_Tree_Data_Access) return Variable_Value
   is
      Current      : Variable_Id;
      The_Variable : Variable;

   begin
      Current := In_Variables;
      while Current /= No_Variable loop
         The_Variable := Shared.Variable_Elements.Table (Current);

         if Variable_Name = The_Variable.Name then
            return The_Variable.Value;
         else
            Current := The_Variable.Next;
         end if;
      end loop;

      return Nil_Variable_Value;
   end Value_Of;

   ------------------------
   -- Write_Program_Name --
   ------------------------

   procedure Write_Program_Name is
   begin
      if Program_Name /= null then
         Write_Str (Program_Name.all & ": ");
      end if;
   end Write_Program_Name;

   ----------------------------
   -- Write_Source_Info_File --
   ----------------------------

   procedure Write_Source_Info_File (Tree : Project_Tree_Ref) is
      Iter   : Source_Iterator := For_Each_Source (Tree);
      Source : GPR.Source_Id;
      File   : Text_File;

   begin
      if Opt.Verbose_Mode then
         Write_Line ("Writing new source info file " &
                     Tree.Source_Info_File_Name.all);
      end if;

      Create (File, Tree.Source_Info_File_Name.all);

      if not Is_Valid (File) then
         Write_Line ("warning: unable to create source info file """ &
                     Tree.Source_Info_File_Name.all & '"');
         return;
      end if;

      loop
         Source := Element (Iter);
         exit when Source = No_Source;

         if not Source.Locally_Removed and then
           Source.Replaced_By = No_Source
         then
            --  Project name

            Put_Line (File, Get_Name_String (Source.Project.Name));

            --  Language name

            Put_Line (File, Get_Name_String (Source.Language.Name));

            --  Kind

            Put_Line (File, Source.Kind'Img);

            --  Display path name

            Put_Line (File, Get_Name_String (Source.Path.Display_Name));

            --  Optional lines:

            --  Path name (P=)

            if Source.Path.Name /= Source.Path.Display_Name then
               Put (File, "P=");
               Put_Line (File, Get_Name_String (Source.Path.Name));
            end if;

            --  Unit name (U=)

            if Source.Unit /= No_Unit_Index then
               Put (File, "U=");
               Put_Line (File, Get_Name_String (Source.Unit.Name));
            end if;

            --  Multi-source index (I=)

            if Source.Index /= 0 then
               Put (File, "I=");
               Put_Line (File, Source.Index'Img);
            end if;

            --  Naming exception ("N=T");

            if Source.Naming_Exception = Yes then
               Put_Line (File, "N=Y");

            elsif Source.Naming_Exception = Inherited then
               Put_Line (File, "N=I");
            end if;

            --  Empty line to indicate end of info on this source

            Put_Line (File, "");
         end if;

         Next (Iter);
      end loop;

      Close (File);
   end Write_Source_Info_File;

   ---------------
   -- Write_Str --
   ---------------

   procedure Write_Str
     (S          : String;
      Max_Length : Positive;
      Separator  : Character)
   is
      First : Positive := S'First;
      Last  : Natural  := S'Last;

   begin
      --  Nothing to do for empty strings

      if S'Length > 0 then

         --  Start on a new line if current line is already longer than
         --  Max_Length.

         if Positive (Column) >= Max_Length then
            Write_Eol;
         end if;

         --  If length of remainder is longer than Max_Length, we need to
         --  cut the remainder in several lines.

         while Positive (Column) + S'Last - First > Max_Length
         loop

            --  Try the maximum length possible

            Last := First + Max_Length - Positive (Column);

            --  Look for last Separator in the line

            while Last >= First and then S (Last) /= Separator loop
               Last := Last - 1;
            end loop;

            --  If we do not find a separator, we output the maximum length
            --  possible.

            if Last < First then
               Last := First + Max_Length - Positive (Column);
            end if;

            Write_Line (S (First .. Last));

            --  Set the beginning of the new remainder

            First := Last + 1;
         end loop;

         --  What is left goes to the buffer, without EOL

         Write_Str (S (First .. S'Last));
      end if;
   end Write_Str;

begin
   declare
      Ext : String_Access := System.OS_Lib.Get_Target_Executable_Suffix;
   begin
      Name_Len := 0;
      Add_Str_To_Name_Buffer (Ext.all);
      Executable_Extension_On_Target := Name_Enter;
      Free (Ext);
   end;
end GPR.Util;
