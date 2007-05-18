------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             P R J . N M S C                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2000-2007, Free Software Foundation, Inc.         --
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

with GNAT.Case_Util;             use GNAT.Case_Util;
with GNAT.Directory_Operations;  use GNAT.Directory_Operations;
with GNAT.HTable;

with Err_Vars; use Err_Vars;
with Fmap;     use Fmap;
with Opt;      use Opt;
with Osint;    use Osint;
with Output;   use Output;
with Prj.Err;
with Prj.Util; use Prj.Util;
with Snames;   use Snames;
with Table;    use Table;
with Targparm; use Targparm;

with Ada.Characters.Handling;    use Ada.Characters.Handling;
with Ada.Directories;            use Ada.Directories;

package body Prj.Nmsc is

   No_Continuation_String : aliased String := "";
   Continuation_String    : aliased String := "\";
   --  Used in Check_Library for continuation error messages at the same
   --  location.

   Error_Report : Put_Line_Access := null;
   --  Set to point to error reporting procedure

   When_No_Sources : Error_Warning := Error;
   --  Indicates what should be done when there is no sources in a non
   --  extending Ada project.

   type Name_Location is record
      Name     : File_Name_Type;
      Location : Source_Ptr;
      Source   : Source_Id := No_Source;
      Except   : Boolean := False;
      Found    : Boolean := False;
   end record;
   --  Information about file names found in string list attribute
   --  Source_Files or in a source list file, stored in hash table
   --  Source_Names, used by procedure Get_Path_Names_And_Record_Sources.

   No_Name_Location : constant Name_Location :=
                        (Name     => No_File,
                         Location => No_Location,
                         Source   => No_Source,
                         Except   => False,
                         Found    => False);

   package Source_Names is new GNAT.HTable.Simple_HTable
     (Header_Num => Header_Num,
      Element    => Name_Location,
      No_Element => No_Name_Location,
      Key        => File_Name_Type,
      Hash       => Hash,
      Equal      => "=");
   --  Hash table to store file names found in string list attribute
   --  Source_Files or in a source list file, stored in hash table
   --  Source_Names, used by procedure Get_Path_Names_And_Record_Sources.

   package Recursive_Dirs is new GNAT.HTable.Simple_HTable
     (Header_Num => Header_Num,
      Element    => Boolean,
      No_Element => False,
      Key        => Name_Id,
      Hash       => Hash,
      Equal      => "=");
   --  Hash table to store recursive source directories, to avoid looking
   --  several times, and to avoid cycles that may be introduced by symbolic
   --  links.

   type Ada_Naming_Exception_Id is new Nat;
   No_Ada_Naming_Exception : constant Ada_Naming_Exception_Id := 0;

   type Unit_Info is record
      Kind : Spec_Or_Body;
      Unit : Name_Id;
      Next : Ada_Naming_Exception_Id := No_Ada_Naming_Exception;
   end record;
   --  No_Unit : constant Unit_Info :=
   --              (Specification, No_Name, No_Ada_Naming_Exception);

   package Ada_Naming_Exception_Table is new Table.Table
     (Table_Component_Type => Unit_Info,
      Table_Index_Type     => Ada_Naming_Exception_Id,
      Table_Low_Bound      => 1,
      Table_Initial        => 20,
      Table_Increment      => 100,
      Table_Name           => "Prj.Nmsc.Ada_Naming_Exception_Table");

   package Ada_Naming_Exceptions is new GNAT.HTable.Simple_HTable
     (Header_Num => Header_Num,
      Element    => Ada_Naming_Exception_Id,
      No_Element => No_Ada_Naming_Exception,
      Key        => File_Name_Type,
      Hash       => Hash,
      Equal      => "=");
   --  A hash table to store naming exceptions for Ada. For each file name
   --  there is one or several unit in table Ada_Naming_Exception_Table.

   function Hash (Unit : Unit_Info) return Header_Num;

   type Name_And_Index is record
      Name  : Name_Id := No_Name;
      Index : Int     := 0;
   end record;
   No_Name_And_Index : constant Name_And_Index :=
                         (Name => No_Name, Index => 0);

   package Reverse_Ada_Naming_Exceptions is new GNAT.HTable.Simple_HTable
     (Header_Num => Header_Num,
      Element    => Name_And_Index,
      No_Element => No_Name_And_Index,
      Key        => Unit_Info,
      Hash       => Hash,
      Equal      => "=");
   --  A table to check if a unit with an exceptional name will hide
   --  a source with a file name following the naming convention.

   procedure Add_Source
     (Id      : Source_Id;
      Data    : in out Project_Data;
      In_Tree : Project_Tree_Ref);
   --  Add a new source to the different lists: list of all sources in the
   --  project tree, list of source of a project and list of sources of a
   --  language.

   procedure Check_Ada_Name (Name : String; Unit : out Name_Id);
   --  Check that a name is a valid Ada unit name

   procedure Check_Naming_Schemes
     (Data    : in out Project_Data;
      Project : Project_Id;
      In_Tree : Project_Tree_Ref);
   --  Check the naming scheme part of Data

   procedure Check_If_Externally_Built
     (Project : Project_Id;
      In_Tree : Project_Tree_Ref;
      Data    : in out Project_Data);
   --  Check attribute Externally_Built of project Project in project tree
   --  In_Tree and modify its data Data if it has the value "true".

   procedure Check_Library_Attributes
     (Project   : Project_Id;
      In_Tree : Project_Tree_Ref;
      Data      : in out Project_Data);
   --  Check the library attributes of project Project in project tree In_Tree
   --  and modify its data Data accordingly.

   procedure Check_Package_Naming
     (Project : Project_Id;
      In_Tree : Project_Tree_Ref;
      Data    : in out Project_Data);
   --  Check package Naming of project Project in project tree In_Tree and
   --  modify its data Data accordingly.

   procedure Check_Programming_Languages
     (In_Tree       : Project_Tree_Ref;
      Project       : Project_Id;
      Data          : in out Project_Data);
   --  Check attribute Languages for the project with data Data in project
   --  tree In_Tree and set the components of Data for all the programming
   --  languages indicated in attribute Languages, if any.

   procedure Check_Stand_Alone_Library
     (Project   : Project_Id;
      In_Tree   : Project_Tree_Ref;
      Data      : in out Project_Data;
      Extending : Boolean);
   --  Check if project Project in project tree In_Tree is a Stand-Alone
   --  Library project, and modify its data Data accordingly if it is one.

   function Compute_Directory_Last (Dir : String) return Natural;
   --  Return the index of the last significant character in Dir. This is used
   --  to avoid duplicates '/' at the end of directory names

   procedure Error_Msg
     (Project       : Project_Id;
      In_Tree       : Project_Tree_Ref;
      Msg           : String;
      Flag_Location : Source_Ptr);
   --  Output an error message. If Error_Report is null, simply call
   --  Prj.Err.Error_Msg. Otherwise, disregard Flag_Location and use
   --  Error_Report.

   procedure Free_Ada_Naming_Exceptions;
   --  Free the internal hash tables used for checking naming exceptions

   procedure Get_Directories
     (Project : Project_Id;
      In_Tree : Project_Tree_Ref;
      Data    : in out Project_Data);
   --  Get the object directory, the exec directory and the source directories
   --  of a project.

   procedure Get_Mains
     (Project : Project_Id;
      In_Tree : Project_Tree_Ref;
      Data    : in out Project_Data);
   --  Get the mains of a project from attribute Main, if it exists, and put
   --  them in the project data.

   procedure Get_Sources_From_File
     (Path     : String;
      Location : Source_Ptr;
      Project  : Project_Id;
      In_Tree  : Project_Tree_Ref);
   --  Get the list of sources from a text file and put them in hash table
   --  Source_Names.

   procedure Locate_Directory
     (Project  : Project_Id;
      In_Tree  : Project_Tree_Ref;
      Name     : File_Name_Type;
      Parent   : Path_Name_Type;
      Dir      : out Path_Name_Type;
      Display  : out Path_Name_Type;
      Create   : String := "";
      Location : Source_Ptr := No_Location);
   --  Locate a directory. Name is the directory name. Parent is the root
   --  directory, if Name a relative path name. Dir is set to the canonical
   --  case path name of the directory, and Display is the directory path name
   --  for display purposes. If the directory does not exist and Project_Setup
   --  is True and Create is a non null string, an attempt is made to create
   --  the directory. If the directory does not exist and Project_Setup is
   --  false, then Dir and Display are set to No_Name.

   procedure Look_For_Sources
     (Project      : Project_Id;
      In_Tree      : Project_Tree_Ref;
      Data         : in out Project_Data;
      Follow_Links : Boolean);
   --  Find all the sources of project Project in project tree In_Tree and
   --  update its Data accordingly. Resolve symbolic links in the path names
   --  if Follow_Links is True.

   function Path_Name_Of
     (File_Name : File_Name_Type;
      Directory : Path_Name_Type) return String;
   --  Returns the path name of a (non project) file.
   --  Returns an empty string if file cannot be found.

   procedure Remove_Source
     (Id          : Source_Id;
      Replaced_By : Source_Id;
      Project     : Project_Id;
      Data        : in out Project_Data;
      In_Tree     : Project_Tree_Ref);

   procedure Report_No_Sources
     (Project   : Project_Id;
      Lang_Name : String;
      In_Tree   : Project_Tree_Ref;
      Location  : Source_Ptr);
   --  Report an error or a warning depending on the value of When_No_Sources
   --  when there are no sources for language Lang_Name.

   procedure Show_Source_Dirs
     (Data : Project_Data; In_Tree : Project_Tree_Ref);
   --  List all the source directories of a project

   ----------------
   -- Add_Source --
   ----------------

   procedure Add_Source
     (Id      : Source_Id;
      Data    : in out Project_Data;
      In_Tree : Project_Tree_Ref)
   is
      Language : constant Language_Index :=
                   In_Tree.Sources.Table (Id).Language;

      Source   : Source_Id;

   begin
      --  Add the source to the global list

      In_Tree.Sources.Table (Id).Next_In_Sources := In_Tree.First_Source;
      In_Tree.First_Source := Id;

      --  Add the source to the project list

      Source := Data.Last_Source;

      if Source = No_Source then
         Data.First_Source := Id;

      else
         In_Tree.Sources.Table (Source).Next_In_Project := Id;
      end if;

      Data.Last_Source := Id;

      --  Add the source to the language list

      In_Tree.Sources.Table (Id).Next_In_Lang :=
        In_Tree.Languages_Data.Table (Language).First_Source;
      In_Tree.Languages_Data.Table (Language).First_Source := Id;
   end Add_Source;

   -----------
   -- Check --
   -----------

   procedure Check
     (Project         : Project_Id;
      In_Tree         : Project_Tree_Ref;
      Report_Error    : Put_Line_Access;
      Follow_Links    : Boolean;
      When_No_Sources : Error_Warning)
   is
      Data      : Project_Data := In_Tree.Projects.Table (Project);
      Extending : Boolean := False;

      Lang_Proc_Pkg   : Package_Id;
      Linker_Name     : Variable_Value;
      Min_Linker_Opts : Variable_Value;

   begin
      Nmsc.When_No_Sources := When_No_Sources;
      Error_Report := Report_Error;

      Recursive_Dirs.Reset;

      Check_If_Externally_Built (Project, In_Tree, Data);

      --  Object, exec and source directories

      Get_Directories (Project, In_Tree, Data);

      --  Get the programming languages

      Check_Programming_Languages (In_Tree, Project, Data);

      --  Library attributes

      Check_Library_Attributes (Project, In_Tree, Data);

      if Current_Verbosity = High then
         Show_Source_Dirs (Data, In_Tree);
      end if;

      Check_Package_Naming (Project, In_Tree, Data);

      Extending := Data.Extends /= No_Project;

      Check_Naming_Schemes (Data, Project, In_Tree);

      --  Find the sources

      if Data.Source_Dirs /= Nil_String then
         Look_For_Sources (Project, In_Tree, Data, Follow_Links);

         if (not Data.Externally_Built) and then (not Extending) then
            declare
               Language : Language_Index;
               Source   : Source_Id;
               Src_Data : Source_Data;
               Alt_Lang : Alternate_Language_Id;
               Alt_Lang_Data : Alternate_Language_Data;

            begin
               Language := Data.First_Language_Processing;

               while Language /= No_Language_Index loop
                  Source := Data.First_Source;

                  Source_Loop :
                  while Source /= No_Source loop
                     Src_Data := In_Tree.Sources.Table (Source);

                     exit Source_Loop when Src_Data.Language = Language;

                     Alt_Lang := Src_Data.Alternate_Languages;

                     Alternate_Loop :
                     while Alt_Lang /= No_Alternate_Language loop
                        Alt_Lang_Data :=
                          In_Tree.Alt_Langs.Table (Alt_Lang);
                        exit Source_Loop
                               when Alt_Lang_Data.Language = Language;
                        Alt_Lang := Alt_Lang_Data.Next;
                     end loop Alternate_Loop;

                     Source := Src_Data.Next_In_Project;
                  end loop Source_Loop;

                  if Source = No_Source then
                     Report_No_Sources
                       (Project,
                        Get_Name_String
                          (In_Tree.Languages_Data.Table
                             (Language).Display_Name),
                        In_Tree,
                        Data.Location);
                  end if;

                  Language := In_Tree.Languages_Data.Table (Language).Next;
               end loop;
            end;
         end if;
      end if;

      --  If it is a library project file, check if it is a standalone library

      if Data.Library then
         Check_Stand_Alone_Library (Project, In_Tree, Data, Extending);
      end if;

      --  Put the list of Mains, if any, in the project data

      Get_Mains (Project, In_Tree, Data);

      --  Check if there is a linker specified

      Lang_Proc_Pkg :=
        Value_Of (Name_Language_Processing, Data.Decl.Packages, In_Tree);

      if Lang_Proc_Pkg /= No_Package then
         Linker_Name :=
           Value_Of
             (Variable_Name => Name_Linker,
              In_Variables  =>
                In_Tree.Packages.Table (Lang_Proc_Pkg).Decl.Attributes,
              In_Tree       => In_Tree);

         if Linker_Name /= Nil_Variable_Value then
            Get_Name_String (Linker_Name.Value);

            if Name_Len > 0 then
               --  A non empty linker name was specified

               Data.Linker_Name := File_Name_Type (Linker_Name.Value);

               --  Check if Minimum_Linker_Options is specified

               Min_Linker_Opts :=
                 Value_Of
                   (Variable_Name => Name_Minimum_Linker_Options,
                    In_Variables  =>
                      In_Tree.Packages.Table
                                         (Lang_Proc_Pkg).Decl.Attributes,
                    In_Tree       => In_Tree);

               if Min_Linker_Opts /= Nil_Variable_Value then
                  declare
                     Current : String_List_Id := Min_Linker_Opts.Values;
                     Element : String_Element;
                     NL_Id   : Name_List_Index := No_Name_List;
                  begin
                     while Current /= Nil_String loop
                        Element :=
                          In_Tree.String_Elements.Table (Current);
                        Name_List_Table.Increment_Last (In_Tree.Name_Lists);

                        if NL_Id = No_Name_List then
                           Data.Minimum_Linker_Options :=
                             Name_List_Table.Last (In_Tree.Name_Lists);

                        else
                           In_Tree.Name_Lists.Table (NL_Id).Next :=
                             Name_List_Table.Last (In_Tree.Name_Lists);
                        end if;

                        NL_Id := Name_List_Table.Last (In_Tree.Name_Lists);
                        In_Tree.Name_Lists.Table (NL_Id) :=
                          (Element.Value, No_Name_List);

                        Current := Element.Next;
                     end loop;
                  end;
               end if;
            end if;
         end if;
      end if;

      --  Update the project data in the Projects table

      In_Tree.Projects.Table (Project) := Data;

      Free_Ada_Naming_Exceptions;
   end Check;

   --------------------
   -- Check_Ada_Name --
   --------------------

   procedure Check_Ada_Name (Name : String; Unit : out Name_Id) is
      The_Name        : String := Name;
      Real_Name       : Name_Id;
      Need_Letter     : Boolean := True;
      Last_Underscore : Boolean := False;
      OK              : Boolean := The_Name'Length > 0;

   begin
      To_Lower (The_Name);

      Name_Len := The_Name'Length;
      Name_Buffer (1 .. Name_Len) := The_Name;

      --  Special cases of children of packages A, G, I and S on VMS

      if OpenVMS_On_Target and then
        Name_Len > 3 and then
        Name_Buffer (2 .. 3) = "__" and then
        ((Name_Buffer (1) = 'a') or else (Name_Buffer (1) = 'g') or else
         (Name_Buffer (1) = 'i') or else (Name_Buffer (1) = 's'))
      then
         Name_Buffer (2) := '.';
         Name_Buffer (3 .. Name_Len - 1) := Name_Buffer (4 .. Name_Len);
         Name_Len := Name_Len - 1;
      end if;

      Real_Name := Name_Find;

      --  Check first that the given name is not an Ada reserved word

      if Get_Name_Table_Byte (Real_Name) /= 0
        and then Real_Name /= Name_Project
        and then Real_Name /= Name_Extends
        and then Real_Name /= Name_External
      then
         Unit := No_Name;

         if Current_Verbosity = High then
            Write_Str (The_Name);
            Write_Line (" is an Ada reserved word.");
         end if;

         return;
      end if;

      for Index in The_Name'Range loop
         if Need_Letter then

            --  We need a letter (at the beginning, and following a dot),
            --  but we don't have one.

            if Is_Letter (The_Name (Index)) then
               Need_Letter := False;

            else
               OK := False;

               if Current_Verbosity = High then
                  Write_Int  (Types.Int (Index));
                  Write_Str  (": '");
                  Write_Char (The_Name (Index));
                  Write_Line ("' is not a letter.");
               end if;

               exit;
            end if;

         elsif Last_Underscore
           and then (The_Name (Index) = '_' or else The_Name (Index) = '.')
         then
            --  Two underscores are illegal, and a dot cannot follow
            --  an underscore.

            OK := False;

            if Current_Verbosity = High then
               Write_Int  (Types.Int (Index));
               Write_Str  (": '");
               Write_Char (The_Name (Index));
               Write_Line ("' is illegal here.");
            end if;

            exit;

         elsif The_Name (Index) = '.' then

            --  We need a letter after a dot

            Need_Letter := True;

         elsif The_Name (Index) = '_' then
            Last_Underscore := True;

         else
            --  We need an letter or a digit

            Last_Underscore := False;

            if not Is_Alphanumeric (The_Name (Index)) then
               OK := False;

               if Current_Verbosity = High then
                  Write_Int  (Types.Int (Index));
                  Write_Str  (": '");
                  Write_Char (The_Name (Index));
                  Write_Line ("' is not alphanumeric.");
               end if;

               exit;
            end if;
         end if;
      end loop;

      --  Cannot end with an underscore or a dot

      OK := OK and then not Need_Letter and then not Last_Underscore;

      if OK then
         Unit := Real_Name;

      else
         --  Signal a problem with No_Name

         Unit := No_Name;
      end if;
   end Check_Ada_Name;

   -------------------------------
   -- Check_If_Externally_Built --
   -------------------------------

   procedure Check_If_Externally_Built
     (Project : Project_Id;
      In_Tree : Project_Tree_Ref;
      Data    : in out Project_Data)
   is
      Externally_Built : constant Variable_Value :=
                           Util.Value_Of
                            (Name_Externally_Built,
                             Data.Decl.Attributes, In_Tree);

   begin
      if not Externally_Built.Default then
         Get_Name_String (Externally_Built.Value);
         To_Lower (Name_Buffer (1 .. Name_Len));

         if Name_Buffer (1 .. Name_Len) = "true" then
            Data.Externally_Built := True;

         elsif Name_Buffer (1 .. Name_Len) /= "false" then
            Error_Msg (Project, In_Tree,
                       "Externally_Built may only be true or false",
                       Externally_Built.Location);
         end if;
      end if;

      if Current_Verbosity = High then
         Write_Str ("Project is ");

         if not Data.Externally_Built then
            Write_Str ("not ");
         end if;

         Write_Line ("externally built.");
      end if;
   end Check_If_Externally_Built;

   -----------------------------
   -- Check_Naming_Schemes --
   -----------------------------

   procedure Check_Naming_Schemes
     (Data    : in out Project_Data;
      Project : Project_Id;
      In_Tree : Project_Tree_Ref)
   is
      Naming_Id : constant Package_Id :=
                    Util.Value_Of (Name_Naming, Data.Decl.Packages, In_Tree);

      Naming    : Package_Element;

      procedure Get_Exceptions (Kind : Source_Kind);

      procedure Get_Unit_Exceptions (Kind : Source_Kind);

      --------------------
      -- Get_Exceptions --
      --------------------

      procedure Get_Exceptions (Kind : Source_Kind) is
         Exceptions : Array_Element_Id;

         Exception_List            : Variable_Value;
         Element_Id                : String_List_Id;
         Element                   : String_Element;
         File_Name                 : File_Name_Type;

         Lang_Id                   : Language_Index :=
                                       Data.First_Language_Processing;
         Lang                      : Name_Id;

         Source                    : Source_Id;

      begin
         if Kind = Impl then
            Exceptions := Value_Of
              (Name_Implementation_Exceptions,
               In_Arrays => Naming.Decl.Arrays,
               In_Tree   => In_Tree);

         else
            Exceptions := Value_Of
              (Name_Specification_Exceptions,
               In_Arrays => Naming.Decl.Arrays,
               In_Tree   => In_Tree);
         end if;

         while Lang_Id /= No_Language_Index loop
            if In_Tree.Languages_Data.Table (Lang_Id).Config.Kind =
                                                               File_Based
            then
               Lang := In_Tree.Languages_Data.Table (Lang_Id).Name;

               Exception_List := Value_Of
                 (Index    => Lang,
                  In_Array => Exceptions,
                  In_Tree  => In_Tree);

               if Exception_List /= Nil_Variable_Value then
                  Element_Id := Exception_List.Values;

                  while Element_Id /= Nil_String loop
                     Element :=
                       In_Tree.String_Elements.Table (Element_Id);
                     Get_Name_String (Element.Value);
                     Canonical_Case_File_Name
                       (Name_Buffer (1 .. Name_Len));
                     File_Name := Name_Find;

                     Source := Data.First_Source;

                     while Source /= No_Source
                       and then
                       In_Tree.Sources.Table (Source).File /= File_Name
                     loop
                        Source :=
                          In_Tree.Sources.Table (Source).Next_In_Project;
                     end loop;

                     if Source = No_Source then
                        --  This is a new source. Create an entry for it
                        --  in the Sources table.

                        Source_Data_Table.Increment_Last
                          (In_Tree.Sources);
                        Source := Source_Data_Table.Last
                          (In_Tree.Sources);

                        if Current_Verbosity = High then
                           Write_Str ("Adding source #");
                           Write_Line (Source'Img);
                        end if;

                        declare
                           Src_Data : Source_Data := No_Source_Data;

                        begin
                           Src_Data.Project := Project;
                           Src_Data.Language_Name := Lang;
                           Src_Data.Language := Lang_Id;
                           Src_Data.Kind := Kind;
                           Src_Data.File := File_Name;
                           Src_Data.Display_File :=
                             File_Name_Type (Element.Value);
                           Src_Data.Object := Object_Name (File_Name);
                           Src_Data.Dependency :=
                             In_Tree.Languages_Data.Table
                                         (Lang_Id).Config.Dependency_Kind;
                           Src_Data.Dep_Name :=
                             Dependency_Name (File_Name, Src_Data.Dependency);
                           Src_Data.Switches := Switches_Name (File_Name);
                           Src_Data.Naming_Exception := True;
                           In_Tree.Sources.Table (Source) := Src_Data;
                        end;

                        Add_Source (Source, Data, In_Tree);

                     else
                        --  Check if the file name is already recorded for
                        --  another language or another kind.

                        if
                          In_Tree.Sources.Table (Source).Language /= Lang_Id
                        then
                           Error_Msg
                             (Project,
                              In_Tree,
                              "the same file cannot be a source " &
                              "of two languages",
                              Element.Location);

                        elsif In_Tree.Sources.Table (Source).Kind /= Kind then
                           Error_Msg
                             (Project,
                              In_Tree,
                              "the same file cannot be a source " &
                              "and a template",
                              Element.Location);
                        end if;

                        --  If the file is already recorded for the same
                        --  language and the same kind, it means that the file
                        --  name appears several times in the *_Exceptions
                        --  attribute; so there is nothing to do.

                     end if;

                     Element_Id := Element.Next;
                  end loop;
               end if;
            end if;

            Lang_Id := In_Tree.Languages_Data.Table (Lang_Id).Next;
         end loop;
      end Get_Exceptions;

      -------------------------
      -- Get_Unit_Exceptions --
      -------------------------

      procedure Get_Unit_Exceptions (Kind : Source_Kind) is
         Exceptions : Array_Element_Id;
         Element    : Array_Element;

         Unit       : Name_Id;

         Index      : Int;

         File_Name  : File_Name_Type;

         Lang_Id    : constant Language_Index :=
                        Data.Unit_Based_Language_Index;
         Lang       : constant Name_Id :=
                        Data.Unit_Based_Language_Name;

         Source            : Source_Id;
         Source_To_Replace : Source_Id := No_Source;

         Other_Project  : Project_Id;

         Other_Part : Source_Id;

      begin
         if Lang_Id = No_Language_Index or else Lang = No_Name then
            return;
         end if;

         if Kind = Impl then
            Exceptions := Value_Of
              (Name_Body,
               In_Arrays => Naming.Decl.Arrays,
               In_Tree   => In_Tree);

            if Exceptions = No_Array_Element then
               Exceptions := Value_Of
                 (Name_Implementation,
                  In_Arrays => Naming.Decl.Arrays,
                  In_Tree   => In_Tree);
            end if;

         else
            Exceptions := Value_Of
              (Name_Spec,
               In_Arrays => Naming.Decl.Arrays,
               In_Tree   => In_Tree);

            if Exceptions = No_Array_Element then
               Exceptions := Value_Of
                 (Name_Specification,
                  In_Arrays => Naming.Decl.Arrays,
                  In_Tree   => In_Tree);
            end if;

         end if;

         while Exceptions /= No_Array_Element loop
            Element := In_Tree.Array_Elements.Table (Exceptions);

            Get_Name_String (Element.Value.Value);
            Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));
            File_Name := Name_Find;

            Get_Name_String (Element.Index);
            To_Lower (Name_Buffer (1 .. Name_Len));
            Unit := Name_Find;

            Index := Element.Value.Index;

            --  For Ada, check if it is a valid unit name

            if Lang = Name_Ada then
               Get_Name_String (Element.Index);
               Check_Ada_Name (Name_Buffer (1 .. Name_Len), Unit);

               if Unit = No_Name then
                  Err_Vars.Error_Msg_Name_1 := Element.Index;
                  Error_Msg
                    (Project, In_Tree,
                     "%% is not a valid unit name.",
                     Element.Value.Location);
               end if;
            end if;

            if Unit /= No_Name then
               --  Check if the source already exists

               Source := In_Tree.First_Source;
               Source_To_Replace := No_Source;

               while Source /= No_Source and then
                 (In_Tree.Sources.Table (Source).Unit /= Unit or else
                  In_Tree.Sources.Table (Source).Index /= Index)
               loop
                  Source := In_Tree.Sources.Table (Source).Next_In_Sources;
               end loop;

               if Source /= No_Source then
                  if In_Tree.Sources.Table (Source).Kind /= Kind then
                     Other_Part := Source;

                     loop
                        Source :=
                          In_Tree.Sources.Table (Source).Next_In_Sources;

                        exit when Source = No_Source or else
                          (In_Tree.Sources.Table (Source).Unit = Unit
                           and then
                           In_Tree.Sources.Table (Source).Index = Index);
                     end loop;
                  end if;

                  if Source /= No_Source then
                     Other_Project := In_Tree.Sources.Table (Source).Project;

                     if Is_Extending (Project, Other_Project, In_Tree) then
                        Other_Part :=
                          In_Tree.Sources.Table (Source).Other_Part;

                        --  Record the source to be removed

                        Source_To_Replace := Source;
                        Source := No_Source;

                     else
                        Error_Msg_Name_1 := Unit;

                        Error_Msg
                          (Project,
                           In_Tree,
                           "unit%% cannot belong to two projects " &
                           "simultaneously",
                           Element.Value.Location);
                     end if;
                  end if;
               end if;

               if Source = No_Source then
                  Source_Data_Table.Increment_Last (In_Tree.Sources);
                  Source := Source_Data_Table.Last (In_Tree.Sources);

                  if Current_Verbosity = High then
                     Write_Str ("Adding source #");
                     Write_Str (Source'Img);
                     Write_Str (", File : ");
                     Write_Str (Get_Name_String (File_Name));
                     Write_Str (", Unit : ");
                     Write_Line (Get_Name_String (Unit));
                  end if;

                  declare
                     Src_Data : Source_Data := No_Source_Data;

                  begin
                     Src_Data.Project           := Project;
                     Src_Data.Language_Name     := Lang;
                     Src_Data.Language          := Lang_Id;
                     Src_Data.Kind              := Kind;
                     Src_Data.Other_Part        := Other_Part;
                     Src_Data.Unit              := Unit;
                     Src_Data.Index             := Index;
                     Src_Data.File              := File_Name;
                     Src_Data.Object            := Object_Name (File_Name);
                     Src_Data.Display_File      :=
                       File_Name_Type (Element.Value.Value);
                     Src_Data.Dependency := In_Tree.Languages_Data.Table
                       (Lang_Id).Config.Dependency_Kind;
                     Src_Data.Dep_Name          :=
                       Dependency_Name (File_Name, Src_Data.Dependency);
                     Src_Data.Switches := Switches_Name (File_Name);
                     Src_Data.Naming_Exception  := True;
                     In_Tree.Sources.Table (Source) := Src_Data;
                  end;

                  Add_Source (Source, Data, In_Tree);

                  if Source_To_Replace /= No_Source then
                     Remove_Source
                       (Source_To_Replace, Source, Project, Data, In_Tree);
                  end if;
               end if;
            end if;

            Exceptions := Element.Next;
         end loop;

      end Get_Unit_Exceptions;

   --  Start of processing for Check_Naming_Schemes

   begin
      if not In_Configuration then
         --  Look into package Naming, if there is one

         if Naming_Id /= No_Package then
            Naming := In_Tree.Packages.Table (Naming_Id);

            if Current_Verbosity = High then
               Write_Line ("Checking package Naming.");
            end if;

            --  We are now checking if attribute Dot_Replacement, Casing,
            --  and/or Separate_Suffix exist.

            --  For each attribute, if it does not exist, we do nothing,
            --  because we already have the default.
            --  Otherwise, for all unit-based languages, we put the declared
            --  value in the language config.

            declare
               Dot_Repl        : constant  Variable_Value :=
                                   Util.Value_Of
                                     (Name_Dot_Replacement,
                                      Naming.Decl.Attributes, In_Tree);
               Dot_Replacement : File_Name_Type := No_File;

               Casing_String : constant Variable_Value :=
                                 Util.Value_Of
                                   (Name_Casing,
                                    Naming.Decl.Attributes,
                                    In_Tree);
               Casing          : Casing_Type;
               Casing_Defined  : Boolean := False;

               Sep_Suffix : constant Variable_Value :=
                              Prj.Util.Value_Of
                                (Variable_Name => Name_Separate_Suffix,
                                 In_Variables  => Naming.Decl.Attributes,
                                 In_Tree       => In_Tree);
               Separate_Suffix : File_Name_Type := No_File;

               Lang_Id : Language_Index;
            begin
               --  Check attribute Dot_Replacement

               if not Dot_Repl.Default then
                  Get_Name_String (Dot_Repl.Value);

                  if Name_Len = 0 then
                     Error_Msg
                       (Project, In_Tree,
                        "Dot_Replacement cannot be empty",
                        Dot_Repl.Location);

                  else
                     Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));
                     Dot_Replacement := Name_Find;

                     if Current_Verbosity = High then
                        Write_Str  ("  Dot_Replacement = """);
                        Write_Str  (Get_Name_String (Dot_Replacement));
                        Write_Char ('"');
                        Write_Eol;
                     end if;
                  end if;
               end if;

               --  Check attribute Casing

               if not Casing_String.Default then
                  declare
                     Casing_Image : constant String :=
                                      Get_Name_String (Casing_String.Value);
                  begin
                     declare
                        Casing_Value : constant Casing_Type :=
                                         Value (Casing_Image);
                     begin
                        Casing := Casing_Value;
                        Casing_Defined := True;

                        if Current_Verbosity = High then
                           Write_Str  ("  Casing = ");
                           Write_Str  (Image (Casing));
                           Write_Char ('.');
                           Write_Eol;
                        end if;
                     end;

                  exception
                     when Constraint_Error =>
                        if Casing_Image'Length = 0 then
                           Error_Msg
                             (Project, In_Tree,
                              "Casing cannot be an empty string",
                              Casing_String.Location);

                        else
                           Name_Len := Casing_Image'Length;
                           Name_Buffer (1 .. Name_Len) := Casing_Image;
                           Err_Vars.Error_Msg_Name_1 := Name_Find;
                           Error_Msg
                             (Project, In_Tree,
                              "%% is not a correct Casing",
                              Casing_String.Location);
                        end if;
                  end;
               end if;

               if not Sep_Suffix.Default then
                  Get_Name_String (Sep_Suffix.Value);

                  if Name_Len = 0 then
                     Error_Msg
                       (Project, In_Tree,
                        "Separate_Suffix cannot be empty",
                        Sep_Suffix.Location);

                  else
                     Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));
                     Separate_Suffix := Name_Find;

                     if Current_Verbosity = High then
                        Write_Str ("  Separate_Suffix = """);
                        Write_Str
                          (Get_Name_String (Data.Naming.Separate_Suffix));
                        Write_Char ('"');
                        Write_Eol;
                     end if;
                  end if;
               end if;

               --  For all unit based languages, if any, set the specified
               --  value of Dot_Replacement, Casing and/or Separate_Suffix.

               if Dot_Replacement /= No_File or else
                 Casing_Defined or else
                 Separate_Suffix /= No_File
               then
                  Lang_Id := Data.First_Language_Processing;

                  while Lang_Id /= No_Language_Index loop
                     if In_Tree.Languages_Data.Table
                       (Lang_Id).Config.Kind = Unit_Based
                     then
                        if Dot_Replacement /= No_File then
                           In_Tree.Languages_Data.Table
                             (Lang_Id).Config.Naming_Data.Dot_Replacement :=
                             Dot_Replacement;
                        end if;

                        if Casing_Defined then
                           In_Tree.Languages_Data.Table
                             (Lang_Id).Config.Naming_Data.Casing := Casing;
                        end if;

                        if Separate_Suffix /= No_File then
                           In_Tree.Languages_Data.Table
                             (Lang_Id).Config.Naming_Data.Separate_Suffix :=
                               Separate_Suffix;
                        end if;
                     end if;

                     Lang_Id :=
                       In_Tree.Languages_Data.Table (Lang_Id).Next;
                  end loop;
               end if;
            end;

            --  Next, get the spec and body suffixes

            declare
               Suffix : Variable_Value;

               Lang_Id : Language_Index := Data.First_Language_Processing;
               Lang    : Name_Id;
            begin
               while Lang_Id /= No_Language_Index loop
                  Lang := In_Tree.Languages_Data.Table (Lang_Id).Name;

                  --  Spec_Suffix

                  Suffix := Value_Of
                    (Name                    => Lang,
                     Attribute_Or_Array_Name => Name_Spec_Suffix,
                     In_Package              => Naming_Id,
                     In_Tree                 => In_Tree);

                  if Suffix = Nil_Variable_Value then
                     Suffix := Value_Of
                       (Name                    => Lang,
                        Attribute_Or_Array_Name => Name_Specification_Suffix,
                        In_Package              => Naming_Id,
                        In_Tree                 => In_Tree);
                  end if;

                  if Suffix /= Nil_Variable_Value then
                     In_Tree.Languages_Data.Table (Lang_Id).
                       Config.Naming_Data.Spec_Suffix :=
                         File_Name_Type (Suffix.Value);
                  end if;

                  --  Body_Suffix

                  Suffix := Value_Of
                    (Name                    => Lang,
                     Attribute_Or_Array_Name => Name_Body_Suffix,
                     In_Package              => Naming_Id,
                     In_Tree                 => In_Tree);

                  if Suffix = Nil_Variable_Value then
                     Suffix := Value_Of
                       (Name                    => Lang,
                        Attribute_Or_Array_Name => Name_Implementation_Suffix,
                        In_Package              => Naming_Id,
                        In_Tree                 => In_Tree);
                  end if;

                  if Suffix /= Nil_Variable_Value then
                     In_Tree.Languages_Data.Table (Lang_Id).
                       Config.Naming_Data.Body_Suffix :=
                         File_Name_Type (Suffix.Value);
                  end if;

                  Lang_Id := In_Tree.Languages_Data.Table (Lang_Id).Next;
               end loop;
            end;

            --  Get the exceptions for file based languages

            Get_Exceptions (Spec);
            Get_Exceptions (Impl);

            --  Get the exceptions for unit based languages

            Get_Unit_Exceptions (Spec);
            Get_Unit_Exceptions (Impl);

         end if;
      end if;
   end Check_Naming_Schemes;

   ------------------------------
   -- Check_Library_Attributes --
   ------------------------------

   procedure Check_Library_Attributes
     (Project : Project_Id;
      In_Tree : Project_Tree_Ref;
      Data    : in out Project_Data)
   is
      Attributes   : constant Prj.Variable_Id := Data.Decl.Attributes;

      Lib_Dir      : constant Prj.Variable_Value :=
                       Prj.Util.Value_Of
                         (Snames.Name_Library_Dir, Attributes, In_Tree);

      Lib_Name     : constant Prj.Variable_Value :=
                       Prj.Util.Value_Of
                         (Snames.Name_Library_Name, Attributes, In_Tree);

      Lib_Version  : constant Prj.Variable_Value :=
                       Prj.Util.Value_Of
                         (Snames.Name_Library_Version, Attributes, In_Tree);

      Lib_ALI_Dir  : constant Prj.Variable_Value :=
                       Prj.Util.Value_Of
                         (Snames.Name_Library_Ali_Dir, Attributes, In_Tree);

      The_Lib_Kind : constant Prj.Variable_Value :=
                       Prj.Util.Value_Of
                         (Snames.Name_Library_Kind, Attributes, In_Tree);

      Imported_Project_List : Project_List := Empty_Project_List;

      Continuation : String_Access := No_Continuation_String'Access;

      Support_For_Libraries : Library_Support;

      procedure Check_Library (Proj : Project_Id; Extends : Boolean);
      --  Check if an imported or extended project if also a library project

      -------------------
      -- Check_Library --
      -------------------

      procedure Check_Library (Proj : Project_Id; Extends : Boolean) is
         Proj_Data : Project_Data;

      begin
         if Proj /= No_Project then
            Proj_Data := In_Tree.Projects.Table (Proj);

            if not Proj_Data.Library then
               --  The only not library projects that are OK are those that
               --  have no sources.

               if Proj_Data.Source_Dirs /= Nil_String then

                  Error_Msg_Name_1 := Data.Name;
                  Error_Msg_Name_2 := Proj_Data.Name;

                  if Extends then
                     Error_Msg
                       (Project, In_Tree,
                        Continuation.all &
                        "library project %% cannot extend project %% " &
                        "that is not a library project",
                        Data.Location);

                  else
                     Error_Msg
                       (Project, In_Tree,
                        Continuation.all &
                        "library project %% cannot import project %% " &
                        "that is not a library project",
                        Data.Location);
                  end if;

                  Continuation := Continuation_String'Access;
               end if;

            elsif Data.Library_Kind /= Static and then
                  Proj_Data.Library_Kind = Static
            then
               Error_Msg_Name_1 := Data.Name;
               Error_Msg_Name_2 := Proj_Data.Name;

               if Extends then
                  Error_Msg
                    (Project, In_Tree,
                     Continuation.all &
                     "shared library project %% cannot extend static " &
                     "library project %%",
                     Data.Location);

               else
                  Error_Msg
                    (Project, In_Tree,
                     Continuation.all &
                     "shared library project %% cannot import static " &
                     "library project %%",
                     Data.Location);
               end if;

               Continuation := Continuation_String'Access;
            end if;
         end if;
      end Check_Library;

   begin
      --  Special case of extending project

      if Data.Extends /= No_Project then
         declare
            Extended_Data : constant Project_Data :=
                           In_Tree.Projects.Table (Data.Extends);

         begin
            --  If the project extended is a library project, we inherit
            --  the library name, if it is not redefined; we check that
            --  the library directory is specified.

            if Extended_Data.Library then
               if Lib_Name.Default then
                  Data.Library_Name := Extended_Data.Library_Name;
               end if;

               if Lib_Dir.Default then
                  if not Data.Virtual then
                     Error_Msg
                       (Project, In_Tree,
                        "a project extending a library project must " &
                        "specify an attribute Library_Dir",
                        Data.Location);
                  end if;
               end if;
            end if;
         end;
      end if;

      pragma Assert (Lib_Dir.Kind = Single);

      if Lib_Dir.Value = Empty_String then
         if Current_Verbosity = High then
            Write_Line ("No library directory");
         end if;

      else
         --  Find path name, check that it is a directory

         Locate_Directory
           (Project,
            In_Tree,
            File_Name_Type (Lib_Dir.Value),
            Data.Display_Directory,
            Data.Library_Dir,
            Data.Display_Library_Dir,
            Create => "library",
            Location => Lib_Dir.Location);

         if Data.Library_Dir = No_Path then

            --  Get the absolute name of the library directory that
            --  does not exist, to report an error.

            declare
               Dir_Name : constant String := Get_Name_String (Lib_Dir.Value);

            begin
               if Is_Absolute_Path (Dir_Name) then
                  Err_Vars.Error_Msg_File_1 := File_Name_Type (Lib_Dir.Value);

               else
                  Get_Name_String (Data.Display_Directory);

                  if Name_Buffer (Name_Len) /= Directory_Separator then
                     Name_Len := Name_Len + 1;
                     Name_Buffer (Name_Len) := Directory_Separator;
                  end if;

                  Name_Buffer
                    (Name_Len + 1 .. Name_Len + Dir_Name'Length) :=
                    Dir_Name;
                  Name_Len := Name_Len + Dir_Name'Length;
                  Err_Vars.Error_Msg_File_1 := Name_Find;
               end if;

               --  Report the error

               Error_Msg
                 (Project, In_Tree,
                  "library directory { does not exist",
                  Lib_Dir.Location);
            end;

         --  The library directory cannot be the same as the Object directory

         elsif Data.Library_Dir = Data.Object_Directory then
            Error_Msg
              (Project, In_Tree,
               "library directory cannot be the same " &
               "as object directory",
               Lib_Dir.Location);
            Data.Library_Dir := No_Path;
            Data.Display_Library_Dir := No_Path;

         else
            declare
               OK       : Boolean := True;
               Dirs_Id  : String_List_Id;
               Dir_Elem : String_Element;

            begin
               --  The library directory cannot be the same as a source
               --  directory of the current project.

               Dirs_Id := Data.Source_Dirs;
               while Dirs_Id /= Nil_String loop
                  Dir_Elem := In_Tree.String_Elements.Table (Dirs_Id);
                  Dirs_Id  := Dir_Elem.Next;

                  if Data.Library_Dir = Path_Name_Type (Dir_Elem.Value) then
                     Err_Vars.Error_Msg_File_1 :=
                       File_Name_Type (Dir_Elem.Value);
                     Error_Msg
                       (Project, In_Tree,
                        "library directory cannot be the same " &
                        "as source directory {",
                        Lib_Dir.Location);
                     OK := False;
                     exit;
                  end if;
               end loop;

               if OK then

                  --  The library directory cannot be the same as a source
                  --  directory of another project either.

                  Project_Loop :
                  for Pid in 1 .. Project_Table.Last (In_Tree.Projects) loop
                     if Pid /= Project then
                        Dirs_Id := In_Tree.Projects.Table (Pid).Source_Dirs;

                        Dir_Loop : while Dirs_Id /= Nil_String loop
                           Dir_Elem := In_Tree.String_Elements.Table (Dirs_Id);
                           Dirs_Id  := Dir_Elem.Next;

                           if Data.Library_Dir =
                             Path_Name_Type (Dir_Elem.Value)
                           then
                              Err_Vars.Error_Msg_File_1 :=
                                File_Name_Type (Dir_Elem.Value);
                              Err_Vars.Error_Msg_Name_1 :=
                                In_Tree.Projects.Table (Pid).Name;

                              Error_Msg
                                (Project, In_Tree,
                                 "library directory cannot be the same " &
                                 "as source directory { of project %%",
                                 Lib_Dir.Location);
                              OK := False;
                              exit Project_Loop;
                           end if;
                        end loop Dir_Loop;
                     end if;
                  end loop Project_Loop;
               end if;

               if not OK then
                  Data.Library_Dir := No_Path;
                  Data.Display_Library_Dir := No_Path;

               elsif Current_Verbosity = High then

                  --  Display the Library directory in high verbosity

                  Write_Str ("Library directory =""");
                  Write_Str (Get_Name_String (Data.Display_Library_Dir));
                  Write_Line ("""");
               end if;
            end;
         end if;
      end if;

      pragma Assert (Lib_Name.Kind = Single);

      if Lib_Name.Value = Empty_String then
         if Current_Verbosity = High
           and then Data.Library_Name = No_Name
         then
            Write_Line ("No library name");
         end if;

      else
         --  There is no restriction on the syntax of library names

         Data.Library_Name := Lib_Name.Value;
      end if;

      if Data.Library_Name /= No_Name
        and then Current_Verbosity = High
      then
         Write_Str ("Library name = """);
         Write_Str (Get_Name_String (Data.Library_Name));
         Write_Line ("""");
      end if;

      Data.Library :=
        Data.Library_Dir /= No_Path
        and then
      Data.Library_Name /= No_Name;

      if Data.Library then
         Support_For_Libraries := In_Tree.Lib_Support;

         if Support_For_Libraries = Prj.None then
            Error_Msg
              (Project, In_Tree,
               "?libraries are not supported on this platform",
               Lib_Name.Location);
            Data.Library := False;

         else
            if Lib_ALI_Dir.Value = Empty_String then
               if Current_Verbosity = High then
                  Write_Line ("No library 'A'L'I directory specified");
               end if;
               Data.Library_ALI_Dir := Data.Library_Dir;
               Data.Display_Library_ALI_Dir := Data.Display_Library_Dir;

            else
               --  Find path name, check that it is a directory

               Locate_Directory
                 (Project,
                  In_Tree,
                  File_Name_Type (Lib_ALI_Dir.Value),
                  Data.Display_Directory,
                  Data.Library_ALI_Dir,
                  Data.Display_Library_ALI_Dir,
                  Create   => "library ALI",
                  Location => Lib_ALI_Dir.Location);

               if Data.Library_ALI_Dir = No_Path then

                  --  Get the absolute name of the library ALI directory that
                  --  does not exist, to report an error.

                  declare
                     Dir_Name : constant String :=
                                  Get_Name_String (Lib_ALI_Dir.Value);

                  begin
                     if Is_Absolute_Path (Dir_Name) then
                        Err_Vars.Error_Msg_File_1 :=
                          File_Name_Type (Lib_Dir.Value);

                     else
                        Get_Name_String (Data.Display_Directory);

                        if Name_Buffer (Name_Len) /= Directory_Separator then
                           Name_Len := Name_Len + 1;
                           Name_Buffer (Name_Len) := Directory_Separator;
                        end if;

                        Name_Buffer
                          (Name_Len + 1 .. Name_Len + Dir_Name'Length) :=
                          Dir_Name;
                        Name_Len := Name_Len + Dir_Name'Length;
                        Err_Vars.Error_Msg_File_1 := Name_Find;
                     end if;

                     --  Report the error

                     Error_Msg
                       (Project, In_Tree,
                        "library 'A'L'I directory { does not exist",
                        Lib_ALI_Dir.Location);
                  end;
               end if;

               if Data.Library_ALI_Dir /= Data.Library_Dir then

                  --  The library ALI directory cannot be the same as the
                  --  Object directory.

                  if Data.Library_ALI_Dir = Data.Object_Directory then
                     Error_Msg
                       (Project, In_Tree,
                        "library 'A'L'I directory cannot be the same " &
                        "as object directory",
                        Lib_ALI_Dir.Location);
                     Data.Library_ALI_Dir := No_Path;
                     Data.Display_Library_ALI_Dir := No_Path;

                  else
                     declare
                        OK       : Boolean := True;
                        Dirs_Id  : String_List_Id;
                        Dir_Elem : String_Element;

                     begin
                        --  The library ALI directory cannot be the same as
                        --  a source directory of the current project.

                        Dirs_Id := Data.Source_Dirs;
                        while Dirs_Id /= Nil_String loop
                           Dir_Elem := In_Tree.String_Elements.Table (Dirs_Id);
                           Dirs_Id  := Dir_Elem.Next;

                           if Data.Library_ALI_Dir =
                             Path_Name_Type (Dir_Elem.Value)
                           then
                              Err_Vars.Error_Msg_File_1 :=
                                File_Name_Type (Dir_Elem.Value);
                              Error_Msg
                                (Project, In_Tree,
                                 "library 'A'L'I directory cannot be " &
                                 "the same as source directory {",
                                 Lib_ALI_Dir.Location);
                              OK := False;
                              exit;
                           end if;
                        end loop;

                        if OK then

                           --  The library ALI directory cannot be the same as
                           --  a source directory of another project either.

                           ALI_Project_Loop :
                           for
                             Pid in 1 .. Project_Table.Last (In_Tree.Projects)
                           loop
                              if Pid /= Project then
                                 Dirs_Id :=
                                   In_Tree.Projects.Table (Pid).Source_Dirs;

                                 ALI_Dir_Loop :
                                 while Dirs_Id /= Nil_String loop
                                    Dir_Elem :=
                                      In_Tree.String_Elements.Table (Dirs_Id);
                                    Dirs_Id  := Dir_Elem.Next;

                                    if Data.Library_ALI_Dir =
                                        Path_Name_Type (Dir_Elem.Value)
                                    then
                                       Err_Vars.Error_Msg_File_1 :=
                                         File_Name_Type (Dir_Elem.Value);
                                       Err_Vars.Error_Msg_Name_1 :=
                                         In_Tree.Projects.Table (Pid).Name;

                                       Error_Msg
                                         (Project, In_Tree,
                                          "library 'A'L'I directory cannot " &
                                          "be the same as source directory " &
                                          "{ of project %%",
                                          Lib_ALI_Dir.Location);
                                       OK := False;
                                       exit ALI_Project_Loop;
                                    end if;
                                 end loop ALI_Dir_Loop;
                              end if;
                           end loop ALI_Project_Loop;
                        end if;

                        if not OK then
                           Data.Library_ALI_Dir := No_Path;
                           Data.Display_Library_ALI_Dir := No_Path;

                        elsif Current_Verbosity = High then

                           --  Display the Library ALI directory in high
                           --  verbosity.

                           Write_Str ("Library ALI directory =""");
                           Write_Str
                             (Get_Name_String (Data.Display_Library_ALI_Dir));
                           Write_Line ("""");
                        end if;
                     end;
                  end if;
               end if;
            end if;

            pragma Assert (Lib_Version.Kind = Single);

            if Lib_Version.Value = Empty_String then
               if Current_Verbosity = High then
                  Write_Line ("No library version specified");
               end if;

            else
               Data.Lib_Internal_Name := Lib_Version.Value;
            end if;

            pragma Assert (The_Lib_Kind.Kind = Single);

            if The_Lib_Kind.Value = Empty_String then
               if Current_Verbosity = High then
                  Write_Line ("No library kind specified");
               end if;

            else
               Get_Name_String (The_Lib_Kind.Value);

               declare
                  Kind_Name : constant String :=
                                To_Lower (Name_Buffer (1 .. Name_Len));

                  OK : Boolean := True;

               begin
                  if Kind_Name = "static" then
                     Data.Library_Kind := Static;

                  elsif Kind_Name = "dynamic" then
                     Data.Library_Kind := Dynamic;

                  elsif Kind_Name = "relocatable" then
                     Data.Library_Kind := Relocatable;

                  else
                     Error_Msg
                       (Project, In_Tree,
                        "illegal value for Library_Kind",
                        The_Lib_Kind.Location);
                     OK := False;
                  end if;

                  if Current_Verbosity = High and then OK then
                     Write_Str ("Library kind = ");
                     Write_Line (Kind_Name);
                  end if;

                  if Data.Library_Kind /= Static and then
                    Support_For_Libraries = Prj.Static_Only
                  then
                     Error_Msg
                       (Project, In_Tree,
                        "only static libraries are supported " &
                        "on this platform",
                        The_Lib_Kind.Location);
                     Data.Library := False;
                  end if;
               end;
            end if;

            if Data.Library then
               if Current_Verbosity = High then
                  Write_Line ("This is a library project file");
               end if;

               Check_Library (Data.Extends, Extends => True);

               Imported_Project_List := Data.Imported_Projects;
               while Imported_Project_List /= Empty_Project_List loop
                  Check_Library
                    (In_Tree.Project_Lists.Table
                       (Imported_Project_List).Project,
                     Extends => False);
                  Imported_Project_List :=
                    In_Tree.Project_Lists.Table
                      (Imported_Project_List).Next;
               end loop;
            end if;

         end if;
      end if;

      if Data.Extends /= No_Project then
         In_Tree.Projects.Table (Data.Extends).Library := False;
      end if;
   end Check_Library_Attributes;

   --------------------------
   -- Check_Package_Naming --
   --------------------------

   procedure Check_Package_Naming
     (Project : Project_Id;
      In_Tree : Project_Tree_Ref;
      Data    : in out Project_Data)
   is
      Naming_Id : constant Package_Id :=
                    Util.Value_Of (Name_Naming, Data.Decl.Packages, In_Tree);

      Naming    : Package_Element;

   begin
      --  If there is a package Naming, we will put in Data.Naming
      --  what is in this package Naming.

      if Naming_Id /= No_Package then
         Naming := In_Tree.Packages.Table (Naming_Id);

         if Current_Verbosity = High then
            Write_Line ("Checking ""Naming"".");
         end if;

         --  Check Spec_Suffix

         declare
            Spec_Suffixs : Array_Element_Id :=
                             Util.Value_Of
                               (Name_Spec_Suffix,
                                Naming.Decl.Arrays,
                                In_Tree);

            Suffix  : Array_Element_Id;
            Element : Array_Element;
            Suffix2 : Array_Element_Id;

         begin
            --  If some suffixs have been specified, we make sure that
            --  for each language for which a default suffix has been
            --  specified, there is a suffix specified, either the one
            --  in the project file or if there were none, the default.

            if Spec_Suffixs /= No_Array_Element then
               Suffix := Data.Naming.Spec_Suffix;

               while Suffix /= No_Array_Element loop
                  Element :=
                    In_Tree.Array_Elements.Table (Suffix);
                  Suffix2 := Spec_Suffixs;

                  while Suffix2 /= No_Array_Element loop
                     exit when In_Tree.Array_Elements.Table
                                (Suffix2).Index = Element.Index;
                     Suffix2 := In_Tree.Array_Elements.Table
                                 (Suffix2).Next;
                  end loop;

                  --  There is a registered default suffix, but no
                  --  suffix specified in the project file.
                  --  Add the default to the array.

                  if Suffix2 = No_Array_Element then
                     Array_Element_Table.Increment_Last
                       (In_Tree.Array_Elements);
                     In_Tree.Array_Elements.Table
                       (Array_Element_Table.Last
                          (In_Tree.Array_Elements)) :=
                       (Index                => Element.Index,
                        Src_Index            => Element.Src_Index,
                        Index_Case_Sensitive => False,
                        Value                => Element.Value,
                        Next                 => Spec_Suffixs);
                     Spec_Suffixs := Array_Element_Table.Last
                                       (In_Tree.Array_Elements);
                  end if;

                  Suffix := Element.Next;
               end loop;

               --  Put the resulting array as the specification suffixs

               Data.Naming.Spec_Suffix := Spec_Suffixs;
            end if;
         end;

         declare
            Current : Array_Element_Id := Data.Naming.Spec_Suffix;
            Element : Array_Element;

         begin
            while Current /= No_Array_Element loop
               Element := In_Tree.Array_Elements.Table (Current);
               Get_Name_String (Element.Value.Value);

               if Name_Len = 0 then
                  Error_Msg
                    (Project, In_Tree,
                     "Spec_Suffix cannot be empty",
                     Element.Value.Location);
               end if;

               In_Tree.Array_Elements.Table (Current) := Element;
               Current := Element.Next;
            end loop;
         end;

         --  Check Body_Suffix

         declare
            Impl_Suffixs : Array_Element_Id :=
              Util.Value_Of
                (Name_Body_Suffix,
                 Naming.Decl.Arrays,
                 In_Tree);

            Suffix       : Array_Element_Id;
            Element      : Array_Element;
            Suffix2      : Array_Element_Id;

         begin
            --  If some suffixes have been specified, we make sure that
            --  for each language for which a default suffix has been
            --  specified, there is a suffix specified, either the one
            --  in the project file or if there were noe, the default.

            if Impl_Suffixs /= No_Array_Element then
               Suffix := Data.Naming.Body_Suffix;

               while Suffix /= No_Array_Element loop
                  Element :=
                    In_Tree.Array_Elements.Table (Suffix);
                  Suffix2 := Impl_Suffixs;

                  while Suffix2 /= No_Array_Element loop
                     exit when In_Tree.Array_Elements.Table
                                (Suffix2).Index = Element.Index;
                     Suffix2 := In_Tree.Array_Elements.Table
                                  (Suffix2).Next;
                  end loop;

                  --  There is a registered default suffix, but no suffix was
                  --  specified in the project file. Add the default to the
                  --  array.

                  if Suffix2 = No_Array_Element then
                     Array_Element_Table.Increment_Last
                       (In_Tree.Array_Elements);
                     In_Tree.Array_Elements.Table
                       (Array_Element_Table.Last
                          (In_Tree.Array_Elements)) :=
                       (Index                => Element.Index,
                        Src_Index            => Element.Src_Index,
                        Index_Case_Sensitive => False,
                        Value                => Element.Value,
                        Next                 => Impl_Suffixs);
                     Impl_Suffixs := Array_Element_Table.Last
                                       (In_Tree.Array_Elements);
                  end if;

                  Suffix := Element.Next;
               end loop;

               --  Put the resulting array as the implementation suffixs

               Data.Naming.Body_Suffix := Impl_Suffixs;
            end if;
         end;

         declare
            Current : Array_Element_Id := Data.Naming.Body_Suffix;
            Element : Array_Element;

         begin
            while Current /= No_Array_Element loop
               Element := In_Tree.Array_Elements.Table (Current);
               Get_Name_String (Element.Value.Value);

               if Name_Len = 0 then
                  Error_Msg
                    (Project, In_Tree,
                     "Body_Suffix cannot be empty",
                     Element.Value.Location);
               end if;

               In_Tree.Array_Elements.Table (Current) := Element;
               Current := Element.Next;
            end loop;
         end;

         --  Get the exceptions, if any

         Data.Naming.Specification_Exceptions :=
           Util.Value_Of
             (Name_Specification_Exceptions,
              In_Arrays => Naming.Decl.Arrays,
              In_Tree   => In_Tree);

         Data.Naming.Implementation_Exceptions :=
           Util.Value_Of
             (Name_Implementation_Exceptions,
              In_Arrays => Naming.Decl.Arrays,
              In_Tree   => In_Tree);
      end if;
   end Check_Package_Naming;

   ---------------------------------
   -- Check_Programming_Languages --
   ---------------------------------

   procedure Check_Programming_Languages
     (In_Tree       : Project_Tree_Ref;
      Project       : Project_Id;
      Data          : in out Project_Data)
   is
      Languages : Variable_Value := Nil_Variable_Value;
      Lang      : Language_Index;

   begin
      Languages :=
        Prj.Util.Value_Of (Name_Languages, Data.Decl.Attributes, In_Tree);

      if Data.Source_Dirs /= Nil_String then

         --  Check if languages are specified in this project

         if Languages.Default then

            --  Attribute Languages is not specified. So, it defaults to
            --  a project of the default language only.

            Name_List_Table.Increment_Last (In_Tree.Name_Lists);
            Data.Languages := Name_List_Table.Last (In_Tree.Name_Lists);

            if In_Tree.Default_Language = No_Name then
               Error_Msg
                 (Project,
                  In_Tree,
                  "no languages defined for this project",
                  Data.Location);

            else
               In_Tree.Name_Lists.Table (Data.Languages) :=
                 (Name => In_Tree.Default_Language, Next => No_Name_List);
               Language_Data_Table.Increment_Last (In_Tree.Languages_Data);
               Data.First_Language_Processing :=
                 Language_Data_Table.Last (In_Tree.Languages_Data);
               In_Tree.Languages_Data.Table
                 (Data.First_Language_Processing) := No_Language_Data;
               In_Tree.Languages_Data.Table
                 (Data.First_Language_Processing).Name :=
                 In_Tree.Default_Language;
               Get_Name_String (In_Tree.Default_Language);
               Name_Buffer (1) := GNAT.Case_Util.To_Upper (Name_Buffer (1));
               In_Tree.Languages_Data.Table
                 (Data.First_Language_Processing).Display_Name := Name_Find;

               Lang := In_Tree.First_Language;

               while Lang /= No_Language_Index loop
                  if In_Tree.Languages_Data.Table (Lang).Name =
                    In_Tree.Default_Language
                  then
                     In_Tree.Languages_Data.Table
                       (Data.First_Language_Processing).Config :=
                       In_Tree.Languages_Data.Table (Lang).Config;

                     if In_Tree.Languages_Data.Table (Lang).Config.Kind =
                       Unit_Based
                     then
                        Data.Unit_Based_Language_Name :=
                          In_Tree.Default_Language;
                        Data.Unit_Based_Language_Index :=
                          Data.First_Language_Processing;
                     end if;

                     exit;
                  end if;

                  Lang := In_Tree.Languages_Data.Table (Lang).Next;
               end loop;
            end if;

         else
            declare
               Current           : String_List_Id := Languages.Values;
               Element           : String_Element;
               Lang_Name         : Name_Id;
               Display_Lang_Name : Name_Id;
               Index             : Language_Index;
               Lang_Data         : Language_Data;
               NL_Id             : Name_List_Index := No_Name_List;
               Config            : Language_Config;

            begin
               if Current = Nil_String then
                  Data.Source_Dirs := Nil_String;

               else
                  --  Look through all the languages specified in attribute
                  --  Languages, if any.

                  while Current /= Nil_String loop
                     Element :=
                       In_Tree.String_Elements.Table (Current);
                     Display_Lang_Name := Element.Value;
                     Get_Name_String (Element.Value);
                     To_Lower (Name_Buffer (1 .. Name_Len));
                     Lang_Name := Name_Find;

                     Name_List_Table.Increment_Last (In_Tree.Name_Lists);

                     if NL_Id = No_Name_List then
                        Data.Languages :=
                          Name_List_Table.Last (In_Tree.Name_Lists);

                     else
                        In_Tree.Name_Lists.Table (NL_Id).Next :=
                          Name_List_Table.Last (In_Tree.Name_Lists);
                     end if;

                     NL_Id := Name_List_Table.Last (In_Tree.Name_Lists);
                     In_Tree.Name_Lists.Table (NL_Id) :=
                       (Lang_Name, No_Name_List);

                     Index := Data.First_Language_Processing;

                     while Index /= No_Language_Index loop
                        exit when
                          Lang_Name =
                            In_Tree.Languages_Data.Table (Index).Name;
                        Index := In_Tree.Languages_Data.Table (Index).Next;
                     end loop;

                     if Index = No_Language_Index then
                        Language_Data_Table.Increment_Last
                          (In_Tree.Languages_Data);
                        Index :=
                          Language_Data_Table.Last (In_Tree.Languages_Data);
                        Lang_Data.Name := Lang_Name;
                        Lang_Data.Display_Name := Element.Value;
                        Lang_Data.Next := Data.First_Language_Processing;
                        In_Tree.Languages_Data.Table (Index) := Lang_Data;
                        Data.First_Language_Processing := Index;

                        Index := In_Tree.First_Language;

                        while Index /= No_Language_Index loop
                           exit when
                             Lang_Name =
                               In_Tree.Languages_Data.Table (Index).Name;
                           Index :=
                             In_Tree.Languages_Data.Table (Index).Next;
                        end loop;

                        if Index = No_Language_Index then
                           Error_Msg
                             (Project, In_Tree,
                              "language """ &
                              Get_Name_String (Display_Lang_Name) &
                              """ not found in configuration",
                              Languages.Location);

                        else
                           Config :=
                             In_Tree.Languages_Data.Table (Index).Config;
                           In_Tree.Languages_Data.Table
                             (Data.First_Language_Processing).Config :=
                             Config;

                           if Config.Kind = Unit_Based then
                              if
                                Data.Unit_Based_Language_Name = No_Name
                              then
                                 Data.Unit_Based_Language_Name := Lang_Name;
                                 Data.Unit_Based_Language_Index :=
                                   Language_Data_Table.Last
                                     (In_Tree.Languages_Data);

                              else
                                 Error_Msg
                                   (Project, In_Tree,
                                    "not allowed to have several " &
                                    "unit-based languages in the same " &
                                    "project",
                                    Languages.Location);
                              end if;
                           end if;
                        end if;
                     end if;

                     Current := Element.Next;
                  end loop;
               end if;
            end;
         end if;
      end if;
   end Check_Programming_Languages;

   -------------------------------
   -- Check_Stand_Alone_Library --
   -------------------------------

   procedure Check_Stand_Alone_Library
     (Project   : Project_Id;
      In_Tree   : Project_Tree_Ref;
      Data      : in out Project_Data;
      Extending : Boolean)
   is
      pragma Unreferenced (Extending);

      Lib_Interfaces      : constant Prj.Variable_Value :=
                              Prj.Util.Value_Of
                                (Snames.Name_Library_Interface,
                                 Data.Decl.Attributes,
                                 In_Tree);

      Lib_Auto_Init       : constant Prj.Variable_Value :=
                              Prj.Util.Value_Of
                                (Snames.Name_Library_Auto_Init,
                                 Data.Decl.Attributes,
                                 In_Tree);

      Lib_Src_Dir         : constant Prj.Variable_Value :=
                              Prj.Util.Value_Of
                                (Snames.Name_Library_Src_Dir,
                                 Data.Decl.Attributes,
                                 In_Tree);

      Lib_Symbol_File     : constant Prj.Variable_Value :=
                              Prj.Util.Value_Of
                                (Snames.Name_Library_Symbol_File,
                                 Data.Decl.Attributes,
                                 In_Tree);

      Lib_Symbol_Policy   : constant Prj.Variable_Value :=
                              Prj.Util.Value_Of
                                (Snames.Name_Library_Symbol_Policy,
                                 Data.Decl.Attributes,
                                 In_Tree);

      Lib_Ref_Symbol_File : constant Prj.Variable_Value :=
                              Prj.Util.Value_Of
                                (Snames.Name_Library_Reference_Symbol_File,
                                 Data.Decl.Attributes,
                                 In_Tree);

      Auto_Init_Supported : Boolean;

      OK                  : Boolean := True;

      Source              : Source_Id;
      Next_Proj           : Project_Id;

   begin
      Auto_Init_Supported := In_Tree.Auto_Init_Supported;

      pragma Assert (Lib_Interfaces.Kind = List);

      --  It is a stand-alone library project file if attribute
      --  Library_Interface is defined.

      if not Lib_Interfaces.Default then
         SAL_Library : declare
            Interfaces     : String_List_Id := Lib_Interfaces.Values;
            Interface_ALIs : String_List_Id := Nil_String;
            Unit           : Name_Id;

         begin
            Data.Standalone_Library := True;

            --  Library_Interface cannot be an empty list

            if Interfaces = Nil_String then
               Error_Msg
                 (Project, In_Tree,
                  "Library_Interface cannot be an empty list",
                  Lib_Interfaces.Location);
            end if;

            --  Process each unit name specified in the attribute
            --  Library_Interface.

            while Interfaces /= Nil_String loop
               Get_Name_String
                 (In_Tree.String_Elements.Table (Interfaces).Value);
               To_Lower (Name_Buffer (1 .. Name_Len));

               if Name_Len = 0 then
                  Error_Msg
                    (Project, In_Tree,
                     "an interface cannot be an empty string",
                     In_Tree.String_Elements.Table (Interfaces).Location);

               else
                  Unit := Name_Find;
                  Error_Msg_Name_1 := Unit;

                  Next_Proj := Data.Extends;
                  Source := Data.First_Source;

                  loop
                     while Source /= No_Source and then
                           In_Tree.Sources.Table (Source).Unit /= Unit
                     loop
                        Source :=
                          In_Tree.Sources.Table (Source).Next_In_Project;
                     end loop;

                     exit when Source /= No_Source or else
                               Next_Proj = No_Project;

                     Source :=
                       In_Tree.Projects.Table (Next_Proj).First_Source;
                     Next_Proj :=
                       In_Tree.Projects.Table (Next_Proj).Extends;
                  end loop;

                  if Source /= No_Source then
                     if In_Tree.Sources.Table (Source).Kind = Sep then
                        Source := No_Source;

                     elsif In_Tree.Sources.Table (Source).Kind = Spec
                       and then
                       In_Tree.Sources.Table (Source).Other_Part /=
                       No_Source
                     then
                        Source := In_Tree.Sources.Table (Source).Other_Part;
                     end if;
                  end if;

                  if Source /= No_Source then
                     if In_Tree.Sources.Table (Source).Project /= Project
                       and then
                         not Is_Extending
                           (Project,
                            In_Tree.Sources.Table (Source).Project,
                            In_Tree)
                     then
                        Source := No_Source;
                     end if;
                  end if;

                  if Source = No_Source then
                        Error_Msg
                          (Project, In_Tree,
                           "%% is not an unit of this project",
                           In_Tree.String_Elements.Table
                             (Interfaces).Location);

                  else
                     if In_Tree.Sources.Table (Source).Kind = Spec and then
                       In_Tree.Sources.Table (Source).Other_Part /=
                         No_Source
                     then
                        Source :=
                          In_Tree.Sources.Table (Source).Other_Part;
                     end if;

                     String_Element_Table.Increment_Last
                       (In_Tree.String_Elements);
                     In_Tree.String_Elements.Table
                       (String_Element_Table.Last
                          (In_Tree.String_Elements)) :=
                       (Value         =>
                          Name_Id (In_Tree.Sources.Table (Source).Dep_Name),
                        Index         => 0,
                        Display_Value =>
                          Name_Id (In_Tree.Sources.Table (Source).Dep_Name),
                        Location      =>
                          In_Tree.String_Elements.Table
                            (Interfaces).Location,
                        Flag          => False,
                        Next          => Interface_ALIs);
                     Interface_ALIs := String_Element_Table.Last
                       (In_Tree.String_Elements);
                  end if;

               end if;

               Interfaces :=
                 In_Tree.String_Elements.Table (Interfaces).Next;
            end loop;

            --  Put the list of Interface ALIs in the project data

            Data.Lib_Interface_ALIs := Interface_ALIs;

            --  Check value of attribute Library_Auto_Init and set
            --  Lib_Auto_Init accordingly.

            if Lib_Auto_Init.Default then

               --  If no attribute Library_Auto_Init is declared, then
               --  set auto init only if it is supported.

               Data.Lib_Auto_Init := Auto_Init_Supported;

            else
               Get_Name_String (Lib_Auto_Init.Value);
               To_Lower (Name_Buffer (1 .. Name_Len));

               if Name_Buffer (1 .. Name_Len) = "false" then
                  Data.Lib_Auto_Init := False;

               elsif Name_Buffer (1 .. Name_Len) = "true" then
                  if Auto_Init_Supported then
                     Data.Lib_Auto_Init := True;

                  else
                     --  Library_Auto_Init cannot be "true" if auto init
                     --  is not supported

                     Error_Msg
                       (Project, In_Tree,
                        "library auto init not supported " &
                        "on this platform",
                        Lib_Auto_Init.Location);
                  end if;

               else
                  Error_Msg
                    (Project, In_Tree,
                     "invalid value for attribute Library_Auto_Init",
                     Lib_Auto_Init.Location);
               end if;
            end if;
         end SAL_Library;

         --  If attribute Library_Src_Dir is defined and not the
         --  empty string, check if the directory exist and is not
         --  the object directory or one of the source directories.
         --  This is the directory where copies of the interface
         --  sources will be copied. Note that this directory may be
         --  the library directory.

         if Lib_Src_Dir.Value /= Empty_String then
            declare
               Dir_Id : constant File_Name_Type :=
                          File_Name_Type (Lib_Src_Dir.Value);

            begin
               Locate_Directory
                 (Project,
                  In_Tree,
                  Dir_Id,
                  Data.Display_Directory,
                  Data.Library_Src_Dir,
                  Data.Display_Library_Src_Dir,
                  Create => "library source copy",
                  Location => Lib_Src_Dir.Location);

               --  If directory does not exist, report an error

               if Data.Library_Src_Dir = No_Path then

                  --  Get the absolute name of the library directory
                  --  that does not exist, to report an error.

                  declare
                     Dir_Name : constant String :=
                       Get_Name_String (Dir_Id);

                  begin
                     if Is_Absolute_Path (Dir_Name) then
                        Err_Vars.Error_Msg_File_1 := Dir_Id;

                     else
                        Get_Name_String (Data.Directory);

                        if Name_Buffer (Name_Len) /=
                          Directory_Separator
                        then
                           Name_Len := Name_Len + 1;
                           Name_Buffer (Name_Len) :=
                             Directory_Separator;
                        end if;

                        Name_Buffer
                          (Name_Len + 1 ..
                             Name_Len + Dir_Name'Length) :=
                            Dir_Name;
                        Name_Len := Name_Len + Dir_Name'Length;
                        Err_Vars.Error_Msg_Name_1 := Name_Find;
                     end if;

                     --  Report the error

                     Error_Msg
                       (Project, In_Tree,
                        "Directory { does not exist",
                        Lib_Src_Dir.Location);
                  end;

                  --  Report an error if it is the same as the object
                  --  directory.

               elsif Data.Library_Src_Dir = Data.Object_Directory then
                  Error_Msg
                    (Project, In_Tree,
                     "directory to copy interfaces cannot be " &
                     "the object directory",
                     Lib_Src_Dir.Location);
                  Data.Library_Src_Dir := No_Path;

               else
                  declare
                     Src_Dirs : String_List_Id;
                     Src_Dir  : String_Element;

                  begin
                     --  Interface copy directory cannot be one of the source
                     --  directory of the current project.

                     Src_Dirs := Data.Source_Dirs;
                     while Src_Dirs /= Nil_String loop
                        Src_Dir := In_Tree.String_Elements.Table
                                                          (Src_Dirs);

                        --  Report error if it is one of the source directories

                        if Data.Library_Src_Dir =
                          Path_Name_Type (Src_Dir.Value)
                        then
                           Error_Msg
                             (Project, In_Tree,
                              "directory to copy interfaces cannot " &
                              "be one of the source directories",
                              Lib_Src_Dir.Location);
                           Data.Library_Src_Dir := No_Path;
                           exit;
                        end if;

                        Src_Dirs := Src_Dir.Next;
                     end loop;

                     if Data.Library_Src_Dir /= No_Path then

                        --  It cannot be a source directory of any other
                        --  project either.

                        Project_Loop : for Pid in 1 ..
                          Project_Table.Last (In_Tree.Projects)
                        loop
                           Src_Dirs :=
                             In_Tree.Projects.Table (Pid).Source_Dirs;
                           Dir_Loop : while Src_Dirs /= Nil_String loop
                              Src_Dir :=
                                In_Tree.String_Elements.Table (Src_Dirs);

                              --  Report error if it is one of the source
                              --  directories

                              if Data.Library_Src_Dir =
                                Path_Name_Type (Src_Dir.Value)
                              then
                                 Error_Msg_File_1 :=
                                   File_Name_Type (Src_Dir.Value);
                                 Error_Msg_Name_1 :=
                                   In_Tree.Projects.Table (Pid).Name;
                                 Error_Msg
                                   (Project, In_Tree,
                                    "directory to copy interfaces cannot " &
                                    "be the same as source directory { of " &
                                    "project %%",
                                    Lib_Src_Dir.Location);
                                 Data.Library_Src_Dir := No_Path;
                                 exit Project_Loop;
                              end if;

                              Src_Dirs := Src_Dir.Next;
                           end loop Dir_Loop;
                        end loop Project_Loop;
                     end if;
                  end;

                  --  In high verbosity, if there is a valid Library_Src_Dir,
                  --  display its path name.

                  if Data.Library_Src_Dir /= No_Path
                    and then Current_Verbosity = High
                  then
                     Write_Str ("Directory to copy interfaces =""");
                     Write_Str (Get_Name_String (Data.Library_Src_Dir));
                     Write_Line ("""");
                  end if;
               end if;
            end;
         end if;

         --  Check the symbol related attributes

         --  First, the symbol policy

         if not Lib_Symbol_Policy.Default then
            declare
               Value : constant String :=
                 To_Lower
                   (Get_Name_String (Lib_Symbol_Policy.Value));

            begin
               --  Symbol policy must hove one of a limited number of values

               if Value = "autonomous" or else Value = "default" then
                  Data.Symbol_Data.Symbol_Policy := Autonomous;

               elsif Value = "compliant" then
                  Data.Symbol_Data.Symbol_Policy := Compliant;

               elsif Value = "controlled" then
                  Data.Symbol_Data.Symbol_Policy := Controlled;

               elsif Value = "restricted" then
                  Data.Symbol_Data.Symbol_Policy := Restricted;

               else
                  Error_Msg
                    (Project, In_Tree,
                     "illegal value for Library_Symbol_Policy",
                     Lib_Symbol_Policy.Location);
               end if;
            end;
         end if;

         --  If attribute Library_Symbol_File is not specified, symbol policy
         --  cannot be Restricted.

         if Lib_Symbol_File.Default then
            if Data.Symbol_Data.Symbol_Policy = Restricted then
               Error_Msg
                 (Project, In_Tree,
                  "Library_Symbol_File needs to be defined when " &
                  "symbol policy is Restricted",
                  Lib_Symbol_Policy.Location);
            end if;

         else
            --  Library_Symbol_File is defined. Check that the file exists

            Data.Symbol_Data.Symbol_File :=
              Path_Name_Type (Lib_Symbol_File.Value);

            Get_Name_String (Lib_Symbol_File.Value);

            if Name_Len = 0 then
               Error_Msg
                 (Project, In_Tree,
                  "symbol file name cannot be an empty string",
                  Lib_Symbol_File.Location);

            else
               OK := not Is_Absolute_Path (Name_Buffer (1 .. Name_Len));

               if OK then
                  for J in 1 .. Name_Len loop
                     if Name_Buffer (J) = '/'
                       or else Name_Buffer (J) = Directory_Separator
                     then
                        OK := False;
                        exit;
                     end if;
                  end loop;
               end if;

               if not OK then
                  Error_Msg_File_1 := File_Name_Type (Lib_Symbol_File.Value);
                  Error_Msg
                    (Project, In_Tree,
                     "symbol file name { is illegal. " &
                     "Name canot include directory info.",
                     Lib_Symbol_File.Location);
               end if;
            end if;
         end if;

         --  If attribute Library_Reference_Symbol_File is not defined,
         --  symbol policy cannot be Compilant or Controlled.

         if Lib_Ref_Symbol_File.Default then
            if Data.Symbol_Data.Symbol_Policy = Compliant
              or else Data.Symbol_Data.Symbol_Policy = Controlled
            then
               Error_Msg
                 (Project, In_Tree,
                  "a reference symbol file need to be defined",
                  Lib_Symbol_Policy.Location);
            end if;

         else
            --  Library_Reference_Symbol_File is defined, check file exists

            Data.Symbol_Data.Reference :=
              Path_Name_Type (Lib_Ref_Symbol_File.Value);

            Get_Name_String (Lib_Ref_Symbol_File.Value);

            if Name_Len = 0 then
               Error_Msg
                 (Project, In_Tree,
                  "reference symbol file name cannot be an empty string",
                  Lib_Symbol_File.Location);

            else
               OK := not Is_Absolute_Path (Name_Buffer (1 .. Name_Len));

               if OK then
                  for J in 1 .. Name_Len loop
                     if Name_Buffer (J) = '/'
                       or else Name_Buffer (J) = Directory_Separator
                     then
                        OK := False;
                        exit;
                     end if;
                  end loop;
               end if;

               if not OK then
                  Error_Msg_File_1 :=
                    File_Name_Type (Lib_Ref_Symbol_File.Value);
                  Error_Msg
                    (Project, In_Tree,
                     "reference symbol file { name is illegal. " &
                     "Name canot include directory info.",
                     Lib_Ref_Symbol_File.Location);
               end if;

               if not Is_Regular_File
                 (Get_Name_String (Data.Object_Directory) &
                  Directory_Separator &
                  Get_Name_String (Lib_Ref_Symbol_File.Value))
               then
                  Error_Msg_File_1 :=
                    File_Name_Type (Lib_Ref_Symbol_File.Value);

                  --  For controlled symbol policy, it is an error if the
                  --  reference symbol file does not exist. For other symbol
                  --  policies, this is just a warning

                  Error_Msg_Warn :=
                    Data.Symbol_Data.Symbol_Policy /= Controlled;

                  Error_Msg
                    (Project, In_Tree,
                     "<library reference symbol file { does not exist",
                     Lib_Ref_Symbol_File.Location);

                  --  In addition in the non-controlled case, if symbol policy
                  --  is Compliant, it is changed to Autonomous, because there
                  --  is no reference to check against, and we don't want to
                  --  fail in this case.

                  if Data.Symbol_Data.Symbol_Policy /= Controlled then
                     if Data.Symbol_Data.Symbol_Policy = Compliant then
                        Data.Symbol_Data.Symbol_Policy := Autonomous;
                     end if;
                  end if;
               end if;
            end if;
         end if;
      end if;
   end Check_Stand_Alone_Library;

   ----------------------------
   -- Compute_Directory_Last --
   ----------------------------

   function Compute_Directory_Last (Dir : String) return Natural is
   begin
      if Dir'Length > 1
        and then (Dir (Dir'Last - 1) = Directory_Separator
                  or else Dir (Dir'Last - 1) = '/')
      then
         return Dir'Last - 1;
      else
         return Dir'Last;
      end if;
   end Compute_Directory_Last;

   ---------------
   -- Error_Msg --
   ---------------

   procedure Error_Msg
     (Project       : Project_Id;
      In_Tree       : Project_Tree_Ref;
      Msg           : String;
      Flag_Location : Source_Ptr)
   is
      Real_Location : Source_Ptr := Flag_Location;
      Error_Buffer  : String (1 .. 5_000);
      Error_Last    : Natural := 0;
      Name_Number   : Natural := 0;
      File_Number   : Natural := 0;
      First         : Positive := Msg'First;
      Index         : Positive;

      procedure Add (C : Character);
      --  Add a character to the buffer

      procedure Add (S : String);
      --  Add a string to the buffer

      procedure Add_Name;
      --  Add a name to the buffer

      procedure Add_File;
      --  Add a file name to the buffer

      ---------
      -- Add --
      ---------

      procedure Add (C : Character) is
      begin
         Error_Last := Error_Last + 1;
         Error_Buffer (Error_Last) := C;
      end Add;

      procedure Add (S : String) is
      begin
         Error_Buffer (Error_Last + 1 .. Error_Last + S'Length) := S;
         Error_Last := Error_Last + S'Length;
      end Add;

      --------------
      -- Add_File --
      --------------

      procedure Add_File is
         File : File_Name_Type;
      begin
         Add ('"');
         File_Number := File_Number + 1;

         case File_Number is
            when 1 =>
               File := Err_Vars.Error_Msg_File_1;
            when 2 =>
               File := Err_Vars.Error_Msg_File_2;
            when 3 =>
               File := Err_Vars.Error_Msg_File_3;
            when others =>
               null;
         end case;

         Get_Name_String (File);
         Add (Name_Buffer (1 .. Name_Len));
         Add ('"');
      end Add_File;

      --------------
      -- Add_Name --
      --------------

      procedure Add_Name is
         Name : Name_Id;
      begin
         Add ('"');
         Name_Number := Name_Number + 1;

         case Name_Number is
            when 1 =>
               Name := Err_Vars.Error_Msg_Name_1;
            when 2 =>
               Name := Err_Vars.Error_Msg_Name_2;
            when 3 =>
               Name := Err_Vars.Error_Msg_Name_3;
            when others =>
               null;
         end case;

         Get_Name_String (Name);
         Add (Name_Buffer (1 .. Name_Len));
         Add ('"');
      end Add_Name;

   --  Start of processing for Error_Msg

   begin
      --  If location of error is unknown, use the location of the project

      if Real_Location = No_Location then
         Real_Location := In_Tree.Projects.Table (Project).Location;
      end if;

      if Error_Report = null then
         Prj.Err.Error_Msg (Msg, Real_Location);
         return;
      end if;

      --  Ignore continuation character

      if Msg (First) = '\' then
         First := First + 1;

         --  Warning character is always the first one in this package
         --  this is an undocumented kludge!!!

      elsif Msg (First) = '?' then
         First := First + 1;
         Add ("Warning: ");

      elsif Msg (First) = '<' then
         First := First + 1;

         if Err_Vars.Error_Msg_Warn then
            Add ("Warning: ");
         end if;
      end if;

      Index := First;
      while Index <= Msg'Last loop
         if Msg (Index) = '{' then
            Add_File;

         elsif Msg (Index) = '%' then
            if Index < Msg'Last and then Msg (Index + 1) = '%' then
               Index := Index + 1;
            end if;

            Add_Name;
         else
            Add (Msg (Index));
         end if;
         Index := Index + 1;

      end loop;

      Error_Report (Error_Buffer (1 .. Error_Last), Project, In_Tree);
   end Error_Msg;

   --------------------------------
   -- Free_Ada_Naming_Exceptions --
   --------------------------------

   procedure Free_Ada_Naming_Exceptions is
   begin
      Ada_Naming_Exception_Table.Set_Last (0);
      Ada_Naming_Exceptions.Reset;
      Reverse_Ada_Naming_Exceptions.Reset;
   end Free_Ada_Naming_Exceptions;

   ---------------------
   -- Get_Directories --
   ---------------------

   procedure Get_Directories
     (Project : Project_Id;
      In_Tree : Project_Tree_Ref;
      Data    : in out Project_Data)
   is
      Object_Dir  : constant Variable_Value :=
                      Util.Value_Of
                        (Name_Object_Dir, Data.Decl.Attributes, In_Tree);

      Exec_Dir    : constant Variable_Value :=
                      Util.Value_Of
                        (Name_Exec_Dir, Data.Decl.Attributes, In_Tree);

      Source_Dirs : constant Variable_Value :=
                      Util.Value_Of
                        (Name_Source_Dirs, Data.Decl.Attributes, In_Tree);

      Source_Files : constant Variable_Value :=
                      Util.Value_Of
                        (Name_Source_Files, Data.Decl.Attributes, In_Tree);

      Last_Source_Dir : String_List_Id  := Nil_String;

      procedure Find_Source_Dirs
        (From     : File_Name_Type;
         Location : Source_Ptr);
      --  Find one or several source directories, and add them
      --  to the list of source directories of the project.

      ----------------------
      -- Find_Source_Dirs --
      ----------------------

      procedure Find_Source_Dirs
        (From     : File_Name_Type;
         Location : Source_Ptr)
      is
         Directory : constant String := Get_Name_String (From);
         Element   : String_Element;

         procedure Recursive_Find_Dirs (Path : Name_Id);
         --  Find all the subdirectories (recursively) of Path and add them
         --  to the list of source directories of the project.

         -------------------------
         -- Recursive_Find_Dirs --
         -------------------------

         procedure Recursive_Find_Dirs (Path : Name_Id) is
            Dir      : Dir_Type;
            Name     : String (1 .. 250);
            Last     : Natural;
            List     : String_List_Id := Data.Source_Dirs;
            Element  : String_Element;
            Found    : Boolean := False;

            Non_Canonical_Path : Name_Id := No_Name;
            Canonical_Path     : Name_Id := No_Name;

            The_Path : constant String :=
                         Normalize_Pathname (Get_Name_String (Path)) &
                         Directory_Separator;

            The_Path_Last : constant Natural :=
                              Compute_Directory_Last (The_Path);

         begin
            Name_Len := The_Path_Last - The_Path'First + 1;
            Name_Buffer (1 .. Name_Len) :=
              The_Path (The_Path'First .. The_Path_Last);
            Non_Canonical_Path := Name_Find;
            Get_Name_String (Non_Canonical_Path);
            Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));
            Canonical_Path := Name_Find;

            --  To avoid processing the same directory several times, check
            --  if the directory is already in Recursive_Dirs. If it is,
            --  then there is nothing to do, just return. If it is not, put
            --  it there and continue recursive processing.

            if Recursive_Dirs.Get (Canonical_Path) then
               return;

            else
               Recursive_Dirs.Set (Canonical_Path, True);
            end if;

            --  Check if directory is already in list

            while List /= Nil_String loop
               Element := In_Tree.String_Elements.Table (List);

               if Element.Value /= No_Name then
                  Found := Element.Value = Canonical_Path;
                  exit when Found;
               end if;

               List := Element.Next;
            end loop;

            --  If directory is not already in list, put it there

            if not Found then
               if Current_Verbosity = High then
                  Write_Str  ("   ");
                  Write_Line (The_Path (The_Path'First .. The_Path_Last));
               end if;

               String_Element_Table.Increment_Last
                 (In_Tree.String_Elements);
               Element :=
                 (Value    => Canonical_Path,
                  Display_Value => Non_Canonical_Path,
                  Location => No_Location,
                  Flag     => False,
                  Next     => Nil_String,
                  Index    => 0);

               --  Case of first source directory

               if Last_Source_Dir = Nil_String then
                  Data.Source_Dirs := String_Element_Table.Last
                                        (In_Tree.String_Elements);

                  --  Here we already have source directories

               else
                  --  Link the previous last to the new one

                  In_Tree.String_Elements.Table
                    (Last_Source_Dir).Next :=
                      String_Element_Table.Last
                        (In_Tree.String_Elements);
               end if;

               --  And register this source directory as the new last

               Last_Source_Dir  := String_Element_Table.Last
                 (In_Tree.String_Elements);
               In_Tree.String_Elements.Table (Last_Source_Dir) :=
                 Element;
            end if;

            --  Now look for subdirectories. We do that even when this
            --  directory is already in the list, because some of its
            --  subdirectories may not be in the list yet.

            Open (Dir, The_Path (The_Path'First .. The_Path_Last));

            loop
               Read (Dir, Name, Last);
               exit when Last = 0;

               if Name (1 .. Last) /= "."
                 and then Name (1 .. Last) /= ".."
               then
                  --  Avoid . and .. directories

                  if Current_Verbosity = High then
                     Write_Str  ("   Checking ");
                     Write_Line (Name (1 .. Last));
                  end if;

                  declare
                     Path_Name : constant String :=
                                   Normalize_Pathname
                                     (Name      => Name (1 .. Last),
                                      Directory =>
                                        The_Path
                                          (The_Path'First .. The_Path_Last),
                                      Resolve_Links  => False,
                                      Case_Sensitive => True);

                  begin
                     if Is_Directory (Path_Name) then

                        --  We have found a new subdirectory, call self

                        Name_Len := Path_Name'Length;
                        Name_Buffer (1 .. Name_Len) := Path_Name;
                        Recursive_Find_Dirs (Name_Find);
                     end if;
                  end;
               end if;
            end loop;

            Close (Dir);

         exception
            when Directory_Error =>
               null;
         end Recursive_Find_Dirs;

      --  Start of processing for Find_Source_Dirs

      begin
         if Current_Verbosity = High then
            Write_Str ("Find_Source_Dirs (""");
            Write_Str (Directory);
            Write_Line (""")");
         end if;

         --  First, check if we are looking for a directory tree,
         --  indicated by "/**" at the end.

         if Directory'Length >= 3
           and then Directory (Directory'Last - 1 .. Directory'Last) = "**"
           and then (Directory (Directory'Last - 2) = '/'
                       or else
                     Directory (Directory'Last - 2) = Directory_Separator)
         then
            Data.Known_Order_Of_Source_Dirs := False;

            Name_Len := Directory'Length - 3;

            if Name_Len = 0 then

               --  This is the case of "/**": all directories
               --  in the file system.

               Name_Len := 1;
               Name_Buffer (1) := Directory (Directory'First);

            else
               Name_Buffer (1 .. Name_Len) :=
                 Directory (Directory'First .. Directory'Last - 3);
            end if;

            if Current_Verbosity = High then
               Write_Str ("Looking for all subdirectories of """);
               Write_Str (Name_Buffer (1 .. Name_Len));
               Write_Line ("""");
            end if;

            declare
               Base_Dir : constant File_Name_Type := Name_Find;
               Root_Dir : constant String :=
                            Normalize_Pathname
                              (Name      => Get_Name_String (Base_Dir),
                               Directory =>
                                 Get_Name_String (Data.Display_Directory),
                               Resolve_Links  => False,
                               Case_Sensitive => True);

            begin
               if Root_Dir'Length = 0 then
                  Err_Vars.Error_Msg_File_1 := Base_Dir;

                  if Location = No_Location then
                     Error_Msg
                       (Project, In_Tree,
                        "{ is not a valid directory.",
                        Data.Location);
                  else
                     Error_Msg
                       (Project, In_Tree,
                        "{ is not a valid directory.",
                        Location);
                  end if;

               else
                  --  We have an existing directory, we register it and all
                  --  of its subdirectories.

                  if Current_Verbosity = High then
                     Write_Line ("Looking for source directories:");
                  end if;

                  Name_Len := Root_Dir'Length;
                  Name_Buffer (1 .. Name_Len) := Root_Dir;
                  Recursive_Find_Dirs (Name_Find);

                  if Current_Verbosity = High then
                     Write_Line ("End of looking for source directories.");
                  end if;
               end if;
            end;

         --  We have a single directory

         else
            declare
               Path_Name         : Path_Name_Type;
               Display_Path_Name : Path_Name_Type;

            begin
               Locate_Directory
                 (Project,
                  In_Tree,
                  From,
                  Data.Display_Directory,
                  Path_Name,
                  Display_Path_Name);

               if Path_Name = No_Path then
                  Err_Vars.Error_Msg_File_1 := From;

                  if Location = No_Location then
                     Error_Msg
                       (Project, In_Tree,
                        "{ is not a valid directory",
                        Data.Location);
                  else
                     Error_Msg
                       (Project, In_Tree,
                        "{ is not a valid directory",
                        Location);
                  end if;

               else
                  --  As it is an existing directory, we add it to
                  --  the list of directories.

                  String_Element_Table.Increment_Last
                    (In_Tree.String_Elements);
                  Element.Value := Name_Id (Path_Name);
                  Element.Display_Value := Name_Id (Display_Path_Name);

                  if Last_Source_Dir = Nil_String then

                     --  This is the first source directory

                     Data.Source_Dirs := String_Element_Table.Last
                                        (In_Tree.String_Elements);

                  else
                     --  We already have source directories,
                     --  link the previous last to the new one.

                     In_Tree.String_Elements.Table
                       (Last_Source_Dir).Next :=
                         String_Element_Table.Last
                           (In_Tree.String_Elements);
                  end if;

                  --  And register this source directory as the new last

                  Last_Source_Dir := String_Element_Table.Last
                    (In_Tree.String_Elements);
                  In_Tree.String_Elements.Table
                    (Last_Source_Dir) := Element;
               end if;
            end;
         end if;
      end Find_Source_Dirs;

   --  Start of processing for Get_Directories

   begin
      if Current_Verbosity = High then
         Write_Line ("Starting to look for directories");
      end if;

      --  Check the object directory

      pragma Assert (Object_Dir.Kind = Single,
                     "Object_Dir is not a single string");

      --  We set the object directory to its default

      Data.Object_Directory   := Data.Directory;
      Data.Display_Object_Dir := Data.Display_Directory;

      if Object_Dir.Value /= Empty_String then
         Get_Name_String (Object_Dir.Value);

         if Name_Len = 0 then
            Error_Msg
              (Project, In_Tree,
               "Object_Dir cannot be empty",
               Object_Dir.Location);

         else
            --  We check that the specified object directory does exist

            Locate_Directory
              (Project,
               In_Tree,
               File_Name_Type (Object_Dir.Value),
               Data.Display_Directory,
               Data.Object_Directory,
               Data.Display_Object_Dir,
               Create   => "object",
               Location => Object_Dir.Location);

            if Data.Object_Directory = No_Path then

               --  The object directory does not exist, report an error if the
               --  project is not externally built.

               if not Data.Externally_Built then
                  Err_Vars.Error_Msg_File_1 :=
                    File_Name_Type (Object_Dir.Value);
                  Error_Msg
                    (Project, In_Tree,
                     "the object directory { cannot be found",
                     Data.Location);
               end if;

               --  Do not keep a nil Object_Directory. Set it to the specified
               --  (relative or absolute) path. This is for the benefit of
               --  tools that recover from errors; for example, these tools
               --  could create the non existent directory.

               Data.Display_Object_Dir := Path_Name_Type (Object_Dir.Value);
               Get_Name_String (Object_Dir.Value);
               Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));
               Data.Object_Directory := Name_Find;
            end if;
         end if;
      end if;

      if Current_Verbosity = High then
         if Data.Object_Directory = No_Path then
            Write_Line ("No object directory");
         else
            Write_Str ("Object directory: """);
            Write_Str (Get_Name_String (Data.Display_Object_Dir));
            Write_Line ("""");
         end if;
      end if;

      --  Check the exec directory

      pragma Assert (Exec_Dir.Kind = Single,
                     "Exec_Dir is not a single string");

      --  We set the object directory to its default

      Data.Exec_Directory   := Data.Object_Directory;
      Data.Display_Exec_Dir := Data.Display_Object_Dir;

      if Exec_Dir.Value /= Empty_String then
         Get_Name_String (Exec_Dir.Value);

         if Name_Len = 0 then
            Error_Msg
              (Project, In_Tree,
               "Exec_Dir cannot be empty",
               Exec_Dir.Location);

         else
            --  We check that the specified object directory
            --  does exist.

            Locate_Directory
              (Project,
               In_Tree,
               File_Name_Type (Exec_Dir.Value),
               Data.Display_Directory,
               Data.Exec_Directory,
               Data.Display_Exec_Dir,
               Create   => "exec",
               Location => Exec_Dir.Location);

            if Data.Exec_Directory = No_Path then
               Err_Vars.Error_Msg_File_1 := File_Name_Type (Exec_Dir.Value);
               Error_Msg
                 (Project, In_Tree,
                  "the exec directory { cannot be found",
                  Data.Location);
            end if;
         end if;
      end if;

      if Current_Verbosity = High then
         if Data.Exec_Directory = No_Path then
            Write_Line ("No exec directory");
         else
            Write_Str ("Exec directory: """);
            Write_Str (Get_Name_String (Data.Display_Exec_Dir));
            Write_Line ("""");
         end if;
      end if;

      --  Look for the source directories

      if Current_Verbosity = High then
         Write_Line ("Starting to look for source directories");
      end if;

      pragma Assert (Source_Dirs.Kind = List, "Source_Dirs is not a list");

      if (not Source_Files.Default) and then
        Source_Files.Values = Nil_String
      then
         Data.Source_Dirs := Nil_String;

         if Data.Extends = No_Project
           and then Data.Object_Directory = Data.Directory
         then
            Data.Object_Directory := No_Path;
         end if;

      elsif Source_Dirs.Default then

         --  No Source_Dirs specified: the single source directory
         --  is the one containing the project file

         String_Element_Table.Increment_Last
           (In_Tree.String_Elements);
         Data.Source_Dirs := String_Element_Table.Last
           (In_Tree.String_Elements);
         In_Tree.String_Elements.Table (Data.Source_Dirs) :=
           (Value         => Name_Id (Data.Directory),
            Display_Value => Name_Id (Data.Display_Directory),
            Location      => No_Location,
            Flag          => False,
            Next          => Nil_String,
            Index         => 0);

         if Current_Verbosity = High then
            Write_Line ("Single source directory:");
            Write_Str ("    """);
            Write_Str (Get_Name_String (Data.Display_Directory));
            Write_Line ("""");
         end if;

      elsif Source_Dirs.Values = Nil_String then

         --  If Source_Dirs is an empty string list, this means
         --  that this project contains no source. For projects that
         --  don't extend other projects, this also means that there is no
         --  need for an object directory, if not specified.

         if Data.Extends = No_Project
           and then  Data.Object_Directory = Data.Directory
         then
            Data.Object_Directory := No_Path;
         end if;

         Data.Source_Dirs           := Nil_String;

      else
         declare
            Source_Dir : String_List_Id := Source_Dirs.Values;
            Element    : String_Element;

         begin
            --  We will find the source directories for each
            --  element of the list

            while Source_Dir /= Nil_String loop
               Element :=
                 In_Tree.String_Elements.Table (Source_Dir);
               Find_Source_Dirs
                 (File_Name_Type (Element.Value), Element.Location);
               Source_Dir := Element.Next;
            end loop;
         end;
      end if;

      if Current_Verbosity = High then
         Write_Line ("Putting source directories in canonical cases");
      end if;

      declare
         Current : String_List_Id := Data.Source_Dirs;
         Element : String_Element;

      begin
         while Current /= Nil_String loop
            Element := In_Tree.String_Elements.Table (Current);
            if Element.Value /= No_Name then
               Get_Name_String (Element.Value);
               Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));
               Element.Value := Name_Find;
               In_Tree.String_Elements.Table (Current) := Element;
            end if;

            Current := Element.Next;
         end loop;
      end;

   end Get_Directories;

   ---------------
   -- Get_Mains --
   ---------------

   procedure Get_Mains
     (Project : Project_Id;
      In_Tree : Project_Tree_Ref;
      Data    : in out Project_Data)
   is
      Mains : constant Variable_Value :=
                Prj.Util.Value_Of (Name_Main, Data.Decl.Attributes, In_Tree);

   begin
      Data.Mains := Mains.Values;

      --  If no Mains were specified, and if we are an extending
      --  project, inherit the Mains from the project we are extending.

      if Mains.Default then
         if Data.Extends /= No_Project then
            Data.Mains :=
              In_Tree.Projects.Table (Data.Extends).Mains;
         end if;

      --  In a library project file, Main cannot be specified

      elsif Data.Library then
         Error_Msg
           (Project, In_Tree,
            "a library project file cannot have Main specified",
            Mains.Location);
      end if;
   end Get_Mains;

   ---------------------------
   -- Get_Sources_From_File --
   ---------------------------

   procedure Get_Sources_From_File
     (Path     : String;
      Location : Source_Ptr;
      Project  : Project_Id;
      In_Tree  : Project_Tree_Ref)
   is
      File        : Prj.Util.Text_File;
      Line        : String (1 .. 250);
      Last        : Natural;
      Source_Name : File_Name_Type;
      Name_Loc    : Name_Location;

   begin
      if Current_Verbosity = High then
         Write_Str  ("Opening """);
         Write_Str  (Path);
         Write_Line (""".");
      end if;

      --  Open the file

      Prj.Util.Open (File, Path);

      if not Prj.Util.Is_Valid (File) then
         Error_Msg (Project, In_Tree, "file does not exist", Location);
      else
         --  Read the lines one by one

         while not Prj.Util.End_Of_File (File) loop
            Prj.Util.Get_Line (File, Line, Last);

            --  A non empty, non comment line should contain a file name

            if Last /= 0
              and then (Last = 1 or else Line (1 .. 2) /= "--")
            then
               --  ??? we should check that there is no directory information

               Name_Len := Last;
               Name_Buffer (1 .. Name_Len) := Line (1 .. Last);
               Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));
               Source_Name := Name_Find;
               Name_Loc := Source_Names.Get (Source_Name);

               if Name_Loc = No_Name_Location then
                  Name_Loc :=
                    (Name     => Source_Name,
                     Location => Location,
                     Source   => No_Source,
                     Except   => False,
                     Found    => False);
               end if;

               Source_Names.Set (Source_Name, Name_Loc);
            end if;
         end loop;

         Prj.Util.Close (File);

      end if;
   end Get_Sources_From_File;

   ----------
   -- Hash --
   ----------

   function Hash (Unit : Unit_Info) return Header_Num is
   begin
      return Header_Num (Unit.Unit mod 2048);
   end Hash;

   ----------------------
   -- Locate_Directory --
   ----------------------

   procedure Locate_Directory
     (Project  : Project_Id;
      In_Tree  : Project_Tree_Ref;
      Name     : File_Name_Type;
      Parent   : Path_Name_Type;
      Dir      : out Path_Name_Type;
      Display  : out Path_Name_Type;
      Create   : String := "";
      Location : Source_Ptr := No_Location)
   is
      The_Name        : String := Get_Name_String (Name);

      The_Parent      : constant String :=
                          Get_Name_String (Parent) & Directory_Separator;

      The_Parent_Last : constant Natural :=
                          Compute_Directory_Last (The_Parent);

      Full_Name       : File_Name_Type;

   begin
      --  Convert '/' to directory separator (for Windows)

      for J in The_Name'Range loop
         if The_Name (J) = '/' then
            The_Name (J) := Directory_Separator;
         end if;
      end loop;

      if Current_Verbosity = High then
         Write_Str ("Locate_Directory (""");
         Write_Str (The_Name);
         Write_Str (""", """);
         Write_Str (The_Parent);
         Write_Line (""")");
      end if;

      Dir     := No_Path;
      Display := No_Path;

      if Is_Absolute_Path (The_Name) then
         Full_Name := Name;

      else
         Name_Len := 0;
         Add_Str_To_Name_Buffer
           (The_Parent (The_Parent'First .. The_Parent_Last));
         Add_Str_To_Name_Buffer (The_Name);
         Full_Name := Name_Find;
      end if;

      declare
         Full_Path_Name : constant String := Get_Name_String (Full_Name);

      begin
         if Setup_Projects and then Create'Length > 0
           and then not Is_Directory (Full_Path_Name)
         then
            begin
               Create_Path (Full_Path_Name);

               if not Quiet_Output then
                  Write_Str (Create);
                  Write_Str (" directory """);
                  Write_Str (Full_Path_Name);
                  Write_Line (""" created");
               end if;

            exception
               when Use_Error =>
                  Error_Msg
                    (Project, In_Tree,
                     "could not create " & Create &
                     " directory " & Full_Path_Name,
                     Location);
            end;
         end if;
         if Is_Directory (Full_Path_Name) then
            declare
               Normed : constant String :=
                          Normalize_Pathname
                            (Full_Path_Name,
                             Resolve_Links  => False,
                             Case_Sensitive => True);

               Canonical_Path : constant String :=
                                  Normalize_Pathname
                                    (Normed,
                                     Resolve_Links  => True,
                                     Case_Sensitive => False);

            begin
               Name_Len := Normed'Length;
               Name_Buffer (1 .. Name_Len) := Normed;
               Display := Name_Find;

               Name_Len := Canonical_Path'Length;
               Name_Buffer (1 .. Name_Len) := Canonical_Path;
               Dir := Name_Find;
            end;
         end if;
      end;
   end Locate_Directory;

   ----------------------
   -- Look_For_Sources --
   ----------------------

   procedure Look_For_Sources
     (Project      : Project_Id;
      In_Tree      : Project_Tree_Ref;
      Data         : in out Project_Data;
      Follow_Links : Boolean)
   is
      procedure Get_Sources_From_File
        (Path     : String;
         Location : Source_Ptr);
      --  Get the sources of a project from a text file

      procedure Search_Directories (For_All_Sources : Boolean);
      --  Search the source directories to find the sources.
      --  If For_All_Sources is True, check each regular file name against
      --  the naming schemes of the different languages. Otherwise consider
      --  only the file names in the hash table Source_Names.

      ---------------------------
      -- Get_Sources_From_File --
      ---------------------------

      procedure Get_Sources_From_File
        (Path     : String;
         Location : Source_Ptr)
      is
      begin
         --  Get the list of sources from the file and put them in hash table
         --  Source_Names.

         Get_Sources_From_File (Path, Location, Project, In_Tree);

      end Get_Sources_From_File;

      ------------------------
      -- Search_Directories --
      ------------------------

      procedure Search_Directories (For_All_Sources : Boolean) is
         Source_Dir      : String_List_Id := Data.Source_Dirs;
         Element         : String_Element;
         Dir             : Dir_Type;
         Name            : String (1 .. 1_000);
         Last            : Natural;

         File_Name         : File_Name_Type;
         Display_File_Name : File_Name_Type;

         Source            : Source_Id;
         Source_To_Replace : Source_Id := No_Source;
         Src_Data          : Source_Data;
         Add_Src           : Boolean;

         Name_Loc        : Name_Location;

         Check_Name      : Boolean;

         Language              : Language_Index;
         Language_Name         : Name_Id;
         Display_Language_Name : Name_Id;
         Unit                  : Name_Id;
         Kind                  : Source_Kind := Spec;
         Alternate_Languages   : Alternate_Language_Id :=
                                   No_Alternate_Language;

         OK : Boolean;

         procedure Check_Naming_Schemes;
         --  Check if the file name File_Name conforms to one of the naming
         --  schemes of the project. If it does, set the global variables
         --  Language, Language_Name, Display_Language_Name, Unit and Kind
         --  appropriately. If it does not, set Language to No_Language_Index.

         --------------------------
         -- Check_Naming_Schemes --
         --------------------------

         procedure Check_Naming_Schemes is
            Filename : constant String := Get_Name_String (File_Name);
            Last     : Positive := Filename'Last;

            Config   : Language_Config;

            Lang     : Name_List_Index  := Data.Languages;

            Header_File    : Boolean := False;
            First_Language : Language_Index;

         begin
            Unit := No_Name;

            while Lang /= No_Name_List loop

               Language := Data.First_Language_Processing;
               Language_Name := In_Tree.Name_Lists.Table (Lang).Name;

               while Language /= No_Language_Index loop
                  if In_Tree.Languages_Data.Table (Language).Name =
                    Language_Name
                  then
                     Display_Language_Name :=
                       In_Tree.Languages_Data.Table (Language).Display_Name;
                     Config := In_Tree.Languages_Data.Table (Language).Config;

                     if Config.Kind = File_Based then
                        --  For file based languages, there is no Unit.
                        --  Just check if the file name has the implementation
                        --  or, if it is specified, the template suffix of the
                        --  language.

                        Unit := No_Name;

                        if not Header_File and then
                          Config.Naming_Data.Body_Suffix /= No_File
                        then
                           declare
                              Impl_Suffix : constant String :=
                                              Get_Name_String
                                              (Config.Naming_Data.Body_Suffix);

                           begin
                              if Filename'Length > Impl_Suffix'Length
                                and then
                                  Filename
                                    (Last - Impl_Suffix'Length + 1 .. Last) =
                                  Impl_Suffix
                              then
                                 Kind := Impl;

                                 if Current_Verbosity = High then
                                    Write_Str ("     source of language ");
                                    Write_Line
                                      (Get_Name_String
                                         (Display_Language_Name));
                                 end if;

                                 return;
                              end if;
                           end;
                        end if;

                        if Config.Naming_Data.Spec_Suffix /= No_File then
                           declare
                              Spec_Suffix : constant String :=
                                              Get_Name_String
                                             (Config.Naming_Data.Spec_Suffix);

                           begin
                              if Filename'Length > Spec_Suffix'Length
                                and then
                                  Filename
                                    (Last - Spec_Suffix'Length + 1 .. Last) =
                                    Spec_Suffix
                              then
                                 Kind := Spec;

                                 if Current_Verbosity = High then
                                    Write_Str
                                      ("     header file of language ");
                                    Write_Line
                                      (Get_Name_String
                                         (Display_Language_Name));
                                 end if;

                                 if Header_File then
                                    Alternate_Language_Table.Increment_Last
                                      (In_Tree.Alt_Langs);
                                    In_Tree.Alt_Langs.Table
                                      (Alternate_Language_Table.Last
                                         (In_Tree.Alt_Langs)) :=
                                      (Language => Language,
                                       Next     => Alternate_Languages);
                                    Alternate_Languages :=
                                      Alternate_Language_Table.Last
                                       (In_Tree.Alt_Langs);
                                 else
                                    Header_File    := True;
                                    First_Language := Language;
                                 end if;
                              end if;
                           end;
                        end if;

                     elsif not Header_File then
                        --  Unit based language

                        OK := Config.Naming_Data.Dot_Replacement /= No_File;

                        if OK then
                           --  Check casing

                           case Config.Naming_Data.Casing is
                           when All_Lower_Case =>
                              for J in Filename'Range loop
                                 if Is_Letter (Filename (J)) then
                                    if not Is_Lower (Filename (J)) then
                                       OK := False;
                                       exit;
                                    end if;
                                 end if;
                              end loop;

                           when All_Upper_Case =>
                              for J in Filename'Range loop
                                 if Is_Letter (Filename (J)) then
                                    if not Is_Upper (Filename (J)) then
                                       OK := False;
                                       exit;
                                    end if;
                                 end if;
                              end loop;

                           when others =>
                              OK := False;
                           end case;
                        end if;

                        if OK then
                           OK := False;

                           if Config.Naming_Data.Separate_Suffix /= No_File
                              and then
                               Config.Naming_Data.Separate_Suffix /=
                                 Config.Naming_Data.Body_Suffix
                           then
                              declare
                                 Suffix : constant String :=
                                   Get_Name_String
                                     (Config.Naming_Data.Separate_Suffix);

                              begin
                                 if Filename'Length > Suffix'Length
                                   and then
                                     Filename
                                     (Last - Suffix'Length + 1 .. Last) =
                                       Suffix
                                 then
                                    Kind := Sep;
                                    Last := Last - Suffix'Length;
                                    OK := True;
                                 end if;
                              end;
                           end if;

                           if not OK and then
                             Config.Naming_Data.Body_Suffix /= No_File
                           then
                              declare
                                 Suffix : constant String :=
                                   Get_Name_String
                                     (Config.Naming_Data.Body_Suffix);

                              begin
                                 if Filename'Length > Suffix'Length
                                   and then
                                     Filename
                                     (Last - Suffix'Length + 1 .. Last) =
                                       Suffix
                                 then
                                    Kind := Impl;
                                    Last := Last - Suffix'Length;
                                    OK := True;
                                 end if;
                              end;
                           end if;

                           if not OK and then
                             Config.Naming_Data.Spec_Suffix /= No_File
                           then
                              declare
                                 Suffix : constant String :=
                                   Get_Name_String
                                     (Config.Naming_Data.Spec_Suffix);

                              begin
                                 if Filename'Length > Suffix'Length
                                   and then
                                     Filename
                                     (Last - Suffix'Length + 1 .. Last) =
                                       Suffix
                                 then
                                    Kind := Spec;
                                    Last := Last - Suffix'Length;
                                    OK := True;
                                 end if;
                              end;
                           end if;
                        end if;

                        if OK then
                           --  Replace dot replacements with dots

                           Name_Len := 0;

                           declare
                              J   : Positive := Filename'First;
                              Dot_Replacement : constant String :=
                                Get_Name_String
                                  (Config.Naming_Data.Dot_Replacement);
                              Max : constant Positive :=
                                      Last - Dot_Replacement'Length + 1;

                           begin
                              loop
                                 Name_Len := Name_Len + 1;

                                 if J <= Max and then
                                   Filename
                                     (J .. J + Dot_Replacement'Length - 1) =
                                     Dot_Replacement
                                 then
                                    Name_Buffer (Name_Len) := '.';
                                    J := J + Dot_Replacement'Length;
                                 else
                                    if Filename (J) = '.' then
                                       OK := False;
                                       exit;
                                    end if;

                                    Name_Buffer (Name_Len) :=
                                      GNAT.Case_Util.To_Lower (Filename (J));
                                    J := J + 1;
                                 end if;

                                 exit when J > Last;
                              end loop;
                           end;
                        end if;

                        if OK then
                           --  The name buffer should contain the name of the
                           --  the unit, if it is one.
                           --  Check that this is a valid unit name

                           Check_Ada_Name (Name_Buffer (1 .. Name_Len), Unit);

                           if Unit /= No_Name then

                              if Current_Verbosity = High then
                                 if Kind = Spec then
                                    Write_Str ("     spec of ");

                                 else
                                    Write_Str ("     body of ");
                                 end if;

                                 Write_Str (Get_Name_String (Unit));
                                 Write_Str (" (language ");
                                 Write_Str
                                   (Get_Name_String (Display_Language_Name));
                                 Write_Line (")");
                              end if;

                              return;
                           end if;
                        end if;
                     end if;
                  end if;

                  Language := In_Tree.Languages_Data.Table (Language).Next;
               end loop;

               Lang := In_Tree.Name_Lists.Table (Lang).Next;
            end loop;

            if Header_File then
               Language := First_Language;

            else
               Language := No_Language_Index;

               if Current_Verbosity = High then
                  Write_Line ("     not a source of any language");
               end if;
            end if;
         end Check_Naming_Schemes;

      begin
         if Current_Verbosity = High then
            Write_Line ("Looking for sources:");
         end if;

         --  For each subdirectory

         while Source_Dir /= Nil_String loop
            begin
               Element := In_Tree.String_Elements.Table (Source_Dir);
               if Element.Value /= No_Name then
                  Get_Name_String (Element.Display_Value);

                  declare
                     Source_Directory : constant String :=
                                          Name_Buffer (1 .. Name_Len) &
                                          Directory_Separator;
                     Dir_Last         : constant Natural :=
                                          Compute_Directory_Last
                                            (Source_Directory);

                  begin
                     if Current_Verbosity = High then
                        Write_Str ("Source_Dir = ");
                        Write_Line (Source_Directory);
                     end if;

                     --  We look to every entry in the source directory

                     Open (Dir, Source_Directory
                             (Source_Directory'First .. Dir_Last));

                     loop
                        Read (Dir, Name, Last);

                        exit when Last = 0;

                        if Is_Regular_File
                          (Source_Directory & Name (1 .. Last))
                        then

                           if Current_Verbosity = High then
                              Write_Str  ("   Checking ");
                              Write_Line (Name (1 .. Last));
                           end if;

                           Source_To_Replace := No_Source;

                           Name_Len := Last;
                           Name_Buffer (1 .. Name_Len) := Name (1 .. Last);
                           Display_File_Name := Name_Find;
                           Canonical_Case_File_Name
                             (Name_Buffer (1 .. Name_Len));
                           File_Name := Name_Find;

                           declare
                              Display_Path    : constant String :=
                                                  Normalize_Pathname
                                                   (Name           =>
                                                      Name (1 .. Last),
                                                    Directory      =>
                                                      Source_Directory
                                                     (Source_Directory'First ..
                                                      Dir_Last),
                                                    Resolve_Links  =>
                                                      Follow_Links,
                                                    Case_Sensitive => True);
                              Path            : String := Display_Path;
                              Path_Id         : Path_Name_Type;
                              Display_Path_Id : Path_Name_Type;

                           begin
                              Canonical_Case_File_Name (Path);
                              Name_Len := Path'Length;
                              Name_Buffer (1 .. Name_Len) := Path;
                              Path_Id := Name_Find;

                              Name_Len := Display_Path'Length;
                              Name_Buffer (1 .. Name_Len) := Display_Path;
                              Display_Path_Id := Name_Find;

                              Name_Loc := Source_Names.Get (File_Name);
                              Check_Name := False;

                              if Name_Loc = No_Name_Location then
                                 Check_Name := For_All_Sources;

                              else
                                 if Name_Loc.Found then
                                    --  Check if it is allowed to have the
                                    --  same file name in several source
                                    --  directories.

                                    if
                                      not Data.Known_Order_Of_Source_Dirs
                                    then
                                       Error_Msg_File_1 := File_Name;
                                       Error_Msg
                                         (Project, In_Tree,
                                          "{ is found in several " &
                                          "source directories",
                                          Name_Loc.Location);
                                    end if;

                                 else
                                    Name_Loc.Found := True;

                                    if Name_Loc.Source = No_Source then
                                       Check_Name := True;

                                    else
                                       In_Tree.Sources.Table
                                         (Name_Loc.Source).Path := Path_Id;

                                       Source_Paths_Htable.Set
                                         (In_Tree.Source_Paths_HT,
                                          Path_Id,
                                          Name_Loc.Source);

                                       In_Tree.Sources.Table
                                         (Name_Loc.Source).Display_Path :=
                                         Display_Path_Id;
                                    end if;
                                 end if;
                              end if;

                              if Check_Name then
                                 Alternate_Languages := No_Alternate_Language;
                                 Check_Naming_Schemes;

                                 if Language = No_Language_Index then
                                    if Name_Loc.Found then
                                       --  A file name in a list must be
                                       --  a source of a language.

                                       Error_Msg_File_1 := File_Name;
                                       Error_Msg
                                         (Project, In_Tree,
                                          "language unknown for {",
                                          Name_Loc.Location);
                                    end if;

                                 else
                                    --  Check if the same file name or unit
                                    --  is used in the project tree.

                                    Source := In_Tree.First_Source;
                                    Add_Src := True;

                                    while Source /= No_Source loop
                                       Src_Data :=
                                         In_Tree.Sources.Table (Source);

                                       if (Unit /= No_Name and then
                                           Src_Data.Unit = Unit and then
                                           Src_Data.Kind = Kind)
                                          or else
                                          (Unit = No_Name and then
                                           Src_Data.File = File_Name)
                                       then
                                          --  Do not allow the same unit name
                                          --  in different projects, except if
                                          --  one is extending the other.

                                          --  For a file based language,
                                          --  the same file name replaces
                                          --  a file in a project being
                                          --  extended, but it is allowed
                                          --  to have the same file name in
                                          --  unrelated projects.

                                          if Is_Extending
                                               (Project,
                                                Src_Data.Project,
                                                In_Tree)
                                          then
                                             Source_To_Replace := Source;

                                          elsif Unit /= No_Name then
                                             Error_Msg_Name_1 := Unit;
                                             Error_Msg
                                               (Project, In_Tree,
                                                "unit %% cannot belong to " &
                                                "several projects",
                                                No_Location);
                                             Add_Src := False;
                                          end if;
                                       end if;

                                       Source := Src_Data.Next_In_Sources;
                                    end loop;

                                    if Add_Src then
                                       Source_Data_Table.Increment_Last
                                         (In_Tree.Sources);
                                       Source := Source_Data_Table.Last
                                         (In_Tree.Sources);

                                       declare
                                          Data : Source_Data;
                                       begin
                                          Data.Project := Project;
                                          Data.Language_Name := Language_Name;
                                          Data.Language := Language;
                                          Data.Alternate_Languages :=
                                            Alternate_Languages;
                                          Data.Kind := Kind;
                                          Data.Unit := Unit;
                                          Data.File := File_Name;
                                          Data.Object :=
                                            Object_Name (File_Name);
                                          Data.Dependency :=
                                            In_Tree.Languages_Data.Table
                                             (Language).Config.Dependency_Kind;
                                          Data.Dep_Name :=
                                            Dependency_Name
                                              (File_Name, Data.Dependency);
                                          Data.Switches :=
                                            Switches_Name (File_Name);
                                          Data.Display_File :=
                                            Display_File_Name;
                                          Data.Path := Path_Id;
                                          Data.Display_Path :=
                                            Display_Path_Id;
                                          In_Tree.Sources.Table (Source) :=
                                            Data;
                                       end;

                                       Add_Source (Source, Data, In_Tree);

                                       Source_Paths_Htable.Set
                                         (In_Tree.Source_Paths_HT,
                                          Path_Id,
                                          Source);

                                       if Source_To_Replace /= No_Source then
                                          Remove_Source
                                            (Source_To_Replace,
                                             Source,
                                             Project,
                                             Data,
                                             In_Tree);
                                       end if;
                                    end if;
                                 end if;
                              end if;
                           end;
                        end if;
                     end loop;

                     Close (Dir);
                  end;
               end if;

            exception
               when Directory_Error =>
                  null;
            end;
            Source_Dir := Element.Next;
         end loop;

         if Current_Verbosity = High then
            Write_Line ("end Looking for sources.");
         end if;

      end Search_Directories;

   begin
      if Data.First_Language_Processing /= No_Language_Index then
         --  First, put all the naming exceptions, if any, in the Source_Names
         --  table.

         Source_Names.Reset;

         declare
            Source    : Source_Id;
            Src_Data  : Source_Data;
            Name_Loc  : Name_Location;

         begin
            Source := Data.First_Source;

            while Source /= No_Source loop
               Src_Data := In_Tree.Sources.Table (Source);
               Name_Loc := (Name     => Src_Data.File,
                            Location => No_Location,
                            Source   => Source,
                            Except   => Src_Data.Unit /= No_Name,
                            Found    => False);

               if Current_Verbosity = High then
                  Write_Str ("Putting source #");
                  Write_Str (Source'Img);
                  Write_Str (", file ");
                  Write_Str (Get_Name_String (Src_Data.File));
                  Write_Line (" in Source_Names");
               end if;

               Source_Names.Set
                 (K => Src_Data.File,
                  E => Name_Loc);

               Source := Src_Data.Next_In_Project;
            end loop;
         end;

         --  Now check attributes Sources and Source_List_File

         declare
            Sources          : constant Variable_Value :=
                                 Util.Value_Of
                                   (Name_Source_Files,
                                    Data.Decl.Attributes,
                                    In_Tree);

            Source_List_File : constant Variable_Value :=
                                 Util.Value_Of
                                   (Name_Source_List_File,
                                    Data.Decl.Attributes,
                                    In_Tree);

            Locally_Removed  : constant Variable_Value :=
                                 Util.Value_Of
                                   (Name_Locally_Removed_Files,
                                    Data.Decl.Attributes,
                                    In_Tree);
            Name_Loc         : Name_Location;

         begin
            if not Sources.Default then
               if not Source_List_File.Default then
                  Error_Msg
                    (Project, In_Tree,
                     "?both variables source_files and " &
                     "source_list_file are present",
                     Source_List_File.Location);
               end if;

               --  Sources is a list of file names

               declare
                  Current  : String_List_Id := Sources.Values;
                  Element  : String_Element;
                  Location : Source_Ptr;
                  Name     : File_Name_Type;

               begin
                  if Current = Nil_String then
                     Data.First_Language_Processing := No_Language_Index;
                  end if;

                  while Current /= Nil_String loop
                     Element :=
                       In_Tree.String_Elements.Table (Current);
                     Get_Name_String (Element.Value);
                     Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));
                     Name := Name_Find;

                     --  If the element has no location, then use the
                     --  location of Sources to report possible errors.

                     if Element.Location = No_Location then
                        Location := Sources.Location;
                     else
                        Location := Element.Location;
                     end if;

                     Name_Loc := Source_Names.Get (Name);

                     if Name_Loc = No_Name_Location then
                        Name_Loc :=
                          (Name     => Name,
                           Location => Location,
                           Source   => No_Source,
                           Except   => False,
                           Found    => False);
                        Source_Names.Set (Name, Name_Loc);
                     end if;

                     Current := Element.Next;
                  end loop;
               end;

            elsif not Source_List_File.Default then
               --  Source_List_File is the name of the file
               --  that contains the source file names

               declare
                  Source_File_Path_Name : constant String :=
                                            Path_Name_Of
                                              (File_Name_Type
                                                 (Source_List_File.Value),
                                               Data.Directory);

               begin
                  if Source_File_Path_Name'Length = 0 then
                     Err_Vars.Error_Msg_File_1 :=
                       File_Name_Type (Source_List_File.Value);
                     Error_Msg
                       (Project, In_Tree,
                        "file with sources { does not exist",
                        Source_List_File.Location);

                  else
                     Get_Sources_From_File
                       (Source_File_Path_Name,
                        Source_List_File.Location);
                  end if;
               end;
            end if;

            Search_Directories
              (For_All_Sources =>
                 Sources.Default and then Source_List_File.Default);

            --  If there are sources that are locally removed, mark them as
            --  such.

            if not Locally_Removed.Default then

               declare
                  Current  : String_List_Id := Locally_Removed.Values;
                  Element  : String_Element;
                  Location : Source_Ptr;
                  OK       : Boolean;
                  Name     : File_Name_Type;
                  Source   : Source_Id;
                  Src_Data : Source_Data;

               begin
                  while Current /= Nil_String loop
                     Element :=
                       In_Tree.String_Elements.Table (Current);
                     Get_Name_String (Element.Value);
                     Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));
                     Name := Name_Find;

                     --  If the element has no location, then use the
                     --  location of Locally_Removed to report
                     --  possible errors.

                     if Element.Location = No_Location then
                        Location := Locally_Removed.Location;
                     else
                        Location := Element.Location;
                     end if;

                     OK := False;

                     Source := In_Tree.First_Source;

                     while Source /= No_Source loop
                        Src_Data := In_Tree.Sources.Table (Source);

                        if Src_Data.File = Name then
                           --  Check that this is from this project or a
                           --  project that the current project extends.

                           if Src_Data.Project = Project or else
                             Is_Extending
                               (Project, Src_Data.Project, In_Tree)
                           then
                              Src_Data.Locally_Removed := True;
                              In_Tree.Sources.Table (Source) := Src_Data;
                              Add_Forbidden_File_Name (Name);
                              OK := True;
                              exit;
                           end if;
                        end if;

                        Source := Src_Data.Next_In_Sources;
                     end loop;

                     if not OK then
                        Err_Vars.Error_Msg_File_1 := Name;
                        Error_Msg
                          (Project, In_Tree, "unknown file {", Location);
                     end if;

                     Current := Element.Next;
                  end loop;
               end;
            end if;
         end;
      end if;
   end Look_For_Sources;

   ------------------
   -- Path_Name_Of --
   ------------------

   function Path_Name_Of
     (File_Name : File_Name_Type;
      Directory : Path_Name_Type)
      return String
   is
      Result : String_Access;

      The_Directory : constant String := Get_Name_String (Directory);

   begin
      Get_Name_String (File_Name);
      Result := Locate_Regular_File
        (File_Name => Name_Buffer (1 .. Name_Len),
         Path      => The_Directory);

      if Result = null then
         return "";
      else
         Canonical_Case_File_Name (Result.all);
         return Result.all;
      end if;
   end Path_Name_Of;

   -------------------
   -- Remove_Source --
   -------------------

   procedure Remove_Source
     (Id          : Source_Id;
      Replaced_By : Source_Id;
      Project     : Project_Id;
      Data        : in out Project_Data;
      In_Tree     : Project_Tree_Ref)
   is
      Src_Data : constant Source_Data := In_Tree.Sources.Table (Id);

      Source   : Source_Id;

   begin
      if Current_Verbosity = High then
         Write_Str ("Removing source #");
         Write_Line (Id'Img);
      end if;

      In_Tree.Sources.Table (Id).Replaced_By := Replaced_By;
      --  Remove the source from the global list

      Source := In_Tree.First_Source;

      if Source = Id then
         In_Tree.First_Source := Src_Data.Next_In_Sources;

      else
         while In_Tree.Sources.Table (Source).Next_In_Sources /= Id loop
            Source := In_Tree.Sources.Table (Source).Next_In_Sources;
         end loop;

         In_Tree.Sources.Table (Source).Next_In_Sources :=
           Src_Data.Next_In_Sources;
      end if;

      --  Remove the source from the project list

      if Src_Data.Project = Project then
         Source := Data.First_Source;

         if Source = Id then
            Data.First_Source := Src_Data.Next_In_Project;

            if Src_Data.Next_In_Project = No_Source then
               Data.Last_Source := No_Source;
            end if;

         else
            while In_Tree.Sources.Table (Source).Next_In_Project /= Id loop
               Source := In_Tree.Sources.Table (Source).Next_In_Project;
            end loop;

            In_Tree.Sources.Table (Source).Next_In_Project :=
              Src_Data.Next_In_Project;

            if Src_Data.Next_In_Project = No_Source then
               In_Tree.Projects.Table (Src_Data.Project).Last_Source := Source;
            end if;
         end if;

      else
         Source := In_Tree.Projects.Table (Src_Data.Project).First_Source;

         if Source = Id then
            In_Tree.Projects.Table (Src_Data.Project).First_Source :=
              Src_Data.Next_In_Project;

            if Src_Data.Next_In_Project = No_Source then
               In_Tree.Projects.Table (Src_Data.Project).Last_Source :=
                 No_Source;
            end if;

         else
            while In_Tree.Sources.Table (Source).Next_In_Project /= Id loop
               Source := In_Tree.Sources.Table (Source).Next_In_Project;
            end loop;

            In_Tree.Sources.Table (Source).Next_In_Project :=
              Src_Data.Next_In_Project;

            if Src_Data.Next_In_Project = No_Source then
               In_Tree.Projects.Table (Src_Data.Project).Last_Source := Source;
            end if;
         end if;
      end if;

      --  Remove source from the language list

      Source := In_Tree.Languages_Data.Table (Src_Data.Language).First_Source;

      if Source = Id then
         In_Tree.Languages_Data.Table (Src_Data.Language).First_Source :=
           Src_Data.Next_In_Lang;

      else
         while In_Tree.Sources.Table (Source).Next_In_Lang /= Id loop
            Source := In_Tree.Sources.Table (Source).Next_In_Lang;
         end loop;

         In_Tree.Sources.Table (Source).Next_In_Lang :=
           Src_Data.Next_In_Lang;
      end if;

   end Remove_Source;

   -----------------------
   -- Report_No_Sources --
   -----------------------

   procedure Report_No_Sources
     (Project   : Project_Id;
      Lang_Name : String;
      In_Tree   : Project_Tree_Ref;
      Location  : Source_Ptr)
   is
   begin
      case When_No_Sources is
         when Silent =>
            null;

         when Warning | Error =>
            Error_Msg_Warn := When_No_Sources = Warning;
            Error_Msg
              (Project, In_Tree,
               "<there are no " & Lang_Name & " sources in this project",
               Location);
      end case;
   end Report_No_Sources;

   ----------------------
   -- Show_Source_Dirs --
   ----------------------

   procedure Show_Source_Dirs
     (Data    : Project_Data;
      In_Tree : Project_Tree_Ref)
   is
      Current : String_List_Id;
      Element : String_Element;

   begin
      Write_Line ("Source_Dirs:");

      Current := Data.Source_Dirs;
      while Current /= Nil_String loop
         Element := In_Tree.String_Elements.Table (Current);
         Write_Str  ("   ");
         Write_Line (Get_Name_String (Element.Value));
         Current := Element.Next;
      end loop;

      Write_Line ("end Source_Dirs.");
   end Show_Source_Dirs;

end Prj.Nmsc;
