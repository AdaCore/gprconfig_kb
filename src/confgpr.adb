------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              C O N F G P R                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2006-2008, Free Software Foundation, Inc.       --
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

with Ada.Directories; use Ada.Directories;

with Errout;   use Errout;
with Gpr_Util; use Gpr_Util;
with Makeutl;  use Makeutl;
with Namet;    use Namet;
with Opt;      use Opt;
with Osint;    use Osint;
with Output;   use Output;
with Prj;      use Prj;
with Prj.Part;
with Prj.Proc; use Prj.Proc;
with Prj.Tree; use Prj.Tree;
with Prj.Util; use Prj.Util;
with Snames;   use Snames;
with Types;    use Types;

with GNAT.HTable;

with System;
with System.Case_Util; use System.Case_Util;

package body Confgpr is

   Auto_Cgpr : constant String := "auto.cgpr";

   Gprconfig_Name : constant String := "gprconfig";
   Gprconfig_Path : String_Access := null;

   package Language_Htable is new GNAT.HTable.Simple_HTable
     (Header_Num => Prj.Header_Num,
      Element    => Name_Id,
      No_Element => No_Name,
      Key        => Name_Id,
      Hash       => Prj.Hash,
      Equal      => "=");
   --  Hash table to keep the languages used in the project tree

   procedure Add_Attributes
     (Conf_Decl : Declarations;
      User_Decl : in out Declarations);
   --  Process the attributes in the config declarations.
   --  For single string values, if the attribute is not declared in the user
   --  declarations, declare it with the value in the config declarations.
   --  For string list values, prepend the value in the user declarations with
   --  the value in the config declarations.

   --------------------
   -- Add_Attributes --
   --------------------

   procedure Add_Attributes
     (Conf_Decl : Declarations;
      User_Decl : in out Declarations)
   is
      Conf_Attr_Id       : Variable_Id;
      Conf_Attr          : Variable;
      Conf_Array_Id      : Array_Id;
      Conf_Array         : Array_Data;
      Conf_Array_Elem_Id : Array_Element_Id;
      Conf_Array_Elem    : Array_Element;
      Conf_List          : String_List_Id;
      Conf_List_Elem     : String_Element;

      User_Attr_Id       : Variable_Id;
      User_Attr          : Variable;
      User_Array_Id      : Array_Id;
      User_Array         : Array_Data;
      User_Array_Elem_Id : Array_Element_Id;
      User_Array_Elem    : Array_Element;

   begin
      Conf_Attr_Id := Conf_Decl.Attributes;
      User_Attr_Id := User_Decl.Attributes;

      while Conf_Attr_Id /= No_Variable loop
         Conf_Attr :=
           Project_Tree.Variable_Elements.Table (Conf_Attr_Id);
         User_Attr :=
           Project_Tree.Variable_Elements.Table (User_Attr_Id);

         if not Conf_Attr.Value.Default then
            if User_Attr.Value.Default then

               --  No attribute declared in user project file: just copy the
               --  value of the configuration attribute.

               User_Attr.Value := Conf_Attr.Value;
               Project_Tree.Variable_Elements.Table (User_Attr_Id) :=
                 User_Attr;

            elsif User_Attr.Value.Kind = List and then
            Conf_Attr.Value.Values /= Nil_String
            then

               --  List attribute declared in both the user project and the
               --  configuration project: prepend the user list with the
               --  configuration list.

               declare
                  Conf_List : String_List_Id :=
                    Conf_Attr.Value.Values;
                  Conf_Elem : String_Element;
                  User_List : constant String_List_Id :=
                    User_Attr.Value.Values;
                  New_List : String_List_Id;
                  New_Elem : String_Element;

               begin

                  --  Create new list

                  String_Element_Table.Increment_Last
                    (Project_Tree.String_Elements);
                  New_List := String_Element_Table.Last
                    (Project_Tree.String_Elements);

                  --  Value of attribute is new list

                  User_Attr.Value.Values := New_List;
                  Project_Tree.Variable_Elements.Table (User_Attr_Id) :=
                    User_Attr;

                  loop

                     --  Get each element of configuration list

                     Conf_Elem :=
                       Project_Tree.String_Elements.Table (Conf_List);
                     New_Elem := Conf_Elem;
                     Conf_List := Conf_Elem.Next;

                     if Conf_List = Nil_String then

                        --  If it is the last element in the list, connect to
                        --  first element of user list, and we are done.

                        New_Elem.Next := User_List;
                        Project_Tree.String_Elements.Table
                          (New_List) := New_Elem;
                        exit;

                     else

                        --  If it is not the last element in the list, add to
                        --  new list.

                        String_Element_Table.Increment_Last
                          (Project_Tree.String_Elements);
                        New_Elem.Next :=
                          String_Element_Table.Last
                            (Project_Tree.String_Elements);
                        Project_Tree.String_Elements.Table
                          (New_List) := New_Elem;
                        New_List := New_Elem.Next;
                     end if;
                  end loop;
               end;
            end if;
         end if;

         Conf_Attr_Id := Conf_Attr.Next;
         User_Attr_Id := User_Attr.Next;
      end loop;

      Conf_Array_Id := Conf_Decl.Arrays;
      while Conf_Array_Id /= No_Array loop
         Conf_Array := Project_Tree.Arrays.Table (Conf_Array_Id);

         User_Array_Id := User_Decl.Arrays;
         while User_Array_Id /= No_Array loop
            User_Array := Project_Tree.Arrays.Table (User_Array_Id);
            exit when User_Array.Name = Conf_Array.Name;
            User_Array_Id := User_Array.Next;
         end loop;

         --  If this associative array does not exist in the user project file,
         --  do a shallow copy of the full associative array.

         if User_Array_Id = No_Array then
            Array_Table.Increment_Last (Project_Tree.Arrays);
            User_Array := Conf_Array;
            User_Array.Next := User_Decl.Arrays;
            User_Decl.Arrays := Array_Table.Last (Project_Tree.Arrays);
            Project_Tree.Arrays.Table (User_Decl.Arrays) := User_Array;

         else
            --  Otherwise, check each array element

            Conf_Array_Elem_Id := Conf_Array.Value;
            while Conf_Array_Elem_Id /= No_Array_Element loop
               Conf_Array_Elem :=
                 Project_Tree.Array_Elements.Table (Conf_Array_Elem_Id);

               User_Array_Elem_Id := User_Array.Value;
               while User_Array_Elem_Id /= No_Array_Element loop
                  User_Array_Elem :=
                    Project_Tree.Array_Elements.Table (User_Array_Elem_Id);
                  exit when User_Array_Elem.Index = Conf_Array_Elem.Index;
                  User_Array_Elem_Id := User_Array_Elem.Next;
               end loop;

               --  If the array element does not exist in the user array,
               --  insert a shallow copy of the conf array element in the
               --  user array.

               if User_Array_Elem_Id = No_Array_Element then
                  Array_Element_Table.Increment_Last
                    (Project_Tree.Array_Elements);
                  User_Array_Elem := Conf_Array_Elem;
                  User_Array_Elem.Next := User_Array.Value;
                  User_Array.Value :=
                    Array_Element_Table.Last (Project_Tree.Array_Elements);
                  Project_Tree.Array_Elements.Table (User_Array.Value) :=
                    User_Array_Elem;
                  Project_Tree.Arrays.Table (User_Array_Id) := User_Array;

               --  Otherwise, if the value is a string list, prepend the
               --  user array element with the conf array element value.

               elsif Conf_Array_Elem.Value.Kind = List then
                  Conf_List := Conf_Array_Elem.Value.Values;

                  if Conf_List /= Nil_String then
                     declare
                        Link : constant String_List_Id :=
                                 User_Array_Elem.Value.Values;
                        Previous : String_List_Id := Nil_String;
                        Next     : String_List_Id;
                     begin
                        loop
                           Conf_List_Elem :=
                             Project_Tree.String_Elements.Table
                               (Conf_List);
                           String_Element_Table.Increment_Last
                             (Project_Tree.String_Elements);
                           Next :=
                             String_Element_Table.Last
                               (Project_Tree.String_Elements);
                           Project_Tree.String_Elements.Table (Next) :=
                             Conf_List_Elem;

                           if Previous = Nil_String then
                              User_Array_Elem.Value.Values := Next;
                              Project_Tree.Array_Elements.Table
                                (User_Array_Elem_Id) := User_Array_Elem;

                           else
                              Project_Tree.String_Elements.Table
                                (Previous).Next := Next;
                           end if;

                           Previous := Next;

                           Conf_List := Conf_List_Elem.Next;

                           if Conf_List = Nil_String then
                              Project_Tree.String_Elements.Table
                                (Previous).Next := Link;
                              exit;
                           end if;
                        end loop;
                     end;
                  end if;
               end if;

               Conf_Array_Elem_Id := Conf_Array_Elem.Next;
            end loop;
         end if;

         Conf_Array_Id := Conf_Array.Next;
      end loop;
   end Add_Attributes;

   -----------------------
   -- Get_Configuration --
   -----------------------

   procedure Get_Configuration (Packages_To_Check : String_List_Access) is
      Config_Path : String_Access;

      Main_Object_Dir : Path_Name_Type := No_Path;

   begin
      Configuration_Project_Path := null;
      Delete_Autoconf_File := Autoconfiguration;

      declare
         Prefix_Path : constant String := Executable_Prefix_Path;

      begin
         if Prefix_Path'Length /= 0 then
            Config_Path :=
              new String'("." & Path_Separator &
                          Prefix_Path & Directory_Separator &
                          "share" & Directory_Separator & "gpr");
         else
            Config_Path := new String'(".");
         end if;
      end;

      if Target_Name /= null then
         Default_Config_Project_File_Name := new String'
           (Target_Name.all & ".cgpr");

      else
         Default_Config_Project_File_Name := Getenv (Config_Project_Env_Var);
      end if;

      if Default_Config_Project_File_Name'Length = 0 then
         Default_Config_Project_File_Name := new String'(Default_Name);
      end if;

      --  If --config= or --autoconf= was specified, locate configuration
      --  project file.

      if Config_Project_File_Name /= null then
         Configuration_Project_Path :=
           Locate_Regular_File
             (Config_Project_File_Name.all,
              Config_Path.all);

         if Autoconfiguration then
            Autoconfiguration := Configuration_Project_Path = null;

         elsif Configuration_Project_Path = null then
            Osint.Fail
              ("could not locate main configuration project ",
               Config_Project_File_Name.all);
         end if;
      end if;

      --  If no config file is specified, check if there is a default config
      --  file. If there is, there will be no auto configuration.

      if Config_Project_File_Name = null then
         Configuration_Project_Path :=
           Locate_Regular_File
             (Default_Config_Project_File_Name.all,
              Config_Path.all);

         if Configuration_Project_Path /= null then
            Config_Project_File_Name :=
              new String'(Default_Config_Project_File_Name.all);
            Autoconfiguration := False;
            Delete_Autoconf_File := False;
         end if;
      end if;

      --  If Autoconfiguration is True, then get the path of gprconfig

      if Autoconfiguration then
         Gprconfig_Path := Locate_Exec_On_Path (Gprconfig_Name);

         if Gprconfig_Path = null then
            Fail ("could not locate gprconfig for auto-configuration");
         end if;
      end if;

      --  Parse the user project tree

      Set_In_Configuration (False);
      Prj.Initialize (Project_Tree);
      Prj.Tree.Initialize (Project_Node_Tree);

      Prj.Part.Parse
        (In_Tree                => Project_Node_Tree,
         Project                => User_Project_Node,
         Project_File_Name      => Project_File_Name.all,
         Always_Errout_Finalize => False,
         Packages_To_Check      => Packages_To_Check,
         Current_Directory      => Current_Directory);

      if User_Project_Node = Empty_Node then
         --  Don't flush messages. This has already been taken care of by the
         --  above call. Oterwise, it results in se same message being
         --  displayed twice.

         Fail_Program
           ("""", Project_File_Name.all, """ processing failed",
            Flush_Messages => False);
      end if;

      Process_Project_Tree_Phase_1
        (In_Tree                => Project_Tree,
         Project                => Main_Project,
         Success                => Success,
         From_Project_Node      => User_Project_Node,
         From_Project_Node_Tree => Project_Node_Tree,
         Report_Error           => null);

      if not Success then
         Fail_Program ("""", Project_File_Name.all, """ processing failed");
      end if;

      --  If Autoconfiguration is True, get the languages from the user project
      --  tree, call gprconfig and check that the config file exists.

      if Autoconfiguration then

         --  First, find the object directory of the main project

         declare
            Obj_Dir : constant Variable_Value :=
              Value_Of
                (Name_Object_Dir,
                 Project_Tree.Projects.Table (Main_Project).Decl.Attributes,
                 Project_Tree);

         begin
            if Obj_Dir = Nil_Variable_Value or else Obj_Dir.Default then
               Get_Name_String
                 (Project_Tree.Projects.Table (Main_Project).Directory.Name);

            else
               if Is_Absolute_Path (Get_Name_String (Obj_Dir.Value)) then
                  Get_Name_String (Obj_Dir.Value);

               else
                  Name_Len := 0;
                  Add_Str_To_Name_Buffer
                    (Get_Name_String
                       (Project_Tree.Projects.Table
                          (Main_Project).Directory.Name));
                  Add_Char_To_Name_Buffer (Directory_Separator);
                  Add_Str_To_Name_Buffer (Get_Name_String (Obj_Dir.Value));
               end if;
            end if;

            if Subdirs /= null then
               Add_Char_To_Name_Buffer (Directory_Separator);
               Add_Str_To_Name_Buffer (Subdirs.all);
            end if;

            for J in 1 .. Name_Len loop
               if Name_Buffer (J) = '/' then
                  Name_Buffer (J) := Directory_Separator;
               end if;
            end loop;

            Main_Object_Dir := Name_Find;
         end;

         --  Check if the object directory exists. If Setup_Projects is True
         --  (-p) and directory does not exist, attempt to create it.
         --  Otherwise, if directory does not exist, fail without calling
         --  gprconfig.

         declare
            Obj_Dir : constant String := Get_Name_String (Main_Object_Dir);
         begin
            if not Is_Directory (Obj_Dir)
              and then (Setup_Projects or Subdirs /= null)
            then
               begin
                  Create_Path (Obj_Dir);

                  if not Quiet_Output then
                     Write_Str ("object directory """);
                     Write_Str (Obj_Dir);
                     Write_Line (""" created");
                  end if;

               exception
                  when others =>
                     Fail ("could not create object directory ", Obj_Dir);
               end;
            end if;

            if not Is_Directory (Obj_Dir) then
               Fail ("object directory ", Obj_Dir, " does not exist");
            end if;
         end;

         --  Get the languages in the user project tree

         for Project in 1 .. Project_Table.Last (Project_Tree.Projects) loop
            declare
               Data : constant Project_Data :=
                 Project_Tree.Projects.Table (Project);
               Search : Boolean := True;
               Variable : Variable_Value;
               List : String_List_Id;
               Elem : String_Element;
               Lang : Name_Id;

            begin
               Variable :=
                 Value_Of
                   (Name_Source_Dirs,
                    Data.Decl.Attributes,
                    Project_Tree);
               Search :=
                 Variable = Nil_Variable_Value or else
                 Variable.Default or else
                 Variable.Values /= Nil_String;

               if Search then
                  Variable :=
                    Value_Of
                      (Name_Source_Files,
                       Data.Decl.Attributes,
                       Project_Tree);
                  Search :=
                    Variable = Nil_Variable_Value or else
                    Variable.Default or else
                    Variable.Values /= Nil_String;
               end if;

               if Search then
                  Variable :=
                    Value_Of
                      (Name_Languages,
                       Data.Decl.Attributes,
                       Project_Tree);

                  if Variable = Nil_Variable_Value or else
                     Variable.Default
                  then

                     --  Languages is not declared. If it is not an extending
                     --  project, check for Default_Language

                     if Data.Extends = No_Project then
                        Variable :=
                          Value_Of
                            (Name_Default_Language,
                             Data.Decl.Attributes,
                             Project_Tree);

                        if Variable /= Nil_Variable_Value and then
                           not Variable.Default
                        then
                           Get_Name_String (Variable.Value);
                           To_Lower (Name_Buffer (1 .. Name_Len));
                           Lang := Name_Find;
                           Language_Htable.Set (Lang, Lang);

                        else
                           --  If no language is declared, default to Ada

                           Language_Htable.Set (Name_Ada, Name_Ada);
                        end if;
                     end if;

                  elsif Variable.Values /= Nil_String then

                     --  Attribute Languages is declared with a non empty
                     --  list: put all the languages in Language_HTable.

                     List := Variable.Values;
                     while List /= Nil_String loop
                        Elem := Project_Tree.String_Elements.Table (List);

                        Get_Name_String (Elem.Value);
                        To_Lower (Name_Buffer (1 .. Name_Len));
                        Lang := Name_Find;
                        Language_Htable.Set (Lang, Lang);

                        List := Elem.Next;
                     end loop;
                  end if;
               end if;
            end;
         end loop;

         --  If no config file was specified, set the auto.cgpr one

         if Config_Project_File_Name = null then
            Config_Project_File_Name :=
              new String'(Get_Name_String (Main_Object_Dir) &
                          Directory_Separator &
                          Auto_Cgpr);
         end if;

         --  Invoke gprconfig

         declare
            Args     : Argument_List_Access := new Argument_List (1 .. 6);
            Arg_Last : Positive;
            Name     : Name_Id;

         begin
            Args (1) := new String'("--batch");
            Args (2) := new String'("-o");
            Args (3) := Config_Project_File_Name;
            Arg_Last := 3;

            if not Verbose_Mode then
               Arg_Last := Arg_Last + 1;
               Args (Arg_Last) := new String'("-q");
            end if;

            if Target_Name /= null then
               Arg_Last := Arg_Last + 1;
               Args (Arg_Last) :=
                 new String'(Target_Project_Option & Target_Name.all);
            end if;

            Name := Language_Htable.Get_First;
            while Name /= No_Name loop
               if Arg_Last = Args'Last then
                  declare
                     New_Args : constant String_List_Access :=
                       new String_List (1 .. Arg_Last * 2);
                  begin
                     New_Args (Args'Range) := Args.all;
                     Args := New_Args;
                  end;
               end if;

               Arg_Last := Arg_Last + 1;
               Args (Arg_Last) :=
                 new String'("--config=" & Get_Name_String (Name));

               Name := Language_Htable.Get_Next;
            end loop;

            if Verbose_Mode then
               Write_Str (Gprconfig_Name);

               for J in 1 .. Arg_Last loop
                  Write_Char (' ');
                  Write_Str (Args (J).all);
               end loop;

               Write_Eol;

            elsif not Quiet_Output then
               Write_Str ("creating ");
               Write_Str (Simple_Name (Config_Project_File_Name.all));
               Write_Eol;
            end if;

            Spawn (Gprconfig_Path.all, Args (1 .. Arg_Last), Success);

            Configuration_Project_Path :=
              Locate_Regular_File
                (Config_Project_File_Name.all, Config_Path.all);

            if Configuration_Project_Path = null then
               Fail ("could not create ", Config_Project_File_Name.all);
            end if;
         end;
      end if;

      --  Parse the configuration file

      Set_In_Configuration (True);

      if Configuration_Project_Path = null then
         Configuration_Project_Path :=
           Locate_Regular_File
             (Config_Project_File_Name.all,
              Config_Path.all);
      end if;

      if Configuration_Project_Path = null then
         Fail
           ("config file ", Config_Project_File_Name.all, " does not exist");
      end if;

      if Verbose_Mode then
         Write_Line ("Checking configuration");
         Write_Line (Configuration_Project_Path.all);
      end if;

      Prj.Part.Parse
        (In_Tree                => Project_Node_Tree,
         Project                => Config_Project_Node,
         Project_File_Name      => Configuration_Project_Path.all,
         Always_Errout_Finalize => False,
         Packages_To_Check      => Packages_To_Check,
         Current_Directory      => Current_Directory);

      if Config_Project_Node /= Empty_Node then
         Prj.Proc.Process_Project_Tree_Phase_1
           (In_Tree                => Project_Tree,
            Project                => Main_Config_Project,
            Success                => Success,
            From_Project_Node      => Config_Project_Node,
            From_Project_Node_Tree => Project_Node_Tree,
            Report_Error           => null,
            Reset_Tree             => False);
      end if;

      if Config_Project_Node = Empty_Node or else
        Main_Config_Project = No_Project
      then
         Osint.Fail
           ("processing of configuration project """,
            Configuration_Project_Path.all,
            """ failed");
      end if;

      --  Add the configuration attributes to all user projects

      declare
         Conf_Decl    : constant Declarations :=
                        Project_Tree.Projects.Table (Main_Config_Project).Decl;
         Conf_Pack_Id : Package_Id;
         Conf_Pack    : Package_Element;

         User_Decl    : Declarations;
         User_Pack_Id : Package_Id;
         User_Pack    : Package_Element;
      begin
         for Proj in 1 .. Project_Table.Last (Project_Tree.Projects) loop
            if Proj /= Main_Config_Project then
               User_Decl := Project_Tree.Projects.Table (Proj).Decl;
               Add_Attributes
                 (Conf_Decl => Conf_Decl,
                  User_Decl => User_Decl);

               Conf_Pack_Id := Conf_Decl.Packages;
               while Conf_Pack_Id /= No_Package loop
                  Conf_Pack := Project_Tree.Packages.Table (Conf_Pack_Id);

                  User_Pack_Id := User_Decl.Packages;
                  while User_Pack_Id /= No_Package loop
                     User_Pack := Project_Tree.Packages.Table (User_Pack_Id);
                     exit when User_Pack.Name = Conf_Pack.Name;
                     User_Pack_Id := User_Pack.Next;
                  end loop;

                  if User_Pack_Id = No_Package then
                     Package_Table.Increment_Last (Project_Tree.Packages);
                     User_Pack := Conf_Pack;
                     User_Pack.Next := User_Decl.Packages;
                     User_Decl.Packages :=
                       Package_Table.Last (Project_Tree.Packages);
                     Project_Tree.Packages.Table (User_Decl.Packages) :=
                       User_Pack;

                  else
                     Add_Attributes
                       (Conf_Decl => Conf_Pack.Decl,
                        User_Decl => Project_Tree.Packages.Table
                                       (User_Pack_Id).Decl);
                  end if;

                  Conf_Pack_Id := Conf_Pack.Next;
               end loop;

               Project_Tree.Projects.Table (Proj).Decl := User_Decl;
            end if;
         end loop;
      end;

      Set_In_Configuration (False);
   end Get_Configuration;

end Confgpr;
