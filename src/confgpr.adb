------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              C O N F G P R                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2006-2007, Free Software Foundation, Inc.       --
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

with Casing;   use Casing;
with Errout;   use Errout;
with Gpr_Util; use Gpr_Util;
with Makeutl;  use Makeutl;
with Namet;    use Namet;
with Opt;      use Opt;
with Osint;    use Osint;
with Output;   use Output;
with Prj;      use Prj;
with Prj.Pars;
with Prj.Util; use Prj.Util;
with Snames;   use Snames;

with GNAT.OS_Lib;      use GNAT.OS_Lib;

with System;
with System.Case_Util; use System.Case_Util;

package body Confgpr is

   Current_Language       : Name_Id := No_Name;
   Current_Language_Index : Language_Index := No_Language_Index;

   procedure Get_Language_Index_Of (Language : Name_Id);
   --  Set global variables Current_Language and Current_Language_Index in the
   --  project tree to put the language indexed attribute values. If no
   --  language data exists for the language, create a new record and set
   --  Current_Language_Index accordingly.

   -----------------------
   -- Get_Configuration --
   -----------------------

   procedure Get_Configuration (Fail_If_Error : Boolean) is
      Configuration_Project_Path : String_Access;
      Config_Path : String_Access;

      List    : Project_List;
      Element : Project_Element;

      Dot_Replacement : File_Name_Type := No_File;
      Casing          : Casing_Type    := All_Lower_Case;
      Separate_Suffix : File_Name_Type := No_File;

      OK : Boolean := False;
      --  True if configuration if configuration has been successfully checked

      Lang_Index : Language_Index;
      --  The index of the language data being checked

      Lang_Data : Language_Data;
      --  The data of the language being checked

      procedure Cleanup;
      --  Restore environment and reinitialize project tree

      procedure Process_Project_Level (Attributes : Variable_Id);
      --  Process the simple attributes at the project level of a configuration
      --  project.

      procedure Process_Project_Level (Arrays : Array_Id);
      --  Process the associate array attributes at the project level of a
      --  configuration project.

      procedure Process_Packages (Project : Project_Id);
      --  Read the packages of a configuration project

      procedure Report_Failure (S : String; N : Name_Id);
      --  Set OK to False. If Fail_If_Error is True, output an error message

      -------------
      -- Cleanup --
      -------------

      procedure Cleanup is
      begin
         Set_In_Configuration (False);

         --  Remove all configuration projects from the project tree

         for Proj in 1 .. Project_Table.Last (Project_Tree.Projects) loop
            Project_Tree.Projects.Table (Proj) := Empty_Project (Project_Tree);
         end loop;

         Project_Table.Init (Project_Tree.Projects);
      end Cleanup;

      ----------------------
      -- Process_Packages --
      ----------------------

      procedure Process_Packages (Project : Project_Id) is
         Data     : constant Project_Data :=
                      Project_Tree.Projects.Table (Project);
         Packages : Package_Id;
         Element  : Package_Element;

         procedure Process_Binder (Arrays : Array_Id);
         --  Process the associate array attributes of package Binder of a
         --  configuration project.

         procedure Process_Builder (Attributes : Variable_Id);
         --  Process the simple attributes of package Builder of a
         --  configuration project.

         procedure Process_Compiler (Arrays : Array_Id);
         --  Process the associate array attributes of package Compiler of a
         --  configuration project.

         procedure Process_Naming (Attributes : Variable_Id);
         --  Process the simple attributes of package Naming of a configuration
         --  project.

         procedure Process_Naming (Arrays : Array_Id);
         --  Process the associate array attributes of package Naming of a
         --  configuration project.

         procedure Process_Linker (Attributes : Variable_Id);
         --  Process the simple attributes of package Linker of a
         --  configuration project.

         --------------------
         -- Process_Binder --
         --------------------

         procedure Process_Binder (Arrays : Array_Id) is
            Current_Array_Id : Array_Id;
            Current_Array    : Array_Data;
            Element_Id       : Array_Element_Id;
            Element          : Array_Element;
            List             : String_List_Id;

         begin
            --  Process the associative array attribute of package Binder

            Current_Array_Id := Arrays;
            while Current_Array_Id /= No_Array loop
               Current_Array := Project_Tree.Arrays.Table (Current_Array_Id);

               Element_Id := Current_Array.Value;
               while Element_Id /= No_Array_Element loop
                  Element := Project_Tree.Array_Elements.Table (Element_Id);

                  --  Get the name of the language

                  Get_Language_Index_Of (Element.Index);

                  case Current_Array.Name is
                     when Name_Driver =>

                        --  Attribute Driver (<language>)

                        Project_Tree.Languages_Data.Table
                          (Current_Language_Index).Config.Binder_Driver :=
                          File_Name_Type (Element.Value.Value);
                        There_Are_Binder_Drivers := True;

                     when Name_Switches =>

                        --  Attribute Switches (<language>)

                        List := Element.Value.Values;

                        Put (Into_List =>
                             Project_Tree.Languages_Data.Table
                               (Current_Language_Index).Config.
                               Binder_Min_Options,
                             From_List => List,
                             In_Tree   => Project_Tree);

                     when Name_Prefix =>

                        --  Attribute Prefix (<language>)

                        Project_Tree.Languages_Data.Table
                          (Current_Language_Index).Config.Binder_Prefix :=
                          Element.Value.Value;

                     when Name_Objects_Path =>

                        --  Attribute Objects_Path (<language>)

                        Project_Tree.Languages_Data.Table
                          (Current_Language_Index).Config.Objects_Path :=
                          Element.Value.Value;

                     when Name_Objects_Path_File =>

                        --  Attribute Objects_Path (<language>)

                        Project_Tree.Languages_Data.Table
                          (Current_Language_Index).Config.Objects_Path_File :=
                          Element.Value.Value;

                     when others =>
                        null;
                  end case;

                  Element_Id := Element.Next;
               end loop;

               Current_Array_Id := Current_Array.Next;
            end loop;
         end Process_Binder;

         ---------------------
         -- Process_Builder --
         ---------------------

         procedure Process_Builder (Attributes : Variable_Id) is
            Attribute_Id : Variable_Id;
            Attribute    : Variable;

         begin
            --  Process non associated array attribute from package Builder

            Attribute_Id := Attributes;
            while Attribute_Id /= No_Variable loop
               Attribute :=
                 Project_Tree.Variable_Elements.Table (Attribute_Id);

               if not Attribute.Value.Default then
                  if Attribute.Name = Name_Executable_Suffix then

                     --  Attribute Executable_Suffix: the suffix of the
                     --  executables.

                     Project_Tree.Config.Executable_Suffix :=
                       Attribute.Value.Value;
                  end if;
               end if;

               Attribute_Id := Attribute.Next;
            end loop;
         end Process_Builder;

         ----------------------
         -- Process_Compiler --
         ----------------------

         procedure Process_Compiler (Arrays : Array_Id) is
            Current_Array_Id : Array_Id;
            Current_Array    : Array_Data;
            Element_Id       : Array_Element_Id;
            Element          : Array_Element;
            List             : String_List_Id;

         begin
            --  Process the associative array attribute of package Compiler

            Current_Array_Id := Arrays;
            while Current_Array_Id /= No_Array loop
               Current_Array := Project_Tree.Arrays.Table (Current_Array_Id);

               Element_Id := Current_Array.Value;
               while Element_Id /= No_Array_Element loop
                  Element := Project_Tree.Array_Elements.Table (Element_Id);

                  --  Get the name of the language

                  Get_Language_Index_Of (Element.Index);

                  case Current_Array.Name is
                     when Name_Dependency_Switches =>

                        --  Attribute Dependency_Switches (<language>)

                        List := Element.Value.Values;

                        if List = Nil_String then
                           Error_Msg
                             ("dependency option cannot be null",
                              Element.Value.Location);
                        end if;

                        Put (Into_List =>
                             Project_Tree.Languages_Data.Table
                               (Current_Language_Index)
                             .Config.Dependency_Option,
                             From_List => List,
                             In_Tree   => Project_Tree);

                     when Name_Dependency_Driver =>

                        --  Attribute Dependency_Driver (<language>)

                        List := Element.Value.Values;

                        if List = Nil_String then
                           Error_Msg
                             ("compute dependency cannot be null",
                              Element.Value.Location);
                        end if;

                        Put (Into_List =>
                             Project_Tree.Languages_Data.Table
                               (Current_Language_Index)
                             .Config.Compute_Dependency,
                             From_List => List,
                             In_Tree   => Project_Tree);

                     when Name_Include_Option =>

                        --  Attribute Include_Option (<language>)

                        List := Element.Value.Values;

                        if List = Nil_String then
                           Error_Msg
                             ("include option cannot be null",
                              Element.Value.Location);
                        end if;

                        Put (Into_List =>
                             Project_Tree.Languages_Data.Table
                               (Current_Language_Index).Config.Include_Option,
                             From_List => List,
                             In_Tree   => Project_Tree);

                     when Name_Include_Path =>

                        --  Attribute Include_Path (<language>)

                        Project_Tree.Languages_Data.Table
                          (Current_Language_Index).Config.Include_Path :=
                          Element.Value.Value;

                     when Name_Include_Path_File =>

                        --  Attribute Include_Path_File (<language>)

                        Project_Tree.Languages_Data.Table
                          (Current_Language_Index).Config.Include_Path_File :=
                          Element.Value.Value;

                     when Name_Driver =>

                        --  Attribute Driver (<language>)

                        Get_Name_String (Element.Value.Value);

                        if Name_Len = 0 then
                           Error_Msg
                             ("compiler driver name cannot be empty",
                              Element.Value.Location);
                        end if;

                        Project_Tree.Languages_Data.Table
                          (Current_Language_Index).Config.Compiler_Driver :=
                          File_Name_Type (Element.Value.Value);

                     when Name_Switches =>

                        --  Attribute Minimum_Compiler_Options (<language>)

                        List := Element.Value.Values;

                        Put (Into_List =>
                             Project_Tree.Languages_Data.Table
                               (Current_Language_Index).Config.
                               Compiler_Min_Options,
                             From_List => List,
                             In_Tree   => Project_Tree);

                     when Name_Pic_Option =>

                        --  Attribute Compiler_Pic_Option (<language>)

                        List := Element.Value.Values;

                        if List = Nil_String then
                           Error_Msg
                             ("compiler PIC option cannot be null",
                              Element.Value.Location);
                        end if;

                        Put (Into_List =>
                             Project_Tree.Languages_Data.Table
                               (Current_Language_Index).Config.
                               Compilation_PIC_Option,
                             From_List => List,
                             In_Tree   => Project_Tree);

                     when Name_Mapping_File_Switches =>

                        --  Attribute Mapping_File_Switches (<language>)

                        List := Element.Value.Values;

                        if List = Nil_String then
                           Error_Msg
                             ("mapping file switches cannot be null",
                              Element.Value.Location);
                        end if;

                        Put (Into_List =>
                             Project_Tree.Languages_Data.Table
                               (Current_Language_Index).Config.
                               Mapping_File_Switches,
                             From_List => List,
                             In_Tree   => Project_Tree);

                     when Name_Mapping_Spec_Suffix =>

                        --  Attribute Mapping_Spec_Suffix (<language>)

                        Project_Tree.Languages_Data.Table
                          (Current_Language_Index)
                          .Config.Mapping_Spec_Suffix :=
                            File_Name_Type (Element.Value.Value);

                     when Name_Mapping_Body_Suffix =>

                        --  Attribute Mapping_Body_Suffix (<language>)

                        Project_Tree.Languages_Data.Table
                          (Current_Language_Index)
                          .Config.Mapping_Body_Suffix :=
                            File_Name_Type (Element.Value.Value);

                     when Name_Config_File_Switches =>

                        --  Attribute Config_File_Switches (<language>)

                        List := Element.Value.Values;

                        if List = Nil_String then
                           Error_Msg
                             ("config file switches cannot be null",
                              Element.Value.Location);
                        end if;

                        Put (Into_List =>
                             Project_Tree.Languages_Data.Table
                               (Current_Language_Index).Config.
                               Config_File_Switches,
                             From_List => List,
                             In_Tree   => Project_Tree);

                     when Name_Objects_Path =>

                        --  Attribute Objects_Path (<language>)

                        Project_Tree.Languages_Data.Table
                          (Current_Language_Index).Config.Objects_Path :=
                          Element.Value.Value;

                     when Name_Objects_Path_File =>

                        --  Attribute Objects_Path_File (<language>)

                        Project_Tree.Languages_Data.Table
                          (Current_Language_Index).Config.Objects_Path_File :=
                          Element.Value.Value;

                     when Name_Config_Body_File_Name =>

                        --  Attribute Config_Body_File_Name (<language>)

                        Project_Tree.Languages_Data.Table
                          (Current_Language_Index).Config.Config_Body :=
                          Element.Value.Value;

                     when Name_Config_Body_File_Name_Pattern =>

                        --  Attribute Config_Body_File_Name_Pattern
                        --  (<language>)

                        Project_Tree.Languages_Data.Table
                          (Current_Language_Index)
                          .Config.Config_Body_Pattern :=
                          Element.Value.Value;

                     when Name_Config_Spec_File_Name =>

                        --  Attribute Config_Spec_File_Name (<language>)

                        Project_Tree.Languages_Data.Table
                          (Current_Language_Index).Config.Config_Spec :=
                          Element.Value.Value;

                     when Name_Config_Spec_File_Name_Pattern =>

                        --  Attribute Config_Spec_File_Name_Pattern
                        --  (<language>)

                        Project_Tree.Languages_Data.Table
                          (Current_Language_Index)
                          .Config.Config_Spec_Pattern :=
                          Element.Value.Value;

                     when Name_Config_File_Unique =>

                        --  Attribute Config_File_Unique (<language>)

                        begin
                           Project_Tree.Languages_Data.Table
                             (Current_Language_Index)
                             .Config.Config_File_Unique :=
                             Boolean'Value
                               (Get_Name_String (Element.Value.Value));
                        exception
                           when Constraint_Error =>
                              Error_Msg
                                ("illegal value gor Config_File_Unique",
                                 Element.Value.Location);
                        end;

                     when others =>
                        null;
                  end case;

                  Element_Id := Element.Next;
               end loop;

               Current_Array_Id := Current_Array.Next;
            end loop;
         end Process_Compiler;

         --------------------
         -- Process_Naming --
         --------------------

         procedure Process_Naming (Attributes : Variable_Id) is
            Attribute_Id : Variable_Id;
            Attribute    : Variable;

         begin
            --  Process non associated array attribute from package Naming

            Attribute_Id := Attributes;
            while Attribute_Id /= No_Variable loop
               Attribute :=
                 Project_Tree.Variable_Elements.Table (Attribute_Id);

               if not Attribute.Value.Default then
                  if Attribute.Name = Name_Separate_Suffix then

                     --  Attribute Separate_Suffix

                     Separate_Suffix := File_Name_Type (Attribute.Value.Value);

                  elsif Attribute.Name = Name_Casing then

                     --  Attribute Casing

                     begin
                        Casing :=
                          Value (Get_Name_String (Attribute.Value.Value));

                     exception
                        when Constraint_Error =>
                           Error_Msg
                             ("invalid value for Casing",
                              Attribute.Value.Location);
                     end;

                  elsif Attribute.Name = Name_Dot_Replacement then

                     --  Attribute Dot_Replacement

                     Dot_Replacement := File_Name_Type (Attribute.Value.Value);

                  end if;
               end if;

               Attribute_Id := Attribute.Next;
            end loop;
         end Process_Naming;

         procedure Process_Naming (Arrays : Array_Id) is
            Current_Array_Id : Array_Id;
            Current_Array    : Array_Data;
            Element_Id       : Array_Element_Id;
            Element          : Array_Element;
         begin
            --  Process the associative array attribute of package Naming

            Current_Array_Id := Arrays;
            while Current_Array_Id /= No_Array loop
               Current_Array := Project_Tree.Arrays.Table (Current_Array_Id);

               Element_Id := Current_Array.Value;
               while Element_Id /= No_Array_Element loop
                  Element := Project_Tree.Array_Elements.Table (Element_Id);

                  --  Get the name of the language

                  Get_Language_Index_Of (Element.Index);

                  case Current_Array.Name is
                     when Name_Specification_Suffix | Name_Spec_Suffix =>

                        --  Attribute Spec_Suffix (<language>)

                        Project_Tree.Languages_Data.Table
                          (Current_Language_Index).Config.
                          Naming_Data.Spec_Suffix :=
                            File_Name_Type (Element.Value.Value);

                     when Name_Implementation_Suffix | Name_Body_Suffix =>

                        --  Attribute Body_Suffix (<language>)

                        Project_Tree.Languages_Data.Table
                          (Current_Language_Index).Config.
                          Naming_Data.Body_Suffix :=
                            File_Name_Type (Element.Value.Value);

                        Project_Tree.Languages_Data.Table
                          (Current_Language_Index).Config.
                          Naming_Data.Separate_Suffix :=
                            File_Name_Type (Element.Value.Value);

                     when others =>
                        null;
                  end case;

                  Element_Id := Element.Next;
               end loop;

               Current_Array_Id := Current_Array.Next;
            end loop;
         end Process_Naming;

         --------------------
         -- Process_Linker --
         --------------------

         procedure Process_Linker (Attributes : Variable_Id) is
            Attribute_Id : Variable_Id;
            Attribute    : Variable;

         begin
            --  Process non associated array attribute from package Linker

            Attribute_Id := Attributes;
            while Attribute_Id /= No_Variable loop
               Attribute :=
                 Project_Tree.Variable_Elements.Table (Attribute_Id);

               if not Attribute.Value.Default then
                  if Attribute.Name = Name_Driver then

                     --  Attribute Linker'Driver: the default linker to use

                     Project_Tree.Config.Linker :=
                       Path_Name_Type (Attribute.Value.Value);

                  elsif
                    Attribute.Name = Name_Required_Switches
                  then

                     --  Attribute Required_Switches: the minimum
                     --  options to use when invoking the linker

                     Put (Into_List =>
                            Project_Tree.Config.Minimum_Linker_Options,
                          From_List => Attribute.Value.Values,
                          In_Tree   => Project_Tree);

                  end if;
               end if;

               Attribute_Id := Attribute.Next;
            end loop;
         end Process_Linker;

      --  Start of processing for Process_Packages

      begin
         Packages := Data.Decl.Packages;
         while Packages /= No_Package loop
            Element := Project_Tree.Packages.Table (Packages);

            case Element.Name is
               when Name_Binder =>

                  --  Process attributes of package Binder

                  Process_Binder (Element.Decl.Arrays);

               when Name_Builder =>

                  --  Process attributes of package Builder

                  Process_Builder (Element.Decl.Attributes);

               when Name_Compiler =>

                  --  Process attributes of package Compiler

                  Process_Compiler (Element.Decl.Arrays);

               when Name_Linker =>

                  --  Process attributes of package Linker

                  Process_Linker (Element.Decl.Attributes);

               when Name_Naming =>

                  --  Process attributes of package Naming

                  Process_Naming (Element.Decl.Attributes);
                  Process_Naming (Element.Decl.Arrays);

               when others =>
                  null;
            end case;

            Packages := Element.Next;
         end loop;
      end Process_Packages;

      ---------------------------
      -- Process_Project_Level --
      ---------------------------

      procedure Process_Project_Level (Attributes : Variable_Id) is
         Attribute_Id : Variable_Id;
         Attribute    : Variable;
         List         : String_List_Id;

      begin
         --  Process non associated array attribute at project level

         Attribute_Id := Attributes;
         while Attribute_Id /= No_Variable loop
            Attribute :=
              Project_Tree.Variable_Elements.Table (Attribute_Id);

            if not Attribute.Value.Default then
               if Attribute.Name = Name_Default_Language then

                  --  Attribute Default_Language: the single language of
                  --  a project when attribute Languages is not specified.

                  Get_Name_String (Attribute.Value.Value);
                  To_Lower (Name_Buffer (1 .. Name_Len));
                  Project_Tree.Default_Language := Name_Find;

               elsif Attribute.Name = Name_Library_Builder then

                  --  Attribute Library_Builder: the application to invoke
                  --  to build libraries.

                  Project_Tree.Config.Library_Builder :=
                    Path_Name_Type (Attribute.Value.Value);

               elsif Attribute.Name = Name_Archive_Builder then

                  --  Attribute Archive_Builder: the archive builder
                  --  (usually "ar") and its minimum options (usually "cr").

                  List := Attribute.Value.Values;

                  if List = Nil_String then
                     Error_Msg
                       ("archive builder cannot be null",
                        Attribute.Value.Location);
                  end if;

                  Put (Into_List => Project_Tree.Config.Archive_Builder,
                       From_List => List,
                       In_Tree   => Project_Tree);

               elsif Attribute.Name = Name_Archive_Indexer then

                  --  Attribute Archive_Indexer: the optional archive
                  --  indexer (usually "ranlib") with its minimum options
                  --  (usually none).

                  List := Attribute.Value.Values;

                  if List = Nil_String then
                     Error_Msg
                       ("archive indexer cannot be null",
                        Attribute.Value.Location);
                  end if;

                  Put (Into_List => Project_Tree.Config.Archive_Indexer,
                       From_List => List,
                       In_Tree   => Project_Tree);

               elsif Attribute.Name = Name_Library_Partial_Linker then

                  --  Attribute Library_Partial_Linker: the optional linker
                  --  driver with its minimum options, to partially link
                  --  archives.

                  List := Attribute.Value.Values;

                  if List = Nil_String then
                     Error_Msg
                       ("partial linker cannot be null",
                        Attribute.Value.Location);
                  end if;

                  Put (Into_List => Project_Tree.Config.Lib_Partial_Linker,
                       From_List => List,
                       In_Tree   => Project_Tree);

               elsif Attribute.Name = Name_Archive_Suffix then
                  Project_Tree.Config.Archive_Suffix :=
                    File_Name_Type (Attribute.Value.Value);

               elsif Attribute.Name = Name_Linker_Executable_Option then

                  --  Attribute Linker_Executable_Option: optional options
                  --  to specify an executable name. Defaults to "-o".

                  List := Attribute.Value.Values;

                  if List = Nil_String then
                     Error_Msg
                       ("linker executable option cannot be null",
                        Attribute.Value.Location);
                  end if;

                  Put (Into_List =>
                         Project_Tree.Config.Linker_Executable_Option,
                       From_List => List,
                       In_Tree   => Project_Tree);

               elsif Attribute.Name = Name_Linker_Lib_Dir_Option then

                  --  Attribute Linker_Lib_Dir_Option: optional options
                  --  to specify a library search directory. Defaults to
                  --  "-L".

                  Get_Name_String (Attribute.Value.Value);

                  if Name_Len = 0 then
                     Error_Msg
                       ("linker library directory option cannot be empty",
                        Attribute.Value.Location);
                  end if;

                  Project_Tree.Config.Linker_Lib_Dir_Option :=
                    Attribute.Value.Value;

               elsif Attribute.Name = Name_Linker_Lib_Name_Option then

                  --  Attribute Linker_Lib_Name_Option: optional options
                  --  to specify the name of a library to be linked in.
                  --  Defaults to "-l".

                  Get_Name_String (Attribute.Value.Value);

                  if Name_Len = 0 then
                     Error_Msg
                       ("linker library name option cannot be empty",
                        Attribute.Value.Location);
                  end if;

                  Project_Tree.Config.Linker_Lib_Name_Option :=
                    Attribute.Value.Value;

               elsif Attribute.Name = Name_Run_Path_Option then

                  --  Attribute Run_Path_Option: optional options to
                  --  specify a path for libraries.

                  List := Attribute.Value.Values;

                  if List /= Nil_String then
                     Put (Into_List => Project_Tree.Config.Run_Path_Option,
                          From_List => List,
                          In_Tree   => Project_Tree);
                  end if;

               elsif Attribute.Name = Name_Library_Support then
                  declare
                     pragma Unsuppress (All_Checks);
                  begin
                     Project_Tree.Config.Lib_Support :=
                       Library_Support'Value (Get_Name_String
                                              (Attribute.Value.Value));
                  exception
                     when Constraint_Error =>
                        Error_Msg
                          ("invalid value """ &
                           Get_Name_String (Attribute.Value.Value) &
                           """ for Library_Support",
                           Attribute.Value.Location);
                  end;

               elsif Attribute.Name = Name_Shared_Library_Prefix then
                  Project_Tree.Config.Shared_Lib_Prefix :=
                    File_Name_Type (Attribute.Value.Value);

               elsif Attribute.Name = Name_Shared_Library_Suffix then
                  Project_Tree.Config.Shared_Lib_Suffix :=
                    File_Name_Type (Attribute.Value.Value);

               elsif Attribute.Name = Name_Symbolic_Link_Supported then
                  declare
                     pragma Unsuppress (All_Checks);
                  begin
                     Project_Tree.Config.Symbolic_Link_Supported :=
                       Boolean'Value (Get_Name_String
                                      (Attribute.Value.Value));
                  exception
                     when Constraint_Error =>
                        Error_Msg
                          ("invalid value """ &
                           Get_Name_String (Attribute.Value.Value) &
                           """ for Symbolic_Link_Supported",
                           Attribute.Value.Location);
                  end;

               elsif
                 Attribute.Name = Name_Library_Major_Minor_Id_Supported
               then
                  declare
                     pragma Unsuppress (All_Checks);
                  begin
                     Project_Tree.Config.Lib_Maj_Min_Id_Supported :=
                       Boolean'Value (Get_Name_String
                                      (Attribute.Value.Value));
                  exception
                     when Constraint_Error =>
                        Error_Msg
                          ("invalid value """ &
                           Get_Name_String (Attribute.Value.Value) &
                           """ for Library_Major_Minor_Id_Supported",
                           Attribute.Value.Location);
                  end;

               elsif
                 Attribute.Name = Name_Library_Auto_Init_Supported
               then
                  declare
                     pragma Unsuppress (All_Checks);
                  begin
                     Project_Tree.Config.Auto_Init_Supported :=
                       Boolean'Value (Get_Name_String
                                      (Attribute.Value.Value));
                  exception
                     when Constraint_Error =>
                        Error_Msg
                          ("invalid value """ &
                           Get_Name_String (Attribute.Value.Value) &
                           """ for Library_Auto_Init_Supported",
                           Attribute.Value.Location);
                  end;

               elsif
                 Attribute.Name = Name_Shared_Library_Minimum_Switches
               then
                  List := Attribute.Value.Values;

                  if List /= Nil_String then
                     Put (Into_List =>
                            Project_Tree.Config.Shared_Lib_Min_Options,
                          From_List => List,
                          In_Tree   => Project_Tree);
                  end if;

               elsif
                 Attribute.Name = Name_Library_Version_Switches
               then
                  List := Attribute.Value.Values;

                  if List /= Nil_String then
                     Put (Into_List =>
                            Project_Tree.Config.Lib_Version_Options,
                          From_List => List,
                          In_Tree   => Project_Tree);
                  end if;
               end if;
            end if;

            Attribute_Id := Attribute.Next;
         end loop;
      end Process_Project_Level;

      procedure Process_Project_Level (Arrays : Array_Id) is
         Current_Array_Id : Array_Id := Arrays;
         Current_Array    : Array_Data;
         Element_Id       : Array_Element_Id;
         Element          : Array_Element;

      begin
         --  Process the associative array attributes at project level

         while Current_Array_Id /= No_Array loop
            Current_Array := Project_Tree.Arrays.Table (Current_Array_Id);

            Element_Id := Current_Array.Value;
            while Element_Id /= No_Array_Element loop
               Element := Project_Tree.Array_Elements.Table (Element_Id);

               --  Get the name of the language

               Get_Language_Index_Of (Element.Index);

               case Current_Array.Name is

                  when Name_Toolchain_Description =>

                     --  Attribute Toolchain_Description (<language>)

                     Project_Tree.Languages_Data.Table
                       (Current_Language_Index).Config.Toolchain_Description :=
                       Element.Value.Value;

                  when Name_Toolchain_Version =>

                     --  Attribute Toolchain_Version (<language>)

                     Project_Tree.Languages_Data.Table
                       (Current_Language_Index).Config.Toolchain_Version :=
                       Element.Value.Value;

                  when others =>
                     null;
               end case;

               Element_Id := Element.Next;
            end loop;

            Current_Array_Id := Current_Array.Next;
         end loop;
      end Process_Project_Level;

      --------------------
      -- Report_Failure --
      --------------------

      procedure Report_Failure (S : String; N : Name_Id) is
      begin
         OK := False;

         if Fail_If_Error then
            Write_Str (S);

            if N /= No_Name then
               Write_Str (Get_Name_String (N));
            end if;

            Write_Eol;
         end if;
      end Report_Failure;

   --  Start of processing for Get_Configuration

   begin
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
      --  Locate main configuration project file

      Configuration_Project_Path :=
        Locate_Regular_File
          (Config_Project_File_Name.all,
           Config_Path.all);

      if Configuration_Project_Path = null then
         if Fail_If_Error then
            Osint.Fail
              ("could not locate main configuration project ",
               Config_Project_File_Name.all);
         else
            return;
         end if;
      end if;

      --  Parse the configuration project tree

      Set_In_Configuration (True);
      Prj.Initialize (Project_Tree);

      if Verbose_Mode then
         Write_Line ("Checking configuration");
         Write_Line (Configuration_Project_Path.all);
      end if;

      Prj.Pars.Parse
        (Project_Tree, Main_Config_Project, Configuration_Project_Path.all);

      if Main_Config_Project = No_Project then
         if Fail_If_Error then
            Osint.Fail
              ("processing of configuration project """,
               Configuration_Project_Path.all,
               """ failed");

         else
            Cleanup;
            return;
         end if;
      end if;

      --  Process the imported configuration project files

      List :=
        Project_Tree.Projects.Table (Main_Config_Project).Imported_Projects;
      while List /= Empty_Project_List loop
         Element := Project_Tree.Project_Lists.Table (List);

         Process_Packages (Element.Project);

         List := Element.Next;
      end loop;

      --  Finally, process the main configuration project file

      Process_Project_Level
        (Attributes => Project_Tree.Projects.Table
           (Main_Config_Project).Decl.Attributes);

      Process_Project_Level
        (Arrays => Project_Tree.Projects.Table
           (Main_Config_Project).Decl.Arrays);

      Process_Packages (Main_Config_Project);

      --  For unit based languages, set Casing, Dot_Replacement and
      --  Separate_Suffix in Naming_Data.

      Current_Language_Index := Project_Tree.First_Language;
      while Current_Language_Index /= No_Language_Index loop
         if Project_Tree.Languages_Data.Table
           (Current_Language_Index).Config.Kind = Unit_Based
         then
            Project_Tree.Languages_Data.Table
              (Current_Language_Index).Config.Naming_Data.Casing := Casing;
            Project_Tree.Languages_Data.Table
              (Current_Language_Index).Config.Naming_Data.Dot_Replacement :=
              Dot_Replacement;

            if Separate_Suffix /= No_File then
               Project_Tree.Languages_Data.Table
                 (Current_Language_Index).Config.Naming_Data.Separate_Suffix :=
                 Separate_Suffix;
            end if;
         end if;

         Current_Language_Index :=
           Project_Tree.Languages_Data.Table (Current_Language_Index).Next;
      end loop;

      --  Check configuration

      OK := True;

      if Project_Tree.First_Language = No_Language_Index then
         Report_Failure ("No language data in configuration", No_Name);
      end if;

      --  Give empty names to various prefixes/suffixes, if they have not
      --  been specified in the configuration.

      if Project_Tree.Config.Archive_Suffix = No_File then
         Project_Tree.Config.Archive_Suffix := Empty_File;
      end if;

      if Project_Tree.Config.Shared_Lib_Prefix = No_File then
         Project_Tree.Config.Shared_Lib_Prefix := Empty_File;
      end if;

      if Project_Tree.Config.Shared_Lib_Suffix = No_File then
         Project_Tree.Config.Shared_Lib_Suffix := Empty_File;
      end if;

      if OK then
         Lang_Index := Project_Tree.First_Language;
         while Lang_Index /= No_Language_Index loop
            Lang_Data := Project_Tree.Languages_Data.Table (Lang_Index);

            Current_Language := Lang_Data.Display_Name;

            if Lang_Data.Config.Kind = Unit_Based then

               --  For unit based languages, Dot_Replacement, Spec_Suffix and
               --  Body_Suffix need to be specified.

               if Lang_Data.Config.Naming_Data.Dot_Replacement = No_File then
                  Report_Failure
                    ("Dot_Replacement not specified for ", Current_Language);
               end if;

               if Lang_Data.Config.Naming_Data.Spec_Suffix = No_File then
                  Report_Failure
                    ("Spec_Suffix not specified for ", Current_Language);
               end if;

               if Lang_Data.Config.Naming_Data.Body_Suffix = No_File then
                  Report_Failure
                    ("Body_Suffix not specified for ", Current_Language);
               end if;

            else
               --  For file based languages, either Spec_Suffix or Body_Suffix
               --  need to be specified.

               if Lang_Data.Config.Naming_Data.Spec_Suffix = No_File and then
                 Lang_Data.Config.Naming_Data.Body_Suffix = No_File
               then
                  Report_Failure
                    ("no suffixes specified for ", Current_Language);
               end if;
            end if;

            --  For all languages, Compiler_Driver needs to be specified

            if Lang_Data.Config.Compiler_Driver = No_File then
               Report_Failure
                 ("no compiler specified for ", Current_Language);
            end if;

            Lang_Index := Lang_Data.Next;
         end loop;
      end if;

      if not OK then
         if Fail_If_Error then
            Osint.Fail ("language configuration incorrect");
         end if;
      end if;

      --  Cleanup

      Cleanup;
   end Get_Configuration;

   ---------------------------
   -- Get_Language_Index_Of --
   ---------------------------

   procedure Get_Language_Index_Of (Language : Name_Id) is
      Curr_Index : Language_Index;
      Real_Language : Name_Id;

   begin
      Get_Name_String (Language);
      To_Lower (Name_Buffer (1 .. Name_Len));
      Real_Language := Name_Find;

      --  Nothing to do if the language is the same as the current language

      if Current_Language /= Real_Language then
         Current_Language := Real_Language;

         Curr_Index := Project_Tree.First_Language;
         while Curr_Index /= No_Language_Index loop
            if Project_Tree.Languages_Data.Table (Curr_Index).Name =
              Real_Language
            then
               Current_Language_Index := Curr_Index;
               return;
            end if;

            Curr_Index :=
              Project_Tree.Languages_Data.Table (Curr_Index).Next;
         end loop;

         Language_Data_Table.Increment_Last (Project_Tree.Languages_Data);
         Current_Language_Index :=
           Language_Data_Table.Last (Project_Tree.Languages_Data);
         Project_Tree.Languages_Data.Table (Current_Language_Index) :=
           (Name          => Real_Language,
            Display_Name  => Language,
            Config        => No_Language_Config,
            First_Source  => No_Source,
            Mapping_Files => Mapping_Files_Htable.Nil,
            Next          => Project_Tree.First_Language);
         Project_Tree.First_Language := Current_Language_Index;

         if Real_Language = Name_Ada then
            Project_Tree.Languages_Data.Table
              (Current_Language_Index).Config.Kind := Unit_Based;
            Project_Tree.Languages_Data.Table
              (Current_Language_Index).Config.Dependency_Kind := ALI_File;
         end if;
      end if;
   end Get_Language_Index_Of;

end Confgpr;
