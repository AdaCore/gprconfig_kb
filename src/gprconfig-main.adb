------------------------------------------------------------------------------
--                   Copyright (C) 2006, AdaCore                            --
------------------------------------------------------------------------------

with Ada.Containers;            use Ada.Containers;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Ada.Text_IO;               use Ada.Text_IO;
with GNAT.Command_Line;         use GNAT.Command_Line;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Strings;
with GprConfig.Knowledge;       use GprConfig.Knowledge;
with GprConfig.Sdefault;

procedure GprConfig.Main is
   Gprmake : constant String := "gprmake";
   --  Name of the gprmake executable. This is searched for on PATH, and used
   --  to find out the default location for the output file

   Default_Output_File : constant String := "standard.gpr";
   --  Name of the configuration file used by gprmake by default

   Output_File : Unbounded_String;

   type Boolean_Array  is array (Natural range <>) of Boolean;
   type Compiler_Array is array (Natural range <>) of Compiler;

   use Compiler_Lists;

   function Get_Database_Directory return String;
   --  Return the location of the knowledge database

   procedure Help;
   --  Display list of switches

   procedure Display_Compiler
     (Comp : Compiler; Selected : Boolean; Index : Positive);
   --  Display a line describing the compiler

   procedure To_List
     (Comps              : Compiler_Array;
      Selected           : Boolean_Array;
      Selected_Compilers : out Compiler_Lists.List);
   --  Return the list of selected compilers in Comps

   procedure Enter_Custom_Target (Base : Knowledge_Base; Comp : out Compiler);
   --  Ask the user for a custom compiler

   procedure Parse_Config_Parameter
     (Custom_Comps : in out Compiler_Lists.List;
      Config       : String);
   --  Parse the -config parameter, and store the (partial) information found
   --  there in Selected_Compilers

   procedure Select_Compilers_Interactively
     (Base               : Knowledge_Base;
      Selected_Compilers : in out Compiler_Lists.List;
      Custom_Comps       : in out Compiler_Lists.List);
   --  Ask the user for compilers to be selected

   procedure Complete_Command_Line_Compilers
     (Base         : Knowledge_Base;
      Custom_Comps : in out Compiler_Lists.List);
   --  Complete missing information for the compilers specified on the command
   --  line.

   procedure Show_Command_Line_Config (Selected_Comps : Compiler_Lists.List);
   --  Display the batch command line that would have the same effect as the
   --  current selection of compilers.

   function "<" (Comp1, Comp2 : Compiler) return Boolean;
   --  Compare two compilers, so that similar languages are grouped together

   ----------
   -- Help --
   ----------

   procedure Help is
   begin
      Put_Line (" -o file : Name and directory of the output file");
      Put_Line ("           default is " & To_String (Output_File));
      Put_Line (" -db dir : Parse dir as an additional knowledge base");
      Put_Line (" -db-    : Do not load the standard knowledge base from");
      Put_Line ("          " & Get_Database_Directory);
      Put_Line (" -config name,path[,version[,language[,target[,runtime]]]]");
      Put_Line ("           Preselect a compiler. When name is one of the"
                & " names known to gprconfig,");
      Put_Line ("           you do not need to provide any of the optional"
                & " parameter, and can leave an");
      Put_Line ("           empty string instead");
      Put_Line (" -batch  : batch mode, no interactive compiler selection");
      Put_Line (" -v      : verbose mode");
   end Help;

   ----------------------------
   -- Get_Database_Directory --
   ----------------------------

   function Get_Database_Directory return String is
      Prog_Dir : constant String := Get_Program_Directory;
      Suffix : constant String := "share" & Directory_Separator & "gprconfig";
   begin
      return Prog_Dir & Suffix;
   end Get_Database_Directory;

   ----------------------
   -- Display_Compiler --
   ----------------------

   procedure Display_Compiler
     (Comp : Compiler; Selected : Boolean; Index : Positive) is
   begin
      if Selected then
         Put ('*');
      else
         Put (' ');
      end if;

      Put
        ("("
         & Character'Val (Index + Character'Pos ('A') - 1) & ") "
         & To_String (Comp.Name)
         & " " & To_String (Comp.Version)
         & " (" & To_String (Comp.Language));
      if Comp.Runtime /= Null_Unbounded_String then
         Put (',' & To_String (Comp.Runtime) & " runtime");
      end if;

      Put (") ");
      if Comp.Target /= Sdefault.Hostname
        and then Comp.Target /= Null_Unbounded_String
      then
         Put (To_String (Comp.Target) & ' ');
      end if;

      Put_Line ("- " & To_String (Comp.Path));
   end Display_Compiler;

   -------------
   -- To_List --
   -------------

   procedure To_List
     (Comps              : Compiler_Array;
      Selected           : Boolean_Array;
      Selected_Compilers : out Compiler_Lists.List) is
   begin
      for Index in Comps'Range loop
         if Selected (Index) then
            Append (Selected_Compilers, Comps (Index));
         end if;
      end loop;
   end To_List;

   -------------------------
   -- Enter_Custom_Target --
   -------------------------

   procedure Enter_Custom_Target
     (Base : Knowledge_Base; Comp : out Compiler)
   is
      Default_Path : constant String := "/usr/bin/";
      Line : String (1 .. 4096);
      Last : Natural;
      Completion : Compiler_Lists.List;
      Complete : Compiler := No_Compiler;
      Known_Compilers : Unbounded_String;
   begin
      Known_Compiler_Names (Base, Known_Compilers);

      Comp := No_Compiler;
      Put ("Configuration name (" & To_String (Known_Compilers) & "): ");
      Get_Line (Line, Last);
      Comp.Name := To_Unbounded_String (Line (Line'First .. Last));

      Put ("Installation directory [" & Default_Path & "]: ");
      Get_Line (Line, Last);
      if Last = 0 then
         Comp.Path := To_Unbounded_String (Default_Path);
      else
         Comp.Path := To_Unbounded_String (Line (Line'First .. Last));
      end if;

      Find_Matching_Compilers
        (Name      => To_String (Comp.Name),
         Path      => To_String (Comp.Path),
         Base      => Base,
         Compilers => Completion);
      if not Is_Empty (Completion) then
         Complete := Element (First (Completion));
      end if;

      Put ("Version [" & To_String (Complete.Version) & "]: ");
      Get_Line (Line, Last);
      if Last = 0 then
         Comp.Version := Complete.Version;
      else
         Comp.Version := TU (Line (Line'First .. Last));
      end if;

      Put ("Language [" & To_String (Complete.Language) & "]: ");
      Get_Line (Line, Last);
      if Last = 0 then
         Comp.Language := Complete.Language;
      else
         Comp.Language := TU (Line (Line'First .. Last));
      end if;

      Put ("Runtime [" & To_String (Complete.Runtime) & "]: ");
      Get_Line (Line, Last);
      if Last = 0 then
         Comp.Runtime := Complete.Runtime;
      else
         Comp.Runtime := TU (Line (Line'First .. Last));
      end if;

      if Comp.Runtime /= Null_Unbounded_String then
         Put
           ("Runtime directory [" & To_String (Complete.Runtime_Dir) & "]: ");
         Get_Line (Line, Last);
         if Last = 0 then
            Comp.Runtime_Dir := Complete.Runtime_Dir;
         else
            Comp.Runtime_Dir := TU (Line (Line'First .. Last));
         end if;
      end if;

      if Complete.Target = Null_Unbounded_String then
         Put ("Target [" & Sdefault.Hostname & "]: ");
      else
         Put ("Target [" & To_String (Complete.Target) & "]: ");
      end if;

      Get_Line (Line, Last);
      if Last = 0 then
         if Complete.Target = Null_Unbounded_String then
            Comp.Target := To_Unbounded_String (Sdefault.Hostname);
         else
            Comp.Target := Complete.Target;
         end if;
      else
         Comp.Target := TU (Line (Line'First .. Last));
      end if;
   end Enter_Custom_Target;

   ----------------------------
   -- Parse_Config_Parameter --
   ----------------------------

   procedure Parse_Config_Parameter
     (Custom_Comps : in out Compiler_Lists.List;
      Config       : String)
   is
      use String_Lists;
      Map  : String_Lists.List;
      C    : String_Lists.Cursor;
      Comp : Compiler;
   begin
      Get_Words (Config, Filter => Null_Unbounded_String, Map => Map,
                 Allow_Empty_Elements => True);
      if Length (Map) < 2 then
         return;  --  we need at least a name and a path
      end if;

      C := First (Map);
      Comp.Name := TU (Element (C));
      Next (C);
      Comp.Path := TU (Element (C));

      Next (C);
      if Has_Element (C) then
         Comp.Version := TU (Element (C));
         Next (C);
         if Has_Element (C) then
            Comp.Language := TU (Element (C));
            Next (C);
            if Has_Element (C) then
               Comp.Target := TU (Element (C));
               Next (C);
               if Has_Element (C) then
                  Comp.Runtime := TU (Element (C));
               end if;
            end if;
         end if;
      end if;

      Append (Custom_Comps, Comp);
   end Parse_Config_Parameter;

   ---------
   -- "<" --
   ---------

   function "<" (Comp1, Comp2 : Compiler) return Boolean is
   begin
      if Comp1.Language < Comp2.Language then
         return True;
      elsif Comp1.Language > Comp2.Language then
         return False;
      else
         return Comp1.Path < Comp2.Path;
      end if;
   end "<";

   ------------------------------------
   -- Select_Compilers_Interactively --
   ------------------------------------

   procedure Select_Compilers_Interactively
     (Base               : Knowledge_Base;
      Selected_Compilers : in out Compiler_Lists.List;
      Custom_Comps       : in out Compiler_Lists.List)
   is
      package Compiler_Sort is new Compiler_Lists.Generic_Sorting ("<");
      Compilers : Compiler_Lists.List;
   begin
      Find_Compilers_In_Path (Base, Compilers);
      Compiler_Sort.Sort (Compilers);

      declare
         Compilers_Count : constant Natural := Natural (Length (Compilers));
         Selected : Boolean_Array  (1 .. Compilers_Count) := (others => False);
         Selectable : Boolean_Array  (1 .. Compilers_Count) :=
           (others => True);
         Selected_Count  : Natural := 0;
         Comps           : Compiler_Array (1 .. Compilers_Count);
         Comp            : Compiler_Lists.Cursor := First (Compilers);
         Index, Tmp      : Natural;
         Choice          : Character;
         Selected_Target : Unbounded_String;
         Line            : String (1 .. 1024);
      begin
         for C in Comps'Range loop
            Comps (C) := Element (Comp);
            Next (Comp);
         end loop;

         loop
            Put_Line ("--------------------------------------------------");
            Put_Line
              ("gprconfig has detected the following known compilers"
               & " on your PATH:");
            Index := 1;
            while Index <= Comps'Last loop
               if Selectable (Index) or Selected (Index) then
                  Display_Compiler (Comps (Index), Selected (Index), Index);
               end if;
               Index := Index + 1;
            end loop;

            Comp := First (Custom_Comps);
            while Has_Element (Comp) loop
               Display_Compiler (Element (Comp), True, Index);
               Index := Index + 1;
               Next (Comp);
            end loop;

            Put_Line (" (o) Other compiler");
            Put_Line (" (s) Generate configuration file");
            Put ("Toggle selection for: ");
            Get_Line (Line, Tmp);
            if Tmp = 0 then
               Choice := ASCII.NUL;
            else
               Choice := Line (Line'First);
            end if;

            if Choice = 'o' then
               --  Update Selected_Target if needed
               declare
                  Comp : Compiler;
               begin
                  Enter_Custom_Target (Base, Comp);
                  Append (Custom_Comps, Comp);

                  Selected_Count := Selected_Count + 1;
                  if Selected_Count = 1 then
                     Selected_Target := Comp.Target;
                  end if;
               end;

            elsif Choice = 's' then
               exit;

            else
               --  Selected one of the compilers we found ?
               Tmp := Character'Pos (Choice) - Character'Pos ('A') + 1;

               if Tmp in 1 .. Comps'Last then
                  Selected (Tmp) := not Selected (Tmp);

                  if Selected (Tmp) then
                     Selected_Count := Selected_Count + 1;
                     if Selected_Count = 1 then
                        Selected_Target := Comps (Tmp).Target;
                     end if;
                  else
                     Selected_Count := Selected_Count - 1;
                  end if;

                  --  Selected one of the custom compilers
               elsif Tmp in Comps'Last + 1 .. Index - 1 then
                  Selected_Count := Selected_Count - 1;
                  Comp := First (Custom_Comps);
                  Tmp := Tmp - Comps'Last;
                  while Tmp > 0 and then Has_Element (Comp) loop
                     Tmp := Tmp - 1;
                     Next (Comp);
                  end loop;
                  Delete (Custom_Comps, Comp);

               else
                  --  Invalid choice
                  null;
               end if;

               if Selected_Count = 0 then
                  Selected_Target := Null_Unbounded_String;
               end if;

               --  Only keep those compilers that might be compatible with the
               --  current selection. For instance, if we won't know how to
               --  link sources compiled with A and sources compiled with B,
               --  and A is selected, there is no point in showing B.
               --
               --  Only keep other compilers with the same target, since
               --  otherwise linking makes no sense.

               Put_Line
                 ("Filtering the list to only show compatible compilers");
               for C in Comps'Range loop
                  if not Selected (C) then
                     Selected_Compilers := Custom_Comps;
                     To_List (Comps, Selected, Selected_Compilers);
                     Append (Selected_Compilers, Comps (C));
                     Selectable (C) :=
                       (Selected_Count = 0
                        or else Architecture_Equal
                          (To_String (Selected_Target),
                           To_String (Comps (C).Target)))
                       and then Is_Supported_Config (Base, Selected_Compilers);
                  end if;
               end loop;
            end if;
         end loop;

         Selected_Compilers := Custom_Comps;
         To_List (Comps, Selected, Selected_Compilers);
      end;
   end Select_Compilers_Interactively;

   -------------------------------------
   -- Complete_Command_Line_Compilers --
   -------------------------------------

   procedure Complete_Command_Line_Compilers
     (Base         : Knowledge_Base;
      Custom_Comps : in out Compiler_Lists.List)
   is
      procedure Update_Comps (Elem : in out Compiler);
      --  Update element with the appropriate info from the knowledge base

      procedure Update_Comps (Elem : in out Compiler) is
         Completion : Compiler_Lists.List;
      begin
         Find_Matching_Compilers
           (Name      => To_String (Elem.Name),
            Path      => To_String (Elem.Path),
            Base      => Base,
            Compilers => Completion);
         if not Is_Empty (Completion) then
            if Elem.Version = Null_Unbounded_String then
               Elem.Version := Element (First (Completion)).Version;
            end if;
            if Elem.Language = Null_Unbounded_String then
               Elem.Language := Element (First (Completion)).Language;
            end if;
            if Elem.Runtime = Null_Unbounded_String then
               Elem.Runtime := Element (First (Completion)).Runtime;
            end if;
            if Elem.Runtime_Dir = Null_Unbounded_String then
               Elem.Runtime_Dir := Element (First (Completion)).Runtime_Dir;
            end if;
            if Elem.Target = Null_Unbounded_String then
               Elem.Target := Element (First (Completion)).Target;
            end if;
            if Elem.Extra_Tool = Null_Unbounded_String then
               Elem.Extra_Tool := Element (First (Completion)).Extra_Tool;
            end if;
         end if;
      end Update_Comps;

      C : Compiler_Lists.Cursor := First (Custom_Comps);
   begin
      while Has_Element (C) loop
         Update_Element (Custom_Comps, C, Update_Comps'Unrestricted_Access);
         Next (C);
      end loop;
   end Complete_Command_Line_Compilers;

   ------------------------------
   -- Show_Command_Line_Config --
   ------------------------------

   procedure Show_Command_Line_Config (Selected_Comps : Compiler_Lists.List) is
      C : Compiler_Lists.Cursor;
   begin
      if not Is_Empty (Selected_Comps) then
         New_Line;
         Put_Line ("You can regenerate the same config file in batch mode");
         Put_Line (" with the following command line:");
         Put ("gprconfig -batch");

         C := First (Selected_Comps);
         while Has_Element (C) loop
            Put
              (" -config " & To_String (Element (C).Name)
               & "," & To_String (Element (C).Path)
               & "," & To_String (Element (C).Version)
               & "," & To_String (Element (C).Language)
               & "," & To_String (Element (C).Target)
               & "," & To_String (Element (C).Runtime));
            Next (C);
         end loop;
         New_Line;
         New_Line;
      end if;
   end Show_Command_Line_Config;

   Base               : Knowledge_Base;
   Selected_Compilers : Compiler_Lists.List;
   Custom_Comps       : Compiler_Lists.List;
   Load_Standard_Base : Boolean := True;
   Batch              : Boolean := False;

   --  We need to add the executable suffix here, since on windows,
   --  Locate_Exec_On_Path will also return directories with the name "gprmake"
   --  ie the current directory when gprconfig is run from the current dir.
   Exec_Suffix        : constant GNAT.Strings.String_Access :=
     Get_Executable_Suffix;
   Gprmake_Path : GNAT.OS_Lib.String_Access :=
     Locate_Exec_On_Path (Gprmake & Exec_Suffix.all);

begin
   if Gprmake_Path /= null  then
      Output_File := To_Unbounded_String
        (Normalize_Pathname (Dir_Name (Gprmake_Path.all) & "..")
         & Directory_Separator & "share"
         & Directory_Separator & "gpr" & Directory_Separator
         & Default_Output_File);
   end if;
   Free (Gprmake_Path);

   loop
      case Getopt ("batch config: db: h: o: v") is
         when 'b' =>
            Batch := True;

         when 'c' =>
            Parse_Config_Parameter (Custom_Comps, Parameter);

         when 'd' =>
            if Parameter = "-" then
               Load_Standard_Base := False;
            else
               Parse_Knowledge_Base (Base, Parameter);
            end if;

         when 'h' =>
            Help;
            return;

         when 'o' =>
            Output_File := To_Unbounded_String (Parameter);

         when 'v' =>
            Verbose_Mode := True;

         when others =>
            exit;
      end case;
   end loop;

   if Load_Standard_Base then
      Parse_Knowledge_Base (Base, Get_Database_Directory);
   end if;

   Complete_Command_Line_Compilers (Base, Custom_Comps);

   if not Batch then
      Select_Compilers_Interactively
        (Base, Selected_Compilers, Custom_Comps);
      Show_Command_Line_Config (Selected_Compilers);
   else
      Selected_Compilers := Custom_Comps;
   end if;

   if Output_File /= Null_Unbounded_String then
      Generate_Configuration
        (Base, Selected_Compilers, To_String (Output_File));
   end if;

exception
   when End_Error =>
      null;
   when Invalid_Switch | Invalid_Parameter =>
      Help;
end GprConfig.Main;
