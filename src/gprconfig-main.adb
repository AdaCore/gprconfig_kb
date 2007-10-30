------------------------------------------------------------------------------
--                   Copyright (C) 2006-2007, AdaCore                       --
------------------------------------------------------------------------------

with Ada.Characters.Handling;  use Ada.Characters.Handling;
with Ada.Command_Line;
with Ada.Containers;            use Ada.Containers;
with Ada.Exceptions;            use Ada.Exceptions;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Ada.Text_IO;               use Ada.Text_IO;
with GNAT.Command_Line;         use GNAT.Command_Line;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Strings;
with GprConfig.Knowledge;       use GprConfig.Knowledge;
with GprConfig.Sdefault;

procedure GprConfig.Main is
   Gprbuild : constant String := "gprbuild";
   --  Name of the gprbuild executable. This is searched for on PATH, and used
   --  to find out the default location for the output file

   Default_Output_File : constant String := "default.cgpr";
   --  Name of the configuration file used by gprbuild by default

   Output_File : Unbounded_String;

   Selected_Target : Unbounded_String;
   --  Value of --target switch.

   Selected_Targets_Set : Targets_Set_Id;
   --  Targets set id for the selected target.

   Invalid_Config : exception;

   type Boolean_Array  is array (Natural range <>) of Boolean;
   type Compiler_Array is array (Natural range <>) of Compiler;

   use Compiler_Lists;

   function Get_Database_Directory return String;
   --  Return the location of the knowledge database

   procedure Help (Base : Knowledge_Base);
   --  Display list of switches

   procedure Display_Compiler
     (Comp : Compiler; Selected : Boolean; Index : Positive);
   --  Display a line describing the compiler

   procedure To_List
     (Comps              : Compiler_Array;
      Selected           : Boolean_Array;
      Selected_Compilers : in out Compiler_Lists.List);
   --  Return the list of selected compilers in Comps

   procedure Parse_Config_Parameter
     (Custom_Comps : in out Compiler_Lists.List;
      Config       : String);
   --  Parse the --config parameter, and store the (partial) information
   --  found there in Selected_Compilers

   procedure Select_Compilers_Interactively
     (Base               : in out Knowledge_Base;
      Compilers          : Compiler_Lists.List;
      Selected_Compilers : in out Compiler_Lists.List;
      Custom_Comps       : in out Compiler_Lists.List);
   --  Ask the user for compilers to be selected

   procedure Complete_Command_Line_Compilers
     (Base         : in out Knowledge_Base;
      On_Target    : Targets_Set_Id;
      Filters      : Compiler_Lists.List;
      Custom_Comps : out Compiler_Lists.List);
   --  In batch mode, the --config parameters indicate what compilers should be
   --  selected. Each of these switch selects the first matching compiler
   --  available, and all --config switch must match a compiler.
   --  This procedure is used to find matching compilers, and complete info
   --  like their version, runtime,... It should only be called in batch mode,
   --  since otherwise --config only acts as a filter for the compilers that
   --  are found through the knowledge base.

   procedure Filter_Compilers
     (Selected_Comps : in out Compiler_Lists.List;
      Compilers      : Compiler_Lists.List;
      Filters        : Compiler_Lists.List);
   --  For every element of Filters appends the first matching element in
   --  Compilers to Selected_Comps.  If none is found emits a warning.

   procedure Show_Command_Line_Config (Selected_Comps : Compiler_Lists.List);
   --  Display the batch command line that would have the same effect as the
   --  current selection of compilers.

   function "<" (Comp1, Comp2 : Compiler) return Boolean;
   --  Compare two compilers, so that similar languages are grouped together

   ----------
   -- Help --
   ----------

   procedure Help (Base : Knowledge_Base) is
      Known : Unbounded_String;
   begin
      Known_Compiler_Names (Base, Known);

      Put_Line (" --target=target (" & Sdefault.Hostname & " by default)");
      Put_Line ("            Select specified target or all for any target.");
      Put_Line (" -o file  : Name and directory of the output file.");
      Put_Line ("            default is " & To_String (Output_File));
      Put_Line (" --db dir : Parse dir as an additional knowledge base.");
      Put_Line (" --db-    : Do not load the standard knowledge base from:");
      Put_Line ("   " & Get_Database_Directory);
      Put_Line (" --config=language[,version[,runtime[,path[,name]]]]");
      Put_Line ("            Preselect a compiler. When name is one of the"
                & " names known to ");
      Put_Line ("            gprconfig, you do not need to provide any of the"
                & " optional parameter,");
      Put_Line ("            and can leave an empty string instead.");
      Put_Line ("            The known compilers are: " & To_String (Known));
      Put_Line (" --batch  : batch mode, no interactive compiler selection.");
      Put_Line (" -v       : verbose mode.");
      Put_Line (" -q       : quiet output.");
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
      if Comp.Target /= Null_Unbounded_String then
         Put (To_String (Comp.Target) & ' ');
      end if;

      Put_Line ("- " & Name_As_Directory (To_String (Comp.Path))
                & To_String (Comp.Executable));
   end Display_Compiler;

   -------------
   -- To_List --
   -------------

   procedure To_List
     (Comps              : Compiler_Array;
      Selected           : Boolean_Array;
      Selected_Compilers : in out Compiler_Lists.List) is
   begin
      for Index in Comps'Range loop
         if Selected (Index) then
            Append (Selected_Compilers, Comps (Index));
         end if;
      end loop;
   end To_List;

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

      C := First (Map);
      Comp.Language := TU (Element (C));
      Next (C);
      if Has_Element (C) then
         Comp.Version := TU (Element (C));
         Next (C);
         if Has_Element (C) then
            Comp.Runtime := TU (Element (C));
            Next (C);
            if Has_Element (C) then
               Comp.Path := TU (Normalize_Pathname (Element (C),
                                                    Case_Sensitive => False));
               Next (C);
               if Has_Element (C) then
                  Comp.Name := TU (Element (C));
               end if;
            end if;
         end if;
      end if;

      --  Complete_Command_Line_Compilers will check that this is a valid
      --  config
      Append (Custom_Comps, Comp);

   exception
      when E : others =>
         Put_Verbose ("Exception raised: " & Exception_Information (E));
         raise Invalid_Config;
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
         if Comp1.Path_Order < Comp2.Path_Order then
            return True;
         elsif Comp1.Path_Order > Comp2.Path_Order then
            return False;
         else
            if Comp1.Runtime < Comp2.Runtime then
               return True;
            elsif Comp1.Runtime > Comp2.Runtime then
               return False;
            else
               --  More recent version first
               return Comp1.Version > Comp2.Version;
            end if;
         end if;
      end if;
   end "<";

   ------------------------------------
   -- Select_Compilers_Interactively --
   ------------------------------------

   procedure Select_Compilers_Interactively
     (Base               : in out Knowledge_Base;
      Compilers          : Compiler_Lists.List;
      Selected_Compilers : in out Compiler_Lists.List;
      Custom_Comps       : in out Compiler_Lists.List)
   is
      Compilers_Count : constant Natural := Natural (Length (Compilers));
      Selected : Boolean_Array  (1 .. Compilers_Count) := (others => False);
      Selectable : Boolean_Array  (1 .. Compilers_Count) := (others => True);
      Comps           : Compiler_Array (1 .. Compilers_Count);
      Comp            : Compiler_Lists.Cursor := First (Compilers);
      Index, Tmp      : Natural;
      Choice          : Character;
      Line            : String (1 .. 1024);

      procedure Filter_List;
      --  Filter the list to display.
      --  Only keep those compilers that might be compatible with the
      --  current selection. For instance, if we won't know how to
      --  link sources compiled with A and sources compiled with B,
      --  and A is selected, there is no point in showing B.
      --
      --  Only keep other compilers with the same target, since
      --  otherwise linking makes no sense.

      procedure Filter_List is
         Selected_Targets_Set : Targets_Set_Id;
         Tmp_Selection        : Compiler_Lists.List;
         Tmp_Selection2       : Compiler_Lists.List;
         Comp                 : Compiler_Lists.Cursor;
      begin
         --  Simulate the current selection
         Tmp_Selection := Custom_Comps;
         To_List (Comps, Selected, Tmp_Selection);

         if Length (Tmp_Selection) = 0 then
            Selectable := (others => True);
         else
            Selected_Targets_Set :=
              Element (First (Tmp_Selection)).Targets_Set;

            Put_Verbose ("Filtering the list of compilers");
            Put_Verbose
              (" <filter> remove all compilers for target /= "
               & To_String (Selected_Target));

            for C in Comps'Range loop
               if not Selected (C) then
                  --  Is the language already selected ?
                  Selectable (C) := True;
                  Comp := First (Tmp_Selection);
                  while Has_Element (Comp) loop
                     if Element (Comp).Language = Comps (C).Language then
                        Selectable (C) := False;
                        if Verbose_Level > 0 then
                           Put_Verbose ("Already selected language for");
                           Display_Compiler (Comps (C), False, C);
                        end if;
                        exit;
                     end if;
                     Next (Comp);
                  end loop;

                  --  Is the target compatible ?
                  if Selectable (C) then
                     if Comps (C).Targets_Set /= Selected_Targets_Set then
                        Selectable (C) := False;
                        if Verbose_Level > 0 then
                           Put_Verbose ("Incompatible target for:");
                           Display_Compiler (Comps (C), False, C);
                        end if;
                     end if;
                  end if;

                  --  Would adding this compiler to the current selection end
                  --  up with an unsupported config ?
                  if Selectable (C) then
                     Tmp_Selection2 := Tmp_Selection;
                     Append (Tmp_Selection2, Comps (C));
                     Selectable (C) :=
                       Is_Supported_Config (Base, Tmp_Selection2);
                     if not Selectable (C) then
                        if Verbose_Level > 0 then
                           Put_Verbose ("Unsupported config for:");
                           Display_Compiler (Comps (C), False, C);
                        end if;
                     end if;
                  end if;
               end if;
            end loop;
         end if;
      end Filter_List;

   begin
      for C in Comps'Range loop
         Comps (C) := Element (Comp);
         Selected (C) := Contains (Selected_Compilers, Comps (C));
         Next (Comp);
      end loop;

      Filter_List;

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

         Put
           ("Select or unselect the following compiler (or ""s"" to save): ");
         Get_Line (Line, Tmp);
         if Tmp = 0 then
            Choice := ASCII.NUL;
         else
            Choice := Line (Line'First);
         end if;

         if Choice = 's' then
            exit;

         elsif Choice = ASCII.NUL then
            null;

         else
            --  Selected one of the compilers we found ?
            begin
               Tmp := Character'Pos (Choice) - Character'Pos ('A') + 1;
            exception
               when Constraint_Error =>
                  --  No selection
                  exit;
            end;

            if Tmp in 1 .. Comps'Last then
               Selected (Tmp) := not Selected (Tmp);

               --  Selected one of the custom compilers
            elsif Tmp in Comps'Last + 1 .. Index - 1 then
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
         end if;

         Filter_List;
      end loop;

      Selected_Compilers := Custom_Comps;
      To_List (Comps, Selected, Selected_Compilers);
   end Select_Compilers_Interactively;

   -------------------------------------
   -- Complete_Command_Line_Compilers --
   -------------------------------------

   procedure Complete_Command_Line_Compilers
     (Base         : in out Knowledge_Base;
      On_Target    : Targets_Set_Id;
      Filters      : Compiler_Lists.List;
      Custom_Comps : out Compiler_Lists.List)
   is
      C    : Compiler_Lists.Cursor := First (Filters);
      Elem : Compiler;
      Completion : Compiler_Lists.List;
   begin
      while Has_Element (C) loop
         Clear (Completion);
         Elem := Element (C);

         Put_Verbose
           ("Completing info for --config="
            & To_String (Elem.Language)
            & ',' & To_String (Elem.Version)
            & ',' & To_String (Elem.Runtime)
            & ',' & To_String (Elem.Path)
            & ',' & To_String (Elem.Name), 1);

         if Elem.Path /= "" then
            Find_Matching_Compilers
              (Matching  => Elem,
               On_Target => On_Target,
               Base      => Base,
               Compilers => Completion,
               Stop_At_First_Match => True);
         else
            Find_Compilers_In_Path
              (Matching  => Elem,
               On_Target => On_Target,
               Base      => Base,
               Compilers => Completion,
               Stop_At_First_Match => True);
         end if;

         if not Is_Empty (Completion) then
            Elem := Element (First (Completion));
            Put_Verbose ("Found matching compiler "
                         & To_String (Elem.Language)
                         & ',' & To_String (Elem.Version)
                         & ',' & To_String (Elem.Runtime)
                         & ',' & To_String (Elem.Path)
                         & ',' & To_String (Elem.Name), -1);
            Append (Custom_Comps, Elem);
         else
            Put_Verbose ("", -1);
            Put_Line
              (Standard_Error,
               "Error: no matching compiler found for --config="
               & To_String (Elem.Language)
               & ',' & To_String (Elem.Version)
               & ',' & To_String (Elem.Runtime)
               & ',' & To_String (Elem.Path)
               & ',' & To_String (Elem.Name));
            raise Invalid_Config;
         end if;

         Next (C);
      end loop;
   end Complete_Command_Line_Compilers;

   ----------------------
   -- Filter_Compilers --
   ----------------------

   procedure Filter_Compilers
     (Selected_Comps : in out Compiler_Lists.List;
      Compilers      : Compiler_Lists.List;
      Filters        : Compiler_Lists.List)
   is
      function Filter_Match (Comp : Compiler; Filter : Compiler)
                            return Boolean;
      --  Returns True if Comp match Filter.

      ------------------
      -- Filter_Match --
      ------------------

      function Filter_Match (Comp : Compiler; Filter : Compiler)
                            return Boolean is
      begin
         if Length (Filter.Name) > 0 and then Comp.Name /= Filter.Name then
            return False;
         end if;

         if Length (Filter.Path) > 0 and then Filter.Path /= Comp.Path then
            return False;
         end if;

         if Length (Filter.Version) > 0
           and then Filter.Version /= Comp.Version
         then
            return False;
         end if;

         if Length (Filter.Language) > 0
           and then To_Lower (To_String (Filter.Language)) /=
           To_Lower (To_String (Comp.Language))
         then
            return False;
         end if;

         if Length (Filter.Runtime) > 0
           and then Filter.Runtime /= Comp.Runtime
         then
            return False;
         end if;

         return True;
      end Filter_Match;

      F : Compiler_Lists.Cursor := First (Filters);
   begin
      while Has_Element (F) loop
         declare
            C     : Compiler_Lists.Cursor := First (Compilers);
            Comp  : Compiler;
            Filt  : Compiler;
            Found : Boolean := False;
         begin
            Filt := Element (F);
            while Has_Element (C) loop
               Comp := Element (C);
               if Filter_Match (Comp, Filt) then
                  Append (Selected_Comps, Comp);
                  Found := True;
                  exit;
               end if;
               Next (C);
            end loop;
            if not Found then
               Put_Line (Standard_Error,
                         "warning: no matching compiler for filter: ");
               Put_Line
                 (Standard_Error, "  --config="
                  & To_String (Filt.Language)
                  & ',' & To_String (Filt.Version)
                  & ',' & To_String (Filt.Runtime)
                  & ',' & To_String (Filt.Path)
                  & ',' & To_String (Filt.Name));
            end if;
         end;
         Next (F);
      end loop;
   end Filter_Compilers;

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
         Put ("gprconfig --batch");
         Put (" --target=");
         if Selected_Target = Null_Unbounded_String then
            Put ("all");
         else
            Put (To_String (Selected_Target));
         end if;

         C := First (Selected_Comps);
         while Has_Element (C) loop
            Put (" --config=" & To_String (Element (C).Language)
                 & "," & To_String (Element (C).Version)
                 & "," & To_String (Element (C).Runtime)
                 & "," & To_String (Element (C).Path)
                 & "," & To_String (Element (C).Name));
            Next (C);
         end loop;
         New_Line;
         New_Line;
      end if;
   end Show_Command_Line_Config;

   Base               : Knowledge_Base;
   Selected_Compilers : Compiler_Lists.List;
   Custom_Comps       : Compiler_Lists.List;
   Filters            : Compiler_Lists.List;
   Load_Standard_Base : Boolean := True;
   Batch              : Boolean := False;
   Show_Targets       : Boolean := False;

   --  We need to add the executable suffix here, since on windows,
   --  Locate_Exec_On_Path will also return directories with the name
   --  "gprbuild" ie the current directory when gprconfig is run from the
   --  current dir.
   Exec_Suffix        : constant GNAT.Strings.String_Access :=
     Get_Executable_Suffix;
   Gprbuild_Path : GNAT.OS_Lib.String_Access :=
     Locate_Exec_On_Path (Gprbuild & Exec_Suffix.all);

   Compilers : Compiler_Lists.List;
   package Compiler_Sort is new Compiler_Lists.Generic_Sorting ("<");

   Valid_Switches : constant String :=
     "-batch -config= -db: h o: v q -show-targets -target=";

begin
   if Gprbuild_Path /= null  then
      Output_File := To_Unbounded_String
        (Normalize_Pathname (Dir_Name (Gprbuild_Path.all) & "..")
         & Directory_Separator & "share"
         & Directory_Separator & "gpr" & Directory_Separator
         & Default_Output_File);
   end if;
   Free (Gprbuild_Path);

   Selected_Target := TU (Sdefault.Hostname);

   --  First check whether we should parse the default knownledge base.
   --  This needs to be done first, since that influences --config and -h
   --  at least

   loop
      case Getopt (Valid_Switches) is
         when '-' =>
            if Full_Switch = "-db" then
               if Parameter = "-" then
                  Load_Standard_Base := False;
               end if;
            elsif Full_Switch = "-target" then
               if Parameter = "all" then
                  Selected_Target := Null_Unbounded_String;
               else
                  Selected_Target := To_Unbounded_String (Parameter);
                  Output_File := To_Unbounded_String (Parameter & ".cgpr");
               end if;
            end if;

         when 'q' =>
            Quiet_Output := True;
            Verbose_Level := 0;

         when 'v' =>
            Verbose_Level := Verbose_Level + 1;
            Quiet_Output := False;

         when ASCII.NUL =>
            exit;
         when others =>
            null;
      end case;
   end loop;

   if Load_Standard_Base then
      Parse_Knowledge_Base (Base, Get_Database_Directory);
   end if;

   --  Now check all the other command line switches

   Initialize_Option_Scan;

   loop
      case Getopt (Valid_Switches) is
         when '-' =>
            if Full_Switch = "-config" then
               Parse_Config_Parameter (Filters, Parameter);
            elsif Full_Switch = "-batch" then
               Batch := True;
            elsif Full_Switch = "-show-targets" then
               Show_Targets := True;
            elsif Full_Switch = "-db" then
               if Parameter = "-" then
                  null;  --  already processed
               else
                  Parse_Knowledge_Base (Base, Parameter);
               end if;
            end if;

         when 'h' =>
            Help (Base);
            return;

         when 'o' =>
            Output_File := To_Unbounded_String (Parameter);

         when 'q' | 'v' | 't' =>
            null;   --  already processed

         when others =>
            exit;
      end case;
   end loop;

   Put_Verbose ("Only compilers matching target "
                & To_String (Selected_Target)
                & " will be preserved");
   Get_Targets_Set
     (Base, To_String (Selected_Target), Selected_Targets_Set);

   if Batch then
      Complete_Command_Line_Compilers
        (Base,
         Selected_Targets_Set,
         Filters,
         Selected_Compilers);
      Splice (Target => Selected_Compilers,
              Before => First (Selected_Compilers),
              Source => Custom_Comps);

   else
      Find_Compilers_In_Path
        (Base                => Base,
         Matching            => No_Compiler,
         On_Target           => Selected_Targets_Set,
         Compilers           => Compilers,
         Stop_At_First_Match => False);

      if Show_Targets or else Verbose_Level > 0 then
         declare
            use String_Lists;
            All_Target : String_Lists.List;
            C : Compiler_Lists.Cursor := First (Compilers);
         begin
            Put_Line ("List of targets supported by a compiler:");
            while Has_Element (C) loop
               declare
                  Cur_Target : constant String :=
                    To_String (Element (C).Target);
                  T : String_Lists.Cursor := First (All_Target);
                  Dup : Boolean := False;
               begin
                  while Has_Element (T) loop
                     if Element (T) = Cur_Target then
                        Dup := True;
                        exit;
                     end if;
                     Next (T);
                  end loop;
                  if not Dup then
                     Put (Cur_Target);
                     if Cur_Target = Sdefault.Hostname then
                        Put (" (native target)");
                     end if;
                     New_Line;
                     Append (All_Target, Cur_Target);
                  end if;
               end;
               Next (C);
            end loop;
         end;
         if Show_Targets then
            return;
         end if;
      end if;

      if not Is_Empty (Filters) then
         Filter_Compilers (Selected_Compilers, Compilers, Filters);
      end if;

      if Is_Empty (Compilers) then
         if Selected_Target /= Null_Unbounded_String then
            Put_Line
              (Standard_Error,
               "No compilers found for target " & To_String (Selected_Target));
         else
            Put_Line (Standard_Error, "No compilers found");
         end if;
         Ada.Command_Line.Set_Exit_Status (1);
         return;
      end if;

      Compiler_Sort.Sort (Compilers);

      Select_Compilers_Interactively
        (Base, Compilers, Selected_Compilers, Custom_Comps);
      Show_Command_Line_Config (Selected_Compilers);
   end if;

   if Output_File /= Null_Unbounded_String then
      Generate_Configuration
        (Base, Selected_Compilers, To_String (Output_File));
   end if;

exception
   when Invalid_Config =>
      Put_Line
        (Standard_Error, "Invalid configuration specified with --config");
      Ada.Command_Line.Set_Exit_Status (1);
   when Generate_Error =>
      Put_Line
        (Standard_Error, "Generation of configuration files failed");
      Ada.Command_Line.Set_Exit_Status (3);
   when Invalid_Knowledge_Base =>
      Put_Line
        (Standard_Error, "Invalid setup of the gprconfig knowledge base");
      Ada.Command_Line.Set_Exit_Status (4);
   when End_Error =>
      null;
   when Invalid_Switch | Invalid_Parameter =>
      Put_Line ("Invalid command line switch: -" & Full_Switch);
      Help (Base);
      Ada.Command_Line.Set_Exit_Status (2);
end GprConfig.Main;
