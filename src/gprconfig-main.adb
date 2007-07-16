------------------------------------------------------------------------------
--                   Copyright (C) 2006-2007, AdaCore                       --
------------------------------------------------------------------------------

with Ada.Characters.Handling;   use Ada.Characters.Handling;
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
   --  Value of -target switch.

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
      Compilers          : Compiler_Lists.List;
      Selected_Compilers : in out Compiler_Lists.List;
      Custom_Comps       : in out Compiler_Lists.List);
   --  Ask the user for compilers to be selected

   procedure Select_Compilers_For_Lang
     (Languages          : String;
      Compilers          : Compiler_Lists.List;
      Selected_Compilers : in out Compiler_Lists.List);
   --  Select the first compiler for all languages in Languages

   procedure Complete_Command_Line_Compilers
     (Base         : Knowledge_Base;
      Custom_Comps : in out Compiler_Lists.List);
   --  Complete missing information for the compilers specified on the command
   --  line.

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

      Put_Line (" -target=target (" & Sdefault.Hostname & " by default)");
      Put_Line ("           Select specified target or all for any target");
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
      Put_Line ("           The known compilers are: " & To_String (Known));
      Put_Line (" -l[lang1,lang2,...]: Preselect the first compiler for"
                & " each specified language." & ASCII.LF
                & "         No space between -l and its arguments");
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

      Put_Line ("- " & Name_As_Directory (To_String (Comp.Path))
                & To_String (Comp.Executable));
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

      C := First (Map);
      Comp.Name := TU (Element (C));
      Next (C);
      if Has_Element (C) then
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

   -------------------------------
   -- Select_Compilers_For_Lang --
   -------------------------------

   procedure Select_Compilers_For_Lang
     (Languages          : String;
      Compilers          : Compiler_Lists.List;
      Selected_Compilers : in out Compiler_Lists.List)
   is
      use String_Lists;
      Langs : String_Lists.List;
      Comp  : Compiler_Lists.Cursor := First (Compilers);
      C     : String_Lists.Cursor;
   begin
      Get_Words (Words                => Languages,
                 Filter               => Null_Unbounded_String,
                 Map                  => Langs,
                 Allow_Empty_Elements => False);

      while Has_Element (Comp) loop
         C := Find (Langs, To_Lower (To_String (Element (Comp).Language)));
         if Has_Element (C) then
            Append (Selected_Compilers, Element (Comp));
            Delete (Langs, C);
         end if;

         Next (Comp);
      end loop;
   end Select_Compilers_For_Lang;

   ------------------------------------
   -- Select_Compilers_Interactively --
   ------------------------------------

   procedure Select_Compilers_Interactively
     (Base               : Knowledge_Base;
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
         Selected_Target : Unbounded_String;
         Tmp_Selection   : Compiler_Lists.List;
         Tmp_Selection2  : Compiler_Lists.List;
         Comp            : Compiler_Lists.Cursor;
      begin
         --  Simulate the current selection
         Tmp_Selection := Custom_Comps;
         To_List (Comps, Selected, Tmp_Selection);

         if Length (Tmp_Selection) = 0 then
            Selectable := (others => True);
         else
            Selected_Target := Element (First (Tmp_Selection)).Target;

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
                        if Verbose_Mode then
                           Put_Verbose ("Already selected language for");
                           Display_Compiler (Comps (C), False, C);
                        end if;
                        exit;
                     end if;
                     Next (Comp);
                  end loop;

                  --  Is the target compatible ?
                  if Selectable (C) then
                     Selectable (C) := Architecture_Equal
                       (To_String (Selected_Target),
                        To_String (Comps (C).Target));
                     if not Selectable (C) then
                        if Verbose_Mode then
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
                        if Verbose_Mode then
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
            end;

         elsif Choice = 's' then
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
            if Elem.Prefix = Null_Unbounded_String then
               Elem.Prefix := Element (First (Completion)).Prefix;
            end if;
            if Elem.Executable = Null_Unbounded_String then
               Elem.Executable := Element (First (Completion)).Executable;
            end if;
         else
            Put_Verbose
              ("Error while querying missing info for a compiler"
               & " specified on the command line: "
               & To_String (Elem.Name) & "," & To_String (Elem.Path));
            raise Invalid_Config;
         end if;
      end Update_Comps;

      C : Compiler_Lists.Cursor := First (Custom_Comps);
   begin
      while Has_Element (C) loop
         Update_Element (Custom_Comps, C, Update_Comps'Unrestricted_Access);
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
           and then Filter.Language /= Comp.Language
         then
            return False;
         end if;

         if Length (Filter.Target) > 0
           and then Filter.Target /= Comp.Target
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
            while Has_Element (C) loop
               Comp := Element (C);
               Filt := Element (F);
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
                 (Standard_Error, "  -config " & To_String (Filt.Name) & ','
                  & To_String (Filt.Path) & ',' & To_String (Filt.Version)
                  & ',' & To_String (Filt.Language) & ','
                  & To_String (Filt.Target) & ',' & To_String (Filt.Runtime));
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
         Put ("gprconfig -batch");
         Put (" -target=");
         if Selected_Target = Null_Unbounded_String then
            Put ("all");
         else
            Put (To_String (Selected_Target));
         end if;

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
   Preselect_Lang : Unbounded_String;

   Compilers : Compiler_Lists.List;
   package Compiler_Sort is new Compiler_Lists.Generic_Sorting ("<");

   Valid_Switches : constant String :=
     "batch config: db: h o: v l? show-targets target=";

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
   --  This needs to be done first, since that influences -config and -h
   --  at least

   loop
      case Getopt (Valid_Switches) is
         when 'd' =>
            if Parameter = "-" then
               Load_Standard_Base := False;
            end if;
         when 'v' =>
            Verbose_Mode := True;
         when 't' =>
            if Parameter = "all" then
               Selected_Target := Null_Unbounded_String;
            else
               Selected_Target := To_Unbounded_String (Parameter);
               Output_File := To_Unbounded_String (Parameter & ".cgpr");
            end if;
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
         when 'b' =>
            Batch := True;

         when 'c' =>
            Parse_Config_Parameter (Filters, Parameter);

         when 'd' =>
            if Parameter = "-" then
               null;  --  already processed
            else
               Parse_Knowledge_Base (Base, Parameter);
            end if;

         when 'h' =>
            Help (Base);
            return;

         when 'l' =>
            Preselect_Lang := To_Unbounded_String (Parameter);
            if Preselect_Lang = "" then
               Preselect_Lang := To_Unbounded_String ("ada,c,c++");
            end if;

         when 'o' =>
            Output_File := To_Unbounded_String (Parameter);

         when 's' =>
            Show_Targets := True;

         when 'v' | 't' =>
            null;   --  already processed

         when others =>
            exit;
      end case;
   end loop;

   Complete_Command_Line_Compilers (Base, Custom_Comps);
   Find_Compilers_In_Path (Base, Compilers);

   if Show_Targets or else Verbose_Mode then
      declare
         use String_Lists;
         All_Target : String_Lists.List;
         C : Compiler_Lists.Cursor := First (Compilers);
      begin
         Put_Line ("List of targets supported by a compiler:");
         while Has_Element (C) loop
            declare
               Cur_Target : constant String := To_String (Element (C).Target);
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

   --  Remove compilers not matching the target.
   if Selected_Target /= Null_Unbounded_String then
      declare
         C      : Compiler_Lists.Cursor := First (Compilers);
         Next_C : Compiler_Lists.Cursor;
      begin
         while Has_Element (C) loop
            Next_C := Next (C);
            if Element (C).Target /= Selected_Target then
               Put_Verbose ("compiler " & To_String (Element (C).Executable)
                            & " does not match selected target");
               Delete (Compilers, C);
            end if;
            C := Next_C;
         end loop;
      end;
   end if;

   if not Is_Empty (Filters) then
      Filter_Compilers (Selected_Compilers, Compilers, Filters);
   end if;

   if not Batch then
      Compiler_Sort.Sort (Compilers);

      if Preselect_Lang /= Null_Unbounded_String then
         Select_Compilers_For_Lang
           (To_String (Preselect_Lang), Compilers, Selected_Compilers);
      end if;

      Select_Compilers_Interactively
        (Base, Compilers, Selected_Compilers, Custom_Comps);
      Show_Command_Line_Config (Selected_Compilers);
   else
      Splice (Target => Selected_Compilers,
              Before => First (Selected_Compilers),
              Source => Custom_Comps);
   end if;

   if Output_File /= Null_Unbounded_String then
      Generate_Configuration
        (Base, Selected_Compilers, To_String (Output_File));
   end if;

exception
   when Invalid_Config =>
      Put_Line
        (Standard_Error, "Invalid configuration specified with -config");
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
