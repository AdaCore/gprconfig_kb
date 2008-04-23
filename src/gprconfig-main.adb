------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             G P R C O N F I G                            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                   Copyright (C) 2006-2008, AdaCore                       --
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
with GPR_Version;
with Namet;                     use Namet;
with Switch;

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

   use Compiler_Lists;

   function Get_Database_Directory return String;
   --  Return the location of the knowledge database

   procedure Help (Base : Knowledge_Base);
   --  Display list of switches

   procedure Usage;
   --  Display list of options, no specific to current invocation, to be used
   --  when switch --help is used.

   procedure Check_Version_And_Help is new
     Switch.Check_Version_And_Help_G (Usage);

   procedure Parse_Config_Parameter
     (Base                   : Knowledge_Base;
      Custom_Comps           : in out Compiler_Lists.List;
      Config                 : String;
      Langs_With_No_Compiler : out Compiler_Lists.List);
   --  Parse the --config parameter, and store the (partial) information
   --  found there in Selected_Compilers
   --  When a switch matches a language that requires no compiler, an entry is
   --  added to Langs_With_No_Compiler.

   procedure Select_Compilers_Interactively
     (Base               : in out Knowledge_Base;
      Compilers          : in out Compiler_Lists.List);
   --  Ask the user for compilers to be selected

   procedure Complete_Command_Line_Compilers
     (Base         : in out Knowledge_Base;
      On_Target    : Targets_Set_Id;
      Filters      : Compiler_Lists.List;
      Custom_Comps : in out Compiler_Lists.List);
   --  In batch mode, the --config parameters indicate what compilers should be
   --  selected. Each of these switch selects the first matching compiler
   --  available, and all --config switch must match a compiler.
   --  This procedure is used to find matching compilers, and complete info
   --  like their version, runtime,... It should only be called in batch mode,
   --  since otherwise --config only acts as a filter for the compilers that
   --  are found through the knowledge base.

   procedure Show_Command_Line_Config (Compilers : Compiler_Lists.List);
   --  Display the batch command line that would have the same effect as the
   --  current selection of compilers.

   function "<" (Comp1, Comp2 : Compiler) return Boolean;
   --  Compare two compilers, so that similar languages are grouped together

   procedure Filter_List
     (Base      : in out Knowledge_Base;
      Compilers : in out Compiler_Lists.List);
   --  Compute which compilers are selectable by the user in that list,
   --  checking for various criteria like compatibility with the target,
   --  compatibility among the selected compilers,...
   --  Only keep those compilers that might be compatible with the current
   --  selection. For instance, if we won't know how to link sources compiled
   --  with A and sources compiled with B, and A is selected, there is no point
   --  in showing B.
   --  Only keep other compilers with the same target, since
   --  otherwise linking makes no sense.

   procedure Toggle_Selection     (Comp : in out Compiler);

   procedure Mark_As_Selectable   (Comp : in out Compiler);

   procedure Mark_As_Unselectable (Comp : in out Compiler);
   --  Changes some attributes of the compiler. This is used to update elements
   --  in a list.

   function Filter_Match (Comp : Compiler; Filter : Compiler) return Boolean;
   --  Returns True if Comp match Filter (the latter corresponds to a --config
   --  command line argument).

   type Boolean_Array  is array (Count_Type range <>) of Boolean;
   type Cursor_Array   is array (Count_Type range <>) of Compiler_Lists.Cursor;

   type Batch_Iterator (Count : Count_Type) is new Compiler_Iterator with
      record
         Found      : Count_Type := 0;
         Compilers  : Compiler_Lists.List;
         Matched    : Cursor_Array (1 .. Count) := (others => No_Element);
         Filters    : Compiler_Lists.List;

         Found_One  : Boolean_Array (1 .. Count) := (others => False);
         --  Whether we found at least one matching compiler for each filter
      end record;

   procedure Callback
     (Iterator       : in out Batch_Iterator;
      Base           : in out Knowledge_Base;
      Comp           : Compiler;
      From_Extra_Dir : Boolean;
      Continue       : out Boolean);
   --  Search the first compiler matching each --config command line argument.

   type All_Iterator (Count : Count_Type) is new Compiler_Iterator with
      record
         Filter_Matched : Boolean_Array (1 .. Count) := (others => False);
         Filters        : Compiler_Lists.List;
         Compilers      : Compiler_Lists.List;
      end record;

   procedure Callback
     (Iterator       : in out All_Iterator;
      Base           : in out Knowledge_Base;
      Comp           : Compiler;
      From_Extra_Dir : Boolean;
      Continue       : out Boolean);
   --  Search all compilers on path, preselecting the first one matching each
   --  of the filters.

   function Extra_Dirs_From_Filters
     (Filters : Compiler_Lists.List) return Unbounded_String;
   --  Compute the list of directories that should be prepended to the PATH
   --  when searching for compilers.

   ---------
   -- "<" --
   ---------

   function "<" (Comp1, Comp2 : Compiler) return Boolean is
   begin
      case Compare (Comp1.Language_LC, Comp2.Language_LC) is
         when Before =>
            return True;
         when After =>
            return False;
         when Equal =>
            if Comp1.Path_Order < Comp2.Path_Order then
               return True;
            elsif Comp2.Path_Order < Comp1.Path_Order then
               return False;
            else
               case Compare (Comp1.Runtime, Comp2.Runtime) is
                  when Before =>
                     return True;
                  when After =>
                     return False;
                  when Equal =>
                     return Compare (Comp1.Version, Comp2.Version) = Before;
               end case;
            end if;
      end case;
   end "<";

   --------------
   -- Callback --
   --------------

   procedure Callback
     (Iterator       : in out Batch_Iterator;
      Base           : in out Knowledge_Base;
      Comp           : Compiler;
      From_Extra_Dir : Boolean;
      Continue       : out Boolean)
   is
      C           : Compiler_Lists.Cursor := First (Iterator.Filters);
      Index       : Count_Type := 1;
   begin
      while Has_Element (C) loop
         --  A compiler is an "extra_dir" (ie specified on the command line)
         --  can only match if that directory was explicitly specified in
         --  --config. We do not want to find all compilers in /dir if that
         --  directory is not in $PATH

         if (not From_Extra_Dir or else Element (C).Path = Comp.Path)
           and then Filter_Match (Comp => Comp, Filter => Element (C))
         then
            Append (Iterator.Compilers, Comp);

            if Verbose_Level > 0 then
               Put_Verbose
                 ("Saving compiler for possible backtracking: "
                  & To_String (Comp, As_Config_Arg => True)
                  & " (matches --config "
                  & To_String (Element (C), As_Config_Arg => True)
                  & ")");
            end if;

            if Iterator.Matched (Index) = No_Element then
               Iterator.Found := Iterator.Found + 1;

               Put_Verbose
                 ("Selecting it since this filter was not matched yet "
                  & Iterator.Found'Img & "/" & Iterator.Count'Img);

               Iterator.Matched (Index) := Last (Iterator.Compilers);
               Iterator.Found_One (Index) := True;
               Update_Element (Iterator.Compilers, Iterator.Matched (Index),
                               Toggle_Selection'Access);

               --  Only keep those compilers that are not incompatible
               --  (according to the knowledge base). It might happen that none
               --  is selected as a result, but appropriate action is taken in
               --  Complete_Command_Line_Compilers. We ignore incompatible sets
               --  as early as possible, in the hope to limit the number of
               --  system calls if another set is found before all directories
               --  are traversed.

               if not Is_Supported_Config (Base, Iterator.Compilers) then
                  Update_Element (Iterator.Compilers, Iterator.Matched (Index),
                                  Toggle_Selection'Access);
                  Put_Verbose
                    ("Compilers are not compatible, cancelling last"
                     & " compiler found");
                  Iterator.Matched (Index) := No_Element;
                  Iterator.Found := Iterator.Found - 1;
               end if;
            end if;
         end if;

         Index := Index + 1;
         Next (C);
      end loop;

      --  Stop at first compiler
      Continue := Iterator.Found /= Iterator.Count;
   end Callback;

   procedure Callback
     (Iterator       : in out All_Iterator;
      Base           : in out Knowledge_Base;
      Comp           : Compiler;
      From_Extra_Dir : Boolean;
      Continue       : out Boolean)
   is
      pragma Unreferenced (Base);
      New_Comp : Compiler := Comp;
      C        : Compiler_Lists.Cursor;
      Index    : Count_Type := 1;
   begin
      if Iterator.Filter_Matched /=
        (Iterator.Filter_Matched'Range => True)
      then
         C := First (Iterator.Filters);
         while Has_Element (C) loop
            if not Iterator.Filter_Matched (Index)
              and then Filter_Match (Comp => Comp, Filter => Element (C))
            then
               New_Comp.Selected := True;
               Iterator.Filter_Matched (Index) := True;
               exit;
            end if;

            Index := Index + 1;
            Next (C);
         end loop;
      end if;

      --  Ignore compilers from extra directories, unless they have been
      --  selected because of a --config argument

      if New_Comp.Selected
        or else not From_Extra_Dir
      then
         Put_Verbose
           ("Adding compiler to interactive menu "
            & To_String (Comp, True) & " selected=" & New_Comp.Selected'Img);
         Append (Iterator.Compilers, New_Comp);
      end if;

      Continue := True;
   end Callback;

   -------------------------------------
   -- Complete_Command_Line_Compilers --
   -------------------------------------

   procedure Complete_Command_Line_Compilers
     (Base         : in out Knowledge_Base;
      On_Target    : Targets_Set_Id;
      Filters      : Compiler_Lists.List;
      Custom_Comps : in out Compiler_Lists.List)
   is
      Iter  : Batch_Iterator (Length (Filters));

      function Foreach_Nth_Compiler
        (Filter : Compiler_Lists.Cursor) return Boolean;
      --  For all possible compiler matching the filter, check whether we
      --  find a compatible set of compilers matching the next filters.
      --  Return True if one was found (in which case it is the current
      --  selection on exit).

      --------------------------
      -- Foreach_Nth_Compiler --
      --------------------------

      function Foreach_Nth_Compiler
        (Filter : Compiler_Lists.Cursor) return Boolean
      is
         C           : Compiler_Lists.Cursor := First (Iter.Compilers);
         Comp_Filter : constant Compiler := Element (Filter);
      begin
         while Has_Element (C) loop
            if Filter_Match (Element (C), Filter => Comp_Filter) then
               Update_Element (Iter.Compilers, C, Toggle_Selection'Access);

               if Next (Filter) = No_Element then
                  if Verbose_Level > 0 then
                     Put_Verbose ("Testing the following compiler set:", 1);
                     Put_Verbose
                       (To_String (Iter.Compilers, Selected_Only => True));
                  end if;

                  if Is_Supported_Config (Base, Iter.Compilers) then
                     Put_Verbose ("They are compatible", -1);
                     return True;
                  else
                     Put_Verbose ("", -1);
                  end if;

               else
                  if Foreach_Nth_Compiler (Next (Filter)) then
                     return True;
                  end if;
               end if;

               Update_Element (Iter.Compilers, C, Toggle_Selection'Access);
            end if;

            Next (C);
         end loop;

         return False;
      end Foreach_Nth_Compiler;

      C     : Compiler_Lists.Cursor;
      Extra_Dirs : constant Unbounded_String :=
        Extra_Dirs_From_Filters (Filters);
      Found_All : Boolean := True;
   begin
      Iter.Filters   := Filters;

      Put_Verbose ("Completing info for --config parameters, extra_dirs="
                   & To_String (Extra_Dirs), 1);

      Foreach_Compiler_In_Path
        (Iterator   => Iter,
         Base       => Base,
         On_Target  => On_Target,
         Extra_Dirs => To_String (Extra_Dirs));

      Put_Verbose ("", -1);

      --  Check that we could find at least one of each compiler

      C := First (Filters);
      for F in Iter.Found_One'Range loop
         if not Iter.Found_One (F) then
            if not Quiet_Output then
               Put_Line
                 (Standard_Error,
                  "Error: no matching compiler found for --config="
                  & To_String (Element (C), As_Config_Arg => True));
            end if;
            Found_All := False;
         end if;
         Next (C);
      end loop;

      --  If we could find at least one of each compiler, but that our initial
      --  attempt returned incompatible sets of compiler, we do a more thorough
      --  attempt now

      if Found_All
        and then Iter.Found /= Iter.Count
      then
         --  If no compatible set was found, try all possible combinations, in
         --  the hope that we can finally find one. In the following algorithm,
         --  we end up checking again some set that were checked in Callback,
         --  but that would be hard to avoid since the compilers can be found
         --  in any order.

         Put_Verbose ("Attempting to find a supported compiler set", 1);

         --  Unselect all compilers

         C := First (Iter.Compilers);
         while Has_Element (C) loop
            if Element (C).Selected then
               Update_Element (Iter.Compilers, C, Toggle_Selection'Access);
            end if;
            Next (C);
         end loop;

         if not Foreach_Nth_Compiler (First (Iter.Filters)) then
            Put_Line
              (Standard_Error,
               "Error: no set of compatible compilers was found");
            raise Invalid_Config;
         end if;

         Put_Verbose ("", -1);
      end if;

      Splice (Target => Custom_Comps,
              Before => No_Element,
              Source => Iter.Compilers);
   end Complete_Command_Line_Compilers;

   -----------------------------
   -- Extra_Dirs_From_Filters --
   -----------------------------

   function Extra_Dirs_From_Filters
     (Filters : Compiler_Lists.List) return Unbounded_String
   is
      C          : Compiler_Lists.Cursor  := First (Filters);
      Extra_Dirs : Unbounded_String;
      Elem       : Compiler;
   begin
      while Has_Element (C) loop
         Elem := Element (C);
         if Elem.Path /= No_Name then
            Append (Extra_Dirs, Get_Name_String (Elem.Path) & Path_Separator);
         end if;
         Next (C);
      end loop;
      return Extra_Dirs;
   end Extra_Dirs_From_Filters;

   -----------------
   -- Filter_List --
   -----------------

   procedure Filter_List
     (Base      : in out Knowledge_Base;
      Compilers : in out Compiler_Lists.List)
   is
      Comp, Comp2          : Compiler_Lists.Cursor;
      Selectable           : Boolean;

   begin
      Put_Verbose ("Filtering the list of compilers", 1);

      Comp := First (Compilers);
      while Has_Element (Comp) loop
         if not Element (Comp).Selected then
            Selectable := True;

            if Selected_Targets_Set /= All_Target_Sets
              and then Element (Comp).Targets_Set /= All_Target_Sets
              and then Element (Comp).Targets_Set /= Selected_Targets_Set
            then
               Selectable := False;
               if Verbose_Level > 0 then
                  Put_Verbose ("Incompatible target for: "
                              & To_String (Element (Comp), False));
               end if;
            end if;

            if Selectable then
               Comp2 := First (Compilers);
               while Has_Element (Comp2) loop
                  if Element (Comp2).Selected
                    and then Element (Comp2).Language_LC =
                      Element (Comp).Language_LC
                  then
                     Selectable := False;
                     if Verbose_Level > 0 then
                        Put_Verbose ("Already selected language for "
                                     & To_String (Element (Comp), False));
                     end if;
                     exit;
                  end if;
                  Next (Comp2);
               end loop;
            end if;

            if Selectable then
               --  Would adding this compiler to the current selection end
               --  up with an unsupported config ?

               Update_Element (Compilers, Comp, Toggle_Selection'Access);
               if not Is_Supported_Config (Base, Compilers) then
                  Selectable := False;
                  if Verbose_Level > 0 then
                     Put_Verbose ("Unsupported config for: "
                                  & To_String (Element (Comp), False));
                  end if;
               end if;
               Update_Element (Compilers, Comp, Toggle_Selection'Access);
            end if;

            if Selectable then
               Update_Element (Compilers, Comp, Mark_As_Selectable'Access);
            else
               Update_Element (Compilers, Comp, Mark_As_Unselectable'Access);
            end if;
         end if;

         Next (Comp);
      end loop;

      Put_Verbose ("", -1);
   end Filter_List;

   ------------------
   -- Filter_Match --
   ------------------

   function Filter_Match (Comp : Compiler; Filter : Compiler) return Boolean is
   begin
      if Filter.Name /= No_Name and then Comp.Name /= Filter.Name then
         if Verbose_Level > 0 then
            Put_Verbose ("Filter=" & To_String (Filter, True)
                         & ": name does not match");
         end if;
         return False;
      end if;

      if Filter.Path /= No_Name and then Filter.Path /= Comp.Path then
         if Verbose_Level > 0 then
            Put_Verbose ("Filter=" & To_String (Filter, True)
                         & ": path does not match");
         end if;
         return False;
      end if;

      if Filter.Version /= No_Name and then Filter.Version /= Comp.Version then
         if Verbose_Level > 0 then
            Put_Verbose ("Filter=" & To_String (Filter, True)
                         & ": version does not match");
         end if;
         return False;
      end if;

      if Filter.Runtime /= No_Name and then Filter.Runtime /= Comp.Runtime then
         if Verbose_Level > 0 then
            Put_Verbose ("Filter=" & To_String (Filter, True)
                         & ": runtime does not match");
         end if;
         return False;
      end if;

      if Filter.Language_LC /= No_Name
        and then Filter.Language_LC /= Comp.Language_LC
      then
         if Verbose_Level > 0 then
            Put_Verbose ("Filter=" & To_String (Filter, True)
                         & ": language does not match");
         end if;
         return False;
      end if;

      return True;
   end Filter_Match;

   ----------------------------
   -- Get_Database_Directory --
   ----------------------------

   function Get_Database_Directory return String is
      Prog_Dir : constant String := Get_Program_Directory;
      Suffix : constant String := "share" & Directory_Separator & "gprconfig";
   begin
      return Prog_Dir & Suffix;
   end Get_Database_Directory;

   ----------
   -- Help --
   ----------

   procedure Help (Base : Knowledge_Base) is
      Known : Unbounded_String;
   begin
      Known_Compiler_Names (Base, Known);
      Usage;
      Put_Line ("            The known compilers are: " & To_String (Known));
   end Help;

   ------------------------
   -- Mark_As_Selectable --
   ------------------------

   procedure Mark_As_Selectable   (Comp : in out Compiler) is
   begin
      Comp.Selectable := True;
   end Mark_As_Selectable;

   --------------------------
   -- Mark_As_Unselectable --
   --------------------------

   procedure Mark_As_Unselectable (Comp : in out Compiler) is
   begin
      Comp.Selectable := False;
   end Mark_As_Unselectable;

   ----------------------------
   -- Parse_Config_Parameter --
   ----------------------------

   procedure Parse_Config_Parameter
     (Base                   : Knowledge_Base;
      Custom_Comps           : in out Compiler_Lists.List;
      Config                 : String;
      Langs_With_No_Compiler : out Compiler_Lists.List)
   is
      use String_Lists;
      Map  : String_Lists.List;
      C    : String_Lists.Cursor;
      Comp : Compiler;
   begin
      --  Only valid separator is ',', not spaces
      Get_Words (Config, Filter => No_Name, Map => Map,
                 Separator1 => ',', Separator2 => ',',
                 Allow_Empty_Elements => True);

      C := First (Map);
      declare
         LC : constant String := To_Lower (Element (C));
      begin
         Comp.Language_Case := Get_String_Or_No_Name (Element (C));
         Comp.Language_LC   := Get_String_Or_No_Name (LC);

         if Is_Language_With_No_Compiler (Base, LC) then
            Put_Verbose ("Language " & LC & " requires no compiler");
            Comp.Complete := True;
            Comp.Selected := True;
            Comp.Targets_Set := All_Target_Sets;
            Append (Langs_With_No_Compiler, Comp);

         else
            Next (C);
            if Has_Element (C) then
               Comp.Version := Get_String_Or_No_Name (Element (C));
               Next (C);
               if Has_Element (C) then
                  Comp.Runtime := Get_String_Or_No_Name (Element (C));
                  Next (C);
                  if Has_Element (C) then
                     Comp.Path := Get_String_Or_No_Name
                       (Name_As_Directory
                          (Normalize_Pathname (Element (C),
                           Case_Sensitive => False)));
                     Next (C);
                     if Has_Element (C) then
                        Comp.Name := Get_String_Or_No_Name (Element (C));
                     end if;
                  end if;
               end if;
            end if;

            Comp.Complete := False;

            --  Complete_Command_Line_Compilers will check that this is a valid
            --  config
            Put_Verbose ("Language " & LC & " requires a compiler");
            Append (Custom_Comps, Comp);
         end if;
      end;

   exception
      when E : others =>
         Put_Verbose ("Exception raised: " & Exception_Information (E));
         raise Invalid_Config;
   end Parse_Config_Parameter;

   ------------------------------------
   -- Select_Compilers_Interactively --
   ------------------------------------

   procedure Select_Compilers_Interactively
     (Base               : in out Knowledge_Base;
      Compilers          : in out Compiler_Lists.List)
   is
      Comp            : Compiler_Lists.Cursor := First (Compilers);
      Choice          : Character;
      Tmp             : Natural;
      Line            : String (1 .. 1024);

      procedure Update_Index (Comp : in out Compiler);
      --  Set the interactive index for this compiler

      procedure Update_Index (Comp : in out Compiler) is
      begin
         Comp.Index_In_List := Choice;
         Choice := Character'Succ (Choice);
      end Update_Index;

   begin
      Choice := 'A';
      while Has_Element (Comp) loop
         Update_Element (Compilers, Comp, Update_Index'Access);
         Next (Comp);
      end loop;

      loop
         Filter_List (Base, Compilers);

         Put_Line ("--------------------------------------------------");
         Put_Line
           ("gprconfig has found the following compilers on your PATH.");
         Put_Line
           ("Only those matching the target and the selected compilers"
            & " are displayed.");

         Put (To_String
              (Compilers, Selected_Only => False,
               Show_Target => Selected_Targets_Set = All_Target_Sets));

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
            Comp := First (Compilers);
            while Has_Element (Comp) loop
               if Element (Comp).Index_In_List = Choice then
                  Update_Element (Compilers, Comp, Toggle_Selection'Access);
                  exit;
               end if;
               Next (Comp);
            end loop;
         end if;
      end loop;
   end Select_Compilers_Interactively;

   ------------------------------
   -- Show_Command_Line_Config --
   ------------------------------

   procedure Show_Command_Line_Config (Compilers : Compiler_Lists.List) is
      C : Compiler_Lists.Cursor;
   begin
      if not Is_Empty (Compilers) then
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

         C := First (Compilers);
         while Has_Element (C) loop
            if Element (C).Selected then
               Put (" --config="
                    & To_String (Element (C), As_Config_Arg => True));
            end if;
            Next (C);
         end loop;
         New_Line;
         New_Line;
      end if;
   end Show_Command_Line_Config;

   Base               : Knowledge_Base;
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

   ----------------------
   -- Toggle_Selection --
   ----------------------

   procedure Toggle_Selection (Comp : in out Compiler) is
   begin
      Comp.Selected := not Comp.Selected;
   end Toggle_Selection;

   -----------
   -- Usage --
   -----------

   procedure Usage is
   begin
      Put_Line (" --target=target (" & Sdefault.Hostname & " by default)");
      Put_Line
        ("            Select specified target or ""all"" for any target.");
      Put_Line (" --show-targets : List all compiler targets available.");
      Put_Line (" --batch  : batch mode, no interactive compiler selection.");
      Put_Line (" -v       : verbose mode.");
      Put_Line (" -q       : quiet output.");
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
   end Usage;

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

   --  First, check if --version or --help is used

   Check_Version_And_Help
     ("GPRCONFIG",
      "2006",
      Version_String => GPR_Version.Gpr_Version_String);

   --  Now check whether we should parse the default knownledge base.
   --  This needs to be done first, since that influences --config and -h
   --  at least

   Initialize_Option_Scan;

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
            elsif Full_Switch = "-show-targets" then
               --  By default, display all targets available
               Selected_Target := Null_Unbounded_String;
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
               Parse_Config_Parameter (Base, Filters, Parameter, Compilers);
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
         Compilers);
   else
      declare
         Iter : All_Iterator (Length (Filters));
      begin
         Iter.Filters := Filters;
         Foreach_Compiler_In_Path
           (Iterator   => Iter,
            Base       => Base,
            On_Target  => Selected_Targets_Set,
            Extra_Dirs => To_String (Extra_Dirs_From_Filters (Filters)));

         Splice (Target => Compilers,
                 Before => No_Element,
                 Source => Iter.Compilers);
      end;

      if Show_Targets or else Verbose_Level > 0 then
         declare
            use String_Lists;
            All_Target : String_Lists.List;
            C : Compiler_Lists.Cursor := First (Compilers);
         begin
            Put_Line ("List of targets supported by a compiler:");
            while Has_Element (C) loop
               if Element (C).Target /= No_Name then
                  declare
                     Cur_Target : constant String :=
                       Get_Name_String (Element (C).Target);
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
               end if;
               Next (C);
            end loop;
         end;
         if Show_Targets then
            return;
         end if;
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

      Select_Compilers_Interactively (Base, Compilers);
      Show_Command_Line_Config (Compilers);
   end if;

   if Output_File /= Null_Unbounded_String then
      Generate_Configuration (Base, Compilers, To_String (Output_File));
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
