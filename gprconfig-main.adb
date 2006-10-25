------------------------------------------------------------------------------
--                   Copyright (C) 2006, AdaCore                            --
------------------------------------------------------------------------------

with Ada.Command_Line;          use Ada.Command_Line;
with Ada.Directories;           use Ada.Directories;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Ada.Text_IO;               use Ada.Text_IO;
with GNAT.Command_Line;         use GNAT.Command_Line;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GprConfig.Knowledge;       use GprConfig.Knowledge;
with Sdefault;

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

   ----------
   -- Help --
   ----------

   procedure Help is
   begin
      Put_Line (" -o file : Name and directory of the output file");
      Put_Line ("           default is " & To_String (Output_File));
      Put_Line (" -db dir : Parse dir as an additional knowledge base");
      Put_Line (" -db-    : Do not load the standard knowledge base");
   end Help;

   ----------------------------
   -- Get_Database_Directory --
   ----------------------------

   function Get_Database_Directory return String is
      Command : constant String :=
        Containing_Directory (Ada.Command_Line.Command_Name);
      Normalized : constant String :=
        Normalize_Pathname (Command, Resolve_Links => True);
      Suffix : constant String := "share" & Directory_Separator & "gprconfig";
   begin
      if Normalized (Normalized'Last) = Directory_Separator then
         return Normalized & Suffix;
      else
         return Normalized & Directory_Separator & Suffix;
      end if;
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

   Base               : Knowledge_Base;
   Compilers          : Compiler_Lists.List;
   Selected_Compilers : Compiler_Lists.List;
   Gprmake_Path : GNAT.OS_Lib.String_Access := Locate_Exec_On_Path (Gprmake);
   Load_Standard_Base : Boolean := True;

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
      case Getopt ("o: db: h") is
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
         when others =>
            exit;
      end case;
   end loop;

   if Load_Standard_Base then
      Parse_Knowledge_Base (Base, Get_Database_Directory);
   end if;

   Find_Compilers_In_Path (Base, Compilers);

   declare
      Compilers_Count : constant Natural := Natural (Length (Compilers));
      Selected    : Boolean_Array  (1 .. Compilers_Count) := (others => False);
      Selectable  : Boolean_Array  (1 .. Compilers_Count) := (others => True);
      Selected_Count  : Natural := 0;
      Comps           : Compiler_Array (1 .. Compilers_Count);
      Comp            : Compiler_Lists.Cursor := First (Compilers);
      Custom_Comps    : Compiler_Lists.List;
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
            --  current selection. For instance, if we won't know how to link
            --  sources compiled with A and sources compiled with B, and A is
            --  selected, there is no point in showing B.
            --
            --  Only keep other compilers with the same target, since otherwise
            --  linking makes no sense.

            Put_Line ("Filtering the list to only show compatible compilers");
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

   Put_Line ("--------------- Generating output -----------");
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
