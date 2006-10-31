------------------------------------------------------------------------------
--                   Copyright (C) 2006, AdaCore                            --
------------------------------------------------------------------------------

with Ada.Command_Line;          use Ada.Command_Line;
with Ada.Directories;           use Ada.Directories;
with Ada.Environment_Variables; use Ada.Environment_Variables;
with Ada.Strings.Fixed;         use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Ada.Text_IO;               use Ada.Text_IO;
with Glib.XML;                  use Glib;
with GNAT.Case_Util;            use GNAT.Case_Util;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.Expect;               use GNAT.Expect;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Regpat;               use GNAT.Regpat;
with GNAT.Strings;              use GNAT.Strings;
with GprConfig.Sdefault;        use GprConfig.Sdefault;

package body GprConfig.Knowledge is

   package XML_Int is new Glib.XML (Integer);
   use XML_Int;

   package String_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (String, Unbounded_String, Ada.Strings.Hash_Case_Insensitive, "=");

   type External_Value_Item is record
      Value          : Unbounded_String;
      Extracted_From : Unbounded_String;
   end record;
   --  Value is the actual value of the <external_value> node.
   --  Extracted_From will either be set to Value itself, or when the node is
   --  a <directory node> to the full directory, before the regexp match.
   --  When the value comes from a <shell> node, Extracted_From is set to the
   --  full output of the shell command.

   package External_Value_Lists is new Ada.Containers.Doubly_Linked_Lists
     (External_Value_Item);

   use Compiler_Lists, Compiler_Description_Maps;
   use Configuration_Lists, Compilers_Filter_Lists, String_Maps;
   use Compiler_Filter_Lists, External_Value_Lists, String_Lists;

   Ignore_Compiler : exception;
   --  Raised when the compiler should be ignored

   procedure Parse_Compiler_Description
     (Append_To   : in out Compiler_Description_Maps.Map;
      File        : String;
      Description : Node_Ptr);
   --  Parse a compiler description described by N

   procedure Parse_Configuration
     (Append_To   : in out Configuration_Lists.List;
      File        : String;
      Description : Node_Ptr);
   --  Parse a configuration node

   procedure Parse_External_Value
     (Value       : out External_Value;
      File        : String;
      Node        : Node_Ptr);
   --  Parse an XML node that describes an external value

   procedure Find_Compilers_In_Dir
     (Append_To : in out Compiler_Lists.List;
      Base      : Knowledge_Base;
      Directory : String;
      Name      : String := "");
   --  Find all known compilers in a specific directory.
   --  If Name is specified, then only compilers with that given name are
   --  searched for.

   procedure Get_External_Value
     (Value            : External_Value;
      Comp             : Compiler;
      Split_Into_Words : Boolean := True;
      Processed_Value  : out External_Value_Lists.List);
   --  Computes the value of Value, depending on its type. When an external
   --  command needs to be executed, Path is put first on the PATH environment
   --  variable.
   --  Raises Ignore_Compiler if the value doesn't match its <must_have>
   --  regexp.
   --  The <filter> node is also taken into account.
   --  If Split_Into_Words is true, then the value read from <shell> or as a
   --  constant string is further assumed to be a comma-separated or space-
   --  separated string, and split.

   procedure For_Each_Language_Runtime
     (Append_To  : in out Compiler_Lists.List;
      Name       : String;
      Directory  : String;
      Descr      : Compiler_Description);
   --  For each language/runtime parsed in Languages/Runtimes, create a new
   --  compiler in the list.

   procedure Parse_All_Dirs
     (Processed_Value  : out External_Value_Lists.List;
      Current_Dir       : String;
      Path_To_Check     : String;
      Regexp            : Pattern_Matcher;
      Group             : Natural);
   --  Parse all subdirectories of Current_Dir for those that match
   --  Path_To_Check (see description of <directory>). When a match is found,
   --  the regexp is evaluated against the current directory, and the matching
   --  parenthesis group is appended to Append_To (comma-separated)

   function Substitute_Special_Dirs
     (Str         : String;
      Comp        : Compiler;
      Output_Dir  : String) return String;
   --  Substitute the special "$..." names in Directory, as described in the
   --  <directory> tag

   procedure Match
     (Filter            : Compilers_Filter_Lists.List;
      Selected          : Compiler_Lists.List;
      Matching_Compiler : out Compiler;
      Matched           : out Boolean);
   procedure Match
     (Filter            : Compilers_Filter;
      Selected          : Compiler_Lists.List;
      Matching_Compiler : out Compiler;
      Matched           : out Boolean);
   procedure Match
     (Filter            : Compiler_Filter;
      Selected          : Compiler_Lists.List;
      Matching_Compiler : out Compiler;
      Matched           : out Boolean);
   --  Check whether Filter matches (and set Matched to the result).
   --  Matching_Compiler is set if there was a single <compilers> node, and is
   --  to set the first compiler that matched in that node

   function Match
     (Target_Filter : String_Lists.List;
      Negate        : Boolean;
      Selected      : Compiler_Lists.List) return Boolean;
   --  Return True if Filter matches the list of selected configurations

   procedure Merge_Config
     (Packages          : in out String_Maps.Map;
      Selected_Compiler : Compiler;
      Config            : String;
      Output_Dir        : String);
   --  Merge the contents of Config into Packages, so that each attributes ends
   --  up in the right package, and the packages are not duplicated.
   --  Selected_Compiler is the compiler that made the chunk match the filters.
   --  If there were several <compilers> filter, No_Compiler should be passed
   --  in argument.

   procedure Skip_Spaces (Str : String; Index : in out Integer);
   --  Move Index from its current position to the next non-whitespace
   --  character in Str

   procedure Skip_Spaces_Backward (Str : String; Index : in out Integer);
   --  Same as Skip_Spaces, but goes backward

   Exec_Suffix : constant GNAT.Strings.String_Access :=
      Get_Executable_Suffix;

   -----------------------
   -- Name_As_Directory --
   -----------------------

   function Name_As_Directory (Dir : String) return String is
   begin
      if Dir (Dir'Last) = Directory_Separator then
         return Dir;
      else
         return Dir & Directory_Separator;
      end if;
   end Name_As_Directory;

   ---------------------------
   -- Get_Program_Directory --
   ---------------------------

   function Get_Program_Directory return String is
      Command : constant String :=
        Containing_Directory (Ada.Command_Line.Command_Name);
      Normalized : constant String := Normalize_Pathname
        (Command & Directory_Separator & "..", Resolve_Links => True);
   begin
      if Is_Regular_File (Command & "src/gprconfig.ads") then
         --  Special case for gprconfig developers
         if Is_Directory (Command & "share") then
            return Command;
         end if;
      end if;

      if Normalized (Normalized'Last) = Directory_Separator then
         return Normalized;
      else
         return Normalized & Directory_Separator;
      end if;
   end Get_Program_Directory;

   --------
   -- TU --
   --------

   function TU (Str : String) return Unbounded_String is
   begin
      if Str = "" then
         return Null_Unbounded_String;
      else
         return To_Unbounded_String (Str);
      end if;
   end TU;

   --------------------------
   -- Parse_External_Value --
   --------------------------

   procedure Parse_External_Value
     (Value       : out External_Value;
      File        : String;
      Node        : Node_Ptr)
   is
      use type Glib.String_Ptr;
      Tmp : Node_Ptr := Node.Child;
   begin
      Value.Filter     := Null_Unbounded_String;
      Value.Must_Match := Null_Unbounded_String;

      if Node.Value /= null then
         Value := (Typ        => Value_Constant,
                   Filter     => Null_Unbounded_String,
                   Must_Match => Value.Must_Match,
                   Value      => To_Unbounded_String (Node.Value.all));
      end if;

      while Tmp /= null loop
         if Tmp.Tag.all = "external" then
            Value := (Typ        => Value_Shell,
                      Filter     => Value.Filter,
                      Must_Match => Value.Must_Match,
                      Command    => To_Unbounded_String (Tmp.Value.all),
                      Regexp     => To_Unbounded_String
                        (Get_Attribute (Tmp, "regexp", ".*")),
                      Group      => Integer'Value
                        (Get_Attribute (Tmp, "group", "0")));
         elsif Tmp.Tag.all = "directory" then
            Value := (Typ             => Value_Directory,
                      Filter          => Value.Filter,
                      Must_Match      => Value.Must_Match,
                      Directory       => To_Unbounded_String (Tmp.Value.all),
                      Directory_Group => Integer'Value
                        (Get_Attribute (Tmp, "group", "0")));
         elsif Tmp.Tag.all = "filter" then
            Value.Filter := To_Unbounded_String (Tmp.Value.all);
         elsif Tmp.Tag.all = "must_match" then
            Value.Must_Match := To_Unbounded_String (Tmp.Value.all);
         else
            Put_Line (Standard_Error, "Invalid XML description for "
                      & Node.Tag.all & " in file " & File);
            Put_Line (Standard_Error, "    Invalid tag: " & Tmp.Tag.all);
            Value := Null_External_Value;
         end if;

         Tmp := Tmp.Next;
      end  loop;

   exception
      when Constraint_Error =>
      Put_Line (Standard_Error, "Invalid group number for " & Node.Tag.all
                   & " in file " & File);
         Value := Null_External_Value;
   end Parse_External_Value;

   --------------------------------
   -- Parse_Compiler_Description --
   --------------------------------

   procedure Parse_Compiler_Description
     (Append_To   : in out Compiler_Description_Maps.Map;
      File        : String;
      Description : Node_Ptr)
   is
      Compiler : Compiler_Description;
      N        : Node_Ptr := Description.Child;
      Name     : String_Ptr;
   begin
      while N /= null loop
         if N.Tag.all = "executable" then
            Compiler.Executable := To_Unbounded_String (N.Value.all);
         elsif N.Tag.all = "name" then
            Name := N.Value;
         elsif N.Tag.all = "version" then
            Parse_External_Value
              (Value => Compiler.Version,
               File  => File,
               Node  => N);
         elsif N.Tag.all = "languages" then
            Parse_External_Value
              (Value => Compiler.Languages,
               File  => File,
               Node  => N);
         elsif N.Tag.all = "runtimes" then
            Parse_External_Value
              (Value => Compiler.Runtimes,
               File  => File,
               Node  => N);
         elsif N.Tag.all = "target" then
            Parse_External_Value
              (Value => Compiler.Target,
               File  => File,
               Node  => N);
         elsif N.Tag.all = "extra_tool" then
            Compiler.Extra_Tool := To_Unbounded_String (N.Value.all);
         else
            Put_Line (Standard_Error, "Unknown XML tag in " & File & ": "
                      & N.Tag.all);
         end if;

         N := N.Next;
      end loop;

      if Name /= null then
         Include (Append_To, Name.all, Compiler);
      end if;
   end Parse_Compiler_Description;

   -------------------------
   -- Parse_Configuration --
   -------------------------

   procedure Parse_Configuration
     (Append_To   : in out Configuration_Lists.List;
      File        : String;
      Description : Node_Ptr)
   is
      Config    : Configuration;
      N         : Node_Ptr := Description.Child;
      N2        : Node_Ptr;
      Compilers : Compilers_Filter;
      Ignore_Config : Boolean := False;
      Negate    : Boolean;
   begin
      Config.Supported := True;

      while N /= null loop
         if N.Tag.all = "compilers" then
            Compilers := No_Compilers_Filter;
            N2 := N.Child;
            while N2 /= null loop
               if N2.Tag.all = "compiler" then
                  Append
                    (Compilers.Compiler,
                     Compiler_Filter'
                       (Name     => TU (Get_Attribute (N2, "name", "")),
                        Version  => TU (Get_Attribute (N2, "version", "")),
                        Runtime  => TU (Get_Attribute (N2, "runtime", "")),
                        Language => TU (Get_Attribute (N2, "language", ""))));
               else
                  Put_Line (Standard_Error, "Unknown XML tag in " & File & ": "
                            & N2.Tag.all);
               end if;
               N2 := N2.Next;
            end loop;
            Compilers.Negate := Boolean'Value
              (Get_Attribute (N, "negate", "False"));
            Append (Config.Compilers_Filters, Compilers);

         elsif N.Tag.all = "targets" then
            if not Is_Empty (Config.Targets_Filters) then
               Put_Line (Standard_Error,
                         "Can have a single <targets> filter in " & File);
            else
               N2 := N.Child;
               while N2 /= null loop
                  if N2.Tag.all = "target" then
                     Append (Config.Targets_Filters,
                             Get_Attribute (N2, "name", ""));
                  else
                     Put_Line
                       (Standard_Error, "Unknown XML tag in " & File & ": "
                        & N2.Tag.all);
                  end if;
                  N2 := N2.Next;
               end loop;
               Config.Negate_Targets := Boolean'Value
                 (Get_Attribute (N, "negate", "False"));
            end if;

         elsif N.Tag.all = "hosts" then
            --  Resolve this filter immediately. This saves memory, since we
            --  don't need to store it in memory if we know it won't apply.
            N2 := N.Child;
            Negate := Boolean'Value
              (Get_Attribute (N, "negate", "False"));

            Ignore_Config := not Negate;
            while N2 /= null loop
               if N2.Tag.all = "host" then
                  if Match
                    (Get_Attribute (N2, "name", ""), Sdefault.Hostname)
                  then
                     Ignore_Config := Negate;
                     exit;
                  end if;

               else
                  Put_Line (Standard_Error, "Unknown XML tag in " & File & ": "
                            & N2.Tag.all);
               end if;

               N2 := N2.Next;
            end loop;

            exit when Ignore_Config;

         elsif N.Tag.all = "config" then
            if N.Value = null or else N.Value.all = "" then
               Config.Supported := False;
            else
               Append (Config.Config, N.Value.all);
            end if;

         else
            Put_Line (Standard_Error, "Unknown XML tag in " & File & ": "
                      & N.Tag.all);
         end if;

         N := N.Next;
      end loop;

      if not Ignore_Config then
         Append (Append_To, Config);
      end if;
   end Parse_Configuration;

   --------------------------
   -- Parse_Knowledge_Base --
   --------------------------

   procedure Parse_Knowledge_Base
     (Base : out Knowledge_Base; Directory : String)
   is
      Search    : Search_Type;
      File      : Directory_Entry_Type;
      File_Node : Node_Ptr;
      N         : Node_Ptr;
   begin
      Start_Search
        (Search,
         Directory => Directory,
         Pattern   => "*.xml",
         Filter    => (Ordinary_File => True, others => False));

      while More_Entries (Search) loop
         Get_Next_Entry (Search, File);

         File_Node := Parse (Full_Name (File));

         if File_Node = null then
            Put_Line (Standard_Error, "Could not parse " & Simple_Name (File));
         elsif File_Node.Tag.all = "gprconfig" then
            N := File_Node.Child;
            while N /= null loop
               if N.Tag.all = "compiler_description" then
                  Parse_Compiler_Description
                    (Append_To   => Base.Compilers,
                     File        => Simple_Name (File),
                     Description => N);
               elsif N.Tag.all = "configuration" then
                  Parse_Configuration
                    (Append_To   => Base.Configurations,
                     File        => Simple_Name (File),
                     Description => N);
               else
                  Put_Line (Standard_Error,
                            "Unknown XML tag in " & Simple_Name (File) & ": "
                            & N.Tag.all);
               end if;

               N := N.Next;
            end loop;
         else
            Put_Line (Standard_Error,
                      "Invalid toplevel XML tag in " & Simple_Name (File));
         end if;
         Free (File_Node);
      end loop;

      End_Search (Search);

   exception
      when Ada.Directories.Name_Error =>
         Put_Line (Standard_Error, "Directory not found: " & Directory);
   end Parse_Knowledge_Base;

   -----------------------------
   -- Substitute_Special_Dirs --
   -----------------------------

   function Substitute_Special_Dirs
     (Str         : String;
      Comp        : Compiler;
      Output_Dir  : String) return String
   is
      Pos    : Natural := Str'First;
      Last   : Natural := Str'First;
      Result : Unbounded_String;
   begin
      while Pos <= Str'Last loop
         if Str (Pos) = '$' then
            if Pos + 4 <= Str'Last
              and then Str (Pos .. Pos + 4) = "$HOST"
            then
               Append (Result, Str (Last .. Pos - 1));
               Append (Result, Sdefault.Hostname);
               Last := Pos + 5;
               Pos  := Pos + 4;

            elsif Pos + 6 <= Str'Last
              and then Str (Pos .. Pos + 6) = "$TARGET"
            then
               Append (Result, Str (Last .. Pos - 1));
               Append (Result, Comp.Target);
               Last := Pos + 7;
               Pos  := Pos + 6;

            elsif Pos + 11 <= Str'Last
              and then Str (Pos .. Pos + 11) = "$RUNTIME_DIR"
            then
               Append (Result, Str (Last .. Pos - 1));
               Append
                 (Result, Name_As_Directory (To_String (Comp.Runtime_Dir)));
               Last := Pos + 12;
               Pos  := Pos + 11;

            elsif Pos + 7 <= Str'Last
              and then Str (Pos .. Pos + 7) = "$VERSION"
            then
               Append (Result, Str (Last .. Pos - 1));
               Append (Result, Comp.Version);
               Last := Pos + 8;
               Pos  := Pos + 7;

            elsif Pos + 8 <= Str'Last
              and then Str (Pos .. Pos + 8) = "$LANGUAGE"
            then
               Append (Result, Str (Last .. Pos - 1));
               Append (Result, Comp.Language);
               Last := Pos + 9;
               Pos  := Pos + 8;

            elsif Pos + 7 <= Str'Last
              and then Str (Pos .. Pos + 7) = "$RUNTIME"
            then
               Append (Result, Str (Last .. Pos - 1));
               Append (Result, Comp.Runtime);
               Last := Pos + 8;
               Pos  := Pos + 7;

            elsif Pos + 4 <= Str'Last
              and then Str (Pos .. Pos + 4) = "$PATH"
            then
               Append (Result, Str (Last .. Pos - 1));
               Append (Result, Comp.Path);
               Last := Pos + 5;
               Pos  := Pos + 4;

            elsif Pos + 10 <= Str'Last
              and then Str (Pos .. Pos + 10) = "$OUTPUT_DIR"
            then
               Append (Result, Str (Last .. Pos - 1));
               Append (Result, Output_Dir);
               Last := Pos + 11;
               Pos  := Pos + 10;

            elsif Pos + 16 <= Str'Last
              and then Str (Pos .. Pos + 16) = "$GPRCONFIG_PREFIX"
            then
               Append (Result, Str (Last .. Pos - 1));
               Append (Result, Get_Program_Directory);
               Last := Pos + 17;
               Pos  := Pos + 16;

            end if;
         end if;
         Pos := Pos + 1;
      end loop;
      Append (Result, Str (Last .. Str'Last));
      return To_String (Result);
   end Substitute_Special_Dirs;

   --------------------
   -- Parse_All_Dirs --
   --------------------

   procedure Parse_All_Dirs
     (Processed_Value  : out External_Value_Lists.List;
      Current_Dir       : String;
      Path_To_Check     : String;
      Regexp            : Pattern_Matcher;
      Group             : Natural)
   is
      First : constant Integer := Path_To_Check'First;
      Last  : Integer;
   begin
      if Path_To_Check'Length = 0
        or else Path_To_Check = "/"
        or else Path_To_Check = "" & Directory_Separator
      then
         declare
            Matched : Match_Array (0 .. Group);
         begin
            --  We matched, so insert the relevant path. Convert the path to
            --  unix format first, so that the regexp is system-independent
            Match (Regexp, Format_Pathname (Current_Dir, UNIX), Matched);
            if Matched (Group) /= No_Match then
               Append
                 (Processed_Value,
                  (Value => To_Unbounded_String
                     (Current_Dir
                        (Matched (Group).First .. Matched (Group).Last)),
                   Extracted_From => To_Unbounded_String (Current_Dir)));
            end if;
         end;

      else
         Last := First + 1;
         while Last <= Path_To_Check'Last
           and then Path_To_Check (Last) /= '/'
           and then Path_To_Check (Last) /= Directory_Separator
         loop
            Last := Last + 1;
         end loop;

         --  If we do not have a regexp
         if GNAT.Regpat.Quote (Path_To_Check (First .. Last - 1)) =
           Path_To_Check (First .. Last - 1)
         then
            if Ada.Directories.Exists
              (Current_Dir & Directory_Separator
               & Path_To_Check (First .. Last - 1))
            then
               --  If there is such a subdir, keep checking
               Parse_All_Dirs
                 (Processed_Value => Processed_Value,
                  Current_Dir     =>
                    Normalize_Pathname (Current_Dir, Resolve_Links => False)
                    & Directory_Separator
                    & Path_To_Check (First .. Last - 1)
                    & Directory_Separator,
                  Path_To_Check   =>
                    Path_To_Check (Last + 1 .. Path_To_Check'Last),
                  Regexp          => Regexp,
                  Group           => Group);
            end if;

         --  Else we have a regexp, check all files
         else
            declare
               File_Regexp : constant Pattern_Matcher :=
                 Compile (Path_To_Check (First .. Last - 1));
               Search : Search_Type;
               File   : Directory_Entry_Type;
            begin
               Start_Search
                 (Search    => Search,
                  Directory => Current_Dir,
                  Pattern   => "");
               while More_Entries (Search) loop
                  Get_Next_Entry (Search, File);
                  if Simple_Name (File) /= "."
                    and then Simple_Name (File) /= ".."
                    and then Match (File_Regexp, Simple_Name (File))
                  then
                     Parse_All_Dirs
                       (Processed_Value => Processed_Value,
                        Current_Dir     =>
                          Full_Name (File) & Directory_Separator,
                        Path_To_Check   =>
                          Path_To_Check (Last + 1 .. Path_To_Check'Last),
                        Regexp          => Regexp,
                        Group           => Group);
                  end if;
               end loop;
            end;
         end if;
      end if;
   end Parse_All_Dirs;

   ------------------------
   -- Get_External_Value --
   ------------------------

   procedure Get_External_Value
     (Value            : External_Value;
      Comp             : Compiler;
      Split_Into_Words : Boolean := True;
      Processed_Value  : out External_Value_Lists.List)
   is
      Saved_Path : constant String := Ada.Environment_Variables.Value ("PATH");
      Status     : aliased Integer;
      Extracted_From : Unbounded_String;
      Result         : Unbounded_String;
      Split          : String_Lists.List;
      C              : String_Lists.Cursor;
   begin
      Clear (Processed_Value);
      case Value.Typ is
         when Value_Constant =>
            Result := To_Unbounded_String
              (Substitute_Special_Dirs
                 (To_String (Value.Value), Comp, Output_Dir => ""));

         when Value_Shell =>
            Ada.Environment_Variables.Set
              ("PATH", To_String (Comp.Path) & Path_Separator & Saved_Path);
            declare
               Command : constant String := Substitute_Special_Dirs
                 (To_String (Value.Command), Comp, Output_Dir => "");
               Args   : Argument_List_Access := Argument_String_To_List
                 (Command);
               Output : constant String := Get_Command_Output
                 (Command     => Args (Args'First).all,
                  Arguments   => Args (Args'First + 1 .. Args'Last),
                  Input       => "",
                  Status      => Status'Unchecked_Access,
                  Err_To_Out  => True);
               Regexp : constant Pattern_Matcher := Compile
                 (To_String (Value.Regexp), Multiple_Lines);
               Matched : Match_Array (0 .. Value.Group);
            begin
               GNAT.Strings.Free (Args);
               Ada.Environment_Variables.Set ("PATH", Saved_Path);

               Match (Regexp, Output, Matched);
               if Matched (Value.Group) /= No_Match then
                  Extracted_From := To_Unbounded_String (Output);
                  Result := To_Unbounded_String
                    (Output (Matched (Value.Group).First ..
                             Matched (Value.Group).Last));
               else
                  Extracted_From := Null_Unbounded_String;
                  Result         := Null_Unbounded_String;
               end if;
            end;

         when Value_Directory =>
            declare
               Search : constant String := Substitute_Special_Dirs
                 (To_String (Value.Directory), Comp, Output_Dir => "");
            begin
               Parse_All_Dirs
                 (Processed_Value => Processed_Value,
                  Current_Dir     => To_String (Comp.Path),
                  Path_To_Check   => Search,
                  Regexp          => Compile (Search),
                  Group           => Value.Directory_Group);
            end;
      end case;

      if Value.Must_Match /= Null_Unbounded_String
        and then not Match (Expression => To_String (Value.Must_Match),
                            Data       => To_String (Result))
      then
         raise Ignore_Compiler;
      end if;

      case Value.Typ is
         when Value_Directory =>
            null;  --  already split

         when Value_Shell | Value_Constant =>
            if Split_Into_Words then
               Get_Words (Words  => To_String (Result),
                          Filter => Value.Filter,
                          Map    => Split,
                          Allow_Empty_Elements => False);
               C := First (Split);
               while Has_Element (C) loop
                  Append
                    (Processed_Value,
                     External_Value_Item'
                       (Value          => To_Unbounded_String (Element (C)),
                        Extracted_From => Extracted_From));
                  Next (C);
               end loop;

            else
               Append
                 (Processed_Value,
                  External_Value_Item'
                    (Value => Result, Extracted_From => Extracted_From));
            end if;
      end case;
   end Get_External_Value;

   ---------------
   -- Get_Words --
   ---------------

   procedure Get_Words
     (Words  : String;
      Filter : Unbounded_String;
      Map    : out String_Lists.List;
      Allow_Empty_Elements : Boolean)
   is
      First      : Integer := Words'First;
      Last       : Integer;
      Filter_Set : String_Lists.List;
   begin
      if Filter /= Null_Unbounded_String then
         Get_Words (To_String (Filter), Null_Unbounded_String, Filter_Set,
                    Allow_Empty_Elements => True);
      end if;

      while First <= Words'Last
        and then (Words (First) = ' '
                  or else Words (First) = ',')
      loop
         First := First + 1;
      end loop;

      while First <= Words'Last loop
         if Words (First) /= ' ' and then Words (First) /= ',' then
            Last := First + 1;
            while Last <= Words'Last
              and then Words (Last) /= ' '
              and then Words (Last) /= ','
            loop
               Last := Last + 1;
            end loop;
         else
            Last := First;
         end if;

         if (Allow_Empty_Elements or else First <= Last - 1)
           and then
             (Is_Empty (Filter_Set)
              or else Contains (Filter_Set, Words (First .. Last - 1)))
         then
            Append (Map, Words (First .. Last - 1));
         end if;

         First := Last + 1;
      end loop;
   end Get_Words;

   -------------------------------
   -- For_Each_Language_Runtime --
   -------------------------------

   procedure For_Each_Language_Runtime
     (Append_To  : in out Compiler_Lists.List;
      Name       : String;
      Directory  : String;
      Descr      : Compiler_Description)
   is
      Target    : External_Value_Lists.List;
      Version   : External_Value_Lists.List;
      Languages : External_Value_Lists.List;
      Runtimes  : External_Value_Lists.List;
      Comp      : Compiler;
      C, C2     : External_Value_Lists.Cursor;
   begin
      Comp.Name       := To_Unbounded_String (Name);
      Comp.Path       := To_Unbounded_String (Directory);
      Comp.Extra_Tool := Descr.Extra_Tool;

      Get_External_Value
        (Value            => Descr.Target,
         Comp             => Comp,
         Split_Into_Words => False,
         Processed_Value  => Target);
      if not Is_Empty (Target) then
         Comp.Target := Element (First (Target)).Value;
      end if;

      Get_External_Value
        (Value            => Descr.Version,
         Comp             => Comp,
         Split_Into_Words => False,
         Processed_Value  => Version);

      --  If we can't find version, ignore this compiler
      if Is_Empty (Version) then
         raise Ignore_Compiler;
      end if;

      Comp.Version := Element (First (Version)).Value;

      Get_External_Value
        (Value            => Descr.Languages,
         Comp             => Comp,
         Split_Into_Words => True,
         Processed_Value  => Languages);
      Get_External_Value
        (Value            => Descr.Runtimes,
         Comp             => Comp,
         Split_Into_Words => True,
         Processed_Value  => Runtimes);

      C := First (Languages);
      while Has_Element (C) loop
         declare
            L : String := To_String (Element (C).Value);
         begin
            To_Mixed (L);
            Comp.Language := To_Unbounded_String (L);

            if Is_Empty (Runtimes) then
               Append (Append_To, Comp);
            else
               C2 := First (Runtimes);
               while Has_Element (C2) loop
                  Comp.Runtime     := Element (C2).Value;
                  Comp.Runtime_Dir := Element (C2).Extracted_From;
                  Append (Append_To, Comp);
                  Next (C2);
               end loop;
            end if;
         end;

         Next (C);
      end loop;
   end For_Each_Language_Runtime;

   ---------------------------
   -- Find_Compilers_In_Dir --
   ---------------------------

   procedure Find_Compilers_In_Dir
     (Append_To : in out Compiler_Lists.List;
      Base      : Knowledge_Base;
      Directory : String;
      Name      : String := "")
   is
      C      : Compiler_Description_Maps.Cursor;
   begin
      --  Do not search all entries in the directory, but check explictly for
      --  the compilers. This results in a lot less system calls, and thus is
      --  faster

      C := First (Base.Compilers);
      while Has_Element (C) loop
         if Name = "" or else Key (C) = Name then
            declare
               F : constant String := Normalize_Pathname
                 (Name           => To_String (Element (C).Executable),
                  Directory      => Directory,
                  Resolve_Links  => False,
                  Case_Sensitive => True) & Exec_Suffix.all;
            begin
               if Ada.Directories.Exists (F) then
                  For_Each_Language_Runtime
                    (Append_To  => Append_To,
                     Name       => Key (C),
                     Directory  => Directory,
                     Descr      => Element (C));
               end if;
            exception
               when Ada.Directories.Name_Error =>
                  null;
               when Ignore_Compiler =>
                  null;  --  Nothing to do, the compiler has not been inserted
            end;
         end if;

         Next (C);
      end loop;
   end Find_Compilers_In_Dir;

   -----------------------------
   -- Find_Matching_Compilers --
   -----------------------------

   procedure Find_Matching_Compilers
     (Name      : String;
      Path      : String;
      Base      : Knowledge_Base;
      Compilers : out Compiler_Lists.List)
   is
   begin
      Clear (Compilers);
      Find_Compilers_In_Dir
        (Append_To => Compilers,
         Base      => Base,
         Directory => Path,
         Name      => Name);
   end Find_Matching_Compilers;

   ----------------------------
   -- Find_Compilers_In_Path --
   ----------------------------

   procedure Find_Compilers_In_Path
     (Base      : Knowledge_Base;
      Compilers : out Compiler_Lists.List)
   is
      Map : String_Lists.List;
   begin
      if Ada.Environment_Variables.Exists ("PATH") then
         declare
            Path : constant String := Ada.Environment_Variables.Value ("PATH");
            First, Last : Natural;
         begin
            First := Path'First;
            while First <= Path'Last loop
               Last := First + 1;
               while Last <= Path'Last
                 and then Path (Last) /= GNAT.OS_Lib.Path_Separator
               loop
                  Last := Last + 1;
               end loop;

               --  Use a hash to make sure we do not parse the same directory
               --  twice. This is both more efficient and avoids duplicates in
               --  the final result list
               if not Contains (Map, Path (First .. Last - 1)) then
                  Append (Map, Path (First .. Last - 1));
                  Find_Compilers_In_Dir
                    (Append_To => Compilers,
                     Base      => Base,
                     Directory => Path (First .. Last - 1));
               end if;

               First := Last + 1;
            end loop;
         end;
      end if;
   end Find_Compilers_In_Path;

   --------------------------
   -- Known_Compiler_Names --
   --------------------------

   procedure Known_Compiler_Names
     (Base : Knowledge_Base;
      List : out Ada.Strings.Unbounded.Unbounded_String)
   is
      C : Compiler_Description_Maps.Cursor := First (Base.Compilers);
   begin
      List := Null_Unbounded_String;
      while Has_Element (C) loop
         if List /= Null_Unbounded_String then
            Append (List, ",");
         end if;
         Append (List, Key (C));

         Next (C);
      end loop;
   end Known_Compiler_Names;

   -----------
   -- Match --
   -----------

   procedure Match
     (Filter            : Compilers_Filter;
      Selected          : Compiler_Lists.List;
      Matching_Compiler : out Compiler;
      Matched           : out Boolean)
   is
      C : Compiler_Filter_Lists.Cursor := First (Filter.Compiler);
      M : Boolean;
   begin
      while Has_Element (C) loop
         Match (Element (C), Selected, Matching_Compiler, M);
         if M then
            Matched := not Filter.Negate;
            return;
         end if;
         Next (C);
      end loop;
      Matched := Filter.Negate;
   end Match;

   -----------
   -- Match --
   -----------

   procedure Match
     (Filter            : Compiler_Filter;
      Selected          : Compiler_Lists.List;
      Matching_Compiler : out Compiler;
      Matched           : out Boolean)
   is
      C    : Compiler_Lists.Cursor := First (Selected);
      Comp : Compiler;
   begin
      while Has_Element (C) loop
         Comp := Element (C);
         if Filter.Name = Comp.Name
           and then
             (Filter.Version = Null_Unbounded_String
              or else Match
                (Compile (To_String (Filter.Version), Case_Insensitive),
                 To_String (Comp.Version)))
           and then (Filter.Runtime = Null_Unbounded_String
              or else Match
                (Compile (To_String (Filter.Runtime), Case_Insensitive),
                 To_String (Comp.Runtime)))
           and then (Filter.Language = Null_Unbounded_String
              or else Match
                (Compile (To_String (Filter.Language), Case_Insensitive),
                 To_String (Comp.Language)))
         then
            Matching_Compiler := Comp;
            Matched           := True;
            return;
         end if;
         Next (C);
      end loop;
      Matched := False;
   end Match;

   -----------
   -- Match --
   -----------

   procedure Match
     (Filter            : Compilers_Filter_Lists.List;
      Selected          : Compiler_Lists.List;
      Matching_Compiler : out Compiler;
      Matched           : out Boolean)
   is
      use Ada.Containers;
      C : Compilers_Filter_Lists.Cursor := First (Filter);
      M : Boolean;
   begin
      while Has_Element (C) loop
         Match (Element (C), Selected, Matching_Compiler, M);
         if not M then
            Matched := False;
            return;
         end if;
         Next (C);
      end loop;

      if Length (Filter) > 1 then
         Matching_Compiler := No_Compiler;
      end if;
      Matched := True;
   end Match;

   -----------
   -- Match --
   -----------

   function Match
     (Target_Filter : String_Lists.List;
      Negate        : Boolean;
      Selected      : Compiler_Lists.List) return Boolean
   is
      Target : String_Lists.Cursor := First (Target_Filter);
      Comp   : Compiler_Lists.Cursor;
   begin
      if Is_Empty (Target_Filter) then
         return True;

      else
         while Has_Element (Target) loop
            Comp := First (Selected);
            while Has_Element (Comp) loop
               if Match
                 (Compile (Element (Target), Case_Insensitive),
                  To_String (Element (Comp).Target))
               then
                  return not Negate;
               end if;
               Next (Comp);
            end loop;

            Next (Target);
         end loop;
         return Negate;
      end if;
   end Match;

   -----------------
   -- Skip_Spaces --
   -----------------

   procedure Skip_Spaces (Str : String; Index : in out Integer) is
   begin
      while Index <= Str'Last
        and then (Str (Index) = ' ' or else Str (Index) = ASCII.LF)
      loop
         Index := Index + 1;
      end loop;
   end Skip_Spaces;

   procedure Skip_Spaces_Backward (Str : String; Index : in out Integer) is
   begin
      while Index >= Str'First
        and then (Str (Index) = ' ' or else Str (Index) = ASCII.LF)
      loop
         Index := Index - 1;
      end loop;
   end Skip_Spaces_Backward;

   ------------------
   -- Merge_Config --
   ------------------

   procedure Merge_Config
     (Packages          : in out String_Maps.Map;
      Selected_Compiler : Compiler;
      Config            : String;
      Output_Dir        : String)
   is
      procedure Add_Package
        (Name : String; Chunk : String; Prefix : String := "      ");
      --  Add the chunk in the appropriate package

      procedure Add_Package
        (Name : String; Chunk : String; Prefix : String := "      ")
      is
         C : constant String_Maps.Cursor := Find (Packages, Name);
         Replaced : constant String := Substitute_Special_Dirs
           (Chunk, Selected_Compiler, Output_Dir);
      begin
         if Replaced /= "" then
            if Has_Element (C) then
               Replace_Element
                 (Packages,
                  C,
                  Element (C) & ASCII.LF & Prefix
                  & To_Unbounded_String (Replaced));
            else
               Insert
                 (Packages,
                  Name, Prefix & To_Unbounded_String (Replaced));
            end if;
         end if;
      end Add_Package;

      First : Integer := Config'First;
      Pkg_Name_First, Pkg_Name_Last : Integer;
      Pkg_Content_First : Integer;
      Last  : Integer;

   begin
      while First /= 0 and then First <= Config'Last loop
         --  Do we have a toplevel attribute ?
         Skip_Spaces (Config, First);
         Pkg_Name_First := Index (Config (First .. Config'Last), "package ");
         if Pkg_Name_First = 0 then
            Pkg_Name_First := Config'Last + 1;
         end if;

         Last := Pkg_Name_First - 1;
         Skip_Spaces_Backward (Config, Last);
         Add_Package
           (Name   => "",
            Chunk  => Config (First .. Last),
            Prefix => "   ");

         exit when Pkg_Name_First > Config'Last;

         --  Parse the current package
         Pkg_Name_First := Pkg_Name_First + 8;  --  skip "package "
         Skip_Spaces (Config, Pkg_Name_First);

         Pkg_Name_Last := Pkg_Name_First + 1;
         while Pkg_Name_Last <= Config'Last
           and then Config (Pkg_Name_Last) /= ' '
           and then Config (Pkg_Name_Last) /= ASCII.LF
         loop
            Pkg_Name_Last := Pkg_Name_Last + 1;
         end loop;

         Pkg_Content_First := Pkg_Name_Last + 1;
         Skip_Spaces (Config, Pkg_Content_First);
         Pkg_Content_First := Pkg_Content_First + 2; --  skip "is"
         Skip_Spaces (Config, Pkg_Content_First);

         Last := Index (Config (Pkg_Content_First .. Config'Last),
                        "end " & Config (Pkg_Name_First .. Pkg_Name_Last - 1));
         if Last /= 0 then
            First := Last - 1;
            Skip_Spaces_Backward (Config, First);
            Add_Package
              (Name  => Config (Pkg_Name_First .. Pkg_Name_Last - 1),
               Chunk => Config (Pkg_Content_First .. First));

            while Last <= Config'Last and then Config (Last) /= ';' loop
               Last := Last + 1;
            end loop;
            Last := Last + 1;
         end if;
         First := Last;
      end loop;
   end Merge_Config;

   -------------------------
   -- Is_Supported_Config --
   -------------------------

   function Is_Supported_Config
     (Base     : Knowledge_Base;
      Selected : Compiler_Lists.List) return Boolean
   is
      Config : Configuration_Lists.Cursor := First (Base.Configurations);
      M      : Boolean;
      Matching_Compiler : Compiler;
   begin
      while Has_Element (Config) loop
         Match (Element (Config).Compilers_Filters, Selected,
                Matching_Compiler, M);
         if M and then Match
           (Element (Config).Targets_Filters,
            Element (Config).Negate_Targets,
            Selected)
         then
            if not Element (Config).Supported then
               return False;
            end if;
         end if;
         Next (Config);
      end loop;
      return True;
   end Is_Supported_Config;

   ----------------------------
   -- Generate_Configuration --
   ----------------------------

   procedure Generate_Configuration
     (Base        : Knowledge_Base;
      Selected    : Compiler_Lists.List;
      Output_File : String)
   is
      Config   : Configuration_Lists.Cursor := First (Base.Configurations);
      Output            : File_Type;
      Packages          : String_Maps.Map;
      C                 : String_Maps.Cursor;
      Selected_Compiler : Compiler;
      M                 : Boolean;
      Comp              : Compiler_Lists.Cursor;
      Project_Name      : String := Ada.Directories.Base_Name (Output_File);
   begin
      To_Mixed (Project_Name);

      while Has_Element (Config) loop
         Match (Element (Config).Compilers_Filters, Selected,
               Selected_Compiler, M);

         if M
           and then Match (Element (Config).Targets_Filters,
                           Element (Config).Negate_Targets,
                           Selected)
         then
            if not Element (Config).Supported then
               Put_Line
                 (Standard_Error,
                  "Code generated by these compilers cannot be linked together"
                  & " as far as we know.");
               return;
            end if;

            Merge_Config
              (Packages,
               Selected_Compiler,
               To_String (Element (Config).Config),
               Output_Dir => Name_As_Directory
                 (Containing_Directory (Output_File)));
         end if;

         Next (Config);
      end loop;

      Put_Line ("Creating configuration file: " & Output_File);

      Create (Output, Out_File, Output_File);
      Put_Line (Output, "project " & Project_Name & " is");

      C := First (Packages);
      while Has_Element (C) loop
         if Key (C) /= "" then
            Put_Line (Output, "   package " & Key (C) & " is");
         end if;
         Put_Line (Output, To_String (Element (C)));
         if Key (C) /= "" then
            Put_Line (Output, "   end " & Key (C) & ";");
         end if;
         Next (C);
      end loop;

      Put_Line (Output, "end " & Project_Name & ";");

      Close (Output);

      --  Launch external tools
      Comp := First (Selected);
      while Has_Element (Comp) loop
         if Element (Comp).Extra_Tool /= Null_Unbounded_String
           and then Element (Comp).Extra_Tool /= ""
         then
            declare
               Command : constant String := Substitute_Special_Dirs
                 (Str        => To_String (Element (Comp).Extra_Tool),
                  Comp       => Element (Comp),
                  Output_Dir => Containing_Directory (Output_File));
               Args : Argument_List_Access := Argument_String_To_List
                 (Command);
               Status  : Integer;
               pragma Unreferenced (Status);
            begin
               New_Line;
               Put_Line ("Executing " & Command);
               Status := Spawn
                 (Args (Args'First).all, Args (Args'First + 1 .. Args'Last));
               GNAT.Strings.Free (Args);
            end;
         end if;

         Next (Comp);
      end loop;

   exception
      when Ada.Directories.Name_Error =>
         Put_Line ("Could not create the file " & Output_File);
   end Generate_Configuration;

   ------------------------
   -- Architecture_Equal --
   ------------------------

   function Architecture_Equal (Arch1, Arch2 : String) return Boolean is
   begin
      return Arch1 = Arch2;
   end Architecture_Equal;

end GprConfig.Knowledge;
