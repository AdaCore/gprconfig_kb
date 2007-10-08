------------------------------------------------------------------------------
--                   Copyright (C) 2006-2007, AdaCore                       --
------------------------------------------------------------------------------

with Ada.Characters.Handling;   use Ada.Characters.Handling;
with Ada.Command_Line;          use Ada.Command_Line;
with Ada.Containers;            use Ada.Containers;
with Ada.Directories;           use Ada.Directories;
with Ada.Environment_Variables; use Ada.Environment_Variables;
with Ada.Exceptions;            use Ada.Exceptions;
with Ada.IO_Exceptions;
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
   use External_Value_Nodes;

   Case_Sensitive_Files : constant Boolean := Directory_Separator = '\';
   On_Windows           : constant Boolean := Directory_Separator = '\';

   Ignore_Compiler : exception;
   --  Raised when the compiler should be ignored

   Indentation_Level : Integer := 0;
   --  Current indentation level for traces

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

   procedure Parse_Targets_Set
     (Append_To   : in out Targets_Set_Vectors.Vector;
      File        : String;
      Description : Node_Ptr);
   --  Parse a targets set node

   procedure Parse_External_Value
     (Value       : out External_Value;
      File        : String;
      Node        : Node_Ptr);
   --  Parse an XML node that describes an external value

   procedure Find_Compilers_In_Dir
     (Append_To  : in out Compiler_Lists.List;
      Base       : in out Knowledge_Base;
      Check_Executable_Regexp : Boolean;
      Directory  : String;
      Matching   : Compiler  := No_Compiler;
      On_Target  : Targets_Set_Id;
      Path_Order : Integer;
      Stop_At_First_Match : Boolean);
   --  Find all known compilers in a specific directory, that match
   --  Matching (ignoring unset fields in Matching).
   --  Check_Executable_Regexp should be set to true if at least one of the
   --  <executable> nodes we are investigating can be a regexp. This changes
   --  the algorithm used.
   --  If Stop_At_First_Match is true, then only the first compiler found will
   --  be returned, instead of looking for all matching compilers. This
   --  provides a significant speed up in most cases

   procedure Get_External_Value
     (Attribute        : String;
      Value            : External_Value;
      Comp             : Compiler;
      Split_Into_Words : Boolean := True;
      Matching         : String := "";
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
   --  If Matching is specified, then only those values equal to Matching are
   --  stored in Processed_Value.
   --  Comparisong with Matching is case-insensitive (this is needed for
   --  languages, does not matter for versions, is not used for targets)

   procedure For_Each_Language_Runtime
     (Append_To  : in out Compiler_Lists.List;
      Base       : in out Knowledge_Base;
      Name       : String;
      Executable : String;
      Directory  : String;
      Prefix     : String;
      Matching   : Compiler;
      On_Target  : Targets_Set_Id;
      Descr      : Compiler_Description;
      Path_Order : Integer;
      Stop_At_First_Match : Boolean);
   --  For each language/runtime parsed in Languages/Runtimes, create a new
   --  compiler in the list, if it matches Matching.
   --  If Stop_At_First_Match is true, then only the first matching compiler is
   --  returned, which provides a significant speedup in some cases

   function Is_Windows_Executable (Filename : String) return Boolean;
   --  Verify that a given filename is indeed an executable

   procedure Parse_All_Dirs
     (Processed_Value  : out External_Value_Lists.List;
      Current_Dir       : String;
      Path_To_Check     : String;
      Regexp            : Pattern_Matcher;
      Regexp_Str        : String;
      Value_If_Match    : String;
      Group             : Integer;
      Group_Match       : String := "";
      Group_Count       : Natural := 0);
   --  Parse all subdirectories of Current_Dir for those that match
   --  Path_To_Check (see description of <directory>). When a match is found,
   --  the regexp is evaluated against the current directory, and the matching
   --  parenthesis group is appended to Append_To (comma-separated).
   --  If Group is -1, then Value_If_Match is used instead of the parenthesis
   --  group.
   --  Group_Match is the substring that matched Group (if it has been matched
   --  already). Group_Count is the number of parenthesis groups that have been
   --  processed so far. The idea is to compute the matching substring as we
   --  go, since the regexp might no longer match in the end, if for instance
   --  it includes ".." directories.

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

   function Is_Regexp (Str : String) return Boolean;
   --  Whether Str is a regular expression

   Exec_Suffix : constant GNAT.Strings.String_Access :=
     Get_Executable_Suffix;

   function Unquote
     (Str : String; Remove_Quoted : Boolean := False) return String;
   --  Remove special '\' quoting characters from Str.
   --  As a special case, if Remove_Quoted is true, then '\' and the following
   --  char are simply omitted in the output.
   --  For instance:
   --      Str="A\." Remove_Quoted=False  => output is "A."
   --      Str="A\." Remove_Quoted=False  => output is "A"
   -------------
   -- Unquote --
   -------------

   function Unquote
     (Str : String; Remove_Quoted : Boolean := False) return String
   is
      Str2  : String (Str'Range);
      S     : Integer := Str'First;
      Index : Integer := Str2'First;
   begin
      while S <= Str'Last loop
         if Str (S) = '\' then
            S := S + 1;
            if not Remove_Quoted then
               Str2 (Index) := Str (S);
               Index := Index + 1;
            end if;
         else
            Str2 (Index) := Str (S);
            Index := Index + 1;
         end if;
         S     := S + 1;
      end loop;
      return Str2 (Str2'First .. Index - 1);
   end Unquote;

   ---------------------------
   -- Is_Windows_Executable --
   ---------------------------

   function Is_Windows_Executable (Filename : String) return Boolean is

      type Byte is mod 256;
      for Byte'Size use 8;
      for Byte'Alignment use 1;
      type Bytes is array (Positive range <>) of Byte;

      Windows_Pattern : constant Bytes := (77, 90, 144, 0);

      Fd : constant File_Descriptor := Open_Read (Filename, Binary);
      B  : Bytes (1 .. 4);
      N_Read : Integer;
   begin
      N_Read := Read (Fd, B'Address, 4);
      Close (Fd);
      if N_Read < 4 then
         return False;
      else
         if B = Windows_Pattern then
            return True;
         else
            return False;
         end if;
      end if;
   end Is_Windows_Executable;

   ---------------
   -- Is_Regexp --
   ---------------

   function Is_Regexp (Str : String) return Boolean is
      --  Take into account characters quoted by '\'. We just remove them for
      --  now, so that when we quote the regexp it won't see these potentially
      --  special characters.
      --  The goal is that for instance "\.\." is not considered as a regexp,
      --  but "\.." is.
      Str2 : constant String := Unquote (Str, Remove_Quoted => True);
   begin
      return GNAT.Regpat.Quote (Str2) /= Str2;
   end Is_Regexp;

   -----------------
   -- Put_Verbose --
   -----------------

   procedure Put_Verbose (Str : String; Indent_Delta : Integer := 0) is
   begin
      if Verbose_Level > 0 then
         if Indent_Delta < 0 then
            Indentation_Level := Indentation_Level - 2;
         end if;

         if Str /= "" then
            Put_Line (Standard_Error, (1 .. Indentation_Level => ' ') & Str);
         end if;

         if Indent_Delta > 0 then
            Indentation_Level := Indentation_Level + 2;
         end if;
      end if;
   end Put_Verbose;

   -----------------------
   -- Name_As_Directory --
   -----------------------

   function Name_As_Directory (Dir : String) return String is
   begin
      if Dir (Dir'Last) = Directory_Separator
        or else Dir (Dir'Last) = '/'
      then
         return Dir;
      else
         return Dir & Directory_Separator;
      end if;
   end Name_As_Directory;

   ---------------------------
   -- Get_Program_Directory --
   ---------------------------

   function Get_Program_Directory return String is
      function Get_Command return String;
      --  Return the full path to the command being executed

      function Get_Command return String is
         Tmp : constant String := Dir_Name (Ada.Command_Line.Command_Name);
         Tmp2 : GNAT.Strings.String_Access;
      begin
         --  On unix, command_name doesn't include the directory name when the
         --  command was found on the PATH. In such a case, which check on the
         --  PATH ourselves to find it.

         if Tmp = "" or else Tmp = "./" then
            Tmp2 := Locate_Exec_On_Path (Ada.Command_Line.Command_Name);
            if GNAT.Strings."=" (Tmp2, null) then
               return Tmp;
            else
               declare
                  S : constant String := Containing_Directory (Tmp2.all);
               begin
                  GNAT.Strings.Free (Tmp2);
                  return S;
               end;
            end if;
         else
            return Containing_Directory (Ada.Command_Line.Command_Name);
         end if;
      end Get_Command;

      Command : constant String := Name_As_Directory (Get_Command);
      Normalized : constant String := Normalize_Pathname
        (Command & "..", Resolve_Links => True);
   begin
      if Is_Regular_File (Command & "src/gprconfig.ads") then
         --  Special case for gprconfig developers
         if Is_Directory (Command & "share") then
            return Command;
         end if;
      end if;

      return Name_As_Directory (Normalized);
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
      Tmp           : Node_Ptr := Node.Child;
      External_Node : External_Value_Node;
   begin
      Value.Filter     := Null_Unbounded_String;
      Value.Must_Match := Null_Unbounded_String;

      if Node.Value /= null then
         External_Node := (Typ        => Value_Constant,
                           Value      => To_Unbounded_String (Node.Value.all));
         Append (Value.Nodes, External_Node);
      end if;

      while Tmp /= null loop
         if Tmp.Tag.all = "external" then
            External_Node :=
              (Typ        => Value_Shell,
               Command    => To_Unbounded_String (Tmp.Value.all),
               Regexp     => To_Unbounded_String
                 (Get_Attribute (Tmp, "regexp", ".*")),
               Group      => Integer'Value
                 (Get_Attribute (Tmp, "group", "0")));
            Append (Value.Nodes, External_Node);
         elsif Tmp.Tag.all = "directory" then
            External_Node :=
              (Typ             => Value_Directory,
               Directory       => To_Unbounded_String (Tmp.Value.all),
               Dir_If_Match    => Null_Unbounded_String,
               Directory_Group => 0);
            begin
               External_Node.Directory_Group := Integer'Value
                 (Get_Attribute (Tmp, "group", "0"));
            exception
               when Constraint_Error =>
                  External_Node.Directory_Group := -1;
                  External_Node.Dir_If_Match :=
                    To_Unbounded_String (Get_Attribute (Tmp, "group", "0"));
            end;

            Append (Value.Nodes, External_Node);
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
            declare
               Prefix : constant String := Get_Attribute (N, "prefix", "@@");
               Val    : constant String := N.Value.all;
            begin
               Compiler.Executable := To_Unbounded_String (Val);
               Compiler.Prefix_Index := Integer'Value (Prefix);
               Compiler.Executable_Re := new Pattern_Matcher'
                 (Compile (To_String ("^" & Compiler.Executable
                                      & Exec_Suffix.all & "$")));

            exception
               when Expression_Error =>
                  Put_Line
                    ("Invalid regular expression found in the configuration"
                     & " files: " & To_String (Compiler.Executable)
                     & " while parsing " & File);
                  raise Invalid_Knowledge_Base;

               when Constraint_Error =>
                  Compiler.Prefix_Index := -1;
                  null;
            end;

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

   -----------------------
   -- Parse_Targets_Set --
   -----------------------

   procedure Parse_Targets_Set
     (Append_To   : in out Targets_Set_Vectors.Vector;
      File        : String;
      Description : Node_Ptr)
   is
      Set      : Target_Lists.List;
      Pattern  : Pattern_Matcher_Access;
      N        : Node_Ptr := Description.Child;
   begin
      while N /= null loop
         if N.Tag.all = "target" then
            declare
               Val    : constant String := N.Value.all;
            begin
               Pattern := new Pattern_Matcher'(Compile ("^" & Val & "$"));
               Target_Lists.Append (Set, Pattern);

            exception
               when Expression_Error =>
                  Put_Line
                    ("Invalid regular expression " & Val
                     & " found in the target-set while parsing " & File);
                  raise Invalid_Knowledge_Base;

            end;

         else
            Put_Line (Standard_Error, "Unknown XML tag in " & File & ": "
                      & N.Tag.all);
         end if;

         N := N.Next;
      end loop;

      if not Target_Lists.Is_Empty (Set) then
         Targets_Set_Vectors.Append (Append_To, Set);
      end if;
   end Parse_Targets_Set;

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
      Put_Verbose ("Parsing knowledge base at " & Directory);
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
               elsif N.Tag.all = "targetset" then
                  Parse_Targets_Set
                    (Append_To   => Base.Targets_Sets,
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
      when E : others =>
         Put_Verbose ("Unexpected exception while parsing knowledge base: "
                      & Exception_Information (E));
         raise Invalid_Knowledge_Base;
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
      Word_Start, Word_End, Tmp : Natural;
   begin
      while Pos < Str'Last loop
         if Str (Pos) = '$' and then Str (Pos + 1) = '$' then
            Append (Result, Str (Last .. Pos - 1));
            Append (Result, "$");
            Last := Pos + 2;
            Pos  := Last;

         elsif Str (Pos) = '$' then
            if Str (Pos + 1)  = '{' then
               Word_Start := Pos + 2;
               Tmp := Pos + 2;
               while Tmp <= Str'Last and then Str (Tmp) /= '}' loop
                  Tmp := Tmp + 1;
               end loop;
               Tmp := Tmp + 1;
               Word_End := Tmp - 2;
            else
               Word_Start := Pos + 1;
               Tmp := Pos + 1;
               while Tmp <= Str'Last
                 and then (Is_Alphanumeric (Str (Tmp)) or Str (Tmp) = '_')
               loop
                  Tmp := Tmp + 1;
               end loop;
               Word_End := Tmp - 1;
            end if;

            Append (Result, Str (Last ..  Pos - 1));

            if Str (Word_Start .. Word_End) = "HOST" then
               Append (Result, Sdefault.Hostname);
            elsif Str (Word_Start .. Word_End) = "TARGET" then
               Append (Result, Comp.Target);
            elsif Str (Word_Start .. Word_End) = "RUNTIME_DIR" then
               Append
                 (Result, Name_As_Directory (To_String (Comp.Runtime_Dir)));
            elsif Str (Word_Start .. Word_End) = "EXEC" then
               Append (Result, Comp.Executable);
            elsif Str (Word_Start .. Word_End) = "VERSION" then
               Append (Result, Comp.Version);
            elsif Str (Word_Start .. Word_End) = "VERSION2" then
               Append (Result, Comp.Version2);
            elsif Str (Word_Start .. Word_End) = "LANGUAGE" then
               Append (Result, Comp.Language);
            elsif Str (Word_Start .. Word_End) = "RUNTIME" then
               Append (Result, Comp.Runtime);
            elsif Str (Word_Start .. Word_End) = "PREFIX" then
               Append (Result, Comp.Prefix);
            elsif Str (Word_Start .. Word_End) = "PATH" then
               Append (Result, Name_As_Directory (To_String (Comp.Path)));
            elsif Str (Word_Start .. Word_End) = "OUTPUT_DIR" then
               Append (Result, Name_As_Directory (Output_Dir));
            elsif Str (Word_Start .. Word_End) = "GPRCONFIG_PREFIX" then
               Append (Result, Get_Program_Directory);
            end if;

            Last := Tmp;
            Pos  := Last;
         else
            Pos := Pos + 1;
         end if;
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
      Regexp_Str        : String;
      Value_If_Match    : String;
      Group             : Integer;
      Group_Match       : String := "";
      Group_Count       : Natural := 0)
   is
      First : constant Integer := Path_To_Check'First;
      Last  : Integer;
   begin
      if Path_To_Check'Length = 0
        or else Path_To_Check = "/"
        or else Path_To_Check = "" & Directory_Separator
      then
         if Group = -1 then
            Put_Verbose ("<dir>: SAVE " & Current_Dir);
            Append
              (Processed_Value,
               (Value => To_Unbounded_String (Value_If_Match),
                Extracted_From => To_Unbounded_String (Current_Dir)));
         else
            Put_Verbose ("<dir>: SAVE " & Current_Dir);
            Append
              (Processed_Value,
               (Value          => To_Unbounded_String (Group_Match),
                Extracted_From => To_Unbounded_String (Current_Dir)));
         end if;

      else
         --  Do not split on '\', since we document we only accept UNIX paths
         --  anyway. This leaves \ for regexp quotes
         Last := First + 1;
         while Last <= Path_To_Check'Last
           and then Path_To_Check (Last) /= '/'
         loop
            Last := Last + 1;
         end loop;

         --  If we do not have a regexp.
         --  ??? Should we ignore symbolic links (see commented code below),
         --  since for instance for the GNAT runtime we could have an
         --  "adainclude" link pointing to "rts-native/adainclude", and
         --  therefore the runtime appears twice. Since it appears with
         --  different names ("default" and "native"), we currently leave both
         if not Is_Regexp (Path_To_Check (First .. Last - 1)) then
            declare
               Dir : constant String :=
                 Normalize_Pathname (Current_Dir, Resolve_Links => False)
                 & Directory_Separator
                 & Unquote (Path_To_Check (First .. Last - 1));
               Remains : constant String :=
                 Path_To_Check (Last + 1 .. Path_To_Check'Last);
            begin
               if Ada.Directories.Exists (Dir) then
                  Put_Verbose ("<dir>: Recurse into " & Dir);
                  --  If there is such a subdir, keep checking
                  Parse_All_Dirs
                    (Processed_Value => Processed_Value,
                     Current_Dir     => Dir & Directory_Separator,
                     Path_To_Check   => Remains,
                     Regexp          => Regexp,
                     Regexp_Str      => Regexp_Str,
                     Value_If_Match  => Value_If_Match,
                     Group           => Group,
                     Group_Match     => Group_Match,
                     Group_Count     => Group_Count);
               else
                  Put_Verbose ("<dir>: No such directory: " & Dir);
               end if;
            end;

         --  Else we have a regexp, check all files
         else
            declare
               File_Re : constant String := Path_To_Check (First .. Last - 1);
               File_Regexp : constant Pattern_Matcher := Compile (File_Re);
               Search : Search_Type;
               File   : Directory_Entry_Type;
               Filter : Ada.Directories.Filter_Type;
            begin
               if Verbose_Level > 0 and then File_Re = ".." then
                  Put_Verbose
                    ("Potential error: .. is generally not meant as a regexp,"
                     & " and should be quoted in this case, as in \.\.");
               end if;

               if Path_To_Check (Last) = '/' then
                  Put_Verbose
                    ("<dir>: Check directories in " & Current_Dir
                     & " that match " & File_Re);
                  Filter := (Directory => True, others => False);
               else
                  Put_Verbose
                    ("<dir>: Check files in " & Current_Dir
                     & " that match " & File_Re);
                  Filter := (others => True);
               end if;

               Start_Search
                 (Search    => Search,
                  Directory => Current_Dir,
                  Filter    => Filter,
                  Pattern   => "");
               while More_Entries (Search) loop
                  Get_Next_Entry (Search, File);
                  if Simple_Name (File) /= "."
                    and then Simple_Name (File) /= ".."
                  then
                     declare
                        Matched : Match_Array
                          (0 .. Integer'Max (Group, 0));
                        Simple  : constant String := Simple_Name (File);
                        Count  : constant Natural := Paren_Count (File_Regexp);
                     begin
                        Match (File_Regexp, Simple, Matched);
                        if Matched (0) /= No_Match then
                           Put_Verbose
                             ("<dir>: Matched " & Simple_Name (File));

                           if Group_Count < Group
                             and then Group_Count + Count >= Group
                           then
                              Put_Verbose
                                ("<dir>: Found matched group: "
                                 & Simple (Matched (Group - Group_Count).First
                                   .. Matched (Group - Group_Count).Last));
                              Parse_All_Dirs
                                (Processed_Value => Processed_Value,
                                 Current_Dir     =>
                                   Full_Name (File) & Directory_Separator,
                                 Path_To_Check   => Path_To_Check
                                   (Last + 1 .. Path_To_Check'Last),
                                 Regexp          => Regexp,
                                 Regexp_Str      => Regexp_Str,
                                 Value_If_Match  => Value_If_Match,
                                 Group           => Group,
                                 Group_Match     =>
                                   Simple (Matched (Group - Group_Count).First
                                       .. Matched (Group - Group_Count).Last),
                                 Group_Count     => Group_Count + Count);

                           else
                              Parse_All_Dirs
                                (Processed_Value => Processed_Value,
                                 Current_Dir     =>
                                   Full_Name (File) & Directory_Separator,
                                 Path_To_Check   => Path_To_Check
                                   (Last + 1 .. Path_To_Check'Last),
                                 Regexp          => Regexp,
                                 Regexp_Str      => Regexp_Str,
                                 Value_If_Match  => Value_If_Match,
                                 Group           => Group,
                                 Group_Match     => Group_Match,
                                 Group_Count     => Group_Count + Count);
                           end if;
                        end if;
                     end;
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
     (Attribute        : String;
      Value            : External_Value;
      Comp             : Compiler;
      Split_Into_Words : Boolean := True;
      Matching         : String := "";
      Processed_Value  : out External_Value_Lists.List)
   is
      Saved_Path : constant String := Ada.Environment_Variables.Value ("PATH");
      Status     : aliased Integer;
      Extracted_From : Unbounded_String;
      Tmp_Result     : Unbounded_String;
      Split          : String_Lists.List;
      C              : String_Lists.Cursor;
      Node_Cursor    : External_Value_Nodes.Cursor := First (Value.Nodes);
      Node           : External_Value_Node;
      Cursor, Cursor2 : External_Value_Lists.Cursor;
   begin
      Clear (Processed_Value);

      while Has_Element (Node_Cursor) loop
         Node           := Element (Node_Cursor);
         Tmp_Result     := Null_Unbounded_String;
         Extracted_From := Null_Unbounded_String;

         case Node.Typ is
            when Value_Constant =>
               Tmp_Result := To_Unbounded_String
                 (Substitute_Special_Dirs
                    (To_String (Node.Value), Comp, Output_Dir => ""));
               Put_Verbose
                 (Attribute & ": constant := " & To_String (Tmp_Result));

            when Value_Shell =>
               Ada.Environment_Variables.Set
                 ("PATH", To_String (Comp.Path) & Path_Separator & Saved_Path);
               declare
                  Command : constant String := Substitute_Special_Dirs
                    (To_String (Node.Command), Comp, Output_Dir => "");
               begin
                  Extracted_From := Null_Unbounded_String;
                  Tmp_Result     := Null_Unbounded_String;
                  declare
                     Args   : Argument_List_Access := Argument_String_To_List
                       (Command);
                     Output : constant String := Get_Command_Output
                       (Command     => Args (Args'First).all,
                        Arguments   => Args (Args'First + 1 .. Args'Last),
                        Input       => "",
                        Status      => Status'Unchecked_Access,
                        Err_To_Out  => True);
                     Regexp : constant Pattern_Matcher := Compile
                       (To_String (Node.Regexp), Multiple_Lines);
                     Matched : Match_Array (0 .. Node.Group);
                  begin
                     GNAT.Strings.Free (Args);
                     Ada.Environment_Variables.Set ("PATH", Saved_Path);

                     Match (Regexp, Output, Matched);
                     if Matched (Node.Group) /= No_Match then
                        Extracted_From := To_Unbounded_String (Output);
                        Tmp_Result := To_Unbounded_String
                          (Output (Matched (Node.Group).First ..
                             Matched (Node.Group).Last));
                        if Verbose_Level > 1 then
                           Put_Verbose
                             (Attribute & ": executing """ & Command
                              & """ output=""" & Output & """"
                              & " matched=""" & To_String (Tmp_Result)
                              & """");
                        elsif Verbose_Level > 0 then
                           Put_Verbose
                             (Attribute & ": executing """ & Command
                              & """ output=<use -v -v> matched="""
                              & To_String (Tmp_Result) & """");
                        end if;
                     elsif Verbose_Level > 1 then
                        Put_Verbose (Attribute & ": executing """ & Command
                                     & """ output=""" & Output & """"
                                     & " no match");
                     elsif Verbose_Level > 0 then
                        Put_Verbose
                          (Attribute & ": executing """ & Command
                           & """ output=<use -v -v> no match");
                     end if;
                  end;
               exception
                  when Invalid_Process =>
                     Put_Verbose ("Spawn failed for " & Command);
               end;

            when Value_Directory =>
               declare
                  Search : constant String := Substitute_Special_Dirs
                    (To_String (Node.Directory), Comp, Output_Dir => "");
               begin
                  if Search (Search'First) = '/' then
                     Put_Verbose
                       (Attribute & ": search directories matching " & Search
                        & ", starting from /", 1);
                     Parse_All_Dirs
                       (Processed_Value => Processed_Value,
                        Current_Dir     => "",
                        Path_To_Check   => Search,
                        Regexp          =>
                          Compile (Search (Search'First + 1 .. Search'Last)),
                        Regexp_Str      => Search,
                        Value_If_Match  => To_String (Node.Dir_If_Match),
                        Group           => Node.Directory_Group);
                  else
                     Put_Verbose
                       (Attribute & ": search directories matching " & Search
                        & ", starting from " & To_String (Comp.Path), 1);
                     Parse_All_Dirs
                       (Processed_Value => Processed_Value,
                        Current_Dir     => To_String (Comp.Path),
                        Path_To_Check   => Search,
                        Regexp          => Compile (Search),
                        Regexp_Str      => Search,
                        Value_If_Match  => To_String (Node.Dir_If_Match),
                        Group           => Node.Directory_Group);
                  end if;
                  Put_Verbose ("Done search directories", -1);
               end;
         end case;

         if Value.Must_Match /= Null_Unbounded_String
           and then not Match (Expression => To_String (Value.Must_Match),
                               Data       => To_String (Tmp_Result))
         then
            Put_Verbose ("Ignore compiler since external value "
                         & To_String (Tmp_Result) & " must match "
                         & To_String (Value.Must_Match));
            raise Ignore_Compiler;
         end if;

         case Node.Typ is
            when Value_Directory =>
               --  Only keep those matching
               if Matching /= "" then
                  Cursor := First (Processed_Value);
                  while Has_Element (Cursor) loop
                     Cursor2 := Next (Cursor);

                     if To_Lower (To_String (Element (Cursor).Value)) /=
                       To_Lower (Matching)
                     then
                        Delete (Processed_Value, Cursor);
                     end if;

                     Cursor := Cursor2;
                  end loop;

               end if;

            when Value_Shell | Value_Constant =>
               if Tmp_Result = Null_Unbounded_String then
                  null;

               elsif Split_Into_Words then
                  Get_Words (Words  => To_String (Tmp_Result),
                             Filter => Value.Filter,
                             Map    => Split,
                             Allow_Empty_Elements => False);
                  C := First (Split);
                  while Has_Element (C) loop
                     if Matching = ""
                       or else To_Lower (Element (C)) = To_Lower (Matching)
                     then
                        Append
                          (Processed_Value,
                           External_Value_Item'
                             (Value       => To_Unbounded_String (Element (C)),
                              Extracted_From => Extracted_From));
                     end if;
                     Next (C);
                  end loop;

               elsif Matching = ""
                 or else To_Lower (To_String (Tmp_Result)) =
                   To_Lower (Matching)
               then
                  Append
                    (Processed_Value,
                     External_Value_Item'
                       (Value          => Tmp_Result,
                        Extracted_From => Extracted_From));

               else
                  Put_Verbose
                    ("Compiler ignored since this criteria does not match");
               end if;
         end case;

         Next (Node_Cursor);
      end loop;
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

      if not Allow_Empty_Elements then
         while First <= Words'Last
           and then (Words (First) = ' '
                     or else Words (First) = ',')
         loop
            First := First + 1;
         end loop;
      end if;

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
      Base       : in out Knowledge_Base;
      Name       : String;
      Executable : String;
      Directory  : String;
      Prefix     : String;
      Matching   : Compiler;
      On_Target  : Targets_Set_Id;
      Descr      : Compiler_Description;
      Path_Order : Integer;
      Stop_At_First_Match : Boolean)
   is
      Target    : External_Value_Lists.List;
      Version   : External_Value_Lists.List;
      Languages : External_Value_Lists.List;
      Runtimes  : External_Value_Lists.List;
      Comp      : Compiler;
      C, C2     : External_Value_Lists.Cursor;
   begin

      --  verify that the compiler is indeed a real executable
      --  on Windows and not a cygwin symbolic link

      if On_Windows
        and then not Is_Windows_Executable
          (Directory & Directory_Separator & Executable)
      then
         return;
      end if;

      Comp.Name       := To_Unbounded_String (Name);
      Comp.Path       := To_Unbounded_String
        (Normalize_Pathname (Directory, Case_Sensitive => False));
      Comp.Path_Order := Path_Order;
      Comp.Extra_Tool := Descr.Extra_Tool;
      Comp.Prefix     := To_Unbounded_String (Prefix);
      Comp.Executable := To_Unbounded_String (Executable);

      --  Check the language first, since it is often hard-coded, and often
      --  specified in --config, so that provides a speed up since we won't
      --  have to compute the other attributes

      Get_External_Value
        ("languages",
         Value            => Descr.Languages,
         Comp             => Comp,
         Split_Into_Words => True,
         Matching         => To_String (Matching.Language),
         Processed_Value  => Languages);
      if Is_Empty (Languages) then
         Put_Verbose
           ("Ignore compiler, since no matching language (must match "
            & To_String (Matching.Language) & ")");
         raise Ignore_Compiler;
      end if;

      Get_External_Value
        ("target",
         Value            => Descr.Target,
         Comp             => Comp,
         Split_Into_Words => False,
         Matching         => To_String (Matching.Target),
         Processed_Value  => Target);
      if not Is_Empty (Target) then
         Comp.Target := Element (First (Target)).Value;
         Get_Targets_Set (Base, To_String (Comp.Target), Comp.Targets_Set);
      else
         Put_Verbose ("Target unknown for this compiler");
         Comp.Targets_Set := Unknown_Targets_Set;
      end if;

      if On_Target /= All_Target_Sets
        and then Comp.Targets_Set /= On_Target
      then
         Put_Verbose ("Target for this compiler does not match --target");
         raise Ignore_Compiler;
      end if;

      Get_External_Value
        ("version",
         Value            => Descr.Version,
         Comp             => Comp,
         Split_Into_Words => False,
         Matching         => To_String (Matching.Version),
         Processed_Value  => Version);

      --  If we can't find version, ignore this compiler
      if Is_Empty (Version) then
         Put_Verbose ("Ignore compiler, since couldn't guess its version");
         raise Ignore_Compiler;
      end if;

      Comp.Version := Element (First (Version)).Value;

      if Length (Version) >= 2 then
         Comp.Version2 := Element (Next (First (Version))).Value;
      end if;

      Get_External_Value
        ("runtimes",
         Value            => Descr.Runtimes,
         Comp             => Comp,
         Split_Into_Words => True,
         Matching         => To_String (Matching.Runtime),
         Processed_Value  => Runtimes);

      C := First (Languages);
      while Has_Element (C) loop
         declare
            L : String := To_String (Element (C).Value);
         begin
            To_Mixed (L);
            Comp.Language := To_Unbounded_String (L);

            if Is_Empty (Runtimes) then
               if Descr.Runtimes /= Null_External_Value then
                  Put_Verbose ("No runtime found where one is required for: "
                               & To_String (Comp.Path));
               else
                  Append (Append_To, Comp);
                  if Stop_At_First_Match then
                     return;
                  end if;
               end if;
            else
               C2 := First (Runtimes);
               while Has_Element (C2) loop
                  Comp.Runtime     := Element (C2).Value;
                  Comp.Runtime_Dir := Element (C2).Extracted_From;
                  Append (Append_To, Comp);
                  if Stop_At_First_Match then
                     return;
                  end if;
                  Next (C2);
               end loop;
            end if;
         end;

         Next (C);
      end loop;

   exception
      when Ignore_Compiler =>
         null;
   end For_Each_Language_Runtime;

   ---------------------------
   -- Find_Compilers_In_Dir --
   ---------------------------

   procedure Find_Compilers_In_Dir
     (Append_To  : in out Compiler_Lists.List;
      Base       : in out Knowledge_Base;
      Check_Executable_Regexp : Boolean;
      Directory  : String;
      Matching   : Compiler  := No_Compiler;
      On_Target  : Targets_Set_Id;
      Path_Order : Integer;
      Stop_At_First_Match : Boolean)
   is
      C            : Compiler_Description_Maps.Cursor;
      Search       : Search_Type;
      Dir          : Directory_Entry_Type;
      Initial_Length : Count_Type;
   begin
      --  Since the name of an executable can be a regular expression, we need
      --  to look at all files in the directory to see if they match. This
      --  requires more system calls than if the name was always a simple
      --  string. So we first check which of the two algorithms should be used.

      Put_Verbose ("Search compilers """ & To_String (Matching.Name) & """ in "
                   & Directory & " regexp="
                   & Boolean'Image (Check_Executable_Regexp)
                   & " stop_at_first=" & Boolean'Image (Stop_At_First_Match),
                   1);

      if Check_Executable_Regexp then
         begin
            Start_Search
              (Search    => Search,
               Directory => Directory,
               Pattern   => "");
         exception
            when Ada.Directories.Name_Error =>
               Put_Verbose ("No such directory:" & Directory);
               return;
         end;

         For_All_Files_In_Dir : loop
            begin
               exit For_All_Files_In_Dir when not More_Entries (Search);
               Get_Next_Entry (Search, Dir);
               C := First (Base.Compilers);
               while Has_Element (C) loop
                  if Matching.Name = "" or else Key (C) = Matching.Name then
                     declare
                        Simple  : constant String := Simple_Name (Dir);
                        Matches : Match_Array
                          (0 .. Integer'Max (0, Element (C).Prefix_Index));
                        Matched : Boolean;
                        Prefix  : Unbounded_String;
                     begin
                        if Element (C).Executable_Re /= null then
                           Match (Element (C).Executable_Re.all,
                                  Data       => Simple,
                                  Matches    => Matches);
                           Matched := Matches (0) /= No_Match;
                        else
                           Matched := (To_String (Element (C).Executable) &
                             Exec_Suffix.all) = Simple_Name (Dir);
                        end if;

                        if Matched then
                           Put_Verbose
                             (Key (C) & " is candidate: filename=" & Simple,
                              1);

                           if Element (C).Executable_Re /= null
                             and then Element (C).Prefix_Index >= 0
                             and then Matches (Element (C).Prefix_Index) /=
                             No_Match
                           then
                              Prefix := To_Unbounded_String
                                (Simple (Matches
                                 (Element (C).Prefix_Index).First
                                 ..  Matches (Element (C).Prefix_Index).Last));
                           end if;

                           Initial_Length := Length (Append_To);
                           For_Each_Language_Runtime
                             (Append_To  => Append_To,
                              Base       => Base,
                              Name       => Key (C),
                              Executable => Simple,
                              Directory  => Directory,
                              Matching   => Matching,
                              On_Target  => On_Target,
                              Prefix     => To_String (Prefix),
                              Descr      => Element (C),
                              Path_Order => Path_Order,
                              Stop_At_First_Match => Stop_At_First_Match);

                           Put_Verbose ("", -1);

                           exit For_All_Files_In_Dir when
                             Stop_At_First_Match
                             and then Length (Append_To) > Initial_Length;
                        end if;
                     end;
                  end if;
                  Next (C);
               end loop;
            exception
               when Ada.Directories.Name_Error =>
                  null;
            end;
         end loop For_All_Files_In_Dir;

      else
         --  Do not search all entries in the directory, but check explictly
         --  for the compilers. This results in a lot less system calls, and
         --  thus is faster.

         C := First (Base.Compilers);
         while Has_Element (C) loop
            if Matching.Name = "" or else Key (C) = Matching.Name then
               declare
                  F : constant String := Normalize_Pathname
                    (Name           => To_String (Element (C).Executable),
                     Directory      => Directory,
                     Resolve_Links  => False,
                     Case_Sensitive => Case_Sensitive_Files) & Exec_Suffix.all;
               begin
                  if Ada.Directories.Exists (F) then
                     Put_Verbose ("--------------------------------------");
                     Put_Verbose
                       ("Processing " & Key (C) & " in " & Directory);
                     Initial_Length := Length (Append_To);
                     For_Each_Language_Runtime
                       (Append_To  => Append_To,
                        Base       => Base,
                        Name       => Key (C),
                        Executable => To_String (Element (C).Executable),
                        Prefix     => "",
                        Matching   => Matching,
                        On_Target  => On_Target,
                        Directory  => Directory,
                        Descr      => Element (C),
                        Path_Order => Path_Order,
                        Stop_At_First_Match => Stop_At_First_Match);

                     exit when Stop_At_First_Match
                       and then Length (Append_To) > Initial_Length;
                  end if;
               exception
                  when Ada.Directories.Name_Error =>
                     null;
                  when Ignore_Compiler =>
                     --  Nothing to do, the compiler has not been inserted
                     null;
               end;
            end if;

            Next (C);
         end loop;
      end if;

      Put_Verbose ("", -1);
   end Find_Compilers_In_Dir;

   -----------------------------
   -- Find_Matching_Compilers --
   -----------------------------

   procedure Find_Matching_Compilers
     (Matching   : Compiler;
      On_Target  : Targets_Set_Id;
      Base       : in out Knowledge_Base;
      Compilers  : out Compiler_Lists.List;
      Stop_At_First_Match : Boolean)
   is
      Check_Regexp : Boolean := False;
      C            : Compiler_Description_Maps.Cursor;
   begin
      Clear (Compilers);

      C := First (Base.Compilers);
      while Has_Element (C) loop
         if (Matching.Name = "" or else Key (C) = Matching.Name)
           and then Element (C).Executable_Re /= null
         then
            Check_Regexp := True;
            exit;
         end if;
         Next (C);
      end loop;

      Find_Compilers_In_Dir
        (Append_To  => Compilers,
         Base       => Base,
         Check_Executable_Regexp => Check_Regexp,
         Directory  => To_String (Matching.Path),
         Matching   => Matching,
         On_Target  => On_Target,
         Path_Order => 0,
        Stop_At_First_Match => Stop_At_First_Match);
   end Find_Matching_Compilers;

   ----------------------------
   -- Find_Compilers_In_Path --
   ----------------------------

   procedure Find_Compilers_In_Path
     (Base      : in out Knowledge_Base;
      On_Target : Targets_Set_Id;
      Matching  : Compiler := No_Compiler;
      Compilers : out Compiler_Lists.List;
      Stop_At_First_Match : Boolean)
   is
      Map        : String_Lists.List;
      Path_Order : Positive := 1;
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

               Put_Verbose ("Parsing PATH: " & Path (First .. Last - 1));

               --  Use a hash to make sure we do not parse the same directory
               --  twice. This is both more efficient and avoids duplicates in
               --  the final result list
               if (Case_Sensitive_Files
                   and then not Contains (Map, Path (First .. Last - 1)))
                 or else (not Case_Sensitive_Files
                          and then not Contains
                            (Map, To_Lower (Path (First .. Last - 1))))
               then
                  if Case_Sensitive_Files then
                     Append (Map, To_Lower (Path (First .. Last - 1)));
                  else
                     Append (Map, Path (First .. Last - 1));
                  end if;

                  --  We know that at least GNAT uses a regular expression for
                  --  its <executable> node, so we have to handle regexps
                  Find_Compilers_In_Dir
                    (Append_To               => Compilers,
                     Base                    => Base,
                     Check_Executable_Regexp => True,
                     Directory               => Path (First .. Last - 1),
                     Path_Order              => Path_Order,
                     Matching                => Matching,
                     On_Target               => On_Target,
                     Stop_At_First_Match     => Stop_At_First_Match);
                  exit when Stop_At_First_Match
                    and then Length (Compilers) > 0;
               end if;

               Path_Order := Path_Order + 1;
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
         if (Filter.Name = Null_Unbounded_String
             or else Filter.Name = Comp.Name)
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
                     or else To_Lower (To_String (Filter.Language))
                           = To_Lower (To_String (Comp.Language)))
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
      Selected_Compiler : Compiler;
      M                 : Boolean;
      Comp              : Compiler_Lists.Cursor;
      Project_Name      : String := "Default";

      procedure Gen (C : String_Maps.Cursor);
      --  C is a cursor of the map "Packages"
      --  Generate the chunk of the config file corresponding to the
      --  given package.

      procedure Gen (C : String_Maps.Cursor) is
      begin
         if Has_Element (C) then
            if Key (C) /= "" then
               New_Line (Output);
               Put_Line (Output, "   package " & Key (C) & " is");
            end if;
            Put_Line (Output, To_String (Element (C)));
            if Key (C) /= "" then
               Put_Line (Output, "   end " & Key (C) & ";");
            end if;
         end if;
      end Gen;

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
                  "Code generated by these compilers cannot be linked"
                  & " as far as we know.");
               return;
            end if;

            Merge_Config
              (Packages,
               Selected_Compiler,
               To_String (Element (Config).Config),
               Output_Dir => Containing_Directory (Output_File));
         end if;

         Next (Config);
      end loop;

      if Is_Empty (Packages) then
         Put_Line ("No configuration found");
         raise Generate_Error;
      end if;

      if not Quiet_Output then
         Put_Line ("Creating configuration file: " & Output_File);
      end if;

      Create (Output, Out_File, Output_File);
      Put_Line (Output, "project " & Project_Name & " is");

      Gen (Find (Packages, ""));
      Gen (Find (Packages, "Compiler"));
      Gen (Find (Packages, "Naming"));
      Gen (Find (Packages, "Binder"));
      Gen (Find (Packages, "Linker"));

      Put_Line (Output, "end " & Project_Name & ";");

      Close (Output);

      --  Launch external tools
      Comp := First (Selected);
      while Has_Element (Comp) loop
         if Element (Comp).Extra_Tool /= Null_Unbounded_String
           and then Element (Comp).Extra_Tool /= ""
         then
            declare
               Args : Argument_List_Access := Argument_String_To_List
                 (To_String (Element (Comp).Extra_Tool));
               Tmp  : GNAT.Strings.String_Access;
               Status  : Integer;
            begin
               New_Line;
               Put ("Executing ");
               for A in Args'Range loop
                  Tmp := Args (A);
                  Args (A) := new String'
                    (Substitute_Special_Dirs
                       (Str        => Tmp.all,
                        Comp       => Element (Comp),
                        Output_Dir => Containing_Directory (Output_File)));
                  Put (Args (A).all & " ");
                  GNAT.Strings.Free (Tmp);
               end loop;
               New_Line;

               Status := Spawn
                 (Args (Args'First).all, Args (Args'First + 1 .. Args'Last));
               if Status /= 0 then
                  Put_Line ("Could not execute " & Args (Args'First).all);
                  raise Generate_Error;
               end if;
               GNAT.Strings.Free (Args);
            end;
         end if;

         Next (Comp);
      end loop;

   exception
      when Ada.Directories.Name_Error | Ada.IO_Exceptions.Use_Error =>
         Put_Line ("Could not create the file " & Output_File);
         raise Generate_Error;
   end Generate_Configuration;

   ----------------------
   --  Get_Targets_Set --
   ----------------------

   procedure Get_Targets_Set
     (Base   : in out Knowledge_Base;
      Target : String;
      Id     : out Targets_Set_Id)
   is
      use Targets_Set_Vectors;
      use Target_Lists;
   begin
      if Target = "" then
         Id := All_Target_Sets;
         return;
      end if;

      for I in First_Index (Base.Targets_Sets)
        .. Last_Index (Base.Targets_Sets)
      loop
         declare
            Set : constant Target_Lists.List :=
              Element (Base.Targets_Sets, I);
            C : Target_Lists.Cursor := First (Set);
         begin
            while Has_Element (C) loop
               if GNAT.Regpat.Match (Element (C).all, Target) > 0 then
                  Id := I;
                  return;
               end if;
               Next (C);
            end loop;
         end;
      end loop;

      --  Create a new set.
      declare
         Set : Target_Lists.List;
      begin
         Append (Set, new Pattern_Matcher'(Compile ("^" & Target & "$")));
         Append (Base.Targets_Sets, Set);
         Id := Last_Index (Base.Targets_Sets);
         Put_Verbose ("create a new target set for " & Target);
      end;
   end Get_Targets_Set;
end GprConfig.Knowledge;
