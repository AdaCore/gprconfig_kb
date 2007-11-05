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
with DOM.Core.Nodes;            use DOM.Core, DOM.Core.Nodes;
with DOM.Core.Documents;
with DOM.Readers;               use DOM.Readers;
with Input_Sources.File;        use Input_Sources.File;
with GNAT.Case_Util;            use GNAT.Case_Util;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.Expect;               use GNAT.Expect;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Regpat;               use GNAT.Regpat;
with GNAT.Strings;              use GNAT.Strings;
with GprConfig.Sdefault;        use GprConfig.Sdefault;
with Sax.Readers;               use Sax.Readers;

package body GprConfig.Knowledge is

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

   package CDM renames Compiler_Description_Maps;
   package CFL renames Compiler_Filter_Lists;
   use Compiler_Lists, CFL, Compilers_Filter_Lists;
   use Configuration_Lists, String_Maps;
   use External_Value_Lists, String_Lists;
   use External_Value_Nodes;

   Case_Sensitive_Files : constant Boolean := Directory_Separator = '\';
   On_Windows           : constant Boolean := Directory_Separator = '\';

   Ignore_Compiler : exception;
   --  Raised when the compiler should be ignored

   Indentation_Level : Integer := 0;
   --  Current indentation level for traces

   procedure Parse_Compiler_Description
     (Base        : in out Knowledge_Base;
      Append_To   : in out CDM.Map;
      File        : String;
      Description : Node);
   --  Parse a compiler description described by N

   procedure Parse_Configuration
     (Append_To   : in out Configuration_Lists.List;
      File        : String;
      Description : Node);
   --  Parse a configuration node

   procedure Parse_Targets_Set
     (Append_To   : in out Targets_Set_Vectors.Vector;
      File        : String;
      Description : Node);
   --  Parse a targets set node

   procedure Parse_External_Value
     (Value    : out External_Value;
      File     : String;
      External : Node);
   --  Parse an XML node that describes an external value

   procedure Put_Verbose (Config : Configuration);
   --  Debug put for Config

   function Get_Attribute
     (N : Node; Attribute : String; Default : String) return String;
   --  Return the value of an attribute, or Default if the attribute does not
   --  exist

   function Node_Value_As_String (N : Node) return String;
   --  Return the value of the node, concatenating all Text children

   procedure Foreach_Compiler_In_Dir
     (Iterator   : in out Compiler_Iterator'Class;
      Base       : in out Knowledge_Base;
      Directory  : String;
      From_Extra_Dir : Boolean;
      On_Target  : Targets_Set_Id;
      Path_Order : Integer;
      Continue   : out Boolean);
   --  Find all known compilers in Directory, and call Iterator.Callback as
   --  appropriate.

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

   procedure Foreach_Language_Runtime
     (Iterator   : in out Compiler_Iterator'Class;
      Base       : in out Knowledge_Base;
      Name       : String;
      Executable : String;
      Directory  : String;
      Prefix     : String;
      From_Extra_Dir : Boolean;
      On_Target  : Targets_Set_Id;
      Descr      : Compiler_Description;
      Path_Order : Integer;
      Continue   : out Boolean);
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
      Compilers         : Compiler_Lists.List;
      Matching_Compiler : out Compiler;
      Matched           : out Boolean);
   procedure Match
     (Filter            : Compilers_Filter;
      Compilers         : Compiler_Lists.List;
      Matching_Compiler : out Compiler;
      Matched           : out Boolean);
   procedure Match
     (Filter            : Compiler_Filter;
      Compilers         : Compiler_Lists.List;
      Matching_Compiler : out Compiler;
      Matched           : out Boolean);
   --  Check whether Filter matches (and set Matched to the result).
   --  Matching_Compiler is set if there was a single <compilers> node, and is
   --  to set the first compiler that matched in that node

   function Match
     (Target_Filter : String_Lists.List;
      Negate        : Boolean;
      Compilers     : Compiler_Lists.List) return Boolean;
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

   -------------------
   -- Get_Attribute --
   -------------------

   function Get_Attribute
     (N : Node; Attribute : String; Default : String) return String
   is
      Attr : constant Node := Get_Named_Item (Attributes (N), Attribute);
   begin
      if Attr = null then
         return Default;
      else
         return Node_Value (Attr);
      end if;
   end Get_Attribute;

   --------------------------
   -- Node_Value_As_String --
   --------------------------

   function Node_Value_As_String (N : Node) return String is
      Result : Unbounded_String;
      Child : Node := First_Child (N);
   begin
      while Child /= null loop
         exit when Node_Type (Child) = Element_Node;
         Append (Result, Node_Value (Child));
         Child := Next_Sibling (Child);
      end loop;

      return To_String (Result);
   end Node_Value_As_String;

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

         if Tmp = "" or else Tmp = "./" or else Tmp = ".\" then
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
     (Value         : out External_Value;
      File          : String;
      External      : Node)
   is
      Tmp           : Node := First_Child (External);
      External_Node : External_Value_Node;
      Is_Done       : Boolean := True;
      Static_Value  : constant String := Node_Value_As_String (External);
      Has_Static    : Boolean := False;
   begin
      for S in Static_Value'Range loop
         if Static_Value (S) /= ' '
           and then Static_Value (S) /= ASCII.LF
         then
            Has_Static := True;
            exit;
         end if;
      end loop;

      --  Constant value is not within a nested node.
      if Has_Static then
         External_Node :=
           (Typ        => Value_Constant,
            Value      => To_Unbounded_String (Static_Value));
         Append (Value, External_Node);
         Is_Done := False;
      end if;

      while Tmp /= null loop
         if Node_Type (Tmp) /= Element_Node then
            null;

         elsif Node_Name (Tmp) = "external" then
            if not Is_Done then
               Append (Value, (Typ => Value_Done));
            end if;

            External_Node :=
              (Typ        => Value_Shell,
               Command    => To_Unbounded_String (Node_Value_As_String (Tmp)));
            Append (Value, External_Node);
            Is_Done := False;

         elsif Node_Name (Tmp) = "directory" then
            External_Node :=
              (Typ         => Value_Directory,
               Directory   => To_Unbounded_String (Node_Value_As_String (Tmp)),
               Dir_If_Match => Null_Unbounded_String,
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
            Append (Value, External_Node);
            Is_Done := True;

         elsif Node_Name (Tmp) = "getenv" then
            if not Is_Done then
               Append (Value, (Typ => Value_Done));
            end if;

            declare
               Name : constant String := Get_Attribute (Tmp, "name", "");
            begin
               if Ada.Environment_Variables.Exists (Name) then
                  External_Node :=
                    (Typ        => Value_Constant,
                     Value      => To_Unbounded_String
                     (Ada.Environment_Variables.Value (Name)));
               else
                  Put_Verbose ("warning: environment variable '" & Name
                               & "' is not defined");
                  External_Node :=
                    (Typ        => Value_Constant,
                     Value      => Null_Unbounded_String);
               end if;
            end;
            Append (Value, External_Node);
            Is_Done := False;

         elsif Node_Name (Tmp) = "filter" then
            External_Node :=
              (Typ        => Value_Filter,
               Filter    => To_Unbounded_String (Node_Value_As_String (Tmp)));
            Append (Value, External_Node);
            Is_Done := True;

         elsif Node_Name (Tmp) = "must_match" then
            External_Node :=
              (Typ        => Value_Must_Match,
               Must_Match => To_Unbounded_String (Node_Value_As_String (Tmp)));
            Append (Value, External_Node);
            Is_Done := True;

         elsif Node_Name (Tmp) = "grep" then
            External_Node :=
              (Typ        => Value_Grep,
               Regexp     => To_Unbounded_String
                 (Get_Attribute (Tmp, "regexp", ".*")),
               Group      => Integer'Value
                 (Get_Attribute (Tmp, "group", "0")));
            Append (Value, External_Node);

         else
            Put_Line (Standard_Error, "Invalid XML description for "
                      & Node_Name (External) & " in file " & File);
            Put_Line (Standard_Error, "    Invalid tag: " & Node_Name (Tmp));
            Value := Null_External_Value;
         end if;

         Tmp := Next_Sibling (Tmp);
      end  loop;

      if not Is_Done then
         Append (Value, (Typ => Value_Done));
      end if;

   exception
      when Constraint_Error =>
         Put_Line (Standard_Error, "Invalid group number for "
                   & Node_Name (External)
                   & " in file " & File);
         Value := Null_External_Value;
   end Parse_External_Value;

   --------------------------------
   -- Parse_Compiler_Description --
   --------------------------------

   procedure Parse_Compiler_Description
     (Base        : in out Knowledge_Base;
      Append_To   : in out CDM.Map;
      File        : String;
      Description : Node)
   is
      Compiler : Compiler_Description;
      N        : Node := First_Child (Description);
      Name     : Unbounded_String;
   begin
      while N /= null loop
         if Node_Type (N) /= Element_Node then
            null;

         elsif Node_Name (N) = "executable" then
            declare
               Prefix : constant String := Get_Attribute (N, "prefix", "@@");
               Val    : constant String := Node_Value_As_String (N);
            begin
               Compiler.Executable := To_Unbounded_String (Val);
               Compiler.Prefix_Index := Integer'Value (Prefix);
               Compiler.Executable_Re := new Pattern_Matcher'
                 (Compile (To_String ("^" & Compiler.Executable
                                      & Exec_Suffix.all & "$")));
               Base.Check_Executable_Regexp := True;

            exception
               when Expression_Error =>
                  Put_Line
                    (Standard_Error,
                     "Invalid regular expression found in the configuration"
                     & " files: " & To_String (Compiler.Executable)
                     & " while parsing " & File);
                  raise Invalid_Knowledge_Base;

               when Constraint_Error =>
                  Compiler.Prefix_Index := -1;
                  null;
            end;

         elsif Node_Name (N) = "name" then
            Name := To_Unbounded_String (Node_Value_As_String (N));

         elsif Node_Name (N) = "version" then
            Parse_External_Value
              (Value    => Compiler.Version,
               File     => File,
               External => N);

         elsif Node_Name (N) = "languages" then
            Parse_External_Value
              (Value    => Compiler.Languages,
               File     => File,
               External => N);

         elsif Node_Name (N) = "runtimes" then
            Parse_External_Value
              (Value    => Compiler.Runtimes,
               File     => File,
               External => N);

         elsif Node_Name (N) = "target" then
            Parse_External_Value
              (Value    => Compiler.Target,
               File     => File,
               External => N);

         else
            Put_Line (Standard_Error, "Unknown XML tag in " & File & ": "
                      & Node_Name (N));
            raise Invalid_Knowledge_Base;
         end if;

         N := Next_Sibling (N);
      end loop;

      if Name /= "" then
         CDM.Include (Append_To, To_String (Name), Compiler);
      end if;
   end Parse_Compiler_Description;

   -------------------------
   -- Parse_Configuration --
   -------------------------

   procedure Parse_Configuration
     (Append_To   : in out Configuration_Lists.List;
      File        : String;
      Description : Node)
   is
      Config    : Configuration;
      N         : Node := First_Child (Description);
      N2        : Node;
      Compilers : Compilers_Filter;
      Ignore_Config : Boolean := False;
      Negate    : Boolean;
   begin
      Config.Supported := True;

      while N /= null loop
         if Node_Type (N) /= Element_Node then
            null;

         elsif Node_Name (N) = "compilers" then
            Compilers := No_Compilers_Filter;
            N2 := First_Child (N);
            while N2 /= null loop
               if Node_Type (N2) /= Element_Node then
                  null;

               elsif Node_Name (N2) = "compiler" then
                  Append
                    (Compilers.Compiler,
                     Compiler_Filter'
                       (Name     => TU (Get_Attribute (N2, "name", "")),
                        Version  => TU (Get_Attribute (N2, "version", "")),
                        Runtime  => TU (Get_Attribute (N2, "runtime", "")),
                        Language => TU (Get_Attribute (N2, "language", ""))));
               else
                  Put_Line (Standard_Error, "Unknown XML tag in " & File & ": "
                            & Node_Name (N2));
                  raise Invalid_Knowledge_Base;
               end if;

               N2 := Next_Sibling (N2);
            end loop;

            Compilers.Negate := Boolean'Value
              (Get_Attribute (N, "negate", "False"));
            Append (Config.Compilers_Filters, Compilers);

         elsif Node_Name (N) = "targets" then
            if not Is_Empty (Config.Targets_Filters) then
               Put_Line (Standard_Error,
                         "Can have a single <targets> filter in " & File);
            else
               N2 := First_Child (N);
               while N2 /= null loop
                  if Node_Type (N2) /= Element_Node then
                     null;

                  elsif Node_Name (N2) = "target" then
                     Append (Config.Targets_Filters,
                             Get_Attribute (N2, "name", ""));
                  else
                     Put_Line
                       (Standard_Error, "Unknown XML tag in " & File & ": "
                        & Node_Name (N2));
                     raise Invalid_Knowledge_Base;
                  end if;

                  N2 := Next_Sibling (N2);
               end loop;
               Config.Negate_Targets := Boolean'Value
                 (Get_Attribute (N, "negate", "False"));
            end if;

         elsif Node_Name (N) = "hosts" then
            --  Resolve this filter immediately. This saves memory, since we
            --  don't need to store it in memory if we know it won't apply.
            N2 := First_Child (N);
            Negate := Boolean'Value
              (Get_Attribute (N, "negate", "False"));

            Ignore_Config := not Negate;
            while N2 /= null loop
               if Node_Type (N2) /= Element_Node then
                  null;

               elsif Node_Name (N2) = "host" then
                  if Match
                    (Get_Attribute (N2, "name", ""), Sdefault.Hostname)
                  then
                     Ignore_Config := Negate;
                     exit;
                  end if;

               else
                  Put_Line (Standard_Error, "Unknown XML tag in " & File & ": "
                            & Node_Name (N2));
                  raise Invalid_Knowledge_Base;
               end if;

               N2 := Next_Sibling (N2);
            end loop;

            exit when Ignore_Config;

         elsif Node_Name (N) = "config" then
            if Node_Value_As_String (N) = "" then
               Config.Supported := False;
            else
               Append (Config.Config, Node_Value_As_String (N));
            end if;

         else
            Put_Line (Standard_Error, "Unknown XML tag in " & File & ": "
                      & Node_Name (N));
            raise Invalid_Knowledge_Base;
         end if;

         N := Next_Sibling (N);
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
      Description : Node)
   is
      Set      : Target_Lists.List;
      Pattern  : Pattern_Matcher_Access;
      N        : Node := First_Child (Description);
   begin
      while N /= null loop
         if Node_Type (N) /= Element_Node then
            null;

         elsif Node_Name (N) = "target" then
            declare
               Val    : constant String := Node_Value_As_String (N);
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
                      & Node_Name (N));
            raise Invalid_Knowledge_Base;
         end if;

         N := Next_Sibling (N);
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
      File_Node : Node;
      N         : Node;
      Reader    : Tree_Reader;
      Input     : File_Input;
   begin
      Put_Verbose ("Parsing knowledge base at " & Directory);
      Start_Search
        (Search,
         Directory => Directory,
         Pattern   => "*.xml",
         Filter    => (Ordinary_File => True, others => False));

      while More_Entries (Search) loop
         Get_Next_Entry (Search, File);

         Put_Verbose ("Parsing file " & Full_Name (File));
         Open (Full_Name (File), Input);
         Parse (Reader, Input);
         Close (Input);
         File_Node := DOM.Core.Documents.Get_Element (Get_Tree (Reader));

         if Node_Name (File_Node) = "gprconfig" then
            N := First_Child (File_Node);
            while N /= null loop
               if Node_Type (N) /= Element_Node then
                  null;

               elsif Node_Name (N) = "compiler_description" then
                  Parse_Compiler_Description
                    (Base        => Base,
                     Append_To   => Base.Compilers,
                     File        => Simple_Name (File),
                     Description => N);

               elsif Node_Name (N) = "configuration" then
                  Parse_Configuration
                    (Append_To   => Base.Configurations,
                     File        => Simple_Name (File),
                     Description => N);

               elsif Node_Name (N) = "targetset" then
                  Parse_Targets_Set
                    (Append_To   => Base.Targets_Sets,
                     File        => Simple_Name (File),
                     Description => N);

               else
                  Put_Line (Standard_Error,
                            "Unknown XML tag in " & Simple_Name (File) & ": "
                            & Node_Name (N));
                  raise Invalid_Knowledge_Base;
               end if;

               N := Next_Sibling (N);
            end loop;
         else
            Put_Line (Standard_Error,
                      "Invalid toplevel XML tag in " & Simple_Name (File));
         end if;

         Free (Reader);
      end loop;

      End_Search (Search);

   exception
      when Ada.Directories.Name_Error =>
         Put_Line (Standard_Error, "Directory not found: " & Directory);

      when E : XML_Fatal_Error =>
         Put_Line (Standard_Error, Exception_Message (E));
         raise Invalid_Knowledge_Base;

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
      Node_Cursor    : External_Value_Nodes.Cursor := First (Value);
      Node           : External_Value_Node;
      Cursor, Cursor2 : External_Value_Lists.Cursor;
   begin
      Clear (Processed_Value);

      while Has_Element (Node_Cursor) loop

         while Has_Element (Node_Cursor) loop
            Node := External_Value_Nodes.Element (Node_Cursor);

            case Node.Typ is
               when Value_Constant =>
                  Tmp_Result := To_Unbounded_String
                    (Substitute_Special_Dirs
                     (To_String (Node.Value), Comp, Output_Dir => ""));
                  Put_Verbose
                    (Attribute & ": constant := " & To_String (Tmp_Result));

               when Value_Shell =>
                  Ada.Environment_Variables.Set
                    ("PATH",
                     To_String (Comp.Path) & Path_Separator & Saved_Path);
                  declare
                     Command : constant String := Substitute_Special_Dirs
                       (To_String (Node.Command), Comp, Output_Dir => "");
                  begin
                     Extracted_From := Null_Unbounded_String;
                     Tmp_Result     := Null_Unbounded_String;
                     declare
                        Args   : Argument_List_Access :=
                          Argument_String_To_List (Command);
                        Output : constant String := Get_Command_Output
                          (Command     => Args (Args'First).all,
                           Arguments   => Args (Args'First + 1 .. Args'Last),
                           Input       => "",
                           Status      => Status'Unchecked_Access,
                           Err_To_Out  => True);
                     begin
                        GNAT.Strings.Free (Args);
                        Ada.Environment_Variables.Set ("PATH", Saved_Path);
                        Tmp_Result := To_Unbounded_String (Output);

                        if Verbose_Level > 1 then
                           Put_Verbose (Attribute & ": executing """ & Command
                                        & """ output=""" & Output & """");
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
                          (Attribute & ": search directories matching "
                           & Search & ", starting from /", 1);
                        Parse_All_Dirs
                          (Processed_Value => Processed_Value,
                           Current_Dir     => "",
                           Path_To_Check   => Search,
                           Regexp          =>
                             Compile (Search (Search'First + 1
                                              .. Search'Last)),
                           Regexp_Str      => Search,
                           Value_If_Match  => To_String (Node.Dir_If_Match),
                           Group           => Node.Directory_Group);
                     else
                        Put_Verbose
                          (Attribute & ": search directories matching "
                           & Search & ", starting from "
                           & To_String (Comp.Path), 1);
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

                  --  Only keep those matching
                  if Matching /= "" then
                     Cursor := First (Processed_Value);
                     while Has_Element (Cursor) loop
                        Cursor2 := Next (Cursor);

                        if To_Lower
                          (To_String
                             (External_Value_Lists.Element (Cursor).Value)) /=
                          To_Lower (Matching)
                        then
                           Delete (Processed_Value, Cursor);
                        end if;

                        Cursor := Cursor2;
                     end loop;
                  end if;

               when Value_Grep =>
                  declare
                     Regexp : constant Pattern_Matcher := Compile
                       (To_String (Node.Regexp), Multiple_Lines);
                     Matched : Match_Array (0 .. Node.Group);
                     Tmp_Str : constant String := To_String (Tmp_Result);
                  begin
                     Match (Regexp, Tmp_Str, Matched);
                     if Matched (0) /= No_Match then
                        Tmp_Result := To_Unbounded_String
                          (Tmp_Str (Matched (Node.Group).First ..
                                    Matched (Node.Group).Last));
                        Put_Verbose (Attribute & ": grep matched="""
                                     & To_String (Tmp_Result) & """");
                     else
                        Tmp_Result := Null_Unbounded_String;
                        Put_Verbose (Attribute & ": grep no match");
                     end if;
                  end;

               when Value_Must_Match =>
                  if not Match (Expression => To_String (Node.Must_Match),
                                Data       => To_String (Tmp_Result))
                  then
                     Put_Verbose ("Ignore compiler since external value """
                                  & To_String (Tmp_Result) & """ must match "
                                  & To_String (Node.Must_Match));
                     Tmp_Result := Null_Unbounded_String;
                  end if;
                  exit;
               when Value_Done
                 | Value_Filter =>
                  exit;
            end case;

            Next (Node_Cursor);
         end loop;

         case Node.Typ is
            when Value_Done | Value_Filter | Value_Must_Match =>
               if Tmp_Result = Null_Unbounded_String then
                  null;

               elsif Split_Into_Words then
                  declare
                     Split          : String_Lists.List;
                     C              : String_Lists.Cursor;
                     Filter         : Unbounded_String;
                  begin
                     if Node.Typ = Value_Filter then
                        Filter := Node.Filter;
                     else
                        Filter := Null_Unbounded_String;
                     end if;
                     Get_Words (Words  => To_String (Tmp_Result),
                                Filter => Filter,
                                Map    => Split,
                                Allow_Empty_Elements => False);
                     C := First (Split);
                     while Has_Element (C) loop
                        if Matching = ""
                          or else To_Lower (String_Lists.Element (C)) =
                            To_Lower (Matching)
                        then
                           Append
                             (Processed_Value,
                              External_Value_Item'
                                (Value    => To_Unbounded_String
                                   (String_Lists.Element (C)),
                               Extracted_From => Extracted_From));
                        end if;
                        Next (C);
                     end loop;
                  end;

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
            when others =>
               null;
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

   ------------------------------
   -- Foreach_Language_Runtime --
   ------------------------------

   procedure Foreach_Language_Runtime
     (Iterator   : in out Compiler_Iterator'Class;
      Base       : in out Knowledge_Base;
      Name       : String;
      Executable : String;
      Directory  : String;
      Prefix     : String;
      From_Extra_Dir : Boolean;
      On_Target  : Targets_Set_Id;
      Descr      : Compiler_Description;
      Path_Order : Integer;
      Continue   : out Boolean)
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
         Continue := True;
         return;
      end if;

      Comp.Name       := To_Unbounded_String (Name);
      Comp.Path       := To_Unbounded_String
        (Normalize_Pathname (Directory, Case_Sensitive => False));
      Comp.Path_Order := Path_Order;
      Comp.Prefix     := To_Unbounded_String (Prefix);
      Comp.Executable := To_Unbounded_String (Executable);

      --  Check the target first, for efficiency. If it doesn't match, no need
      --  to compute other attributes.

      Get_External_Value
        ("target",
         Value            => Descr.Target,
         Comp             => Comp,
         Split_Into_Words => False,
         Processed_Value  => Target);
      if not Is_Empty (Target) then
         Comp.Target := External_Value_Lists.Element (First (Target)).Value;
         Get_Targets_Set (Base, To_String (Comp.Target), Comp.Targets_Set);
      else
         Put_Verbose ("Target unknown for this compiler");
         Comp.Targets_Set := Unknown_Targets_Set;
      end if;

      if On_Target /= All_Target_Sets
        and then Comp.Targets_Set /= On_Target
      then
         Put_Verbose ("Target for this compiler does not match --target");
         Continue := True;
         return;
      end if;

      --  Then get the value of the remaining attributes. For most of them, we
      --  must be able to find a valid value, or the compiler is simply ignored

      Get_External_Value
        ("languages",
         Value            => Descr.Languages,
         Comp             => Comp,
         Split_Into_Words => True,
         Processed_Value  => Languages);
      if Is_Empty (Languages) then
         Put_Verbose ("Ignore compiler, since no language could be computed");
         Continue := True;
         return;
      end if;

      Get_External_Value
        ("version",
         Value            => Descr.Version,
         Comp             => Comp,
         Split_Into_Words => False,
         Processed_Value  => Version);

      if Is_Empty (Version) then
         Put_Verbose ("Ignore compiler, since couldn't guess its version");
         Continue := True;
         return;
      end if;

      Comp.Version := External_Value_Lists.Element (First (Version)).Value;

      if Length (Version) >= 2 then
         Comp.Version2 :=
           External_Value_Lists.Element (Next (First (Version))).Value;
      end if;

      Get_External_Value
        ("runtimes",
         Value            => Descr.Runtimes,
         Comp             => Comp,
         Split_Into_Words => True,
         Processed_Value  => Runtimes);

      C := First (Languages);
      while Has_Element (C) loop
         declare
            L : String := To_String (External_Value_Lists.Element (C).Value);
         begin
            To_Mixed (L);
            Comp.Language := To_Unbounded_String (L);

            if Is_Empty (Runtimes) then
               if Descr.Runtimes /= Null_External_Value then
                  Put_Verbose ("No runtime found where one is required for: "
                               & To_String (Comp.Path));
               else
                  Callback
                    (Iterator       => Iterator,
                     Base           => Base,
                     Comp           => Comp,
                     From_Extra_Dir => From_Extra_Dir,
                     Continue       => Continue);
                  if not Continue then
                     return;
                  end if;
               end if;

            else
               C2 := First (Runtimes);
               while Has_Element (C2) loop
                  Comp.Runtime     := External_Value_Lists.Element (C2).Value;
                  Comp.Runtime_Dir :=
                    External_Value_Lists.Element (C2).Extracted_From;
                  Callback
                    (Iterator       => Iterator,
                     Base           => Base,
                     Comp           => Comp,
                     From_Extra_Dir => From_Extra_Dir,
                     Continue       => Continue);
                  if not Continue then
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
   end Foreach_Language_Runtime;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (Comp          : Compiler;
      As_Config_Arg : Boolean;
      Show_Target   : Boolean := False) return String
   is
      function Runtime_Or_Empty return String;
      function Selected return String;
      function Target return String;
      --  Return various aspects of the compiler;

      function Runtime_Or_Empty return String is
      begin
         if Comp.Runtime /= Null_Unbounded_String then
            return " (" & To_String (Comp.Runtime) & " runtime)";
         else
            return "";
         end if;
      end Runtime_Or_Empty;

      function Selected return String is
      begin
         if Comp.Selected then
            return "*";
         else
            return " ";
         end if;
      end Selected;

      function Target return String is
      begin
         if Show_Target then
            return " on " & To_String (Comp.Target);
         else
            return "";
         end if;
      end Target;

   begin
      if As_Config_Arg then
         return To_String (Comp.Language)
           & ',' & To_String (Comp.Version)
           & ',' & To_String (Comp.Runtime)
           & ',' & To_String (Comp.Path)
           & ',' & To_String (Comp.Name);

      else
         return Selected
           & "(" & Comp.Index_In_List & ") "
           & To_String (Comp.Name) & " for "
           & To_String (Comp.Language)
           & " in " & Name_As_Directory (To_String (Comp.Path))
           & Target
           & " version " & To_String (Comp.Version)
           & Runtime_Or_Empty;
      end if;
   end To_String;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (Compilers     : Compiler_Lists.List;
      Selected_Only : Boolean;
      Show_Target   : Boolean := False) return String
   is
      Comp   : Compiler_Lists.Cursor := First (Compilers);
      Result : Unbounded_String;
   begin
      while Has_Element (Comp) loop
         if Compiler_Lists.Element (Comp).Selected
           or else (not Selected_Only
                    and then Compiler_Lists.Element (Comp).Selectable)
         then
            Append
              (Result,
               To_String
                 (Compiler_Lists.Element (Comp), False,
                  Show_Target => Show_Target));
            Append (Result, ASCII.LF);
         end if;
         Next (Comp);
      end loop;
      return To_String (Result);
   end To_String;

   -----------------------------
   -- Foreach_Compiler_In_Dir --
   -----------------------------

   procedure Foreach_Compiler_In_Dir
     (Iterator   : in out Compiler_Iterator'Class;
      Base       : in out Knowledge_Base;
      Directory  : String;
      From_Extra_Dir : Boolean;
      On_Target  : Targets_Set_Id;
      Path_Order : Integer;
      Continue   : out Boolean)
   is
      use CDM;
      C            : CDM.Cursor;
      Search       : Search_Type;
      Dir          : Directory_Entry_Type;
   begin
      --  Since the name of an executable can be a regular expression, we need
      --  to look at all files in the directory to see if they match. This
      --  requires more system calls than if the name was always a simple
      --  string. So we first check which of the two algorithms should be used.

      Continue := True;

      Put_Verbose ("Foreach compiler in "
                   & Directory & " regexp="
                   & Boolean'Image (Base.Check_Executable_Regexp)
                   & " extra_dir=" & From_Extra_Dir'Img,
                   1);

      if Base.Check_Executable_Regexp then
         begin
            Start_Search
              (Search    => Search,
               Directory => Directory,
               Pattern   => "");
         exception
            when Ada.Directories.Name_Error =>
               Put_Verbose ("No such directory:" & Directory, -1);
               Continue := True;
               return;
         end;

         For_All_Files_In_Dir : loop
            begin
               exit For_All_Files_In_Dir when not More_Entries (Search);
               Get_Next_Entry (Search, Dir);
               C := First (Base.Compilers);
               while Has_Element (C) loop
                  declare
                     Simple  : constant String := Simple_Name (Dir);
                     Matches : Match_Array
                       (0 .. Integer'Max (0, CDM.Element (C).Prefix_Index));
                     Matched : Boolean;
                     Prefix  : Unbounded_String;
                  begin
                     if CDM.Element (C).Executable_Re /= null then
                        Match
                          (CDM.Element (C).Executable_Re.all,
                           Data       => Simple,
                           Matches    => Matches);
                        Matched := Matches (0) /= No_Match;
                     else
                        Matched := (To_String (CDM.Element (C).Executable) &
                                    Exec_Suffix.all) = Simple_Name (Dir);
                     end if;

                     if Matched then
                        Put_Verbose
                          (Key (C) & " is candidate: filename=" & Simple,
                           1);

                        if CDM.Element (C).Executable_Re /= null
                          and then CDM.Element (C).Prefix_Index >= 0
                          and then Matches (CDM.Element (C).Prefix_Index) /=
                          No_Match
                        then
                           Prefix := To_Unbounded_String
                             (Simple (Matches
                              (CDM.Element (C).Prefix_Index).First ..
                                Matches (CDM.Element (C).Prefix_Index).Last));
                        end if;

                        Continue := True;
                        Foreach_Language_Runtime
                          (Iterator   => Iterator,
                           Base       => Base,
                           Name       => Key (C),
                           Executable => Simple,
                           Directory  => Directory,
                           On_Target  => On_Target,
                           Prefix     => To_String (Prefix),
                           From_Extra_Dir => From_Extra_Dir,
                           Descr      => CDM.Element (C),
                           Path_Order => Path_Order,
                           Continue   => Continue);

                        Put_Verbose ("", -1);

                        exit For_All_Files_In_Dir when not Continue;
                     end if;
                  end;
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
            declare
               F : constant String := Normalize_Pathname
                 (Name           => To_String (CDM.Element (C).Executable),
                  Directory      => Directory,
                  Resolve_Links  => False,
                  Case_Sensitive => Case_Sensitive_Files) & Exec_Suffix.all;
            begin
               if Ada.Directories.Exists (F) then
                  Put_Verbose ("--------------------------------------");
                  Put_Verbose
                    ("Processing " & Key (C) & " in " & Directory);
                  Foreach_Language_Runtime
                    (Iterator   => Iterator,
                     Base       => Base,
                     Name       => Key (C),
                     Executable => To_String (CDM.Element (C).Executable),
                     Prefix     => "",
                     From_Extra_Dir => From_Extra_Dir,
                     On_Target  => On_Target,
                     Directory  => Directory,
                     Descr      => CDM.Element (C),
                     Path_Order => Path_Order,
                     Continue   => Continue);
                  exit when not Continue;
               end if;
            exception
               when Ada.Directories.Name_Error =>
                  null;
               when Ignore_Compiler =>
                  --  Nothing to do, the compiler has not been inserted
                  null;
            end;

            Next (C);
         end loop;
      end if;

      Put_Verbose ("", -1);
   end Foreach_Compiler_In_Dir;

   ------------------------------
   -- Foreach_Compiler_In_Path --
   ------------------------------

   procedure Foreach_Compiler_In_Path
     (Iterator            : in out Compiler_Iterator;
      Base                : in out Knowledge_Base;
      On_Target           : Targets_Set_Id;
      Extra_Dirs          : String := "")
   is
      Dirs : String_Lists.List;
      Map  : String_Lists.List;

      procedure Process_Path
        (Path : String; Prefix : Character; Prepend_To_List : Boolean);
      --  Add a directory to the list of directories to examine

      procedure Process_Path
        (Path : String; Prefix : Character; Prepend_To_List : Boolean)
      is
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

            declare
               --  Use a hash to make sure we do not parse the same directory
               --  twice. This is both more efficient and avoids duplicates in
               --  the final result list. To handle the case of links (on linux
               --  for instance /usr/bin/X11 points to ".", ie /usr/bin, and
               --  compilers would appear duplicated), we resolve symbolic
               --  links. This call is also set to fold to lower-case when
               --  appropriate

               Normalized : constant String :=
                 Name_As_Directory
                   (Normalize_Pathname
                      (Path (First .. Last - 1),
                       Resolve_Links  => True,
                       Case_Sensitive => False));
            begin
               if not Contains (Map, Normalized) then
                  Append (Map, Normalized);

                  Put_Verbose ("Will examine "
                               & Prefix & " " & Path (First .. Last - 1));

                  if Prepend_To_List then
                     Prepend (Dirs, Prefix & Path (First .. Last - 1));
                  else
                     Append (Dirs, Prefix & Path (First .. Last - 1));
                  end if;
               end if;
            end;
            First := Last + 1;
         end loop;
      end Process_Path;

      Dir        : String_Lists.Cursor;
      Path_Order : Positive := 1;
      Continue   : Boolean;
   begin
      --  Preprocess the list of directories that will be searched. When a
      --  directory appears both in Extra_Dirs and in Path, we prepend it to
      --  the PATH for optimization purposes: no need to look in all the PATH
      --  if the compiler(s) will match in that directory. However, this has
      --  the result that a command line with --config that specifies a path
      --  and one that doesn't might find the second compiler in the same
      --  path even if it is not the first one on the PATH. That's minor, and
      --  a workaround is for the user to specify path for all --config args.
      --
      --  We will also need to know later whether the directory comes from
      --  PATH or extra_dirs. If a directory appears in both, it is said to
      --  come from PATH, so that all its compilers are taken into account.
      --  As a special convention, the first character of the directory name is
      --  set to 'E' if the dir comes from extra_dirs, or 'P' if it comes from
      --  PATH.

      if Ada.Environment_Variables.Exists ("PATH") then
         Process_Path (Ada.Environment_Variables.Value ("PATH"), 'P', False);
      end if;

      if Extra_Dirs /= "" then
         Process_Path (Extra_Dirs, 'E', Prepend_To_List => True);
      end if;

      Dir := First (Dirs);
      while Has_Element (Dir) loop
         declare
            P : constant String := String_Lists.Element (Dir);
         begin
            Foreach_Compiler_In_Dir
              (Iterator                => Iterator,
               Base                    => Base,
               Directory               => P (P'First + 1 .. P'Last),
               From_Extra_Dir          => P (P'First) = 'E',
               Path_Order              => Path_Order,
               On_Target               => On_Target,
               Continue                => Continue);
            exit when not Continue;
         end;

         Path_Order := Path_Order + 1;
         Next (Dir);
      end loop;
   end Foreach_Compiler_In_Path;

   --------------------------
   -- Known_Compiler_Names --
   --------------------------

   procedure Known_Compiler_Names
     (Base : Knowledge_Base;
      List : out Ada.Strings.Unbounded.Unbounded_String)
   is
      use CDM;
      C : CDM.Cursor := First (Base.Compilers);
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
      Compilers         : Compiler_Lists.List;
      Matching_Compiler : out Compiler;
      Matched           : out Boolean)
   is
      C : CFL.Cursor := First (Filter.Compiler);
      M : Boolean;
   begin
      while Has_Element (C) loop
         Match (CFL.Element (C), Compilers, Matching_Compiler, M);
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
      Compilers         : Compiler_Lists.List;
      Matching_Compiler : out Compiler;
      Matched           : out Boolean)
   is
      C    : Compiler_Lists.Cursor := First (Compilers);
      Comp : Compiler;
   begin
      while Has_Element (C) loop
         Comp := Compiler_Lists.Element (C);
         if Comp.Selected
           and then (Filter.Name = Null_Unbounded_String
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
      Compilers         : Compiler_Lists.List;
      Matching_Compiler : out Compiler;
      Matched           : out Boolean)
   is
      C : Compilers_Filter_Lists.Cursor := First (Filter);
      M : Boolean;
   begin
      while Has_Element (C) loop
         Match (Compilers_Filter_Lists.Element (C),
                Compilers, Matching_Compiler, M);
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
      Compilers     : Compiler_Lists.List) return Boolean
   is
      Target : String_Lists.Cursor := First (Target_Filter);
      Comp   : Compiler_Lists.Cursor;
   begin
      if Is_Empty (Target_Filter) then
         return True;

      else
         while Has_Element (Target) loop
            Comp := First (Compilers);
            while Has_Element (Comp) loop
               if Compiler_Lists.Element (Comp).Selected
                 and then Match
                 (Compile (String_Lists.Element (Target), Case_Insensitive),
                  To_String (Compiler_Lists.Element (Comp).Target))
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
                  String_Maps.Element (C) & ASCII.LF & Prefix
                  & Replaced);
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

   -----------------
   -- Put_Verbose --
   -----------------

   procedure Put_Verbose (Config : Configuration) is
      C : Compilers_Filter_Lists.Cursor := First (Config.Compilers_Filters);
      Comp_Filter : Compilers_Filter;
      Comp : Compiler_Filter_Lists.Cursor;
      Filter : Compiler_Filter;
   begin
      while Has_Element (C) loop
         Comp_Filter := Compilers_Filter_Lists.Element (C);
         Put_Verbose
           ("<compilers negate='" & Comp_Filter.Negate'Img & "'>", 1);
         Comp := First (Comp_Filter.Compiler);
         while Has_Element (Comp) loop
            Filter := Compiler_Filter_Lists.Element (Comp);
            Put_Verbose
              ("<compiler name='"
               & To_String (Filter.Name) & "' version='"
               & To_String (Filter.Version) & "' runtime='"
               & To_String (Filter.Runtime) & "' language='"
               & To_String (Filter.Language) & "' />");
            Next (Comp);
         end loop;
         Put_Verbose ("</compilers>", -1);
         Next (C);
      end loop;

      Put_Verbose ("<config supported='" & Config.Supported'Img & "' />");
   end Put_Verbose;

   -------------------------
   -- Is_Supported_Config --
   -------------------------

   function Is_Supported_Config
     (Base      : Knowledge_Base;
      Compilers : Compiler_Lists.List) return Boolean
   is
      Config : Configuration_Lists.Cursor := First (Base.Configurations);
      M      : Boolean;
      Matching_Compiler : Compiler;
   begin
      while Has_Element (Config) loop
         Match (Configuration_Lists.Element (Config).Compilers_Filters,
                Compilers, Matching_Compiler, M);
         if M and then Match
           (Configuration_Lists.Element (Config).Targets_Filters,
            Configuration_Lists.Element (Config).Negate_Targets,
            Compilers)
         then
            if not Configuration_Lists.Element (Config).Supported then
               if Verbose_Level > 0 then
                  Put_Verbose
                    ("Selected compilers are not compatible, because of:");
                  Put_Verbose (Configuration_Lists.Element (Config));
               end if;
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
      Compilers   : Compiler_Lists.List;
      Output_File : String)
   is
      Config   : Configuration_Lists.Cursor := First (Base.Configurations);
      Output            : File_Type;
      Packages          : String_Maps.Map;
      Selected_Compiler : Compiler;
      M                 : Boolean;
      Project_Name      : String := "Default";

      procedure Gen (C : String_Maps.Cursor);
      --  C is a cursor of the map "Packages"
      --  Generate the chunk of the config file corresponding to the
      --  given package.

      procedure Gen_And_Remove (Name : String);
      --  Generate the chunk of the config file corresponding to the
      --  package name and remove it from the map.

      procedure Gen (C : String_Maps.Cursor) is
      begin
         if Key (C) /= "" then
            New_Line (Output);
            Put_Line (Output, "   package " & Key (C) & " is");
         end if;
         Put_Line (Output, To_String (String_Maps.Element (C)));
         if Key (C) /= "" then
            Put_Line (Output, "   end " & Key (C) & ";");
         end if;
      end Gen;

      procedure Gen_And_Remove (Name : String)
      is
         C : String_Maps.Cursor := Find (Packages, Name);
      begin
         if Has_Element (C) then
            Gen (C);
            Delete (Packages, C);
         end if;
      end Gen_And_Remove;

   begin
      To_Mixed (Project_Name);

      while Has_Element (Config) loop
         Match (Configuration_Lists.Element (Config).Compilers_Filters,
                Compilers, Selected_Compiler, M);

         if M
           and then Match
             (Configuration_Lists.Element (Config).Targets_Filters,
              Configuration_Lists.Element (Config).Negate_Targets,
              Compilers)
         then
            if not Configuration_Lists.Element (Config).Supported then
               Put_Line
                 (Standard_Error,
                  "Code generated by these compilers cannot be linked"
                  & " as far as we know.");
               return;
            end if;

            Merge_Config
              (Packages,
               Selected_Compiler,
               To_String (Configuration_Lists.Element (Config).Config),
               Output_Dir => Containing_Directory (Output_File));
         end if;

         Next (Config);
      end loop;

      if Is_Empty (Packages) then
         Put_Line ("No valid configuration found");
         raise Generate_Error;
      end if;

      if not Quiet_Output then
         Put_Line ("Creating configuration file: " & Output_File);
      end if;

      Create (Output, Out_File, Output_File);

      Put_Line (Output,
                "--  This gpr configuration file was generated by gprconfig");
      Put_Line (Output, "--  using this command line:");
      declare
         Len : Natural := 0;
      begin
         Put (Output, "--  " & Command_Name);
         Len := 4 + Command_Name'Length;
         for I in 1 .. Argument_Count loop
            if Len + Argument (I)'Length > 78 then
               New_Line (Output);
               Put (Output, "--   ");
               Len := 5;
            end if;
            Put (Output, ' ');
            Put (Output, Argument (I));
            Len := Len + 1 + Argument (I)'Length;
         end loop;
         New_Line (Output);
         New_Line (Output);
      end;

      Put_Line (Output, "project " & Project_Name & " is");

      --  Generate known packages in order.  This takes care of possible
      --  dependencies.
      Gen_And_Remove ("");
      Gen_And_Remove ("Builder");
      Gen_And_Remove ("Compiler");
      Gen_And_Remove ("Naming");
      Gen_And_Remove ("Binder");
      Gen_And_Remove ("Linker");

      --  Generate remaining packages.
      Iterate (Packages, Gen'Access);

      Put_Line (Output, "end " & Project_Name & ";");

      Close (Output);

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
              Targets_Set_Vectors.Element (Base.Targets_Sets, I);
            C : Target_Lists.Cursor := First (Set);
         begin
            while Has_Element (C) loop
               if GNAT.Regpat.Match
                 (Target_Lists.Element (C).all, Target) > 0
               then
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
         Put_Verbose ("create a new target set for " & Target);
         Append (Set, new Pattern_Matcher'(Compile ("^" & Target & "$")));
         Append (Base.Targets_Sets, Set);
         Id := Last_Index (Base.Targets_Sets);
      end;
   end Get_Targets_Set;
end GprConfig.Knowledge;
