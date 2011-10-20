------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             G P R C O N F I G                            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                   Copyright (C) 2006-2011, AdaCore                       --
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

with Ada.Unchecked_Deallocation;
with Ada.Characters.Handling;   use Ada.Characters.Handling;
with Ada.Command_Line;          use Ada.Command_Line;
with Ada.Containers;            use Ada.Containers;
with Ada.Directories;           use Ada.Directories;
with Ada.Environment_Variables; use Ada.Environment_Variables;
with Ada.Exceptions;            use Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Strings.Fixed;         use Ada.Strings.Fixed;
with Ada.Strings.Hash_Case_Insensitive;
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
with Makeutl;                   use Makeutl;
with Namet;                     use Namet;
with Opt;
with Prj;                       use Prj;
with Sax.Readers;               use Sax.Readers;

package body GprConfig.Knowledge is

   package String_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (String, Unbounded_String, Ada.Strings.Hash_Case_Insensitive, "=");

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Pattern_Matcher, Pattern_Matcher_Access);

   type External_Value_Item is record
      Value          : Name_Id;
      Extracted_From : Name_Id;
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

   function Get_Variable_Value
     (Comp : Compiler;
      Name : String) return String;
   --  Return the value of a predefined or user-defined variable.
   --  If the variable is not defined a warning is emitted and an empty
   --  string is returned.

   function Get_Name_String_Or_Null (Name : Name_Id) return String;
   --  Return the string stored in Name (or the empty string if Name is
   --  No_Name)

   procedure Put_Verbose (Config : Configuration);
   --  Debug put for Config

   function Get_Attribute
     (N : Node; Attribute : String; Default : String) return String;
   --  Return the value of an attribute, or Default if the attribute does not
   --  exist

   function Is_Supported_Config
     (Base      : Knowledge_Base;
      Compilers : Compiler_Lists.List) return Boolean;
   --  Whether we know how to link code compiled with all the selected
   --  compilers.

   function Is_Language_With_No_Compiler
     (Base        : Knowledge_Base;
      Language_LC : String) return Boolean;
   --  Given a language name (lower case), returns True if that language is
   --  known to require no compiler

   function Node_Value_As_String (N : Node) return String;
   --  Return the value of the node, concatenating all Text children

   function Ends_With (Str, Suffix : String) return Boolean;
   --  Whether the string ends with Suffix. Always True if Suffix is the empty
   --  string.

   procedure Foreach_Compiler_In_Dir
     (Iterator       : in out Compiler_Iterator'Class;
      Base           : in out Knowledge_Base;
      Directory      : String;
      From_Extra_Dir : Boolean;
      On_Target      : Targets_Set_Id;
      Path_Order     : Integer;
      Continue       : out Boolean);
   --  Find all known compilers in Directory, and call Iterator.Callback as
   --  appropriate.

   procedure Get_Words
     (Words                : String;
      Filter               : Namet.Name_Id;
      Separator1           : Character;
      Separator2           : Character;
      Map                  : out String_Lists.List;
      Allow_Empty_Elements : Boolean);
   --  Return the list of words in Words. Splitting is done on special
   --  characters, so as to be compatible with a list of languages or a list of
   --  runtimes
   --  If Allow_Empty_Elements is false, then empty strings are not stored in
   --  the list.

   function Name_As_Directory (Dir : String) return String;
   --  Ensure that Dir ends with a directory separator

   function Get_String (Str : String) return Namet.Name_Id;
   function Get_String_Or_No_Name (Str : String) return Namet.Name_Id;
   --  Same as Name_Find, but does not require the user to modify
   --  Name_Buffer manually.
   --  The second version returns No_Name is the string is empty

   procedure Get_External_Value
     (Attribute        : String;
      Value            : External_Value;
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
   --  Comparisong with Matching is case-insensitive (this is needed for
   --  languages, does not matter for versions, is not used for targets)
   --
   --  This is only for use within a <compiler_description> context.

   procedure Foreach_Language_Runtime
     (Iterator       : in out Compiler_Iterator'Class;
      Base           : in out Knowledge_Base;
      Name           : Name_Id;
      Executable     : Name_Id;
      Directory      : String;
      Prefix         : Name_Id;
      From_Extra_Dir : Boolean;
      On_Target      : Targets_Set_Id;
      Descr          : Compiler_Description;
      Path_Order     : Integer;
      Continue       : out Boolean);
   --  For each language/runtime parsed in Languages/Runtimes, create a new
   --  compiler in the list, if it matches Matching.
   --  If Stop_At_First_Match is true, then only the first matching compiler is
   --  returned, which provides a significant speedup in some cases

   function Is_Windows_Executable (Filename : String) return Boolean;
   --  Verify that a given filename is indeed an executable

   procedure Parse_All_Dirs
     (Processed_Value : out External_Value_Lists.List;
      Current_Dir     : String;
      Path_To_Check   : String;
      Regexp          : Pattern_Matcher;
      Regexp_Str      : String;
      Value_If_Match  : Name_Id;
      Group           : Integer;
      Group_Match     : String := "";
      Group_Count     : Natural := 0);
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

   generic
      with function Callback (Var_Name, Index : String) return String;
   function Substitute_Variables (Str : String) return String;
   --  Substitute variables in Str (their value is computed through Callback).

   function Substitute_Variables_In_Compiler_Description
     (Str : String; Comp : Compiler) return String;
   function Substitute_Variables_In_Configuration
     (Base  : Knowledge_Base;
      Str   : String;
      Comps : Compiler_Lists.List) return String;
   --  Substitute the special "$..." names.
   --  Depending on the XML nodes we are in (specified by the context) the list
   --  of variables might be different.

   procedure Match
     (Filter            : Compilers_Filter_Lists.List;
      Compilers         : Compiler_Lists.List;
      Matching_Compiler : out Compiler_Access;
      Matched           : out Boolean);
   procedure Match
     (Filter            : Compilers_Filter;
      Compilers         : Compiler_Lists.List;
      Matching_Compiler : out Compiler_Access;
      Matched           : out Boolean);
   procedure Match
     (Filter            : Compiler_Filter;
      Compilers         : Compiler_Lists.List;
      Matching_Compiler : out Compiler_Access;
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
     (Base      : Knowledge_Base;
      Packages  : in out String_Maps.Map;
      Compilers : Compiler_Lists.List;
      Config    : String);
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
      Child  : Node := First_Child (N);
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

   ---------------
   -- Ends_With --
   ---------------

   function Ends_With (Str, Suffix : String) return Boolean is
   begin
      return Suffix = ""
        or else
          (Str'Length >= Suffix'Length
           and then Str (Str'Last - Suffix'Length + 1 .. Str'Last) = Suffix);
   end Ends_With;

   ---------------------------
   -- Is_Windows_Executable --
   ---------------------------

   function Is_Windows_Executable (Filename : String) return Boolean is

      type Byte is mod 256;
      for Byte'Size use 8;
      for Byte'Alignment use 1;
      type Bytes is array (Positive range <>) of Byte;

      Windows_Pattern : constant Bytes := (77, 90, 144, 0);

      Fd     : constant File_Descriptor := Open_Read (Filename, Binary);
      B      : Bytes (1 .. 4);
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
      if Current_Verbosity /= Default then
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
      if Dir = ""
        or else Dir (Dir'Last) = Directory_Separator
        or else Dir (Dir'Last) = '/'
      then
         return Dir;
      else
         return Dir & Directory_Separator;
      end if;
   end Name_As_Directory;

   ----------------------------------
   -- Is_Language_With_No_Compiler --
   ----------------------------------

   function Is_Language_With_No_Compiler
     (Base        : Knowledge_Base;
      Language_LC : String) return Boolean
   is
      C : String_Lists.Cursor := First (Base.No_Compilers);
   begin
      while Has_Element (C) loop
         if String_Lists.Element (C) = Language_LC then
            return True;
         end if;
         Next (C);
      end loop;
      return False;
   end Is_Language_With_No_Compiler;

   --------------------------
   -- Parse_Knowledge_Base --
   --------------------------

   procedure Parse_Knowledge_Base
     (Base                : in out Knowledge_Base;
      Directory           : String;
      Parse_Compiler_Info : Boolean := True)
   is

      procedure Parse_Compiler_Description
        (Base        : in out Knowledge_Base;
         File        : String;
         Description : Node);
      --  Parse a compiler description described by N. Appends the result to
      --  Base.Compilers or Base.No_Compilers

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

      --------------------------------
      -- Parse_Compiler_Description --
      --------------------------------

      procedure Parse_Compiler_Description
        (Base        : in out Knowledge_Base;
         File        : String;
         Description : Node)
      is
         procedure Parse_External_Value
           (Value    : out External_Value;
            File     : String;
            External : Node);
         --  Parse an XML node that describes an external value

         --------------------------
         -- Parse_External_Value --
         --------------------------

         procedure Parse_External_Value
           (Value    : out External_Value;
            File     : String;
            External : Node)
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
                  Value      => Get_String (Static_Value));
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
                     Command    => Get_String (Node_Value_As_String (Tmp)));
                  Append (Value, External_Node);
                  Is_Done := False;

               elsif Node_Name (Tmp) = "directory" then
                  External_Node :=
                    (Typ             => Value_Directory,
                     Directory       => Get_String
                       (Node_Value_As_String (Tmp)),
                     Dir_If_Match    => No_Name,
                     Directory_Group => 0);
                  begin
                     External_Node.Directory_Group := Integer'Value
                       (Get_Attribute (Tmp, "group", "0"));
                  exception
                     when Constraint_Error =>
                        External_Node.Directory_Group := -1;
                        External_Node.Dir_If_Match :=
                          Get_String (Get_Attribute (Tmp, "group", "0"));
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
                           Value      => Get_String
                             (Ada.Environment_Variables.Value (Name)));
                     else
                        Put_Verbose ("warning: environment variable '" & Name
                                     & "' is not defined");
                        External_Node :=
                          (Typ        => Value_Constant,
                           Value      => No_Name);
                     end if;
                  end;
                  Append (Value, External_Node);
                  Is_Done := False;

               elsif Node_Name (Tmp) = "filter" then
                  External_Node :=
                    (Typ        => Value_Filter,
                     Filter     => Get_String (Node_Value_As_String (Tmp)));
                  Append (Value, External_Node);
                  Is_Done := True;

               elsif Node_Name (Tmp) = "must_match" then
                  External_Node :=
                    (Typ        => Value_Must_Match,
                     Must_Match => Get_String (Node_Value_As_String (Tmp)));
                  Append (Value, External_Node);
                  Is_Done := True;

               elsif Node_Name (Tmp) = "grep" then
                  External_Node :=
                    (Typ        => Value_Grep,
                     Regexp_Re  => new Pattern_Matcher'
                       (Compile (Get_Attribute (Tmp, "regexp", ".*"),
                        Multiple_Lines)),
                     Group      => Integer'Value
                       (Get_Attribute (Tmp, "group", "0")));
                  Append (Value, External_Node);

               else
                  Put_Line (Standard_Error, "Invalid XML description for "
                            & Node_Name (External) & " in file " & File);
                  Put_Line
                    (Standard_Error, "    Invalid tag: " & Node_Name (Tmp));
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

         Compiler : Compiler_Description;
         N        : Node := First_Child (Description);
         Lang     : External_Value_Lists.List;
         C        : External_Value_Lists.Cursor;
      begin
         while N /= null loop
            if Node_Type (N) /= Element_Node then
               null;

            elsif Node_Name (N) = "executable" then
               declare
                  Prefix : constant String :=
                             Get_Attribute (N, "prefix", "@@");
                  Val    : constant String := Node_Value_As_String (N);
               begin
                  if Val = "" then
                     --  A special language that requires no executable. We do
                     --  not store it in the list of compilers, since these
                     --  should not be detected on the PATH anyway.

                     Compiler.Executable := No_Name;

                  else
                     Compiler.Executable := Get_String (Val);

                     begin
                        Compiler.Prefix_Index := Integer'Value (Prefix);
                     exception
                        when Constraint_Error =>
                           Compiler.Prefix_Index := -1;
                     end;

                     if not Ends_With (Val, Exec_Suffix.all) then
                        Compiler.Executable_Re := new Pattern_Matcher'
                          (Compile ("^" & Val & Exec_Suffix.all & "$"));
                     else
                        Compiler.Executable_Re := new Pattern_Matcher'
                          (Compile ("^" & Val & "$"));
                     end if;
                     Base.Check_Executable_Regexp := True;
                  end if;

               exception
                  when Expression_Error =>
                     Put_Line
                       (Standard_Error,
                        "Invalid regular expression found in the configuration"
                        & " files: " & Val
                        & " while parsing " & File);
                     Unchecked_Free (Compiler.Executable_Re);
               end;

            elsif Node_Name (N) = "name" then
               Compiler.Name := Get_String (Node_Value_As_String (N));

            elsif Node_Name (N) = "version" then
               Parse_External_Value
                 (Value    => Compiler.Version,
                  File     => File,
                  External => N);

            elsif Node_Name (N) = "variable" then
               declare
                  Name   : constant String := Get_Attribute (N, "name", "@@");
               begin
                  Append (Compiler.Variables, (Typ      => Value_Variable,
                                               Var_Name => Get_String (Name)));
                  Parse_External_Value
                    (Value    => Compiler.Variables,
                     File     => File,
                     External => N);
               end;

            elsif Node_Name (N) = "languages" then
               Parse_External_Value
                 (Value    => Compiler.Languages,
                  File     => File,
                  External => N);

            elsif Node_Name (N) = "runtimes" then
               declare
                  Defaults : constant String :=
                               Get_Attribute (N, "default", "");
               begin
                  if Defaults /= "" then
                     Get_Words (Defaults, No_Name, ' ', ',',
                                Compiler.Default_Runtimes, False);
                  end if;
                  Parse_External_Value
                    (Value    => Compiler.Runtimes,
                     File     => File,
                     External => N);
               end;

            elsif Node_Name (N) = "target" then
               Parse_External_Value
                 (Value    => Compiler.Target,
                  File     => File,
                  External => N);

            else
               Put_Line
                 (Standard_Error, "Unknown XML tag in " & File & ": "
                  & Node_Name (N));
               raise Invalid_Knowledge_Base;
            end if;

            N := Next_Sibling (N);
         end loop;

         if Compiler.Executable = No_Name then
            Get_External_Value
              (Attribute        => "languages",
               Value            => Compiler.Languages,
               Comp             => No_Compiler,
               Split_Into_Words => True,
               Processed_Value  => Lang);
            C := First (Lang);
            while Has_Element (C) loop
               String_Lists.Append
                 (Base.No_Compilers,
                  To_Lower
                    (Get_Name_String
                       (External_Value_Lists.Element (C).Value)));
               Next (C);
            end loop;

         elsif Compiler.Name /= No_Name then
            CDM.Include (Base.Compilers, Compiler.Name, Compiler);
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
         Config        : Configuration;
         Chunk         : Unbounded_String;
         N             : Node := First_Child (Description);
         N2            : Node;
         Compilers     : Compilers_Filter;
         Ignore_Config : Boolean := False;
         Negate        : Boolean;
         Filter        : Compiler_Filter;
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
                     declare
                        Version : constant String :=
                                    Get_Attribute (N2, "version", "");
                        Runtime : constant String :=
                                    Get_Attribute (N2, "runtime", "");
                     begin
                        Filter := Compiler_Filter'
                          (Name          => Get_String_Or_No_Name
                             (Get_Attribute (N2, "name", "")),
                           Version       => Get_String_Or_No_Name (Version),
                           Version_Re    => null,
                           Runtime       => Get_String_Or_No_Name (Runtime),
                           Runtime_Re    => null,
                           Language_LC   => Get_String_Or_No_Name
                             (To_Lower (Get_Attribute (N2, "language", ""))));

                        if Version /= "" then
                           Filter.Version_Re := new Pattern_Matcher'
                             (Compile (Version, Case_Insensitive));
                        end if;

                        if Runtime /= "" then
                           Filter.Runtime_Re := new Pattern_Matcher'
                             (Compile (Runtime, Case_Insensitive));
                        end if;
                     end;

                     Append (Compilers.Compiler, Filter);

                  else
                     Put_Line
                       (Standard_Error, "Unknown XML tag in " & File & ": "
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
                     Put_Line
                       (Standard_Error, "Unknown XML tag in " & File & ": "
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
                  Append (Chunk, Node_Value_As_String (N));
               end if;

            else
               Put_Line (Standard_Error, "Unknown XML tag in " & File & ": "
                         & Node_Name (N));
               raise Invalid_Knowledge_Base;
            end if;

            N := Next_Sibling (N);
         end loop;

         if not Ignore_Config then
            Config.Config := Get_String (To_String (Chunk));
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
         Name    : Name_Id := No_Name;
         Set     : Target_Lists.List;
         Pattern : Pattern_Matcher_Access;
         N       : Node := First_Child (Description);
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

                  if Name = No_Name then
                     Name := Get_String (Val);
                  end if;

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
            Targets_Set_Vectors.Append (Append_To, (Name, Set));
         end if;
      end Parse_Targets_Set;

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
                  if Parse_Compiler_Info then
                     Parse_Compiler_Description
                       (Base        => Base,
                        File        => Simple_Name (File),
                        Description => N);
                  end if;

               elsif Node_Name (N) = "configuration" then
                  if Parse_Compiler_Info then
                     Parse_Configuration
                       (Append_To   => Base.Configurations,
                        File        => Simple_Name (File),
                        Description => N);
                  end if;

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
         Put_Verbose ("Directory not found: " & Directory);

      when Invalid_Knowledge_Base =>
         raise;

      when E : XML_Fatal_Error =>
         Put_Line (Standard_Error, Exception_Message (E));
         raise Invalid_Knowledge_Base;

      when E : others =>
         Put_Line
           (Standard_Error,
            "Unexpected exception while parsing knowledge base: "
            & Exception_Information (E));
         raise Invalid_Knowledge_Base;
   end Parse_Knowledge_Base;

   ------------------------
   -- Get_Variable_Value --
   ------------------------

   function Get_Variable_Value
     (Comp : Compiler;
      Name : String) return String
   is
      N : constant Name_Id := Get_String (Name);
   begin
      if Variables_Maps.Contains (Comp.Variables, N) then
         return Get_Name_String (Variables_Maps.Element (Comp.Variables, N));
      elsif Name = "HOST" then
         return Sdefault.Hostname;
      elsif Name = "TARGET" then
         return Get_Name_String (Comp.Target);
      elsif Name = "RUNTIME_DIR" then
         return Name_As_Directory (Get_Name_String (Comp.Runtime_Dir));
      elsif Name = "EXEC" then
         return Get_Name_String_Or_Null (Comp.Executable);
      elsif Name = "VERSION" then
         return Get_Name_String_Or_Null (Comp.Version);
      elsif Name = "LANGUAGE" then
         return Get_Name_String_Or_Null (Comp.Language_LC);
      elsif Name = "RUNTIME" then
         return Get_Name_String_Or_Null (Comp.Runtime);
      elsif Name = "PREFIX" then
         return Get_Name_String_Or_Null (Comp.Prefix);
      elsif Name = "PATH" then
         return Get_Name_String (Comp.Path);
      elsif Name = "GPRCONFIG_PREFIX" then
         return Executable_Prefix_Path;
      end if;

      raise Invalid_Knowledge_Base
        with "variable '" & Name & "' is not defined";
   end Get_Variable_Value;

   --------------------------
   -- Substitute_Variables --
   --------------------------

   function Substitute_Variables (Str : String) return String is
      Str_Len                   : constant Natural := Str'Last;
      Pos                       : Natural := Str'First;
      Last                      : Natural := Pos;
      Result                    : Unbounded_String;
      Word_Start, Word_End, Tmp : Natural;
      Has_Index                 : Boolean;
   begin
      while Pos < Str_Len loop
         if Str (Pos) = '$' and then Str (Pos + 1) = '$' then
            Append (Result, Str (Last .. Pos - 1));
            Append (Result, "$");
            Last := Pos + 2;
            Pos  := Last;

         elsif Str (Pos) = '$' then
            if Str (Pos + 1)  = '{' then
               Word_Start := Pos + 2;
               Tmp := Pos + 2;
               while Tmp <= Str_Len and then Str (Tmp) /= '}' loop
                  Tmp := Tmp + 1;
               end loop;
               Tmp := Tmp + 1;
               Word_End := Tmp - 2;
            else
               Word_Start := Pos + 1;
               Tmp := Pos + 1;
               while Tmp <= Str_Len
                 and then (Is_Alphanumeric (Str (Tmp)) or else Str (Tmp) = '_')
               loop
                  Tmp := Tmp + 1;
               end loop;
               Word_End := Tmp - 1;
            end if;

            Append (Result, Str (Last ..  Pos - 1));

            Has_Index := False;

            for W in Word_Start .. Word_End loop
               if Str (W) = '(' then
                  Has_Index := True;

                  if Str (Word_End) /= ')' then
                     Put_Line
                       (Standard_Error,
                         "Missing closing parenthesis in variable name: "
                         & Str (Word_Start .. Word_End));
                     raise Invalid_Knowledge_Base;
                  else
                     Append
                       (Result,
                        Callback
                          (Var_Name => Str (Word_Start .. W - 1),
                           Index    => Str (W + 1 .. Word_End - 1)));
                  end if;
                  exit;
               end if;
            end loop;

            if not Has_Index then
               Append (Result, Callback (Str (Word_Start .. Word_End), ""));
            end if;

            Last := Tmp;
            Pos  := Last;
         else
            Pos := Pos + 1;
         end if;
      end loop;
      Append (Result, Str (Last .. Str_Len));
      return To_String (Result);
   end Substitute_Variables;

   --------------------------------------------------
   -- Substitute_Variables_In_Compiler_Description --
   --------------------------------------------------

   function Substitute_Variables_In_Compiler_Description
     (Str  : String;
      Comp : Compiler) return String
   is
      function Callback (Var_Name, Index : String) return String;

      --------------
      -- Callback --
      --------------

      function Callback (Var_Name, Index : String) return String is
      begin
         if Index /= "" then
            Put_Line
              (Standard_Error,
               "Indexed variables only allowed in <configuration> (in "
                & Var_Name & "(" & Index & ")");
            raise Invalid_Knowledge_Base;
         end if;

         return Get_Variable_Value (Comp, Var_Name);
      end Callback;

      function Do_Substitute is new Substitute_Variables (Callback);
   begin
      return Do_Substitute (Str);
   end Substitute_Variables_In_Compiler_Description;

   -------------------------------------------
   -- Substitute_Variables_In_Configuration --
   -------------------------------------------

   function Substitute_Variables_In_Configuration
     (Base  : Knowledge_Base;
      Str   : String;
      Comps : Compiler_Lists.List) return String
   is
      function Callback (Var_Name, Index : String) return String;

      --------------
      -- Callback --
      --------------

      function Callback (Var_Name, Index : String) return String is
         C    : Compiler_Lists.Cursor;
         Comp : Compiler_Access;
         Idx  : constant Name_Id := Get_String_Or_No_Name (To_Lower (Index));
      begin
         if Var_Name = "GPRCONFIG_PREFIX" then
            return Executable_Prefix_Path;

         elsif Index = "" then

            if Var_Name = "TARGET"
              and then not Is_Empty (Comps)
            then
               --  Can have an optional language index.
               --  If there is no index, all compilers share the same target,
               --  so just take that of the first compiler in the list

               return Normalized_Target
                 (Base,
                  Compiler_Lists.Element (First (Comps)).Targets_Set);

            else
               Put_Line
                 (Standard_Error,
                  "Ambiguous variable substitution, need to specify the"
                  & " language (in " & Var_Name & ")");
               raise Invalid_Knowledge_Base;
            end if;

         else
            C := First (Comps);
            while Has_Element (C) loop
               Comp := Compiler_Lists.Element (C);

               if Comp.Selected
                 and then Comp.Language_LC = Idx
               then
                  return Get_Variable_Value (Comp.all, Var_Name);
               end if;

               Next (C);
            end loop;
         end if;

         return "";
      end Callback;

      function Do_Substitute is new Substitute_Variables (Callback);
   begin
      return Do_Substitute (Str);
   end Substitute_Variables_In_Configuration;

   --------------------
   -- Parse_All_Dirs --
   --------------------

   procedure Parse_All_Dirs
     (Processed_Value : out External_Value_Lists.List;
      Current_Dir     : String;
      Path_To_Check   : String;
      Regexp          : Pattern_Matcher;
      Regexp_Str      : String;
      Value_If_Match  : Name_Id;
      Group           : Integer;
      Group_Match     : String := "";
      Group_Count     : Natural := 0)
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
               (Value          => Value_If_Match,
                Extracted_From => Get_String (Current_Dir)));
         else
            Put_Verbose ("<dir>: SAVE " & Current_Dir);
            Append
              (Processed_Value,
               (Value          => Get_String (Group_Match),
                Extracted_From => Get_String (Current_Dir)));
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
               Dir     : constant String :=
                           Normalize_Pathname
                             (Current_Dir, Resolve_Links => False)
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
               File_Re     : constant String :=
                               Path_To_Check (First .. Last - 1);
               File_Regexp : constant Pattern_Matcher := Compile (File_Re);
               Search      : Search_Type;
               File        : Directory_Entry_Type;
               Filter      : Ada.Directories.Filter_Type;
            begin
               if Current_Verbosity /= Default and then File_Re = ".." then
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
                        Matched : Match_Array (0 .. Integer'Max (Group, 0));
                        Simple  : constant String := Simple_Name (File);
                        Count   : constant Natural :=
                                    Paren_Count (File_Regexp);
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
      Processed_Value  : out External_Value_Lists.List)
   is
      Saved_Path     : constant String :=
                         Ada.Environment_Variables.Value ("PATH");
      Status         : aliased Integer;
      Extracted_From : Name_Id;
      Tmp_Result     : Unbounded_String;
      Node_Cursor    : External_Value_Nodes.Cursor := First (Value);
      Node           : External_Value_Node;
      From_Static    : Boolean := False;
   begin
      Clear (Processed_Value);

      while Has_Element (Node_Cursor) loop

         while Has_Element (Node_Cursor) loop
            Node := External_Value_Nodes.Element (Node_Cursor);

            case Node.Typ is
               when Value_Variable =>
                  Extracted_From := Node.Var_Name;

               when Value_Constant =>
                  if Node.Value = No_Name then
                     Tmp_Result := Null_Unbounded_String;
                  else
                     Tmp_Result := To_Unbounded_String
                       (Substitute_Variables_In_Compiler_Description
                          (Get_Name_String (Node.Value), Comp));
                  end if;
                  From_Static := True;
                  Put_Verbose
                    (Attribute & ": constant := " & To_String (Tmp_Result));

               when Value_Shell =>
                  Ada.Environment_Variables.Set
                    ("PATH",
                     Get_Name_String (Comp.Path)
                     & Path_Separator & Saved_Path);

                  declare
                     Command : constant String :=
                                 Substitute_Variables_In_Compiler_Description
                                   (Get_Name_String (Node.Command), Comp);
                  begin
                     Tmp_Result := Null_Unbounded_String;
                     declare
                        Args   : Argument_List_Access :=
                                   Argument_String_To_List (Command);
                        Output : constant String := Get_Command_Output
                          (Command    => Args (Args'First).all,
                           Arguments  => Args (Args'First + 1 .. Args'Last),
                           Input      => "",
                           Status     => Status'Unchecked_Access,
                           Err_To_Out => True);
                     begin
                        GNAT.Strings.Free (Args);
                        Ada.Environment_Variables.Set ("PATH", Saved_Path);
                        Tmp_Result := To_Unbounded_String (Output);

                        if Current_Verbosity = High then
                           Put_Verbose (Attribute & ": executing """ & Command
                                        & """ output=""" & Output & """");
                        elsif Current_Verbosity = Medium then
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
                     Search : constant String :=
                       Substitute_Variables_In_Compiler_Description
                         (Get_Name_String (Node.Directory), Comp);
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
                           Value_If_Match  => Node.Dir_If_Match,
                           Group           => Node.Directory_Group);
                     else
                        if Current_Verbosity /= Default then
                           Put_Verbose
                             (Attribute & ": search directories matching "
                              & Search & ", starting from "
                              & Get_Name_String (Comp.Path), 1);
                        end if;
                        Parse_All_Dirs
                          (Processed_Value => Processed_Value,
                           Current_Dir     => Get_Name_String (Comp.Path),
                           Path_To_Check   => Search,
                           Regexp          => Compile (Search),
                           Regexp_Str      => Search,
                           Value_If_Match  => Node.Dir_If_Match,
                           Group           => Node.Directory_Group);
                     end if;
                     Put_Verbose ("Done search directories", -1);
                  end;

               when Value_Grep =>
                  declare
                     Matched : Match_Array (0 .. Node.Group);
                     Tmp_Str : constant String := To_String (Tmp_Result);
                  begin
                     Match (Node.Regexp_Re.all, Tmp_Str, Matched);
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
                  if not Match
                    (Expression => Get_Name_String (Node.Must_Match),
                     Data       => To_String (Tmp_Result))
                  then
                     if Current_Verbosity /= Default then
                        Put_Verbose
                          ("Ignore compiler since external value """
                           & To_String (Tmp_Result) & """ must match "
                           & Get_Name_String (Node.Must_Match));
                     end if;
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
                     Split  : String_Lists.List;
                     C      : String_Lists.Cursor;
                     Filter : Name_Id;
                  begin
                     if Node.Typ = Value_Filter then
                        Filter := Node.Filter;
                     else
                        Filter := No_Name;
                     end if;

                     --  When an external value is defined as a static string,
                     --  the only valid separator is ','. When computed
                     --  however, we also allow space as a separator
                     if From_Static then
                        Get_Words
                          (Words                => To_String (Tmp_Result),
                           Filter               => Filter,
                           Separator1           => ',',
                           Separator2           => ',',
                           Map                  => Split,
                           Allow_Empty_Elements => False);

                     else
                        Get_Words
                          (Words                => To_String (Tmp_Result),
                           Filter               => Filter,
                           Separator1           => ' ',
                           Separator2           => ',',
                           Map                  => Split,
                           Allow_Empty_Elements => False);
                     end if;

                     C := First (Split);
                     while Has_Element (C) loop
                        Append
                          (Processed_Value,
                           External_Value_Item'
                             (Value          => Get_String
                                (String_Lists.Element (C)),
                              Extracted_From => Extracted_From));
                        Next (C);
                     end loop;
                  end;

               else
                  Append
                    (Processed_Value,
                     External_Value_Item'
                       (Value          => Get_String (To_String (Tmp_Result)),
                        Extracted_From => Extracted_From));
               end if;
            when others =>
               null;
         end case;

         Extracted_From := No_Name;

         Next (Node_Cursor);
      end loop;
   end Get_External_Value;

   ---------------
   -- Get_Words --
   ---------------

   procedure Get_Words
     (Words                : String;
      Filter               : Namet.Name_Id;
      Separator1           : Character;
      Separator2           : Character;
      Map                  : out String_Lists.List;
      Allow_Empty_Elements : Boolean)
   is
      First      : Integer := Words'First;
      Last       : Integer;
      Filter_Set : String_Lists.List;
   begin
      if Filter /= No_Name then
         Get_Words (Get_Name_String (Filter), No_Name, Separator1,
                    Separator2, Filter_Set,
                    Allow_Empty_Elements => True);
      end if;

      if not Allow_Empty_Elements then
         while First <= Words'Last
           and then (Words (First) = Separator1
                     or else Words (First) = Separator2)
         loop
            First := First + 1;
         end loop;
      end if;

      while First <= Words'Last loop
         if Words (First) /= Separator1
           and then Words (First) /= Separator2
         then
            Last := First + 1;
            while Last <= Words'Last
              and then Words (Last) /= Separator1
              and then Words (Last) /= Separator2
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
     (Iterator       : in out Compiler_Iterator'Class;
      Base           : in out Knowledge_Base;
      Name           : Name_Id;
      Executable     : Name_Id;
      Directory      : String;
      Prefix         : Name_Id;
      From_Extra_Dir : Boolean;
      On_Target      : Targets_Set_Id;
      Descr          : Compiler_Description;
      Path_Order     : Integer;
      Continue       : out Boolean)
   is
      Target    : External_Value_Lists.List;
      Version   : External_Value_Lists.List;
      Languages : External_Value_Lists.List;
      Runtimes  : External_Value_Lists.List;
      Variables : External_Value_Lists.List;
      Comp      : Compiler;
      C, C2     : External_Value_Lists.Cursor;
      CS        : String_Lists.Cursor;
   begin
      Continue := True;

      --  verify that the compiler is indeed a real executable
      --  on Windows and not a cygwin symbolic link

      if On_Windows
        and then not Is_Windows_Executable
          (Directory & Directory_Separator & Get_Name_String (Executable))
      then
         Continue := True;
         return;
      end if;

      Comp.Name       := Name;
      Comp.Path       := Get_String
        (Name_As_Directory
           (Normalize_Pathname (Directory, Case_Sensitive => False)));
      Comp.Base_Name  := Get_String
        (GNAT.Directory_Operations.Base_Name
           (Get_Name_String (Executable), Suffix => Exec_Suffix.all));
      Comp.Path_Order := Path_Order;
      Comp.Prefix     := Prefix;
      Comp.Executable := Executable;

      --  Check the target first, for efficiency. If it doesn't match, no need
      --  to compute other attributes.

      if Executable /= No_Name then
         if not Is_Empty (Descr.Target) then
            Get_External_Value
              ("target",
               Value            => Descr.Target,
               Comp             => Comp,
               Split_Into_Words => False,
               Processed_Value  => Target);

            if not Is_Empty (Target) then
               Comp.Target :=
                 External_Value_Lists.Element (First (Target)).Value;
               Get_Targets_Set
                 (Base, Get_Name_String (Comp.Target), Comp.Targets_Set);
            else
               Put_Verbose ("Target unknown for this compiler");
               Comp.Targets_Set := Unknown_Targets_Set;
            end if;

            if On_Target /= All_Target_Sets
              and then Comp.Targets_Set /= On_Target
            then
               Put_Verbose
                 ("Target for this compiler does not match --target");
               Continue := True;
               return;
            end if;

         else
            Put_Verbose ("Target unspecified, always match");
            Comp.Targets_Set := All_Target_Sets;
         end if;

         --  Then get the value of the remaining attributes. For most of them,
         --  we must be able to find a valid value, or the compiler is simply
         --  ignored

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

         Get_External_Value
           ("variables",
            Value            => Descr.Variables,
            Comp             => Comp,
            Split_Into_Words => False,
            Processed_Value  => Variables);

         C := First (Variables);
         while Has_Element (C) loop
            declare
               Ext : constant External_Value_Item :=
                       External_Value_Lists.Element (C);
            begin
               if Ext.Value = No_Name then
                  if Current_Verbosity /= Default then
                     Put_Verbose
                       ("Ignore compiler since variable '"
                        & Get_Name_String (Ext.Extracted_From) & "' is empty");
                  end if;
                  Continue := True;
                  return;
               end if;

               if Variables_Maps.Contains
                 (Comp.Variables, Ext.Extracted_From)
               then
                  Put_Line
                    (Standard_Error, "Variable '"
                     & Get_Name_String (Ext.Extracted_From)
                     & "' is already defined");
               else
                  Variables_Maps.Insert
                    (Comp.Variables, Ext.Extracted_From, Ext.Value);
               end if;
            end;
            Next (C);
         end loop;
      end if;

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

      if Executable /= No_Name then
         Get_External_Value
           ("runtimes",
            Value            => Descr.Runtimes,
            Comp             => Comp,
            Split_Into_Words => True,
            Processed_Value  => Runtimes);
         --  Put default runtime first

         if not Is_Empty (Runtimes) then
            CS := First (Descr.Default_Runtimes);
            Defaults_Loop :
            while Has_Element (CS) loop
               C2 := First (Runtimes);
               while Has_Element (C2) loop
                  if Get_Name_String (External_Value_Lists.Element (C2).Value)
                    = String_Lists.Element (CS)
                  then
                     Prepend (Runtimes, External_Value_Lists.Element (C2));
                     Delete (Runtimes, C2);
                     exit Defaults_Loop;
                  end if;
                  Next (C2);
               end loop;
               Next (CS);
            end loop Defaults_Loop;
         end if;
      end if;

      C := First (Languages);
      while Has_Element (C) loop
         declare
            L : constant Name_Id := External_Value_Lists.Element (C).Value;
         begin
            Comp.Language_Case := L;
            Comp.Language_LC   := Get_String (To_Lower (Get_Name_String (L)));

            if Is_Empty (Runtimes) then
               if Descr.Runtimes /= Null_External_Value then
                  Put_Verbose ("No runtime found where one is required for: "
                               & Get_Name_String (Comp.Path));
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
     (Base            : Knowledge_Base;
      Comp            : Compiler;
      As_Config_Arg   : Boolean;
      Show_Target     : Boolean := False;
      Rank_In_List    : Integer := -1;
      Parser_Friendly : Boolean := False) return String
   is
      function Runtime_Or_Empty return String;
      function Rank return String;
      function Target return String;
      --  Return various aspects of the compiler;

      ----------------------
      -- Runtime_Or_Empty --
      ----------------------

      function Runtime_Or_Empty return String is
      begin
         if Comp.Runtime /= No_Name then
            return " (" & Get_Name_String (Comp.Runtime) & " runtime)";
         else
            return "";
         end if;
      end Runtime_Or_Empty;

      ----------
      -- Rank --
      ----------

      function Rank return String is
         Result : String (1 .. 4) := "    ";
         Img : constant String := Rank_In_List'Img;
      begin
         if Rank_In_List > 0 then
            Result (4 - Img'Length + 1 .. 4) := Img;
         end if;

         if Comp.Selected then
            Result (1) := '*';
         end if;

         return Result;
      end Rank;

      ------------
      -- Target --
      ------------

      function Target return String is
      begin
         if Show_Target then
            return " on " & Get_Name_String (Comp.Target);
         else
            return "";
         end if;
      end Target;

   begin
      if As_Config_Arg then
         return Get_Name_String_Or_Null (Comp.Language_Case)
           & ',' & Get_Name_String_Or_Null (Comp.Version)
           & ',' & Get_Name_String_Or_Null (Comp.Runtime)
           & ',' & Get_Name_String_Or_Null (Comp.Path)
           & ',' & Get_Name_String_Or_Null (Comp.Name);

      elsif Parser_Friendly then
         return Rank & " target:"
           & Get_Name_String_Or_Null (Comp.Target) & ASCII.LF
           & Rank & " normalized_target:"
           & Normalized_Target (Base, Comp.Targets_Set) & ASCII.LF
           & Rank & " executable:"
           & Get_Name_String_Or_Null (Comp.Executable) & ASCII.LF
           & Rank & " path:"
           & Get_Name_String_Or_Null (Comp.Path) & ASCII.LF
           & Rank & " lang:"
           & Get_Name_String_Or_Null (Comp.Language_Case) & ASCII.LF
           & Rank & " name:"
           & Get_Name_String_Or_Null (Comp.Name) & ASCII.LF
           & Rank & " version:"
           & Get_Name_String_Or_Null (Comp.Version) & ASCII.LF
           & Rank & " runtime:"
           & Get_Name_String_Or_Null (Comp.Runtime) & ASCII.LF
           & Rank & " native:"
           & Boolean'Image
               (Query_Targets_Set (Base, Hostname) = Comp.Targets_Set);

      elsif Comp.Executable = No_Name then
         --  A language that requires no compiler

         return Rank
           & ". "
           & Get_Name_String_Or_Null (Comp.Language_Case)
           & " (no compiler required)";

      else
         return Rank
           & ". "
           & Get_Name_String_Or_Null (Comp.Name) & " for "
           & Get_Name_String_Or_Null (Comp.Language_Case)
           & " in " & Get_Name_String_Or_Null (Comp.Path)
           & Target
           & " version " & Get_Name_String_Or_Null (Comp.Version)
           & Runtime_Or_Empty;
      end if;
   end To_String;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (Base            : Knowledge_Base;
      Compilers       : Compiler_Lists.List;
      Selected_Only   : Boolean;
      Show_Target     : Boolean := False;
      Parser_Friendly : Boolean := False) return String
   is
      Comp   : Compiler_Lists.Cursor := First (Compilers);
      Result : Unbounded_String;
      Rank   : Natural := 1;
   begin
      while Has_Element (Comp) loop
         if Compiler_Lists.Element (Comp).Selected
           or else (not Selected_Only
                    and then Compiler_Lists.Element (Comp).Selectable)
         then
            Append
              (Result,
               To_String
                 (Base,
                  Compiler_Lists.Element (Comp).all, False,
                  Show_Target     => Show_Target,
                  Rank_In_List    => Rank,
                  Parser_Friendly => Parser_Friendly));
            Append (Result, ASCII.LF);
         end if;

         Rank := Rank + 1;
         Next (Comp);
      end loop;
      return To_String (Result);
   end To_String;

   -----------------------------
   -- Foreach_Compiler_In_Dir --
   -----------------------------

   procedure Foreach_Compiler_In_Dir
     (Iterator       : in out Compiler_Iterator'Class;
      Base           : in out Knowledge_Base;
      Directory      : String;
      From_Extra_Dir : Boolean;
      On_Target      : Targets_Set_Id;
      Path_Order     : Integer;
      Continue       : out Boolean)
   is
      use CDM;

      function Executable_Pattern return String;
      pragma Inline (Executable_Pattern);
      --  Returns a pattern which matchs executable

      ------------------------
      -- Executable_Pattern --
      ------------------------

      function Executable_Pattern return String is
      begin
         if On_Windows then
            return "*.{exe,bat,cmd}";
         else
            return "";
         end if;
      end Executable_Pattern;

      C      : CDM.Cursor;
      Search : Search_Type;
      Dir    : Directory_Entry_Type;
   begin
      --  Since the name of an executable can be a regular expression, we need
      --  to look at all files in the directory to see if they match. This
      --  requires more system calls than if the name was always a simple
      --  string. So we first check which of the two algorithms should be used.

      Continue := True;

      if Current_Verbosity /= Default then
         Put_Verbose ("Foreach compiler in "
                      & Directory & " regexp="
                      & Boolean'Image (Base.Check_Executable_Regexp)
                      & " extra_dir=" & From_Extra_Dir'Img,
                      1);
      end if;

      if Base.Check_Executable_Regexp then
         begin
            Start_Search
              (Search    => Search,
               Directory => Directory,
               Pattern   => Executable_Pattern);
         exception
            when Ada.Directories.Name_Error =>
               Put_Verbose ("No such directory:" & Directory, -1);
               Continue := True;
               return;
            when Ada.Directories.Use_Error =>
               Put_Verbose ("Directory not readable:" & Directory, -1);
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
                     Config  : constant Compiler_Description :=
                                 CDM.Element (C);
                     Simple  : constant String := Simple_Name (Dir);
                     Matches : Match_Array
                       (0 .. Integer'Max (0, Config.Prefix_Index));
                     Matched : Boolean;
                     Prefix  : Name_Id := No_Name;
                  begin
                     --  A language with no expected compiler => always match
                     if Config.Executable = No_Name then
                        Put_Verbose
                          (Get_Name_String (Key (C))
                           & " requires no compiler",
                           1);
                        Continue := True;
                        Foreach_Language_Runtime
                          (Iterator       => Iterator,
                           Base           => Base,
                           Name           => Key (C),
                           Executable     => No_Name,
                           Directory      => "",
                           On_Target      => Unknown_Targets_Set,
                           Prefix         => No_Name,
                           From_Extra_Dir => From_Extra_Dir,
                           Descr          => Config,
                           Path_Order     => Path_Order,
                           Continue       => Continue);
                        Put_Verbose ("", -1);
                        exit For_All_Files_In_Dir when not Continue;
                        Matched := False;

                     elsif Config.Executable_Re /= null then
                        Match
                          (Config.Executable_Re.all,
                           Data       => Simple,
                           Matches    => Matches);
                        Matched := Matches (0) /= No_Match;
                     else
                        Matched := (Get_Name_String (Config.Executable) &
                                    Exec_Suffix.all) = Simple_Name (Dir);
                     end if;

                     if Matched then
                        Put_Verbose
                          (Get_Name_String (Key (C))
                           & " is candidate: filename=" & Simple,
                           1);

                        if Config.Executable_Re /= null
                          and then Config.Prefix_Index >= 0
                          and then Matches (Config.Prefix_Index) /=
                          No_Match
                        then
                           Prefix := Get_String
                             (Simple (Matches
                              (Config.Prefix_Index).First ..
                                Matches (Config.Prefix_Index).Last));
                        end if;

                        Continue := True;
                        Foreach_Language_Runtime
                          (Iterator       => Iterator,
                           Base           => Base,
                           Name           => Key (C),
                           Executable     => Get_String (Simple),
                           Directory      => Directory,
                           On_Target      => On_Target,
                           Prefix         => Prefix,
                           From_Extra_Dir => From_Extra_Dir,
                           Descr          => Config,
                           Path_Order     => Path_Order,
                           Continue       => Continue);

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
               Config : constant Compiler_Description :=
                           CDM.Element (C);
               F      : constant String :=
                          Normalize_Pathname
                            (Name           =>
                               Get_Name_String (Config.Executable),
                             Directory      => Directory,
                             Resolve_Links  => False,
                             Case_Sensitive =>
                               Case_Sensitive_Files) & Exec_Suffix.all;
            begin
               if Ada.Directories.Exists (F) then
                  Put_Verbose ("--------------------------------------");
                  Put_Verbose
                    ("Processing "
                     & Get_Name_String (Config.Name) & " in " & Directory);
                  Foreach_Language_Runtime
                    (Iterator       => Iterator,
                     Base           => Base,
                     Name           => Key (C),
                     Executable     => Config.Executable,
                     Prefix         => No_Name,
                     From_Extra_Dir => From_Extra_Dir,
                     On_Target      => On_Target,
                     Directory      => Directory,
                     Descr          => Config,
                     Path_Order     => Path_Order,
                     Continue       => Continue);
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
     (Iterator   : in out Compiler_Iterator;
      Base       : in out Knowledge_Base;
      On_Target  : Targets_Set_Id;
      Extra_Dirs : String := "")
   is
      Dirs : String_Lists.List;
      Map  : String_Lists.List;

      procedure Process_Path
        (Path : String; Prefix : Character; Prepend_To_List : Boolean);
      --  Add a directory to the list of directories to examine

      ------------------
      -- Process_Path --
      ------------------

      procedure Process_Path
        (Path : String; Prefix : Character; Prepend_To_List : Boolean)
      is
         First, Last : Natural;
      begin
         First := Path'First;
         while First <= Path'Last loop
            --  Skip null entries on PATH
            if Path (First) = GNAT.OS_Lib.Path_Separator then
               First := First + 1;

            else
               Last := First + 1;
               while Last <= Path'Last
                 and then Path (Last) /= GNAT.OS_Lib.Path_Separator
               loop
                  Last := Last + 1;
               end loop;

               declare
                  --  Use a hash to make sure we do not parse the same
                  --  directory twice. This is both more efficient and avoids
                  --  duplicates in the final result list. To handle the case
                  --  of links (on linux for instance /usr/bin/X11 points to
                  --  ".", ie /usr/bin, and compilers would appear duplicated),
                  --  we resolve symbolic links. This call is also set to fold
                  --  to lower-case when appropriate

                  Normalized : constant String :=
                                 Name_As_Directory
                                   (Normalize_Pathname
                                      (Path (First .. Last - 1),
                                       Resolve_Links  => True,
                                       Case_Sensitive => False));
               begin
                  if not Contains (Map, Normalized) then
                     Append (Map, Normalized);

                     --  Rerun normalize_pathname without resolve_links so that
                     --  the displayed path looks familiar to the user (no ..,
                     --  ./ or quotes, but still using the path as shown in
                     --  $PATH)
                     declare
                        Final_Path : constant String :=
                                       Normalize_Pathname
                                         (Path (First .. Last - 1),
                                          Resolve_Links  => False,
                                          Case_Sensitive => False);
                     begin
                        --  Windows is somewhat slow at parsing directories, do
                        --  not look into any directory under C:\windows as
                        --  there is no compiler to be found there anyway.

                        if not On_Windows
                          or else
                            (Final_Path'Length > 10
                             and then
                             To_Lower (Final_Path
                               (Final_Path'First .. Final_Path'First + 9)) /=
                             "c:\windows")
                        then
                           Put_Verbose ("Will examine "
                                        & Prefix & " " & Final_Path);

                           if Prepend_To_List then
                              Prepend (Dirs, Prefix & Final_Path);
                           else
                              Append (Dirs, Prefix & Final_Path);
                           end if;
                        end if;
                     end;
                  end if;
               end;

               First := Last + 1;
            end if;
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
              (Iterator       => Iterator,
               Base           => Base,
               Directory      => P (P'First + 1 .. P'Last),
               From_Extra_Dir => P (P'First) = 'E',
               Path_Order     => Path_Order,
               On_Target      => On_Target,
               Continue       => Continue);
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
         Append (List, Get_Name_String (Key (C)));

         Next (C);
      end loop;
   end Known_Compiler_Names;

   -----------
   -- Match --
   -----------

   procedure Match
     (Filter            : Compilers_Filter;
      Compilers         : Compiler_Lists.List;
      Matching_Compiler : out Compiler_Access;
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

   ------------------
   -- Filter_Match --
   ------------------

   function Filter_Match
     (Base : Knowledge_Base;
      Comp   : Compiler;
      Filter : Compiler) return Boolean is
   begin
      if Filter.Name /= No_Name
        and then Comp.Name /= Filter.Name
        and then Comp.Base_Name /= Filter.Name
      then
         if Current_Verbosity /= Default then
            Put_Verbose ("Filter=" & To_String (Base, Filter, True)
                         & ": name does not match");
         end if;
         return False;
      end if;

      if Filter.Path /= No_Name and then Filter.Path /= Comp.Path then
         if Current_Verbosity /= Default then
            Put_Verbose ("Filter=" & To_String (Base, Filter, True)
                         & ": path does not match");
         end if;
         return False;
      end if;

      if Filter.Version /= No_Name and then Filter.Version /= Comp.Version then
         if Current_Verbosity /= Default then
            Put_Verbose ("Filter=" & To_String (Base, Filter, True)
                         & ": version does not match");
         end if;
         return False;
      end if;

      if Filter.Runtime /= No_Name and then Filter.Runtime /= Comp.Runtime then
         if Current_Verbosity /= Default then
            Put_Verbose ("Filter=" & To_String (Base, Filter, True)
                         & ": runtime does not match");
         end if;
         return False;
      end if;

      if Filter.Language_LC /= No_Name
        and then Filter.Language_LC /= Comp.Language_LC
      then
         if Current_Verbosity /= Default then
            Put_Verbose ("Filter=" & To_String (Base, Filter, True)
                         & ": language does not match");
         end if;
         return False;
      end if;

      return True;
   end Filter_Match;

   -----------
   -- Match --
   -----------

   procedure Match
     (Filter            : Compiler_Filter;
      Compilers         : Compiler_Lists.List;
      Matching_Compiler : out Compiler_Access;
      Matched           : out Boolean)
   is
      C    : Compiler_Lists.Cursor := First (Compilers);
      Comp : Compiler_Access;
   begin
      while Has_Element (C) loop
         Comp := Compiler_Lists.Element (C);

         if Comp.Selected
           and then (Filter.Name = No_Name
                     or else Filter.Name = Comp.Name
                     or else Comp.Base_Name = Filter.Name)
           and then
             (Filter.Version_Re = null
              or else
                (Comp.Version /= No_Name
                 and then Match
                   (Filter.Version_Re.all, Get_Name_String (Comp.Version))))
           and then (Filter.Runtime_Re = null
                     or else
                       (Comp.Runtime /= No_Name and then
                        Match (Filter.Runtime_Re.all,
                               Get_Name_String (Comp.Runtime))))
           and then (Filter.Language_LC = No_Name
                     or else Filter.Language_LC = Comp.Language_LC)
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
      Matching_Compiler : out Compiler_Access;
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

      if Length (Filter) /= 1 then
         Matching_Compiler := null;
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
            declare
               Pattern : constant Pattern_Matcher :=
                           Compile
                             (String_Lists.Element (Target), Case_Insensitive);
            begin
               Comp := First (Compilers);
               while Has_Element (Comp) loop
                  if Compiler_Lists.Element (Comp).Selected then
                     if Compiler_Lists.Element (Comp).Target = No_Name then
                        if Match (Pattern, "") then
                           return not Negate;
                        end if;

                     elsif Match
                       (Pattern,
                        Get_Name_String (Compiler_Lists.Element (Comp).Target))
                     then
                        return not Negate;
                     end if;
                  end if;
                  Next (Comp);
               end loop;
            end;

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
     (Base      : Knowledge_Base;
      Packages  : in out String_Maps.Map;
      Compilers : Compiler_Lists.List;
      Config    : String)
   is
      procedure Add_Package
        (Name : String; Chunk : String; Prefix : String := "      ");
      --  Add the chunk in the appropriate package

      -----------------
      -- Add_Package --
      -----------------

      procedure Add_Package
        (Name : String; Chunk : String; Prefix : String := "      ")
      is
         C        : constant String_Maps.Cursor := Find (Packages, Name);
         Replaced : constant String :=
                      Substitute_Variables_In_Configuration
                        (Base, Chunk, Compilers);
      begin
         if Replaced /= "" then
            if Has_Element (C) then
               Replace_Element
                 (Packages,
                  C,
                  String_Maps.Element (C) & ASCII.LF & Prefix & Replaced);
            else
               Insert
                 (Packages,
                  Name, Prefix & To_Unbounded_String (Replaced));
            end if;
         end if;
      end Add_Package;

      First                         : Integer := Config'First;
      Pkg_Name_First, Pkg_Name_Last : Integer;
      Pkg_Content_First             : Integer;
      Last                          : Integer;

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
      C           : Compilers_Filter_Lists.Cursor :=
                      First (Config.Compilers_Filters);
      Comp_Filter : Compilers_Filter;
      Comp        : Compiler_Filter_Lists.Cursor;
      Filter      : Compiler_Filter;
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
               & Get_Name_String (Filter.Name) & "' version='"
               & Get_Name_String (Filter.Version) & "' runtime='"
               & Get_Name_String (Filter.Runtime) & "' language='"
               & Get_Name_String (Filter.Language_LC) & "' />");
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
      Config            : Configuration_Lists.Cursor :=
                            First (Base.Configurations);
      M                 : Boolean;
      Matching_Compiler : Compiler_Access;
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
               if Current_Verbosity /= Default then
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
      Output_File : String;
      Target      : String)
   is
      Config            : Configuration_Lists.Cursor :=
                            First (Base.Configurations);
      Output            : File_Type;
      Packages          : String_Maps.Map;
      Selected_Compiler : Compiler_Access;
      M                 : Boolean;
      Project_Name      : String := "Default";

      procedure Gen (C : String_Maps.Cursor);
      --  C is a cursor of the map "Packages"
      --  Generate the chunk of the config file corresponding to the
      --  given package.

      procedure Gen_And_Remove (Name : String);
      --  Generate the chunk of the config file corresponding to the
      --  package name and remove it from the map.

      ---------
      -- Gen --
      ---------

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

      --------------------
      -- Gen_And_Remove --
      --------------------

      procedure Gen_And_Remove (Name : String) is
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
              (Base,
               Packages,
               Compilers,
               Get_Name_String (Configuration_Lists.Element (Config).Config));
         end if;

         Next (Config);
      end loop;

      if Is_Empty (Packages) then
         Put_Line ("No valid configuration found");
         raise Generate_Error;
      end if;

      if not Opt.Quiet_Output then
         Put_Line ("Creating configuration file: " & Output_File);
      end if;

      Create (Output, Out_File, Output_File);

      Put_Line (Output,
                "--  This gpr configuration file was generated by gprconfig");
      Put_Line (Output, "--  using this command line:");
      Put (Output, "--  " & Command_Name);
      for I in 1 .. Argument_Count loop
         Put (Output, ' ');
         Put (Output, Argument (I));
      end loop;
      New_Line (Output);
      New_Line (Output);

      Put_Line (Output, "configuration project " & Project_Name & " is");

      if Target'Length > 0 and then Target /= "all" then
         Put_Line (Output, "   for Target use """ & Target & """;");
      end if;

      --  Generate known packages in order.  This takes care of possible
      --  dependencies.

      Gen_And_Remove ("");
      Gen_And_Remove ("Builder");
      Gen_And_Remove ("Compiler");
      Gen_And_Remove ("Naming");
      Gen_And_Remove ("Binder");
      Gen_And_Remove ("Linker");

      --  Generate remaining packages

      Iterate (Packages, Gen'Access);

      Put_Line (Output, "end " & Project_Name & ";");

      Close (Output);

   exception
      when Ada.Directories.Name_Error | Ada.IO_Exceptions.Use_Error =>
         Put_Line ("Could not create the file " & Output_File);
         raise Generate_Error;
   end Generate_Configuration;

   -----------------------
   -- Query_Targets_Set --
   -----------------------

   function Query_Targets_Set
     (Base   : Knowledge_Base;
      Target : String) return Targets_Set_Id
   is
      use Targets_Set_Vectors;
      use Target_Lists;
   begin
      if Target = "" then
         return All_Target_Sets;
      end if;

      for I in First_Index (Base.Targets_Sets)
        .. Last_Index (Base.Targets_Sets)
      loop
         declare
            Set : constant Target_Lists.List :=
                    Targets_Set_Vectors.Element
                      (Base.Targets_Sets, I).Patterns;
            C   : Target_Lists.Cursor := First (Set);
         begin
            while Has_Element (C) loop
               if GNAT.Regpat.Match
                 (Target_Lists.Element (C).all, Target) > 0
               then
                  return I;
               end if;

               Next (C);
            end loop;
         end;
      end loop;

      return Unknown_Targets_Set;
   end Query_Targets_Set;

   ----------------------
   --  Get_Targets_Set --
   ----------------------

   procedure Get_Targets_Set
     (Base   : in out Knowledge_Base;
      Target : String;
      Id     : out Targets_Set_Id)
   is
   begin
      Id := Query_Targets_Set (Base, Target);

      if Id /= Unknown_Targets_Set then
         return;
      end if;

      --  Create a new set.
      declare
         Set : Target_Lists.List;
      begin
         Put_Verbose ("create a new target set for " & Target);
         Set.Append (new Pattern_Matcher'(Compile ("^" & Target & "$")));
         Base.Targets_Sets.Append ((Get_String (Target), Set));
         Id := Base.Targets_Sets.Last_Index;
      end;
   end Get_Targets_Set;

   -----------------------
   -- Normalized_Target --
   -----------------------

   function Normalized_Target
     (Base : Knowledge_Base;
      Set  : Targets_Set_Id) return String
   is
      Result : constant Target_Set_Description :=
                 Targets_Set_Vectors.Element (Base.Targets_Sets, Set);
   begin
      return Get_Name_String (Result.Name);
   end Normalized_Target;

   ----------------
   -- Get_String --
   ----------------

   function Get_String (Str : String) return Namet.Name_Id is
   begin
      Name_Len := Str'Length;
      Name_Buffer (1 .. Name_Len) := Str;
      return Name_Find;
   end Get_String;

   ---------------------------
   -- Get_String_Or_No_Name --
   ---------------------------

   function Get_String_Or_No_Name (Str : String) return Namet.Name_Id is
   begin
      if Str = "" then
         return No_Name;
      else
         Name_Len := Str'Length;
         Name_Buffer (1 .. Name_Len) := Str;
         return Name_Find;
      end if;
   end Get_String_Or_No_Name;

   ---------------------------
   -- Hash_Case_Insensitive --
   ---------------------------

   function Hash_Case_Insensitive
     (Name : Namet.Name_Id) return Ada.Containers.Hash_Type is
   begin
      return Hash_Type (Name);
   end Hash_Case_Insensitive;

   -----------------------------
   -- Get_Name_String_Or_Null --
   -----------------------------

   function Get_Name_String_Or_Null (Name : Name_Id) return String is
   begin
      if Name = No_Name then
         return "";
      else
         Get_Name_String (Name);
         return Name_Buffer (1 .. Name_Len);
      end if;
   end Get_Name_String_Or_Null;

   -------------------
   -- Set_Selection --
   -------------------

   procedure Set_Selection
     (Compilers : in out Compiler_Lists.List;
      Cursor    : Compiler_Lists.Cursor;
      Selected  : Boolean)
   is
      procedure Internal (Comp : in out Compiler_Access);

      --------------
      -- Internal --
      --------------

      procedure Internal (Comp : in out Compiler_Access) is
      begin
         Set_Selection (Comp.all, Selected);
      end Internal;

   begin
      Update_Element (Compilers, Cursor, Internal'Access);
   end Set_Selection;

   -------------------
   -- Set_Selection --
   -------------------

   procedure Set_Selection
     (Comp     : in out Compiler;
      Selected : Boolean) is
   begin
      Comp.Selected := Selected;
   end Set_Selection;

   -----------------------------
   -- Extra_Dirs_From_Filters --
   -----------------------------

   function Extra_Dirs_From_Filters
     (Filters : Compiler_Lists.List) return String
   is
      C          : Compiler_Lists.Cursor  := First (Filters);
      Extra_Dirs : Unbounded_String;
      Elem       : Compiler_Access;
   begin
      while Has_Element (C) loop
         Elem := Compiler_Lists.Element (C);
         if Elem.Path /= No_Name then
            Append (Extra_Dirs, Get_Name_String (Elem.Path) & Path_Separator);
         end if;
         Next (C);
      end loop;
      return To_String (Extra_Dirs);
   end Extra_Dirs_From_Filters;

   -------------------------------------
   -- Complete_Command_Line_Compilers --
   -------------------------------------

   procedure Complete_Command_Line_Compilers
     (Base      : in out Knowledge_Base;
      On_Target : Targets_Set_Id;
      Filters   : Compiler_Lists.List;
      Compilers : in out Compiler_Lists.List)
   is
      type Cursor_Array
        is array (Count_Type range <>) of Compiler_Lists.Cursor;
      type Boolean_Array  is array (Count_Type range <>) of Boolean;

      type Batch_Iterator (Count : Count_Type) is new Compiler_Iterator with
         record
            Found      : Count_Type := 0;
            Compilers  : Compiler_Lists.List;
            Matched    : Cursor_Array (1 .. Count) :=
              (others => Compiler_Lists.No_Element);
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
      --  Search the first compiler matching each --config command line
      --  argument.

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
         C     : Compiler_Lists.Cursor := First (Iterator.Filters);
         Index : Count_Type := 1;
         Ncomp : Compiler_Access;
         El    : Compiler_Access;
      begin
         while Has_Element (C) loop
            Ncomp := null;
            El := Compiler_Lists.Element (C);

            --  A compiler in an "extra_dir" (ie specified on the command line)
            --  can only match if that directory was explicitly specified in
            --  --config. We do not want to find all compilers in /dir if that
            --  directory is not in $PATH

            if (not From_Extra_Dir or else El.Path = Comp.Path)
              and then Filter_Match (Base, Comp => Comp, Filter => El.all)
            then
               Ncomp := new Compiler'(Comp);
               if El.Runtime_Dir /= No_Name
                 and then Get_Name_String (Comp.Runtime) = "default"
               then
                  Ncomp.Runtime_Dir := El.Runtime_Dir;
                  Ncomp.Runtime := No_Name;
               end if;

               Append (Iterator.Compilers, Ncomp);

               if Current_Verbosity /= Default then
                  Put_Verbose
                    ("Saving compiler for possible backtracking: "
                     & To_String (Base, Ncomp.all, As_Config_Arg => True)
                     & " (matches --config "
                     & To_String (Base, El.all, As_Config_Arg => True)
                     & ")");
               end if;

               if Iterator.Matched (Index) = Compiler_Lists.No_Element then
                  Iterator.Found := Iterator.Found + 1;

                  Put_Verbose
                    ("Selecting it since this filter was not matched yet "
                     & Iterator.Found'Img & "/" & Iterator.Count'Img);

                  Iterator.Matched (Index) := Last (Iterator.Compilers);
                  Iterator.Found_One (Index) := True;
                  Set_Selection
                    (Iterator.Compilers, Iterator.Matched (Index),
                     True);

                  --  Only keep those compilers that are not incompatible
                  --  (according to the knowledge base). It might happen that
                  --  none is selected as a result, but appropriate action is
                  --  taken in Complete_Command_Line_Compilers. We ignore
                  --  incompatible sets as early as possible, in the hope to
                  --  limit the number of system calls if another set is found
                  --  before all directories are traversed.

                  if not Is_Supported_Config (Base, Iterator.Compilers) then
                     Set_Selection
                       (Iterator.Compilers, Iterator.Matched (Index), False);
                     Put_Verbose
                       ("Compilers are not compatible, cancelling last"
                        & " compiler found");
                     Iterator.Matched (Index) := Compiler_Lists.No_Element;
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
         Comp_Filter : constant Compiler_Access :=
                         Compiler_Lists.Element (Filter);
      begin
         while Has_Element (C) loop
            if Filter_Match
              (Base, Compiler_Lists.Element (C).all, Filter => Comp_Filter.all)
            then
               Set_Selection (Iter.Compilers, C, True);

               if Next (Filter) = Compiler_Lists.No_Element then
                  if Current_Verbosity /= Default then
                     Put_Verbose ("Testing the following compiler set:", 1);
                     Put_Verbose
                       (To_String
                          (Base, Iter.Compilers, Selected_Only => True));
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

               Set_Selection (Iter.Compilers, C, False);
            end if;

            Next (C);
         end loop;

         return False;
      end Foreach_Nth_Compiler;

      C          : Compiler_Lists.Cursor;
      Extra_Dirs : constant String := Extra_Dirs_From_Filters (Filters);
      Found_All  : Boolean := True;
   begin
      Iter.Filters   := Filters;

      Put_Verbose ("Completing info for --config parameters, extra_dirs="
                   & Extra_Dirs, 1);

      --  Find all the compilers in PATH and Extra_Dirs

      Foreach_Compiler_In_Path
        (Iterator   => Iter,
         Base       => Base,
         On_Target  => On_Target,
         Extra_Dirs => Extra_Dirs);

      Put_Verbose ("", -1);

      --  Check that we could find at least one of each compiler

      C := First (Filters);
      for F in Iter.Found_One'Range loop
         if not Iter.Found_One (F) then
            if not Opt.Quiet_Output then
               Put_Line
                 (Standard_Error,
                  "Error: no matching compiler found for --config="
                  & To_String
                    (Base, Compiler_Lists.Element (C).all,
                     As_Config_Arg => True));
            end if;
            Ada.Command_Line.Set_Exit_Status (1);
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
            Set_Selection (Iter.Compilers, C, False);
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

      Splice (Target => Compilers,
              Before => Compiler_Lists.No_Element,
              Source => Iter.Compilers);
   end Complete_Command_Line_Compilers;

   --------------------------------------
   -- Default_Knowledge_Base_Directory --
   --------------------------------------

   function Default_Knowledge_Base_Directory return String is
      Prog_Dir : constant String := Executable_Prefix_Path;
      Suffix   : constant String :=
                   "share" & Directory_Separator & "gprconfig";
   begin
      return Prog_Dir & Suffix;
   end Default_Knowledge_Base_Directory;

   --------------------
   -- Display_Before --
   --------------------

   function Display_Before (Comp1, Comp2 : Compiler_Access) return Boolean is

      type Compare_Type is (Before, Equal, After);
      function Compare (Name1, Name2 : Namet.Name_Id) return Compare_Type;
      --  Compare alphabetically two strings

      -------------
      -- Compare --
      -------------

      function Compare (Name1, Name2 : Namet.Name_Id) return Compare_Type is
      begin
         if Name1 = No_Name then
            if Name2 = No_Name then
               return Equal;
            else
               return Before;
            end if;

         elsif Name2 = No_Name then
            return After;
         end if;

         Get_Name_String (Name1);

         declare
            Str1 : constant String (1 .. Name_Len) :=
                     Name_Buffer (1 .. Name_Len);
         begin
            Get_Name_String (Name2);
            if Str1 < Name_Buffer (1 .. Name_Len) then
               return Before;
            elsif Str1 > Name_Buffer (1 .. Name_Len) then
               return After;
            else
               return Equal;
            end if;
         end;
      end Compare;

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
   end Display_Before;

   ----------------------------
   -- Parse_Config_Parameter --
   ----------------------------

   procedure Parse_Config_Parameter
     (Base              : Knowledge_Base;
      Config            : String;
      Compiler          : out Compiler_Access;
      Requires_Compiler : out Boolean)
   is
      Map : String_Lists.List;
      C   : String_Lists.Cursor;
   begin
      --  Only valid separator is ',', not spaces
      Get_Words
        (Config, Filter       => No_Name,
         Map                  => Map,
         Separator1           => ',',
         Separator2           => ',',
         Allow_Empty_Elements => True);

      Compiler := new GprConfig.Knowledge.Compiler;

      C := First (Map);
      declare
         LC : constant String := To_Lower (String_Lists.Element (C));
      begin
         Compiler.Language_Case :=
           Get_String_Or_No_Name (String_Lists.Element (C));
         Compiler.Language_LC   := Get_String_Or_No_Name (LC);

         if Is_Language_With_No_Compiler (Base, LC) then
            Put_Verbose ("Language " & LC & " requires no compiler");
            Compiler.Complete := True;
            Compiler.Selected := True;
            Compiler.Targets_Set := All_Target_Sets;
            Requires_Compiler := False;

         else
            Requires_Compiler := True;

            Next (C);
            if Has_Element (C) then
               Compiler.Version :=
                 Get_String_Or_No_Name (String_Lists.Element (C));
               Next (C);
               if Has_Element (C) then
                  declare
                     Rts : constant String := String_Lists.Element (C);
                  begin
                     --  If the runtime is a full path, only set Runtime_Dir
                     --  so that we could match the config for the default
                     --  runtime, but with this runtime path.

                     if Rts'Length > 0 and then Is_Absolute_Path (Rts) then
                        Compiler.Runtime := No_Name;
                        Compiler.Runtime_Dir := Get_String (Rts);
                     else
                        Compiler.Runtime := Get_String_Or_No_Name (Rts);
                     end if;
                  end;
                  Next (C);
                  if Has_Element (C) then
                     Compiler.Path := Get_String_Or_No_Name
                       (Name_As_Directory
                          (Normalize_Pathname (String_Lists.Element (C),
                           Case_Sensitive => False)));
                     Next (C);

                     if Has_Element (C) then
                        --  the name could be either a name as defined in the
                        --  knowledge base, or the base name of the executable
                        --  we are looking for. It must not include the exec
                        --  suffix.

                        Compiler.Name := Get_String_Or_No_Name
                          (GNAT.Directory_Operations.Base_Name
                             (String_Lists.Element (C),
                              Suffix => Exec_Suffix.all));
                     end if;
                  end if;
               end if;
            end if;

            Compiler.Complete := False;

            --  Complete_Command_Line_Compilers will check that this is a valid
            --  config
            Put_Verbose ("Language " & LC & " requires a compiler");
         end if;
      end;

   exception
      when E : others =>
         Put_Verbose ("Exception raised: " & Exception_Information (E));
         raise Invalid_Config;
   end Parse_Config_Parameter;

   ---------------------------
   -- Filter_Compilers_List --
   ---------------------------

   procedure Filter_Compilers_List
     (Base           : Knowledge_Base;
      Compilers      : in out Compiler_Lists.List;
      For_Target_Set : Targets_Set_Id)
   is
      procedure Mark_As_Selectable (Comp : in out Compiler_Access);
      procedure Mark_As_Unselectable (Comp : in out Compiler_Access);

      ------------------------
      -- Mark_As_Selectable --
      ------------------------

      procedure Mark_As_Selectable   (Comp : in out Compiler_Access) is
      begin
         Comp.Selectable := True;
      end Mark_As_Selectable;

      --------------------------
      -- Mark_As_Unselectable --
      --------------------------

      procedure Mark_As_Unselectable (Comp : in out Compiler_Access) is
      begin
         Comp.Selectable := False;
      end Mark_As_Unselectable;

      Comp, Comp2 : Compiler_Lists.Cursor;
      Selectable  : Boolean;

   begin
      Put_Verbose ("Filtering the list of compilers", 1);

      Comp := First (Compilers);
      while Has_Element (Comp) loop
         if not Compiler_Lists.Element (Comp).Selected then
            Selectable := True;

            if For_Target_Set /= All_Target_Sets
              and then Compiler_Lists.Element (Comp).Targets_Set /=
                 All_Target_Sets
              and then Compiler_Lists.Element (Comp).Targets_Set /=
                 For_Target_Set
            then
               Selectable := False;
               if Current_Verbosity /= Default then
                  Put_Verbose
                    ("Incompatible target for: "
                     & To_String
                       (Base, Compiler_Lists.Element (Comp).all, False));
               end if;
            end if;

            if Selectable then
               Comp2 := First (Compilers);
               while Has_Element (Comp2) loop
                  if Compiler_Lists.Element (Comp2).Selected
                    and then Compiler_Lists.Element (Comp2).Language_LC =
                    Compiler_Lists.Element (Comp).Language_LC
                  then
                     Selectable := False;
                     if Current_Verbosity /= Default then
                        Put_Verbose
                          ("Already selected language for "
                           & To_String
                             (Base, Compiler_Lists.Element (Comp).all, False));
                     end if;
                     exit;
                  end if;
                  Next (Comp2);
               end loop;
            end if;

            if Selectable then
               --  Would adding this compiler to the current selection end
               --  up with an unsupported config ?

               Set_Selection (Compilers, Comp, True);

               if not Is_Supported_Config (Base, Compilers) then
                  Selectable := False;

                  if Current_Verbosity /= Default then
                     Put_Verbose
                       ("Unsupported config for: "
                        & To_String
                          (Base, Compiler_Lists.Element (Comp).all, False));
                  end if;
               end if;
               Set_Selection (Compilers, Comp, False);
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
   end Filter_Compilers_List;

   -----------------
   -- Is_Selected --
   -----------------

   function Is_Selected (Comp : Compiler) return Boolean is
   begin
      return Comp.Selected;
   end Is_Selected;

   ------------
   -- Target --
   ------------

   function Target (Comp : Compiler) return Namet.Name_Id is
   begin
      return Comp.Target;
   end Target;

   --------------------
   -- Runtime_Dir_Of --
   --------------------

   function Runtime_Dir_Of (Comp : Compiler_Access) return Namet.Name_Id is
   begin
      if Comp = null then
         return Namet.No_Name;

      else
         return Comp.Runtime_Dir;
      end if;
   end Runtime_Dir_Of;

end GprConfig.Knowledge;
