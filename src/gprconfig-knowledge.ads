------------------------------------------------------------------------------
--                   Copyright (C) 2006, AdaCore                            --
------------------------------------------------------------------------------

--  This unit is responsible for parsing the gprconfig knowledge base.

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash_Case_Insensitive;
with Ada.Strings.Unbounded;

package GprConfig.Knowledge is

   type Knowledge_Base is private;

   procedure Parse_Knowledge_Base
     (Base : out Knowledge_Base; Directory : String);
   --  Parse info from the knowledge base, and store it in memory.
   --  Only information relevant to the current host is parsed.

   type Compiler is record
      Name        : Ada.Strings.Unbounded.Unbounded_String;
      Target      : Ada.Strings.Unbounded.Unbounded_String;
      Path        : Ada.Strings.Unbounded.Unbounded_String;
      Version     : Ada.Strings.Unbounded.Unbounded_String;
      Runtime     : Ada.Strings.Unbounded.Unbounded_String;
      Runtime_Dir : Ada.Strings.Unbounded.Unbounded_String;
      Language    : Ada.Strings.Unbounded.Unbounded_String;
      Extra_Tool  : Ada.Strings.Unbounded.Unbounded_String;
   end record;
   No_Compiler : constant Compiler;
   --  Describes one of the compilers found on the PATH.
   --  Path is the directory that contains the compiler executable.

   package Compiler_Lists is new Ada.Containers.Doubly_Linked_Lists (Compiler);

   procedure Find_Compilers_In_Path
     (Base      : Knowledge_Base;
      Compilers : out Compiler_Lists.List);
   --  Return the list of compilers found on PATH

   procedure Find_Matching_Compilers
     (Name      : String;
      Path      : String;
      Base      : Knowledge_Base;
      Compilers : out Compiler_Lists.List);
   --  Given a compiler and its name, find out as much information as we can.
   --  If the compiler is totally unknown, the returned list will be empty.

   procedure Known_Compiler_Names
     (Base : Knowledge_Base;
      List : out Ada.Strings.Unbounded.Unbounded_String);
   --  Set List to the comma-separated list of known compilers

   procedure Generate_Configuration
     (Base        : Knowledge_Base;
      Selected    : Compiler_Lists.List;
      Output_File : String);
   --  Generate the configuration file for the list of selected compilers

   function Is_Supported_Config
     (Base     : Knowledge_Base;
      Selected : Compiler_Lists.List) return Boolean;
   --  Whether we know how to link code compiled with all these selected
   --  compilers

   function Architecture_Equal (Arch1, Arch2 : String) return Boolean;
   --  Compares two architectures.
   --  ??? The comparison for now is the same as "=", but in the future we
   --  should have a way in the .xml file to indicate that "i686-pc-linux-gnu"
   --  is the same as "i686-suse-linux" for instance.

   function TU (Str : String) return Ada.Strings.Unbounded.Unbounded_String;
   --  returns an unbounded string for Str (or Null_Unbounded_String if
   --  Str is the empty string)

   package String_Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists
     (String);

   procedure Get_Words
     (Words  : String;
      Filter : Ada.Strings.Unbounded.Unbounded_String;
      Map    : out String_Lists.List;
      Allow_Empty_Elements : Boolean);
   --  Return the list of words in Words. Splitting is done on special
   --  characters, so as to be compatible with a list of languages or a list of
   --  runtimes
   --  If Allow_Empty_Elements is false, then empty strings are not stored in
   --  the list.

   function Get_Program_Directory return String;
   --  Get the directory in which the application is installed. For instance,
   --  it would return /usr/local if the gprconfig executable is
   --  /usr/local/bin/gprconfig.
   --  The returned value always ends with a directory separator

   function Name_As_Directory (Dir : String) return String;
   --  Ensure that Dir ends with a directory separator

private
   No_Compiler : constant Compiler :=
     (Name        => Ada.Strings.Unbounded.Null_Unbounded_String,
      Target      => Ada.Strings.Unbounded.Null_Unbounded_String,
      Path        => Ada.Strings.Unbounded.Null_Unbounded_String,
      Version     => Ada.Strings.Unbounded.Null_Unbounded_String,
      Runtime     => Ada.Strings.Unbounded.Null_Unbounded_String,
      Runtime_Dir => Ada.Strings.Unbounded.Null_Unbounded_String,
      Language    => Ada.Strings.Unbounded.Null_Unbounded_String,
      Extra_Tool  => Ada.Strings.Unbounded.Null_Unbounded_String);

   type External_Value_Type is (Value_Constant,
                                Value_Shell,
                                Value_Directory);
   type External_Value_Node
     (Typ : External_Value_Type := Value_Constant) is
      record
         case Typ is
            when Value_Constant =>
               Value           : Ada.Strings.Unbounded.Unbounded_String;
            when Value_Shell    =>
               Command         : Ada.Strings.Unbounded.Unbounded_String;
               Regexp          : Ada.Strings.Unbounded.Unbounded_String;
               Group           : Natural;
            when Value_Directory =>
               Directory       : Ada.Strings.Unbounded.Unbounded_String;
               Directory_Group : Integer;
               Dir_If_Match    : Ada.Strings.Unbounded.Unbounded_String;
         end case;
      end record;

   package External_Value_Nodes is new Ada.Containers.Doubly_Linked_Lists
     (External_Value_Node);

   type External_Value is record
      Filter     : Ada.Strings.Unbounded.Unbounded_String;
      Must_Match : Ada.Strings.Unbounded.Unbounded_String;
      Nodes      : External_Value_Nodes.List;
   end record;

   Null_External_Value : constant External_Value :=
     (Filter     => Ada.Strings.Unbounded.Null_Unbounded_String,
      Must_Match => Ada.Strings.Unbounded.Null_Unbounded_String,
      Nodes      => External_Value_Nodes.Empty_List);

   type Compiler_Description is record
      Executable : Ada.Strings.Unbounded.Unbounded_String;
      Version    : External_Value;
      Languages  : External_Value;
      Runtimes   : External_Value;
      Target     : External_Value;
      Extra_Tool : Ada.Strings.Unbounded.Unbounded_String;
   end record;
   package Compiler_Description_Maps is new
     Ada.Containers.Indefinite_Hashed_Maps
       (String, Compiler_Description, Ada.Strings.Hash_Case_Insensitive, "=");

   type Compiler_Filter is record
      Name       : Ada.Strings.Unbounded.Unbounded_String;
      Version    : Ada.Strings.Unbounded.Unbounded_String;
      Runtime    : Ada.Strings.Unbounded.Unbounded_String;
      Language   : Ada.Strings.Unbounded.Unbounded_String;
   end record;
   --  Representation for a <compiler> node (in <configuration>)

   package Compiler_Filter_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Compiler_Filter);

   type Compilers_Filter is record
      Compiler : Compiler_Filter_Lists.List;
      Negate   : Boolean := False;
   end record;
   No_Compilers_Filter : constant Compilers_Filter :=
     (Compiler => Compiler_Filter_Lists.Empty_List,
      Negate   => False);
   --  a <compilers> filter, that matches if any of its <compiler> child
   --  matches.

   package Compilers_Filter_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Compilers_Filter);

   type Configuration is record
      Compilers_Filters : Compilers_Filter_Lists.List;
      Targets_Filters   : String_Lists.List;  --  these are regexps
      Negate_Targets    : Boolean  := False;
      Config            : Ada.Strings.Unbounded.Unbounded_String;

      Supported         : Boolean;
      --  Whether the combination of compilers is supported
   end record;

   package Configuration_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Configuration);

   type Knowledge_Base is record
      Compilers      : Compiler_Description_Maps.Map;
      Configurations : Configuration_Lists.List;
   end record;

end GprConfig.Knowledge;
