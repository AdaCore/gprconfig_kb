------------------------------------------------------------------------------
--                   Copyright (C) 2006, AdaCore                            --
------------------------------------------------------------------------------

--  This unit is responsible for parsing the gprconfig knowledge base.

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Indefinite_Hashed_Sets;
with Ada.Strings.Hash;
with Ada.Strings.Hash_Case_Insensitive;
with Ada.Strings.Unbounded;

package GprConfig.Knowledge is

   type Knowledge_Base is private;

   procedure Parse_Knowledge_Base
     (Base : out Knowledge_Base; Directory : String);
   --  Parse info from the knowledge base, and store it in memory.
   --  Only information relevant to the current host is parsed.

   type Compiler is record
      Name       : Ada.Strings.Unbounded.Unbounded_String;
      Target     : Ada.Strings.Unbounded.Unbounded_String;
      Path       : Ada.Strings.Unbounded.Unbounded_String;
      Version    : Ada.Strings.Unbounded.Unbounded_String;
      Runtime    : Ada.Strings.Unbounded.Unbounded_String;
      Language   : Ada.Strings.Unbounded.Unbounded_String;
      Extra_Tool : Ada.Strings.Unbounded.Unbounded_String;
   end record;
   --  Describes one of the compilers found on the PATH.
   --  Path is the directory that contains the compiler executable.

   package Compiler_Lists is new Ada.Containers.Doubly_Linked_Lists (Compiler);

   procedure Find_Compilers_In_Path
     (Base      : Knowledge_Base;
      Compilers : out Compiler_Lists.List);
   --  Return the list of compilers found on PATH

   procedure Generate_Configuration
     (Base        : Knowledge_Base;
      Selected    : Compiler_Lists.List;
      Output_File : String);
   --  Generate the configuration file for the list of selected compilers

private
   type External_Value_Type is (Value_Constant,
                                Value_Shell,
                                Value_Directory);

   type External_Value (Typ : External_Value_Type := Value_Constant) is record
      Filter     : Ada.Strings.Unbounded.Unbounded_String;
      Must_Match : Ada.Strings.Unbounded.Unbounded_String;
      case Typ is
         when Value_Constant =>
            Value           : Ada.Strings.Unbounded.Unbounded_String;
         when Value_Shell    =>
            Command         : Ada.Strings.Unbounded.Unbounded_String;
            Regexp          : Ada.Strings.Unbounded.Unbounded_String;
            Group           : Natural;
         when Value_Directory =>
            Directory       : Ada.Strings.Unbounded.Unbounded_String;
            Directory_Group : Natural;
      end case;
   end record;
   Null_External_Value : constant External_Value :=
     (Typ        => Value_Constant,
      Filter     => Ada.Strings.Unbounded.Null_Unbounded_String,
      Must_Match => Ada.Strings.Unbounded.Null_Unbounded_String,
      Value      => Ada.Strings.Unbounded.Null_Unbounded_String);

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
   end record;
   No_Compilers_Filter : constant Compilers_Filter :=
     (Compiler => Compiler_Filter_Lists.Empty_List);
   --  a <compilers> filter, that matches if any of its <compiler> child
   --  matches.

   package Compilers_Filter_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Compilers_Filter);
   package String_Sets is new Ada.Containers.Indefinite_Hashed_Sets
     (String, Ada.Strings.Hash, --  ??? On case-insensitive systems
      "=");

   type Configuration is record
      Compilers_Filters : Compilers_Filter_Lists.List;
      Targets_Filters   : String_Sets.Set;  --  these are regexps
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
