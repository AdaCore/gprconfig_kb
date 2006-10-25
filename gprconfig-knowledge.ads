------------------------------------------------------------------------------
--                   Copyright (C) 2006, AdaCore                            --
------------------------------------------------------------------------------

--  This unit is responsible for parsing the gprconfig knowledge base.
--  The files is that base are XML files, with the following known tags:
--      <gprconfig>:  the toplevel tag, accepts any of the following:
--
--      <compiler_description>: Describes how to find a compiler on the disk,
--          and extra basic information from its installation.
--          Supports the following child tags:
--          <name>:       Name of the compiler (display only). This must be
--                        unique in all configuration files.
--          <executable>: Name of the executable to search for in PATH.
--          <version>:    How to get the version number of the compiler. It is
--                        an external_value, see below. If the version cannot
--                        be found using this method, the compiler is ignored.
--          <languages>:  How to get the list of languages of the compiler. It
--                        is an external_value, see below.
--                        The value is finally parsed, and split into words,
--                        thus for instance "ada,c++,c" is a list of three
--                        languages
--          <runtimes>:   How to get the list of runtimes for the compiler. It
--                        is an external_value, see below.
--                        The value is finally parsed and split into words, as
--                        for languages.
--          <extra_tool>: An extra tool to execute if this compiler is
--                        selected. One example is a tool to generate a user-
--                        visible project file, like ada_runtime.gpr.
--
--       <configuration>: Describes a chunk of a configuration file for gprmake
--          This supports the following child tags:
--          <compilers>:  This configuration will be taken into account if any
--                        of the <compiler> child tag of <compilers> matches.
--                        The <compilers> node can be specified several times,
--                        in which case each of the <compilers> tag must match,
--                        (ie at least one of their <compiler> child matches).
--          <hosts>:      Matches if any of its <host> child matches. There can
--                        be a single <hosts> tag per configuration.
--          <config>:     The actual configuration file chunk to merge into
--                        the output file when all filters match.
--
--  Tags that might appear in several places (see above):
--  external_value ::= (constant string | <external> | <directory>)
--                     [<filter>] [<must_match>]
--
--      <external match="regexp" group="1">command</external>
--           Executes the command, then parse its output with the specified
--           regular expression. Group is the parenthesis group that contains
--           the result to return.  0 => everything.
--           The command is executed with the PATH from the environment
--           variable, except the current directory is put first. For instance,
--           when describing the GNAT compiler, the executable we search could
--           be "gnatmake", and the command for version is "gnatls -v". This
--           ensures that the gnatls we execute is the found matching gnatmake.
--
--      <directory group="1">lib/gcc/$HOST/.*/rts-(.*)/adainclude</directory>
--           Parses all files in the given directories, and return the matching
--           parenthesis group described in group.
--           Special substrings are recognized:
--             $HOST   => the current host name on which gprconfig is running
--             regexp  => all directories or files in current dir. The regexp
--                        cannot contain a directory separator.
--           The directory is relative to the current directory (see comment
--           for <external>
--
--      <filter>value1,value2,..</filter>
--           Whatever method was used to retrieve the value of external_value,
--           only those words that match one of those given in <filter> will be
--           returned. For instance, if you compiler returns "c,java" as its
--           supported languages, if filter is "c", then only "c" is returned,
--           "java" is filtered out.
--
--      <must_match>regexp</must_match>
--           If this tag is given, then the value of the external_value must
--           match the regexp. If it doesn't, this compiler gets ignored (at
--           least for this specific XML file, but another XML file might
--           describe it)
--
--      <compiler name="name" version="..." runtime="..." language="..."/>
--           Matches if the compiler "name" (which must match one of the <name>
--           tags from the <compiler_configuration>), with the given version,
--           is in use. All attributes except "name" are optional.
--
--      <host name="name" />
--           Matches if the current host is "name"

with Ada.Containers.Doubly_Linked_Lists;
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
      Name       : Ada.Strings.Unbounded.Unbounded_String;
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
      Extra_Tool : Ada.Strings.Unbounded.Unbounded_String;
   end record;
   package Compiler_Description_Maps is new
     Ada.Containers.Indefinite_Hashed_Maps
       (String, Compiler_Description, Ada.Strings.Hash_Case_Insensitive, "=");

   type Compilers_Filter is record
      Compiler : Compiler_Lists.List;
   end record;
   No_Compilers_Filter : constant Compilers_Filter :=
     (Compiler => Compiler_Lists.Empty_List);
   --  a <compilers> filter, that matches if any of its <compiler> child
   --  matches.

   package Compilers_Filter_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Compilers_Filter);

   type Configuration is record
      Compilers_Filters : Compilers_Filter_Lists.List;
      Config            : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   package Configuration_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Configuration);

   type Knowledge_Base is record
      Compilers      : Compiler_Description_Maps.Map;
      Configurations : Configuration_Lists.List;
   end record;

end GprConfig.Knowledge;
