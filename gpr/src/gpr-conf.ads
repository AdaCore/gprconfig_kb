------------------------------------------------------------------------------
--                                                                          --
--                           GPR PROJECT MANAGER                            --
--                                                                          --
--            Copyright (C) 2006-2015, Free Software Foundation, Inc.       --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

--  The following package manipulates the configuration files

with GPR.Tree;
with GPR.Proc;

package GPR.Conf is

   type Config_File_Hook is access procedure
     (Config_File       : in out GPR.Project_Node_Id;
      Project_Node_Tree : GPR.Tree.Project_Node_Tree_Ref);
   --  Hook called after the config file has been parsed. This lets the
   --  application do last minute changes to it (GPS uses this to add the
   --  default naming schemes for instance). At that point, the config file
   --  has not been applied to the project yet. When no config file was found,
   --  and automatic generation is disabled, it is possible that Config_File
   --  is set to Empty_Node when this procedure is called. You can then decide
   --  to create a new config file if you need.

   No_Configuration_File : constant String := "/";
   --  When specified as a parameter Config_File_Name in the procedures below,
   --  no existing configuration project file is parsed. This is used by
   --  gnatmake, gnatclean and the GNAT driver to avoid parsing an existing
   --  default configuration project file.

   procedure Add_Db_Switch_Arg (N : Name_Id);
   --  Add one argument to the --db switch

   procedure Parse_Project_And_Apply_Config
     (Main_Project               : out GPR.Project_Id;
      User_Project_Node          : out GPR.Project_Node_Id;
      Config_File_Name           : String                        := "";
      Autoconf_Specified         : Boolean;
      Project_File_Name          : String;
      Project_Tree               : GPR.Project_Tree_Ref;
      Project_Node_Tree          : GPR.Tree.Project_Node_Tree_Ref;
      Env                        : in out GPR.Tree.Environment;
      Packages_To_Check          : String_List_Access;
      Allow_Automatic_Generation : Boolean                       := True;
      Automatically_Generated    : out Boolean;
      Config_File_Path           : out String_Access;
      Target_Name                : String                        := "";
      Normalized_Hostname        : String;
      On_Load_Config             : Config_File_Hook              := null;
      Implicit_Project           : Boolean                       := False;
      On_New_Tree_Loaded         : GPR.Proc.Tree_Loaded_Callback := null);
   --  Find the main configuration project and parse the project tree rooted at
   --  this configuration project.
   --
   --  Project_Node_Tree must have been initialized first (and possibly the
   --  value for external references and project path should also have been
   --  set).
   --
   --  If the processing fails, Main_Project is set to No_Project. If the error
   --  happened while parsing the project itself (i.e. creating the tree),
   --  User_Project_Node is also set to Empty_Node.
   --
   --  If Config_File_Name is No_Configuration_File, then no configuration
   --  project file is parsed. Normally, in this case On_Load_Config is not
   --  null, and it is used to create a configuration project file in memory.
   --
   --  Autoconf_Specified indicates whether the user has specified --autoconf.
   --  If this is the case, the config file might be (re)generated, as
   --  appropriate, to match languages and target if the one specified doesn't
   --  already match.
   --
   --  Normalized_Hostname is the host on which gprbuild is returned,
   --  normalized so that we can more easily compare it with what is stored in
   --  configuration files. It is used when the target is unspecified, although
   --  we need to know the target specified by the user (Target_Name) when
   --  computing the name of the default config file that should be used.
   --
   --  If specified, On_Load_Config is called just after the config file has
   --  been created/loaded. You can then modify it before it is later applied
   --  to the project itself.
   --
   --  Any error in generating or parsing the config file is reported via the
   --  Invalid_Config exception, with an appropriate message. Any error while
   --  parsing the project file results in No_Project.
   --
   --  If Implicit_Project is True, the main project file being parsed is
   --  deemed to be in the current working directory, even if it is not the
   --  case. Implicit_Project is set to True when a tool such as gprbuild is
   --  invoked without a project file and is using an implicit project file
   --  that is virtually in the current working directory, but is physically
   --  in another directory.
   --
   --  If specified, On_New_Tree_Loaded is called after each aggregated project
   --  has been processed succesfully.

   procedure Process_Project_And_Apply_Config
     (Main_Project               : out GPR.Project_Id;
      User_Project_Node          : GPR.Project_Node_Id;
      Config_File_Name           : String                        := "";
      Autoconf_Specified         : Boolean;
      Project_Tree               : GPR.Project_Tree_Ref;
      Project_Node_Tree          : GPR.Tree.Project_Node_Tree_Ref;
      Env                        : in out GPR.Tree.Environment;
      Packages_To_Check          : String_List_Access;
      Allow_Automatic_Generation : Boolean                       := True;
      Automatically_Generated    : out Boolean;
      Config_File_Path           : out String_Access;
      Target_Name                : String                        := "";
      Normalized_Hostname        : String;
      On_Load_Config             : Config_File_Hook              := null;
      Reset_Tree                 : Boolean                       := True;
      On_New_Tree_Loaded         : GPR.Proc.Tree_Loaded_Callback := null;
      Do_Phase_1                 : Boolean                       := True);
   --  Same as above, except the project must already have been parsed through
   --  GPR.Part.Parse, and only the processing of the project and the
   --  configuration is done at this level.
   --
   --  If Reset_Tree is true, all projects are first removed from the tree.
   --  When_No_Sources indicates what should be done when no sources are found
   --  for one of the languages of the project.
   --
   --  If Require_Sources_Other_Lang is true, then all languages must have at
   --  least one source file, or an error is reported via When_No_Sources. If
   --  it is false, this is only required for Ada (and only if it is a language
   --  of the project).
   --
   --  If Do_Phase_1 is False, then GPR.Proc.Process_Project_Tree_Phase_1
   --  should not be called, as it has already been invoked successfully.

   Invalid_Config : exception;

   procedure Get_Or_Create_Configuration_File
     (Project                    : GPR.Project_Id;
      Conf_Project               : Project_Id;
      Project_Tree               : GPR.Project_Tree_Ref;
      Project_Node_Tree          : GPR.Tree.Project_Node_Tree_Ref;
      Env                        : in out GPR.Tree.Environment;
      Allow_Automatic_Generation : Boolean;
      Config_File_Name           : String             := "";
      Autoconf_Specified         : Boolean;
      Target_Name                : String             := "";
      Normalized_Hostname        : String;
      Packages_To_Check          : String_List_Access := null;
      Config                     : out GPR.Project_Id;
      Config_File_Path           : out String_Access;
      Automatically_Generated    : out Boolean;
      On_Load_Config             : Config_File_Hook   := null);
   --  Compute the name of the configuration file that should be used. If no
   --  default configuration file is found, a new one will be automatically
   --  generated if Allow_Automatic_Generation is true. This configuration
   --  project file will be generated in the object directory of project
   --  Conf_Project.
   --
   --  Any error in generating or parsing the config file is reported via the
   --  Invalid_Config exception, with an appropriate message.
   --
   --  On exit, Configuration_Project_Path is never null (if none could be
   --  found, Os.Fail was called and the program exited anyway).
   --
   --  The choice and generation of a configuration file depends on several
   --  attributes of the user's project file (given by the Project argument),
   --  e.g. list of languages that must be supported. Project must therefore
   --  have been partially processed (phase one of the processing only).
   --
   --  Config_File_Name should be set to the name of the config file specified
   --  by the user (either through gprbuild's --config or --autoconf switches).
   --  In the latter case, Autoconf_Specified should be set to true to indicate
   --  that the configuration file can be regenerated to match target and
   --  languages. This name can either be an absolute path, or the base name
   --  that will be searched in the default config file directories (which
   --  depends on the installation path for the tools).
   --
   --  Target_Name is used to chose the configuration file that will be used
   --  from among several possibilities.
   --
   --  If a project file could be found, it is automatically parsed and
   --  processed (and Packages_To_Check is used to indicate which packages
   --  should be processed).

--     procedure Add_Default_GNAT_Naming_Scheme
--       (Config_File  : in out GPR.Tree.Project_Node_Id;
--        Project_Tree : GPR.Tree.Project_Node_Tree_Ref);
   --  A hook that will create a new config file (in memory), used for
   --  Get_Or_Create_Configuration_File and Process_Project_And_Apply_Config
   --  and add the default GNAT naming scheme to it. Nothing is done if the
   --  config_file already exists, to avoid overriding what the user might
   --  have put in there.

   --------------
   -- Runtimes --
   --------------

   procedure Set_Runtime_For (Language : Name_Id; RTS_Name : String);
   --  Specifies the runtime to use for a specific language. Most of the time
   --  this should be used for Ada, but other languages can also specify their
   --  own runtime. This is in general specified via the --RTS command line
   --  switch, and results in a specific component passed to gprconfig's
   --  --config switch then automatically generating a configuration file.

   function Runtime_Name_For (Language : Name_Id) return String;
   --  Returns the runtime name for a language. Returns the value set by the
   --  last call to Set_Runtime_For, if any, otherwise returns an empty string.

   function Runtime_Name_Set_For (Language : Name_Id) return Boolean;
   --  Returns True only if Set_Runtime_For has been called for the Language

end GPR.Conf;
