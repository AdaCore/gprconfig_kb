------------------------------------------------------------------------------
--                                                                          --
--                             GPR TECHNOLOGY                               --
--                                                                          --
--                     Copyright (C) 2012-2015, AdaCore                     --
--                                                                          --
-- This is  free  software;  you can redistribute it and/or modify it under --
-- terms of the  GNU  General Public License as published by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for more details.  You should have received  a copy of the  GNU  --
-- General Public License distributed with GNAT; see file  COPYING. If not, --
-- see <http://www.gnu.org/licenses/>.                                      --
--                                                                          --
------------------------------------------------------------------------------

private with GNAT.OS_Lib;

private with GPR;

package Gprinstall is

private

   use GPR;

   use GNAT.OS_Lib;

   DS : constant Character := GNAT.OS_Lib.Directory_Separator;

   Display_Paths : Boolean := False;
   --  Set by switch --display-paths: config project path and user project path
   --  will be displayed after all command lines witches have been scanned.

   Project_File_Name_Expected : Boolean := False;
   --  True when last switch was -P

   Main_Project_Dir : String_Access;
   --  The absolute path of the project directory of the main project,
   --  initialized in procedure Initialize.

   Force_Installations : Boolean := False;
   --  True if gprinstall is allowed to overwrite existing files

   --  A Param, track if it is set on the command line or if it is the default
   --  value.

   type Param is record
      V       : String_Access;
      Default : Boolean := False;
   end record;

   function Dup (P : Param) return Param;
   --  Return a copy of P

   procedure Free (P : in out Param);
   --  Free P

   Global_Prefix_Dir : Param := (null, True);
   --  Root installation directory

   Global_Exec_Subdir : Param := (new String'("bin" & DS), True);
   --  Subdirectory for executable

   Global_Lib_Subdir : Param := (new String'("lib" & DS), True);
   --  Subdirectory for libraries

   Global_Link_Lib_Subdir : Param := (new String'("lib" & DS), True);
   --  Subdirectory for libraries sym links (on UNIX)

   Global_Sources_Subdir : Param := (new String'("include" & DS), True);
   --  Subdirectory for sources

   Global_Project_Subdir : Param :=
                             (new String'("share" & DS & "gpr" & DS), True);
   --  Subdirectory used for the installed generated project file

   Global_Install_Mode : Param := (new String'("dev"), True);
   --  Either dev or usage.
   --  "dev" if the installation is for developers (source of the libraries
   --  are also installed). If set to "usage" only the shared libraries are
   --  installed and/or the main executables.

   Global_Install_Name : Param := (new String'("default"), True);
   --  The installation name, the default value is the project name without
   --  extension.

   Build_Var  : String_Access;
   --  Name of the build variable for installed project file

   No_Build_Var : Boolean := False;
   --  Wether a build variable is to be generated

   Build_Name : String_Access := new String'("default");
   --  Name of the current build

   Search_Project_Dir_Expected : Boolean := False;
   --  True when last switch was -aP

   Project_Tree : constant Project_Tree_Ref :=
                    new Project_Tree_Data (Is_Root_Tree => True);
   --  The project tree

   Copyright_Output : Boolean := False;
   Usage_Output     : Boolean := False;
   --  Flags to avoid multiple displays of Copyright notice and of Usage

   Usage_Needed : Boolean := False;
   --  Set by swith -h: usage will be displayed after all command line
   --  switches have been scanned.

   Recursive : Boolean := False;
   --  Installation will recurse into all imported projects

   Dry_Run : Boolean := False;
   --  Whether the actual installation takes place or not. If Dry_Run is set to
   --  True then the action will be displayed on the console but actually not
   --  performed.

   type Usage_Kind is (Install_Mode, Uninstall_Mode, List_Mode);

   Usage_Mode : Usage_Kind := Install_Mode;
   --  Set to true if project is to be uninstalled

   Output_Stats : Boolean := False;
   --  Wether the stats are to be displayed when listing installed packages

   All_Sources : Boolean := True;
   --  By default install all the sources. If set to False install only
   --  the sources needed to use the project (the interface for a SAL).

   Add_Lib_Link : Boolean := True;
   --  Wether to copy the shared library into the executable directory on
   --  Windows or create a link into the lib directory on UNIX.

   Create_Dest_Dir : Boolean := False;
   --  Wether to create the missing directories in the destination point

   Sig_Line : constant String := "S ";
   --  The prefix of the line containing the original project's signature

   Sources_Only : Boolean := False;
   --  Whether to copy only the projects sources. This means that the object,
   --  library, executable files are not to be copied.

end Gprinstall;
