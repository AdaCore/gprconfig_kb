------------------------------------------------------------------------------
--                                                                          --
--                             GPR TECHNOLOGY                               --
--                                                                          --
--                     Copyright (C) 2006-2019, AdaCore                     --
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

--  This package contains the implementation of gprclean.
--  See gprclean.adb

with GNAT.OS_Lib;    use GNAT.OS_Lib;

with GPR;            use GPR;
with Gpr_Build_Util; use Gpr_Build_Util;

package Gprclean is

   --  Everyting private so only accessible to child packages

private

   Object_Suffix : constant String := Get_Target_Object_Suffix.all;
   --  The suffix of object files on this platform

   Initialized : Boolean := False;
   --  Set to True by the first call to Initialize.
   --  To avoid reinitialization of some packages.

   Project_Tree : constant Project_Tree_Ref :=
                    new Project_Tree_Data (Is_Root_Tree => True);
   --  The project tree

   Force_Deletions : Boolean := False;
   --  Set to True by switch -f. When True, attempts to delete non writable
   --  files will be done.

   Do_Nothing : Boolean := False;
   --  Set to True when switch -n is specified. When True, no file is deleted.
   --  gnatclean only lists the files that would have been deleted if the
   --  switch -n had not been specified.

   Remove_Empty_Dir : Boolean := False;
   --  Set to True when switch -p is specified. When True, the empty directory
   --  where the artefact files was deleted will be deleted too.

   File_Deleted : Boolean := False;
   --  Set to True if at least one file has been deleted

   Copyright_Displayed : Boolean := False;
   Usage_Displayed     : Boolean := False;
   --  Flags set to True when the action is performed, to avoid duplicate
   --  displays.

   All_Projects : Boolean := False;
   --  Set to True when option -r is used, so that all projects in the project
   --  tree are cleaned.

   Processed_Projects : Project_Vectors.Vector;
   --  Table to keep track of what project files have been processed, when
   --  switch -r is specified.

   procedure Clean_Project
     (Project            : Project_Id;
      Project_Tree       : Project_Tree_Ref;
      Main               : Boolean;
      Remove_Executables : Boolean);
   --  Do the cleaning work for Project.
   --  This procedure calls itself recursively when there are several
   --  project files in the tree rooted at the main project file and switch -r
   --  has been specified.
   --  Main is True iff Project is a main project.
   --  If Remove_Executables is true, the binder files and results of the
   --  linker are also removed.

   procedure Delete (In_Directory : String; File : String);
   --  Delete one file, or list the file name if switch -n is specified

end Gprclean;
