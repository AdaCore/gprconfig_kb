------------------------------------------------------------------------------
--                                                                          --
--                           GPR PROJECT MANAGER                            --
--                                                                          --
--          Copyright (C) 2000-2015, Free Software Foundation, Inc.         --
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

--  Implements the parsing of project files into a tree

with GPR.Tree; use GPR.Tree;

package GPR.Part is

   type Errout_Mode is
     (Always_Finalize,
      Finalize_If_Error,
      Never_Finalize);
   --  Whether Parse should call Errout.Finalize (which prints the error
   --  messages on stdout). When Never_Finalize is used, Errout is not reset
   --  either at the beginning of Parse.

   procedure Parse
     (In_Tree           : Project_Node_Tree_Ref;
      Project           : out Project_Node_Id;
      Project_File_Name : String;
      Errout_Handling   : Errout_Mode := Always_Finalize;
      Packages_To_Check : String_List_Access;
      Store_Comments    : Boolean := False;
      Current_Directory : String := "";
      Is_Config_File    : Boolean;
      Env               : in out GPR.Tree.Environment;
      Target_Name       : String := "";
      Implicit_Project  : Boolean := False);
   --  Parse project file and all its imported project files and create a tree.
   --  Return the node for the project (or Empty_Node if parsing failed). If
   --  Always_Errout_Finalize is True, Errout.Finalize is called in all cases,
   --  Otherwise, Errout.Finalize is only called if there are errors (but not
   --  if there are only warnings). Packages_To_Check indicates the packages
   --  where any unknown attribute produces an error. For other packages, an
   --  unknown attribute produces a warning. When Store_Comments is True,
   --  comments are stored in the parse tree.
   --
   --  Current_Directory is used for optimization purposes only, avoiding extra
   --  system calls.
   --
   --  Is_Config_File should be set to True if the project represents a config
   --  file (.cgpr) since some specific checks apply.
   --
   --  Target_Name will be used to initialize the default project path, unless
   --  In_Tree.Project_Path has already been initialized (which is the
   --  recommended use).
   --
   --  If Implicit_Project is True, the main project file being parsed is
   --  deemed to be in the current working directory, even if it is not the
   --  case. Implicit_Project is set to True when a tool such as gprbuild is
   --  invoked without a project file and is using an implicit project file
   --  that is virtually in the current working directory, but is physically
   --  in another directory.

end GPR.Part;
