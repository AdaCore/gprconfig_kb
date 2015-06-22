------------------------------------------------------------------------------
--                                                                          --
--                           GPR PROJECT MANAGER                            --
--                                                                          --
--          Copyright (C) 2001-2015, Free Software Foundation, Inc.         --
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

--  Parse a list of declarative items in a project file

with GPR.Tree;

private package GPR.Dect is

   procedure Parse
     (In_Tree           : GPR.Tree.Project_Node_Tree_Ref;
      Declarations      : out GPR.Project_Node_Id;
      Current_Project   : GPR.Project_Node_Id;
      Extends           : GPR.Project_Node_Id;
      Packages_To_Check : String_List_Access;
      Is_Config_File    : Boolean;
      Flags             : Processing_Flags);
   --  Parse project declarative items
   --
   --  In_Tree is the project node tree
   --
   --  Declarations is the resulting project node
   --
   --  Current_Project is the project node of the project for which the
   --  declarative items are parsed.
   --
   --  Extends is the project node of the project that project Current_Project
   --  extends. If project Current-Project does not extend any project,
   --  Extends has the value Empty_Node.
   --
   --  Packages_To_Check is the list of packages that needs to be checked.
   --  For legal packages declared in project Current_Project that are not in
   --  Packages_To_Check, only the syntax of the declarations are checked, not
   --  the attribute names and kinds.
   --
   --  Is_Config_File should be set to True if the project represents a config
   --  file (.cgpr) since some specific checks apply.

end GPR.Dect;
