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

--  Find source dirs and source files for a project

with GPR.Tree;

private package GPR.Nmsc is

   procedure Process_Naming_Scheme
     (Tree         : Project_Tree_Ref;
      Root_Project : Project_Id;
      Node_Tree    : GPR.Tree.Project_Node_Tree_Ref;
      Flags        : Processing_Flags);
   --  Perform consistency and semantic checks on all the projects in the tree.
   --  This procedure interprets the various case statements in the project
   --  based on the current external references. After checking the validity of
   --  the naming scheme, it searches for all the source files of the project.
   --  The result of this procedure is a filled-in data structure for
   --  Project_Id which contains all the information about the project. This
   --  information is only valid while the external references are preserved.

   procedure Process_Aggregated_Projects
     (Tree      : Project_Tree_Ref;
      Project   : Project_Id;
      Node_Tree : GPR.Tree.Project_Node_Tree_Ref;
      Flags     : Processing_Flags);
   --  Assuming Project is an aggregate project, find out (based on the
   --  current external references) what are the projects it aggregates.
   --  This has to be done in phase 1 of the processing, so that we know the
   --  full list of languages required for root_project and its aggregated
   --  projects. As a result, it cannot be done as part of
   --  Process_Naming_Scheme.

end GPR.Nmsc;
