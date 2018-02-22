------------------------------------------------------------------------------
--                                                                          --
--                           GPR PROJECT MANAGER                            --
--                                                                          --
--          Copyright (C) 2001-2018, Free Software Foundation, Inc.         --
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

--  This package is used to convert a project file tree (see prj-tree.ads) to
--  project file data structures (see prj.ads), taking into account the
--  environment (external references).

with GPR.Tree;  use GPR.Tree;

package GPR.Proc is

   type Tree_Loaded_Callback is access procedure
     (Node_Tree    : Project_Node_Tree_Ref;
      Tree         : Project_Tree_Ref;
      Project_Node : Project_Node_Id;
      Project      : Project_Id);
   --  Callback used after the phase 1 of the processing of each aggregated
   --  project to get access to project trees of aggregated projects.

   procedure Process_Project_Tree_Phase_1
     (In_Tree                : Project_Tree_Ref;
      Project                : out Project_Id;
      Packages_To_Check      : String_List_Access;
      Success                : out Boolean;
      From_Project_Node      : Project_Node_Id;
      From_Project_Node_Tree : Project_Node_Tree_Ref;
      Env                    : in out GPR.Tree.Environment;
      Reset_Tree             : Boolean              := True;
      On_New_Tree_Loaded     : Tree_Loaded_Callback := null);
   --  Process a project tree (ie the direct resulting of parsing a .gpr file)
   --  based on the current external references.
   --
   --  The result of this phase_1 is a partial project tree (Project) where
   --  only a few fields have been initialized (in particular the list of
   --  languages). These are the fields that are necessary to run gprconfig if
   --  needed to automatically generate a configuration file. This first phase
   --  of the processing does not require a configuration file.
   --
   --  When Reset_Tree is True, all the project data are removed from the
   --  project table before processing.
   --
   --  If specified, On_New_Tree_Loaded is called after each aggregated project
   --  has been processed succesfully.

   procedure Process_Project_Tree_Phase_2
     (In_Tree                : Project_Tree_Ref;
      Project                : Project_Id;
      Success                : out Boolean;
      From_Project_Node      : Project_Node_Id;
      From_Project_Node_Tree : Project_Node_Tree_Ref;
      Env                    : GPR.Tree.Environment);
   --  Perform the second phase of the processing, filling the rest of the
   --  project with the information extracted from the project tree. This phase
   --  requires that the configuration file has already been parsed (in fact
   --  we currently assume that the contents of the configuration file has
   --  been included in Project through Confgpr.Apply_Config_File). The
   --  parameters are the same as for phase_1, with the addition of:

   procedure Process
     (In_Tree                : Project_Tree_Ref;
      Project                : out Project_Id;
      Packages_To_Check      : String_List_Access;
      Success                : out Boolean;
      From_Project_Node      : Project_Node_Id;
      From_Project_Node_Tree : Project_Node_Tree_Ref;
      Env                    : in out GPR.Tree.Environment;
      Reset_Tree             : Boolean              := True;
      On_New_Tree_Loaded     : Tree_Loaded_Callback := null);
   --  Performs the two phases of the processing

   procedure Set_Default_Runtime_For (Language : Name_Id; Value : String);
   --  Set the default value for the runtime of Language. To be used for the
   --  value of 'Runtime(<Language>) when Runtime (<language>) is not declared.

   function Expression
     (Project                : Project_Id;
      Shared                 : Shared_Project_Tree_Data_Access;
      From_Project_Node      : Project_Node_Id;
      From_Project_Node_Tree : Project_Node_Tree_Ref;
      Env                    : GPR.Tree.Environment;
      Pkg                    : Package_Id;
      First_Term             : Project_Node_Id;
      Kind                   : Variable_Kind) return Variable_Value;
   --  From N_Expression project node From_Project_Node, compute the value
   --  of an expression and return it as a Variable_Value.

end GPR.Proc;
