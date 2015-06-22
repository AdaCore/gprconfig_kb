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

--  This package is the Project File Pretty Printer

--    Used to output a project file from a project file tree.
--    Used by gnatname to update or create project files.
--    Also used GPS to display project file trees.
--    Also be used for debugging tools that create project file trees.

with GPR.Tree;

package GPR.PP is

   --  The following access to procedure types are used to redirect output when
   --  calling Pretty_Print.

   type Write_Char_Ap is access procedure (C : Character);

   type Write_Eol_Ap  is access procedure;

   type Write_Str_Ap is access procedure (S : String);

   subtype Max_Length_Of_Line is Positive range 50 .. 255;

   procedure Pretty_Print
     (Project                            : GPR.Project_Node_Id;
      In_Tree                            : GPR.Tree.Project_Node_Tree_Ref;
      Increment                          : Positive       := 3;
      Eliminate_Empty_Case_Constructions : Boolean        := False;
      Minimize_Empty_Lines               : Boolean        := False;
      W_Char                             : Write_Char_Ap  := null;
      W_Eol                              : Write_Eol_Ap   := null;
      W_Str                              : Write_Str_Ap   := null;
      Backward_Compatibility             : Boolean;
      Id                                 : Project_Id     := No_Project;
      Max_Line_Length                    : Max_Length_Of_Line :=
                                             Max_Length_Of_Line'Last;
      Initial_Indent                     : Natural := 0);
   --  Output a project file, using either the default output routines, or the
   --  ones specified by W_Char, W_Eol and W_Str.
   --
   --  Increment is the number of spaces for each indentation level
   --
   --  W_Char, W_Eol and W_Str can be used to change the default output
   --  procedures. The default values force the output to Standard_Output.
   --
   --  If Eliminate_Empty_Case_Constructions is True, then case constructions
   --  and case items that do not include any declarations will not be output.
   --
   --  If Minimize_Empty_Lines is True, empty lines will be output only after
   --  the last with clause, after the line declaring the project name, after
   --  the last declarative item of the project and before each package
   --  declaration. Otherwise, more empty lines are output.
   --
   --  If Backward_Compatibility is True, then new attributes (Spec,
   --  Spec_Suffix, Body, Body_Suffix) will be replaced by obsolete ones
   --  (Specification, Specification_Suffix, Implementation,
   --  Implementation_Suffix).
   --
   --  Id is used to compute the display name of the project including its
   --  proper casing.
   --
   --  Max_Line_Length is the maximum line length in the project file
   --
   --  Initial_Indent is the initial indentation

private

   procedure Output_Statistics;
   --  This procedure can be used after one or more calls to Pretty_Print to
   --  display what Project_Node_Kinds have not been exercised by the call(s)
   --  to Pretty_Print. It is used only for testing purposes.

   procedure wpr
     (Project : GPR.Project_Node_Id;
      In_Tree : GPR.Tree.Project_Node_Tree_Ref);
   --  Wrapper for use from gdb: call Pretty_Print with default parameters

end GPR.PP;
