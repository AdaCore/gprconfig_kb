------------------------------------------------------------------------------
--                                                                          --
--                           GPR PROJECT MANAGER                            --
--                                                                          --
--          Copyright (C) 2001-2017, Free Software Foundation, Inc.         --
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

--  This package defines the structure of the Project File tree

with GNAT.Table;

with GPR.Env;
with GPR.Ext;

package GPR.Tree is

   subtype Project_Node_Tree_Ref is GPR.Project_Node_Tree_Ref;

   -----------------
   -- Environment --
   -----------------

   --  The following record contains the context in which projects are parsed
   --  and processed (finding importing project, resolving external values,..).

   type Environment is record
      External : GPR.Ext.External_References;
      --  External references are stored in this hash table (and manipulated
      --  through subprograms in prj-ext.ads). External references are
      --  project-tree specific so that one can load the same tree twice but
      --  have two views of it, for instance.

      Project_Path : aliased GPR.Env.Project_Search_Path;
      --  The project path is tree specific, since we might want to load
      --  simultaneously multiple projects, each with its own search path, in
      --  particular when using different compilers with different default
      --  search directories.

      Flags : Processing_Flags;
      --  Configure errors and warnings
   end record;

   procedure Initialize
     (Self  : out Environment;
      Flags : Processing_Flags);
   --  Initialize a new environment

   procedure Initialize_And_Copy
     (Self      : out Environment;
      Copy_From : Environment);
   --  Initialize a new environment, copying its values from Copy_From

   procedure Free (Self : in out Environment);
   --  Free the memory used by Self

   procedure Override_Flags
     (Self : in out Environment; Flags : Processing_Flags);
   --  Override the subprogram called in case there are parsing errors. This
   --  is needed in applications that do their own error handling, since the
   --  error handler is likely to be a local subprogram in this case (which
   --  can't be stored when the flags are created).

   function Present (Node : Project_Node_Id) return Boolean;
   pragma Inline (Present);
   --  Return True if Node /= Empty_Node

   function No (Node : Project_Node_Id) return Boolean;
   pragma Inline (No);
   --  Return True if Node = Empty_Node

   procedure Initialize (Tree : Project_Node_Tree_Ref);
   --  Initialize the Project File tree: empty the Project_Nodes table
   --  and reset the Projects_Htable.

   function Default_Project_Node
     (In_Tree       : Project_Node_Tree_Ref;
      Of_Kind       : Project_Node_Kind;
      And_Expr_Kind : Variable_Kind := Undefined) return Project_Node_Id;
   --  Returns a Project_Node_Record with the specified Kind and Expr_Kind. All
   --  the other components have default nil values.
   --  To create a node for a project itself, see Create_Project below instead

   function Hash (N : Project_Node_Id) return Header_Num;
   --  Used for hash tables where the key is a Project_Node_Id

   function Imported_Or_Extended_Project_Of
     (Project   : Project_Node_Id;
      In_Tree   : Project_Node_Tree_Ref;
      With_Name : Name_Id) return Project_Node_Id;
   --  Return the node of a project imported or extended by project Project and
   --  whose name is With_Name. Return Empty_Node if there is no such project.

   --------------
   -- Comments --
   --------------

   type Comment_State is private;
   --  A type to store the values of several global variables related to
   --  comments.

   procedure Save (S : out Comment_State);
   --  Save in variable S the comment state. Called before scanning a new
   --  project file.

   procedure Restore_And_Free (S : in out Comment_State);
   --  Restore the comment state to a previously saved value. Called after
   --  scanning a project file. Frees the memory occupied by S

   procedure Reset_State;
   --  Set the comment state to its initial value. Called before scanning a
   --  new project file.

   function There_Are_Unkept_Comments return Boolean;
   --  Indicates that some of the comments in a project file could not be
   --  stored in the parse tree.

   procedure Set_Previous_Line_Node (To : Project_Node_Id);
   --  Indicate the node on the previous line. If there are comments
   --  immediately following this line, then they should be associated with
   --  this node.

   procedure Set_Previous_End_Node (To : Project_Node_Id);
   --  Indicate that on the previous line the "end" belongs to node To.
   --  If there are comments immediately following this "end" line, they
   --  should be associated with this node.

   procedure Set_End_Of_Line (To : Project_Node_Id);
   --  Indicate the node on the current line. If there is an end of line
   --  comment, then it should be associated with this node.

   procedure Set_Next_End_Node (To : Project_Node_Id);
   --  Put node To on the top of the end node stack. When an END line is found
   --  with this node on the top of the end node stack, the comments, if any,
   --  immediately preceding this "end" line will be associated with this node.

   procedure Remove_Next_End_Node;
   --  Remove the top of the end node stack

   ------------------------
   -- Comment Processing --
   ------------------------

   type Comment_Data is record
      Value                     : Name_Id := No_Name;
      Follows_Empty_Line        : Boolean := False;
      Is_Followed_By_Empty_Line : Boolean := False;
   end record;
   --  Component type for Comments Table below

   package Comments is new GNAT.Table
     (Table_Component_Type => Comment_Data,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 10,
      Table_Increment      => 100);
   --  A table to store the comments that may be stored is the tree

   procedure Scan (In_Tree : Project_Node_Tree_Ref);
   --  Scan the tokens and accumulate comments

   type Comment_Location is
     (Before, After, Before_End, After_End, End_Of_Line);
   --  Used in call to Add_Comments below

   procedure Add_Comments
     (To      : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      Where   : Comment_Location);
   --  Add comments to this node

   ----------------------
   -- Access Functions --
   ----------------------

   --  The following query functions are part of the abstract interface
   --  of the Project File tree. They provide access to fields of a project.

   --  The access functions should be called only with valid arguments.
   --  For each function the condition of validity is specified. If an access
   --  function is called with invalid arguments, then exception
   --  Assertion_Error is raised if assertions are enabled, otherwise the
   --  behaviour is not defined and may result in a crash.

   function Name_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Name_Id;
   pragma Inline (Name_Of);
   --  Valid for all non empty nodes. May return No_Name for nodes that have
   --  no names.

   function Display_Name_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Name_Id;
   pragma Inline (Display_Name_Of);
   --  Valid only for N_Project node. Returns the display name of the project.

   function Kind_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Kind;
   pragma Inline (Kind_Of);
   --  Valid for all non empty nodes

   function Location_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Source_Ptr;
   pragma Inline (Location_Of);
   --  Valid for all non empty nodes

   function First_Comment_After
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   --  Valid only for N_Comment_Zones nodes

   function First_Comment_After_End
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   --  Valid only for N_Comment_Zones nodes

   function First_Comment_Before
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   --  Valid only for N_Comment_Zones nodes

   function First_Comment_Before_End
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   --  Valid only for N_Comment_Zones nodes

   function Next_Comment
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   --  Valid only for N_Comment nodes

   function End_Of_Line_Comment
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Name_Id;
   --  Valid only for non empty nodes

   function Follows_Empty_Line
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Boolean;
   --  Valid only for N_Comment nodes

   function Is_Followed_By_Empty_Line
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Boolean;
   --  Valid only for N_Comment nodes

   function Parent_Project_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   pragma Inline (Parent_Project_Of);
   --  Valid only for N_Project nodes

   function Project_File_Includes_Unkept_Comments
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Boolean;
   --  Valid only for N_Project nodes

   function Directory_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Path_Name_Type;
   pragma Inline (Directory_Of);
   --  Returns the directory that contains the project file. This always ends
   --  with a directory separator. Only valid for N_Project nodes.

   function Expression_Kind_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Variable_Kind;
   pragma Inline (Expression_Kind_Of);
   --  Only valid for N_Literal_String, N_Attribute_Declaration,
   --  N_Variable_Declaration, N_Typed_Variable_Declaration, N_Expression,
   --  N_Term, N_Variable_Reference, N_Attribute_Reference nodes or
   --  N_External_Value.

   function Is_Extending_All
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Boolean;
   pragma Inline (Is_Extending_All);
   --  Only valid for N_Project and N_With_Clause

   function Is_Not_Last_In_List
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Boolean;
   pragma Inline (Is_Not_Last_In_List);
   --  Only valid for N_With_Clause

   function First_Variable_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Variable_Node_Id;
   pragma Inline (First_Variable_Of);
   --  Only valid for N_Project or N_Package_Declaration nodes

   function First_Package_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Package_Declaration_Id;
   pragma Inline (First_Package_Of);
   --  Only valid for N_Project nodes

   function Package_Id_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Package_Node_Id;
   pragma Inline (Package_Id_Of);
   --  Only valid for N_Package_Declaration nodes

   function Path_Name_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Path_Name_Type;
   pragma Inline (Path_Name_Of);
   --  Only valid for N_Project and N_With_Clause nodes

   function String_Value_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Name_Id;
   pragma Inline (String_Value_Of);
   --  Only valid for N_With_Clause, N_Literal_String nodes or N_Comment.
   --  For a N_With_Clause created automatically for a virtual extending
   --  project, No_Name is returned.

   function Source_Index_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Int;
   pragma Inline (Source_Index_Of);
   --  Only valid for N_Literal_String and N_Attribute_Declaration nodes

   function First_With_Clause_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   pragma Inline (First_With_Clause_Of);
   --  Only valid for N_Project nodes

   function Project_Declaration_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   pragma Inline (Project_Declaration_Of);
   --  Only valid for N_Project nodes

   function Project_Qualifier_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Qualifier;
   pragma Inline (Project_Qualifier_Of);
   --  Only valid for N_Project nodes

   function Extending_Project_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   pragma Inline (Extending_Project_Of);
   --  Only valid for N_Project_Declaration nodes

   function First_String_Type_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   pragma Inline (First_String_Type_Of);
   --  Only valid for N_Project nodes

   function Extended_Project_Path_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Path_Name_Type;
   pragma Inline (Extended_Project_Path_Of);
   --  Only valid for N_With_Clause nodes

   function Project_Node_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   pragma Inline (Project_Node_Of);
   --  Only valid for N_With_Clause, N_Variable_Reference,
   --  N_Attribute_Reference, N_String_Type_Declaration and
   --  N_Typed_Variable_Declaration nodes.

   function Non_Limited_Project_Node_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   pragma Inline (Non_Limited_Project_Node_Of);
   --  Only valid for N_With_Clause nodes. Returns Empty_Node for limited
   --  imported project files, otherwise returns the same result as
   --  Project_Node_Of.

   function Next_With_Clause_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   pragma Inline (Next_With_Clause_Of);
   --  Only valid for N_With_Clause nodes

   function First_Declarative_Item_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   pragma Inline (First_Declarative_Item_Of);
   --  Only valid for N_Project_Declaration, N_Case_Item and
   --  N_Package_Declaration.

   function Extended_Project_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   pragma Inline (Extended_Project_Of);
   --  Only valid for N_Project_Declaration nodes

   function Current_Item_Node
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   pragma Inline (Current_Item_Node);
   --  Only valid for N_Declarative_Item nodes

   function Next_Declarative_Item
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   pragma Inline (Next_Declarative_Item);
   --  Only valid for N_Declarative_Item node

   function Project_Of_Renamed_Package_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   pragma Inline (Project_Of_Renamed_Package_Of);
   --  Only valid for N_Package_Declaration nodes. May return Empty_Node.

   function Next_Package_In_Project
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   pragma Inline (Next_Package_In_Project);
   --  Only valid for N_Package_Declaration nodes

   function First_Literal_String
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   pragma Inline (First_Literal_String);
   --  Only valid for N_String_Type_Declaration nodes

   function Next_String_Type
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   pragma Inline (Next_String_Type);
   --  Only valid for N_String_Type_Declaration nodes

   function Next_Literal_String
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   pragma Inline (Next_Literal_String);
   --  Only valid for N_Literal_String nodes

   function Expression_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   pragma Inline (Expression_Of);
   --  Only valid for N_Attribute_Declaration, N_Typed_Variable_Declaration
   --  or N_Variable_Declaration nodes

   function Associative_Project_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref)
      return  Project_Node_Id;
   pragma Inline (Associative_Project_Of);
   --  Only valid for N_Attribute_Declaration nodes

   function Associative_Package_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref)
      return  Project_Node_Id;
   pragma Inline (Associative_Package_Of);
   --  Only valid for N_Attribute_Declaration nodes

   function Value_Is_Valid
     (For_Typed_Variable : Project_Node_Id;
      In_Tree            : Project_Node_Tree_Ref;
      Value              : Name_Id) return Boolean;
   pragma Inline (Value_Is_Valid);
   --  Only valid for N_Typed_Variable_Declaration. Returns True if Value is
   --  in the list of allowed strings for For_Typed_Variable. False otherwise.

   function Associative_Array_Index_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Name_Id;
   pragma Inline (Associative_Array_Index_Of);
   --  Only valid for N_Attribute_Declaration and N_Attribute_Reference.
   --  Returns No_Name for non associative array attributes.

   function Next_Variable
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   pragma Inline (Next_Variable);
   --  Only valid for N_Typed_Variable_Declaration or N_Variable_Declaration
   --  nodes.

   function First_Term
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   pragma Inline (First_Term);
   --  Only valid for N_Expression nodes

   function Next_Expression_In_List
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   pragma Inline (Next_Expression_In_List);
   --  Only valid for N_Expression nodes

   function Current_Term
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   pragma Inline (Current_Term);
   --  Only valid for N_Term nodes

   function Next_Term
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   pragma Inline (Next_Term);
   --  Only valid for N_Term nodes

   function First_Expression_In_List
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   pragma Inline (First_Expression_In_List);
   --  Only valid for N_Literal_String_List nodes

   function Package_Node_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   pragma Inline (Package_Node_Of);
   --  Only valid for N_Variable_Reference or N_Attribute_Reference nodes.
   --  May return Empty_Node.

   function Default_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Attribute_Default_Value;
   pragma Inline (Default_Of);
   --  Only valid for N_Attribute_Reference nodes

   function String_Type_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   pragma Inline (String_Type_Of);
   --  Only valid for N_Variable_Reference or N_Typed_Variable_Declaration
   --  nodes.

   function External_Reference_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   pragma Inline (External_Reference_Of);
   --  Only valid for N_External_Value nodes

   function External_Default_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   pragma Inline (External_Default_Of);
   --  Only valid for N_External_Value nodes

   function String_Argument_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   pragma Inline (String_Argument_Of);
   --  Only valid for N_Split nodes

   function Separator_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   pragma Inline (Separator_Of);
   --  Only valid for N_Split nodes

   function Case_Variable_Reference_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   pragma Inline (Case_Variable_Reference_Of);
   --  Only valid for N_Case_Construction nodes

   function First_Case_Item_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   pragma Inline (First_Case_Item_Of);
   --  Only valid for N_Case_Construction nodes

   function First_Choice_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   pragma Inline (First_Choice_Of);
   --  Only valid for N_Case_Item nodes. Return the first choice in a
   --  N_Case_Item, or Empty_Node if this is when others.

   function Is_Config_Concatenable
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Boolean;
   pragma Inline (Is_Config_Concatenable);
   --  Only valid for N_Attribute_Declaration and N_Attribute_Reference

   function Next_Case_Item
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   pragma Inline (Next_Case_Item);
   --  Only valid for N_Case_Item nodes

   function Case_Insensitive
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Boolean;
   --  Only valid for N_Attribute_Declaration and N_Attribute_Reference nodes

   -----------------------
   -- Create procedures --
   -----------------------
   --  The following procedures are used to edit a project file tree. They are
   --  slightly higher-level than the Set_* procedures below

   function Create_Project
     (In_Tree        : Project_Node_Tree_Ref;
      Name           : Name_Id;
      Full_Path      : Path_Name_Type;
      Is_Config_File : Boolean := False) return Project_Node_Id;
   --  Create a new node for a project and register it in the tree so that it
   --  can be retrieved later on.

   function Create_Package
     (Tree    : Project_Node_Tree_Ref;
      Project : Project_Node_Id;
      Pkg     : String) return Project_Node_Id;
   --  Create a new package in Project. If the package already exists, it is
   --  returned. The name of the package *must* be lower-cases, or none of its
   --  attributes will be recognized.

   function Create_Attribute
     (Tree       : Project_Node_Tree_Ref;
      Prj_Or_Pkg : Project_Node_Id;
      Name       : Name_Id;
      Index_Name : Name_Id         := No_Name;
      Kind       : Variable_Kind   := List;
      At_Index   : Integer         := 0;
      Value      : Project_Node_Id := Empty_Project_Node)
      return Project_Node_Id;
   --  Create a new attribute. The new declaration is added at the end of the
   --  declarative item list for Prj_Or_Pkg (a project or a package), but
   --  before any package declaration). No addition is done if Prj_Or_Pkg is
   --  Empty_Node. If Index_Name is not "", then if creates an attribute value
   --  for a specific index. At_Index is used for the " at <idx>" in the naming
   --  exceptions.
   --
   --  To set the value of the attribute, either provide a value for Value, or
   --  use Set_Expression_Of to set the value of the attribute (in which case
   --  Enclose_In_Expression might be useful). The former is recommended since
   --  it will more correctly handle cases where the index needs to be set on
   --  the expression rather than on the index of the attribute (i.e. 'for
   --  Specification ("unit") use "file" at 3', versus 'for Executable ("file"
   --  at 3) use "name"'). Value must be a N_String_Literal if an index will be
   --  added to it.

   function Create_Literal_String
     (Str  : Name_Id;
      Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   --  Create a literal string whose value is Str

   procedure Add_At_End
     (Tree                  : Project_Node_Tree_Ref;
      Parent                : Project_Node_Id;
      Expr                  : Project_Node_Id;
      Add_Before_First_Pkg  : Boolean := False;
      Add_Before_First_Case : Boolean := False);
   --  Add a new declarative item in the list in Parent. This new declarative
   --  item will contain Expr (unless Expr is already a declarative item, in
   --  which case it is added directly to the list). The new item is inserted
   --  at the end of the list, unless Add_Before_First_Pkg is True. In the
   --  latter case, it is added just before the first case construction is
   --  seen, or before the first package (this assumes that all packages are
   --  found at the end of the project, which isn't true in the general case
   --  unless you have normalized the project to match this description).

   function Enclose_In_Expression
     (Node : Project_Node_Id;
      Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   --  Enclose the Node inside a N_Expression node, and return this expression.
   --  This does nothing if Node is already a N_Expression.

   --------------------
   -- Set Procedures --
   --------------------

   --  The following procedures are part of the abstract interface of the
   --  Project File tree.

   --  Foe each Set_* procedure the condition of validity is specified. If an
   --  access function is called with invalid arguments, then exception
   --  Assertion_Error is raised if assertions are enabled, otherwise the
   --  behaviour is not defined and may result in a crash.

   --  These are very low-level, and manipulate the tree itself directly. You
   --  should look at the Create_* procedure instead if you want to use higher
   --  level constructs

   procedure Set_Name_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Name_Id);
   pragma Inline (Set_Name_Of);
   --  Valid for all non empty nodes

   procedure Set_Display_Name_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Name_Id);
   pragma Inline (Set_Display_Name_Of);
   --  Valid only for N_Project nodes

   procedure Set_Kind_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Kind);
   pragma Inline (Set_Kind_Of);
   --  Valid for all non empty nodes

   procedure Set_Location_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Source_Ptr);
   pragma Inline (Set_Location_Of);
   --  Valid for all non empty nodes

   procedure Set_First_Comment_After
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id);
   pragma Inline (Set_First_Comment_After);
   --  Valid only for N_Comment_Zones nodes

   procedure Set_First_Comment_After_End
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id);
   pragma Inline (Set_First_Comment_After_End);
   --  Valid only for N_Comment_Zones nodes

   procedure Set_First_Comment_Before
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id);
   pragma Inline (Set_First_Comment_Before);
   --  Valid only for N_Comment_Zones nodes

   procedure Set_First_Comment_Before_End
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id);
   pragma Inline (Set_First_Comment_Before_End);
   --  Valid only for N_Comment_Zones nodes

   procedure Set_Next_Comment
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id);
   pragma Inline (Set_Next_Comment);
   --  Valid only for N_Comment nodes

   procedure Set_Parent_Project_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id);
   --  Valid only for N_Project nodes

   procedure Set_Project_File_Includes_Unkept_Comments
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Boolean);
   --  Valid only for N_Project nodes

   procedure Set_Directory_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Path_Name_Type);
   pragma Inline (Set_Directory_Of);
   --  Valid only for N_Project nodes

   procedure Set_Expression_Kind_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Variable_Kind);
   pragma Inline (Set_Expression_Kind_Of);
   --  Only valid for N_Literal_String, N_Attribute_Declaration,
   --  N_Variable_Declaration, N_Typed_Variable_Declaration, N_Expression,
   --  N_Term, N_Variable_Reference, N_Attribute_Reference or N_External_Value
   --  nodes.

   procedure Set_Is_Extending_All
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref);
   pragma Inline (Set_Is_Extending_All);
   --  Only valid for N_Project and N_With_Clause

   procedure Set_Is_Not_Last_In_List
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref);
   pragma Inline (Set_Is_Not_Last_In_List);
   --  Only valid for N_With_Clause

   procedure Set_First_Variable_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Variable_Node_Id);
   pragma Inline (Set_First_Variable_Of);
   --  Only valid for N_Project or N_Package_Declaration nodes

   procedure Set_First_Package_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Package_Declaration_Id);
   pragma Inline (Set_First_Package_Of);
   --  Only valid for N_Project nodes

   procedure Set_Package_Id_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Package_Node_Id);
   pragma Inline (Set_Package_Id_Of);
   --  Only valid for N_Package_Declaration nodes

   procedure Set_Path_Name_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Path_Name_Type);
   pragma Inline (Set_Path_Name_Of);
   --  Only valid for N_Project and N_With_Clause nodes

   procedure Set_String_Value_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Name_Id);
   pragma Inline (Set_String_Value_Of);
   --  Only valid for N_With_Clause, N_Literal_String nodes or N_Comment.

   procedure Set_Source_Index_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Int);
   pragma Inline (Set_Source_Index_Of);
   --  Only valid for N_Literal_String and N_Attribute_Declaration nodes. For
   --  N_Literal_String, set the source index of the literal string. For
   --  N_Attribute_Declaration, set the source index of the index of the
   --  associative array element.

   procedure Set_First_With_Clause_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id);
   pragma Inline (Set_First_With_Clause_Of);
   --  Only valid for N_Project nodes

   procedure Set_Project_Declaration_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id);
   pragma Inline (Set_Project_Declaration_Of);
   --  Only valid for N_Project nodes

   procedure Set_Project_Qualifier_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Qualifier);
   pragma Inline (Set_Project_Qualifier_Of);
   --  Only valid for N_Project nodes

   procedure Set_Extending_Project_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id);
   pragma Inline (Set_Extending_Project_Of);
   --  Only valid for N_Project_Declaration nodes

   procedure Set_First_String_Type_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id);
   pragma Inline (Set_First_String_Type_Of);
   --  Only valid for N_Project nodes

   procedure Set_Extended_Project_Path_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Path_Name_Type);
   pragma Inline (Set_Extended_Project_Path_Of);
   --  Only valid for N_With_Clause nodes

   procedure Set_Project_Node_Of
     (Node         : Project_Node_Id;
      In_Tree      : Project_Node_Tree_Ref;
      To           : Project_Node_Id;
      Limited_With : Boolean := False);
   pragma Inline (Set_Project_Node_Of);
   --  Only valid for N_With_Clause, N_Variable_Reference,
   --  N_Attribute_Reference, N_String_Type_Declaration and
   --  N_Typed_Variable_Declaration nodes.

   procedure Set_Next_With_Clause_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id);
   pragma Inline (Set_Next_With_Clause_Of);
   --  Only valid for N_With_Clause nodes

   procedure Set_First_Declarative_Item_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id);
   pragma Inline (Set_First_Declarative_Item_Of);
   --  Only valid for N_Project_Declaration, N_Case_Item and
   --  N_Package_Declaration.

   procedure Set_Extended_Project_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id);
   pragma Inline (Set_Extended_Project_Of);
   --  Only valid for N_Project_Declaration nodes

   procedure Set_Current_Item_Node
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id);
   pragma Inline (Set_Current_Item_Node);
   --  Only valid for N_Declarative_Item nodes

   procedure Set_Next_Declarative_Item
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id);
   pragma Inline (Set_Next_Declarative_Item);
   --  Only valid for N_Declarative_Item node

   procedure Set_Project_Of_Renamed_Package_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id);
   pragma Inline (Set_Project_Of_Renamed_Package_Of);
   --  Only valid for N_Package_Declaration nodes.

   procedure Set_Next_Package_In_Project
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id);
   pragma Inline (Set_Next_Package_In_Project);
   --  Only valid for N_Package_Declaration nodes

   procedure Set_First_Literal_String
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id);
   pragma Inline (Set_First_Literal_String);
   --  Only valid for N_String_Type_Declaration nodes

   procedure Set_Next_String_Type
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id);
   pragma Inline (Set_Next_String_Type);
   --  Only valid for N_String_Type_Declaration nodes

   procedure Set_Next_Literal_String
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id);
   pragma Inline (Set_Next_Literal_String);
   --  Only valid for N_Literal_String nodes

   procedure Set_Expression_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id);
   pragma Inline (Set_Expression_Of);
   --  Only valid for N_Attribute_Declaration, N_Typed_Variable_Declaration
   --  or N_Variable_Declaration nodes

   procedure Set_Associative_Project_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id);
   pragma Inline (Set_Associative_Project_Of);
   --  Only valid for N_Attribute_Declaration nodes

   procedure Set_Associative_Package_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id);
   pragma Inline (Set_Associative_Package_Of);
   --  Only valid for N_Attribute_Declaration nodes

   procedure Set_Associative_Array_Index_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Name_Id);
   pragma Inline (Set_Associative_Array_Index_Of);
   --  Only valid for N_Attribute_Declaration and N_Attribute_Reference.

   procedure Set_Next_Variable
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id);
   pragma Inline (Set_Next_Variable);
   --  Only valid for N_Typed_Variable_Declaration or N_Variable_Declaration
   --  nodes.

   procedure Set_First_Term
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id);
   pragma Inline (Set_First_Term);
   --  Only valid for N_Expression nodes

   procedure Set_Next_Expression_In_List
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id);
   pragma Inline (Set_Next_Expression_In_List);
   --  Only valid for N_Expression nodes

   procedure Set_Current_Term
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id);
   pragma Inline (Set_Current_Term);
   --  Only valid for N_Term nodes

   procedure Set_Next_Term
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id);
   pragma Inline (Set_Next_Term);
   --  Only valid for N_Term nodes

   procedure Set_First_Expression_In_List
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id);
   pragma Inline (Set_First_Expression_In_List);
   --  Only valid for N_Literal_String_List nodes

   procedure Set_Package_Node_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id);
   pragma Inline (Set_Package_Node_Of);
   --  Only valid for N_Variable_Reference or N_Attribute_Reference nodes

   procedure Set_Default_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Attribute_Default_Value);
   pragma Inline (Set_Default_Of);
   --  Only valid for N_Attribute_Reference nodes

   procedure Set_String_Type_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id);
   pragma Inline (Set_String_Type_Of);
   --  Only valid for N_Variable_Reference or N_Typed_Variable_Declaration
   --  nodes.

   procedure Set_External_Reference_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id);
   pragma Inline (Set_External_Reference_Of);
   --  Only valid for N_External_Value nodes

   procedure Set_External_Default_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id);
   pragma Inline (Set_External_Default_Of);
   --  Only valid for N_External_Value nodes

   procedure Set_String_Argument_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id);
   pragma Inline (Set_String_Argument_Of);
   --  Only valid for N_Split

   procedure Set_Separator_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id);
   pragma Inline (Set_Separator_Of);
   --  Only valid for N_Split

   procedure Set_Case_Variable_Reference_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id);
   pragma Inline (Set_Case_Variable_Reference_Of);
   --  Only valid for N_Case_Construction nodes

   procedure Set_First_Case_Item_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id);
   pragma Inline (Set_First_Case_Item_Of);
   --  Only valid for N_Case_Construction nodes

   procedure Set_Is_Config_Concatenable
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Boolean);
   pragma Inline (Set_Is_Config_Concatenable);
   --  Only valid for N_Attribute_Declaration and N_Attribute_Reference

   procedure Set_First_Choice_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id);
   pragma Inline (Set_First_Choice_Of);
   --  Only valid for N_Case_Item nodes.

   procedure Set_Next_Case_Item
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id);
   pragma Inline (Set_Next_Case_Item);
   --  Only valid for N_Case_Item nodes.

   procedure Set_Case_Insensitive
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Boolean);
   --  Only valid for N_Attribute_Declaration and N_Attribute_Reference nodes

private
   type Comment_Array is array (Positive range <>) of Comment_Data;
   type Comments_Ptr is access Comment_Array;

   type Comment_State is record
      End_Of_Line_Node   : Project_Node_Id := Empty_Project_Node;
      Previous_Line_Node : Project_Node_Id := Empty_Project_Node;
      Previous_End_Node  : Project_Node_Id := Empty_Project_Node;
      Unkept_Comments    : Boolean := False;
      Comments           : Comments_Ptr := null;
   end record;

end GPR.Tree;
