------------------------------------------------------------------------------
--                                                                          --
--                           GPR PROJECT MANAGER                            --
--                                                                          --
--          Copyright (C) 1992-2015, Free Software Foundation, Inc.         --
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

with GPR.Names; use GPR.Names;
with GPR.Osint; use GPR.Osint;

package GPR.Scans is

   type Token_Type is
     (Tok_Integer_Literal,
      Tok_Real_Literal,
      Tok_String_Literal,
      Tok_Char_Literal,
      Tok_Operator_Symbol,
      Tok_Identifier,
      Tok_Double_Asterisk, -- **
      Tok_Ampersand,       -- &
      Tok_Minus,           -- -
      Tok_Plus,            -- +
      Tok_Asterisk,        -- *
      Tok_Mod,
      Tok_Rem,
      Tok_Slash,           -- /
      Tok_New,
      Tok_Abs,
      Tok_Others,
      Tok_Null,
      Tok_Raise,
      Tok_Dot,             -- .
      Tok_Apostrophe,      -- '
      Tok_Left_Paren,      -- (
      Tok_Delta,
      Tok_Digits,
      Tok_Range,
      Tok_Right_Paren,     -- )
      Tok_Comma,           -- ,
      Tok_And,
      Tok_Or,
      Tok_Xor,
      Tok_Less,            -- <
      Tok_Equal,           -- =
      Tok_Greater,         -- >
      Tok_Not_Equal,       -- /=
      Tok_Greater_Equal,   -- >=
      Tok_Less_Equal,      -- <=
      Tok_In,
      Tok_Not,
      Tok_Box,             -- <>
      Tok_Colon_Equal,     -- :=
      Tok_Colon,           -- :
      Tok_Greater_Greater, -- >>
      Tok_Abstract,
      Tok_Access,
      Tok_Aliased,
      Tok_All,
      Tok_Array,
      Tok_At,
      Tok_Body,
      Tok_Constant,
      Tok_Do,
      Tok_Is,
      Tok_Interface,
      Tok_Limited,
      Tok_Of,
      Tok_Out,
      Tok_Record,
      Tok_Renames,
      Tok_Reverse,
      Tok_Some,
      Tok_Tagged,
      Tok_Then,
      Tok_Less_Less,       -- <<
      Tok_Abort,
      Tok_Accept,
      Tok_Case,
      Tok_Delay,
      Tok_Else,
      Tok_Elsif,
      Tok_End,
      Tok_Exception,
      Tok_Exit,
      Tok_Goto,
      Tok_If,
      Tok_Pragma,
      Tok_Requeue,
      Tok_Return,
      Tok_Select,
      Tok_Terminate,
      Tok_Until,
      Tok_When,
      Tok_Begin,
      Tok_Declare,
      Tok_For,
      Tok_Loop,
      Tok_While,
      Tok_Entry,
      Tok_Protected,
      Tok_Task,
      Tok_Type,
      Tok_Subtype,
      Tok_Overriding,
      Tok_Synchronized,
      Tok_Use,
      Tok_Function,
      Tok_Generic,
      Tok_Package,
      Tok_Procedure,
      Tok_Private,
      Tok_With,
      Tok_Separate,
      Tok_EOF,
      Tok_Semicolon,
      Tok_Arrow,           -- =>
      Tok_Vertical_Bar,    -- |
      Tok_Dot_Dot,         -- ..
      Tok_Project,
      Tok_Extends,
      Tok_External,
      Tok_External_As_List,
      Tok_Comment,
      Tok_End_Of_Line,
      Tok_Special,
      Tok_SPARK_Hide,
      No_Token);
   --  No_Token is used for initializing Token values to indicate that no value
   --  has been set yet.

   subtype Token_Class_Cunit is
     Token_Type range Tok_Function .. Tok_Separate;
   --  Tokens which can begin a compilation unit

   subtype Token_Class_Literal is
     Token_Type range Tok_Integer_Literal .. Tok_Operator_Symbol;
   --  Literal

   type Token_Flag_Array is array (Token_Type) of Boolean;
   Is_Reserved_Keyword : constant Token_Flag_Array :=
     Token_Flag_Array'
       (Tok_Mod      .. Tok_Rem      => True,
        Tok_New      .. Tok_Null     => True,
        Tok_Delta    .. Tok_Range    => True,
        Tok_And      .. Tok_Xor      => True,
        Tok_In       .. Tok_Not      => True,
        Tok_Abstract .. Tok_Then     => True,
        Tok_Abort    .. Tok_Separate => True,
        others                       => False);
   --  Flag array used to test for reserved word

   Special_Character : Character;
   --  Valid only when Token = Tok_Special. Returns one of the characters
   --  '#', '$', '?', '@', '`', '\', '^', '~', or '_'.

   --------------------------
   -- Scan State Variables --
   --------------------------

   --  Note: these variables can only be referenced during the parsing of a
   --  file. Reference to any of them from Sem or the expander is wrong.

   --  These variables are initialized as required by Scn.Initialize_Scanner,
   --  and should not be referenced before such a call. However, there are
   --  situations in which these variables are saved and restored, and this
   --  may happen before the first Initialize_Scanner call, resulting in the
   --  assignment of invalid values. To avoid this, and allow building with
   --  the -gnatVa switch, we initialize some variables to known valid values.

   Scan_Ptr : Source_Ptr := No_Location; -- init for -gnatVa
   --  Current scan pointer location. After a call to Scan, this points
   --  just past the end of the token just scanned.

   Token : Token_Type := No_Token; -- init for -gnatVa
   --  Type of current token

   Token_Ptr : Source_Ptr := No_Location; -- init for -gnatVa
   --  Pointer to first character of current token

   Current_Line_Start : Source_Ptr := No_Location; -- init for -gnatVa
   --  Pointer to first character of line containing current token

   Start_Column : Column_Number := No_Column_Number; -- init for -gnatVa
   --  Starting column number (zero origin) of the first non-blank character on
   --  the line containing the current token. This is used for error recovery
   --  circuits which depend on looking at the column line up.

   Type_Token_Location : Source_Ptr := No_Location; -- init for -gnatVa
   --  Within a type declaration, gives the location of the TYPE keyword that
   --  opened the type declaration. Used in checking the end column of a record
   --  declaration, which can line up either with the TYPE keyword, or with the
   --  start of the line containing the RECORD keyword.

   Checksum : Word := 0; -- init for -gnatVa
   --  Used to accumulate a CRC representing the tokens in the source file
   --  being compiled. This CRC includes only program tokens, and excludes
   --  comments.

   First_Non_Blank_Location : Source_Ptr := No_Location; -- init for -gnatVa
   --  Location of first non-blank character on the line containing the current
   --  token (i.e. the location of the character whose column number is stored
   --  in Start_Column).

   Token_Node : Node_Id := Empty_Node;
   --  Node table Id for the current token. This is set only if the current
   --  token is one for which the scanner constructs a node (i.e. it is
   --  an identifier, operator symbol, or literal). For other token types,
   --  Token_Node is undefined.

   Token_Name : Name_Id := No_Name;
   --  For identifiers, this is set to the Name_Id of the identifier scanned.
   --  For all other tokens, Token_Name is set to Error_Name. Note that it
   --  would be possible for the caller to extract this information from
   --  Token_Node. We set Token_Name separately for two reasons. First it
   --  allows a quicker test for a specific identifier. Second, it allows a
   --  version of the parser to be built that does not build tree nodes,
   --  usable as a syntax checker.

   Prev_Token : Token_Type := No_Token;
   --  Type of previous token

   Prev_Token_Ptr : Source_Ptr;
   --  Pointer to first character of previous token

   Comment_Id : Name_Id := No_Name;
   --  Valid only when Token = Tok_Comment. Store the string that follows the
   --  "--" of a comment when scanning project files.

   Character_Code : Char_Code;
   --  Valid only when Token is Tok_Char_Literal. Contains the value of the
   --  scanned literal.

   Int_Literal_Value : Int;
   --  Valid only when Token = Tok_Integer_Literal, contains the value of the
   --  scanned literal.

   --------------------------------------------------------
   -- Procedures for Saving and Restoring the Scan State --
   --------------------------------------------------------

   --  The following procedures can be used to save and restore the entire
   --  scan state. They are used in cases where it is necessary to backup
   --  the scan during the parse.

   type Saved_Scan_State is private;
   --  Used for saving and restoring the scan state

   procedure Save_Scan_State (Saved_State : out Saved_Scan_State);
   --  pragma Inline (Save_Scan_State); Saves the current scan state for
   --  possible later restoration. Note that there is no harm in saving
   --  the state and then never restoring it.

   procedure Restore_Scan_State (Saved_State : Saved_Scan_State);
   --  pragma Inline (Restore_Scan_State);
   --  Restores a scan state saved by a call to Save_Scan_State.
   --  The saved scan state must refer to the current source file.

private
   type Saved_Scan_State is record
      Save_Scan_Ptr                 : Source_Ptr;
      Save_Token                    : Token_Type;
      Save_Token_Ptr                : Source_Ptr;
      Save_Current_Line_Start       : Source_Ptr;
      Save_Start_Column             : Column_Number;
      Save_Checksum                 : Word;
      Save_First_Non_Blank_Location : Source_Ptr;
      Save_Token_Node               : Node_Id;
      Save_Token_Name               : Name_Id;
      Save_Prev_Token               : Token_Type;
      Save_Prev_Token_Ptr           : Source_Ptr;
   end record;

end GPR.Scans;
