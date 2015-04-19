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

--  This package contains the input routines used for reading the
--  input source file. The actual I/O routines are in OS_Interface,
--  with this module containing only the system independent processing.

--  General Note: throughout the compiler, we use the term line or source
--  line to refer to a physical line in the source, terminated by the end of
--  physical line sequence.

--  There are two distinct concepts of line terminator in GNAT

--    A logical line terminator is what corresponds to the "end of a line" as
--    described in RM 2.2 (13). Any of the characters FF, LF, CR or VT or any
--    wide character that is a Line or Paragraph Separator acts as an end of
--    logical line in this sense, and it is essentially irrelevant whether one
--    or more appears in sequence (since if a sequence of such characters is
--    regarded as separate ends of line, then the intervening logical lines
--    are null in any case).

--    A physical line terminator is a sequence of format effectors that is
--    treated as ending a physical line. Physical lines have no Ada semantic
--    significance, but they are significant for error reporting purposes,
--    since errors are identified by line and column location.

--  In GNAT, a physical line is ended by any of the sequences LF, CR/LF, or
--  CR. LF is used in typical Unix systems, CR/LF in DOS systems, and CR
--  alone in System 7. In addition, we recognize any of these sequences in
--  any of the operating systems, for better behavior in treating foreign
--  files (e.g. a Unix file with LF terminators transferred to a DOS system).
--  Finally, wide character codes in categories Separator, Line and Separator,
--  Paragraph are considered to be physical line terminators.

with GNAT.Table;

with GPR.Scans; use GPR.Scans;
with GPR.Osint; use GPR.Osint;

package GPR.Sinput is

   function Last_Source_File return Source_File_Index;
   --  Index of last source file table entry

   function Num_Source_Files return Nat;
   --  Number of source file table entries

   procedure Initialize;
   --  Initialize internal tables

   Main_Source_File : Source_File_Index := No_Source_File;
   --  This is set to the source file index of the main unit

   function Load_File (Path : String) return Source_File_Index;
   --  Load a file into memory and Initialize the Scans state

   -----------------------------
   -- Source_File_Index_Table --
   -----------------------------

   --  The Get_Source_File_Index function is called very frequently. Earlier
   --  versions cached a single entry, but then reverted to a serial search,
   --  and this proved to be a significant source of inefficiency. We then
   --  switched to using a table with a start point followed by a serial
   --  search. Now we make sure source buffers are on a reasonable boundary
   --  (see Types.Source_Align), and we can just use a direct look up in the
   --  following table.

   --  Note that this array is pretty large, but in most operating systems
   --  it will not be allocated in physical memory unless it is actually used.

   Source_File_Index_Table :
     array (Int range 0 .. 1 + (Int'Last / Source_Align)) of Source_File_Index;

   procedure Set_Source_File_Index_Table (Xnew : Source_File_Index);
   --  Sets entries in the Source_File_Index_Table for the newly created
   --  Source_File table entry whose index is Xnew. The Source_First and
   --  Source_Last fields of this entry must be set before the call.

   function Get_Column_Number (P : Source_Ptr) return Column_Number;
   --  The ones-origin column number of the specified Source_Ptr value is
   --  determined and returned. Tab characters if present are assumed to
   --  represent the standard 1,9,17.. spacing pattern.

   function Get_Line_Number
     (P : Source_Ptr) return Line_Number;
   --  The line number of the specified source position is obtained by
   --  doing a binary search on the source positions in the lines table
   --  for the unit containing the given source position. The returned
   --  value is the line number in the source being compiled.

   function Get_Source_File_Index (S : Source_Ptr) return Source_File_Index;
   pragma Inline (Get_Source_File_Index);
   --  Return file table index of file identified by given source pointer
   --  value. This call must always succeed, since any valid source pointer
   --  value belongs to some previously loaded source file.

   function Line_Start (P : Source_Ptr) return Source_Ptr;
   --  Finds the source position of the start of the line containing the
   --  given source location.

   function Line_Start
     (L : Line_Number;
      S : Source_File_Index) return Source_Ptr;
   --  Finds the source position of the start of the given line in the
   --  given source file, using a physical line number to identify the line.

   function Num_Source_Lines (S : Source_File_Index) return Nat;
   --  Returns the number of source lines (this is equivalent to reading
   --  the value of Last_Source_Line, but returns Nat rather than a
   --  physical line number.

   function Full_File_Name    (S : Source_File_Index) return File_Name_Type;
   function Full_Ref_Name     (S : Source_File_Index) return File_Name_Type;
   function Reference_Name    (S : Source_File_Index) return File_Name_Type;
   function Source_First      (S : Source_File_Index) return Source_Ptr;
   function Source_Last       (S : Source_File_Index) return Source_Ptr;
   function Source_Text       (S : Source_File_Index) return Source_Buffer_Ptr;

   procedure Skip_Wide (S : Source_Buffer_Ptr; P : in out Source_Ptr);
   --  Similar to the above procedure, but operates on a source buffer
   --  instead of a string, with P being a Source_Ptr referencing the
   --  contents of the source buffer.

   -----------------------
   -- Checksum Handling --
   -----------------------

   --  As a source file is scanned, a checksum is computed by taking all the
   --  non-blank characters in the file, excluding comment characters, the
   --  minus-minus sequence starting a comment, and all control characters
   --  except ESC.

   --  The checksum algorithm used is the standard CRC-32 algorithm, as
   --  implemented by System.CRC32, except that we do not bother with the
   --  final XOR with all 1 bits.

   --  This algorithm ensures that the checksum includes all semantically
   --  significant aspects of the program represented by the source file,
   --  but is insensitive to layout, presence or contents of comments, wide
   --  character representation method, or casing conventions outside strings.

   --  Scans.Checksum is initialized appropriately at the start of scanning
   --  a file, and copied into the Source_Checksum field of the file table
   --  entry when the end of file is encountered.

   -----------------
   -- Global Data --
   -----------------

   Current_Source_File : Source_File_Index := No_Source_File;
   --  Source_File table index of source file currently being scanned.
   --  Initialized so that some tools (such as gprbuild) can be built with
   --  -gnatVa and pragma Initialized_Scalars without problems.

   Source : Source_Buffer_Ptr;
   --  Current source (copy of Source_File.Table (Current_Source_Unit).Source)

   Internal_Source : aliased Source_Buffer (1 .. 81);
   --  This buffer is used internally in the compiler when the lexical analyzer
   --  is used to scan a string from within the compiler. The procedure is to
   --  establish Internal_Source_Ptr as the value of Source, set the string to
   --  be scanned, appropriately terminated, in this buffer, and set Scan_Ptr
   --  to point to the start of the buffer. It is a fatal error if the scanner
   --  signals an error while scanning a token in this internal buffer.

   Internal_Source_Ptr : constant Source_Buffer_Ptr :=
                           Internal_Source'Unrestricted_Access;
   --  Pointer to internal source buffer

   procedure Clear_Source_File_Table;
   --  This procedure frees memory allocated in the Source_File table (in the
   --  private part of package Sinput). It should only be used when it is
   --  guaranteed that all source files that have been loaded so far will not
   --  be accessed before being reloaded. It is intended for tools that parse
   --  several times sources, to avoid memory leaks.

   procedure Reset_First;
   --  Indicate that the next project loaded should be considered as the first
   --  one, so that Sinput.Main_Source_File is set for this project file. This
   --  is to get the correct number of lines when error finalization is called.

   function Source_File_Is_Subunit (X : Source_File_Index) return Boolean;
   --  This function determines if a source file represents a subunit. It works
   --  by scanning for the first compilation unit token, and returning True if
   --  it is the token SEPARATE. It will return False otherwise, meaning that
   --  the file cannot possibly be a legal subunit. This function does NOT do a
   --  complete parse of the file, or build a tree. It is used in gnatmake and
   --  gprbuild to decide if a body without a spec in a project file needs to
   --  be compiled or not. Returns False if X = No_Source_File.

   type Saved_Project_Scan_State is limited private;
   --  Used to save project scan state in following two routines

   procedure Save_Project_Scan_State
     (Saved_State : out Saved_Project_Scan_State);
   pragma Inline (Save_Project_Scan_State);
   --  Save the Scans state, as well as the values of Source and
   --  Current_Source_File.

   procedure Restore_Project_Scan_State
     (Saved_State : Saved_Project_Scan_State);
   pragma Inline (Restore_Project_Scan_State);
   --  Restore the Scans state and the values of Source and
   --  Current_Source_File.

   procedure Check_For_BOM;
   --  Check if the current source starts with a BOM. Scan_Ptr needs to be at
   --  the start of the current source. If the current source starts with a
   --  recognized BOM, then some flags such as Wide_Character_Encoding_Method
   --  are set accordingly, and the Scan_Ptr on return points past this BOM.
   --  An error message is output and Unrecoverable_Error raised if a non-
   --  recognized BOM is detected. The call has no effect if no BOM is found.

   -------------------------
   -- Source_Lines Tables --
   -------------------------

   type Lines_Table_Type is
     array (Line_Number range <>) of Source_Ptr;
   --  Type used for lines table

   type Lines_Table_Ptr is access all Lines_Table_Type;
   --  Type used for pointers to line tables

   --  See earlier descriptions for meanings of public fields

   type Source_File_Record is record
      File_Name         : File_Name_Type;
      Reference_Name    : File_Name_Type;
      Debug_Source_Name : File_Name_Type;
      Full_Debug_Name   : File_Name_Type;
      Full_File_Name    : File_Name_Type;
      Full_Ref_Name     : File_Name_Type;
      Source_Text       : Source_Buffer_Ptr;
      Source_First      : Source_Ptr;
      Source_Last       : Source_Ptr;
      Source_Checksum   : Word;
      Last_Source_Line  : Line_Number;
      Time_Stamp        : Time_Stamp_Type;

      --  The following fields are for internal use only (i.e. only in the
      --  body of Sinput or its children, with no direct access by clients).

      Lines_Table : Lines_Table_Ptr;
      --  Pointer to lines table for this source. Updated as additional
      --  lines are accessed using the Skip_Line_Terminators procedure.
      --  Note: the lines table for an instantiation entry refers to the
      --  original line numbers of the template see Sinput-L for details.

      Lines_Table_Max : Line_Number;
      --  Maximum subscript values for currently allocated Lines_Table
      --  and (if present) the allocated Logical_Lines_Table. The value
      --  Max_Source_Line gives the maximum used value, this gives the
      --  maximum allocated value.

   end record;

   procedure Add_Line_Tables_Entry
     (S : in out Source_File_Record;
      P : Source_Ptr);
   --  Increment line table size by one (reallocating the lines table if
   --  needed) and set the new entry to contain the value P. Also bumps
   --  the Source_Line_Count field. If source reference pragmas are
   --  present, also increments logical lines table size by one, and
   --  sets new entry.

   -----------------------
   -- Source_File Table --
   -----------------------

   Source_File_Initial              : constant := 10;
   Source_File_Increment            : constant := 200;

   package Source_File is new GNAT.Table (
     Table_Component_Type => Source_File_Record,
     Table_Index_Type     => Source_File_Index,
     Table_Low_Bound      => 1,
     Table_Initial        => Source_File_Initial,
     Table_Increment      => Source_File_Increment);

private

   type Saved_Project_Scan_State is record
      Scan_State          : Saved_Scan_State;
      Source              : Source_Buffer_Ptr;
      Current_Source_File : Source_File_Index;
   end record;

   -----------------
   -- Subprograms --
   -----------------

--     procedure Alloc_Line_Tables
--       (S       : in out Source_File_Record;
--        New_Max : Nat);
--     --  Allocate or reallocate the lines table for the given source file so
--     --  that it can accommodate at least New_Max lines. Also allocates or
--     --  reallocates logical lines table if source ref pragmas are present.
--
--     procedure Add_Line_Tables_Entry
--       (S : in out Source_File_Record;
--        P : Source_Ptr);
--     --  Increment line table size by one (reallocating the lines table if
--     --  needed) and set the new entry to contain the value P. Also bumps
--     --  the Source_Line_Count field. If source reference pragmas are
--     --  present, also increments logical lines table size by one, and
--     --  sets new entry.
--
--     procedure Trim_Lines_Table (S : Source_File_Index);
--     --  Set lines table size for entry S in the source file table to
--     --  correspond to the current value of Num_Source_Lines, releasing
--     --  any unused storage. This is used by Sinput.L and Sinput.D.

end GPR.Sinput;
