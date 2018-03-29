------------------------------------------------------------------------------
--                                                                          --
--                           GPR PROJECT MANAGER                            --
--                                                                          --
--                      Copyright (C) 2001-2018, AdaCore                    --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Containers.Vectors;

with GNAT.OS_Lib; use GNAT.OS_Lib;
with GNAT.Regexp; use GNAT.Regexp;

with GPR;         use GPR;
with GPR.Util;    use GPR.Util;

package GPRName is

   Project_Tree : constant Project_Tree_Ref :=
                    new Project_Tree_Data (Is_Root_Tree => True);
   --  The project tree

   procedure Initialize
     (File_Path         : String;
      Preproc_Switches  : String_Vectors.Vector;
      Very_Verbose      : Boolean;
      Flags             : Processing_Flags);
   --  Start the creation or modification of a project file, for gprname.
   --
   --  File_Path is the name of a project file to create if it does not exist
   --  or to modify if it already exists.
   --
   --  Preproc_Switches is a list of switches to be used when invoking the
   --  compiler to get the name and kind of unit of a source file.
   --
   --  Very_Verbose controls the verbosity of the output, in conjunction with
   --  GPR.Opt.Verbose_Mode.

   package Regexp_Vectors is new Ada.Containers.Vectors
     (Positive, Regexp);

   subtype Regexp_List is Regexp_Vectors.Vector;

   type Foreign_Regexp is record
      Language : Name_Id;
      Pattern  : Regexp;
   end record;

   package Frgn_Regexp_Vectors is new Ada.Containers.Vectors
     (Positive, Foreign_Regexp);

   subtype Foreign_Regexp_List is Frgn_Regexp_Vectors.Vector;

   procedure Process
     (Directories       : String_Vectors.Vector;
      Name_Patterns     : Regexp_List;
      Excluded_Patterns : Regexp_List;
      Foreign_Patterns  : Foreign_Regexp_List);
   --  Look for source files in the specified directories, with the specified
   --  patterns.
   --
   --  Directories is the list of source directories where to look for sources
   --
   --  Name_Patterns is a potentially empty list of file name patterns to check
   --  for Ada Sources.
   --
   --  Excluded_Patterns is a potentially empty list of file name patterns that
   --  should not be checked for Ada or non Ada sources.
   --
   --  Foreign_Patterns is a potentially empty list of file name patterns to
   --  check for non Ada sources.
   --
   --  At least one of Name_Patterns and Foreign_Patterns is not empty
   --
   --  Note that this procedure currently assumes that it is only used
   --  by gnatname. If other processes start using it, then an additional
   --  parameter would need to be added, and call to Osint.Program_Name
   --  updated accordingly in the body.

   procedure Finalize;
   --  Write the project file indicated in a call to procedure Initialize,
   --  after one or several calls to procedure Process.

private

   RTS_Specified : String_Access := null;
   --  Used to detect multiple use of --RTS= switch

   Gcc      : constant String := "gcc";
   Gcc_Path : String_Access := null;
   --  Path of the Ada compiler

end GPRName;
