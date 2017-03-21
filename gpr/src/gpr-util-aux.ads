------------------------------------------------------------------------------
--                                                                          --
--                           GPR PROJECT MANAGER                            --
--                                                                          --
--            Copyright (C) 2017, Free Software Foundation, Inc.            --
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

package GPR.Util.Aux is

   procedure Create_Response_File
     (Format            : Response_File_Format;
      Objects           : String_List;
      Other_Arguments   : String_List;
      Resp_File_Options : String_List;
      Name_1            : out Path_Name_Type;
      Name_2            : out Path_Name_Type);
   --  Create a temporary file as a response file that contains either the list
   --  of Objects in the correct Format, or for Format GCC the list of all
   --  arguments. It is the responsibility of the caller to delete this
   --  temporary file if needed.

   procedure Create_Export_Symbols_File
     (Driver_Path         : String;
      Options             : Argument_List;
      Sym_Matcher         : String;
      Format              : Export_File_Format;
      Objects             : String_List;
      Library_Symbol_File : String;
      Export_File_Name    : out Path_Name_Type);
   --  Create an export symbols file for the linker. If Library_Symbol_File is
   --  defined the symbols will be read from this file (one per line) otherwise
   --  the symbols from the listed object files will get exported from a shared
   --  libraries. All other symbols will remain local to the shared library.
   --  Driver_Path is the tool used to list the symbols from an object file.
   --  Options are the options needed by the driver. Sym_Matcher is the regular
   --  expression used to match the symbol out of the tool output. Format
   --  the the export file format to generate. Objects is the list of object
   --  files to use. Finally the generated export filename is returned in
   --  Export_File.

   function Compute_Slave_Env
     (Project : Project_Tree_Ref; Auto : Boolean) return String;
   --  Compute a slave environment based on the command line parameter and
   --  the project variables. We want the same slave environment for identical
   --  build. Data is a string that must be taken into account in the returned
   --  value.

   function Get_Slaves_Hosts
     (Project_Tree : Project_Tree_Ref;
      Arg          : String) return String;
   --  Given the actual argument "--distributed[=...]" return the coma
   --  separated list of slave hosts. This routine handle the GPR_SLAVE and
   --  GPR_SLAVES_FILE environment variables.

end GPR.Util.Aux;
