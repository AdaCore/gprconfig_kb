------------------------------------------------------------------------------
--                                                                          --
--                           GPR PROJECT MANAGER                            --
--                                                                          --
--          Copyright (C) 2003-2017, Free Software Foundation, Inc.         --
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

--  This package is used by the Project Manager to create temporary files. If
--  environment variable TMPDIR is defined and designates an absolute path,
--  temporary files are create in this directory. Otherwise, temporary files
--  are created in the current working directory.

package GPR.Tempdir is

   procedure Create_Temp_File
     (FD   : out File_Descriptor;
      Name : out Path_Name_Type);
   --  Create a temporary text file and return its file descriptor and its
   --  path name as a Name_Id. If one of the environment variables TMPDIR, TEMP
   --  or TMP is defined and its value is an absolute path, the temp file is
   --  created in the directory designated by the first of these environment
   --  variables that meet these conditions, otherwise, it is created in the
   --  current directory. If temporary file cannot be created, FD gets the
   --  value Invalid_FD and Name gets the value No_Name.

   procedure Use_Temp_Dir (Status : Boolean);
   --  Specify if the temp file should be created in the system temporary
   --  directory as specified by the corresponding environment variables. If
   --  Status is False, the temp files will be created into the current working
   --  directory.

   function Temporary_Directory_Path return String;
   --  Returns the full path of the temporary directory in use.
   --  Returns an empty string if there is no temporary directory in use,
   --  either because Use_Temp_Dir was called with Status set to False,
   --  or none of the environment variables are defined.

end GPR.Tempdir;
