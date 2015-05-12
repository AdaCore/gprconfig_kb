------------------------------------------------------------------------------
--                                                                          --
--                             GPR TECHNOLOGY                               --
--                                                                          --
--                     Copyright (C) 2012-2015, AdaCore                     --
--                                                                          --
-- This is  free  software;  you can redistribute it and/or modify it under --
-- terms of the  GNU  General Public License as published by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for more details.  You should have received  a copy of the  GNU  --
-- General Public License distributed with GNAT; see file  COPYING. If not, --
-- see <http://www.gnu.org/licenses/>.                                      --
--                                                                          --
------------------------------------------------------------------------------

with GNAT.OS_Lib;

with Gprbuild.Compilation.Process; use Gprbuild.Compilation.Process;

package Gprbuild.Compilation.Slave is

   procedure Record_Slaves (Option : String);
   --  Record the slaves as passed on the command line

   procedure Register_Remote_Slaves
     (Tree    : Project_Tree_Ref;
      Project : Project_Id);
   --  Register the slaves describes in Build_Slaves attribute of project's
   --  Remote package. This routine also initialize the slaves sources. This
   --  routine must be called before any other in this unit.

   procedure Clean_Up_Remote_Slaves
     (Tree    : Project_Tree_Ref;
      Project : Project_Id);
   --  Send a clean-up request to all remote slaves. The slaves are then asked
   --  to remove all the sources and build artifacts for the given project.

   function Run
     (Project  : Project_Id;
      Language : String;
      Options  : GNAT.OS_Lib.Argument_List;
      Obj_Name : String;
      Dep_Name : String := "";
      Env      : String := "") return Id;
   --  Send a compilation job to one slave that has still some free slot. There
   --  is also free slot when this routine is called (gprbuild ensure this).

   procedure Unregister_Remote_Slaves (From_Signal : Boolean := False);
   --  Unregister all slaves, send them notification about the end of the
   --  current build. This routine must be called after the compilation phase
   --  and before the bind and link ones. It is safe to call this routine
   --  multiple times, the first call will do the clean-up, next calls are
   --  just no-op. From_Signal must be set when called from a signal, for
   --  example when calling this routine from the ctrl-c handler.

   function Get_Max_Processes return Natural;
   --  Returns the maximum number of processes supported by the compilation
   --  engine. This is the sum of the parallel local builds as specified by
   --  the -j option and all the sum of the processes supported by each slaves.

end Gprbuild.Compilation.Slave;
