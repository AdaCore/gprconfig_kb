------------------------------------------------------------------------------
--                                                                          --
--                           GPR PROJECT MANAGER                            --
--                                                                          --
--          Copyright (C) 2012-2018, Free Software Foundation, Inc.         --
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

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

with GNAT.Sockets;

with GPR.Compilation.Protocol;
with GPR.Compilation.Sync;
with GPR.Util;                 use GPR.Util;

package GPR.Compilation.Slave is

   use Ada;
   use Ada.Strings.Unbounded;

   use GPR.Compilation;
   use GPR.Compilation.Protocol;

   procedure Record_Slaves (Option : String);
   --  Record the slaves as passed on the command line

   procedure Register_Remote_Slaves
     (Tree    : Project_Tree_Ref;
      Project : Project_Id);
   --  Register the slaves describes in Build_Slaves attribute of project's
   --  Remote package. This routine also initialize the slaves sources. This
   --  routine must be called before any other in this unit.

   function Channel (Host : String) return Protocol.Communication_Channel;
   --  Returns the communication channel for the given host. Returns No_Channel
   --  if host has not been registered.

   procedure Clean_Up_Remote_Slaves
     (Tree    : Project_Tree_Ref;
      Project : Project_Id);
   --  Send a clean-up request to all remote slaves. The slaves are then asked
   --  to remove all the sources and build artifacts for the given project.

   function Run
     (Project  : Project_Id;
      Language : String;
      Options  : String_Vectors.Vector;
      Obj_Name : String;
      Dep_Name : String := "";
      Env      : String := "") return GPR.Compilation.Id;
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

   --  ???????????????????????????????????????????????????????????????????
   --  ??? following routines/types are exposed here to be shared with
   --  ??? LibGPR2. Should be moved into LibGPR2 body when LibGPR will
   --  ??? be discontinued.
   --  ???????????????????????????????????????????????????????????????????

   Root_Dir     : Unbounded_String;
   --  Root directory from where the sources are to be synchronized with the
   --  slaves. This is by default the directory containing the main project
   --  file. The value is changed with the Root_Dir attribute value of the
   --  project file's Remote package.

   type Slave_Data is record
      Host : Unbounded_String;
      Port : GNAT.Sockets.Port_Type;
   end record;

   No_Slave_Data : constant Slave_Data :=
                     (Port => GNAT.Sockets.Port_Type'Last, others => <>);

   package Slaves_N is new Containers.Vectors (Positive, Slave_Data);

   Slaves_Data : Slaves_N.Vector;

   procedure Register_Remote_Slave
     (S_Data                     : Slave_Data;
      Project_Name               : String;
      Excluded_Patterns          : Sync.Str_Vect.Vector;
      Included_Patterns          : Sync.Str_Vect.Vector;
      Included_Artifact_Patterns : Sync.Str_Vect.Vector;
      Synchronize                : Boolean);
   --  Register a slave living on Host for the given project name. User is
   --  used when calling rsync, it is the remote machine user name, if empty
   --  the local user name is used.

   procedure Start_Waiting_Task;
   --  Start the remote waiting task if needed

end GPR.Compilation.Slave;
