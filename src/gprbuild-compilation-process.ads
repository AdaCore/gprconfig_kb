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

--  This package abstract out a process which can be either local or remote.
--  The communication with the remote instances are done through sockets.

with GNAT.OS_Lib;

package Gprbuild.Compilation.Process is

   type Id is private;

   type Remote_Id is mod 2 ** 64;
   --  Represent a remote process id, this number is unique across all slaves.
   --  Such number if created by the slaves using a slave id (unique number)
   --  and a compilation number. Bother numbers are 32bits value:
   --
   --     63                 32  31                     0
   --      |    [slave id]      |  [compilation number] |

   Invalid_Process : constant Id;

   function Create_Local (Pid : GNAT.OS_Lib.Process_Id) return Id;
   --  Returns a local process for Pid

   function Create_Remote (Pid : Remote_Id) return Id;
   --  Returns a remote process (one running on a slave) for Pid

   procedure Record_Environment
     (Project     : Project_Id;
      Language    : Name_Id;
      Name, Value : String);
   --  Record an environment variable to set when spawning a compilation. This
   --  is for example to set CPATH if needed for the compilation of C sources.

   function Run
     (Executable  : String;
      Options     : GNAT.OS_Lib.Argument_List;
      Project     : Project_Id;
      Obj_Name    : String;
      Source      : String := "";
      Language    : String := "";
      Dep_Name    : String := "";
      Output_File : String := "";
      Err_To_Out  : Boolean := False;
      Force_Local : Boolean := False) return Id;
   --  Run Executable with the given options locally or on a remote slave.
   --  Dep_File name is the name of the file that is expected to be generated
   --  if the compilation is successful. If Force_Local is set then the
   --  compilation will happen on the local machine.

   function Get_Maximum_Processes return Positive;
   --  The maximum number of simultaneous compilation supported. This is the
   --  sum of the local parallelism and the sum of of remote slaves supported
   --  processes.

   --  For the hash table of jobs

   type Header_Num is range 0 .. 2047;

   function Hash (Process : Id) return Header_Num;

   function Image (Pid : Remote_Id) return String;
   --  Returns the string representation of Pid

   procedure Record_Remote_Failure (Pid : Id; Slave : String);
   --  This is to be able to display on which slaves a specific compilation has
   --  failed.

   function Get_Slave_For (Pid : Id) return String;
   --  Returns the slave for the given compilation, or the empty string if the
   --  compilation was successful or conducted locally.

   procedure Add_Result (Process : Id; Status : Boolean; Slave : String := "");
   --  Add process Id with the given status into the list of results

   procedure Wait_Result (Process : out Id; Status : out Boolean);
   --  Wait for a process to terminate (so a compilation process result) to be
   --  available and returns the process Id and the corresponding status.

private

   type Process_Kind is (Local, Remote);

   type Id (Kind : Process_Kind := Local) is record
      case Kind is
         when Local  => Pid   : Process_Id;
         when Remote => R_Pid : Remote_Id;
      end case;
   end record;

   Invalid_Process : constant Id := (Local, Pid => Invalid_Pid);

   Local_Process   : Shared_Counter;

end Gprbuild.Compilation.Process;
