------------------------------------------------------------------------------
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--         G P R B U I L D . C O M P I L A T I O N . P R O C E S S          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2012, Free Software Foundation, Inc.            --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

--  This package abstract out a process which can be either local or remote.
--  The communication with the remote instances are done through sockets.

with GNAT.OS_Lib;

package Gprbuild.Compilation.Process is

   type Id is private;

   Invalid_Process : constant Id;

   procedure Wait (Process : out Id; Status : out Boolean);
   --  Wait for a process to terminate, returns the process id and the status

   function Run
     (Executable  : String;
      Options     : GNAT.OS_Lib.Argument_List;
      Output_File : String := "";
      Err_To_Out  : Boolean := False;
      Force_Local : Boolean := False) return Id;
   --  Run Executable with the given options locally

   function Get_Maximum_Processes return Positive;
   --  The maximum number of simultaneous compilation supported

   --  For the hash table of jobs

   type Header_Num is range 0 .. 2047;

   function Hash (Process : Id) return Header_Num;

private

   type Process_Kind is (Local, Remote);

   type Id (Kind : Process_Kind := Local) is record
      case Kind is
         when Local  => Pid : Process_Id;
         when Remote => null;
      end case;
   end record;

   Invalid_Process : constant Id := (Local, Pid => Invalid_Pid);

end Gprbuild.Compilation.Process;
