------------------------------------------------------------------------------
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--         G P R B U I L D . C O M P I L A T I O N . P R O C E S S          --
--                                                                          --
--                                 B o d y                                  --
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

package body Gprbuild.Compilation.Process is

   ---------------------------
   -- Get_Maximum_Processes --
   ---------------------------

   function Get_Maximum_Processes return Positive is
   begin
      return Opt.Maximum_Processes;
   end Get_Maximum_Processes;

   ----------
   -- Hash --
   ----------

   function Hash (Process : Id) return Header_Num is
      Modulo : constant Integer := Integer (Header_Num'Last) + 1;
   begin
      return Header_Num (Pid_To_Integer (Process.Pid) mod Modulo);
   end Hash;

   ---------
   -- Run --
   ---------

   function Run
     (Executable  : String;
      Options     : GNAT.OS_Lib.Argument_List;
      Output_File : String := "";
      Err_To_Out  : Boolean := False;
      Force_Local : Boolean := False) return Id
   is
      pragma Unreferenced (Force_Local);
      P : Id (Local);
   begin
      if Output_File = "" then
         P.Pid := Non_Blocking_Spawn (Executable, Options);
      else
         P.Pid := Non_Blocking_Spawn
           (Executable, Options, Output_File, Err_To_Out);
      end if;

      return P;
   end Run;

   ----------
   -- Wait --
   ----------

   procedure Wait (Process : out Id; Status : out Boolean) is
   begin
      Wait_Process (Process.Pid, Status);
   end Wait;

end Gprbuild.Compilation.Process;
