------------------------------------------------------------------------------
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--         G P R B U I L D . C O M P I L A T I O N . P R O C E S S          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2012-2013, Free Software Foundation, Inc.          --
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

with Ada.Exceptions; use Ada.Exceptions;

with Output; use Output;

with Gprbuild.Compilation.Result;
with Gprbuild.Compilation.Slave;
with Gpr_Util;                    use Gpr_Util;

package body Gprbuild.Compilation.Process is

   use Ada;

   task type Wait_Local;
   type Wait_Local_Ref is access Wait_Local;

   WL            : Wait_Local_Ref;
   Local_Process : Shared_Counter;

   ------------------
   -- Create_Local --
   ------------------

   function Create_Local (Pid : Process_Id) return Id is
   begin
      return Id'(Local, Pid);
   end Create_Local;

   -------------------
   -- Create_Remote --
   -------------------

   function Create_Remote (Pid : Integer) return Id is
   begin
      return Id'(Remote, Pid);
   end Create_Remote;

   ---------------------------
   -- Get_Maximum_Processes --
   ---------------------------

   function Get_Maximum_Processes return Positive is
   begin
      return Opt.Maximum_Processes + Slave.Get_Max_Processes;
   end Get_Maximum_Processes;

   ----------
   -- Hash --
   ----------

   function Hash (Process : Id) return Header_Num is
      Modulo : constant Integer := Integer (Header_Num'Last) + 1;
      Pid    : Integer;
   begin
      if Process.Kind = Local then
         Pid := Pid_To_Integer (Process.Pid);
      else
         Pid := Process.R_Pid;
      end if;

      return Header_Num (Pid mod Modulo);
   end Hash;

   ---------
   -- Run --
   ---------

   function Run
     (Executable  : String;
      Options     : GNAT.OS_Lib.Argument_List;
      Language    : String := "";
      Dep_Name    : String := "";
      Output_File : String := "";
      Err_To_Out  : Boolean := False;
      Force_Local : Boolean := False) return Id is
   begin
      --  Initialize the task waiting for local process only in distributed
      --  mode. In standard mode, the process are waited for in the
      --  Compilation.Result.Wait procedure.

      if Distributed_Mode and then WL = null then
         WL := new Wait_Local;
      end if;

      --  Run locally first, then send jobs to remote slaves. Note that to
      --  build remotely we need an output file and a language, if one of
      --  this requirement is not fulfilled we just run the process locally.

      if Force_Local
        or else not Distributed_Mode
        or else Local_Process.Count < Opt.Maximum_Processes
        or else Output_File /= ""
        or else Language = ""
      then
         Run_Local : declare
            P : Id (Local);
         begin
            if Output_File = "" then
               P.Pid := Non_Blocking_Spawn (Executable, Options);
            else
               P.Pid := Non_Blocking_Spawn
                 (Executable, Options, Output_File, Err_To_Out);
            end if;

            Local_Process.Increment;

            return P;
         end Run_Local;

      else
         return Slave.Run (Language, Options, Dep_Name);
      end if;
   end Run;

   ----------------
   -- Wait_Local --
   ----------------

   task body Wait_Local is
      Pid    : Process_Id;
      Status : Boolean;
   begin
      loop
         Local_Process.Wait_Non_Zero;

         Wait_Process (Pid, Status);
         Result.Add (Id'(Local, Pid), Status);
         Local_Process.Decrement;
      end loop;
   exception
      when E : others =>
         Write_Line (Exception_Information (E));
         OS_Exit (1);
   end Wait_Local;

end Gprbuild.Compilation.Process;
