------------------------------------------------------------------------------
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--          G P R B U I L D . C O M P I L A T I O N . R E S U L T           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2012-2014, Free Software Foundation, Inc.          --
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

with Ada.Containers.Doubly_Linked_Lists;

with Gpr_Util; use Gpr_Util;

package body Gprbuild.Compilation.Result is

   use Ada;
   use type Containers.Count_Type;

   type Process_Data is record
      Process : Id;
      Status  : Boolean;
   end record;

   package Endded_Process is new Containers.Doubly_Linked_Lists (Process_Data);

   protected Results is
      procedure Add (Result : Process_Data);
      entry Get (Result : out Process_Data);
   private
      List : Endded_Process.List;
   end Results;

   ---------
   -- Add --
   ---------

   procedure Add (Process : Id; Status : Boolean; Slave : String := "") is
   begin
      Results.Add (Process_Data'(Process, Status));

      --  For a compilation failure records the slave to be able to report it

      if not Status and then Slave /= "" then
         Record_Remote_Failure (Process, Slave);
      end if;
   end Add;

   -------------
   -- Results --
   -------------

   protected body Results is

      ---------
      -- Add --
      ---------

      procedure Add (Result : Process_Data) is
      begin
         List.Append (Result);
      end Add;

      ---------
      -- Get --
      ---------

      entry Get (Result : out Process_Data) when List.Length /= 0 is
      begin
         Result := List.First_Element;
         List.Delete_First;
      end Get;

   end Results;

   ----------
   -- Wait --
   ----------

   procedure Wait (Process : out Id; Status : out Boolean) is
      Data : Process_Data;
      Pid  : Process_Id;
   begin
      --  In distributed mode we wait for a result to be available into the
      --  shared Results list. This list is filed with results from local
      --  process (see Compilation.Process.Wait_Local) and also with the
      --  remotes ones (see Compilation.Slave.Wait_Remote).

      if Distributed_Mode then
         Results.Get (Data);
         Process := Data.Process;
         Status := Data.Status;

      else
         --  In non distributed mode just wait for a compilation to terminate

         Wait_Process (Pid, Status);
         Process := Create_Local (Pid);
      end if;
   end Wait;

end Gprbuild.Compilation.Result;
