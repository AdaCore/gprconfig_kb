------------------------------------------------------------------------------
--                                                                          --
--                             GPR TECHNOLOGY                               --
--                                                                          --
--                       Copyright (C) 2015, AdaCore                        --
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

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;    use Ada.Text_IO;

package body Gprbuild.Compilation.Process.Waiter is

   task Wait_Local;

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
         Local_Process.Decrement;
         Add_Result (Create_Local (Pid), Status);
      end loop;
   exception
      when E : others =>
         Put_Line (Exception_Information (E));
         OS_Exit (1);
   end Wait_Local;

end Gprbuild.Compilation.Process.Waiter;
