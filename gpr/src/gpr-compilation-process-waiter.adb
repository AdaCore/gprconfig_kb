------------------------------------------------------------------------------
--                                                                          --
--                           GPR PROJECT MANAGER                            --
--                                                                          --
--          Copyright (C) 2015-2017, Free Software Foundation, Inc.         --
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

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;    use Ada.Text_IO;

package body GPR.Compilation.Process.Waiter is

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

end GPR.Compilation.Process.Waiter;
