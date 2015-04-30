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

with Gprbuild.Compilation.Process; use Gprbuild.Compilation.Process;

package Gprbuild.Compilation.Result is

   procedure Add (Process : Id; Status : Boolean; Slave : String := "");
   --  Add process Id with the given status into the list of results

   procedure Wait (Process : out Id; Status : out Boolean);
   --  Wait for a process to terminate (so a compilation process result) to be
   --  available and returns the process Id and the corresponding status.

end Gprbuild.Compilation.Result;
