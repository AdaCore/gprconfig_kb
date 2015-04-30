------------------------------------------------------------------------------
--                                                                          --
--                             GPR TECHNOLOGY                               --
--                                                                          --
--                     Copyright (C) 2014-2015, AdaCore                     --
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

--  Synchronize data to/from the slave. The usage is:
--
--    1. call To_Slave for every slave to be synchronise
--    2. call Wait to wait for the synchronization to be terminated

with Gprbuild.Compilation.Protocol;

private package Gprbuild.Compilation.Sync is

   procedure To_Slave
     (Channel           : Protocol.Communication_Channel;
      Project_Name      : String;
      Root_Dir          : String;
      Slave_Root_Dir    : String;
      Host              : String;
      Excluded_Patterns : Str_Vect.Vector;
      Included_Patterns : Str_Vect.Vector);
   --  Synchronize from from the build master to the slave depending on the
   --  Sync method.

   procedure Wait;
   --  Wait for all synchronization to be terminated

end Gprbuild.Compilation.Sync;
