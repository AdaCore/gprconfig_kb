------------------------------------------------------------------------------
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--           G P R B U I L D . C O M P I L A T I O N . S L A V E            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2014, Free Software Foundation, Inc.            --
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

--  Synchronize data to/from the slave. The usage is:
--
--    1. call To_Slave or From_Slave for every slave to be synchronise
--    2. call Wait to wait for the synchronization to be terminated

with Gprbuild.Compilation.Protocol;

private package Gprbuild.Compilation.Sync is

   procedure To_Slave
     (Sync              : Protocol.Sync_Kind;
      Project_Name      : String;
      Root_Dir          : String;
      Slave_Root_Dir    : String;
      User              : String;
      Host              : String;
      Excluded_Patterns : Str_Vect.Vector;
      Included_Patterns : Str_Vect.Vector);
   --  Synchronize from from the build master to the slave depending on the
   --  Sync method.

   procedure From_Slave
     (Sync                       : Protocol.Sync_Kind;
      Project_Name               : String;
      Root_Dir                   : String;
      Slave_Root_Dir             : String;
      User                       : String;
      Host                       : String;
      Included_Artifact_Patterns : Str_Vect.Vector);
   --  Synchronize from the slave to the build master depending on the Sync
   --  method.

   procedure Wait;
   --  Wait for all synchronization to be terminated

end Gprbuild.Compilation.Sync;
