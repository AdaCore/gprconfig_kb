------------------------------------------------------------------------------
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                 G P R B U I L D . C O M P I L A T I O N                  --
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

package body Gprbuild.Compilation is

   --------------------
   -- Shared_Counter --
   --------------------

   protected body Shared_Counter is

      -----------
      -- Count --
      -----------

      function Count return Natural is
      begin
         return Counter;
      end Count;

      ---------------
      -- Decrement --
      ---------------

      procedure Decrement is
      begin
         Counter := Counter - 1;
      end Decrement;

      ---------------
      -- Increment --
      ---------------

      procedure Increment is
      begin
         Counter := Counter + 1;
      end Increment;

      -----------
      -- Reset --
      -----------

      procedure Reset is
      begin
         Counter := 0;
      end Reset;

      -------------------
      -- Wait_Non_Zero --
      -------------------

      entry Wait_Non_Zero when Counter /= 0 is
      begin
         null;
      end Wait_Non_Zero;

   end Shared_Counter;

end Gprbuild.Compilation;
