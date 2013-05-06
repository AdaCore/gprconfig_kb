------------------------------------------------------------------------------
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                 G P R B U I L D . C O M P I L A T I O N                  --
--                                                                          --
--                                 S p e c                                  --
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

--  This is the root package for the compilation support. It handles the local
--  and distributed compilation modes.

package Gprbuild.Compilation is

   Default_Port : constant := 8484;

   --  A simple concurrent counter type

   protected type Shared_Counter is

      function Count return Natural;
      --  Returns the current counter value

      procedure Increment;
      --  Increment by one

      procedure Decrement;
      --  Decrement by one

      procedure Reset;
      --  Reset counter to 0

      entry Wait_Non_Zero;
      --  Returns when the counter is above zero

      procedure Set_Threshold (Value : Positive);
      --  Record a value that will act as a barrier to let a client enter

      entry Wait_Less_Threshold;
      --  Let a caller enter only if the current counter number is less than
      --  the specified threshold.

   private
      Threshold : Positive := 1;
      Counter   : Natural := 0;
   end Shared_Counter;

end Gprbuild.Compilation;
