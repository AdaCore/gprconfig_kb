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

with Ada.Characters.Latin_1;

package Gprbuild.Compilation is

   Default_Port : constant := 8484;

   Opts_Sep : constant Character := Ada.Characters.Latin_1.HT;

   --  Command options separator, that is the separator used for options to be
   --  passed to the executed command.

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

   private
      Counter : Natural := 0;
   end Shared_Counter;

   procedure Set_Env
     (Env   : String;
      Fail  : Boolean;
      Force : Boolean := False);
   --  Set environemnt given an Env variable containing a set of name=value
   --  separated with Opts_Sep.
   --
   --  name=value[<opts_sep>name=value]
   --
   --  If Fail is true the program will exit if the a format error is detected.
   --  If Force is set to True the environement will always be set otherwise it
   --  will be set only if not already set.

end Gprbuild.Compilation;
