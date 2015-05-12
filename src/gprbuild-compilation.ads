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

--  This is the root package for the compilation support. It handles the local
--  and distributed compilation modes.

with Ada.Characters.Latin_1;

private with Ada.Containers.Indefinite_Vectors;

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

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
   --  Set environment given an Env variable containing a set of name=value
   --  separated with Opts_Sep.
   --
   --  name=value[<opts_sep>name=value]
   --
   --  If Fail is true the program will exit if the a format error is detected.
   --  If Force is set to True the environment will always be set otherwise it
   --  will be set only if not already set.

   --  The set of files for a given project (associated with a synchronization
   --  job).

   type File_Data is record
      Path_Name : Ada.Strings.Unbounded.Unbounded_String;
      Timestamp : Time_Stamp_Type; -- YYYYMMDDhhmmss
   end record;

   package File_Data_Set is new Ada.Containers.Vectors (Positive, File_Data);

private

   package Str_Vect is
     new Ada.Containers.Indefinite_Vectors (Positive, String);

end Gprbuild.Compilation;
