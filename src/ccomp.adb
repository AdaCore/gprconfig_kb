------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                C C O M P                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2010, Free Software Foundation, Inc.            --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This program is used on VMS as a front end to invoke the DEC C compiler CC

pragma Extend_System (Aux_DEC);
with Ada.Command_Line; use Ada.Command_Line;
with System;           use System;

procedure Ccomp is
   subtype Cond_Value_Type is System.Unsigned_Longword;

   type String_Access is access String;

   Output_File_Name : String_Access;

   procedure Spawn (Status : out Cond_Value_Type; Command : String;
                   Input_File : String := String'Null_Parameter;
                   Output_File : String := String'Null_Parameter);
   pragma Import (External, Spawn);
   pragma Import_Valued_Procedure
           (Spawn, "LIB$SPAWN",
            (Cond_Value_Type, String, String, String),
            (Value, Descriptor (S), Descriptor (S), Descriptor (S)));
   --  LIB$SPAWN is used to invoke the CC compiler

   procedure Stop (Status : Cond_Value_Type);
   pragma Import (External, Stop);
   pragma Import_Procedure (Stop, "LIB$STOP", Mechanism => Value);
   --  LIB$STOP is used to set the error code when the invocation of CC fails

   Success : constant Cond_Value_Type := 1;

   Command : constant String := "cc";
   Len : Natural := Command'Length;
   Status : Cond_Value_Type;

   Include_Directory : constant String := "/INCLUDE_DIRECTORY=";

   Output_File : constant String := "-o";

   procedure Add (S : in out String; Last : in out Natural; Value : String);
   --  Add string Value to string variable S, updating Last

   ---------
   -- Add --
   ---------

   procedure Add (S : in out String; Last : in out Natural; Value : String) is
   begin
      S (Last + 1 .. Last + Value'Length) := Value;
      Last := Last + Value'Length;
   end Add;

begin
   --  All arguments are given to CC in one single string, so we need to
   --  compute a maximum length for this string.

   for J in 1 .. Argument_Count loop
      Len := Len + 1 + Argument (J)'Length;
   end loop;

   Len := Len + 3;

   declare
      Command_String : String (1 .. Len);
      --  This is the command string that will be used to invoke CC

      Last_Command   : Natural := 0;

      Includes       : String (1 .. Len);
      --  As they can be only one /INCLUDE_DIRECTORY= option, we regroupe all
      --  directories in string Includes.

      Last_Include   : Natural := 0;

      Arg_Num : Natural;
   begin
      Add (Command_String, Last_Command, Command);

      Arg_Num := 0;
      while Arg_Num < Argument_Count loop
         Arg_Num := Arg_Num + 1;

         declare
            Arg : constant String := Argument (Arg_Num);
         begin
            --  If this command is /INCLUDE_DIRECTORY=, add the directory to
            --  string Includes.

            if Arg'Length > Include_Directory'Length and then
              Arg (Arg'First .. Arg'First + Include_Directory'Length - 1) =
              Include_Directory
            then
               if Last_Include = 0 then
                  Add (Includes, Last_Include, Include_Directory & "(");
               else
                  Add (Includes, Last_Include, ",");
               end if;

               Add
                 (Includes,
                  Last_Include,
                  Arg (Arg'First + Include_Directory'Length .. Arg'Last));

            --  If it is "-o", the next argument is the output file

            elsif Arg = Output_File then
               if Arg_Num < Argument_Count then
                  Arg_Num := Arg_Num + 1;
                  Output_File_Name := new String'(Argument (Arg_Num));
               end if;

            --  Otherwise, add argument to the command string

            else
               Add (Command_String, Last_Command, " " & Arg);
            end if;
         end;
      end loop;

      --  If there was at least one /INCLUDE_DIRECTORY= switch, add
      --  /INCLUDE_DIRECTORY= with all directories to the command string.

      if Last_Include /= 0 then
         Add
           (Command_String,
            Last_Command,
            " " & Includes (1 .. Last_Include) & ")");
      end if;

      --  Invoke CC
      declare
         Cmd : constant String (1 .. Last_Command) :=
                 Command_String (1 .. Last_Command);
      begin
         if Output_File_Name /= null then
            Spawn (Status, Cmd, Output_File => Output_File_Name.all);
         else
            Spawn (Status, Cmd);
         end if;

         if (Status mod 2) /= Success then
            Stop (Status);
         end if;
      end;
   end;
end Ccomp;
