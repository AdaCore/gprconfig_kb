------------------------------------------------------------------------------
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                C C O M P                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2010-2012, Free Software Foundation, Inc.          --
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

--  This program is used on VMS as a front end to invoke the DEC C compiler CC

pragma Extend_System (Aux_DEC);

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO;      use Ada.Text_IO;
with GNAT.OS_Lib;      use GNAT.OS_Lib;
with Osint;            use Osint;
with System;           use System;

procedure Ccomp is
   subtype Cond_Value_Type is System.Unsigned_Longword;

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
   Status : Cond_Value_Type;

   Include_Directory : constant String := "/INCLUDE_DIRECTORY=";

   Mms_Dependencies  : constant String := "/MMS_DEPENDENCIES=FILE=";

   Output_File : constant String := "-o";

   Verbose     : Boolean := False;

   procedure Add
     (S     : in out String_Access;
      Last  : in out Natural;
      Value : String);
   --  Add string Value to string variable S, updating Last

   ---------
   -- Add --
   ---------

   procedure Add
     (S     : in out String_Access;
      Last  : in out Natural;
      Value : String)
   is
   begin
      while S'Last < Last + Value'Length loop
         declare
            New_S : constant String_Access := new String (1 .. 2 * S'Last);

         begin
            New_S (1 .. Last) := S (1 .. Last);
            Free (S);
            S := New_S;
         end;
      end loop;

      S (Last + 1 .. Last + Value'Length) := Value;
      Last := Last + Value'Length;
   end Add;

begin
   declare
      Command_String : String_Access := new String (1 .. 40);
      --  This is the command string that will be used to invoke CC

      Last_Command   : Natural := 0;

      Includes       : String_Access := new String (1 .. 40);
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

               declare
                  Dir : constant String :=
                    Arg (Arg'First + Include_Directory'Length .. Arg'Last);
                  New_Dir : String_Access;

               begin
                  if Is_Directory (Dir) then
                     New_Dir := To_Host_Dir_Spec (Dir, False);
                     Add (Includes, Last_Include, New_Dir.all);

                  else
                     Add (Includes, Last_Include, Dir);
                  end if;
               end;

            elsif Arg'Length > Mms_Dependencies'Length and then
              Arg (Arg'First .. Arg'First + Mms_Dependencies'Length - 1) =
              Mms_Dependencies
            then
               Add (Command_String,
                    Last_Command,
                    " " & Mms_Dependencies &
                    To_Host_File_Spec
                      (Arg
                        (Arg'First + Mms_Dependencies'Length ..
                         Arg'Last)).all);

            --  If it is "-o", the next argument is the output file

            elsif Arg = Output_File then
               if Arg_Num < Argument_Count then
                  Arg_Num := Arg_Num + 1;

                  Output_File_Name := To_Host_File_Spec (Argument (Arg_Num));
               end if;

            --  If it is "-v", skip the argument and set Verbose to True

            elsif Arg = "-v" then
               Verbose := True;

            --  Otherwise, add argument to the command string

            else
               declare
                  New_Arg : String_Access;

               begin
                  if Is_Regular_File (Arg) then
                     New_Arg := To_Host_File_Spec (Arg);

                  elsif Is_Directory (Arg) then
                     New_Arg := To_Host_Dir_Spec (Arg, False);
                  end if;

                  if New_Arg /= null then
                     Add (Command_String, Last_Command, " " & New_Arg.all);

                  else
                     Add (Command_String, Last_Command, " " & Arg);
                  end if;
               end;
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
         if Verbose then
            Put_Line (Cmd);
         end if;

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
