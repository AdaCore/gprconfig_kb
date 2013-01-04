------------------------------------------------------------------------------
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             G P R S L A V E                              --
--                                                                          --
--                                 B o d y                                  --
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

with Ada.Containers.Ordered_Sets;

with Ada.Directories;               use Ada.Directories;
with Ada.Exceptions;                use Ada.Exceptions;
with Ada.Strings.Fixed;             use Ada.Strings;
with Ada.Strings.Unbounded;         use Ada.Strings.Unbounded;
with Ada.Text_IO;                   use Ada.Text_IO;
with System.Multiprocessors;        use System;

with GNAT.Command_Line;             use GNAT;
with GNAT.OS_Lib;                   use GNAT.OS_Lib;
with GNAT.Sockets;                  use GNAT.Sockets;
with GNAT.String_Split;             use GNAT.String_Split;
with GNAT.Strings;

with Gpr_Util;                      use Gpr_Util;
with Gprbuild.Compilation;          use Gprbuild.Compilation;
with Gprbuild.Compilation.Protocol; use Gprbuild.Compilation.Protocol;

procedure Gprslave is

   use Ada;

   type Job_Data is record
      Pid      : Integer;
      Dir      : Unbounded_String;
      Dep_File : Unbounded_String;
      Output   : Unbounded_String;
   end record;

   function "<" (J1, J2 : Job_Data) return Boolean is (J1.Pid < J2.Pid);
   function "=" (J1, J2 : Job_Data) return Boolean is (J1.Pid = J2.Pid);

   package Job_Data_Set is new Containers.Ordered_Sets (Job_Data);

   function Get_Args (Slices : Slice_Set) return Argument_List;
   --  Returns an Argument_List corresponding to the Slice_Set

   procedure Wait_For_Master;
   --  Wait for a build master to connect, initialize the globval communication
   --  channel below. Send the slave config to the build master.

   function Image (Value : Long_Integer) return String;
   --  Return Value string representation without the leading space

   function Work_Directory return String;
   --  Directory where compilation are to be done, this is the directory named
   --  after the project under the Root_Directory.

   procedure Parse_Command_Line;
   --  Parse the command line options, set variables below accordingly

   procedure Output_Compilation (File : String);
   --  Output compilation information

   function Get_Output_File return String;
   --  Returns a unique output file

   task Wait_Completion;

   --  A mutex to avoid interweaved responses on the channel

   protected Mutex is
      entry Seize;
      procedure Release;
   private
      Free : Boolean := True;
   end Mutex;

   Job_Set : Job_Data_Set.Set; -- current jobs waiting for completion
   Jobs    : Shared_Counter;   -- number of jobs running

   --  Command line parameters statuses

   Port           : aliased Integer;
   Max_Processes  : aliased Integer;
   Help           : aliased Boolean;
   Verbose        : aliased Boolean;
   Debug          : aliased Boolean;
   Root_Directory : aliased GNAT.Strings.String_Access :=
                       new String'(Current_Directory);
   --  Root directoty for the gprslave environment. All projects sources and
   --  compilations are done under this directory.

   --  Running instances statuses

   Channel      : Communication_Channel; -- communication with build master
   Address      : Sock_Addr_Type;
   Server       : Socket_Type;
   Socket       : Socket_Type;
   Project_Name : Unbounded_String;
   OS           : Unbounded_String;
   Sync         : Sync_Kind;
   Index        : Long_Integer := 0;

   --------------
   -- Get_Args --
   --------------

   function Get_Args (Slices : Slice_Set) return Argument_List is
      Args : Argument_List (1 .. Integer (Slice_Count (Slices)));
   begin
      for K in Args'Range loop
         declare
            A : constant String := Slice (Slices, Slice_Number (K));
            P : constant Natural := Fixed.Index (A, Full_Path_Tag);
         begin
            if P = 0 then
               Args (K) := new String'(A);
            else
               Args (K) := new String'
                 (A (A'First .. P - 1)
                  & Work_Directory
                  & Directory_Separator & A (P + 2 .. A'Last));
            end if;
         end;
      end loop;

      return Args;
   end Get_Args;

   ---------------------
   -- Get_Output_File --
   ---------------------

   function Get_Output_File return String is
      Filename : constant String := "output.slave." & Image (Index);
   begin
      Index := Index + 1;
      return Compose (Work_Directory, Filename);
   end Get_Output_File;

   -----------
   -- Image --
   -----------

   function Image (Value : Long_Integer) return String is
      I : constant String := Long_Integer'Image (Value);
   begin
      return (if I (I'First) = '-'
              then I
              else I (I'First + 1 .. I'Last));
   end Image;

   -----------
   -- Mutex --
   -----------

   protected body Mutex is

      -----------
      -- Seize --
      -----------

      entry Seize when Free is
      begin
         Free := False;
      end Seize;

      -------------
      -- Release --
      -------------

      procedure Release is
      begin
         Free := True;
      end Release;

   end Mutex;

   ------------------------
   -- Output_Compilation --
   ------------------------

   procedure Output_Compilation (File : String) is
      RDL : constant Natural := Root_Directory'Length;
   begin
      if Verbose then
         if File'Length > RDL
           and then File (File'First .. File'First + RDL - 1)
                    = Root_Directory.all
         then
            Text_IO.Put_Line
              ("Compiling: " & File (File'First + RDL + 1 .. File'Last));
         else
            Text_IO.Put_Line ("Compiling: " & File);
         end if;
      end if;
   end Output_Compilation;

   ------------------------
   -- Parse_Command_Line --
   ------------------------

   procedure Parse_Command_Line is
      use GNAT.Command_Line;

      Config : Command_Line_Configuration;
   begin
      Define_Switch
        (Config, Help'Access,
         "-h", Long_Switch => "--help",
         Help => "display this help message");

      Define_Switch
        (Config, Max_Processes'Access,
         "-j:", Long_Switch => "--jobs=",
         Initial => Integer (Multiprocessors.Number_Of_CPUs),
         Default => Integer (Multiprocessors.Number_Of_CPUs),
         Help    => "set the maximum simultaneous compilation");

      Define_Switch
        (Config, Root_Directory'Access,
         "-d:", Long_Switch => "--directory=",
         Help    => "set the root directory");

      Define_Switch
        (Config, Port'Access,
         "-p:", Long_Switch => "--port=",
         Initial => Integer (Default_Port),
         Default => Integer (Default_Port),
         Help    => "set the port the slave will listen to");

      Define_Switch
        (Config, Verbose'Access,
         "-v", Long_Switch => "--verbose",
         Help => "activate verbose mode, display extra information");

      Define_Switch
        (Config, Debug'Access,
         "-vv", Long_Switch => "--debug",
         Help => "activate debug mode, display lot of information (imply -v)");

      Set_Usage (Config, Usage => "[switches]");

      Getopt (Config);

      if Help then
         Display_Help (Config);
         OS_Exit (1);
      end if;

      if Debug then
         Verbose := True;
      end if;

      --  Ensure Root_Directory does not ends with a directory separator

      if Root_Directory (Root_Directory'Last) in '/' | '\' then
         Delete_Last : declare
            RD : constant String := Root_Directory
              (Root_Directory'First .. Root_Directory'Last - 1);
         begin
            Free (Root_Directory);
            Root_Directory := new String'(RD);
         end Delete_Last;
      end if;

   exception
      when E : Invalid_Switch =>
         Put_Line (Exception_Information (E));
         OS_Exit (1);

      when Exit_From_Command_Line =>
         OS_Exit (1);
   end Parse_Command_Line;

   ---------------------
   -- Wait_Completion --
   ---------------------

   task body Wait_Completion is

      procedure Send_Dep_File (Filename : String);
      --  Send Filename back to the build master

      -------------------
      -- Send_Dep_File --
      -------------------

      procedure Send_Dep_File (Filename : String) is
      begin
         if Debug then
            Put_Line ("# send dep_file to master '" & Filename & ''');
         end if;

         Send_File (Channel, Filename);
      end Send_Dep_File;

      Pid     : Process_Id;
      Success : Boolean;
      Data    : Job_Data;
      Pos     : Job_Data_Set.Cursor;
   begin
      loop
         Jobs.Wait_Non_Zero;

         Wait_Process (Pid, Success);

         --  Set Pid (the key)

         Data.Pid := Pid_To_Integer (Pid);

         --  Look for it into the set

         Pos := Job_Set.Find (Data);

         --  Note that if there is not such element it could be because the
         --  build master has been killed before the end of the compilation.
         --  In this case an EC message is received by the slave and the
         --  Job_Set is clear. See Main_Loop in gprslave's body.

         if Job_Data_Set.Has_Element (Pos) then
            Data := Job_Data_Set.Element (Pos);

            --  Enter a critical section to:
            --    - send atomic response to build master
            --    - make sure the current directory is the work directory

            Mutex.Seize;

            declare
               DS       : Character renames Directory_Separator;
               Dir      : constant String := To_String (Data.Dir);
               Dep_File : constant String := To_String (Data.Dep_File);
            begin
               Send_Output (Channel, To_String (Data.Output));

               if Success and then Sync /= Protocol.File then
                  --  No Dep_File to send back if the compilation was not
                  --  successful.

                  if Dir = "" then
                     Send_Dep_File (Work_Directory & DS & Dep_File);
                  else
                     Send_Dep_File (Work_Directory & DS & Dir & DS & Dep_File);
                  end if;
               end if;
            end;

            if Debug then
               Put_Line ("# compilation status " & Boolean'Image (Success));
            end if;

            if Success then
               Send_Ok (Channel, Pid_To_Integer (Pid));
            else
               Send_Ko (Channel, Pid_To_Integer (Pid));
            end if;

            Mutex.Release;

            Jobs.Decrement;
         end if;
      end loop;
   exception
      when E : others =>
         Put_Line (Exception_Information (E));
         OS_Exit (1);
   end Wait_Completion;

   ---------------------
   -- Wait_For_Master --
   ---------------------

   procedure Wait_For_Master is
   begin
      Wait_Compatible_Master : loop
         --  Wait for a connection

         Accept_Socket (Server, Socket, Address);

         Channel := Create (Socket);

         --  Initial handshake

         begin
            Get_Context (Channel, OS, Project_Name, Sync);
         exception
            when E : others =>
               if Verbose then
                  Put_Line (Exception_Information (E));
               end if;
         end;

         if Verbose then
            Put_Line ("Handling project : " & To_String (Project_Name));

            if To_String (OS) /= Get_OS
              and then To_String (OS) /= Any_OS
            then
               Send_Ko (Channel);

               Put_Line
                 ("   rejected, master OS is imcompatible " & To_String (OS));

            else
               exit Wait_Compatible_Master;
            end if;
         end if;
      end loop Wait_Compatible_Master;

      --  Move to root directory before creating a new project environment

      Set_Directory (Root_Directory.all);

      Set_Rewrite (Channel, From => Work_Directory, To => Full_Path_Tag);

      if not Exists (To_String (Project_Name)) then
         if Debug then
            Put_Line
              ("# create project directory '"
               & To_String (Project_Name) & " in " & Current_Directory);
         end if;

         Create_Directory (To_String (Project_Name));
      end if;

      Send_Slave_Config (Channel, Max_Processes, Root_Directory.all);

      --  Now move into the work directory (Root_Directory & Project_Name)

      Set_Directory (Work_Directory);
   end Wait_For_Master;

   --------------------
   -- Work_Directory --
   --------------------

   function Work_Directory return String is
   begin
      return Compose (Root_Directory.all, To_String (Project_Name));
   end Work_Directory;

begin
   Parse_Command_Line;

   --  If verbose

   if Verbose then
      Put_Line
        ("gprslave on " & Host_Name & ":" & Image (Long_Integer (Port))
         & " (" & Get_OS & ")");
      Put_Line ("  max processes :" & Integer'Image (Max_Processes));
   end if;

   --  Wait for a gprbuild connection on any addresses

   Address.Addr := Any_Inet_Addr;
   Address.Port := Port_Type (Port);

   Create_Socket (Server);

   Set_Socket_Option (Server, Socket_Level, (Reuse_Address, True));

   Bind_Socket (Server, Address);

   Listen_Socket (Server);

   Main_Loop : loop
      Wait_For_Master;

      --  We have a connection

      Handle_Compilation : loop
         --  Move to work directory

         declare
            Cmd  : constant Command := Get_Command (Channel);
            Pid  : Process_Id;
            List : Slice_Set;
         begin
            if Debug then
               Put ("# command: " & Command_Kind'Image (Kind (Cmd)));

               declare
                  List : constant Slice_Set := Args (Cmd);
               begin
                  for K in 1 .. Slice_Count (List) loop
                     Put (", " & Slice (List, K));
                  end loop;
               end;
               New_Line;
            end if;

            --  Move to project environment, which is:
            --  Root_Directory & Project_Name. We can do this only now since we
            --  know that at this point the sources have been synchronized.

            if Kind (Cmd) = EX then
               Setup_Execute_Process : declare
                  Dir : constant String := Slice (Args (Cmd), 1);
               begin
                  --  Enter a critical section to:
                  --     - move to directory where the command is executed
                  --     - execute the compilation command
                  --     - register a new job and acknowledge
                  --     - move back to working directory

                  Mutex.Seize;

                  --  Create/Move to object dir if any, note that if we have
                  --  an absolute path name here it is because the Build_Root
                  --  is probably not properly set. Try to fail gracefully to
                  --  report a proper error message to the build master.
                  --
                  --  If we have an absolute pathname, just start the process
                  --  into the to directory. The output file will be created
                  --  there and will be reported to the master.

                  if Dir /= "" and then not Is_Absolute_Path (Dir) then
                     if not Is_Directory (Dir) then
                        Create_Directory (Dir);
                     end if;
                  end if;

                  if Debug then
                     Put_Line ("# move to directory " & Dir);
                  end if;

                  Set_Directory (Dir);

                  Create (List, Slice (Args (Cmd), 4), ";");

                  Execute_Process : declare
                     Out_File : constant String := Get_Output_File;
                     Dep_File : constant String := Slice (Args (Cmd), 3);
                     O        : Argument_List := Get_Args (List);
                  begin
                     Output_Compilation (O (O'Last).all);

                     Pid := Non_Blocking_Spawn
                       (Slice (Args (Cmd), 2), O, Out_File);

                     Send_Ack (Channel, Pid_To_Integer (Pid));

                     if Debug then
                        Put_Line
                          ("#   pid" & Integer'Image (Pid_To_Integer (Pid)));
                        Put_Line ("#   dep_file " & Dep_File);
                        Put_Line ("#   out_file " & Out_File);
                     end if;

                     Job_Set.Insert
                       (Job_Data'(Pid_To_Integer (Pid),
                        To_Unbounded_String
                          (if Is_Absolute_Path (Dir) then "" else Dir),
                        To_Unbounded_String (Dep_File),
                        To_Unbounded_String (Out_File)));

                     if Debug then
                        Put_Line ("# move to directory " & Work_Directory);
                     end if;

                     Set_Directory (Work_Directory);

                     Mutex.Release;

                     for K in O'Range loop
                        Free (O (K));
                     end loop;

                     Jobs.Increment;
                  end Execute_Process;
               end Setup_Execute_Process;

            elsif Kind (Cmd) = FL then
               null;

            elsif Kind (Cmd) = CU then
               Clean_Up_Request : begin
                  Project_Name := To_Unbounded_String (Slice (Args (Cmd), 1));

                  if Exists (Work_Directory) then
                     if Verbose then
                        Put_Line ("Delete " & Work_Directory);
                     end if;

                     Delete_Tree (Work_Directory);
                  end if;

                  Send_Ok (Channel);

               exception
                  when others =>
                     Send_Ko (Channel);
               end Clean_Up_Request;

            elsif Kind (Cmd) = EC then
               --  No more compilation for this project
               Close (Channel);
               Job_Set.Clear;
               Jobs.Reset;
               exit Handle_Compilation;

            else
               raise Constraint_Error
                 with "unexpected command " & Command_Kind'Image (Kind (Cmd));
            end if;

         exception
            when E : others =>
               Put_Line ("Error: " & Exception_Message (E));

               --  In case of an exception, communication endded prematurately
               --  or some wrong command received, make sure we clean the slave
               --  state and we listen to new commands. Not doing that could
               --  make the slave unresponding.
               Close (Channel);
               Job_Set.Clear;
               Jobs.Reset;
               exit Handle_Compilation;
         end;
      end loop Handle_Compilation;

      if Verbose then
         Put_Line ("End project : " & To_String (Project_Name));
      end if;
   end loop Main_Loop;

exception
   when E : others =>
      Put_Line ("Unexpected error : " & Exception_Information (E));
      OS_Exit (1);
end Gprslave;
