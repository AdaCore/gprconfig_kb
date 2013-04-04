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

with Ada.Characters.Handling;               use Ada.Characters.Handling;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Directories;                       use Ada.Directories;
with Ada.Exceptions;                        use Ada.Exceptions;
with Ada.Strings.Equal_Case_Insensitive;
with Ada.Strings.Fixed;                     use Ada.Strings;
with Ada.Strings.Hash_Case_Insensitive;
with Ada.Strings.Unbounded;                 use Ada.Strings.Unbounded;
with Ada.Text_IO;                           use Ada.Text_IO;
with System.Multiprocessors;                use System;

with Csets;                         use Csets;
with Namet;                         use Namet;
with Prj;                           use Prj;
with Prj.Env;                       use Prj.Env;
with Prj.Part;                      use Prj.Part;
with Prj.Proc;                      use Prj.Proc;
with Prj.Tree;                      use Prj.Tree;
with Snames;                        use Snames;

with GNAT.Command_Line;             use GNAT;
with GNAT.OS_Lib;                   use GNAT.OS_Lib;
with GNAT.Sockets;                  use GNAT.Sockets;
with GNAT.String_Split;             use GNAT.String_Split;
with GNAT.Strings;

with Gprbuild.Compilation;          use Gprbuild.Compilation;
with Gprbuild.Compilation.Protocol; use Gprbuild.Compilation.Protocol;
with GprConfig.Knowledge;           use GprConfig.Knowledge;

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

   function Get_Driver (Language : String) return String;
   --  Returns the compiler driver for the given language and the current
   --  target as retreived from the initial handshake context exchange.

   task Wait_Completion;

   --  A mutex to avoid interweaved responses on the channel

   protected Mutex is
      entry Seize;
      procedure Release;
   private
      Free : Boolean := True;
   end Mutex;

   package Drivers_Cache is new Containers.Indefinite_Hashed_Maps
     (String, String,
      Ada.Strings.Hash_Case_Insensitive, Ada.Strings.Equal_Case_Insensitive);

   Cache   : Drivers_Cache.Map;

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
   Sync         : Sync_Kind;
   Index        : Long_Integer := 0;

   --  Knowledge base

   Base                 : Knowledge_Base;
   Target               : Unbounded_String;
   Selected_Targets_Set : Targets_Set_Id;

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

   ----------------
   -- Get_Driver --
   ----------------

   function Get_Driver (Language : String) return String is
      Key                  : constant String :=
                               To_String (Target) & '+' & Language;
      Position             : constant Drivers_Cache.Cursor := Cache.Find (Key);
      Compilers, Filters   : Compiler_Lists.List;
      Requires_Comp        : Boolean;
      Comp                 : Compiler_Access;
      Project_Node_Tree    : Project_Node_Tree_Ref;
      Project_Node         : Project_Node_Id := Empty_Node;
      Project_Tree         : Project_Tree_Ref;
      Project              : Project_Id;
      Env                  : Environment;
      Success              : Boolean;
      Driver               : Unbounded_String := To_Unbounded_String (Key);
   begin
      if Drivers_Cache.Has_Element (Position) then
         return Drivers_Cache.Element (Position);

      else
         Project_Node_Tree := new Project_Node_Tree_Data;

         --  Generate the configuration project for this language and target

         Parse_Config_Parameter
           (Base              => Base,
            Config            => Language,
            Compiler          => Comp,
            Requires_Compiler => Requires_Comp);

         if Requires_Comp then
            Filters.Append (Comp);
         else
            Compilers.Append (Comp);
         end if;

         Complete_Command_Line_Compilers
           (Base,
            Selected_Targets_Set,
            Filters,
            Compilers);

         Generate_Configuration
           (Base, Compilers, "slave_tmp.cgpr", To_String (Target));

         Prj.Tree.Initialize (Env, Prj.Gprbuild_Flags);
         Prj.Tree.Initialize (Project_Node_Tree);
         Prj.Initialize (Prj.No_Project_Tree);

         Prj.Env.Initialize_Default_Project_Path
           (Env.Project_Path, Target_Name => To_String (Target));

         --  Load the configuration project

         Prj.Part.Parse
           (Project_Node_Tree, Project_Node,
            "slave_tmp.cgpr",
            Errout_Handling   => Prj.Part.Finalize_If_Error,
            Packages_To_Check => null,
            Is_Config_File    => True,
            Target_Name       => To_String (Target),
            Env               => Env);

         Project_Tree := new Project_Tree_Data;
         Prj.Initialize (Project_Tree);

         Proc.Process
           (Project_Tree, Project, null, Success,
            Project_Node, Project_Node_Tree, Env);

         if not Success then
            return Key;
         end if;

         --  Parse it to find the driver for this language

         Look_Driver : declare
            Pcks : Package_Table.Table_Ptr renames
                     Project_Tree.Shared.Packages.Table;
            Pck  : Package_Id := Project.Decl.Packages;
         begin
            Look_Compiler_Package : while Pck /= No_Package loop
               if Pcks (Pck).Decl /= No_Declarations
                 and then Pcks (Pck).Name = Name_Compiler
               then
                  --  Look for the Driver ("<language>") attribute

                  declare
                     Id : Array_Id := Pcks (Pck).Decl.Arrays;
                  begin
                     while Id /= No_Array loop
                        declare
                           V : constant Array_Data :=
                                 Project_Tree.Shared.Arrays.Table (Id);
                        begin
                           if V.Name = Name_Driver
                             and then V.Value /= No_Array_Element
                           then
                              --  Check if element is for the given language,
                              --  and if so return the corresponding value.

                              declare
                                 E : constant Array_Element :=
                                       Project_Tree.Shared.
                                         Array_Elements.Table (V.Value);
                              begin
                                 if Get_Name_String (E.Index) =
                                   To_Lower (Language)
                                 then
                                    Driver := To_Unbounded_String
                                      (Get_Name_String (E.Value.Value));
                                    exit Look_Compiler_Package;
                                 end if;
                              end;
                           end if;
                        end;

                        Id := Project_Tree.Shared.Arrays.Table (Id).Next;
                     end loop;
                  end;
               end if;

               Pck := Pcks (Pck).Next;
            end loop Look_Compiler_Package;
         end Look_Driver;

         --  Record this driver for the language and target into the cache

         Cache.Insert (Key, To_String (Driver));

         --  Clean-up and free project structure

         Directories.Delete_File ("slave_tmp.cgpr");

         return To_String (Driver);
      end if;

   exception
      when others =>
         --  Be sure we never propagate an exception from this routine, in
         --  case of problem we just return the key, this will be used as an
         --  executable and will be reported to the master as a proper build
         --  failure.
         return Key;
   end Get_Driver;

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

                  Send_Dep_File
                    (Work_Directory
                     & (if Dir /= "" then DS & Dir else "") & DS & Dep_File);
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
      --  Wait for a connection

      Accept_Socket (Server, Socket, Address);

      Channel := Create (Socket);

      --  Initial handshake

      begin
         Get_Context (Channel, Target, Project_Name, Sync);
      exception
         when E : others =>
            if Verbose then
               Put_Line (Exception_Information (E));
            end if;
      end;

      Get_Targets_Set
        (Base, To_String (Target), Selected_Targets_Set);

      if Verbose then
         Put_Line ("Handling project : " & To_String (Project_Name));
         Put_Line ("Compiling for    : " & To_String (Target));
      end if;

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

   --  Initialize the project support

   Namet.Initialize;
   Csets.Initialize;
   Snames.Initialize;

   Parse_Knowledge_Base (Base, Default_Knowledge_Base_Directory);

   --  Wait for a gprbuild connection on any addresses

   Address.Addr := Any_Inet_Addr;
   Address.Port := Port_Type (Port);

   Create_Socket (Server);

   Set_Socket_Option (Server, Socket_Level, (Reuse_Address, True));

   Bind_Socket (Server, Address);

   if Port = 0 then
      Address := Get_Socket_Name (Server);
   end if;

   --  If verbose

   if Verbose then
      Put_Line
        ("gprslave on " & Host_Name
         & ":" & Image (Long_Integer (Address.Port)));
      Put_Line ("  max processes :" & Integer'Image (Max_Processes));
      Flush;
   end if;

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
                     Language : constant String := Slice (Args (Cmd), 2);
                     Out_File : constant String := Get_Output_File;
                     Dep_File : constant String := Slice (Args (Cmd), 3);
                     O        : Argument_List := Get_Args (List);
                  begin
                     Output_Compilation (O (O'Last).all);

                     Pid := Non_Blocking_Spawn
                       (Get_Driver (Language), O, Out_File);

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
