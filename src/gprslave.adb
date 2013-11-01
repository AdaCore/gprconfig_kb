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
with Ada.Containers.Vectors;
with Ada.Directories;                       use Ada.Directories;
with Ada.Exceptions;                        use Ada.Exceptions;
with Ada.Strings.Equal_Case_Insensitive;
with Ada.Strings.Fixed;                     use Ada.Strings;
with Ada.Strings.Hash_Case_Insensitive;
with Ada.Strings.Unbounded;                 use Ada.Strings.Unbounded;
with Ada.Text_IO;                           use Ada.Text_IO;
with Interfaces;
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
with GNAT.CRC32;
with GNAT.OS_Lib;                   use GNAT.OS_Lib;
with GNAT.Sockets;                  use GNAT.Sockets;
with GNAT.String_Split;             use GNAT.String_Split;
with GNAT.Strings;

with Gprbuild.Compilation;          use Gprbuild.Compilation;
with Gprbuild.Compilation.Process;  use Gprbuild.Compilation.Process;
with Gprbuild.Compilation.Protocol; use Gprbuild.Compilation.Protocol;
with GprConfig.Knowledge;           use GprConfig.Knowledge;

procedure Gprslave is

   use Ada;

   --  Data for a build master

   type Build_Master is record
      Channel      : Communication_Channel; -- communication with build master
      Socket       : Socket_Type;
      Project_Name : Unbounded_String;
      Target       : Unbounded_String;
      Build_Env    : Unbounded_String;
      Sync         : Sync_Kind;
   end record;

   function "<" (B1, B2 : Build_Master)
      return Boolean is (To_C (B1.Socket) < To_C (B2.Socket));

   function "="
     (B1, B2 : Build_Master) return Boolean is (B1.Socket = B2.Socket);

   package Builder_Set is new Containers.Ordered_Sets (Build_Master);

   --  Representation of a job data

   type Job_Data is record
      Cmd        : Command;
      Id         : Remote_Id;         -- job id must be uniq across all slaves
      Pid        : Integer;           -- the OS process id
      Dep_Dir    : Unbounded_String;
      Dep_File   : Unbounded_String;
      Output     : Unbounded_String;
      Build_Sock : Socket_Type; -- key used to get the corresponding builder
   end record;

   No_Job : constant Job_Data := (Id => -1, others => <>);

   function "<" (J1, J2 : Job_Data) return Boolean is (J1.Pid < J2.Pid);
   function "=" (J1, J2 : Job_Data) return Boolean is (J1.Pid = J2.Pid);

   package Job_Data_Set is new Containers.Ordered_Sets (Job_Data);

   package To_Run_Set is new Containers.Vectors (Positive, Job_Data);

   function Get_Args
     (Builder : Build_Master; Slices : Slice_Set) return Argument_List;
   --  Returns an Argument_List corresponding to the Slice_Set

   procedure Wait_For_Master;
   --  Wait for a build master to connect, initialize the globval communication
   --  channel below. Send the slave config to the build master.

   function Image (Value : Long_Integer) return String;
   --  Return Value string representation without the leading space

   function Work_Directory (Builder : Build_Master) return String;
   --  Directory where compilation are to be done, this is the directory named
   --  after the project under the Root_Directory.

   procedure Parse_Command_Line;
   --  Parse the command line options, set variables below accordingly

   function Get_Slave_Id return Remote_Id;

   task Wait_Completion;
   --  Waiting for job completion and sending back the response to the build
   --  masters.

   task Protocol_Handler;
   --  Waiting for incoming requests from the masters, take corresponding
   --  actions.

   task Run_Compilation;
   --  Task running a maximum of Max_Process compilation simultaneously. These
   --  jobs are taken from the To_Run protected object.

   --  A mutex to avoid interweaved responses on the channel and keep the
   --  working directory. Indeed the tasks are moving to some working directory
   --  from where a compilation has to be run or where the builder environment
   --  has to be created. We must ensure that all those actions are atomic.

   protected Mutex is
      entry Seize;
      procedure Release;
   private
      Free : Boolean := True;
   end Mutex;

   --  Protected builders data set (used by environment task and the
   --  Protocol_Handler).

   protected Builders is

      procedure Insert (Builder : Build_Master);
      --  Add Builder into the set

      procedure Remove (Builder : Build_Master);
      --  Remove Builder from the set

      function Get (Socket : Socket_Type) return Build_Master;
      --  Get the builder using Socket

      entry Get_Socket_Set (Socket_Set : out Socket_Set_Type);
      --  Get a socket set for all builders

   private
      Builders : Builder_Set.Set;
   end Builders;

   --  Queue of Job to run, A FIFO list

   protected To_Run is

      procedure Push (Job : Job_Data);

      entry Pop (Job : out Job_Data);

   private
      Set : To_Run_Set.Vector;
   end To_Run;

   --  Set of running jobs

   protected Running is

      procedure Register (Job : Job_Data);
      --  Register a running Job

      procedure Get (Job : out Job_Data; Pid : Process_Id);
      --  Get Job having the given Pid

      procedure Set_Max (Max : Positive);
      --  Set the maximum running processes simultaneously

      entry Wait_Slot;
      --  Wait for a running slot to be available

      entry Wait;
      --  Wait for at least one running process

   private
      Set   : Job_Data_Set.Set;
      Count : Natural := 0;
      Max   : Natural := 0;
   end Running;

   Compiler_Path : constant OS_Lib.String_Access :=
                     Locate_Exec_On_Path ("gnatls");

   Slave_Id : Remote_Id;
   --  Host Id used to compose a unique job id across all running slaves

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

   Address : Sock_Addr_Type;
   Server  : Socket_Type;
   Index   : Long_Integer := 0;

   --  Knowledge base

   Base                 : Knowledge_Base;
   Selected_Targets_Set : Targets_Set_Id;

   --------------
   -- Builders --
   --------------

   protected body Builders is

      ---------
      -- Get --
      ---------

      function Get (Socket : Socket_Type) return Build_Master is
         Builder : Build_Master;
         Pos     : Builder_Set.Cursor;
      begin
         Builder.Socket := Socket;
         Pos := Builders.Find (Builder);

         if Builder_Set.Has_Element (Pos) then
            Builder := Builder_Set.Element (Pos);
         end if;
         return Builder;
      end Get;

      --------------------
      -- Get_Socket_Set --
      --------------------

      entry Get_Socket_Set (Socket_Set : out Socket_Set_Type)
        when not Builders.Is_Empty is
      begin
         Empty (Socket_Set);
         for B of Builders loop
            Set (Socket_Set, B.Socket);
         end loop;
      end Get_Socket_Set;
      ------------
      -- Insert --
      ------------

      procedure Insert (Builder : Build_Master) is
      begin
         Builders.Insert (Builder);
      end Insert;

      ------------
      -- Remove --
      ------------

      procedure Remove (Builder : Build_Master) is
      begin
         Builders.Delete (Builder);
      end Remove;

   end Builders;

   --------------
   -- Get_Args --
   --------------

   function Get_Args
     (Builder : Build_Master; Slices : Slice_Set) return Argument_List
   is
      Args : Argument_List (1 .. Integer (Slice_Count (Slices)));
   begin
      for K in Args'Range loop
         declare
            A : constant String := Slice (Slices, Slice_Number (K));
            P : constant Natural := Fixed.Index (A, WD_Path_Tag);
         begin
            if P = 0 then
               Args (K) := new String'(A);
            else
               Args (K) := new String'
                 (A (A'First .. P - 1)
                  & Work_Directory (Builder)
                  & Directory_Separator
                  & A (P + WD_Path_Tag'Length .. A'Last));
            end if;
         end;
      end loop;

      return Args;
   end Get_Args;

   -----------------
   -- Get_Slave_Id --
   -----------------

   function Get_Slave_Id return Remote_Id is
      use GNAT.CRC32;
      use type Interfaces.Unsigned_32;
      CRC : GNAT.CRC32.CRC32;
   begin
      Initialize (CRC);
      Update (CRC, Host_Name);
      --  Set the host id as the 32 higher bits
      return Remote_Id (Get_Value (CRC)) * 2 ** 32;
   end Get_Slave_Id;

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

      Running.Set_Max (Max_Processes);

   exception
      when E : Invalid_Switch =>
         Put_Line (Exception_Information (E));
         OS_Exit (1);

      when Exit_From_Command_Line =>
         OS_Exit (1);
   end Parse_Command_Line;

   ----------------------
   -- Protocol_Handler --
   ----------------------

   task body Protocol_Handler is

      type Job_Number is mod 2**32;
      --  A 32bits integer which wrap around. This is no problem as we want
      --  to be able to identify running process. There won't be 2**32 process
      --  running at the same time. So it is safe restart numbering at 0.

      Selector     : Selector_Type;
      R_Socket_Set : Socket_Set_Type;
      Empty_Set    : Socket_Set_Type;
      Status       : Selector_Status;
      Builder      : Build_Master;
      Socket       : Socket_Type;
      Jid          : Job_Number := 0;
   begin
      --  Create selector

      Create_Selector (Selector);

      Empty (Empty_Set);
      --  For now do not check write status

      Handle_Commands : loop

         --  Wait for some commands from one of the build master

         Builders.Get_Socket_Set (R_Socket_Set);

         Check_Selector (Selector, R_Socket_Set, Empty_Set, Status);

         Get (R_Socket_Set, Socket);

         if Socket /= No_Socket then
            Builder := Builders.Get (Socket);

            Mutex.Seize;

            declare
               Cmd : constant Command := Get_Command (Builder.Channel);
            begin
               if Debug then
                  Put ("# command: " & Command_Kind'Image (Kind (Cmd)));

                  declare
                     List : constant Argument_List_Access := Args (Cmd);
                  begin
                     if List /= null then
                        for K in List'Range loop
                           Put (", " & List (K).all);
                        end loop;
                     end if;
                  end;
                  New_Line;
               end if;

               if Kind (Cmd) = EX then
                  Record_Job : declare
                     Id : constant Remote_Id := Slave_Id + Remote_Id (Jid);
                     --  Note that the Id above should be unique across all
                     --  running slaves. This is not the process id, but an id
                     --  sent back to the build master to identify the actual
                     --  job.
                  begin
                     Jid := Jid + 1;
                     if Debug then
                        Put_Line ("# register compilation " & Image (Id));
                     end if;

                     To_Run.Push
                       (Job_Data'(Cmd,
                        Id, -1,
                        Null_Unbounded_String,
                        Null_Unbounded_String,
                        Null_Unbounded_String,
                        Builder.Socket));

                     Send_Ack (Builder.Channel, Id);
                  end Record_Job;

               elsif Kind (Cmd) = FL then
                  null;

               elsif Kind (Cmd) = CU then
                  Clean_Up_Request : begin
                     Builder.Project_Name :=
                       To_Unbounded_String (Args (Cmd)(1).all);

                     if Exists (Work_Directory (Builder)) then
                        if Verbose then
                           Put_Line ("Delete " & Work_Directory (Builder));
                        end if;

                        Delete_Tree (Work_Directory (Builder));
                     end if;

                     Send_Ok (Builder.Channel);

                  exception
                     when others =>
                        Send_Ko (Builder.Channel);
                  end Clean_Up_Request;

               elsif Kind (Cmd) = EC then
                  --  No more compilation for this project
                  Close (Builder.Channel);
                  Builders.Remove (Builder);

                  if Verbose then
                     Put_Line
                       ("End project : " & To_String (Builder.Project_Name));
                  end if;

               else
                  raise Constraint_Error with "unexpected command "
                    & Command_Kind'Image (Kind (Cmd));
               end if;

            exception
               when E : others =>
                  Put_Line ("Error: " & Exception_Information (E));

                  --  In case of an exception, communication endded
                  --  prematurately or some wrong command received, make sure
                  --  we clean the slave state and we listen to new commands.
                  --  Not doing that could make the slave unresponding.
                  Close (Builder.Channel);
            end;

            Mutex.Release;
         end if;
      end loop Handle_Commands;

   exception
      when E : others =>
         Put_Line ("Unrecoverable error: Protocol_Handler.");
         Put_Line (Exception_Information (E));
         OS_Exit (1);
   end Protocol_Handler;

   ---------------------
   -- Run_Compilation --
   ---------------------

   task body Run_Compilation is

      function Get_Driver
        (Builder : Build_Master; Language : String) return String;
      --  Returns the compiler driver for the given language and the current
      --  target as retreived from the initial handshake context exchange.

      function Get_Output_File (Builder : Build_Master) return String;
      --  Returns a unique output file

      procedure Output_Compilation (File : String);
      --  Output compilation information

      package Drivers_Cache is new Containers.Indefinite_Hashed_Maps
        (String, String,
         Ada.Strings.Hash_Case_Insensitive,
         Ada.Strings.Equal_Case_Insensitive);

      Cache : Drivers_Cache.Map;

      ----------------
      -- Get_Driver --
      ----------------

      function Get_Driver
        (Builder : Build_Master; Language : String) return String
      is
         Key                : constant String :=
                                To_String (Builder.Target) & '+' & Language;
         Position           : constant Drivers_Cache.Cursor :=
                                Cache.Find (Key);
         Compilers, Filters : Compiler_Lists.List;
         Requires_Comp      : Boolean;
         Comp               : Compiler_Access;
         Project_Node_Tree  : Project_Node_Tree_Ref;
         Project_Node       : Project_Node_Id := Empty_Node;
         Project_Tree       : Project_Tree_Ref;
         Project            : Project_Id;
         Env                : Environment;
         Success            : Boolean;
         Driver             : Unbounded_String := To_Unbounded_String (Key);
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
              (Base, Compilers, "slave_tmp.cgpr", To_String (Builder.Target));

            Prj.Tree.Initialize (Env, Prj.Gprbuild_Flags);
            Prj.Tree.Initialize (Project_Node_Tree);
            Prj.Initialize (Prj.No_Project_Tree);

            Prj.Env.Initialize_Default_Project_Path
              (Env.Project_Path, Target_Name => To_String (Builder.Target));

            --  Load the configuration project

            Prj.Part.Parse
              (Project_Node_Tree, Project_Node,
               "slave_tmp.cgpr",
               Errout_Handling   => Prj.Part.Finalize_If_Error,
               Packages_To_Check => null,
               Is_Config_File    => True,
               Target_Name       => To_String (Builder.Target),
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
                                 --  Check if element is for the given
                                 --  language, and if so return the
                                 --  corresponding value.

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

      function Get_Output_File (Builder : Build_Master) return String is
         Filename : constant String := "output.slave." & Image (Index);
      begin
         Index := Index + 1;
         return Compose (Work_Directory (Builder), Filename);
      end Get_Output_File;

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

      Job : Job_Data;
   begin
      loop
         --  Launch a new compiilation only if the maximum of simultaneous
         --  process has not yet been reached.

         Running.Wait_Slot;

         To_Run.Pop (Job);

         Process : declare
            Builder : constant Build_Master := Builders.Get (Job.Build_Sock);
            Dir     : constant String := Args (Job.Cmd)(1).all;
            List    : Slice_Set;
            Pid     : Process_Id;
         begin
            --  Enter a critical section to:
            --     - move to directory where the command is executed
            --     - execute the compilation command
            --     - register a new job and acknowledge
            --     - move back to working directory

            Mutex.Seize;

            Set_Directory (Work_Directory (Builder));

            --  Create/Move to object dir if any, note that if we
            --  have an absolute path name here it is because the
            --  Build_Root is probably not properly set. Try to fail
            --  gracefully to report a proper error message to the
            --  build master.
            --
            --  If we have an absolute pathname, just start the
            --  process into the to directory. The output file will
            --  be created there and will be reported to the master.

            if Dir /= "" and then not Is_Absolute_Path (Dir) then
               if not Is_Directory (Dir) then
                  Create_Directory (Dir);
               end if;
            end if;

            if Debug then
               Put_Line ("# move to directory " & Dir);
            end if;

            Set_Directory (Dir);

            Create (List, Args (Job.Cmd)(4).all, ";");

            Execute : declare
               Language : constant String := Args (Job.Cmd)(2).all;
               Out_File : constant String :=
                            Get_Output_File (Builder);
               Dep_File : constant String := Args (Job.Cmd)(3).all;
               O        : Argument_List := Get_Args (Builder, List);
            begin
               Output_Compilation (O (O'Last).all);

               Pid := Non_Blocking_Spawn
                 (Get_Driver (Builder, Language), O, Out_File);

               if Debug then
                  Put_Line
                    ("#   pid"
                     & Integer'Image (Pid_To_Integer (Pid)));
                  Put_Line ("#   dep_file " & Dep_File);
                  Put_Line ("#   out_file " & Out_File);
               end if;

               Job.Pid      := Pid_To_Integer (Pid);
               Job.Dep_File := To_Unbounded_String (Dep_File);
               Job.Output   := To_Unbounded_String (Out_File);
               Job.Dep_Dir  := To_Unbounded_String
                 ((if Is_Absolute_Path (Dir) then "" else Dir));

               Running.Register (Job);

               if Debug then
                  Put_Line ("# move to directory " & Work_Directory (Builder));
               end if;

               Mutex.Release;

               for K in O'Range loop
                  Free (O (K));
               end loop;
            end Execute;
         exception
            when E : others =>
               Mutex.Release;
               if Debug then
                  Put_Line
                    ("# Error in Run_Compilation: "
                     & Exception_Information (E));
               end if;
         end Process;
      end loop;

   exception
      when E : others =>
         Put_Line ("Unrecoverable error: Run_Compilation.");
         Put_Line (Exception_Information (E));
         OS_Exit (1);
   end Run_Compilation;

   -------------
   -- Running --
   -------------

   protected body Running is

      --------------
      -- Register --
      --------------

      procedure Register (Job : Job_Data) is
      begin
         Set.Insert (Job);
         Count := Count + 1;
      end Register;

      ---------
      -- Get --
      ---------

      procedure Get (Job : out Job_Data; Pid : Process_Id) is
         Pos : Job_Data_Set.Cursor;
      begin
         Job.Pid := Pid_To_Integer (Pid);
         Pos := Set.Find (Job);

         if Job_Data_Set.Has_Element (Pos) then
            Job := Job_Data_Set.Element (Pos);
         else
            Job := No_Job;
         end if;

         Set.Delete (Job);
         Count := Count - 1;
      end Get;

      -------------
      -- Set_Max --
      -------------

      procedure Set_Max (Max : Positive) is
      begin
         Running.Max := Max;
      end Set_Max;

      ----------
      -- Wait --
      ----------

      entry Wait when Count > 0 is
      begin
         null;
      end Wait;

      ---------------
      -- Wait_Slot --
      ---------------

      entry Wait_Slot when Count < Max is
      begin
         null;
      end Wait_Slot;

   end Running;

   ------------
   -- To_Run --
   ------------

   protected body To_Run is

      ----------
      -- Push --
      ----------

      procedure Push (Job : Job_Data) is
      begin
         Set.Append (Job);
      end Push;

      ---------
      -- Pop --
      ---------

      entry Pop (Job : out Job_Data) when not Set.Is_Empty is
      begin
         Job := Set.First_Element;
         Set.Delete_First;
      end Pop;

   end To_Run;

   ---------------------
   -- Wait_Completion --
   ---------------------

   task body Wait_Completion is

      procedure Send_Dep_File (Builder : Build_Master; Filename : String);
      --  Send Filename back to the build master

      -------------------
      -- Send_Dep_File --
      -------------------

      procedure Send_Dep_File (Builder : Build_Master; Filename : String) is
      begin
         if Debug then
            Put_Line ("# send dep_file to master '" & Filename & ''');
         end if;

         Send_File (Builder.Channel, Filename);
      end Send_Dep_File;

      Pid     : Process_Id;
      Success : Boolean;
      Job     : Job_Data;
      Builder : Build_Master;

   begin
      loop
         --  Wait for a job to complete only if there is job running

         Running.Wait;

         Wait_Process (Pid, Success);

         Running.Get (Job, Pid);

         --  Note that if there is not such element it could be because the
         --  build master has been killed before the end of the compilation.
         --  In this case an EC message is received by the slave and the
         --  Job_Set is clear. See Main_Loop in gprslave's body.

         if Job /= No_Job then
            Mutex.Seize;

            declare
               A : Argument_List_Access := Args (Job.Cmd);
            begin
               --  Free args

               for K in A'Range loop
                  Free (A (K));
               end loop;

               Free (A);
            end;

            --  Now get the corresponding build master

            Builder := Builders.Get (Job.Build_Sock);
            --  ???What to do if the builder is not found???

            --  Enter a critical section to:
            --    - send atomic response to build master
            --    - make sure the current directory is the work directory

            begin
               if Debug then
                  Put_Line ("# job " & Image (Job.Id) & " terminated");
               end if;

               declare
                  DS       : Character renames Directory_Separator;
                  Dep_Dir  : constant String := To_String (Job.Dep_Dir);
                  Dep_File : constant String := To_String (Job.Dep_File);
                  Out_File : constant String := To_String (Job.Output);
                  S        : Boolean;
               begin
                  Send_Output (Builder.Channel, Out_File);

                  OS_Lib.Delete_File (Out_File, S);

                  if Success and then Builder.Sync /= Protocol.File then
                     --  No Dep_File to send back if the compilation was not
                     --  successful.

                     Send_Dep_File
                       (Builder,
                        Work_Directory (Builder)
                        & (if Dep_Dir /= "" then DS & Dep_Dir else "")
                        & DS & Dep_File);
                  end if;
               end;

               if Debug then
                  Put_Line ("# compilation status " & Boolean'Image (Success));
               end if;

               if Success then
                  Send_Ok (Builder.Channel, Job.Id);
               else
                  Send_Ko (Builder.Channel, Job.Id);
               end if;
            exception
               when others =>
                  --  An exception can be raised if the builder master has been
                  --  terminated. In this case the comminication won't succeed.
               if Debug then
                  Put_Line ("# cannot send response to build master");
               end if;
            end;

            Mutex.Release;

         else
            if Debug then
               Put_Line ("# unknown job data for pid");
            end if;
         end if;
      end loop;

   exception
      when E : others =>
         Put_Line ("Unrecoverable error: Wait_Completion.");
         Put_Line (Exception_Information (E));
         OS_Exit (1);
   end Wait_Completion;

   ---------------------
   -- Wait_For_Master --
   ---------------------

   procedure Wait_For_Master is
      Builder : Build_Master;
   begin
      --  Wait for a connection

      Wait_Incoming_Master : loop
         begin
            Accept_Socket (Server, Builder.Socket, Address);
            exit Wait_Incoming_Master;
         exception
            when E : Socket_Error =>
               if Resolve_Exception (E) /= Interrupted_System_Call then
                  raise;
               end if;
         end;
      end loop Wait_Incoming_Master;

      Builder.Channel := Create (Builder.Socket);

      --  Initial handshake

      begin
         Get_Context
           (Builder.Channel, Builder.Target,
            Builder.Project_Name, Builder.Build_Env, Builder.Sync);
      exception
         when E : others =>
            if Verbose then
               Put_Line (Exception_Information (E));
            end if;
      end;

      Get_Targets_Set
        (Base, To_String (Builder.Target), Selected_Targets_Set);

      if Verbose then
         Put_Line ("Handling project : " & To_String (Builder.Project_Name));
         Put_Line ("Compiling for    : " & To_String (Builder.Target));
      end if;

      Mutex.Seize;

      --  Move to root directory before creating a new project environment

      Set_Directory (Root_Directory.all);

      if not Exists (To_String (Builder.Build_Env)) then
         if Debug then
            Put_Line
              ("# create build environment directory '"
               & To_String (Builder.Build_Env)
               & "' in " & Current_Directory);
         end if;

         Create_Directory (To_String (Builder.Build_Env));
      end if;

      Set_Directory (To_String (Builder.Build_Env));

      if not Exists (To_String (Builder.Project_Name)) then
         if Debug then
            Put_Line
              ("# create project directory '"
               & To_String (Builder.Project_Name)
               & "' in " & Current_Directory);
         end if;

         Create_Directory (To_String (Builder.Project_Name));
      end if;

      Mutex.Release;

      --  Configure slave, note that this does not need to be into the critical
      --  section has the builder is not yet known in the system. At this point
      --  no compilation can be received for this slave anyway.

      Set_Rewrite_WD
        (Builder.Channel, Path => Work_Directory (Builder));

      --  For Ada compilers, rewrite the root directory

      if Debug then
         if Compiler_Path = null then
            Put_Line ("# compiler path is null.");
         else
            Put_Line
              ("# compiler path is : "
               & Containing_Directory
                 (Containing_Directory (Compiler_Path.all)));
         end if;
      end if;

      if Compiler_Path /= null then
         Set_Rewrite_CD
           (Builder.Channel,
            Path => Containing_Directory
              (Containing_Directory (Compiler_Path.all)));
      end if;

      Send_Slave_Config
        (Builder.Channel, Max_Processes,
         Compose (Root_Directory.all, To_String (Builder.Build_Env)));

      --  Register the new builder

      Builders.Insert (Builder);
   end Wait_For_Master;

   --------------------
   -- Work_Directory --
   --------------------

   function Work_Directory (Builder : Build_Master) return String is
   begin
      return Compose
        (Compose (Root_Directory.all, To_String (Builder.Build_Env)),
         To_String (Builder.Project_Name));
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

   --  Initialize the host key used to create unique pid

   Slave_Id := Get_Slave_Id;

   if Debug then
      Put_Line ("# slave id " & Image (Slave_Id));
   end if;

   Listen_Socket (Server);

   Main_Loop : loop
      Wait_For_Master;
   end loop Main_Loop;

exception
   when E : others =>
      Put_Line ("Unrecoverable error: GprSlave.");
      Put_Line (Exception_Information (E));
      OS_Exit (1);
end Gprslave;
