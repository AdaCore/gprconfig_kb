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

with Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones;               use Ada.Calendar;
with Ada.Characters.Handling;               use Ada.Characters.Handling;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;
with Ada.Directories;                       use Ada.Directories;
with Ada.Exceptions;                        use Ada.Exceptions;
with Ada.Finalization;                      use Ada.Finalization;
with Ada.Strings.Equal_Case_Insensitive;
with Ada.Strings.Fixed;                     use Ada.Strings;
with Ada.Strings.Hash_Case_Insensitive;
with Ada.Strings.Unbounded;                 use Ada.Strings.Unbounded;
with Ada.Text_IO;                           use Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Interfaces;
with System.Multiprocessors;                use System;

with GNAT.Command_Line;       use GNAT;
with GNAT.CRC32;
with GNAT.Exception_Traces;
with GNAT.OS_Lib;             use GNAT.OS_Lib;
with GNAT.Sockets;            use GNAT.Sockets;
with GNAT.String_Split;       use GNAT.String_Split;
with GNAT.Strings;
with GNAT.Traceback.Symbolic; use GNAT.Traceback;
                              use GNAT.Traceback.Symbolic;

with Gpr_Util;                      use Gpr_Util;
with GPR_Version;
with Gprbuild.Compilation;          use Gprbuild.Compilation;
with Gprbuild.Compilation.Process;  use Gprbuild.Compilation.Process;
with Gprbuild.Compilation.Protocol; use Gprbuild.Compilation.Protocol;
with GprConfig.Knowledge;           use GprConfig.Knowledge;
with GPR;                           use GPR;
with GPR.Opt;                       use GPR.Opt;
with GPR.Env;                       use GPR.Env;
with GPR.Names;                     use GPR.Names;
with GPR.Part;                      use GPR.Part;
with GPR.Proc;                      use GPR.Proc;
with GPR.Tree;                      use GPR.Tree;
with GPR.Snames;                    use GPR.Snames;

procedure Gprslave is

   use Ada;

   type UID is mod 9999;

   --  The Status is shared by the same build master object. It first has a
   --  reference counter to free the memory associated with this status and
   --  a boolean used a a mutex to lock/unlock the object to allow proper
   --  concurrent access.

   type Status is record
      Id     : UID;
      Locked : Boolean := False;
      Count  : Natural := 0;
   end record;

   type Shared_Status is access Status;

   --  Data for a build master

   type Build_Master is new Finalization.Controlled with record
      Channel      : Communication_Channel; -- communication with build master
      Socket       : Socket_Type;
      Project_Name : Unbounded_String;
      Target       : Unbounded_String;
      Build_Env    : Unbounded_String;
      Sync         : Boolean;
      Status       : Shared_Status;
   end record;

   overriding procedure Initialize (Builder : in out Build_Master);
   overriding procedure Adjust     (Builder : in out Build_Master);
   overriding procedure Finalize   (Builder : in out Build_Master);

   protected Controlled_Build_Master is
      procedure Initialize (Builder : in out Build_Master);
      procedure Adjust     (Builder : in out Build_Master);
      procedure Finalize   (Builder : in out Build_Master);
   end Controlled_Build_Master;

   package Builder is

      function "<" (B1, B2 : Build_Master) return Boolean is
        (To_C (B1.Socket) < To_C (B2.Socket));

      function "=" (B1, B2 : Build_Master) return Boolean is
        (B1.Socket = B2.Socket);

      package Set is new Containers.Ordered_Sets (Build_Master);

   end Builder;

   package Builder_Set renames Builder.Set;

   --  Representation of a job data

   type Job_Data is record
      Cmd        : Command;
      Id         : Remote_Id;         -- job id must be uniq across all slaves
      Pid        : Process_Id;        -- the OS process id
      Dep_Dir    : Unbounded_String;
      Dep_File   : Unbounded_String;
      Obj_File   : Unbounded_String;
      Output     : Unbounded_String;
      Build_Sock : Socket_Type; -- key used to get the corresponding builder
      Killed     : Boolean;
   end record;

   No_Job : constant Job_Data := (Id => -1, others => <>);

   function "<" (J1, J2 : Job_Data) return Boolean is
     (Pid_To_Integer (J1.Pid) < Pid_To_Integer (J2.Pid));

   function "=" (J1, J2 : Job_Data) return Boolean is
     (Pid_To_Integer (J1.Pid) = Pid_To_Integer (J2.Pid));

   package Job_Data_Set is new Containers.Ordered_Sets (Job_Data);

   package To_Run_Set is new Containers.Vectors (Positive, Job_Data);

   function Get_Arg
     (Builder : Build_Master; Value : String) return String with Inline;
   --  Returns Value with possible translation of the local repositories

   function Get_Args
     (Builder : Build_Master; Slices : Slice_Set) return Argument_List;
   --  Returns an Argument_List corresponding to the Slice_Set

   function Image (Value : Long_Integer) return String;
   --  Return Value string representation without the leading space

   function Work_Directory (Builder : Build_Master) return String;
   --  Directory where compilation are to be done, this is the directory named
   --  after the project under the Root_Directory.

   procedure Parse_Command_Line;
   --  Parse the command line options, set variables below accordingly

   function Get_Slave_Id return Remote_Id;

   function Is_Active_Build_Master (Builder : Build_Master) return Boolean is
     (Builder.Project_Name /= Null_Unbounded_String
      and then Builder.Status /= null);

   procedure Close_Builder (Builder : in out Build_Master; Ack : Boolean);
   --  Close the channel and socket and remove the builder from the slave. This
   --  procedure never fails. Send a OK message if Ack is True.

   procedure Activate_Symbolic_Traceback;
   --  Activate symbolic trace-back

   --
   --  Belows are the main objects which handle the concurrent requests
   --

   procedure Wait_For_Master;
   --  Wait for a build master to connect, initialize the global communication
   --  channel. This procedure is run under the environment task. Send the
   --  slave config to the build master. Either a builder object is created and
   --  inserted into the Builders protected object or the builder is rejected
   --  because of inconsistent state:
   --
   --  1. the builder and the slave are not using the same compiler.
   --  2. the slave is already handling compilation for this project
   --     environment.

   task Wait_Requests;
   --  Waiting for incoming requests from the masters, take corresponding
   --  actions. Three actions are handled here:
   --
   --  1. EX - execute a compilation
   --     A compilation request is inserted into To_Run protected object.
   --
   --  2. CU - execute a clean-up
   --     A clean-up request is inserted into To_Run protected object.
   --
   --  3. EC - stop execution for the given builder

   task Execute_Job;
   --  Task running a maximum of Max_Process compilation simultaneously. These
   --  jobs are taken from the To_Run protected object (a FIFO list).
   --
   --  Jobs taken from To_Run protected object are removed, executed
   --  asynchronously and inserted into the Running protected object with
   --  the corresponding process Id and builder.
   --
   --  IMPORTANT NOTE : this is the only task that can change the working
   --  directory (Set_Directory for example). This makes locking circuitry
   --  lighter and more efficient.

   task type Wait_Completion;
   --  Waiting for completion of compilation jobs. The Pid is retreived with
   --  the corresponding builder, then it sends back the response to the build
   --  masters. The response is OK or NOK depending on compilation result. If
   --  OK the auxiliaries files (.ali, .o) are sent back to the build master.
   --
   --  This is the only task with multiple instance. As sending back resulting
   --  objects and ALI files can take some time haaving multiple instance
   --  permit to send results to different builders simultaneously.

   protected Builders is

      --  Protected builders data set (used by environment task and the
      --  Protocol_Handler).
      --
      --  The list of builder, one for each build master. Inserted here when a
      --  compilation starts and removed when an end-of-compilation message is
      --  received or a master is interrupted.

      procedure Insert (Builder : Build_Master);
      --  Add Builder into the set

      procedure Remove (Builder : in out Build_Master);
      --  Remove Builder from the set

      function Get (Socket : Socket_Type) return Build_Master;
      --  Get the builder using Socket

      function Exists (Socket : Socket_Type) return Boolean;
      --  Returns True if the build master corresponding to socket is found.
      --  False otherwise.

      entry Get_Socket_Set (Socket_Set : out Socket_Set_Type);
      --  Get a socket set for all builders

      procedure Initialize (Builder : in out Build_Master);
      --  Set the UID for this build master. This Id is only used in log
      --  message to identify a specific build.

      function Working_Dir_Exists (Directory : String) return Boolean;
      --  Returns True if Directory is already used by a registered build
      --  master. This is to ensure that a unique build will happen in a
      --  given directory.

      entry Lock (Builder : in out Build_Master);
      --  Lock builder against concurrent use, must be released

      procedure Release (Builder : in out Build_Master);
      --  Release builder locked with entry above

   private

      entry Try_Lock (Builder : in out Build_Master);
      --  The lock is already taken, the tasks are queued here to wait for the
      --  builder to be released.

      Current_Id : UID := 0;
      Builders   : Builder_Set.Set;
      To_Check   : Natural := 0; -- number of task to let go through Try_Lock
   end Builders;

   protected To_Run is

      --  Queue of Job to run, A FIFO list of jobs comming from all registered
      --  builders.

      procedure Push (Job : Job_Data);

      entry Pop (Job : out Job_Data);

   private
      Set : To_Run_Set.Vector;
   end To_Run;

   protected Running is

      --  Set of running jobs. Removed when the compilation terminates or when
      --  killed because of a builder is interrupted.

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

      procedure Kill_Processes (Socket : Socket_Type);
      --  Kill all processes whose builder is registered with Socket. This
      --  is used when a builder is interrupted to kill all corresponding
      --  processes.

      function Count return Natural;
      --  Number of job running

   private
      Set     : Job_Data_Set.Set;
      Dead    : Job_Data_Set.Set; -- job which failed to start
      N_Count : Natural := 0;     -- actual number of running process
      Max     : Natural := 0;
   end Running;

   --  Ensure that all IO are serialized, especially the spawn of process which
   --  must never happen during other IO. This is needed as the spawned process
   --  will inherit the standard IO descriptors.

   protected IO is

      procedure Message
        (Builder  : Build_Master;
         Str      : String;
         Is_Debug : Boolean := False;
         Force    : Boolean := False) with Inline;
      procedure Message
        (Str      : String;
         Is_Debug : Boolean := False;
         Force    : Boolean := False) with Inline;
      --  Display a message (in verbose mode) and adds a leading timestamp.
      --  Also display the message in debug mode if Is_Debug is set.

      procedure Spawn
        (Driver   : String;
         Options  : Argument_List;
         Out_File : String;
         Pid      : out Process_Id);

   end IO;

   Compiler_Path : constant OS_Lib.String_Access :=
                     Locate_Exec_On_Path ("gnatls");

   Slave_Id : Remote_Id;
   --  Host Id used to compose a unique job id across all running slaves

   --  Command line parameters statuses

   Port           : aliased Integer;
   Max_Processes  : aliased Integer;
   Max_Responses  : aliased Integer;
   Help           : aliased Boolean := False;
   Verbose        : aliased Boolean := False;
   Debug          : aliased Boolean := False;
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

   --  Handle response

   type Response_Handler_Set is array (Positive range <>) of Wait_Completion;
   type Response_Handler_Set_Access is access Response_Handler_Set;

   Response_Handlers : Response_Handler_Set_Access with Unreferenced;
   --  Sending response to a build master may take some time as the object file
   --  is sent back over the socket with the corresponding dependency file.

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Builder : in out Build_Master) is
   begin
      Controlled_Build_Master.Adjust (Builder);
   end Adjust;

   ---------------------------------
   -- Activate_Symbolic_Traceback --
   ---------------------------------

   procedure Activate_Symbolic_Traceback is
   begin
      Exception_Traces.Trace_On (Exception_Traces.Unhandled_Raise);
      Exception_Traces.Set_Trace_Decorator
        (Traceback.Symbolic.Symbolic_Traceback'Access);
   end Activate_Symbolic_Traceback;

   --------------
   -- Builders --
   --------------

   protected body Builders is

      ------------
      -- Exists --
      ------------

      function Exists (Socket : Socket_Type) return Boolean is
         Builder : Build_Master;
      begin
         Builder.Socket := Socket;
         return Builder_Set.Has_Element (Builders.Find (Builder));
      end Exists;

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

      ----------------
      -- Initialize --
      ----------------

      procedure Initialize (Builder : in out Build_Master) is
      begin
         Builder.Status.Id := Current_Id;
         Current_Id := Current_Id + 1;
      end Initialize;

      ------------
      -- Insert --
      ------------

      procedure Insert (Builder : Build_Master) is
      begin
         Builders.Insert (Builder);
      end Insert;

      ----------
      -- Lock --
      ----------

      entry Lock (Builder : in out Build_Master) when True is
      begin
         if Builder.Status.Locked then
            requeue Try_Lock;
         else
            Builder.Status.Locked := True;
         end if;
      end Lock;

      -------------
      -- Release --
      -------------

      procedure Release (Builder : in out Build_Master) is
      begin
         Builder.Status.Locked := False;
         if Try_Lock'Count > 0 then
            To_Check := To_Check + Try_Lock'Count;
         end if;
      end Release;

      ------------
      -- Remove --
      ------------

      procedure Remove (Builder : in out Build_Master) is
      begin
         Builders.Exclude (Builder);
         Running.Kill_Processes (Builder.Socket);
         Release (Builder);
      end Remove;

      --------------
      -- Try_Lock --
      --------------

      entry Try_Lock (Builder : in out Build_Master) when To_Check > 0 is
      begin
         To_Check := To_Check - 1;

         if Builder.Status.Locked then
            requeue Try_Lock;
         else
            Builder.Status.Locked := True;
         end if;
      end Try_Lock;

      ------------------------
      -- Working_Dir_Exists --
      ------------------------

      function Working_Dir_Exists (Directory : String) return Boolean is
      begin
         for B of Builders loop
            if Work_Directory (B) = Directory then
               return True;
            end if;
         end loop;
         return False;
      end Working_Dir_Exists;

   end Builders;

   -------------------
   -- Close_Builder --
   -------------------

   procedure Close_Builder (Builder : in out Build_Master; Ack : Boolean) is
   begin
      --  First unregister the builder

      Builders.Remove (Builder);

      --  Now close the channel/socket. This routine is used when the builder
      --  has encountered an error, so the associated socket may be in a bad
      --  state. Make sure we do not fail here.

      begin
         --  Send an Ack message before closing if requested

         if Ack then
            Send_Ok (Builder.Channel);
         end if;

         Close (Builder.Channel);
         Close_Socket (Builder.Socket);
      exception
         when others =>
            null;
      end;
   end Close_Builder;

   -----------------------------
   -- Controlled_Build_Master --
   -----------------------------

   protected body Controlled_Build_Master is

      ------------
      -- Adjust --
      ------------

      procedure Adjust (Builder : in out Build_Master) is
      begin
         Builder.Status.Count := Builder.Status.Count + 1;
      end Adjust;

      --------------
      -- Finalize --
      --------------

      procedure Finalize (Builder : in out Build_Master) is
         procedure Unchecked_Free is
           new Unchecked_Deallocation (Status, Shared_Status);
         S : Shared_Status := Builder.Status;
      begin
         Builder.Status := null;

         S.Count := S.Count - 1;

         if S.Count = 0 then
            Unchecked_Free (S);
         end if;
      end Finalize;

      ----------------
      -- Initialize --
      ----------------

      procedure Initialize (Builder : in out Build_Master) is
      begin
         Builder.Status := new Status'(0, False, 1);
      end Initialize;

   end Controlled_Build_Master;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Builder : in out Build_Master) is
   begin
      Controlled_Build_Master.Finalize (Builder);
   end Finalize;

   -------------
   -- Get_Arg --
   -------------

   function Get_Arg (Builder : Build_Master; Value : String) return String is
      P : constant Natural := Fixed.Index (Value, WD_Path_Tag);
   begin
      if P = 0 then
         return Value;
      else
         return Value (Value'First .. P - 1)
           & Work_Directory (Builder)
           & Directory_Separator
           & Get_Arg (Builder, Value (P + WD_Path_Tag'Length .. Value'Last));
      end if;
   end Get_Arg;

   --------------
   -- Get_Args --
   --------------

   function Get_Args
     (Builder : Build_Master; Slices : Slice_Set) return Argument_List
   is
      Args : Argument_List (1 .. Integer (Slice_Count (Slices)));
   begin
      for K in Args'Range loop
         Args (K) := new String'
           (Get_Arg (Builder, Slice (Slices, Slice_Number (K))));
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

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (Builder : in out Build_Master) is
   begin
      Controlled_Build_Master.Initialize (Builder);
   end Initialize;

   --------
   -- IO --
   --------

   protected body IO is

   -------------
   -- Message --
   -------------

      procedure Message
        (Str      : String;
         Is_Debug : Boolean := False;
         Force    : Boolean := False) is
      begin
         if Force or (Verbose and not Is_Debug) or (Debug and Is_Debug) then
            Put_Line
              ('[' & Calendar.Formatting.Image (Calendar.Clock) & "] " & Str);
         end if;
      end Message;

      procedure Message
        (Builder  : Build_Master;
         Str      : String;
         Is_Debug : Boolean := False;
         Force    : Boolean := False)
      is
         package UID_IO is new Text_IO.Modular_IO (UID);
      begin
         if Force or (Verbose and not Is_Debug) or (Debug and Is_Debug) then
            UID_IO.Put (Builder.Status.Id, Width => 4);
            Put (' ');
            Message (Str, Is_Debug, Force);
         end if;
      end Message;

      -----------
      -- Spawn --
      -----------

      procedure Spawn
        (Driver   : String;
         Options  : Argument_List;
         Out_File : String;
         Pid      : out Process_Id) is
      begin
         Pid := OS_Lib.Non_Blocking_Spawn (Driver, Options, Out_File);
      end Spawn;

   end IO;

   ------------------------
   -- Parse_Command_Line --
   ------------------------

   procedure Parse_Command_Line is
      use GNAT.Command_Line;

      procedure Usage;

      procedure Check_Version_And_Help is new
        Check_Version_And_Help_G (Usage);

      Config : Command_Line_Configuration;

      -----------
      -- Usage --
      -----------

      procedure Usage is
      begin
         Display_Help (Config);
      end Usage;

   begin
      Define_Switch
        (Config, Help'Access,
         "-h", Long_Switch => "--help",
         Help => "display this help message and exit");

      Define_Switch
        (Config, Verbose'Access,
         "-V", Long_Switch => "--version",
         Help => "display version and exit");

      Define_Switch
        (Config, Max_Processes'Access,
         "-j:", Long_Switch => "--jobs=",
         Initial => Integer (Multiprocessors.Number_Of_CPUs),
         Default => Integer (Multiprocessors.Number_Of_CPUs),
         Help    => "set the maximum simultaneous compilation");

      Define_Switch
        (Config, Max_Responses'Access,
         "-r:", Long_Switch => "--response-handler=",
         Initial => Integer (2),
         Default => Integer (2),
         Help    => "maximum number of simultaneous responses sent back");

      Define_Switch
        (Config, Root_Directory'Access,
         "-d:", Long_Switch => "--directory=",
         Help => "set the root directory");

      Define_Switch
        (Config, Port'Access,
         "-p:", Long_Switch => "--port=",
         Initial => Integer (Default_Port),
         Default => Integer (Default_Port),
         Help    => "set the port the slave will listen to");

      Define_Switch
        (Config, Verbose'Access,
         "-v", Long_Switch => "--verbose",
         Help => "verbose mode, display extra information");

      Define_Switch
        (Config, Debug'Access,
         "-vv", Long_Switch => "--debug",
         Help => "debug mode, display lot of information (imply -v)");

      Set_Usage (Config, Usage => "[switches]");

      Check_Version_And_Help
        ("GPRSLAVE",
         "2013",
         Version_String => GPR_Version.Gpr_Version_String);

      Getopt (Config);

      if Debug then
         Verbose := True;
      end if;

      --  To avoid error messages for unknown languages that are not described
      --  in the XML database, use the quiet mode if Verbose is not set.

      if not Verbose then
         Opt.Quiet_Output := True;
      end if;

      --  First ensure Root_Directory is an absolute path-name. This is
      --  needed to be able to create directory for a specific builder without
      --  enforcing that the current directory be in a critical section.
      --  Indeed, it is then possible to create a directory under this
      --  absolute path-name directly.

      if not Is_Absolute_Path (Root_Directory.all) then

         --  Not an absolute path, this means that we have passed a directory
         --  relative to the current directory with option -d/--directory.

         declare
            RD : constant String := Root_Directory.all;
         begin
            Free (Root_Directory);
            Root_Directory :=
              new String'(Ensure_Directory (Current_Directory) & RD);
         end;
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
      when Invalid_Switch =>
         OS_Exit (1);

      when Exit_From_Command_Line =>
         OS_Exit (1);
   end Parse_Command_Line;

   -------------------
   -- Wait_Requests --
   -------------------

   task body Wait_Requests is

      type Job_Number is mod 2**32;
      --  A 32bits integer which wrap around. This is no problem as we want
      --  to be able to identify running process. There won't be 2**32 process
      --  running at the same time. So it is safe restart numbering at 0.

      Selector     : Selector_Type;
      R_Socket_Set : Socket_Set_Type;
      E_Socket_Set : Socket_Set_Type;
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

         Copy (R_Socket_Set, E_Socket_Set);

         Wait_Incoming_Data : loop
            begin
               Check_Selector
                 (Selector, R_Socket_Set, Empty_Set, E_Socket_Set, Status);
               exit Wait_Incoming_Data;
            exception
               when E : Socket_Error =>
                  if Resolve_Exception (E) /= Interrupted_System_Call then
                     Status := Aborted;
                     exit Wait_Incoming_Data;
                  end if;
            end;
         end loop Wait_Incoming_Data;

         if Status /= Aborted then
            --  Check for socket error first, if a socket is in error just
            --  close the builder and remove it from the list. From there
            --  we abort any action.

            Get (E_Socket_Set, Socket);

            if Socket /= No_Socket then
               Builder := Builders.Get (Socket);
               IO.Message
                 (Builder, "# Error socket signaled", Is_Debug => True);
               Status := Aborted;
            end if;
         end if;

         if Status = Aborted then
            --  Either the selector has been aborted or the Socket was not
            --  found in the response. We can suppose that in this case the
            --  client is killed and we do not have to keep it in the registry.

            Get (R_Socket_Set, Socket);

            if Socket /= No_Socket then
               Builder := Builders.Get (Socket);
               Close_Builder (Builder, Ack => False);
            end if;

         else
            Get (R_Socket_Set, Socket);

            if Socket /= No_Socket then
               Builder := Builders.Get (Socket);

               if Is_Active_Build_Master (Builder) then
                  Builders.Lock (Builder);

                  declare
                     Cmd : constant Command := Get_Command (Builder.Channel);
                     V   : Unbounded_String;
                  begin
                     if Debug then
                        V := To_Unbounded_String
                          ("# command: " & Command_Kind'Image (Kind (Cmd)));

                        declare
                           List : constant Argument_List_Access := Args (Cmd);
                        begin
                           if List /= null then
                              for K in List'Range loop
                                 Append (V, ", " & List (K).all);
                              end loop;
                           end if;
                        end;

                        IO.Message (Builder, To_String (V), Is_Debug => True);
                     end if;

                     if Kind (Cmd) = EX then
                        Record_Job : declare
                           Id : constant Remote_Id :=
                                  Slave_Id + Remote_Id (Jid);
                           --  Note that the Id above should be unique across
                           --  all running slaves. This is not the process
                           --  id, but an id sent back to the build master
                           --  to identify the actual job.
                        begin
                           Jid := Jid + 1;
                           IO.Message
                             (Builder,
                              "# register compilation " & Image (Id), True);

                           To_Run.Push
                             (Job_Data'(Cmd,
                              Id, OS_Lib.Invalid_Pid,
                              Null_Unbounded_String,
                              Null_Unbounded_String,
                              Null_Unbounded_String,
                              Null_Unbounded_String,
                              Builder.Socket, False));

                           Send_Ack (Builder.Channel, Id);
                        end Record_Job;

                     elsif Kind (Cmd) = FL then
                        null;

                     elsif Kind (Cmd) = CU then
                        Clean_Up_Request : begin

                           To_Run.Push
                             (Job_Data'(Cmd,
                              0, OS_Lib.Invalid_Pid,
                              Null_Unbounded_String,
                              Null_Unbounded_String,
                              Null_Unbounded_String,
                              Null_Unbounded_String,
                              Builder.Socket, False));
                        end Clean_Up_Request;

                     elsif Kind (Cmd) in EC | SI then
                        --  No more compilation for this project. Send an
                        --  Ack only if we are not handling a kill signal
                        --  (receiving SI means that the socket has been
                        --  detected to be closed).

                        Close_Builder (Builder, Ack => (Kind (Cmd) = EC));

                        IO.Message
                          (Builder,
                           "End project : "
                           & To_String (Builder.Project_Name));

                     else
                        raise Constraint_Error with "unexpected command "
                          & Command_Kind'Image (Kind (Cmd));
                     end if;

                  exception
                     when Socket_Error =>
                        --  The build master has probably been killed. We
                        --  cannot communicate with it. Just close the channel.

                        Close_Builder (Builder, Ack => False);

                        IO.Message
                          (Builder,
                           "Interrupted project : "
                           & To_String (Builder.Project_Name));

                     when E : others =>
                        IO.Message
                          (Builder,
                           "Error: "
                           & Exception_Information (E), Force => True);

                        --  In case of an exception, communication endded
                        --  prematurately or some wrong command received, make
                        --  sure we clean the slave state and we listen to new
                        --  commands. Not doing that could make the slave
                        --  unresponding.

                        Close_Builder (Builder, Ack => False);
                  end;

                  --  The lock is released and freed if we have an EC command

                  Builders.Release (Builder);

               else
                  IO.Message
                    ("# build master not found, cannot handle request.",
                     Is_Debug => True);
               end if;
            end if;
         end if;
      end loop Handle_Commands;

   exception
      when E : others =>
         IO.Message
           (Builder, "Unrecoverable error: Protocol_Handler.", Force => True);
         IO.Message (Builder, Symbolic_Traceback (E), Force => True);
         OS_Exit (1);
   end Wait_Requests;

   -----------------
   -- Execute_Job --
   -----------------

   task body Execute_Job is

      function Get_Driver
        (Builder : Build_Master; Language, Project : String) return String;
      --  Returns the compiler driver for the given language and the current
      --  target as retreived from the initial handshake context exchange.

      function Get_Output_File (Builder : Build_Master) return String;
      --  Returns a unique output file

      procedure Output_Compilation (Builder : Build_Master; File : String);
      --  Output compilation information

      procedure Do_Compile (Job : in out Job_Data);
      --  Run a compilation job

      procedure Do_Clean (Job : Job_Data);
      --  Run  a clean job

      package Drivers_Cache is new Containers.Indefinite_Hashed_Maps
        (String, String,
         Ada.Strings.Hash_Case_Insensitive,
         Ada.Strings.Equal_Case_Insensitive);

      Cache : Drivers_Cache.Map;

      ----------------
      -- Get_Driver --
      ----------------

      function Get_Driver
        (Builder : Build_Master; Language, Project : String) return String
      is
         procedure Look_Driver (Project_Name : String; Is_Config : Boolean);
         --  Set Driver with the found driver for the Language

         Key                : constant String :=
                                To_String (Builder.Target) & '+' & Language;
         Position           : constant Drivers_Cache.Cursor :=
                                Cache.Find (Key);
         Compilers, Filters : Compiler_Lists.List;
         Requires_Comp      : Boolean;
         Comp               : Compiler_Access;
         Env                : Environment;
         Success            : Boolean;
         Driver             : Unbounded_String := To_Unbounded_String (Key);

         -----------------
         -- Look_Driver --
         -----------------

         procedure Look_Driver (Project_Name : String; Is_Config : Boolean) is
            Project_Node_Tree : Project_Node_Tree_Ref;
            Project_Node      : Project_Node_Id := Empty_Project_Node;
            Project_Tree      : Project_Tree_Ref;
            Project           : Project_Id;
         begin
            Project_Node_Tree := new Project_Node_Tree_Data;
            GPR.Tree.Initialize (Project_Node_Tree);

            GPR.Part.Parse
              (Project_Node_Tree, Project_Node,
               Project_Name,
               Errout_Handling   => GPR.Part.Finalize_If_Error,
               Packages_To_Check => null,
               Is_Config_File    => Is_Config,
               Target_Name       => To_String (Builder.Target),
               Env               => Env);

            Project_Tree := new Project_Tree_Data;
            GPR.Initialize (Project_Tree);

            Proc.Process
              (Project_Tree, Project, null, Success,
               Project_Node, Project_Node_Tree, Env);

            if not Success then
               return;
            end if;

            declare
               Pcks : Package_Table.Table_Ptr
                        renames Project_Tree.Shared.Packages.Table;
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
            end;

         exception
            --  Never propagate an exception, the driver won't be set anyway
            when others =>
               null;
         end Look_Driver;

      begin
         if Drivers_Cache.Has_Element (Position) then
            return Drivers_Cache.Element (Position);

         else
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

            --  Generate configuration project file

            Generate_Configuration
              (Base, Compilers, "slave_tmp.cgpr", To_String (Builder.Target));

            GPR.Tree.Initialize (Env, GPR.Gprbuild_Flags);
            GPR.Initialize (GPR.No_Project_Tree);

            GPR.Env.Initialize_Default_Project_Path
              (Env.Project_Path, Target_Name => To_String (Builder.Target));

            --  Parse it to find the driver for this language

            Look_Driver ("slave_tmp.cgpr", Is_Config => True);
            Directories.Delete_File ("slave_tmp.cgpr");

            --  Language is not found in the knowledge base, check the project
            --  to see if there is a definition for the language.

            if Driver = Key then
               Look_Driver (Project, Is_Config => False);

               --  Ensure that we have a full-path name
               declare
                  Exe : OS_Lib.String_Access :=
                          Locate_Exec_On_Path (To_String (Driver));
               begin
                  Driver := To_Unbounded_String (Exe.all);
                  Free (Exe);
               end;
            end if;

            --  Record this driver for the language and target into the cache

            Cache.Insert (Key, To_String (Driver));

            --  Clean-up and free project structure

            IO.Message
              (Builder,
               "# driver for " & Language & " is : " & To_String (Driver),
               Is_Debug => True);

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

      procedure Output_Compilation
        (Builder : Build_Master;
         File    : String)
      is

         function Prefix return String;
         --  Returns a prefix for the display with a progress indication

         ------------
         -- Prefix --
         ------------

         function Prefix return String is
            Active : constant String := Natural'Image (Running.Count + 1);
            Max    : constant String := Natural'Image (Max_Processes);
         begin
            return "Compiling (" & Active (Active'First + 1 .. Active'Last)
              & '/' & Max (Max'First + 1 .. Max'Last) & ") : ";
         end Prefix;

         RDL : constant Natural := Root_Directory'Length;

      begin
         if Verbose then
            if File'Length > RDL
              and then File (File'First .. File'First + RDL - 1)
              = Root_Directory.all
            then
               IO.Message
                 (Builder,
                  Prefix & File (File'First + RDL + 1 .. File'Last));
            else
               IO.Message (Builder, Prefix & File);
            end if;
         end if;
      end Output_Compilation;

      ----------------
      -- Do_Compile --
      ----------------

      procedure Do_Compile (Job : in out Job_Data) is
         Builder : constant Build_Master := Builders.Get (Job.Build_Sock);
         Dir     : constant String := Args (Job.Cmd)(2).all;
         List    : Slice_Set;
         Pid     : Process_Id;
      begin
         --  Enter a critical section to:
         --     - move to directory where the command is executed
         --     - execute the compilation command
         --     - register a new job and acknowledge
         --     - move back to working directory

         IO.Message
           (Builder, "# move to work directory " & Work_Directory (Builder),
            Is_Debug => True);

         --  It is safe to change directory here without a lock as this is
         --  the only place where it happens and there is a single instance
         --  of this task.

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
         --
         --  Note that the following block should never fail otherwise the
         --  process won't be started. Even if we know the compilation will
         --  fail we need to move forward as the result for this compilation
         --  is waited for by the build master.

         begin
            if Dir /= "" then
               if not Is_Absolute_Path (Dir)
                 and then not Is_Directory (Dir)
               then
                  Create_Directory (Dir);
               end if;

               IO.Message
                 (Builder, "# move to directory " & Dir, Is_Debug => True);

               Set_Directory (Dir);
            end if;
         exception
            when others =>
               IO.Message
                 (Builder, "# cannot move to object directory",
                  Is_Debug => True);
         end;

         Create (List, Args (Job.Cmd) (6).all, String'(1 => Opts_Sep));

         Execute  : declare
            Project  : constant String :=
                         Get_Arg (Builder, Args (Job.Cmd) (1).all);
            Language : constant String := Args (Job.Cmd) (3).all;
            Out_File : constant String :=
                         Get_Output_File (Builder);
            Obj_File : constant String := Args (Job.Cmd) (4).all;
            Dep_File : constant String := Args (Job.Cmd) (5).all;
            Env      : constant String :=
                         Get_Arg (Builder, Args (Job.Cmd) (7).all);
            O        : Argument_List := Get_Args (Builder, List);
         begin
            Output_Compilation (Builder, O (O'Last).all);

            --  Set compiler environment

            Set_Env (Env, Fail => False, Force => True);

            --  It is critical to ensure that no IO is done while spawning
            --  the process.

            IO.Spawn
              (Get_Driver (Builder, Language, Project), O, Out_File, Pid);

            IO.Message
              (Builder, "#   pid" & Integer'Image (Pid_To_Integer (Pid)),
               Is_Debug => True);
            IO.Message (Builder, "#   dep_file " & Dep_File, Is_Debug => True);
            IO.Message (Builder, "#   out_file " & Out_File, Is_Debug => True);
            IO.Message (Builder, "#   obj_file " & Obj_File, Is_Debug => True);

            Job.Pid      := Pid;
            Job.Dep_File := To_Unbounded_String (Dep_File);
            Job.Obj_File := To_Unbounded_String (Obj_File);
            Job.Output   := To_Unbounded_String (Out_File);
            Job.Dep_Dir  := To_Unbounded_String
              ((if Is_Absolute_Path (Dir) then "" else Dir));

            --  Note that we want to register the job even if Pid is
            --  Invalid_Process. We want it to be recorded into the running
            --  process to be able to be retrieved by the Wait_Completion
            --  task and a proper NOK message to be sent to the builder.

            Running.Register (Job);

            for K in O'Range loop
               Free (O (K));
            end loop;
         end Execute;
      exception
         when E : others =>
            IO.Message
              (Builder,
               "# Error in Execute_Job: " & Symbolic_Traceback (E),
               Is_Debug => True);
      end Do_Compile;

      --------------
      -- Do_Clean --
      --------------

      procedure Do_Clean (Job : Job_Data) is
         Builder : Build_Master := Builders.Get (Job.Build_Sock);
      begin
         Builder.Project_Name :=
           To_Unbounded_String (Args (Job.Cmd)(1).all);

         declare
            WD : constant String := Work_Directory (Builder);
         begin
            if Exists (WD) then
               IO.Message (Builder, "Delete " & WD);

               --  Cannot delete if the process is still under
               --  the working directory, so move to the slave
               --  root directory.

               Set_Directory (Root_Directory.all);

               Delete_Tree (WD);
            end if;
         end;

         Send_Ok (Builder.Channel);
      exception
         when E : others =>
            IO.Message
              (Builder,
               "# clean-up error " & Symbolic_Traceback (E),
               True);
            Send_Ko (Builder.Channel);
      end Do_Clean;

      Job : Job_Data;
   begin
      loop
         --  Launch a new compilation only if the maximum of simultaneous
         --  process has not yet been reached.

         Running.Wait_Slot;

         To_Run.Pop (Job);

         --  Only launch the job if the corresponding builder is still active.
         --  It could be the case that the builder has been interrupted
         --  (ctrl-c) and so removed from the set.

         if Builders.Exists (Job.Build_Sock) then
            if Kind (Job.Cmd) = EX then
               Do_Compile (Job);
            else
               Do_Clean (Job);
            end if;
         end if;
      end loop;

   exception
      when E : others =>
         IO.Message ("Unrecoverable error: Execute_Job.", Force => True);
         IO.Message (Exception_Information (E), Force => True);
         OS_Exit (1);
   end Execute_Job;

   -------------
   -- Running --
   -------------

   protected body Running is

      -----------
      -- Count --
      -----------

      function Count return Natural is
      begin
         return N_Count;
      end Count;

      --------------------
      -- Kill_Processes --
      --------------------

      procedure Kill_Processes (Socket : Socket_Type) is
         To_Kill : Job_Data_Set.Set;
         C       : Job_Data_Set.Cursor;
      begin
         --  First pass, record all job for the given builder

         for Job of Set loop
            if Job.Build_Sock = Socket then
               To_Kill.Insert (Job);
            end if;
         end loop;

         --  Second pass, kill processes and mark them as killed. Those jobs
         --  are interrupted and the builder removed, so there is no point to
         --  try to send back the compilation result to the master.
         --
         --  This also ensure a faster termination of the build master.

         for Job of To_Kill loop
            --  Mark job as killed into the set
            C := Set.Find (Job);
            Set (C).Killed := True;

            Kill_Process_Tree (Job.Pid, Hard_Kill => True);
            IO.Message
              ("# kill job" & Integer'Image (Pid_To_Integer (Job.Pid)),
               Is_Debug => True);
         end loop;
      end Kill_Processes;

      --------------
      -- Register --
      --------------

      procedure Register (Job : Job_Data) is
      begin
         --  Let's ensure that while the job was prepared the builder was not
         --  hard-killed. If so we kill the process right now. The result won't
         --  be used anyway and we do not want it to linger here and possibly
         --  corrupt a new launched compilation for the same object file.
         --
         --  Note that it is still inserted into the job set for the job exit
         --  status to be read. This ensure that the job is properly terminated
         --  by the OS (on Linux the process would stay as <defunct> for
         --  example).

         if not Builders.Exists (Job.Build_Sock) then
            IO.Message
              ("# kill job (missing builder)"
               & Integer'Image (Pid_To_Integer (Job.Pid)),
               Is_Debug => True);

            Kill (Job.Pid, Hard_Kill => True);

            Insert_Killed_Job : declare
               Killed_Job : Job_Data := Job;
            begin
               Killed_Job.Killed := True;
               Set.Insert (Killed_Job);
            end Insert_Killed_Job;

         elsif Job.Pid = OS_Lib.Invalid_Pid then
            Dead.Insert (Job);
         else
            Set.Insert (Job);
         end if;

         N_Count := N_Count + 1;
      end Register;

      ---------
      -- Get --
      ---------

      procedure Get (Job : out Job_Data; Pid : Process_Id) is
         Pos : Job_Data_Set.Cursor;
      begin
         if Dead.Is_Empty then
            Job.Pid := Pid;
            Pos := Set.Find (Job);

            --  Not that a job could be not found here because the Pid is one
            --  of gprconfig runned to generate a configuration file for a
            --  specific language.

            if Job_Data_Set.Has_Element (Pos) then
               Job := Job_Data_Set.Element (Pos);
               Set.Delete (Job);
               N_Count := N_Count - 1;

               --  If this is a job which has been killed (see Kill_Processes
               --  above), set to No_Job. We do this as the Wait_Completion
               --  task must not do anything with such a process (no need to
               --  send back answers as anyway the build master is not running
               --  anymore).

               if Job.Killed then
                  Job := No_Job;
               end if;

            else
               Job := No_Job;
            end if;

         else
            Job := Dead.First_Element;
            Dead.Delete_First;
            N_Count := N_Count - 1;
         end if;
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
         --  Always adds the clean-up job in front of the queue, this is
         --  friendler as we do not want the user to wait for all current
         --  compilation to terminate.

         if Kind (Job.Cmd) = CU then
            Set.Prepend (Job);
         else
            Set.Append (Job);
         end if;
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

      Pid     : Process_Id;
      Success : Boolean;
      Job     : Job_Data;
      Builder : Build_Master;

   begin
      loop
         --  Wait for a job to complete only if there is job running

         Running.Wait;

         Wait_Process (Pid, Success);

         --  If a "dead" jobs is returned success is forced to False

         if Pid = OS_Lib.Invalid_Pid then
            Success := False;
         end if;

         Running.Get (Job, Pid);

         --  Note that if there is not such element it could be because the
         --  build master has been killed before the end of the compilation.
         --  In this case an EC message is received by the slave and the
         --  Job_Set is clear. See Main_Loop in gprslave's body.

         if Job /= No_Job then
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

            if Is_Active_Build_Master (Builder) then
               Builders.Lock (Builder);

               begin
                  IO.Message
                    (Builder,
                     "# job " & Image (Job.Id) & " terminated",
                     Is_Debug => True);

                  declare
                     DS       : Character renames Directory_Separator;
                     Dep_Dir  : constant String := To_String (Job.Dep_Dir);
                     Dep_File : constant String := To_String (Job.Dep_File);
                     Obj_File : constant String := To_String (Job.Obj_File);
                     Out_File : constant String := To_String (Job.Output);
                     S        : Boolean;
                  begin
                     if Exists (Out_File) then
                        Send_Output (Builder.Channel, Out_File);
                     end if;

                     OS_Lib.Delete_File (Out_File, S);

                     if Success then
                        --  No Dep_File to send back if the compilation was not
                        --  successful.

                        declare
                           D_File : constant String :=
                                      Work_Directory (Builder)
                                    & (if Dep_Dir /= ""
                                       then DS & Dep_Dir else "")
                                    & DS & Dep_File;
                        begin
                           if Exists (D_File)
                             and then Kind (D_File) = Ordinary_File
                           then
                              Send_File
                                (Builder.Channel, D_File, Rewrite => True);
                           end if;
                        end;

                        declare
                           O_File : constant String :=
                                      Work_Directory (Builder)
                                    & (if Dep_Dir /= ""
                                       then DS & Dep_Dir else "")
                                    & DS & Obj_File;
                        begin
                           if Exists (O_File) then
                              Send_File
                                (Builder.Channel, O_File, Rewrite => False);
                           end if;
                        end;
                     end if;
                  end;

                  IO.Message
                    (Builder,
                     "# compilation status " & Boolean'Image (Success),
                     Is_Debug => True);

                  if Success then
                     Send_Ok (Builder.Channel, Job.Id);
                  else
                     Send_Ko (Builder.Channel, Job.Id);
                  end if;

                  Builders.Release (Builder);

               exception
                  when E : others =>
                     --  An exception can be raised if the builder master has
                     --  been terminated. In this case the communication won't
                     --  succeed.

                     IO.Message
                       (Builder,
                        "# cannot send response to build master "
                        & Exception_Information (E),
                        Is_Debug => True);

                     --  Remove it from the list

                     Builders.Remove (Builder);
               end;

            else
               IO.Message
                 ("# build master not found, cannot send response.",
                  Is_Debug => True);
            end if;

         else
            --  This is not necessarily an error as we could get a Pid of a
            --  gprconfig run launched to generate a configuration file for a
            --  specific language. So we do not want to fail in this case.

            IO.Message ("# unknown job data for pid "
                     & Integer'Image (Pid_To_Integer (Pid)), Is_Debug => True);
         end if;
      end loop;

   exception
      when E : others =>
         Put_Line ("Unrecoverable error: Wait_Completion.");
         Put_Line (Symbolic_Traceback (E));
         OS_Exit (1);
   end Wait_Completion;

   ---------------------
   -- Wait_For_Master --
   ---------------------

   procedure Wait_For_Master is
      use Stamps;

      procedure Sync_Gpr (Builder : in out Build_Master);

      --------------
      -- Sync_Gpr --
      --------------

      procedure Sync_Gpr (Builder : in out Build_Master) is

         use type Containers.Count_Type;

         package Files is new Containers.Indefinite_Ordered_Sets (String);

         procedure Delete_Files (Except : Files.Set);
         --  Delete all files in the current working tree except those in
         --  Except set.

         WD : constant String := Work_Directory (Builder);

         ------------------
         -- Delete_Files --
         ------------------

         procedure Delete_Files (Except : Files.Set) is

            procedure Process (Path : String);
            --  Search recursively the Path

            procedure Process (Path : String) is

               procedure Check (File : Directory_Entry_Type);
               --  Remove this file if not part of Except set

               -----------
               -- Check --
               -----------

               procedure Check (File : Directory_Entry_Type) is
                  S_Name     : constant String := Simple_Name (File);
                  Entry_Name : constant String :=
                                 Path & Directory_Separator & S_Name;
               begin
                  if Kind (File) = Directory then
                     if S_Name not in "." | ".."
                       and then not Is_Symbolic_Link (Entry_Name)
                     then
                        Process (Entry_Name);
                     end if;

                  else
                     if not Except.Contains (Entry_Name) then
                        IO.Message
                          (Builder,
                           "# delete excluded '" & Entry_Name & ''',
                           Is_Debug => True);

                        Delete_File (Entry_Name);
                     end if;
                  end if;
               end Check;

            begin
               Search
                 (Directory => Path,
                  Pattern   => "*",
                  Filter    => (Special_File => False, others => True),
                  Process   => Check'Access);
            end Process;

         begin
            Process (WD);
         end Delete_Files;

         Total_File        : Natural := 0;
         Total_Transferred : Natural := 0;
         In_Master         : Files.Set;

      begin
         Check_Time_Stamps : loop
            declare
               To_Sync : File_Data_Set.Vector;
               Cmd     : Command;
               K       : Positive := 1;
            begin
               Cmd := Get_Command (Builder.Channel);

               if Debug then
                  Put ("# command: " & Command_Kind'Image (Kind (Cmd)));

                  if Args (Cmd) /= null then
                     for K in Args (Cmd)'Range loop
                        Put (", " & Args (Cmd) (K).all);
                     end loop;
                  end if;
                  New_Line;
               end if;

               if Kind (Cmd) = TS then
                  --  Check all files in the argument of the command. This is a
                  --  list of couple (filename and time stamp).

                  Check_All_Files : loop
                     Total_File := Total_File + 1;

                     declare
                        Path_Name  : constant String := Args (Cmd) (K).all;
                        Full_Path  : constant String :=
                                       WD & Directory_Separator & Path_Name;
                        TS         : constant Time_Stamp_Type :=
                                       Time_Stamp_Type
                                         (Args (Cmd) (K + 1).all);
                        File_Stamp : Time_Stamp_Type;
                        Exists     : Boolean;
                     begin
                        if Directories.Exists (Full_Path) then
                           File_Stamp :=
                             To_Time_Stamp
                             (Modification_Time (Full_Path)
                              - Duration (Time_Zones.UTC_Time_Offset) * 60.0);
                           Exists := True;
                        else
                           Exists := False;
                        end if;

                        In_Master.Insert (Full_Path);

                        if not Exists or else File_Stamp /= TS then
                           To_Sync.Append
                             (File_Data'
                                (To_Unbounded_String (Path_Name), TS));
                        end if;
                     end;

                     K := K + 2;
                     exit Check_All_Files when K > Args (Cmd)'Length;
                  end loop Check_All_Files;

                  --  If all files are up-to-data

                  if To_Sync.Length = 0 then
                     Send_Ok (Builder.Channel);

                  else
                     --  Some files are to be synchronized, send the list of
                     --  names back to the master.

                     Send_Ko (Builder.Channel, To_Sync);

                     --  We then receive the files contents in the same order

                     Get_RAW_Data : declare
                        Max : constant String :=
                                Containers.Count_Type'Image (To_Sync.Length);
                        N   : Natural := 0;
                     begin
                        for W of To_Sync loop
                           declare
                              Full_Path : constant String :=
                                            WD & Directory_Separator
                                            & To_String (W.Path_Name);
                           begin
                              Create_Path (Containing_Directory (Full_Path));

                              Get_RAW_File_Content
                                (Builder.Channel, Full_Path, W.Timestamp);
                           end;

                           N := N + 1;

                           if N mod 100 = 0 then
                              IO.Message
                                (Builder,
                                 "File transfered"
                                 & Natural'Image (N) & "/" & Max);
                           end if;
                        end loop;
                     end Get_RAW_Data;

                     Total_Transferred :=
                       Total_Transferred + Natural (To_Sync.Length);
                  end if;

               elsif Kind (Cmd) = ES then
                  --  Delete all files not part of the list sent by the master.
                  --  This is needed to remove files in previous build removed
                  --  since then on the master. Again we need to do that as we
                  --  can't let around unnedded specs or bodies.

                  Delete_Files (Except => In_Master);

                  exit Check_Time_Stamps;

               elsif Kind (Cmd) = EC then
                  --  Cannot communicate with build master anymore, we then
                  --  receive an end-of-compilation. Exit now. Note that we do
                  --  not need to remove the builder from the list as it is not
                  --  yet registered.

                  Send_Ok (Builder.Channel);

                  Close (Builder.Channel);
                  Close_Socket (Builder.Socket);
                  Builder.Socket := No_Socket;

                  exit Check_Time_Stamps;

               end if;
            end;
         end loop Check_Time_Stamps;

         IO.Message (Builder, "Files    total:" & Natural'Image (Total_File));
         IO.Message
           (Builder, "  transferred :" & Natural'Image (Total_Transferred));

      exception
         when others =>
            IO.Message (Builder, "Lost connection with " & Image (Address));
            Close (Builder.Channel);
            Close_Socket (Builder.Socket);
            Builder.Socket := No_Socket;
      end Sync_Gpr;

      Builder      : Build_Master;
      Clock_Status : Boolean;

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

      --  We must call explicitely Initialize here to ensure that the Builder
      --  object Status access will be changed for this new builder.

      Controlled_Build_Master.Initialize (Builder);

      --  Then initialize the new builder Id

      Builders.Initialize (Builder);

      IO.Message (Builder, "Connecting with " & Image (Address));

      --  Initial handshake

      declare
         Master_Timestamp : Time_Stamp_Type;
         Version          : Unbounded_String;
      begin
         Get_Context
           (Builder.Channel, Builder.Target,
            Builder.Project_Name, Builder.Build_Env, Builder.Sync,
            Master_Timestamp, Version);

         Clock_Status := Check_Diff (Master_Timestamp, UTC_Time);

         if To_String (Version) /= GPR_Version.Gpr_Version_String then
            IO.Message
              (Builder, "Reject non compatible build for "
               & To_String (Builder.Project_Name));
            Send_Ko (Builder.Channel);
            return;
         end if;

         if Builders.Working_Dir_Exists (Work_Directory (Builder)) then
            IO.Message
              (Builder, "Cannot use the same build environment for "
               & To_String (Builder.Project_Name));
            Send_Ko
              (Builder.Channel,
               "build environment "
               & To_String (Builder.Build_Env) & " already in use");
            return;
         end if;

      exception
         when E : others =>
            IO.Message (Builder, Exception_Information (E));
            --  Do not try to go further
            Send_Ko (Builder.Channel);
            return;
      end;

      Get_Targets_Set
        (Base, To_String (Builder.Target), Selected_Targets_Set);

      IO.Message
        (Builder, "Handling project : " & To_String (Builder.Project_Name));
      IO.Message (Builder, "Compiling for    : " & To_String (Builder.Target));

      --  Create slave environment if needed

      if not Exists (Work_Directory (Builder)) then
         IO.Message
           (Builder,
            "# create build environment directory: "
            & Work_Directory (Builder), Is_Debug => True);

         Create_Path (Work_Directory (Builder));
      end if;

      --  Configure slave, note that this does not need to be into the critical
      --  section has the builder is not yet known in the system. At this point
      --  no compilation can be received for this slave anyway.

      Set_Rewrite_WD
        (Builder.Channel, Path => Work_Directory (Builder));

      --  For Ada compilers, rewrite the root directory

      if Compiler_Path = null then
         IO.Message (Builder, "# compiler path is null.", Is_Debug => True);
      else
         IO.Message
           (Builder,
            "# compiler path is : "
            & Containing_Directory (Containing_Directory (Compiler_Path.all)),
            Is_Debug => True);
      end if;

      if Compiler_Path /= null then
         Set_Rewrite_CD
           (Builder.Channel,
            Path => Containing_Directory
              (Containing_Directory (Compiler_Path.all)));
      end if;

      --  It is safe to write to this builder outside of a lock here as this
      --  builder is not yet registered into the slave.

      Send_Slave_Config
        (Builder.Channel, Max_Processes,
         Compose (Root_Directory.all, To_String (Builder.Build_Env)),
         Clock_Status);

      --  If we are using the Gpr synchronisation, it is time to do it here.
      --  Note that we want to avoid the rewriting rules below that are
      --  requiring some CPU cycles not needed at this stage.

      if Builder.Sync then
         --  Move to projet directory
         Sync_Gpr (Builder);
      end if;

      --  Register the new builder

      if Builder.Socket /= No_Socket then
         Builders.Insert (Builder);
      end if;

   exception
      when E : others =>
         IO.Message
           (Builder, "Unrecoverable error: Wait_For_Master.", Force => True);
         IO.Message (Builder, Symbolic_Traceback (E), Force => True);
         OS_Exit (1);
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

   Snames.Initialize;

   Parse_Knowledge_Base (Base, Default_Knowledge_Base_Directory);

   Activate_Symbolic_Traceback;

   --  Always create the lib/object directories on the slave, this is needed
   --  when parsing a projet file to retreive a specific driver.

   Opt.Setup_Projects := True;

   --  Setup the response handlers

   if Max_Responses < 1 then
      Max_Responses := 1;
   elsif Max_Responses > Max_Processes then
      Max_Responses := Max_Processes;
   end if;

   Response_Handlers := new Response_Handler_Set (1 .. Max_Responses);

   --  Wait for a gprbuild connection on any addresses

   Address.Addr := Any_Inet_Addr;
   Address.Port := Port_Type (Port);

   Create_Socket (Server);

   Set_Socket_Option (Server, Socket_Level, (Reuse_Address, True));

   Bind_Socket (Server, Address);

   if Port = 0 then
      Address := Get_Socket_Name (Server);
   end if;

   Put_Line
     ("GPRSLAVE " & GPR_Version.Gpr_Version_String & " on " & Host_Name
      & ":" & Image (Long_Integer (Address.Port)));
   Put_Line ("  max processes :" & Integer'Image (Max_Processes));
   Put_Line ("  max responses :" & Integer'Image (Max_Responses));

   --  Initialize the host key used to create unique pid

   Slave_Id := Get_Slave_Id;

   IO.Message ("# slave id " & Image (Slave_Id), Is_Debug => True);

   Listen_Socket (Server);

   Main_Loop : loop
      Wait_For_Master;
   end loop Main_Loop;

exception
   when E : others =>
      IO.Message ("Unrecoverable error: GprSlave.", Force => True);
      IO.Message (Symbolic_Traceback (E), Force => True);
      OS_Exit (1);
end Gprslave;
