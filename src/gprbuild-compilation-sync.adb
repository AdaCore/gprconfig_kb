------------------------------------------------------------------------------
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--           G P R B U I L D . C O M P I L A T I O N . S L A V E            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2014, Free Software Foundation, Inc.            --
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

with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Vectors;

with Ada.Directories;         use Ada.Directories;
with Ada.Exceptions;          use Ada.Exceptions;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;

with GNAT.Regexp;             use GNAT.Regexp;

with Gpr_Util; use Gpr_Util;
with Output;   use Output;

package body Gprbuild.Compilation.Sync is

   use Ada;
   use type Containers.Count_Type;

   package Hosts_Set is
     new Containers.Indefinite_Ordered_Maps (Integer, String);

   procedure Wait_Rsync (N : Natural);
   --  Wait for N rsync processes. If one of the is in error exit

   function User_Host (User, Host : String) return String is
     (if User = "" then Host else User & '@' & Host);

   Default_Excluded_Patterns : Str_Vect.Vector;
   --  Default excluded patterns to use when in excluded mode as opposed to
   --  include mode where we describe the patterns to include specifically.

   Rsync_Cmd : constant GNAT.OS_Lib.String_Access :=
                 Locate_Exec_On_Path ("rsync");

   Rsync_Count : Natural := 0;
   --  The number of rsync process started, we need to wait for them to
   --  terminate.

   Max_Gpr_Sync : constant := 10;
   --  The number of parallele synchronization done for the gpr protocol. This
   --  is currenlty fixed to 6 but could probable be a parameter. The number is
   --  high, as these tasks are mostly doing IO and so are not CPU demanding,
   --  the goal is to saturate the network bandwidth.

   Hosts : Hosts_Set.Map;

   procedure To_Slave_Rsync
     (Project_Name      : String;
      Root_Dir          : String;
      Slave_Root_Dir    : String;
      User              : String;
      Host              : String;
      Excluded_Patterns : Str_Vect.Vector;
      Included_Patterns : Str_Vect.Vector);
   --  Rsync based synchronization to the slave

   procedure From_Slave_Rsync
     (Project_Name               : String;
      Root_Dir                   : String;
      Slave_Root_Dir             : String;
      User                       : String;
      Host                       : String;
      Included_Artifact_Patterns : Str_Vect.Vector);
   --  Rsync based synchronization from the slave

   procedure To_Slave_Gpr
     (Channel           : Protocol.Communication_Channel;
      Project_Name      : String;
      Root_Dir          : String;
      Slave_Root_Dir    : String;
      User              : String;
      Host              : String;
      Excluded_Patterns : Str_Vect.Vector;
      Included_Patterns : Str_Vect.Vector);
   --  Gpr based synchronization to the slave

   --  Data for each synchronization job for the Gpr protocol

   type Gpr_Data is record
      Channel           : Protocol.Communication_Channel;
      Project_Name      : Unbounded_String;
      Root_Dir          : Unbounded_String;
      Slave_Root_Dir    : Unbounded_String;
      User, Host        : Unbounded_String;
      Excluded_Patterns : Str_Vect.Vector;
      Included_Patterns : Str_Vect.Vector;
   end record;

   package Gpr_Data_Set is new Containers.Vectors (Positive, Gpr_Data);

   --  Queue of job to be done for the gpr protocol

   protected Gpr_Queue is

      procedure Add (Job : Gpr_Data);
      --  Add a new synchronization job

      entry Get
        (Job   : out Gpr_Data;
         Files : out File_Data_Set.Vector;
         Stop  : out Boolean);
      --  Get a synchronization job with the corresponding files, Stop is set
      --  to True if there is no more job to handle and False otherwise.

      procedure No_More_Job;

   private
      procedure Set_Project_Files (Job : Gpr_Data);
      --  Set the project files to be synchronized

      Jobs           : Gpr_Data_Set.Vector;
      Project_Files  : File_Data_Set.Vector;
      PF_Initialized : Boolean := False;
      No_More        : Boolean := False;
   end Gpr_Queue;

   --  Synchronization job are handled by the Gpr_Sync tasks

   task type Gpr_Sync is
      entry Stop;
   end Gpr_Sync;

   type Gpr_Sync_Tasks is array (1 .. Max_Gpr_Sync) of Gpr_Sync;
   type Sync_Tasks_Ref is access all Gpr_Sync_Tasks;

   Sync_Tasks : Sync_Tasks_Ref;
   --  Only allocated (and so started) if a some slaves are using the gpr
   --  protocol. Otherwise this variable will stay null.

   ----------------
   -- From_Slave --
   ----------------

   procedure From_Slave
     (Sync                       : Protocol.Sync_Kind;
      Project_Name               : String;
      Root_Dir                   : String;
      Slave_Root_Dir             : String;
      User                       : String;
      Host                       : String;
      Included_Artifact_Patterns : Str_Vect.Vector) is
   begin
      case Sync is
         when Protocol.Gpr =>
            --  Nothing to do as the artifacts are copied after each
            --  compilation.
            null;

         when Protocol.Rsync =>
            From_Slave_Rsync
              (Project_Name, Root_Dir, Slave_Root_Dir, User, Host,
               Included_Artifact_Patterns);
      end case;
   end From_Slave;

   ----------------------
   -- From_Slave_Rsync --
   ----------------------

   procedure From_Slave_Rsync
     (Project_Name               : String;
      Root_Dir                   : String;
      Slave_Root_Dir             : String;
      User                       : String;
      Host                       : String;
      Included_Artifact_Patterns : Str_Vect.Vector)
   is
      Args : Argument_List
        (1 ..
           6 + Positive'Max
             (4, Natural (Included_Artifact_Patterns.Length)));
      N    : Positive range 3 .. Args'Last := 3;
      Pid  : Process_Id;
   begin
      Rsync_Count := 0;

      --  Archive mode, compression and ignore VCS

      Args (1) := new String'("-az");
      Args (2) := new String'("--update");

      --  Check all subdirectories

      Args (3) := new String'("--include=*/");

      if Included_Artifact_Patterns.Length = 0 then
         --  Include known suffix (objects, dependencies)

         Args (4) := new String'("--include=*.o");
         Args (5) := new String'("--include=*.gli");
         Args (6) := new String'("--include=*.obj");
         Args (7) := new String'("--include=*.coff");

         N := 7;
      else
         for P of Included_Artifact_Patterns loop
            N := N + 1;
            Args (N) := new String'("--include=" & P);
         end loop;
      end if;

      --  Exclude everything else

      N := N + 1;
      Args (N) := new String'("--exclude=*");

      --  Local and remote directory

      N := N + 1;
      Args (N) := new String'
        (User_Host (User, Host)
         & ":" & Compose (Slave_Root_Dir, Project_Name) & "/");

      N := N + 1;
      Args (N) := new String'(Root_Dir);

      if Opt.Verbose_Mode then
         Write_Line ("  synchronize back data");
         Write_Line ("    from: " & Args (N - 1).all);
         Write_Line ("    to  : " & Args (N).all);
      end if;

      Pid := Non_Blocking_Spawn (Rsync_Cmd.all, Args (1 .. N));
      Hosts.Insert (Pid_To_Integer (Pid), Host);

      Rsync_Count := Rsync_Count + 1;

      for A of Args loop
         Free (A);
      end loop;
   end From_Slave_Rsync;

   ---------------
   -- Gpr_Queue --
   ---------------

   protected body Gpr_Queue is

      ---------
      -- Add --
      ---------

      procedure Add (Job : Gpr_Data) is
      begin
         Jobs.Append (Job);
      end Add;

      ---------
      -- Get --
      ---------

      entry Get
        (Job   : out Gpr_Data;
         Files : out File_Data_Set.Vector;
         Stop  : out Boolean) when Jobs.Length > 0 or No_More is
      begin
         if Jobs.Length = 0 and then No_More then
            Stop := True;

         else
            Stop := False;
            Job := Jobs.First_Element;
            Jobs.Delete_First;

            if not PF_Initialized then
               Set_Project_Files (Job);
            end if;

            Files := Project_Files;
         end if;
      end Get;

      -----------------
      -- No_More_Job --
      -----------------

      procedure No_More_Job is
      begin
         No_More := True;
      end No_More_Job;

      -----------------------
      -- Set_Project_Files --
      -----------------------

      procedure Set_Project_Files (Job : Gpr_Data) is

         Root_Dir : constant String :=
                      (if Job.Root_Dir = Null_Unbounded_String
                       then "." else To_String (Job.Root_Dir));

         type Regexp_Set is array (Containers.Count_Type range <>) of Regexp;

         I_Regexp : Regexp_Set (1 .. Job.Included_Patterns.Length);
         E_Regexp : Regexp_Set (1 .. Job.Excluded_Patterns.Length
                                       + Default_Excluded_Patterns.Length);

         procedure Process (Prefix : String);

         procedure Process (Prefix : String) is
            procedure Check (File : Directory_Entry_Type);
            --  Check and add this file if it passes the filters

            -----------
            -- Check --
            -----------

            procedure Check (File : Directory_Entry_Type) is
               use GNAT;

               function Match
                 (Name : String; R_Set : Regexp_Set)
                  return Boolean with Inline;
               --  Returns true if Name is matched by one of the regexp in
               --  R_Set.

               -----------
               -- Match --
               -----------

               function Match
                 (Name : String; R_Set : Regexp_Set) return Boolean is
               begin
                  for Regexp of R_Set loop
                     if Match (Name, Regexp) then
                        return True;
                     end if;
                  end loop;
                  return False;
               end Match;

               S_Name     : constant String := Simple_Name (File);
               Entry_Name : constant String := Prefix & S_Name;
               Is_File    : Boolean;

            begin
               if Kind (File) = Directory then
                  if S_Name not in "." | ".."
                    and then (I_Regexp'Length /= 0
                              or else not Match (S_Name, E_Regexp))
                    and then not Is_Symbolic_Link
                      (Root_Dir & Directory_Separator & Entry_Name)
                  then
                     Process (Entry_Name & Directory_Separator);
                  end if;

               else
                  if I_Regexp'Length = 0 then
                     if Match (S_Name, E_Regexp) then
                        Is_File := False;
                     else
                        Is_File := True;
                     end if;

                  else
                     if Match (S_Name, I_Regexp) then
                        Is_File := True;
                     else
                        Is_File := False;
                     end if;
                  end if;

                  if Is_File then
                     Project_Files.Append
                       (File_Data'
                          (To_Unbounded_String (Entry_Name),
                           To_Time_Stamp (Modification_Time (File)),
                           Dummy_Time_Stamp));
                  end if;
               end if;
            end Check;

         begin
            Search
              (Directory =>
                 Root_Dir
                 & (if Prefix = "" then "" else Directory_Separator & Prefix),
               Pattern   => "*",
               Filter    => (Special_File => False, others => True),
               Process   => Check'Access);
         end Process;

      begin
         --  Compile the patterns

         declare
            K : Containers.Count_Type := 1;
         begin
            for P of Job.Included_Patterns loop
               I_Regexp (K) := Compile (P, Glob => True);
               K := K + 1;
            end loop;

            K := 1;
            for P of Default_Excluded_Patterns loop
               E_Regexp (K) := Compile (P, Glob => True);
               K := K + 1;
            end loop;

            for P of Job.Excluded_Patterns loop
               E_Regexp (K) := Compile (P, Glob => True);
               K := K + 1;
            end loop;
         end;

         --  Check the files under the project root

         Process (Prefix => "");

         PF_Initialized := True;
      end Set_Project_Files;

   end Gpr_Queue;

   --------------
   -- Gpr_Sync --
   --------------

   task body Gpr_Sync is
      Job         : Gpr_Data;
      Files       : File_Data_Set.Vector;
      No_More_Job : Boolean;
   begin
      For_Slave : loop
         --  Get a new job and the associated files if any

         Gpr_Queue.Get (Job, Files, No_More_Job);

         exit For_Slave when No_More_Job;

         declare
            Chunk_Size : constant := 500;
            --  This constant controls the number of files sent with the sync
            --  command. Doing one at a time is really time consumming as
            --  we have for every file and send and a receive command on
            --  the socket.

            F_List     : File_Data_Set.Vector;
            Count      : Natural := 0;

         begin
            --  Synchronize each file in the list we got

            for F of Files loop
               if Count = Chunk_Size then
                  Protocol.Sync_Files
                    (Job.Channel, To_String (Job.Root_Dir), F_List);

                  F_List.Clear;
                  Count := 0;
               end if;

               F_List.Append (F);
               Count := Count + 1;
            end loop;

            --  Then send the last chunk if any

            if Count /= 0 then
               Protocol.Sync_Files
                 (Job.Channel, To_String (Job.Root_Dir), F_List);
            end if;

            Protocol.Send_End_Of_File_List (Job.Channel);
         end;
      end loop For_Slave;

      accept Stop;

   exception
      when E : others =>
         Write_Line (Exception_Information (E));
         OS_Exit (1);
   end Gpr_Sync;

   --------------
   -- To_Slave --
   --------------

   procedure To_Slave
     (Sync              : Protocol.Sync_Kind;
      Channel           : Protocol.Communication_Channel;
      Project_Name      : String;
      Root_Dir          : String;
      Slave_Root_Dir    : String;
      User              : String;
      Host              : String;
      Excluded_Patterns : Str_Vect.Vector;
      Included_Patterns : Str_Vect.Vector) is
   begin
      case Sync is
         when Protocol.Gpr =>
            To_Slave_Gpr
              (Channel, Project_Name, Root_Dir, Slave_Root_Dir, User, Host,
               Excluded_Patterns, Included_Patterns);

         when Protocol.Rsync =>
            To_Slave_Rsync
              (Project_Name, Root_Dir, Slave_Root_Dir, User, Host,
               Excluded_Patterns, Included_Patterns);
      end case;
   end To_Slave;

   ------------------
   -- To_Slave_Gpr --
   ------------------

   procedure To_Slave_Gpr
     (Channel           : Protocol.Communication_Channel;
      Project_Name      : String;
      Root_Dir          : String;
      Slave_Root_Dir    : String;
      User              : String;
      Host              : String;
      Excluded_Patterns : Str_Vect.Vector;
      Included_Patterns : Str_Vect.Vector) is
   begin
      --  Starts the tasks if not already done

      if Sync_Tasks = null then
         Sync_Tasks := new Gpr_Sync_Tasks;
      end if;

      Gpr_Queue.Add
        (Gpr_Data'
           (Channel,
            To_Unbounded_String (Project_Name),
            To_Unbounded_String (Root_Dir),
            To_Unbounded_String (Slave_Root_Dir),
            To_Unbounded_String (User),
            To_Unbounded_String (Host),
            Excluded_Patterns, Included_Patterns));
   end To_Slave_Gpr;

   --------------------
   -- To_Slave_Rsync --
   --------------------

   procedure To_Slave_Rsync
     (Project_Name      : String;
      Root_Dir          : String;
      Slave_Root_Dir    : String;
      User              : String;
      Host              : String;
      Excluded_Patterns : Str_Vect.Vector;
      Included_Patterns : Str_Vect.Vector)
   is

      procedure Add_Arg (Str : String);
      --  Add new argument

      Args : Argument_List
        (1 ..
           6 + Positive'Max
             (11 + Natural (Excluded_Patterns.Length),
              2 + Natural (Included_Patterns.Length)));
      N    : Natural range 0 .. Args'Last := 0;

      --------------
      -- Add_Args --
      --------------

      procedure Add_Arg (Str : String) is
      begin
         N := N + 1;
         Args (N) := new String'(Str);
      end Add_Arg;

      Pid : Process_Id;

   begin
      --  Check for rsync tool

      if Rsync_Cmd = null then
         Write_Line
           ("error: rsync not found for " & Host);
         OS_Exit (1);
      end if;

      Rsync_Count := 0;

      --  Archive mode, compression and ignore VCS

      Add_Arg ("-arz");

      if Included_Patterns.Length = 0 then
         --  Default excluded patterns objects/ali

         for P of Default_Excluded_Patterns loop
            Add_Arg ("--exclude=" & P);
         end loop;

         --  Add any user's defined excluded patterns

         for P of Excluded_Patterns loop
            Add_Arg ("--exclude=" & P);
         end loop;

      else
         --  Include sub-directories

         Add_Arg ("--include=*/");

         --  Add any user's defined included patterns

         for P of Included_Patterns loop
            Add_Arg ("--include=" & P);
         end loop;

         --  Then we exclude everything else

         Add_Arg ("--exclude=*");
      end if;

      --  Delete remote files not in local directory

      Add_Arg ("--delete-excluded");
      Add_Arg ("--delete");
      Add_Arg ("--copy-links");

      --  Local and remote directory

      Add_Arg (Root_Dir & "/");
      Add_Arg
        (User_Host (User, Host) & ":"
         & Compose (Slave_Root_Dir, Project_Name));

      if Opt.Verbose_Mode then
         Write_Line ("  synchronize data");
         Write_Line ("    from: " & Args (N - 1).all);
         Write_Line ("    to  : " & Args (N).all);
      end if;

      Pid := Non_Blocking_Spawn (Rsync_Cmd.all, Args (1 .. N));

      Hosts.Insert (Pid_To_Integer (Pid), Host);

      Rsync_Count := Rsync_Count + 1;

      for A of Args (1 .. N) loop
         Free (A);
      end loop;
   end To_Slave_Rsync;

   ----------------
   -- Wait_Rsync --
   ----------------

   procedure Wait_Rsync (N : Natural) is
      Pid     : Process_Id;
      Success : Boolean;
      Error   : Boolean := False;
      Host    : Hosts_Set.Cursor;
   begin
      for K in 1 .. N loop
         Wait_Process (Pid, Success);

         Host := Hosts.Find (Pid_To_Integer (Pid));

         if Success then
            if Opt.Verbose_Mode then
               Write_Line
                 ("  synchronization done for " & Hosts_Set.Element (Host));
            end if;

         else
            Error := True;
            Write_Line ("error: rsync on " & Hosts_Set.Element (Host));
         end if;
      end loop;

      --  If there is any error we cannot continue, just exit now

      if Error then
         OS_Exit (1);
      end if;
   end Wait_Rsync;

   ----------
   -- Wait --
   ----------

   procedure Wait is
   begin
      Gpr_Queue.No_More_Job;

      Wait_Rsync (Rsync_Count);

      if Sync_Tasks /= null then
         for T of Sync_Tasks.all loop
            if not T'Terminated then
               T.Stop;
            end if;
         end loop;
      end if;
   end Wait;

begin
   Default_Excluded_Patterns.Append ("*.o");
   Default_Excluded_Patterns.Append ("*.obj");
   Default_Excluded_Patterns.Append ("*.ali");
   Default_Excluded_Patterns.Append ("*.dll");
   Default_Excluded_Patterns.Append ("*.so");
   Default_Excluded_Patterns.Append ("*.so.*");
   Default_Excluded_Patterns.Append ("*.exe");
   Default_Excluded_Patterns.Append (".git");
   Default_Excluded_Patterns.Append (".svn");
   Default_Excluded_Patterns.Append ("CVS");
   Default_Excluded_Patterns.Append ("gnatinspect.db*");
end Gprbuild.Compilation.Sync;
