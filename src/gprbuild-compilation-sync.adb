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
with Ada.Directories; use Ada.Directories;

with Output; use Output;

package body Gprbuild.Compilation.Sync is

   use Ada;

   package Hosts_Set is
     new Containers.Indefinite_Ordered_Maps (Integer, String);

   procedure Wait_Rsync (N : Natural);
   --  Wait for N rsync processes. If one of the is in error exit

   function User_Host (User, Host : String) return String is
     (if User = "" then Host else User & '@' & Host);

   Rsync_Cmd : constant GNAT.OS_Lib.String_Access :=
                 Locate_Exec_On_Path ("rsync");

   Rsync_Count : Natural := 0;
   --  The number of rsync process started, we need to wait for them to
   --  terminate.

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
         when Protocol.File  =>
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
      use type Containers.Count_Type;

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

   --------------
   -- To_Slave --
   --------------

   procedure To_Slave
     (Sync              : Protocol.Sync_Kind;
      Project_Name      : String;
      Root_Dir          : String;
      Slave_Root_Dir    : String;
      User              : String;
      Host              : String;
      Excluded_Patterns : Str_Vect.Vector;
      Included_Patterns : Str_Vect.Vector) is
   begin
      case Sync is
         when Protocol.File  =>
            null;

         when Protocol.Rsync =>
            To_Slave_Rsync
              (Project_Name, Root_Dir, Slave_Root_Dir, User, Host,
               Excluded_Patterns, Included_Patterns);
      end case;
   end To_Slave;

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
      use type Containers.Count_Type;

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
         --  Exclude objects/ali

         Add_Arg ("--exclude=*.o");
         Add_Arg ("--exclude=*.obj");
         Add_Arg ("--exclude=*.ali");
         Add_Arg ("--exclude=*.dll");
         Add_Arg ("--exclude=*.so");
         Add_Arg ("--exclude=*.so.*");
         Add_Arg ("--exclude=*.exe");
         Add_Arg ("--exclude=.git");
         Add_Arg ("--exclude=.svn");
         Add_Arg ("--exclude=CVS");
         Add_Arg ("--exclude=gnatinspect.db*");

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
      Wait_Rsync (Rsync_Count);
   end Wait;

end Gprbuild.Compilation.Sync;
