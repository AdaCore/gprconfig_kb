------------------------------------------------------------------------------
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--        G P R B U I L D . C O M P I L A T I O N . P R O T O C O L         --
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

with Ada.Directories;            use Ada.Directories;
with Ada.Streams.Stream_IO;
with Ada.Strings.Fixed;          use Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps;

with Rewrite_Data;

package body Gprbuild.Compilation.Protocol is

   Args_Sep : constant Character := '|';
   --  Channel's argument separator

   Opts_Sep : constant Character := ';';
   --  Command options separator, that is the separator used for options to be
   --  passed to the executed command.

   function Args_Count
     (Cmd : Command) return Slice_Number is (Slice_Count (Cmd.Args));
   --  Number of argument in the command

   function Image (N : Natural) return String;
   --  Returns string representation of N without leading space

   procedure Send_File_Internal
     (Channel   : Communication_Channel;
      Path_Name : String;
      Cmd       : Command_Kind);
   --  Send file Path_Name over the channel with rewritting if needed

   ----------
   -- Args --
   ----------

   function Args (Cmd : Command) return Slice_Set is
   begin
      return Cmd.Args;
   end Args;

   -------------------
   -- Clear_Rewrite --
   -------------------

   procedure Clear_Rewrite (Channel : in out Communication_Channel) is
   begin
      Channel.WD_From := Null_Unbounded_String;
      Channel.WD_To := Null_Unbounded_String;
      Channel.CD_From := Null_Unbounded_String;
      Channel.CD_To := Null_Unbounded_String;
   end Clear_Rewrite;

   -----------
   -- Close --
   -----------

   procedure Close (Channel : in out Communication_Channel) is
   begin
      begin
         --  Make sure we never fail, the other end-point could have already
         --  closed the channel (hard ctrl-c).
         Shutdown_Socket (Channel.Sock);
      exception
         when others =>
            null;
      end;

      Channel.Channel := null;
      Clear_Rewrite (Channel);
   end Close;

   ------------
   -- Create --
   ------------

   function Create (Sock : Socket_Type) return Communication_Channel is
   begin
      return Communication_Channel'
        (Sock, Stream (Sock),
         Null_Unbounded_String, Null_Unbounded_String,
         Null_Unbounded_String, Null_Unbounded_String);
   end Create;

   -----------------
   -- Get_Command --
   -----------------

   function Get_Command (Channel : Communication_Channel) return Command is
      use Ada.Streams;
      use Ada.Streams.Stream_IO;

      function Handle_File (Cmd : Command) return Command;
      --  A file has been recieved, write it to disk

      procedure Handle_Output (Cmd : in out Command);
      --  A display output is received, read it and store it into the command

      -----------------
      -- Handle_File --
      -----------------

      function Handle_File (Cmd : Command) return Command is
         File_Name : constant String :=
                       Translate_Receive (Channel, Slice (Cmd.Args, 2));
         Dir       : constant String := Containing_Directory (File_Name);

         procedure Input
           (Item : out Stream_Element_Array;
            Last : out Stream_Element_Offset);
         --  Read and return some data from channel

         procedure Output (Item : Stream_Element_Array);
         --  Write data to file

         Size     : Stream_Element_Count :=
                       Stream_Element_Count'Value (Slice (Cmd.Args, 1));
         --  Number of bytes remaining to be read from channel

         Rewriter    : Rewrite_Data.Buffer :=
                         Rewrite_Data.Create
                           (To_String (Channel.WD_To),
                            To_String (Channel.WD_From));
         Rewriter_CD : aliased Rewrite_Data.Buffer :=
                         Rewrite_Data.Create
                           (To_String (Channel.CD_To),
                            To_String (Channel.CD_From));

         File : File_Type;

         -----------
         -- Input --
         -----------

         procedure Input
           (Item : out Stream_Element_Array;
            Last : out Stream_Element_Offset) is
         begin
            if Size = 0 then
               Last := 0;

            else
               Last := Stream_Element_Count'Min (Item'Length, Size);

               Stream_Element_Array'Read
                 (Channel.Channel, Item (Item'First .. Last));

               Size := Size - Last;
            end if;
         end Input;

         ------------
         -- Output --
         ------------

         procedure Output (Item : Stream_Element_Array) is
         begin
            Write (File, Item);
         end Output;

      begin
         Rewrite_Data.Link (Rewriter, Rewriter_CD'Unchecked_Access);

         if Dir /= "" and then not Exists (Dir) then
            Create_Directory (Dir);
         end if;

         Create (File, Out_File, File_Name);

         Rewrite_Data.Rewrite (Rewriter, Input'Access, Output'Access);

         Close (File);

         return Get_Command (Channel);
      end Handle_File;

      -------------------
      -- Handle_Output --
      -------------------

      procedure Handle_Output (Cmd : in out Command) is
         function Is_Number (Cmd : Command) return Boolean is
           (Is_Subset
              (To_Set (Slice (Cmd.Args, 1)), Constants.Decimal_Digit_Set));
      begin
         if Args_Count (Cmd) = 2
           and then Is_Number (Cmd)
         then
            declare
               Size   : constant Natural :=
                          Natural'Value (Slice (Cmd.Args, 1));
               Result : String (1 .. Size);
            begin
               if Size = 0 then
                  Cmd.Output := Null_Unbounded_String;
               else
                  String'Read (Channel.Channel, Result);
                  Cmd.Output := To_Unbounded_String (Result);
               end if;
            end;

         else
            raise Wrong_Command
              with "Expected DP found " & Command_Kind'Image (Cmd.Cmd);
         end if;
      end Handle_Output;

      Result : Command;

   begin
      declare
         Line : constant String := String'Input (Channel.Channel);
         C    : constant String :=
                  (if Line'Length >= 2
                   then Line (Line'First .. Line'First + 1)
                   else "");
      begin
         if C in
           "EX" | "AK" | "FL" | "OK" | "KO" | "CX" | "CU" | "DP" | "EC"
         then
            Result.Cmd := Command_Kind'Value (C);

            Create
              (Result.Args,
               Line (Line'First + 2 .. Line'Last),
               String'(1 => Args_Sep));

            if Result.Cmd = FL then
               --  We got some file data to write
               return Handle_File (Result);

            elsif Result.Cmd = DP then
               --  We got an output to display
               Handle_Output (Result);
            end if;

         else
            if Line'Length > 0 then
               raise Wrong_Command with Line;
            else
               raise Wrong_Command with "empty command line";
            end if;
         end if;

         return Result;
      end;

   exception
      when others =>
         --  Any exception means that the channel has been closed
         Result.Cmd := EC;
         return Result;
   end Get_Command;

   -----------------
   -- Get_Context --
   -----------------

   procedure Get_Context
     (Channel      : Communication_Channel;
      Target       : out Unbounded_String;
      Project_Name : out Unbounded_String;
      Build_Env    : out Unbounded_String;
      Sync         : out Sync_Kind)
   is
      Line : constant Command := Get_Command (Channel);
   begin
      if Line.Cmd = CX
        and then Args_Count (Line) = 4
      then
         Target       := To_Unbounded_String (Slice (Line.Args, 1));
         Project_Name := To_Unbounded_String (Slice (Line.Args, 2));
         Build_Env    := To_Unbounded_String (Slice (Line.Args, 3));
         Sync         := Sync_Kind'Value (Slice (Line.Args, 4));
      else
         raise Wrong_Command
           with "Expected CX found " & Command_Kind'Image (Line.Cmd);
      end if;
   end Get_Context;

   -------------
   -- Get_Pid --
   -------------

   procedure Get_Pid
     (Channel : Communication_Channel;
      Pid     : out Process.Remote_Id;
      Success : out Boolean)
   is
      Cmd : constant Command := Get_Command (Channel);
   begin
      if Slice_Count (Cmd.Args) = 1
        and then Cmd.Cmd in OK | KO
      then
         Pid := Process.Remote_Id'Value (Slice (Cmd.Args, 1));
         Success := (if Kind (Cmd) = KO then False);
      end if;
   end Get_Pid;

   -----------
   -- Image --
   -----------

   function Image (N : Natural) return String is
      N_Img : constant String := Natural'Image (N);
   begin
      return N_Img (N_Img'First + 1 .. N_Img'Last);
   end Image;

   ----------
   -- Kind --
   ----------

   function Kind (Cmd : Command) return Command_Kind is
   begin
      return Cmd.Cmd;
   end Kind;

   ------------
   -- Output --
   ------------

   function Output (Cmd : Command) return Unbounded_String is
   begin
      return Cmd.Output;
   end Output;

   --------------
   -- Send_Ack --
   --------------

   procedure Send_Ack
     (Channel : Communication_Channel; Pid : Process.Remote_Id) is
   begin
      String'Output
        (Channel.Channel, Command_Kind'Image (AK) & Process.Image (Pid));
   end Send_Ack;

   -------------------
   -- Send_Clean_Up --
   -------------------

   procedure Send_Clean_Up
     (Channel : Communication_Channel; Project_Name : String) is
   begin
      String'Output
        (Channel.Channel, Command_Kind'Image (CU) & Project_Name);
   end Send_Clean_Up;

   ------------------
   -- Send_Context --
   ------------------

   procedure Send_Context
     (Channel      : Communication_Channel;
      Target       : String;
      Project_Name : String;
      Build_Env    : String;
      Sync         : Sync_Kind) is
   begin
      String'Output
        (Channel.Channel,
         Command_Kind'Image (CX) & Target & Args_Sep & Project_Name
         & Args_Sep & Build_Env
         & Args_Sep & Sync_Kind'Image (Sync));
   end Send_Context;

   -----------------------------
   -- Send_End_Of_Compilation --
   -----------------------------

   procedure Send_End_Of_Compilation (Channel : Communication_Channel) is
   begin
      String'Output (Channel.Channel, Command_Kind'Image (EC));
   end Send_End_Of_Compilation;

   ---------------
   -- Send_Exec --
   ---------------

   procedure Send_Exec
     (Channel  : Communication_Channel;
      Dir      : String;
      Command  : String;
      Options  : Argument_List;
      Dep_Name : String;
      Filter   : access function (Str, Sep : String) return String := null)
   is
      R_Cmd : Unbounded_String;
   begin
      --  Options are serialized into a string and separated with Opts_Sep

      for K in Options'Range loop
         if Filter /= null then
            R_Cmd := R_Cmd & Filter (Options (K).all, WD_Path_Tag);
         else
            R_Cmd := R_Cmd & Options (K).all;
         end if;

         if K /= Options'Last then
            R_Cmd := R_Cmd & Opts_Sep;
         end if;
      end loop;

      --  Send the command over the channel

      String'Output
        (Channel.Channel,
         Command_Kind'Image (EX)
         & Dir & Args_Sep & Command & Args_Sep
         & Dep_Name & Args_Sep & To_String (R_Cmd));
   end Send_Exec;

   ---------------
   -- Send_File --
   ---------------

   procedure Send_File
     (Channel   : Communication_Channel;
      Path_Name : String) is
   begin
      Send_File_Internal (Channel, Path_Name, FL);
   end Send_File;

   ------------------------
   -- Send_File_Internal --
   ------------------------

   procedure Send_File_Internal
     (Channel   : Communication_Channel;
      Path_Name : String;
      Cmd       : Command_Kind)
   is
      use Ada.Streams;
      use Ada.Streams.Stream_IO;

      procedure Input
        (Item : out Stream_Element_Array; Last : out Stream_Element_Offset);
      --  Get input data from file

      procedure Output (Item : Stream_Element_Array);
      --  Send data to channel

      function File_Size return Natural;
      --  Compute the size of the file as rewritten

      File     : File_Type;
      F_Size   : Natural;

      Rewriter    : Rewrite_Data.Buffer :=
                      Rewrite_Data.Create
                        (To_String (Channel.WD_From),
                         To_String (Channel.WD_To));
      Rewriter_CD : aliased Rewrite_Data.Buffer :=
                      Rewrite_Data.Create
                        (To_String (Channel.CD_From),
                         To_String (Channel.CD_To));

      ---------------
      -- File_Size --
      ---------------

      function File_Size return Natural is

         procedure Count (Item : Stream_Element_Array);
         --  Count bytes

         Result : Natural := Natural (Size (Path_Name));

         -----------
         -- Count --
         -----------

         procedure Count (Item : Stream_Element_Array) is
         begin
            Result := Result + Item'Length;
         end Count;

      begin
         if Channel.WD_From /= Null_Unbounded_String
           and then Length (Channel.WD_From) <= Result
         then
            Result := 0;
            Rewrite_Data.Rewrite (Rewriter, Input'Access, Count'Access);
            Reset (File);
         end if;

         return Result;
      end File_Size;

      -----------
      -- Input --
      -----------

      procedure Input
        (Item : out Stream_Element_Array; Last : out Stream_Element_Offset) is
      begin
         if End_Of_File (File) then
            Last := 0;
         else
            Read (File, Item, Last);
         end if;
      end Input;

      ------------
      -- Output --
      ------------

      procedure Output (Item : Stream_Element_Array) is
      begin
         Stream_Element_Array'Write (Channel.Channel, Item);
      end Output;

   begin
      Rewrite_Data.Link (Rewriter, Rewriter_CD'Unchecked_Access);

      if Exists (Path_Name) then
         Open (File, In_File, Path_Name);

         --  First compute the file size as translated, note that this means
         --  that we are parsing the file twice.

         F_Size := File_Size;

         String'Output
           (Channel.Channel,
            Command_Kind'Image (Cmd) & Image (F_Size)
            & Args_Sep & Translate_Send (Channel, Path_Name));

         if F_Size /= 0 then
            Rewrite_Data.Rewrite (Rewriter, Input'Access, Output'Access);
         end if;

         Close (File);

      else
         raise Constraint_Error with "File not found : " & Path_Name;
      end if;
   end Send_File_Internal;

   -------------
   -- Send_Ko --
   -------------

   procedure Send_Ko
     (Channel : Communication_Channel; Pid : Process.Remote_Id) is
   begin
      String'Output
        (Channel.Channel, Command_Kind'Image (KO) & Process.Image (Pid));
   end Send_Ko;

   procedure Send_Ko (Channel : Communication_Channel) is
   begin
      String'Output (Channel.Channel, Command_Kind'Image (KO));
   end Send_Ko;

   -------------
   -- Send_Ok --
   -------------

   procedure Send_Ok
     (Channel : Communication_Channel; Pid : Process.Remote_Id) is
   begin
      String'Output
        (Channel.Channel, Command_Kind'Image (OK) & Process.Image (Pid));
   end Send_Ok;

   procedure Send_Ok (Channel : Communication_Channel) is
   begin
      String'Output (Channel.Channel, Command_Kind'Image (OK));
   end Send_Ok;

   -----------------
   -- Send_Output --
   -----------------

   procedure Send_Output
     (Channel : Communication_Channel; File_Name : String) is
   begin
      Send_File_Internal (Channel, File_Name, DP);
   end Send_Output;

   -----------------------
   -- Send_Slave_Config --
   -----------------------

   procedure Send_Slave_Config
     (Channel        : Communication_Channel;
      Max_Process    : Positive;
      Root_Directory : String) is
   begin
      String'Output
        (Channel.Channel,
         Command_Kind'Image (OK)
         & Image (Max_Process) & Args_Sep & Root_Directory);
   end Send_Slave_Config;

   --------------------
   -- Set_Rewrite_CD --
   --------------------

   procedure Set_Rewrite_CD
     (Channel : in out Communication_Channel; Path : String) is
   begin
      Channel.CD_From := To_Unbounded_String (Normalize_Pathname (Path));
      Channel.CD_To := To_Unbounded_String (CD_Path_Tag);
   end Set_Rewrite_CD;

   --------------------
   -- Set_Rewrite_WD --
   --------------------

   procedure Set_Rewrite_WD
     (Channel : in out Communication_Channel; Path : String) is
   begin
      Channel.WD_From := To_Unbounded_String (Path);
      Channel.WD_To := To_Unbounded_String (WD_Path_Tag);
   end Set_Rewrite_WD;

   ----------
   -- Sock --
   ----------

   function Sock (Channel : Communication_Channel) return Socket_Type is
   begin
      return Channel.Sock;
   end Sock;

   -----------------------
   -- Translate_Receive --
   -----------------------

   function Translate_Receive
     (Channel : Communication_Channel; Str : String) return String
   is
      P : constant Natural := Index (Str, To_String (Channel.WD_To));
   begin
      if P = 0 then
         return Str;
      else
         return To_String (Channel.WD_From)
           & Str (P + Length (Channel.WD_To) .. Str'Last);
      end if;
   end Translate_Receive;

   --------------------
   -- Translate_Send --
   --------------------

   function Translate_Send
     (Channel : Communication_Channel; Str : String) return String
   is
      P : constant Natural := Index (Str, To_String (Channel.WD_From));
   begin
      if P = 0 then
         return Str;
      else
         return To_String (Channel.WD_To)
           & Str (P + Length (Channel.WD_From) .. Str'Last);
      end if;
   end Translate_Send;

end Gprbuild.Compilation.Protocol;
