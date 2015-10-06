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

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Strings.Unbounded;                  use Ada.Strings.Unbounded;

with Gpr_Util;                    use Gpr_Util;
with Gprbuild.Compilation.Slave;
with GPR.Names;                   use GPR.Names;

package body Gprbuild.Compilation.Process is

   use Ada;
   use type Containers.Count_Type;

   package Env_Maps is
     new Containers.Indefinite_Ordered_Maps (String, String);
   --  A set of key=value

   package Prj_Maps is new Containers.Indefinite_Ordered_Maps
     (String, Env_Maps.Map, Env_Maps."<", Env_Maps."=");
   --  A set of project+language=map

   function "<" (Left, Right : Id) return Boolean is
     (Left.R_Pid < Right.R_Pid);

   package Failures_Slave_Set is
     new Containers.Indefinite_Ordered_Maps (Id, String);

   function Get_Env (Project : Project_Id; Language : String) return String;
   --  Get the environment for a specific project and language

   Environments : Prj_Maps.Map;
   Failed_Proc  : Failures_Slave_Set.Map;

   type Process_Data is record
      Process : Id;
      Status  : Boolean;
   end record;

   package Endded_Process is new Containers.Doubly_Linked_Lists (Process_Data);

   protected Results is
      procedure Add (Result : Process_Data);
      entry Get (Result : out Process_Data);
   private
      List : Endded_Process.List;
   end Results;

   ----------------
   -- Add_Result --
   ----------------

   procedure Add_Result
     (Process : Id; Status : Boolean; Slave : String := "") is
   begin
      Results.Add (Process_Data'(Process, Status));

      --  For a compilation failure records the slave to be able to report it

      if not Status and then Slave /= "" then
         Record_Remote_Failure (Process, Slave);
      end if;
   end Add_Result;

   ------------------
   -- Create_Local --
   ------------------

   function Create_Local (Pid : Process_Id) return Id is
   begin
      return Id'(Local, Pid);
   end Create_Local;

   -------------------
   -- Create_Remote --
   -------------------

   function Create_Remote (Pid : Remote_Id) return Id is
   begin
      return Id'(Remote, Pid);
   end Create_Remote;

   ---------------------------
   -- Get_Maximum_Processes --
   ---------------------------

   function Get_Maximum_Processes return Positive is
   begin
      return Opt.Maximum_Processes + Slave.Get_Max_Processes;
   end Get_Maximum_Processes;

   -------------
   -- Get_Env --
   -------------

   function Get_Env (Project : Project_Id; Language : String) return String is
      Key  : constant String :=
               Get_Name_String (Project.Name) & "+" & Language;
      Res  : Unbounded_String;
   begin
      if Environments.Contains (Key) then
         for C in Environments (Key).Iterate loop
            if Res /= Null_Unbounded_String then
               Res := Res & Opts_Sep;
            end if;

            Res := Res & Env_Maps.Key (C) & '=' & Env_Maps.Element (C);
         end loop;
      end if;

      return To_String (Res);
   end Get_Env;

   -------------------
   -- Get_Slave_For --
   -------------------

   function Get_Slave_For (Pid : Id) return String is
      use type Failures_Slave_Set.Cursor;

   begin
      if Pid.Kind = Local then
         return "";

      else
         declare
            Pos : constant Failures_Slave_Set.Cursor := Failed_Proc.Find (Pid);
         begin
            if Pos = Failures_Slave_Set.No_Element then
               return "";
            else
               return Failures_Slave_Set.Element (Pos);
            end if;
         end;
      end if;
   end Get_Slave_For;

   ----------
   -- Hash --
   ----------

   function Hash (Process : Id) return Header_Num is
      Modulo : constant Integer := Integer (Header_Num'Last) + 1;
   begin
      if Process.Kind = Local then
         return Header_Num (Pid_To_Integer (Process.Pid) mod Modulo);
      else
         return Header_Num (Process.R_Pid mod Remote_Id (Modulo));
      end if;
   end Hash;

   -----------
   -- Image --
   -----------

   function Image (Pid : Remote_Id) return String is
      N_Img : constant String := Remote_Id'Image (Pid);
   begin
      return N_Img (N_Img'First + 1 .. N_Img'Last);
   end Image;

   ------------------------
   -- Record_Environment --
   ------------------------

   procedure Record_Environment
     (Project     : Project_Id;
      Language    : Name_Id;
      Name, Value : String)
   is
      Lang : constant String := Get_Name_String (Language);
      Key  : constant String := Get_Name_String (Project.Name) & "+" & Lang;
      New_Item : Env_Maps.Map;
   begin
      --  Create new item, variable association

      New_Item.Include (Name, Value);

      if Environments.Contains (Key) then
         if Environments (Key).Contains (Name) then
            Environments (Key).Replace (Name, Value);
         else
            Environments (Key).Insert (Name, Value);
         end if;

      else
         Environments.Insert (Key, New_Item);
      end if;
   end Record_Environment;

   ---------------------------
   -- Record_Remote_Failure --
   ---------------------------

   procedure Record_Remote_Failure (Pid : Id; Slave : String) is
   begin
      Failed_Proc.Insert (Pid, Slave);
   end Record_Remote_Failure;

   -------------
   -- Results --
   -------------

   protected body Results is

      ---------
      -- Add --
      ---------

      procedure Add (Result : Process_Data) is
      begin
         List.Append (Result);
      end Add;

      ---------
      -- Get --
      ---------

      entry Get (Result : out Process_Data) when List.Length /= 0 is
      begin
         Result := List.First_Element;
         List.Delete_First;
      end Get;

   end Results;

   ---------
   -- Run --
   ---------

   function Run
     (Executable  : String;
      Options     : GNAT.OS_Lib.Argument_List;
      Project     : Project_Id;
      Obj_Name    : String;
      Source      : String := "";
      Language    : String := "";
      Dep_Name    : String := "";
      Output_File : String := "";
      Err_To_Out  : Boolean := False;
      Force_Local : Boolean := False) return Id
   is
      Env : constant String := Get_Env (Project, Language);
   begin
      --  Run locally first, then send jobs to remote slaves. Note that to
      --  build remotely we need an output file and a language, if one of
      --  this requirement is not fulfilled we just run the process locally.

      if Force_Local
        or else not Distributed_Mode
        or else Local_Process.Count < Opt.Maximum_Processes
        or else Output_File /= ""
        or else Language = ""
      then
         Run_Local : declare
            P : Id (Local);
         begin
            Set_Env (Env, Fail => True);

            if Output_File /= "" then
               P.Pid := Non_Blocking_Spawn
                 (Executable, Options, Output_File, Err_To_Out);

            elsif Source /= "" and then Complete_Output then
               P.Pid := Non_Blocking_Spawn
                 (Executable, Options,
                  Stdout_File => Source & ".stdout",
                  Stderr_File => Source & ".stderr");

            else
               P.Pid := Non_Blocking_Spawn (Executable, Options);
            end if;

            Local_Process.Increment;

            return P;
         end Run_Local;

      else
         return Slave.Run
           (Project, Language, Options, Obj_Name, Dep_Name, Env);
      end if;
   end Run;

   -----------------
   -- Wait_Result --
   -----------------

   procedure Wait_Result (Process : out Id; Status : out Boolean) is
      Data : Process_Data;
   begin
      Results.Get (Data);
      Process := Data.Process;
      Status := Data.Status;
   end Wait_Result;

end Gprbuild.Compilation.Process;
