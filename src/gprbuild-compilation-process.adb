------------------------------------------------------------------------------
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--         G P R B U I L D . C O M P I L A T I O N . P R O C E S S          --
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

with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Exceptions;                         use Ada.Exceptions;
with Ada.Strings.Unbounded;                  use Ada.Strings.Unbounded;

with Output; use Output;

with Gpr_Util;                    use Gpr_Util;
with Gprbuild.Compilation.Result;
with Gprbuild.Compilation.Slave;

package body Gprbuild.Compilation.Process is

   use Ada;

   package Env_Maps is
     new Containers.Indefinite_Ordered_Maps (String, String);
   --  A set of key=value
   package Prj_Maps is new Containers.Indefinite_Ordered_Maps
     (String, Env_Maps.Map, Env_Maps."<", Env_Maps."=");
   --  A set of project+language=map

   function Get_Env (Project : Project_Id; Language : String) return String;
   --  Get the environment for a specific project and language

   task type Wait_Local;
   type Wait_Local_Ref is access Wait_Local;

   WL            : Wait_Local_Ref;
   Local_Process : Shared_Counter;
   Environments  : Prj_Maps.Map;

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

   ---------
   -- Run --
   ---------

   function Run
     (Executable  : String;
      Options     : GNAT.OS_Lib.Argument_List;
      Project     : Project_Id;
      Language    : String := "";
      Dep_Name    : String := "";
      Output_File : String := "";
      Err_To_Out  : Boolean := False;
      Force_Local : Boolean := False) return Id
   is
      Env : constant String := Get_Env (Project, Language);
   begin
      --  Initialize the task waiting for local process only in distributed
      --  mode. In standard mode, the process are waited for in the
      --  Compilation.Result.Wait procedure.

      if Distributed_Mode and then WL = null then
         WL := new Wait_Local;
      end if;

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

            if Output_File = "" then
               P.Pid := Non_Blocking_Spawn (Executable, Options);
            else
               P.Pid := Non_Blocking_Spawn
                 (Executable, Options, Output_File, Err_To_Out);
            end if;

            Local_Process.Increment;

            return P;
         end Run_Local;

      else
         return Slave.Run (Project, Language, Options, Dep_Name, Env);
      end if;
   end Run;

   ----------------
   -- Wait_Local --
   ----------------

   task body Wait_Local is
      Pid    : Process_Id;
      Status : Boolean;
   begin
      loop
         Local_Process.Wait_Non_Zero;

         Wait_Process (Pid, Status);
         Local_Process.Decrement;
         Result.Add (Id'(Local, Pid), Status);
      end loop;
   exception
      when E : others =>
         Write_Line (Exception_Information (E));
         OS_Exit (1);
   end Wait_Local;

end Gprbuild.Compilation.Process;
