------------------------------------------------------------------------------
--                                                                          --
--                           GPR PROJECT MANAGER                            --
--                                                                          --
--          Copyright (C) 2012-2018, Free Software Foundation, Inc.         --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Environment_Variables; use Ada;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Text_IO;

with GNAT.MD5;          use GNAT;
with GNAT.String_Split; use GNAT.String_Split;

with GPR.Util;

package body GPR.Compilation is

   Last_Env_MD5 : MD5.Message_Digest := (others => <>);
   --  Keep last environment variable set to avoid too many system calls.
   --  ??? Ideally, we should set them when spawning the process, in
   --  which case it would be less expensive to set and could be set
   --  every time.

   -------------------------
   -- Check_Local_Process --
   -------------------------

   procedure Check_Local_Process (Process : Id) is
   begin
      if Process = Invalid_Process then
         declare
            Err : constant String := "spawn failed with ERRNO ="
              & Errno'Img
              & " (" & Errno_Message & ")";
         begin
            GPR.Util.Fail_Program (null, Err);
         end;
      end if;
   end Check_Local_Process;

   -----------
   -- Image --
   -----------

   function Image (Pid : Remote_Id) return String is
      N_Img : constant String := Remote_Id'Image (Pid);
   begin
      return N_Img (N_Img'First + 1 .. N_Img'Last);
   end Image;

   -------------
   -- Set_Env --
   -------------

   procedure Set_Env
     (Env   : String;
      Fail  : Boolean;
      Force : Boolean := False)
   is
      Env_List : Slice_Set;
   begin
      Create (Env_List, Env, String'(1 => Opts_Sep));

      for K in 1 .. Slice_Count (Env_List) loop
         declare
            Var : constant String := Slice (Env_List, K);
            I   : constant Natural := Strings.Fixed.Index (Var, "=");
            Sum : constant MD5.Message_Digest := MD5.Digest (Var);
         begin
            if I /= 0 then
               if Force or else Last_Env_MD5 /= Sum then
                  Environment_Variables.Set
                    (Name  => Var (Var'First .. I - 1),
                     Value => Var (I + 1 .. Var'Last));

                  Last_Env_MD5 := Sum;
               end if;

            elsif Var'Length > 0 then
               --  This is a protocol error, we do not want to fail here as
               --  this routine is used by gprslave. This error message should
               --  never been displayed anyway.

               Text_IO.Put_Line
                 ("wrong environment variable, missing '=' : " & Var);

               if Fail then
                  OS_Exit (1);
               end if;
            end if;
         end;
      end loop;
   end Set_Env;

   --------------------
   -- Shared_Counter --
   --------------------

   protected body Shared_Counter is

      -----------
      -- Count --
      -----------

      function Count return Natural is
      begin
         return Counter;
      end Count;

      ---------------
      -- Decrement --
      ---------------

      procedure Decrement is
      begin
         Counter := Counter - 1;
      end Decrement;

      ---------------
      -- Increment --
      ---------------

      procedure Increment is
      begin
         Counter := Counter + 1;
      end Increment;

      -----------
      -- Reset --
      -----------

      procedure Reset is
      begin
         Counter := 0;
      end Reset;

      -------------------
      -- Wait_Non_Zero --
      -------------------

      entry Wait_Non_Zero when Counter /= 0 is
      begin
         null;
      end Wait_Non_Zero;

   end Shared_Counter;

   -----------------------------------
   -- To_Native_Directory_Separator --
   -----------------------------------

   function To_Native_Directory_Separator (Pathname : String) return String is
      DS : Character renames Directory_Separator;
   begin
      return Strings.Fixed.Translate
        (Pathname,
         Strings.Maps.To_Mapping
           (String'(1 => (if DS = '/' then '\' else '/')),
            String'(1 => DS)));
   end To_Native_Directory_Separator;

end GPR.Compilation;
