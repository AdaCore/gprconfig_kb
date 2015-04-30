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

with Ada.Text_IO;
with Ada.Environment_Variables; use Ada;
with Ada.Strings.Fixed;

with GNAT.MD5;          use GNAT;
with GNAT.String_Split; use GNAT.String_Split;

package body Gprbuild.Compilation is

   Last_Env_MD5 : MD5.Message_Digest := (others => <>);
   --  Keep last environment variable set to avoid too many system calls.
   --  ??? Ideally, we should set them when spawning the process, in
   --  which case it would be less expensive to set and could be set
   --  every time.

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

end Gprbuild.Compilation;
