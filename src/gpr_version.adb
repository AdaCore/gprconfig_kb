------------------------------------------------------------------------------
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                          G P R _ V E R S I O N                           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 1992-2012, Free Software Foundation, Inc.          --
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

with Gnatvsn; use Gnatvsn;
with GprConfig.Sdefault;

package body GPR_Version is

   ------------------------
   -- Gpr_Version_String --
   ------------------------

   function Gpr_Version_String return String is
      Last  : Positive;
      First : Positive;

      Date : String (1 .. 10) := "(unknown) ";
      Host  : constant String := " (" & GprConfig.Sdefault.Hostname & ')';

   begin
      --  Find the beginning and the end of the current date, that is the last
      --  string with 8 consecutive digits in Gnat_Static_Version_String.

      Last := Gnat_Static_Version_String'Last;
      Last_Loop :
      while Last - Gnat_Static_Version_String'First >= 9 loop
         if Gnat_Static_Version_String (Last) not in '0' .. '9' then
            Last := Last - 1;

         else
            First := Last;
            First_Loop :
            while First >= Gnat_Static_Version_String'First and then
                  Gnat_Static_Version_String (First) in '0' .. '9'
            loop
               if Last - First = 7 then
                  Date :=
                    '(' & Gnat_Static_Version_String (First .. Last) & ')';
                  exit Last_Loop;

               else
                  First := First - 1;
               end if;
            end loop First_Loop;

            Last := First;
         end if;
      end loop Last_Loop;

      case Build_Type is
         when Gnatpro =>
            return "Pro " & Gpr_Version & " " & Date & Host;
         when GPL =>
            return "GPL " & Gpr_Version & " " & Date & Host;
         when FSF =>
            return Gpr_Version & " " & Date & Host;
      end case;
   end Gpr_Version_String;

end GPR_Version;
