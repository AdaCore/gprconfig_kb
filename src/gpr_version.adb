------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                          G P R _ V E R S I O N                           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2007, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

package body GPR_Version is

   -----------------------
   -- Gpr_Free_Software --
   -----------------------

   function Gpr_Free_Software return String is
   begin
      case Build_Type is
         when GPL | FSF =>
            return
              "This is free software; see the source for copying conditions." &
              ASCII.LF &
              "There is NO warranty; not even for MERCHANTABILITY or FITNESS" &
              " FOR A PARTICULAR PURPOSE.";

         when Gnatpro =>
            return
              "This is free software; see the source for copying conditions." &
               ASCII.LF &
               "See your AdaCore support agreement for details of warranty" &
               " and support." &
               ASCII.LF &
               "If you do not have a current support agreement, then there" &
               " is absolutely" &
               ASCII.LF &
               "no warranty; not even for MERCHANTABILITY or FITNESS FOR" &
               " A PARTICULAR" &
               ASCII.LF &
               "PURPOSE.";
      end case;
   end Gpr_Free_Software;

   ------------------------
   -- Gpr_Version_String --
   ------------------------

   function Gpr_Version_String return String is
   begin
      case Build_Type is
         when Gnatpro =>
            return "Pro " & Gpr_Static_Version_String;
         when GPL =>
            return "GPL " & Gpr_Static_Version_String;
         when FSF =>
            return Gpr_Static_Version_String;
      end case;
   end Gpr_Version_String;

end GPR_Version;
