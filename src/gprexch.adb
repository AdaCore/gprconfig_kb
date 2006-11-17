------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              G P R E X C H                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2006, Free Software Foundation, Inc.            --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Osint;
pragma Elaborate_All (Osint);

package body Gprexch is

   -------------------------
   -- Get_Binding_Section --
   -------------------------

   function Get_Binding_Section (Label : String) return Binding_Section is
   begin
      for Section in Binding_Section loop
         if Binding_Labels (Section) /= null and then
           Binding_Labels (Section).all = Label
         then
            return Section;
         end if;
      end loop;

      return No_Binding_Section;
   end Get_Binding_Section;

   -------------------------
   -- Get_Library_Section --
   -------------------------

   function Get_Library_Section (Label : String) return Library_Section is
   begin
      for Section in Library_Section loop
         if Library_Labels (Section) /= null and then
           Library_Labels (Section).all = Label
         then
            return Section;
         end if;
      end loop;

      return No_Library_Section;
   end Get_Library_Section;

--  Package elaboration code (checks all labels start with square bracket)

begin
   for J in Binding_Labels'Range loop
      if Binding_Labels (J) /= null and then
        Binding_Labels (J) (Binding_Labels (J)'First) /= '['
      then
         Osint.Fail
           ("binding exchange section label """,
            Binding_Labels (J).all,
            """ does not start with a '['");
      end if;
   end loop;

   for J in Library_Labels'Range loop
      if Library_Labels (J) /= null and then
        Library_Labels (J) (Library_Labels (J)'First) /= '['
      then
         Osint.Fail
           ("library exchange section label """,
            Library_Labels (J).all,
            """ does not start with a '['");
      end if;
   end loop;

end Gprexch;
