------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--            G P R L I B . B U I L D _ S H A R E D _ L I B                 --
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

with MLib.Tgt; use MLib.Tgt;

separate (Gprlib)
procedure Build_Shared_Lib is
   Ofiles  : Argument_List (1 .. Object_Files.Last);
   Options : Argument_List (1 .. Options_Table.Last);

begin
   --  Comments here ???

   for J in Ofiles'Range loop
      Ofiles (J) := Object_Files.Table (J);
   end loop;

   for J in Options'Range loop
      Options (J) := Options_Table.Table (J);
   end loop;

   Build_Dynamic_Library
     (Ofiles       => Ofiles,
      Options      => Options,
      Interfaces   => No_Argument_List,
      Lib_Filename => Library_Name.all,
      Lib_Dir      => Library_Directory.all,
      Symbol_Data  => Prj.No_Symbols,
      Lib_Version  => Library_Version.all,
      Driver_Name  => Driver_Name,
      Auto_Init    => Auto_Init);
end Build_Shared_Lib;
