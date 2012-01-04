------------------------------------------------------------------------------
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--              G P R L I B . B U I L D _ S H A R E D _ L I B               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2006-2012, Free Software Foundation, Inc.          --
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

with MLib.Tgt; use MLib.Tgt;
with MLib.Utl; use MLib.Utl;

separate (Gprlib)
procedure Build_Shared_Lib is
   Ofiles  : Argument_List (1 .. Object_Files.Last);
   Options : Argument_List (1 .. Options_Table.Last);

begin
   --  If runtime library directory is indicated, call Specify_Adalib_Dir so
   --  that function MLib.Libgnat returns it. If we don't know what is the
   --  runtime library directory, set it to the current directory so that
   --  MLib.Libgnat does not fail.

   if Runtime_Library_Dir /= null then
      Specify_Adalib_Dir (Runtime_Library_Dir.all);

   else
      Specify_Adalib_Dir (".");
   end if;

   --  On VMS, use Build_Dynamic_Library to build the library as there is
   --  specific handling of symbols.

   for J in Ofiles'Range loop
      Ofiles (J) := Object_Files.Table (J);
   end loop;

   for J in Options'Range loop
      Options (J) := Options_Table.Table (J);
   end loop;

   Build_Dynamic_Library
     (Ofiles       => Ofiles,
      Options      => Options,
      Interfaces   => MLib.No_Argument_List,
      Lib_Filename => Library_Name.all,
      Lib_Dir      => Library_Directory.all,
      Symbol_Data  => Prj.No_Symbols,
      Lib_Version  => Library_Version.all,
      Driver_Name  => Driver_Name,
      Auto_Init    => Auto_Init);
end Build_Shared_Lib;
