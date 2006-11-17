------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             G P R C L E A N                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2006, Free Software Foundation, Inc.              --
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

--  Gprclean is a utility to delete files produced by the gprmake:
--  dependency files, object files, tree files, library files, interface copy
--  files, binder generated files, executable files and other artifacts.

--  Gprclean is invoked with a project file or a tree of project files with
--  the optional specification of one of several executables.

with Cleangpr;

procedure Gprclean is
begin
   --  The real work is done in package Cleangpr

   Cleangpr.Gprclean;
end Gprclean;
