------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              C O N F G P R                               --
--                                                                          --
--                                 S p e c                                  --
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

--  The following package allow to get the configuration of the Project
--  Manager. It is used by several tools, including gprmake and gprclean.

package Confgpr is

   procedure Get_Configuration (Fail_If_Error : Boolean);
   --  Find the main configuration project and parse the project tree rooted at
   --  this configuration project. Fail_If_Error controls the behavior when an
   --  error is encountered:
   --
   --    If True, the tool fails at the first error.
   --    If False, the project manager sets the mode to Ada_Only automatically.

end Confgpr;
