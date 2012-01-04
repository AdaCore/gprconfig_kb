------------------------------------------------------------------------------
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                          G P R _ V E R S I O N                           --
--                                                                          --
--                                 S p e c                                  --
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

--  This package spec holds version information for the GPR tools.
--  It is updated whenever the release number is changed.

package GPR_Version is

   Gpr_Version : constant String := "1.7.0w";
   --  Static string identifying this version

   function Gpr_Version_String return String;
   --  Version output when GPRBUILD or its related tools, including
   --  GPRCLEAN, are run (with appropriate verbose option switch set).
end GPR_Version;
