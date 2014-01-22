------------------------------------------------------------------------------
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                      G P R I N S T A L L . M A I N                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2012-2014, Free Software Foundation, Inc.          --
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

with Prj;

package Gprinstall.Install is

   use Prj;

   procedure Process
     (Tree    : Project_Tree_Ref;
      Project : Project_Id);
   --  Install Project and possibly all imported projects depending on the
   --  options.

end Gprinstall.Install;
