------------------------------------------------------------------------------
--                                                                          --
--                           GPR PROJECT MANAGER                            --
--                                                                          --
--          Copyright (C) 2005-2018, Free Software Foundation, Inc.         --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

--  This package contains insecure procedures that are intended to be used
--  only inside the GPR hierarcy. It should not be imported by other tools,
--  such as GPS.

package GPR.Attr.PM is

   --  The following procedures should only be used by the Project Manager, as
   --  duplicate names are not checked.

   procedure Add_Unknown_Package (Name : Name_Id; Id : out Package_Node_Id);
   --  Add a new unknown package. The Name cannot be the name of a predefined
   --  or already registered package, but this is not checked.

   procedure Remove_Unknown_Packages;
   --  Remove from the package table all packages that have been added using
   --  procedure Add_Unknown_Package above.

   procedure Add_Attribute
     (To_Package     : Package_Node_Id;
      Attribute_Name : Name_Id;
      Attribute_Node : out Attribute_Node_Id);
   --  Add an attribute to the list for package To_Package. Attribute_Name
   --  cannot be the name of an existing attribute of the package, but this is
   --  not checked. Does nothing if To_Package is Empty_Package.

end GPR.Attr.PM;
