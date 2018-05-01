------------------------------------------------------------------------------
--                                                                          --
--                           GPR PROJECT MANAGER                            --
--                                                                          --
--          Copyright (C) 2004-2018, Free Software Foundation, Inc.         --
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

package body GPR.Attr.PM is

   Last_Known_Package : Pkg_Node_Id := Empty_Pkg;

   -------------------
   -- Add_Attribute --
   -------------------

   procedure Add_Attribute
     (To_Package     : Package_Node_Id;
      Attribute_Name : Name_Id;
      Attribute_Node : out Attribute_Node_Id)
   is
   begin
      --  Only add attribute if package is already defined and is not unknown

      if To_Package /= Empty_Package   and then
         To_Package /= Unknown_Package
      then
         Attrs.Append (
           (Name           => Attribute_Name,
            Var_Kind       => Undefined,
            Optional_Index => False,
            Attr_Kind      => Unknown,
            Read_Only      => False,
            Others_Allowed => False,
            Default        => Empty_Value,
            Config_Concat  => False,
            Next           =>
              Package_Attributes.Table (To_Package.Value).First_Attribute));

         Package_Attributes.Table (To_Package.Value).First_Attribute :=
           Attrs.Last;
         Attribute_Node := (Value => Attrs.Last);
      end if;
   end Add_Attribute;

   -------------------------
   -- Add_Unknown_Package --
   -------------------------

   procedure Add_Unknown_Package (Name : Name_Id; Id : out Package_Node_Id) is
   begin
      if Last_Known_Package = Empty_Pkg then
         Last_Known_Package := Package_Attributes.Last;
      end if;

      Package_Attributes.Increment_Last;
      Id := (Value => Package_Attributes.Last);
      Package_Attributes.Table (Id.Value) :=
        (Name             => Name,
         Known            => False,
         First_Attribute  => Empty_Attr);
   end Add_Unknown_Package;

   -----------------------------
   -- Remove_Unknown_Packages --
   -----------------------------

   procedure Remove_Unknown_Packages is
   begin
      if Last_Known_Package /= Empty_Pkg then
         Package_Attributes.Set_Last (Last_Known_Package);
      end if;
   end Remove_Unknown_Packages;

end GPR.Attr.PM;
