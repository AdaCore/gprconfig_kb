------------------------------------------------------------------------------
--                                                                          --
--                           GPR PROJECT MANAGER                            --
--                                                                          --
--          Copyright (C) 2015, Free Software Foundation, Inc.              --
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

separate (GPR)
package body Stamps is

   -----------------------
   -- Local Subprograms --
   -----------------------

   function V (T : Time_Stamp_Type; X : Time_Stamp_Index) return Nat;
   --  Extract two decimal digit value from time stamp

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Time_Stamp_Type) return Boolean is
   begin
      return not (Left = Right) and then String (Left) < String (Right);
   end "<";

   ----------
   -- "<=" --
   ----------

   function "<=" (Left, Right : Time_Stamp_Type) return Boolean is
   begin
      return not (Left > Right);
   end "<=";

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Time_Stamp_Type) return Boolean is
      Sleft  : Nat;
      Sright : Nat;

   begin
      if String (Left) = String (Right) then
         return True;

      elsif Left (1) = ' ' or else Right (1) = ' ' then
         return False;
      end if;

      --  In the following code we check for a difference of 2 seconds or less

      --  Recall that the time stamp format is:

      --     Y  Y  Y  Y  M  M  D  D  H  H  M  M  S  S
      --    01 02 03 04 05 06 07 08 09 10 11 12 13 14

      --  Note that we do not bother to worry about shifts in the day.
      --  It seems unlikely that such shifts could ever occur in practice
      --  and even if they do we err on the safe side, i.e., we say that the
      --  time stamps are different.

      Sright := V (Right, 13) + 60 * (V (Right, 11) + 60 * V (Right, 09));
      Sleft  := V (Left,  13) + 60 * (V (Left,  11) + 60 * V (Left,  09));

      --  So the check is: dates must be the same, times differ 2 sec at most

      return abs (Sleft - Sright) <= 2
         and then String (Left (1 .. 8)) = String (Right (1 .. 8));
   end "=";

   ---------
   -- ">" --
   ---------

   function ">" (Left, Right : Time_Stamp_Type) return Boolean is
   begin
      return not (Left = Right) and then String (Left) > String (Right);
   end ">";

   ----------
   -- ">=" --
   ----------

   function ">=" (Left, Right : Time_Stamp_Type) return Boolean is
   begin
      return not (Left < Right);
   end ">=";

   -------
   -- V --
   -------

   function V (T : Time_Stamp_Type; X : Time_Stamp_Index) return Nat is
   begin
      return 10 * (Character'Pos (T (X))     - Character'Pos ('0')) +
                   Character'Pos (T (X + 1)) - Character'Pos ('0');
   end V;

end Stamps;
