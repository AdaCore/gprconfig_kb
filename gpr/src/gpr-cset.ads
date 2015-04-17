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

package GPR.Cset is

   function Is_Upper_Case_Letter (C : Character) return Boolean;
   pragma Inline (Is_Upper_Case_Letter);
   --  Determine if character is upper case letter

   function Is_Lower_Case_Letter (C : Character) return Boolean;
   pragma Inline (Is_Lower_Case_Letter);
   --  Determine if character is lower case letter

   function Identifier_Char (C : Character) return Boolean;
   pragma Inline (Identifier_Char);

   function Fold_Lower (C : Character) return Character;
   pragma Inline (Fold_Lower);

   function Fold_Upper (C : Character) return Character;
   pragma Inline (Fold_Upper);

end GPR.Cset;
