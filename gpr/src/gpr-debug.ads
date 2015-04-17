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

--  This package contains global flags used to control the inclusion
--  of debugging code in various phases of the compiler. Some of these
--  flags are also used by the binder and gnatmake.

package GPR.Debug is

   -------------------------
   -- Dynamic Debug Flags --
   -------------------------

   --  Flags that can be used to active various specialized debugging output
   --  information. The flags are preset to False, which corresponds to the
   --  given output being suppressed. The individual flags can be turned on
   --  using the undocumented switch dxxx where xxx is a string of letters for
   --  flags to be turned on. Documentation on the current usage of these flags
   --  is contained in the body of Debug rather than the spec, so that we don't
   --  have to recompile the world when a new debug flag is added.

   Debug_Flag_A : Boolean := False;
   Debug_Flag_B : Boolean := False;
   Debug_Flag_C : Boolean := False;
   Debug_Flag_D : Boolean := False;
   Debug_Flag_E : Boolean := False;
   Debug_Flag_F : Boolean := False;
   Debug_Flag_G : Boolean := False;
   Debug_Flag_H : Boolean := False;
   Debug_Flag_I : Boolean := False;
   Debug_Flag_J : Boolean := False;
   Debug_Flag_K : Boolean := False;
   Debug_Flag_L : Boolean := False;
   Debug_Flag_M : Boolean := False;
   Debug_Flag_N : Boolean := False;
   Debug_Flag_O : Boolean := False;
   Debug_Flag_P : Boolean := False;
   Debug_Flag_Q : Boolean := False;
   Debug_Flag_R : Boolean := False;
   Debug_Flag_S : Boolean := False;
   Debug_Flag_T : Boolean := False;
   Debug_Flag_U : Boolean := False;
   Debug_Flag_V : Boolean := False;
   Debug_Flag_W : Boolean := False;
   Debug_Flag_X : Boolean := False;
   Debug_Flag_Y : Boolean := False;
   Debug_Flag_Z : Boolean := False;

   procedure Set_Debug_Flag (C : Character; Val : Boolean := True);
   --  Where C is a-z, sets the corresponding debug flag to
   --  the given value.

end GPR.Debug;
