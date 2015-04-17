------------------------------------------------------------------------------
--                                                                          --
--                           GPR PROJECT MANAGER                            --
--                                                                          --
--          Copyright (C) 1992-2015, Free Software Foundation, Inc.         --
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

package body GPR.Debug is

   ---------------------------------------------
   -- Documentation for gprbuild Debug Flags  --
   ---------------------------------------------

   --  dm  Display the maximum number of simultaneous compilations.

   --  dn  Do not delete temporary files created by gprbuild at the end
   --      of execution, such as temporary config pragma files, mapping
   --      files or project path files. This debug switch is equivalent to
   --      the standard switch --keep-temp-files. We retain the debug switch
   --      for back compatibility with past usage.

   --  dt  When a time stamp mismatch has been found for an ALI file,
   --      display the source file name, the time stamp expected and
   --      the time stamp found.

   --------------------
   -- Set_Debug_Flag --
   --------------------

   procedure Set_Debug_Flag (C : Character; Val : Boolean := True) is
   begin
      case C is
         when 'a' =>
            Debug_Flag_A := Val;
         when 'b' =>
            Debug_Flag_B := Val;
         when 'c' =>
            Debug_Flag_C := Val;
         when 'd' =>
            Debug_Flag_D := Val;
         when 'e' =>
            Debug_Flag_E := Val;
         when 'f' =>
            Debug_Flag_F := Val;
         when 'g' =>
            Debug_Flag_G := Val;
         when 'h' =>
            Debug_Flag_H := Val;
         when 'i' =>
            Debug_Flag_I := Val;
         when 'j' =>
            Debug_Flag_J := Val;
         when 'k' =>
            Debug_Flag_K := Val;
         when 'l' =>
            Debug_Flag_L := Val;
         when 'm' =>
            Debug_Flag_M := Val;
         when 'n' =>
            Debug_Flag_N := Val;
         when 'o' =>
            Debug_Flag_O := Val;
         when 'p' =>
            Debug_Flag_P := Val;
         when 'q' =>
            Debug_Flag_Q := Val;
         when 'r' =>
            Debug_Flag_R := Val;
         when 's' =>
            Debug_Flag_S := Val;
         when 't' =>
            Debug_Flag_T := Val;
         when 'u' =>
            Debug_Flag_U := Val;
         when 'v' =>
            Debug_Flag_V := Val;
         when 'w' =>
            Debug_Flag_W := Val;
         when 'x' =>
            Debug_Flag_X := Val;
         when 'y' =>
            Debug_Flag_Y := Val;
         when 'z' =>
            Debug_Flag_Z := Val;
         when others =>
            null;
      end case;
   end Set_Debug_Flag;

end GPR.Debug;
