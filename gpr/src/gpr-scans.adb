------------------------------------------------------------------------------
--                                                                          --
--                           GPR PROJECT MANAGER                            --
--                                                                          --
--          Copyright (C) 2001-2015, Free Software Foundation, Inc.         --
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

package body GPR.Scans is

   ------------------------
   -- Restore_Scan_State --
   ------------------------

   procedure Restore_Scan_State (Saved_State : Saved_Scan_State) is
   begin
      Scan_Ptr                 := Saved_State.Save_Scan_Ptr;
      Token                    := Saved_State.Save_Token;
      Token_Ptr                := Saved_State.Save_Token_Ptr;
      Current_Line_Start       := Saved_State.Save_Current_Line_Start;
      Start_Column             := Saved_State.Save_Start_Column;
      Checksum                 := Saved_State.Save_Checksum;
      First_Non_Blank_Location := Saved_State.Save_First_Non_Blank_Location;
      Token_Node               := Saved_State.Save_Token_Node;
      Token_Name               := Saved_State.Save_Token_Name;
      Prev_Token               := Saved_State.Save_Prev_Token;
      Prev_Token_Ptr           := Saved_State.Save_Prev_Token_Ptr;
   end Restore_Scan_State;

   ---------------------
   -- Save_Scan_State --
   ---------------------

   procedure Save_Scan_State (Saved_State : out Saved_Scan_State) is
   begin
      Saved_State.Save_Scan_Ptr                 := Scan_Ptr;
      Saved_State.Save_Token                    := Token;
      Saved_State.Save_Token_Ptr                := Token_Ptr;
      Saved_State.Save_Current_Line_Start       := Current_Line_Start;
      Saved_State.Save_Start_Column             := Start_Column;
      Saved_State.Save_Checksum                 := Checksum;
      Saved_State.Save_First_Non_Blank_Location := First_Non_Blank_Location;
      Saved_State.Save_Token_Node               := Token_Node;
      Saved_State.Save_Token_Name               := Token_Name;
      Saved_State.Save_Prev_Token               := Prev_Token;
      Saved_State.Save_Prev_Token_Ptr           := Prev_Token_Ptr;
   end Save_Scan_State;

end GPR.Scans;
