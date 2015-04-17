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

package body GPR.Cset is

   X_C0 : constant Character := Character'Val (16#C0#);
   X_C1 : constant Character := Character'Val (16#C1#);
   X_C2 : constant Character := Character'Val (16#C2#);
   X_C3 : constant Character := Character'Val (16#C3#);
   X_C4 : constant Character := Character'Val (16#C4#);
   X_C5 : constant Character := Character'Val (16#C5#);
   X_C6 : constant Character := Character'Val (16#C6#);
   X_C7 : constant Character := Character'Val (16#C7#);
   X_C8 : constant Character := Character'Val (16#C8#);
   X_C9 : constant Character := Character'Val (16#C9#);
   X_CA : constant Character := Character'Val (16#CA#);
   X_CB : constant Character := Character'Val (16#CB#);
   X_CC : constant Character := Character'Val (16#CC#);
   X_CD : constant Character := Character'Val (16#CD#);
   X_CE : constant Character := Character'Val (16#CE#);
   X_CF : constant Character := Character'Val (16#CF#);
   X_D0 : constant Character := Character'Val (16#D0#);
   X_D1 : constant Character := Character'Val (16#D1#);
   X_D2 : constant Character := Character'Val (16#D2#);
   X_D3 : constant Character := Character'Val (16#D3#);
   X_D4 : constant Character := Character'Val (16#D4#);
   X_D5 : constant Character := Character'Val (16#D5#);
   X_D6 : constant Character := Character'Val (16#D6#);
--   X_D7 : constant Character := Character'Val (16#D7#);
   X_D8 : constant Character := Character'Val (16#D8#);
   X_D9 : constant Character := Character'Val (16#D9#);
   X_DA : constant Character := Character'Val (16#DA#);
   X_DB : constant Character := Character'Val (16#DB#);
   X_DC : constant Character := Character'Val (16#DC#);
   X_DD : constant Character := Character'Val (16#DD#);
   X_DE : constant Character := Character'Val (16#DE#);
   X_DF : constant Character := Character'Val (16#DF#);
   X_E0 : constant Character := Character'Val (16#E0#);
   X_E1 : constant Character := Character'Val (16#E1#);
   X_E2 : constant Character := Character'Val (16#E2#);
   X_E3 : constant Character := Character'Val (16#E3#);
   X_E4 : constant Character := Character'Val (16#E4#);
   X_E5 : constant Character := Character'Val (16#E5#);
   X_E6 : constant Character := Character'Val (16#E6#);
   X_E7 : constant Character := Character'Val (16#E7#);
   X_E8 : constant Character := Character'Val (16#E8#);
   X_E9 : constant Character := Character'Val (16#E9#);
   X_EA : constant Character := Character'Val (16#EA#);
   X_EB : constant Character := Character'Val (16#EB#);
   X_EC : constant Character := Character'Val (16#EC#);
   X_ED : constant Character := Character'Val (16#ED#);
   X_EE : constant Character := Character'Val (16#EE#);
   X_EF : constant Character := Character'Val (16#EF#);
   X_F0 : constant Character := Character'Val (16#F0#);
   X_F1 : constant Character := Character'Val (16#F1#);
   X_F2 : constant Character := Character'Val (16#F2#);
   X_F3 : constant Character := Character'Val (16#F3#);
   X_F4 : constant Character := Character'Val (16#F4#);
   X_F5 : constant Character := Character'Val (16#F5#);
   X_F6 : constant Character := Character'Val (16#F6#);
--   X_F7 : constant Character := Character'Val (16#F7#);
   X_F8 : constant Character := Character'Val (16#F8#);
   X_F9 : constant Character := Character'Val (16#F9#);
   X_FA : constant Character := Character'Val (16#FA#);
   X_FB : constant Character := Character'Val (16#FB#);
   X_FC : constant Character := Character'Val (16#FC#);
   X_FD : constant Character := Character'Val (16#FD#);
   X_FE : constant Character := Character'Val (16#FE#);
   X_FF : constant Character := Character'Val (16#FF#);

   ------------------------------------------
   -- Definitions for Latin-1 (ISO 8859-1) --
   ------------------------------------------

   type Translate_Table is array (Character) of Character;

   Fold_Upper_Table : constant Translate_Table := Translate_Table'(

      'a' => 'A',  X_E0 => X_C0,  X_F0 => X_D0,
      'b' => 'B',  X_E1 => X_C1,  X_F1 => X_D1,
      'c' => 'C',  X_E2 => X_C2,  X_F2 => X_D2,
      'd' => 'D',  X_E3 => X_C3,  X_F3 => X_D3,
      'e' => 'E',  X_E4 => X_C4,  X_F4 => X_D4,
      'f' => 'F',  X_E5 => X_C5,  X_F5 => X_D5,
      'g' => 'G',  X_E6 => X_C6,  X_F6 => X_D6,
      'h' => 'H',  X_E7 => X_C7,
      'i' => 'I',  X_E8 => X_C8,  X_F8 => X_D8,
      'j' => 'J',  X_E9 => X_C9,  X_F9 => X_D9,
      'k' => 'K',  X_EA => X_CA,  X_FA => X_DA,
      'l' => 'L',  X_EB => X_CB,  X_FB => X_DB,
      'm' => 'M',  X_EC => X_CC,  X_FC => X_DC,
      'n' => 'N',  X_ED => X_CD,  X_FD => X_DD,
      'o' => 'O',  X_EE => X_CE,  X_FE => X_DE,
      'p' => 'P',  X_EF => X_CF,
      'q' => 'Q',
      'r' => 'R',
      's' => 'S',
      't' => 'T',
      'u' => 'U',
      'v' => 'V',
      'w' => 'W',
      'x' => 'X',
      'y' => 'Y',
      'z' => 'Z',

      'A' => 'A',  X_C0 => X_C0,  X_D0 => X_D0,
      'B' => 'B',  X_C1 => X_C1,  X_D1 => X_D1,
      'C' => 'C',  X_C2 => X_C2,  X_D2 => X_D2,
      'D' => 'D',  X_C3 => X_C3,  X_D3 => X_D3,
      'E' => 'E',  X_C4 => X_C4,  X_D4 => X_D4,
      'F' => 'F',  X_C5 => X_C5,  X_D5 => X_D5,
      'G' => 'G',  X_C6 => X_C6,  X_D6 => X_D6,
      'H' => 'H',  X_C7 => X_C7,
      'I' => 'I',  X_C8 => X_C8,  X_D8 => X_D8,
      'J' => 'J',  X_C9 => X_C9,  X_D9 => X_D9,
      'K' => 'K',  X_CA => X_CA,  X_DA => X_DA,
      'L' => 'L',  X_CB => X_CB,  X_DB => X_DB,
      'M' => 'M',  X_CC => X_CC,  X_DC => X_DC,
      'N' => 'N',  X_CD => X_CD,  X_DD => X_DD,
      'O' => 'O',  X_CE => X_CE,  X_DE => X_DE,
      'P' => 'P',  X_CF => X_CF,  X_DF => X_DF,  X_FF => X_FF,
      'Q' => 'Q',
      'R' => 'R',
      'S' => 'S',
      'T' => 'T',
      'U' => 'U',
      'V' => 'V',
      'W' => 'W',
      'X' => 'X',
      'Y' => 'Y',
      'Z' => 'Z',

      '0' => '0',
      '1' => '1',
      '2' => '2',
      '3' => '3',
      '4' => '4',
      '5' => '5',
      '6' => '6',
      '7' => '7',
      '8' => '8',
      '9' => '9',

      '_' => '_',

      others => ' ');

   Fold_Lower_Table : Translate_Table;

   Identifier_Char_Table : array (Character) of Boolean;

   procedure Initialize_Tables;

   --------------------------
   -- Is_Lower_Case_Letter --
   --------------------------

   function Is_Lower_Case_Letter (C : Character) return Boolean is
   begin
      return C /= Fold_Upper (C);
   end Is_Lower_Case_Letter;

   --------------------------
   -- Is_Upper_Case_Letter --
   --------------------------

   function Is_Upper_Case_Letter (C : Character) return Boolean is
   begin
      return C /= Fold_Lower (C);
   end Is_Upper_Case_Letter;

   ----------------
   -- Fold_Lower --
   ----------------

   function Fold_Lower (C : Character) return Character is
   begin
      return Fold_Lower_Table (C);
   end Fold_Lower;

   ----------------
   -- Fold_Upper --
   ----------------

   function Fold_Upper (C : Character) return Character is
   begin
      return Fold_Upper_Table (C);
   end Fold_Upper;

   -----------------------
   -- Initialize_Tables --
   -----------------------

   procedure Initialize_Tables is
   begin
      --  Use Fold_Upper table to compute Fold_Lower table

      Fold_Lower_Table := Fold_Upper_Table;

      for J in Character loop
         if J /= Fold_Upper_Table (J) then
            Fold_Lower_Table (Fold_Upper_Table (J)) := J;
            Fold_Lower_Table (J) := J;
         end if;
      end loop;

      Fold_Lower_Table (' ') := ' ';

      --  Build Identifier_Char table from used entries of Fold_Upper

      for J in Character loop
         Identifier_Char_Table (J) := (Fold_Upper_Table (J) /= ' ');
      end loop;

      --  Always add [ as an identifier character to deal with the brackets
      --  notation for wide characters used in identifiers. Note that if
      --  we are not allowing wide characters in identifiers, then any use
      --  of this notation will be flagged as an error in Scan_Identifier.

      Identifier_Char_Table ('[') := True;
   end Initialize_Tables;

   ---------------------
   -- Identifier_Char --
   ---------------------

   function Identifier_Char (C : Character) return Boolean is
   begin
      return Identifier_Char_Table (C);
   end Identifier_Char;

begin
   Initialize_Tables;
end GPR.Cset;
