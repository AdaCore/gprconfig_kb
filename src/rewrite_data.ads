------------------------------------------------------------------------------
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                         R E W R I T E _ D A T A                          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2012-2013, Free Software Foundation, Inc.          --
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

with Ada.Streams; use Ada.Streams;

package Rewrite_Data is

   type Buffer
     (Size, Size_Pattern, Size_Value : Stream_Element_Offset) is
   limited private;
   type Buffer_Ref is access all Buffer;

   function Create
     (Pattern, Value : String;
      Size           : Stream_Element_Offset := 1_024) return Buffer;
   --  Create and return a buffer

   procedure Write
     (B      : in out Buffer;
      Data   : Stream_Element_Array;
      Output : not null access procedure (Data : Stream_Element_Array));
   --  Write Data into the buffer, call Output for any prepared data

   function Size (B : Buffer) return Natural;
   --  Returns the current size of the buffer (count of Stream_Array_Element)

   procedure Flush
     (B      : in out Buffer;
      Output : not null access procedure (Data : Stream_Element_Array));
   --  Call Output for all remaining data in the buffer. The buffer is
   --  reset and ready for another use after this call.

   procedure Reset (B : in out Buffer);
   pragma Inline (Reset);
   --  Clear all data in buffer, B is ready for another use. Note that this is
   --  not needed after a Flush.

   procedure Rewrite
     (B      : in out Buffer;
      Input  : not null access procedure
                 (Buffer : out Stream_Element_Array;
                  Last   : out Stream_Element_Offset);
      Output : not null access procedure (Data : Stream_Element_Array));
   --  Read data from Input, rewrite them and then call Output

   procedure Link (From : in out Buffer; To : Buffer_Ref);
   --  Link two rewrite buffers, that is all data send to From buffer will be
   --  rewritten and then passed to the To rewrite buffer.

private

   type Buffer
     (Size, Size_Pattern, Size_Value : Stream_Element_Offset) is
   limited record
      --  Fully prepared/rewritten data waiting to be output
      Buffer  : Stream_Element_Array (1 .. Size);

      --  Current data checked, this buffer contains every piece of data
      --  starting with the pattern. It means that at any point:
      --  Current (1 .. Pos_C) = Pattern (1 .. Pos_C)
      Current : Stream_Element_Array (1 .. Size_Pattern);

      --  The pattern to look for
      Pattern : Stream_Element_Array (1 .. Size_Pattern);

      --  The value the pattern is replaced by
      Value   : Stream_Element_Array (1 .. Size_Value);

      Pos_C   : Stream_Element_Offset; -- last valid element in Current
      Pos_B   : Stream_Element_Offset; -- last valid element in Buffer

      Next    : Buffer_Ref;
   end record;

end Rewrite_Data;
