------------------------------------------------------------------------------
--                   Copyright (C) 2006, AdaCore                            --
------------------------------------------------------------------------------

with GNAT.Strings;

package Glib is
   type String_Ptr is new GNAT.Strings.String_Access;
   subtype UTF8_String is String;
end Glib;
