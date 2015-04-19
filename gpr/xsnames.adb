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

with Ada.Text_IO; use Ada.Text_IO;

with GNAT.Case_Util; use GNAT.Case_Util;

procedure Xsnames is
   File_In : File_Type;
   Line : String (1 .. 100);
   Last : Natural;

   Spec_Init_Name : constant String := "snames_spec_init.txt";
   Body_Init_Name : constant String := "snames_body_init.txt";
   File_Name : constant String := "snames.list";

   Spec_File : File_Type;
   Spec_File_Name : constant String := "gpr-snames.ads";

   Body_File : File_Type;
   Body_File_Name : constant String := "gpr-snames.adb";

   Counter : Natural := 0;

   function Image (N : Natural) return String;

   function Image (N : Natural) return String is
      Img : constant String := N'Img;
   begin
      if N < 10 then
         return "00" & Img (2);
      elsif N < 100 then
         return "0" & Img (2 .. 3);
      else
         return Img (2 .. Img'Last);
      end if;
   end Image;

begin
   --  Initialize spec
   Create (Spec_File, Out_File, Spec_File_Name);
   Open (File_In, In_File, Spec_Init_Name);

   while not End_Of_File (File_In) loop
      Get_Line (File_In, Line, Last);
      Put_Line (Spec_File, Line (1 .. Last));
   end loop;

   Close (File_In);
   New_Line (Spec_File);

   --  Initialize body
   Create (Body_File, Out_File, Body_File_Name);
   Open (File_In, In_File, Body_Init_Name);

   while not End_Of_File (File_In) loop
      Get_Line (File_In, Line, Last);
      Put_Line (Body_File, Line (1 .. Last));
   end loop;

   Close (File_In);
   New_Line (Body_File);
   Put_Line (Body_File, "   Initialized : Boolean := False;");
   New_Line (Body_File);
   Put_Line (Body_File, "   procedure Initialize is");
   Put_Line (Body_File, "   begin");
   Put_Line (Body_File, "      if Initialized then return; end if;");
   New_Line (Body_File);

   --  First the single characters

   for Ch in Character range 'A' .. 'Z' loop
      declare
         Lower_Name : String := (1 => Ch);
      begin
         To_Lower (Lower_Name);
         Counter := Counter + 1;
         Put (Spec_File, "   Name_" & Ch);
         Set_Col (Spec_File, 42);
         Put_Line
           (Spec_File, ": constant Name_Id := N + " & Image (Counter) & ";");

         Put (Body_File, "      Add_Name (""");
         Put (Body_File, Lower_Name);
         Put_Line (Body_File, """);");
      end;
   end loop;

   --  Read the names
   Open (File_In, In_File, File_Name);
   while not End_Of_File (File_In) loop
      Get_Line (File_In, Line, Last);

      if Last >= 2 and then Line (1 .. 2) /= "--" then
         Counter := Counter + 1;
         Put (Spec_File, "   Name_" & Line (1 .. Last));
         Set_Col (Spec_File, 42);
         Put_Line
           (Spec_File, ": constant Name_Id := N + " & Image (Counter) & ";");

         declare
            Lower_Name : String := Line (1 .. Last);
         begin
            To_Lower (Lower_Name);
            Put (Body_File, "      Add_Name (""");
            Put (Body_File, Lower_Name);
            Put_Line (Body_File, """);");
         end;
      end if;
   end loop;

   Close (File_In);

   --  Finish the spec

   New_Line (Spec_File);
   Put_Line (Spec_File, "   subtype Reserved_Ada_95 is Name_Id");
   Put_Line (Spec_File, "      range Name_Abort .. Name_Tagged;");
   Put_Line (Spec_File, "   subtype Reserved_Ada_Project is Name_Id");
   Put_Line (Spec_File, "      range Name_Abort .. Name_External_As_List;");
   Put_Line (Spec_File, "   subtype Reserved_Ada_Other is Name_Id");
   Put_Line (Spec_File, "      range Name_Interface .. Name_Some;");
   New_Line (Spec_File);
   Put_Line (Spec_File, "   procedure Initialize;");
   New_Line (Spec_File);
   Put_Line (Spec_File, "end GPR.Snames;");
   Close (Spec_File);

   --  Finish the body

   New_Line (Body_File);
   Put_Line (Body_File, "      Initialized := True;");
   Put_Line (Body_File, "   end Initialize;");
   New_Line (Body_File);
   Put_Line (Body_File, "end GPR.Snames;");
   Close (Body_File);

   --  That's it!
end Xsnames;
