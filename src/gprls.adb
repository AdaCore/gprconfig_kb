------------------------------------------------------------------------------
--                                                                          --
--                             GPR TECHNOLOGY                               --
--                                                                          --
--                       Copyright (C) 2015, AdaCore                        --
--                                                                          --
-- This is  free  software;  you can redistribute it and/or modify it under --
-- terms of the  GNU  General Public License as published by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for more details.  You should have received  a copy of the  GNU  --
-- General Public License distributed with GNAT; see file  COPYING. If not, --
-- see <http://www.gnu.org/licenses/>.                                      --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;

with GPR.Names; use GPR.Names;
with GPR.Opt;   use GPR.Opt;

package body Gprls is

   use Rident;

   function Is_Internal_Unit return Boolean;
   --  Given a unit name stored in Name_Buffer with length in Name_Len,
   --  returns True if this is the name of an internal unit or a child of
   --  an internal unit.

   --------------
   -- Add_File --
   --------------

   procedure Add_File
     (File_Name : String; Source : GPR.Source_Id := No_Source)
   is
   begin
      if Current_Verbosity = High then
         Put_Line ("adding file """ & File_Name & '"');
      end if;

      Number_File_Names := Number_File_Names + 1;

      --  As Add_File may be called for mains specified inside a project file,
      --  File_Names may be too short and needs to be extended.

      if Number_File_Names > File_Names'Last then
         File_Names := new File_Name_Array'(File_Names.all & File_Names.all);
      end if;

      File_Names (Number_File_Names) :=
        (new String'(File_Name), Source, No_ALI_Id);
   end Add_File;

   ------------------------------
   -- Corresponding_Sdep_Entry --
   ------------------------------

   function Corresponding_Sdep_Entry
     (A : ALI_Id;
      U : Unit_Id) return Sdep_Id
   is
   begin
      for D in ALIs.Table (A).First_Sdep .. ALIs.Table (A).Last_Sdep loop
         if Sdep.Table (D).Sfile = Units.Table (U).Sfile then
            return D;
         end if;
      end loop;

--        Error_Msg_Unit_1 := Units.Table (U).Uname;
--        Error_Msg_File_1 := ALIs.Table (A).Afile;
--        New_Line;
--      Error_Msg ("wrong ALI format, can't find dependency line for $ in {");
--        Exit_Program (E_Fatal);
      return No_Sdep_Id;
   end Corresponding_Sdep_Entry;

   -------------------------
   -- Find_General_Layout --
   -------------------------

   procedure Find_General_Layout is
      Max_Unit_Length : Integer := 11;
      Max_Src_Length  : Integer := 11;
      Max_Obj_Length  : Integer := 11;

      Len : Integer;
      --  FS  : File_Name_Type;
      FN_Source : File_Name_Source;
      Id        : ALI_Id;
   begin
      --  Compute maximum of each column

      for J in 1 .. Number_File_Names loop
         FN_Source := File_Names (J);

         Id := FN_Source.The_ALI;
         if Id /= No_ALI_Id then
            Get_Name_String (Units.Table (ALIs.Table (Id).First_Unit).Uname);

            if not Is_Internal_Unit then

               if Print_Unit then
                  Len := Name_Len - 1;
                  Max_Unit_Length := Integer'Max (Max_Unit_Length, Len);
               end if;

               if Print_Source then
                  Get_Name_String (FN_Source.Source.Path.Display_Name);
                  Max_Src_Length := Integer'Max (Max_Src_Length, Name_Len + 1);
               end if;

               if Print_Object then
                  if ALIs.Table (Id).No_Object then
                     Max_Obj_Length :=
                       Integer'Max (Max_Obj_Length, No_Obj'Length);
                  else
                     Get_Name_String
                       (FN_Source.Source.Object_Path);
                     Max_Obj_Length :=
                       Integer'Max (Max_Obj_Length, Name_Len + 1);
                  end if;
               end if;
            end if;
         end if;
      end loop;

      --  Verify is output is not wider than maximum number of columns

      Too_Long :=
        Verbose_Mode
          or else
            (Max_Unit_Length + Max_Src_Length + Max_Obj_Length) > Max_Column;

      --  Set start and end of columns

      Object_Start := 1;
      Object_End   := Object_Start - 1;

      if Print_Object then
         Object_End   := Object_Start + Max_Obj_Length;
      end if;

      Unit_Start := Object_End + 1;
      Unit_End   := Unit_Start - 1;

      if Print_Unit then
         Unit_End   := Unit_Start + Max_Unit_Length;
      end if;

      Source_Start := Unit_End + 1;

      if Source_Start > Spaces'Last then
         Source_Start := Spaces'Last;
      end if;

      Source_End := Source_Start - 1;

      if Print_Source then
         Source_End := Source_Start + Max_Src_Length;
      end if;
   end Find_General_Layout;

   ----------------------
   -- Is_Internal_Unit --
   ----------------------

   function Is_Internal_Unit return Boolean is
      L : Natural renames Name_Len;
      B : String  renames Name_Buffer;
   begin
      return    (L >  3 and then B (1 ..  4) = "ada.")
        or else (L >  6 and then B (1 ..  7) = "system.")
        or else (L > 10 and then B (1 .. 11) = "interfaces.")
        or else (L >  3 and then B (1 ..  4) = "ada%")
        or else (L >  8 and then B (1 ..  9) = "calendar%")
        or else (L >  9 and then B (1 .. 10) = "direct_io%")
        or else (L > 10 and then B (1 .. 11) = "interfaces%")
        or else (L > 13 and then B (1 .. 14) = "io_exceptions%")
        or else (L > 12 and then B (1 .. 13) = "machine_code%")
        or else (L > 13 and then B (1 .. 14) = "sequential_io%")
        or else (L >  6 and then B (1 ..  7) = "system%")
        or else (L >  7 and then B (1 ..  8) = "text_io%")
        or else (L > 20 and then B (1 .. 21) = "unchecked_conversion%")
        or else (L > 22 and then B (1 .. 23) = "unchecked_deallocation%")
        or else (L >  4 and then B (1 ..  5) = "gnat%")
        or else (L >  4 and then B (1 ..  5) = "gnat.");
   end Is_Internal_Unit;

   -------------------
   -- Output_Status --
   -------------------

   procedure Output_Status (FS : File_Status; Verbose : Boolean) is
   begin
      if Verbose then
         case FS is
            when OK =>
               Put (" unchanged");

            when Checksum_OK =>
               Put (" slightly modified");

            when Not_Found =>
               Put (" dependency file not found");

            when Not_Same =>
               Put (" modified");
         end case;

      else
         case FS is
            when OK =>
               Put ("  OK ");

            when Checksum_OK =>
               Put (" MOK ");

            when Not_Found =>
               Put (" ??? ");

            when Not_Same =>
               Put (" DIF ");
         end case;
      end if;
   end Output_Status;

   -----------------
   -- Reset_Print --
   -----------------

   procedure Reset_Print is
   begin
      if not Selective_Output then
         Selective_Output := True;
         Print_Source := False;
         Print_Object := False;
         Print_Unit   := False;
      end if;
   end Reset_Print;

end Gprls;
