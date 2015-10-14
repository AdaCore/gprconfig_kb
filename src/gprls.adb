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

with GNAT.Case_Util; use GNAT.Case_Util;

with GPR.Names; use GPR.Names;
with GPR.Opt;   use GPR.Opt;

package body Gprls is

   use Rident;

   function Image (Restriction : Restriction_Id) return String;
   --  Returns the capitalized image of Restriction

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

      File_Names (Number_File_Names) := (new String'(File_Name), Source);
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

   begin
      --  Compute maximum of each column

      for Id in ALIs.First .. ALIs.Last loop
         Get_Name_String (Units.Table (ALIs.Table (Id).First_Unit).Uname);

         if not Is_Internal_Unit then

            if Print_Unit then
               Len := Name_Len - 1;
               Max_Unit_Length := Integer'Max (Max_Unit_Length, Len);
            end if;

            if Print_Source then
               --  FS := Full_Source_Name (ALIs.Table (Id).Sfile);

               --  if FS = No_File then
               Get_Name_String (ALIs.Table (Id).Sfile);
               Name_Len := Name_Len + 13;
               --  else
               --     Get_Name_String (FS);
               --  end if;

               Max_Src_Length := Integer'Max (Max_Src_Length, Name_Len + 1);
            end if;

            if Print_Object then
               if ALIs.Table (Id).No_Object then
                  Max_Obj_Length :=
                    Integer'Max (Max_Obj_Length, No_Obj'Length);
               else
                  Get_Name_String (ALIs.Table (Id).Ofile_Full_Name);
                  Max_Obj_Length := Integer'Max (Max_Obj_Length, Name_Len + 1);
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

   -----------------
   -- Find_Status --
   -----------------

   procedure Find_Status
     (FS       : in out File_Name_Type;
      Stamp    : Time_Stamp_Type;
      Checksum : Word;
      Status   : out File_Status)
   is
      pragma Unreferenced (Checksum);

      Tmp1 : File_Name_Type;
      --  Tmp2 : File_Name_Type;

   begin
      --  Tmp1 := Full_Source_Name (FS);
      Tmp1 := FS;

      --  if Tmp1 = No_File then
      --     Status := Not_Found;

         --  els
      if File_Stamp (Tmp1) = Stamp then
         FS     := Tmp1;
         Status := OK;

      else
         Status := Not_Found;

      --  elsif Checksums_Match (Get_File_Checksum (FS), Checksum) then
      --   FS := Tmp1;
      --   Status := Checksum_OK;

      --  else
      --     Tmp2 := Matching_Full_Source_Name (FS, Stamp);

      --     if Tmp2 = No_File then
      --        Status := Not_Same;
      --        FS     := Tmp1;

      --     else
      --        Status := Not_First_On_PATH;
      --        FS := Tmp2;
      --     end if;
      end if;
   end Find_Status;

   -----------
   -- Image --
   -----------

   function Image (Restriction : Restriction_Id) return String is
      Result : String := Restriction'Img;
      Skip   : Boolean := True;

   begin
      for J in Result'Range loop
         if Skip then
            Skip := False;
            Result (J) := To_Upper (Result (J));

         elsif Result (J) = '_' then
            Skip := True;

         else
            Result (J) := To_Lower (Result (J));
         end if;
      end loop;

      return Result;
   end Image;

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
   -- Output_Object --
   -------------------

   procedure Output_Object (O : File_Name_Type) is
      Object_Name : String_Access;

   begin
      if Print_Object then
         if O /= No_File then
            Get_Name_String (O);
            Object_Name := new String'(Name_Buffer (1 .. Name_Len));
         else
            Object_Name := No_Obj'Unchecked_Access;
         end if;

         Put (Object_Name.all);

         if Print_Source or else Print_Unit then
            if Too_Long then
               New_Line;
               Put ("   ");
            else
               Put (Spaces (Object_Start + Object_Name'Length .. Object_End));
            end if;
         end if;
      end if;
   end Output_Object;

   -------------------
   -- Output_Source --
   -------------------

   procedure Output_Source (Sdep_I : Sdep_Id) is
      Stamp       : Time_Stamp_Type;
      Checksum    : Word;
      FS          : File_Name_Type;
      Status      : File_Status;
      Object_Name : String_Access;

   begin
      if Sdep_I = No_Sdep_Id then
         return;
      end if;

      Stamp    := Sdep.Table (Sdep_I).Stamp;
      Checksum := Sdep.Table (Sdep_I).Checksum;
      FS       := Sdep.Table (Sdep_I).Sfile;

      if Print_Source then
         Find_Status (FS, Stamp, Checksum, Status);
         Get_Name_String (FS);

         Object_Name := new String'(Name_Buffer (1 .. Name_Len));

         if Verbose_Mode then
            Put ("  Source => ");
            Put (Object_Name.all);

            if not Too_Long then
               Put
                 (Spaces (Source_Start + Object_Name'Length .. Source_End));
            end if;

            Output_Status (Status, Verbose => True);
            New_Line;
            Put ("   ");

         else
            if not Selective_Output then
               Output_Status (Status, Verbose => False);
            end if;

            Put (Object_Name.all);
         end if;
      end if;
   end Output_Source;

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
               Put (" file not found");

            when Not_Same =>
               Put (" modified");

            when Not_First_On_PATH =>
               Put (" unchanged version not first on PATH");
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

            when Not_First_On_PATH =>
               Put (" HID ");
         end case;
      end if;
   end Output_Status;

   -----------------
   -- Output_Unit --
   -----------------

   procedure Output_Unit (ALI : ALI_Id; U_Id : Unit_Id) is
      Kind : Character;
      U    : Unit_Record renames Units.Table (U_Id);

   begin
      if Print_Unit then
         Get_Name_String (U.Uname);
         Kind := Name_Buffer (Name_Len);
         Name_Len := Name_Len - 2;

         if not Verbose_Mode then
            Put (Name_Buffer (1 .. Name_Len));

         else
            Put ("Unit => ");
            New_Line;
            Put ("     Name   => ");
            Put (Name_Buffer (1 .. Name_Len));
            New_Line;
            Put ("     Kind   => ");

            if Units.Table (U_Id).Unit_Kind = 'p' then
               Put ("package ");
            else
               Put ("subprogram ");
            end if;

            if Kind = 's' then
               Put ("spec");
            else
               Put ("body");
            end if;
         end if;

         if Verbose_Mode then
            if U.Preelab             or else
               U.No_Elab             or else
               U.Pure                or else
               U.Dynamic_Elab        or else
               U.Has_RACW            or else
               U.Remote_Types        or else
               U.Shared_Passive      or else
               U.RCI                 or else
               U.Predefined          or else
               U.Internal            or else
               U.Is_Generic          or else
               U.Init_Scalars        or else
               U.SAL_Interface       or else
               U.Body_Needed_For_SAL or else
               U.Elaborate_Body
            then
               New_Line;
               Put ("     Flags  =>");

               if U.Preelab then
                  Put (" Preelaborable");
               end if;

               if U.No_Elab then
                  Put (" No_Elab_Code");
               end if;

               if U.Pure then
                  Put (" Pure");
               end if;

               if U.Dynamic_Elab then
                  Put (" Dynamic_Elab");
               end if;

               if U.Has_RACW then
                  Put (" Has_RACW");
               end if;

               if U.Remote_Types then
                  Put (" Remote_Types");
               end if;

               if U.Shared_Passive then
                  Put (" Shared_Passive");
               end if;

               if U.RCI then
                  Put (" RCI");
               end if;

               if U.Predefined then
                  Put (" Predefined");
               end if;

               if U.Internal then
                  Put (" Internal");
               end if;

               if U.Is_Generic then
                  Put (" Is_Generic");
               end if;

               if U.Init_Scalars then
                  Put (" Init_Scalars");
               end if;

               if U.SAL_Interface then
                  Put (" SAL_Interface");
               end if;

               if U.Body_Needed_For_SAL then
                  Put (" Body_Needed_For_SAL");
               end if;

               if U.Elaborate_Body then
                  Put (" Elaborate Body");
               end if;

               if U.Remote_Types then
                  Put (" Remote_Types");
               end if;

               if U.Shared_Passive then
                  Put (" Shared_Passive");
               end if;

               if U.Predefined then
                  Put (" Predefined");
               end if;
            end if;

            declare
               Restrictions : constant Restrictions_Info :=
                                ALIs.Table (ALI).Restrictions;

            begin
               --  If the source was compiled with pragmas Restrictions,
               --  Display these restrictions.

               if Restrictions.Set /= (All_Restrictions => False) then
                  New_Line;
                  Put ("     pragma Restrictions  =>");

                  --  For boolean restrictions, just display the name of the
                  --  restriction; for valued restrictions, also display the
                  --  restriction value.

                  for Restriction in All_Restrictions loop
                     if Restrictions.Set (Restriction) then
                        New_Line;
                        Put ("       ");
                        Put (Image (Restriction));

                        if Restriction in All_Parameter_Restrictions then
                           Put (" =>");
                           Put (Restrictions.Value (Restriction)'Img);
                        end if;
                     end if;
                  end loop;
               end if;

               --  If the unit violates some Restrictions, display the list of
               --  these restrictions.

               if Restrictions.Violated /= (All_Restrictions => False) then
                  New_Line;
                  Put ("     Restrictions violated =>");

                  --  For boolean restrictions, just display the name of the
                  --  restriction. For valued restrictions, also display the
                  --  restriction value.

                  for Restriction in All_Restrictions loop
                     if Restrictions.Violated (Restriction) then
                        New_Line;
                        Put ("       ");
                        Put (Image (Restriction));

                        if Restriction in All_Parameter_Restrictions then
                           if Restrictions.Count (Restriction) > 0 then
                              Put (" =>");

                              if Restrictions.Unknown (Restriction) then
                                 Put (" at least");
                              end if;

                              Put (Restrictions.Count (Restriction)'Img);
                           end if;
                        end if;
                     end if;
                  end loop;
               end if;
            end;
         end if;

         if Print_Source then
            if Too_Long then
               New_Line;
               Put ("   ");
            else
               Put (Spaces (Unit_Start + Name_Len + 1 .. Unit_End));
            end if;
         end if;
      end if;
   end Output_Unit;

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

   --  procedure Search_RTS (Name : String);
   --  Find include and objects path for the RTS name.

   ---------------
   -- Normalize --
   ---------------

   function Normalize (Path : String) return String is
   begin
      return Normalize_Pathname (Path);
   end Normalize;

end Gprls;
