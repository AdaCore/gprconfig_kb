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

with GPR.Err;
with GPR.Names; use GPR.Names;
with GPR.Opt;   use GPR.Opt;
with GPR.Scans;
with GPR.Sinput;

package body Gprls is

   No_Obj : aliased String := "<no_obj>";

   use GPR.Stamps;

   procedure Find_Status
     (Source   : GPR.Source_Id;
      Stamp    : Time_Stamp_Type;
      Checksum : Word;
      Status   : out File_Status);
   --  Determine the file status (Status) of the file represented by FS with
   --  the expected Stamp and checksum given as argument. FS will be updated
   --  to the full file name if available.

   use Rident;

   -------------
   -- Add_ALI --
   -------------

   procedure Add_ALI
     (ALI_Name : File_Name_Type;
      Spec     : Boolean;
      Source   : GPR.Source_Id)
   is
      A : constant ALI_Kind := (File => ALI_Name, Spec => Spec);
   begin
      ALI_Names.Set (A, Source);
   end Add_ALI;

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

   --------------
   -- Find_ALI --
   --------------

   function Find_ALI (Source : GPR.Source_Id) return ALI_Id is
      Text    : Text_Buffer_Ptr;
      Result  : ALI_Id;
   begin
      Text := Osint.Read_Library_Info (File_Name_Type (Source.Dep_Path));

      if Text /= null then
         Result := Scan_ALI
           (F          => File_Name_Type (Source.Dep_Path),
            T          => Text,
            Ignore_ED  => False,
            Err        => True,
            Read_Lines => "WD");
         Free (Text);
         return Result;

      else
         return No_ALI_Id;
      end if;
   end Find_ALI;

   -----------------
   -- Find_Source --
   -----------------

   function Find_Source
     (ALI_Name : File_Name_Type;
      Spec     : Boolean)
      return GPR.Source_Id
   is
      A : constant ALI_Kind := (File => ALI_Name, Spec => Spec);
   begin
      return ALI_Names.Get (A);
   end Find_Source;

   -----------------
   -- Find_Status --
   -----------------

   procedure Find_Status
     (Source : GPR.Source_Id;
      ALI    : ALI_Id;
      Status : out File_Status)
   is
      U : Unit_Id;
   begin
      if ALI = No_ALI_Id then
         Status := Not_Found;
      else
         if Source.Kind = Spec then
            U := ALIs.Table (ALI).Last_Unit;
         else
            U := ALIs.Table (ALI).First_Unit;
         end if;

         Find_Status (Source, ALI, U, Status);
      end if;
   end Find_Status;

   procedure Find_Status
     (Source : GPR.Source_Id;
      ALI    : ALI_Id;
      U      : Unit_Id;
      Status : out File_Status)
   is
      use GPR.Scans;
      Stamp : constant Time_Stamp_Type := File_Stamp (Source.Path.Name);
      SD : constant Sdep_Id := Corresponding_Sdep_Entry (ALI, U);
      Source_Index : Source_File_Index;
      Checksums_Match : Boolean;
   begin
      if Stamp = Sdep.Table (SD).Stamp then
         Status := OK;

      else
         Checksums_Match := False;
         Source_Index :=
           Sinput.Load_File (Get_Name_String (Source.Path.Name));

         if Source_Index /= No_Source_File then

            Err.Scanner.Initialize_Scanner
              (Source_Index, Err.Scanner.Ada);

            --  Scan the complete file to compute its
            --  checksum.

            loop
               Err.Scanner.Scan;
               exit when Token = Tok_EOF;
            end loop;

            if Scans.Checksum = Sdep.Table (SD).Checksum then
               Checksums_Match := True;
            end if;
         end if;

         if Checksums_Match then
            Status := Checksum_OK;

         else
            Status := Not_Same;
         end if;
      end if;
   end Find_Status;

   procedure Find_Status
     (Source   : GPR.Source_Id;
      Stamp    : Time_Stamp_Type;
      Checksum : Word;
      Status   : out File_Status)
   is
      Source_Index : Source_File_Index;
      Checksums_Match : Boolean;
      use GPR.Scans;

   begin
      if Source = No_Source then
         Status := Not_Found;

      elsif File_Stamp (Source.Path.Name) = Stamp then
         Status := OK;

      else
         Checksums_Match := False;
         Source_Index :=
           Sinput.Load_File (Get_Name_String (Source.Path.Name));

         if Source_Index /= No_Source_File then

            Err.Scanner.Initialize_Scanner
              (Source_Index, Err.Scanner.Ada);

            --  Scan the complete file to compute its
            --  checksum.

            loop
               Err.Scanner.Scan;
               exit when Token = Tok_EOF;
            end loop;

            if Scans.Checksum = Checksum then
               Checksums_Match := True;
            end if;
         end if;

         if Checksums_Match then
            Status := Checksum_OK;

         else
            Status := Not_Same;
         end if;
      end if;
   end Find_Status;

   ----------
   -- Hash --
   ----------

   function Hash (A : ALI_Kind) return GPR.Header_Num is
   begin
      return GPR.Hash (A.File);
   end Hash;

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

         Put_Line (Object_Name.all);

      end if;
   end Output_Object;

   -------------------
   -- Output_Source --
   -------------------

   procedure Output_Source
     (Source : GPR.Source_Id; ALI : ALI_Id; U : Unit_Id := No_Unit_Id)
   is
      Status : File_Status;
   begin
      if U = No_Unit_Id then
         Find_Status (Source, ALI, Status);
      else
         Find_Status (Source, ALI, U, Status);
      end if;

      Put ("     ");

      if Verbose_Mode then
         Put ("Source => ");
         Put (Get_Name_String (Source.Path.Display_Name));
         Output_Status (Status, True);
         New_Line;

      else
         Output_Status (Status, False);
         Put_Line (Get_Name_String (Source.Path.Display_Name));
      end if;
   end Output_Source;

   procedure Output_Source (Sdep_I : Sdep_Id) is
      Stamp       : GPR.Stamps.Time_Stamp_Type;
      Checksum    : Word;
      FS          : File_Name_Type;
      Source      : GPR.Source_Id;
      Status      : File_Status;
      Source_Name : String_Access;

   begin
      if Sdep_I = No_Sdep_Id then
         return;
      end if;

      Stamp    := Sdep.Table (Sdep_I).Stamp;
      Checksum := Sdep.Table (Sdep_I).Checksum;
      FS       := Sdep.Table (Sdep_I).Sfile;

      Source := Source_Files_Htable.Get (Project_Tree.Source_Files_HT, FS);

      if Print_Source then
         Find_Status (Source, Stamp, Checksum, Status);
         Get_Name_String (FS);

         Source_Name := new String'(Name_Buffer (1 .. Name_Len));

         if Verbose_Mode then
            Put ("   Source => ");
            Put (Source_Name.all);

            Output_Status (Status, Verbose => True);
            New_Line;

         else
            if not Selective_Output then
               Put ("   ");
               Output_Status (Status, Verbose => False);
            end if;

            Put_Line (Source_Name.all);
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
   -- Output_Unit --
   -----------------

   procedure Output_Unit (U_Id : Unit_Id) is
      Kind : Character;
      U    : Unit_Record renames Units.Table (U_Id);

   begin
      Get_Name_String (U.Uname);
      Kind := Name_Buffer (Name_Len);
      Name_Len := Name_Len - 2;

      if not Verbose_Mode then
         Put_Line ("   " & Name_Buffer (1 .. Name_Len));

      else
         Put ("   Unit => ");
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
         if U.Preelab            or else
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

            New_Line;
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

end Gprls;
