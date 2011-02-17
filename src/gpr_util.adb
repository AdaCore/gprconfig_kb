------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                            G P R _ U T I L                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2007-2011, Free Software Foundation, Inc.       --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Interfaces.C.Strings;
with Makeutl;  use Makeutl;
with Opt;      use Opt;
with Osint;    use Osint;
with Output;   use Output;
with Tempdir;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;

package body Gpr_Util is

   Libgcc_Subdir_Ptr : Interfaces.C.Strings.chars_ptr;
   pragma Import (C, Libgcc_Subdir_Ptr, "__gnat_default_libgcc_subdir");
   --  Pointer to string indicating the installation subdirectory where a
   --  default shared libgcc might be found.

   GNU_Header  : aliased constant String := "INPUT (";
   GNU_Opening : aliased constant String := """";
   GNU_Closing : aliased constant String := '"' & ASCII.LF;
   GNU_Footer  : aliased constant String := ')' & ASCII.LF;

   --  RTS_Name : String_Access := null;

   -------------------------------
   -- Binder_Exchange_File_Name --
   -------------------------------

   function Binder_Exchange_File_Name
     (Main_Base_Name : File_Name_Type; Prefix : Name_Id)
      return String_Access
   is
      File_Name : constant String := Get_Name_String (Main_Base_Name);
   begin
      Get_Name_String (Prefix);
      Add_Str_To_Name_Buffer (File_Name);
      Add_Str_To_Name_Buffer (Binder_Exchange_Suffix);
      return new String'(Name_Buffer (1 .. Name_Len));
   end Binder_Exchange_File_Name;

   --------------------------
   -- Create_Response_File --
   --------------------------

   procedure Create_Response_File
     (Format            : Response_File_Format;
      Objects           : String_List;
      Other_Arguments   : String_List;
      Resp_File_Options : String_List;
      Name_1            : out Path_Name_Type;
      Name_2            : out Path_Name_Type)
   is
      Resp_File : File_Descriptor;
      Status    : Integer;
      pragma Warnings (Off, Status);
      Closing_Status : Boolean;
      pragma Warnings (Off, Closing_Status);

      function Modified_Argument (Arg : String) return String;
      --  If the argument includes a space, a backslash, or a double quote,
      --  escape the character with a preceding backsash.

      -----------------------
      -- Modified_Argument --
      -----------------------

      function Modified_Argument (Arg : String) return String is
         Result : String (1 .. 2 * Arg'Length);
         Last   : Natural := 0;

         procedure Add (C : Character);

         procedure Add (C : Character) is
         begin
            Last := Last + 1;
            Result (Last) := C;
         end Add;

      begin
         for J in Arg'Range loop
            if Arg (J) = '\' or else Arg (J) = ' ' or else Arg (J) = '"' then
               Add ('\');
            end if;

            Add (Arg (J));
         end loop;

         return Result (1 .. Last);
      end Modified_Argument;

   begin
      Name_2 := No_Path;
      Tempdir.Create_Temp_File (Resp_File, Name => Name_1);

      if Format = GNU or else Format = GCC_GNU then
         Status := Write (Resp_File, GNU_Header'Address, GNU_Header'Length);
      end if;

      for J in Objects'Range loop
         if Format = GNU or else Format = GCC_GNU then
            Status :=
              Write (Resp_File, GNU_Opening'Address, GNU_Opening'Length);
         end if;

         Status :=
           Write (Resp_File, Objects (J).all'Address, Objects (J)'Length);

         if Format = GNU or else Format = GCC_GNU then
            Status :=
              Write (Resp_File, GNU_Closing'Address, GNU_Closing'Length);

         else
            Status :=
              Write (Resp_File, ASCII.LF'Address, 1);
         end if;
      end loop;

      if Format = GNU or else Format = GCC_GNU then
         Status :=
           Write (Resp_File, GNU_Footer'Address, GNU_Footer'Length);
      end if;

      case Format is
         when GCC_GNU | GCC_Object_List | GCC_Option_List =>
            Close (Resp_File, Closing_Status);
            Name_2 := Name_1;
            Tempdir.Create_Temp_File (Resp_File, Name => Name_1);

            declare
               Arg      : constant String :=
                 Modified_Argument (Get_Name_String (Name_2));

            begin
               for J in Resp_File_Options'Range loop
                  Status :=
                    Write
                      (Resp_File,
                       Resp_File_Options (J) (1)'Address,
                       Resp_File_Options (J)'Length);

                  if J < Resp_File_Options'Last then
                     Status := Write (Resp_File, ASCII.LF'Address, 1);
                  end if;
               end loop;

               Status := Write (Resp_File, Arg (1)'Address, Arg'Length);
            end;

            Status := Write (Resp_File, ASCII.LF'Address, 1);

         when GCC =>
            null;

         when others =>
            Close (Resp_File, Closing_Status);
      end case;

      if        Format = GCC
        or else Format = GCC_GNU
        or else Format = GCC_Object_List
        or else Format = GCC_Option_List
      then
         for J in Other_Arguments'Range loop
            declare
               Arg : constant String :=
                 Modified_Argument (Other_Arguments (J).all);

            begin
               Status :=
                 Write (Resp_File, Arg (1)'Address, Arg'Length);
            end;

            Status :=
              Write (Resp_File, ASCII.LF'Address, 1);
         end loop;

         Close (Resp_File, Closing_Status);
      end if;
   end Create_Response_File;

   ------------------------------
   -- Get_Compiler_Driver_Path --
   ------------------------------

   function Get_Compiler_Driver_Path
     (Project_Tree : Project_Tree_Ref;
      Lang         : Language_Ptr) return String_Access is
   begin
      if Lang.Config.Compiler_Driver_Path = null then
         declare
            Compiler_Name : constant String :=
              Get_Name_String (Lang.Config.Compiler_Driver);

         begin
            if Compiler_Name = "" then
               return null;
            end if;

            Lang.Config.Compiler_Driver_Path :=
              Locate_Exec_On_Path (Compiler_Name);

            if Lang.Config.Compiler_Driver_Path = null then
               Fail_Program
                 (Project_Tree, "unable to locate """ & Compiler_Name & '"');
            end if;
         end;
      end if;

      return Lang.Config.Compiler_Driver_Path;
   end Get_Compiler_Driver_Path;

   ----------------------------
   -- Find_Binding_Languages --
   ----------------------------

   procedure Find_Binding_Languages
     (Project_Tree : Project_Tree_Ref)
   is
      B_Data : Binding_Data;

      Language_Name      : Name_Id;
      Binder_Driver_Name : File_Name_Type := No_File;
      Binder_Driver_Path : String_Access;
      Binder_Prefix      : Name_Id;
      Language           : Language_Ptr;

      Config        : Language_Config;
      Project       : Project_List;
      Found         : Boolean;

   begin
      There_Are_Binder_Drivers := False;

      Project := Project_Tree.Projects;
      while Project /= null loop
         Language := Project.Project.Languages;

         while Language /= No_Language_Index loop
            Config := Language.Config;

            Binder_Driver_Name := Config.Binder_Driver;

            if Language.First_Source /= No_Source
              and then Binder_Driver_Name /= No_File
            then
               There_Are_Binder_Drivers := True;
               Language_Name := Language.Name;

               Found := False;
               for B_Index in 1 .. Binding_Languages.Last loop
                  if Binding_Languages.Table (B_Index).Language_Name =
                    Language_Name
                  then
                     Found := True;
                     exit;
                  end if;
               end loop;

               if not Found then
                  Get_Name_String (Binder_Driver_Name);
                  Binder_Driver_Path :=
                    Locate_Exec_On_Path (Name_Buffer (1 .. Name_Len));

                  if Binder_Driver_Path = null then
                     Fail_Program
                       (Project_Tree,
                        "unable to find binder driver " &
                        Name_Buffer (1 .. Name_Len));
                  end if;

                  if Config.Binder_Prefix = No_Name then
                     Binder_Prefix := Empty_String;

                  else
                     Binder_Prefix := Config.Binder_Prefix;
                  end if;

                  for B_Index in 1 .. Binding_Languages.Last loop
                     if Binder_Prefix =
                       Binding_Languages.Table (B_Index).Binder_Prefix
                     then
                        Fail_Program
                          (Project_Tree,
                           "binding prefix cannot be the same for"
                           & " two languages");
                     end if;
                  end loop;

                  B_Data :=
                    (Language           => Language,
                     Language_Name      => Language_Name,
                     Binder_Driver_Name => Binder_Driver_Name,
                     Binder_Driver_Path => Binder_Driver_Path,
                     Binder_Prefix      => Binder_Prefix);
                  Binding_Languages.Append (B_Data);
               end if;
            end if;

            Language := Language.Next;
         end loop;

         Project := Project.Next;
      end loop;
   end Find_Binding_Languages;

   ------------------------------
   -- Look_For_Default_Project --
   ------------------------------

   procedure Look_For_Default_Project is
   begin
      if Is_Regular_File (Default_Project_File_Name) then
         Project_File_Name := new String'(Default_Project_File_Name);

      else
         --  Check if there is a single project file in the current
         --  directory. If there is one and only one, use it.

         declare
            Dir : Dir_Type;
            Str : String (1 .. 255);
            Last : Natural;
            Single : String_Access := null;

         begin
            Open (Dir, ".");

            loop
               Read (Dir, Str, Last);
               exit when Last = 0;

               if Last > Project_File_Extension'Length and then
                 Is_Regular_File (Str (1 .. Last))
               then
                  Canonical_Case_File_Name (Str (1 .. Last));

                  if Str (Last - Project_File_Extension'Length + 1 .. Last)
                    = Project_File_Extension
                  then
                     if Single = null then
                        Single := new String'(Str (1 .. Last));

                     else
                        --  There are several project files in the current
                        --  directory. Reset Single to null and exit.

                        Single := null;
                        exit;
                     end if;
                  end if;
               end if;
            end loop;

            Close (Dir);

            Project_File_Name := Single;
         end;
      end if;

      if (not Quiet_Output) and then Project_File_Name /= null then
         Write_Str ("using project file ");
         Write_Line (Project_File_Name.all);
      end if;
   end Look_For_Default_Project;

   -------------------------
   -- Normalized_Hostname --
   -------------------------

--     function Normalized_Hostname return String is
--        Id : Targets_Set_Id;
--     begin
--        Get_Targets_Set (Base, Hostname, Id);
--        return Normalized_Target (Base, Id);
--     end Normalized_Hostname;

   ------------------
   -- Partial_Name --
   ------------------

   function Partial_Name
     (Lib_Name      : String;
      Number        : Natural;
      Object_Suffix : String) return String
   is
      Img : constant String := Number'Img;
   begin
      return
        Partial_Prefix & Lib_Name &
        '_' & Img (Img'First + 1 .. Img'Last)
        & Object_Suffix;
   end Partial_Name;

   -----------------------
   -- Shared_Libgcc_Dir --
   -----------------------

   function Shared_Libgcc_Dir (Run_Time_Dir : String) return String is
      Path      : String (1 .. Run_Time_Dir'Length + 15);
      Path_Last : constant Natural := Run_Time_Dir'Length;
      GCC_Index : Natural := 0;

   begin
      Path (1 .. Path_Last) := Run_Time_Dir;
      GCC_Index := Index (Path (1 .. Path_Last), "gcc-lib");

      if GCC_Index /= 0 then
         --  This is gcc 2.8.2: the shared version of libgcc is
         --  located in the parent directory of "gcc-lib".

         GCC_Index := GCC_Index - 1;

      else
         GCC_Index := Index (Path (1 .. Path_Last), "/lib/");

         if GCC_Index = 0 then
            GCC_Index :=
              Index
                (Path (1 .. Path_Last),
                 Directory_Separator & "lib" & Directory_Separator);
         end if;

         if GCC_Index /= 0 then
            --  We have found "lib" as a subdirectory in the runtime dir path.
            --  The
            declare
               Subdir : constant String :=
                 Interfaces.C.Strings.Value (Libgcc_Subdir_Ptr);
            begin
               Path
                 (GCC_Index + 1 ..
                    GCC_Index + Subdir'Length) :=
                   Subdir;
               GCC_Index :=
                 GCC_Index + Subdir'Length;
            end;
         end if;
      end if;

      return Path (1 .. GCC_Index);
   end Shared_Libgcc_Dir;

   package body Knowledge is separate;

end Gpr_Util;
