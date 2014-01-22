------------------------------------------------------------------------------
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                      G P R I N S T A L L . M A I N                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2012-2014, Free Software Foundation, Inc.          --
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

with Ada.Containers.Indefinite_Ordered_Sets; use Ada;
with Ada.Directories;                        use Ada.Directories;
with Ada.Text_IO;                            use Ada.Text_IO;

with GNAT.MD5; use GNAT.MD5;

with Gpr_Util; use Gpr_Util;

with Opt;
with Osint;
with Output; use Output;

package body Gprinstall.Uninstall is

   package File_Set is new Containers.Indefinite_Ordered_Sets (String);

   -------------
   -- Process --
   -------------

   procedure Process (Install_Name : String) is

      procedure Delete_File (Position : File_Set.Cursor);
      --  Delete file pointed to by Position, do nothing if the file is not
      --  found.

      procedure Do_Delete (Filename : String);
      --  Delete file or display a message if in dry-run mode

      procedure Delete_Empty_Directory (Dir_Name : String);
      --  Delete Dir_Name if empty, if removed try with parent directory

      function Project_Dir return String;
      --  Returns the full pathname to the project directory

      ----------------------------
      -- Delete_Empty_Directory --
      ----------------------------

      procedure Delete_Empty_Directory (Dir_Name : String) is
         Prj_Dir_Len : constant Natural := Global_Prefix_Dir.V'Length - 1;
         Search      : Search_Type;
         Element     : Directory_Entry_Type;
         Empty       : Boolean := True;
      begin
         --  Do not try to remove a directory past the project dir

         if Dir_Name'Length >= Prj_Dir_Len then
            --  Check whether the directory is empty or not

            Start_Search (Search, Dir_Name, Pattern => "");

            Check_Entry : while More_Entries (Search) loop
               Get_Next_Entry (Search, Element);

               if Simple_Name (Element) /= "."
                 and then Simple_Name (Element) /= ".."
               then
                  Empty := False;
                  exit Check_Entry;
               end if;
            end loop Check_Entry;

            End_Search (Search);

            --  If empty delete it

            if Empty then
               begin
                  Delete_Directory (Dir_Name);
               exception
                  --  This can happen if there is still some sym links into
                  --  the directory.
                  when Text_IO.Use_Error =>
                     null;
               end;

               --  And then try recursively with parent directory

               Delete_Empty_Directory (Containing_Directory (Dir_Name));
            end if;
         end if;
      end Delete_Empty_Directory;

      -----------------
      -- Delete_File --
      -----------------

      procedure Delete_File (Position : File_Set.Cursor) is
         Name : constant String := File_Set.Element (Position);
      begin
         Do_Delete (Global_Prefix_Dir.V.all & Name);
      end Delete_File;

      ---------------
      -- Do_Delete --
      ---------------

      procedure Do_Delete (Filename : String) is
      begin
         if Dry_Run then
            Write_Line ("delete " & Filename);

         elsif Exists (Filename) then
            Delete_File (Filename);
            Delete_Empty_Directory (Containing_Directory (Filename));
         end if;
      end Do_Delete;

      -----------------
      -- Project_Dir --
      -----------------

      function Project_Dir return String is
      begin
         if Is_Absolute_Path (Global_Project_Subdir.V.all) then
            return Global_Project_Subdir.V.all;
         else
            return Global_Prefix_Dir.V.all & Global_Project_Subdir.V.all;
         end if;
      end Project_Dir;

      Dir  : constant String := Project_Dir & "manifests";
      Name : constant String := Dir & DS & Install_Name;

      Man     : File_Type;
      Buffer  : String (1 .. 4096);
      Last    : Natural;
      Files   : File_Set.Set;
      Changed : File_Set.Set;

      --  Ranges in Buffer above, we have the MD5 (32 chars) a space and then
      --  the filename.

      subtype MD5_Range is Positive range Message_Digest'Range;
      subtype Name_Range is Positive range MD5_Range'Last + 2 .. Buffer'Last;

      File_Digest     : Message_Digest;
      Expected_Digest : Message_Digest;
      Removed         : Boolean;

   begin
      --  Check if manifest for this project exists

      if not Exists (Name) then
         if not Opt.Quiet_Output then
            Osint.Fail ("Project " & Name & " not found.");
         end if;
         Osint.Exit_Program (Osint.E_Errors);
      end if;

      if not Opt.Quiet_Output then
         Write_Line ("Uninstall project " & Install_Name);
      end if;

      --  Check each file to be deleted

      Open (Man, In_File, Name);

      while not End_Of_File (Man) loop
         Get_Line (Man, Buffer, Last);

         --  Skip first line if it is the original project's signature

         if Last > MD5_Range'Last
           and then Buffer (1 .. 2) /= Sig_Line
         then
            declare
               F_Name : constant String := Buffer (Name_Range'First .. Last);
            begin
               Expected_Digest := Buffer (MD5_Range);

               if Exists (Global_Prefix_Dir.V.all & F_Name) then
                  File_Digest := File_MD5 (Global_Prefix_Dir.V.all & F_Name);
                  Removed := False;
               else
                  Removed := True;
               end if;

               --  Unconditionnaly add a file to the remove list if digest is
               --  ok, if we are running in force mode or the file has already
               --  been removed.

               if File_Digest = Expected_Digest
                 or else Force_Installations
                 or else Removed
               then
                  --  Make sure we always destroy the symbolic links before the
                  --  files itself.

                  Files.Include (F_Name);

               else
                  Changed.Include (F_Name);
               end if;
            end;
         end if;
      end loop;

      Close (Man);

      --  Delete files

      if Changed.Is_Subset (Of_Set => Files) then
         Files.Iterate (Delete_File'Access);

         --  Then finally delete the manifest for this project

         Do_Delete (Name);

      else
         if not Opt.Quiet_Output then
            Write_Line ("Following files have been changed:");

            declare
               procedure Display (Position : File_Set.Cursor);
               --  Display only if not part of Files set

               -------------
               -- Display --
               -------------

               procedure Display (Position : File_Set.Cursor) is
                  F_Name : constant String := File_Set.Element (Position);
               begin
                  if not Files.Contains (F_Name) then
                     Write_Line (F_Name);
                  end if;
               end Display;

            begin
               Changed.Iterate (Display'Access);
            end;

            Write_Line ("use option -f to force file deletion.");
         end if;
      end if;
   end Process;

end Gprinstall.Uninstall;
