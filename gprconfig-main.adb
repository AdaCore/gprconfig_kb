------------------------------------------------------------------------------
--                   Copyright (C) 2006, AdaCore                            --
------------------------------------------------------------------------------

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;
with GprConfig.Knowledge;   use GprConfig.Knowledge;

procedure GprConfig.Main is
   use Compiler_Lists;

   function "+" (Str : String) return Unbounded_String
     renames Ada.Strings.Unbounded.To_Unbounded_String;

   Base      : Knowledge_Base;
   Compilers : Compiler_Lists.List;
   Comp      : Compiler_Lists.Cursor;
   Selected  : Compiler_Lists.List;
begin
   Parse_Knowledge_Base (Base);
   Find_Compilers_In_Path (Base, Compilers);

   Put_Line ("----------------- List of compilers found on PATH -------");
   Comp := First (Compilers);
   while Has_Element (Comp) loop
      Put_Line (To_String (Element (Comp).Name) & ": "
                & To_String (Element (Comp).Path)
                & " - " & To_String (Element (Comp).Version)
                & " - " & To_String (Element (Comp).Language)
                & " - " & To_String (Element (Comp).Runtime));
      Next (Comp);
   end loop;

   Put_Line ("--------------- Selecting GNAT 5.05w Ada native compiler ---");
   Append
     (Selected,
      (Name       => +"GNAT",
       Version    => +"5.05w",
       Language   => +"Ada",
       Runtime    => +"native",
       Path       => Null_Unbounded_String,
       Extra_Tool => Null_Unbounded_String));
--     Append
--       (Selected,
--        (Name       => +"GCC",
--         Version    => +"3.4.1",
--         Language   => +"C",
--         Runtime    => Null_Unbounded_String,
--         Path       => Null_Unbounded_String,
--         Extra_Tool => Null_Unbounded_String));

   Put_Line ("--------------- Generating output -----------");
   Generate_Configuration (Base, Selected, "standard.gpr");

end GprConfig.Main;
