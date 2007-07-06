with Util;
procedure Main is
   A : aliased integer;
   procedure C_Routine (x : in out Integer);
   pragma Import (C, C_Routine);

begin
   C_Routine (A);
   Util.Print (A);
end;
