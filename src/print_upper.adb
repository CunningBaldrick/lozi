with Ada.Float_Text_IO; use Ada.Float_Text_IO;

procedure Print_Upper (Upper_Bound : Float) is
begin
   if Upper_Bound = 0.0 then
      Put (0.0);
   else
      Put (
        Upper_Bound + 10.0 ** (-Default_Aft), -- round up
        Exp => 0
      );
   end if;
end Print_Upper;
