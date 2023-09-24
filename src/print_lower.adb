with Ada.Float_Text_IO; use Ada.Float_Text_IO;

procedure Print_Lower (Lower_Bound : Float) is
begin
   if Lower_Bound = 0.0 then
      Put (0.0);
   else
      Put (
        Lower_Bound - 10.0 ** (-Default_Aft), -- round down
        Exp => 0
      );
   end if;
end Print_Lower;
