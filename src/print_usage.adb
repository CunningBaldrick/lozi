with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;

procedure Print_Usage is
begin
   Put (Standard_Error, "Usage: ");
   Put (Standard_Error, Command_Name);
   Put (Standard_Error, " [-v]");
   Put (Standard_Error, " A_Numerator A_Denominator");
   Put (Standard_Error, " B_Numerator B_Denominator");
   Put (Standard_Error, " [desired accuracy]");
   New_Line (Standard_Error);
   Flush (Standard_Error);
end Print_Usage;
