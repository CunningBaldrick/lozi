with Ada.Command_Line;
with Ada.Numerics.Long_Elementary_Functions;
with Ada.Text_IO;
with Integers;
with Integers.IO;

procedure Generate_Curve is
   use Ada;
   use Ada.Numerics.Long_Elementary_Functions;
   use Ada.Text_IO;
   use Integers;

   A_Numerator_When_B_Is_One     : Integer_Type;
   A_Denominator_When_B_Is_One   : Integer_Type;
   Minimum_Value_Of_B            : Long_Float;
   Number_Of_Points              : Positive;
   Square_Root_Of_B_Numerator    : Positive;
   Square_Root_Of_B_Denominator  : Positive;

   Spare : Integer_Type;
begin
   if Command_Line.Argument_Count /= 4 then
      Put (Standard_Error, "Usage: ");
      Put (Standard_Error, Command_Line.Command_Name);
      Put (Standard_Error, " A_Numerator_When_B_Is_One A_Denominator_When_B_Is_One"
        & " Minimum_Value_Of_B Number_Of_Points");
      New_Line (Standard_Error);
      Flush (Standard_Error);
      return;
   end if;

   begin
      A_Numerator_When_B_Is_One := To_Integer_Type (
        Integer'Value (Command_Line.Argument (1))
      );
      A_Denominator_When_B_Is_One := To_Integer_Type (
        Integer'Value (Command_Line.Argument (2))
      );
   exception
      when Constraint_Error =>
         Put (Standard_Error, "Error: ");
         Put (Standard_Error, "'A' parameters must be integers");
         New_Line (Standard_Error);
         Flush (Standard_Error);
         return;
   end;

   begin
      Minimum_Value_Of_B := Long_Float'Value (Command_Line.Argument (3));
      if Minimum_Value_Of_B < 0.0 then
         raise Constraint_Error;
      end if;
      if Minimum_Value_Of_B >= 1.0 then
         raise Constraint_Error;
      end if;
   exception
      when Constraint_Error =>
         Put (Standard_Error, "Error: ");
         Put (Standard_Error, "Minimum_Value_Of_B must be a non-negative float smaller than 1");
         New_Line (Standard_Error);
         Flush (Standard_Error);
         return;
   end;

   begin
      Number_Of_Points := Positive'Value (Command_Line.Argument (4));
   exception
      when Constraint_Error =>
         Put (Standard_Error, "Error: ");
         Put (Standard_Error, "Number_Of_Points must be a positive integer");
         New_Line (Standard_Error);
         Flush (Standard_Error);
         return;
   end;

   --  Write sqrt(Minimum_Value_Of_B) as a ratio of integers
   --  Square_Root_Of_B_Numerator / Square_Root_Of_B_Denominator
   --  such that Square_Root_Of_B_Denominator - Square_Root_Of_B_Numerator
   --  equals Number_Of_Points.
   Square_Root_Of_B_Denominator := Integer (
     Long_Float (Number_Of_Points) / (1.0 - Sqrt (Minimum_Value_Of_B)) + 0.5
   );
   Square_Root_Of_B_Numerator := Integer (
     Long_Float (Square_Root_Of_B_Denominator) * Sqrt (Minimum_Value_Of_B) + 0.5
   );

   --  Remove common factors.
   Spare := GCD (A_Numerator_When_B_Is_One, A_Denominator_When_B_Is_One);
   A_Numerator_When_B_Is_One   := A_Numerator_When_B_Is_One / Spare;
   A_Denominator_When_B_Is_One := A_Denominator_When_B_Is_One / Spare;

   while Square_Root_Of_B_Numerator <= Square_Root_Of_B_Denominator loop
      declare
         A_Numerator   : Integer_Type;
         A_Denominator : Integer_Type;
         B_Numerator   : Integer_Type;
         B_Denominator : Integer_Type;

         Square_Root_Of_B_Numerator   : Integer_Type
           := To_Integer_Type (Generate_Curve.Square_Root_Of_B_Numerator);
         Square_Root_Of_B_Denominator : Integer_Type
           := To_Integer_Type (Generate_Curve.Square_Root_Of_B_Denominator);
      begin
         --  Remove common factors.
         Spare := GCD (Square_Root_Of_B_Numerator, Square_Root_Of_B_Denominator);
         Square_Root_Of_B_Numerator   := Square_Root_Of_B_Numerator / Spare;
         Square_Root_Of_B_Denominator := Square_Root_Of_B_Denominator / Spare;

         A_Numerator   := A_Numerator_When_B_Is_One   * Square_Root_Of_B_Numerator;
         A_Denominator := A_Denominator_When_B_Is_One * Square_Root_Of_B_Denominator;

         B_Numerator   := Square_Root_Of_B_Numerator * Square_Root_Of_B_Numerator;
         B_Denominator := Square_Root_Of_B_Denominator * Square_Root_Of_B_Denominator;

         Integers.IO.Put (A_Numerator);
         Put ("  ");
         Integers.IO.Put (A_Denominator);
         Put ("  ");
         Integers.IO.Put (B_Numerator);
         Put ("  ");
         Integers.IO.Put (B_Denominator);
         New_Line;
      end;

      Square_Root_Of_B_Numerator := Square_Root_Of_B_Numerator + 1;
   end loop;
end Generate_Curve;
