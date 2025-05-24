package body Lozi is

   LCM : Integer_Type; -- Positive.
   --  Least common multiple of A_Denominator, B_Denominator and C_Denominator.

   A_Mult : Integer_Type;
   --  A_Numerator * (LCM / A_Denominator)

   B_Mult : Integer_Type;
   --  B_Numerator * (LCM / B_Denominator)

   C_Mult : Integer_Type;
   --  C_Numerator * (LCM / C_Denominator)

   Orientation_Preserving : Boolean;

   ---------------------------
   -- Preserves_Orientation --
   ---------------------------
   function Preserves_Orientation return Boolean is
   begin
      return Orientation_Preserving;
   end Preserves_Orientation;

   ---------
   -- Map --
   ---------
   procedure Map (Point : in out Point_Type) is
      X_Comp : Integer_Type renames Get_Component (x, Point);
      Y_Comp : Integer_Type renames Get_Component (y, Point);
      Z_Comp : Integer_Type renames Get_Component (z, Point);

      Spare : constant Integer_Type := LCM * Z_Comp;
   begin
      Set_Components (
        X_Value => Spare - A_Mult * abs X_Comp + C_Mult * X_Comp + B_Mult * Y_Comp,
        Y_Value => LCM * X_Comp,
        Z_Value => Spare,
        Point   => Point
      );
   end Map;

   procedure Map (Polygon : in out Polygon_Type) is
      Point : Point_Type;
   begin
      for I in 1 .. Polygon.Number_Of_Vertices loop
         Point := Get_Vertex (
           Position => I,
           Polygon  => Polygon
         );

         Map (Point);

         Set_Vertex (
           Position => I,
           Value    => Point,
           Polygon  => Polygon
         );
      end loop;

      if not Orientation_Preserving then
         Reverse_Orientation (Polygon);
      end if;
   end Map;

   --------------------
   -- Set_Parameters --
   --------------------
   procedure Set_Parameters (
     A_Numerator   : Integer_Type;
     A_Denominator : Integer_Type;
     B_Numerator   : Integer_Type;
     B_Denominator : Integer_Type;
     C_Numerator   : Integer_Type;
     C_Denominator : Integer_Type
   ) is
      Spare : Integer_Type;
   begin
      if Is_Zero (A_Denominator) or Is_Zero (B_Denominator) or
        Is_Zero (C_Denominator) then
         --  It is possible to support some zero denominators, but we
         --  never needed it.
         raise Lozi_Error with "Zero denominators not supported";
      end if;

      --  Compute LCM = positive least common multiple of A_Denominator,
      --  B_Denominator and C_Denominator.
      Spare := GCD (A_Denominator, B_Denominator);
      LCM := (A_Denominator / Spare) * B_Denominator;
      Spare := GCD (LCM, C_Denominator);
      LCM := (LCM / Spare) * C_Denominator;
      LCM := abs LCM;

      A_Mult := A_Numerator * (LCM / A_Denominator);
      B_Mult := B_Numerator * (LCM / B_Denominator);
      C_Mult := C_Numerator * (LCM / C_Denominator);
      Orientation_Preserving := Is_Negative (B_Mult);
   end Set_Parameters;
end Lozi;
