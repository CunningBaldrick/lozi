package body Lozi is

   A_Numerator : Integer_Type;
   B_Numerator : Integer_Type;
   Common_Denominator : Integer_Type;
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
      CDZ : Integer_Type renames "*" (Common_Denominator, Z_Comp);
   begin
      Set_Components (
        X_Value => CDZ - A_Numerator * abs X_Comp + B_Numerator * Y_Comp,
        Y_Value => Common_Denominator * X_Comp,
        Z_Value => CDZ,
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
     B_Denominator : Integer_Type
   ) is
      A_Num : Integer_Type;
      A_Den : Integer_Type;
      B_Num : Integer_Type;
      B_Den : Integer_Type;
      Spare : Integer_Type;
   begin
      if Is_Zero (A_Numerator) and Is_Zero (A_Denominator) then
         raise Lozi_Error;
      end if;

      Spare := GCD (A_Numerator, A_Denominator);
      A_Num := A_Numerator / Spare;
      A_Den := A_Denominator / Spare;

      if Is_Negative (A_Den) then
         A_Den := Negate (A_Den);
         A_Num := Negate (A_Num);
      end if;

      if Is_Zero (B_Numerator) and Is_Zero (B_Denominator) then
         raise Lozi_Error;
      end if;

      Spare := GCD (B_Numerator, B_Denominator);
      B_Num := B_Numerator / Spare;
      B_Den := B_Denominator / Spare;

      if Is_Negative (B_Den) then
         B_Den := Negate (B_Den);
         B_Num := Negate (B_Num);
      end if;

      if Is_Zero (A_Denominator) and Is_Zero (B_Denominator) then
         raise Lozi_Error;
      end if;

      Spare := abs GCD (A_Den, B_Den);
      Lozi.Common_Denominator := (A_Den / Spare) * B_Den;
      Lozi.A_Numerator := A_Num * (B_Den / Spare);
      Lozi.B_Numerator := B_Num * (A_Den / Spare);
      Lozi.Orientation_Preserving := Is_Negative (Lozi.B_Numerator);
   end Set_Parameters;
end Lozi;
