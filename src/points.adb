package body Points is
   use Integers;

   -------------
   -- Is_Zero --
   -------------
   function Is_Zero (Right : Point_Type) return Boolean is
   begin
      return Is_Zero (Right (x)) and then Is_Zero (Right (y)) and then
        Is_Zero (Right (z));
   end Is_Zero;

   ----------------
   -- Equivalent --
   ----------------
   function Equivalent (Left, Right : Point_Type) return Boolean is
      Non_Zero : array (1 .. 3) of Coordinate_Type;
      Number_Non_Zero : Natural := 0;
   begin
      for I in Coordinate_Type loop
         declare
            Left_Is_Zero  : constant Boolean := Is_Zero (Left (I));
            Right_Is_Zero : constant Boolean := Is_Zero (Right (I));
         begin
            if Left_Is_Zero /= Right_Is_Zero then
               return False;
            end if;

            if not Left_Is_Zero then
               Number_Non_Zero := Number_Non_Zero + 1;
               Non_Zero (Number_Non_Zero) := I;
            end if;
         end;
      end loop;

      for I in 1 .. Number_Non_Zero loop
         declare
            Coordinate : constant Coordinate_Type := Non_Zero (I);
         begin
            if Is_Positive (Left (Coordinate)) /=
              Is_Positive (Right (Coordinate)) then
               return False;
            end if;
         end;
      end loop;

      if Number_Non_Zero <= 1 then
         return True;
      elsif Number_Non_Zero = 2 then
         declare
            Coordinate_1 : constant Coordinate_Type := Non_Zero (1);
            Coordinate_2 : constant Coordinate_Type := Non_Zero (2);
         begin
            return Right (Coordinate_1) * Left (Coordinate_2) =
              Right (Coordinate_2) * Left (Coordinate_1);
         end;
      else -- Number_Non_Zero = 3
         pragma Assert (Number_Non_Zero = 3);
         return (Right (x) * Left (y) = Right (y) * Left (x)) and then
           (Right (x) * Left (z) = Right (z) * Left (x));
      end if;
   end Equivalent;

   ---------
   -- "+" --
   ---------
   function "+" (Left, Right : Point_Type) return Point_Type is
      Result : Point_Type;
   begin
      Result (x) := Left (x) + Right (x);
      Result (y) := Left (y) + Right (y);
      Result (z) := Left (z) + Right (z);

      return Result;
   end "+";

   ---------
   -- "-" --
   ---------
   function "-" (Left, Right : Point_Type) return Point_Type is
      Result : Point_Type;
   begin
      Result (x) := Left (x) - Right (x);
      Result (y) := Left (y) - Right (y);
      Result (z) := Left (z) - Right (z);

      return Result;
   end "-";

   ------------
   -- Create --
   ------------
   function Create (
     X_Value : Integers.Integer_Type;
     Y_Value : Integers.Integer_Type;
     Z_Value : Integers.Integer_Type
   ) return Point_Type is
      Result : Point_Type;
   begin
      Set_Components (X_Value, Y_Value, Z_Value, Result);

      return Result;
   end Create;

   -------------------
   -- Cross_Product --
   -------------------
   function Cross_Product (Left, Right : Point_Type) return Point_Type is
      Result : Point_Type;
   begin
      Result (x) := Left (y) * Right (z) - Left (z) * Right (y);
      Result (y) := Left (z) * Right (x) - Left (x) * Right (z);
      Result (z) := Left (x) * Right (y) - Left (y) * Right (x);

      return Result;
   end Cross_Product;

   -----------------
   -- Dot_Product --
   -----------------
   function Dot_Product (Left, Right : Point_Type)
     return Integers.Integer_Type is
      Zeros : array (Coordinate_Type) of Boolean;
      All_Zero : Boolean := True;
      First_Non_Zero : Coordinate_Type := z;
   begin
      for I in Coordinate_Type loop
         Zeros (I) := Is_Zero (Left (I)) or else Is_Zero (Right (I));
         if All_Zero and not Zeros (I) then
            All_Zero := False;
            First_Non_Zero := I;
         end if;
      end loop;

      if All_Zero then
         return Zero;
      end if;

      declare
         Result : Integer_Type :=
           Left (First_Non_Zero) * Right (First_Non_Zero);
      begin
         if First_Non_Zero < z then
            for I in Coordinate_Type'Succ (First_Non_Zero) .. z loop
               if not Zeros (I) then
                  Result := Result + Left (I) * Right (I);
               end if;
            end loop;
         end if;

         return Result;
      end;
   end Dot_Product;

   -------------------
   -- Get_Component --
   -------------------
   function Get_Component (
     Coordinate : Coordinate_Type;
     Point      : Point_Type
   ) return Integers.Integer_Type is
   begin
      return Point (Coordinate);
   end Get_Component;

   ------------------------
   -- Linear_Combination --
   ------------------------
   function Linear_Combination
     (Multiplier1 : Integers.Integer_Type;
      Point1      : Point_Type;
      Multiplier2 : Integers.Integer_Type;
      Point2      : Point_Type)
      return Point_Type
   is
      Result : Point_Type;
   begin
      Result (x) := Multiplier1 * Point1 (x) + Multiplier2 * Point2 (x);
      Result (y) := Multiplier1 * Point1 (y) + Multiplier2 * Point2 (y);
      Result (z) := Multiplier1 * Point1 (z) + Multiplier2 * Point2 (z);

      return Result;
   end Linear_Combination;

   ------------
   -- Negate --
   ------------
   function Negate (Right : Point_Type) return Point_Type is
   begin
      return (Negate (Right (x)), Negate (Right (y)), Negate (Right (z)));
   end Negate;

   ------------
   -- Reduce --
   ------------
   procedure Reduce (Point : in out Point_Type) is
      Greatest_Common_Divisor : Integer_Type;
   begin
      if Is_Zero (Point) then
         return;
      end if;

      if Is_Zero (Point (x)) then
         Greatest_Common_Divisor := abs GCD (Point (y), Point (z));
      else
         Greatest_Common_Divisor := GCD (Point (x), Point (y));
         Greatest_Common_Divisor :=
           abs GCD (Point (z), Greatest_Common_Divisor);
      end if;

      Point (x) := Point (x) / Greatest_Common_Divisor;
      Point (y) := Point (y) / Greatest_Common_Divisor;
      Point (z) := Point (z) / Greatest_Common_Divisor;
   end Reduce;

   --------------------
   -- Set_Components --
   --------------------
   procedure Set_Components (
     X_Value : in     Integers.Integer_Type;
     Y_Value : in     Integers.Integer_Type;
     Z_Value : in     Integers.Integer_Type;
     Point   : in out Point_Type
   )
   is
   begin
      Point (x) := X_Value;
      Point (y) := Y_Value;
      Point (z) := Z_Value;
   end Set_Components;
end Points;
