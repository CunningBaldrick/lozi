--*********************************************************
--*********************
--                                                                            *
--*
--  File:        BINUSINU.ADB
--**
--  Description: Big_Number.Signed_Number package body
--**
--  Revision:    0.28 (beta version)
--**
--  Date:        July 8, 2001
--**
--  Author:      J‚r“me Delcourt
--**
--  Mail:        sikander@club-internet.fr
--**
--                                                                            *
--*
--  Copyright (c) J‚r“me Delcourt, 1998, 1999, 2000, 2001
--**
--  62, rue N‚grier
--**
--  59800 Lille
--**
--  FRANCE
--**
--                                                                            *
--*
--  Permission granted to use for any purpose, provided this copyright
--**
--  remains attached and unmodified.
--**
--                                                                            *
--*
--  THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
--**
--  IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
--**
--  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
--**
--                                                                            *
--*
--*****************************************************************************
--*

separate (Big_Number)
package body Signed_Number is

   --**************************************************************************
   --****
   --**************************************************************************
   --****
   --**                              Comparisons
   --**
   --**************************************************************************
   --****
   --**************************************************************************
   --****

   function "<" (Left, Right : in Big_Signed) return Boolean is
   begin
      if (Left.Positive /= Right.Positive) then
         return Boolean (Right.Positive);
      elsif (Left = Right) then
         return False;
      else
         -- Now, x and y have the same sign and are different.
         declare
            Indice : Index_Type := Left.Last_Significant_Digit;
         begin
            -- Left and Right have the same sign but their mantissas are
            --different.
            if (Indice /= Right.Last_Significant_Digit) then
               return ((Indice < Right.Last_Significant_Digit) =
                       Left.Positive);
            end if;
            while (Left.Mantissa (Indice) = Right.Mantissa (Indice))
              and then (Indice > First_Indice)
            loop
               Indice := Indice - 1;
            end loop;
            return ((Left.Mantissa (Indice) < Right.Mantissa (Indice)) =
                    Left.Positive);
         end;
      end if;
   end "<";

   --**************************************************************************
   --****

   function ">" (Left, Right : in Big_Signed) return Boolean is
   -- The comparison "Left > Right" is equivalent to "Right < Left"...
   begin
      return Right < Left;
   end ">";

   --**************************************************************************
   --****

   function "<=" (Left, Right : in Big_Signed) return Boolean is
   begin
      return not (Right < Left);
   end "<=";

   --**************************************************************************
   --****

   function ">=" (Left, Right : in Big_Signed) return Boolean is
   -- The comparison "Left >= Right" is the same test than "Right <= Left"...
   begin
      return Right <= Left;
   end ">=";

   --**************************************************************************
   --****

   function Is_Null (x : in Big_Signed) return Boolean is
   begin
      if (x.Last_Significant_Digit = First_Indice)
        and then (x.Mantissa (First_Indice) = 0)
      then
         return True;
      else
         return False;
      end if;
   end Is_Null;

   --**************************************************************************
   --****

   function Min (x, y : in Big_Signed) return Big_Signed is
   begin
      if (x < y) then
         return x;
      else
         return y;
      end if;
   end Min;

   --**************************************************************************
   --****

   function Max (x, y : in Big_Signed) return Big_Signed is
   begin
      if (x < y) then
         return y;
      else
         return x;
      end if;
   end Max;

   --**************************************************************************
   --****
   --**************************************************************************
   --****
   --**                            Binary operations
   --**
   --**************************************************************************
   --****
   --**************************************************************************
   --****

   procedure Swap1 (x, y : in out Big_Signed) is
      Temp : constant Big_Signed := x;
   begin
      x := y;
      y := Temp;
   end Swap1;

   --**************************************************************************
   --****

   procedure Swap2 (x, y : in out Big_Signed) is
      Max_Index     : constant Index_Type :=
         Max (x.Last_Significant_Digit, y.Last_Significant_Digit);
      Temp          : My_Type;
      Temp_Positive : constant Boolean    := x.Positive;
   begin
      x.Positive := y.Positive;
      y.Positive := Temp_Positive;
      for I in  First_Indice .. Max_Index loop
         Temp           := x.Mantissa (I);
         x.Mantissa (I) := y.Mantissa (I);
         y.Mantissa (I) := Temp;
      end loop;
   end Swap2;

   --**************************************************************************
   --****

   procedure Inc (x : in out Big_Signed) is
      Temp_x : Big_Unsigned := Signed2Unsigned (x);
   begin
      if (x.Last_Significant_Digit = First_Indice)
        and then (x.Mantissa (First_Indice) = 0)
      then
         -- If x = 0...
         x := Big_Signed_One;
         return;
      elsif (x.Positive = True) then
         -- x > 0
         Inc (Temp_x);
         x := Unsigned2Signed (Temp_x, x.Positive);
      else
         -- x < 0
         if (x.Last_Significant_Digit = First_Indice)
           and then (x.Mantissa (First_Indice) = 1)
         then
            x := Big_Signed_Zero;
         else
            Dec (Temp_x);
            x := Unsigned2Signed (Temp_x, x.Positive);
         end if;
      end if;
   end Inc;

   --**************************************************************************
   --****

   procedure Dec (x : in out Big_Signed) is
      Temp_x : Big_Unsigned := Signed2Unsigned (x);
   begin
      if (x.Last_Significant_Digit = First_Indice)
        and then (x.Mantissa (First_Indice) = 0)
      then
         -- If x = 0...
         x          := Big_Signed_One;
         x.Positive := False;
      elsif (x.Positive = True) then
         -- x > 0
         Dec (Temp_x);
         x := Unsigned2Signed (Temp_x, x.Positive);
      else
         -- x < 0
         Inc (Temp_x);
         x := Unsigned2Signed (Temp_x, x.Positive);
      end if;
   end Dec;

   --**************************************************************************
   --****

   function "+" (x, y : in Big_Signed) return Big_Signed is
      Tmp_x : Big_Signed := x;
      Tmp_y : Big_Signed := y;
   begin
      if (Tmp_x.Positive /= Tmp_y.Positive) then
         if (Tmp_x.Positive = False) then
            Swap1 (Tmp_x, Tmp_y);
         end if;
         Tmp_y.Positive := True;
         return Tmp_x - Tmp_y;
      end if;

      -- Now, the function studies the case where x and y have the same sign.
      declare
         Temp_x : constant Big_Unsigned := Signed2Unsigned (Tmp_x);
         Temp_y : constant Big_Unsigned := Signed2Unsigned (Tmp_y);
         Result : constant Big_Signed   :=
            Unsigned2Signed (Temp_x + Temp_y, Tmp_x.Positive);
      begin
         return Result;
      end;
   end "+";

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   function "-" (x, y : in Big_Signed) return Big_Signed is
   begin
      if (x.Positive /= y.Positive) then
         return x + Opposite (y);
      else              -- (x.Positive = y.Positive)
         declare
            Temp_x : constant Big_Unsigned := Signed2Unsigned (x);
            Temp_y : constant Big_Unsigned := Signed2Unsigned (y);
            Result : Big_Signed;
         begin
            if (Absolute (x) >= Absolute (y)) then
               Result := Unsigned2Signed (Temp_x - Temp_y, x.Positive);
            else
               Result := Unsigned2Signed (Temp_y - Temp_x, not x.Positive);
            end if;
            return Result;
         end;
      end if;
   end "-";

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   function "*" (x, y : in Big_Signed) return Big_Signed is
      Temp_x : constant Big_Unsigned := Signed2Unsigned (x);
      Temp_y : constant Big_Unsigned := Signed2Unsigned (y);
      Result : constant Big_Signed   :=
         Unsigned2Signed (Temp_x * Temp_y, (x.Positive = y.Positive));
   begin
      return Result;
   end "*";

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   function "**" (x : in Big_Signed; y : in My_Type) return Big_Signed is
      Temp_Result : Big_Signed := x;
   begin
      if (y = 0) then
         return Big_Signed_One;
      end if;
      if (x.Last_Significant_Digit = First_Indice)
        and then (x.Mantissa (First_Indice) <= 1)
      then
         if (Temp_Result.Positive = False) then
            -- In this case, the result is positive if y MOD 2 = 0, and
            -- negative if y MOD 2 = 0.
            Temp_Result.Positive := (y mod 2) = 0;
         end if;
         return Temp_Result;
      end if;
      for I in  2 .. y loop
         Temp_Result := Temp_Result * x;
      end loop;
      return Temp_Result;
   end "**";

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   function "**" (x : in Big_Signed; y : in Index_Type) return Big_Signed is
      Temp_Result : Big_Signed := x;
   begin
      if (y = 0) then
         return Big_Signed_One;
      end if;
      if (x.Last_Significant_Digit = First_Indice)
        and then (x.Mantissa (First_Indice) <= 1)
      then
         if (Temp_Result.Positive = False) then
            -- In this case, the result is positive if y MOD 2 = 0, and
            -- negative if y MOD 2 = 0.
            Temp_Result.Positive := (y mod 2) = 0;
         end if;
         return Temp_Result;
      end if;
      for I in  2 .. y loop
         Temp_Result := Temp_Result * x;
      end loop;
      return Temp_Result;
   end "**";

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   function "**" (x : in Big_Signed; y : in Big_Unsigned) return Big_Signed is
      Temp_Result : Big_Signed   := x;
      Indice      : Big_Unsigned := Big_Unsigned_One;
   begin
      if (y.Last_Significant_Digit = First_Indice)
        and then (y.Mantissa (First_Indice) = 0)
      then
         -- These two tests are equivalent to the test "y = Big_Unsigned_Zero",
         -- but it's much faster that way.
         return Big_Signed_One;
      end if;
      if (x.Last_Significant_Digit = First_Indice)
        and then (x.Mantissa (First_Indice) <= 1)
      then
         if (Temp_Result.Positive = False) then
            -- In this case, the result is positive if y MOD 2 = 0,
            -- and negative if y MOD 2 = 0.
            -- We have just to check if the lowest digit is a multiple or not
            --of 2.
            Temp_Result.Positive := (y.Mantissa (First_Indice) mod 2) = 0;
         end if;
         return Temp_Result;
      end if;
      while (Indice < y) loop
         Inc (Indice);
         Temp_Result := Temp_Result * x;
      end loop;
      return Temp_Result;
   end "**";

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   function "**" (x, y : in Big_Signed) return Big_Signed is
      Temp_Result : Big_Signed            := x;
      Indice      : Big_Signed            := Big_Signed_One;
      Temp_y      : constant Big_Unsigned := Signed2Unsigned (y);
   begin
      if (y.Positive = False) then
         return Big_Signed_Zero;
      end if;
      return (x ** Temp_y);
   end "**";

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   procedure Big_Div
     (x, y            : in Big_Signed;
      Quot, Remainder : out Big_Signed)
   is
      Temp_x                    : constant Big_Unsigned :=
         Signed2Unsigned (x);
      Temp_y                    : constant Big_Unsigned :=
         Signed2Unsigned (y);
      Temp_Quot, Temp_Remainder : Big_Unsigned;
      Result_Positive           : constant Boolean      :=
         (x.Positive = y.Positive);
   begin
      Big_Div (Temp_x, Temp_y, Temp_Quot, Temp_Remainder);
      Quot      := Unsigned2Signed (Temp_Quot, Result_Positive);
      Remainder := Unsigned2Signed (Temp_Remainder, Result_Positive);
   end Big_Div;

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   procedure Short_Div
     (x         : in Big_Signed;
      y         : in My_Type;
      Quot      : out Big_Signed;
      Remainder : out My_Type)
   is
      Temp_x    : constant Big_Unsigned := Signed2Unsigned (x);
      Temp_Quot : Big_Unsigned;
   begin
      Short_Div (Temp_x, y, Temp_Quot, Remainder);
      Quot := Unsigned2Signed (Temp_Quot, x.Positive);
   end Short_Div;

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   function "/" (x, y : in Big_Signed) return Big_Signed is
      Quot, Remainder : Big_Signed;
   begin
      Big_Div (x, y, Quot, Remainder);
      return Quot;
   end "/";

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   function "mod" (x, y : in Big_Signed) return Big_Signed is
      Quot, Remainder : Big_Signed;
   begin
      Big_Div (x, y, Quot, Remainder);
      if (x > Big_Signed_Zero) then
         return Absolute (Remainder);
      else
         return Absolute (Absolute (y) - Absolute (Remainder));
      end if;
   end "mod";

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   function "/" (x : in Big_Signed; y : in My_Type) return Big_Signed is
      Quot      : Big_Signed;
      Remainder : My_Type;
   begin
      Short_Div (x, y, Quot, Remainder);
      return Quot;
   end "/";

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   function "mod" (x : in Big_Signed; y : in My_Type) return My_Type is
      Quot      : Big_Signed;
      Remainder : My_Type;
   begin
      Short_Div (x, y, Quot, Remainder);
      if (x > Big_Signed_Zero) then
         return Remainder;
      else
         return (y - Remainder);
      end if;
      return Remainder;
   end "mod";

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   function Opposite (x : in Big_Signed) return Big_Signed is
      Temp_Result : Big_Signed := x;
   begin
      if (Temp_Result.Last_Significant_Digit /= First_Indice)
        or else (Temp_Result.Mantissa (First_Indice) /= 0)
      then
         -- If Temp_Result /= 0, then inverse the sign. Note that if x = 0, by
         -- convention, the sign of x is always set to positive.
         Temp_Result.Positive := not Temp_Result.Positive;
      end if;
      return Temp_Result;
   end Opposite;

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   function Absolute (x : in Big_Signed) return Big_Signed is
   begin
      if (x.Positive = False) then
         return Opposite (x);
      else
         return x;
      end if;
   end Absolute;

   --**************************************************************************
   --****
   --**************************************************************************
   --****
   --**                           Special functions
   --**
   --**************************************************************************
   --****
   --**************************************************************************
   --****

   function Number_of_Bytes (x : in Big_Signed) return Largest_Unsigned is
   begin
      return Number_of_Bytes (Signed2Unsigned (x));
   end Number_of_Bytes;

   --**************************************************************************
   --****

   function Number_of_Bits (x : in Big_Signed) return Largest_Unsigned is
   begin
      return Number_of_Bits (Signed2Unsigned (x));
   end Number_of_Bits;

   --**************************************************************************
   --****

   function Square_Root (x : in Big_Signed) return Big_Signed is
      Result, Temp_Result : Big_Signed;
      Two                 : constant Big_Signed :=
        (Positive               => True,
         Last_Significant_Digit => First_Indice,
         Mantissa               => (2, others => 0));
      -- Remark :
      --   In order to avoid the use of a fifth variable, Diff is initialized
      --   3.
      Diff : Big_Signed :=
        (Positive               => True,
         Last_Significant_Digit => First_Indice,
         Mantissa               => (3, others => 0));
   begin
      if (x < Big_Signed_Zero) then
         raise Parameter_Error;
      elsif (x.Last_Significant_Digit = First_Indice)
        and then (x.Mantissa (First_Indice) = 0)
      then
         return Big_Signed_Zero;
      elsif (x <= Diff) then  -- On a provisoirement : Diff = 3.
         return Big_Signed_One;
      end if;

      -- Loop to calculate the square root
      Result := x / Two;
      while (Diff > Big_Signed_One) loop
         Temp_Result := Result;
         Result      := (Temp_Result + x / Temp_Result) / Two;
         if (Temp_Result > Result) then
            Diff := Temp_Result - Result;
         else
            Diff := Result - Temp_Result;
         end if;
      end loop;
      return Result;
   end Square_Root;

   --**************************************************************************
   --****

   function Factorial (x : in Big_Signed) return Big_Signed is
      Temp_x : constant Big_Unsigned := Signed2Unsigned (x);
   begin
      if (x.Positive = False) then
         raise Parameter_Error;
      end if;
      return Unsigned2Signed (Factorial (Temp_x), True);
   end Factorial;

   --**************************************************************************
   --****

   function GCF (x, y : in Big_Signed) return Big_Signed is
      -- In english : GCF = greatest common factor
      Temp_x : constant Big_Unsigned := Signed2Unsigned (x);
      Temp_y : constant Big_Unsigned := Signed2Unsigned (y);
   begin
      return Unsigned2Signed (GCF (Temp_x, Temp_y), True);
   end GCF;

   --**************************************************************************
   --****

   function LCM (x, y : in Big_Signed) return Big_Signed is
      -- finds the Least Common Multiple between 2 arguments : LCM
      Tmp : Big_Signed := (x / GCF (x, y)) * y;
   begin
      if (Tmp.Positive = False) then
         Tmp.Positive := True;
      end if;
      return Tmp;
   end LCM;

   --**************************************************************************
   --****

end Signed_Number;
