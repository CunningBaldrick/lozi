--***********************************************************
--*******************
--                                                                            *
--*
--  File:        BINUBIFR.ADB
--**
--  Description: Big_Number.Big_Fraction package body
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
package body Big_Fraction is

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   function Simplify_Fraction (x : in Fraction) return Fraction is
   -- Here, we don't have to test for a denominator equal to zero, because it
   -- would have been already checked in the functions CreateFraction or
   -- InverseFraction.
   begin
      if (x.Denominator.Last_Significant_Digit = First_Indice)
        and then (x.Denominator.Mantissa (First_Indice) = 1)
      then
         -- If (x.Denominator = Big_Unsigned_One)...
         return x;
      end if;
      if (x.Numerator.Last_Significant_Digit = First_Indice) then
         case x.Numerator.Mantissa (First_Indice) is
            when 0 => -- If (x.Numerator = Big_Unsigned_Zero)...
               return (True, Big_Unsigned_Zero, Big_Unsigned_One);
            when 1 => -- If (x.Numerator = Big_Unsigned_One)...
               return x;
            when others =>
               null;
         end case;
      end if;
      declare
         Temp : constant Big_Unsigned := GCF (x.Numerator, x.Denominator);
      begin
         return (x.Positive, x.Numerator / Temp, x.Denominator / Temp);
      end;
   end Simplify_Fraction;

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   function CreateFraction
     (Numerator, Denominator : in Big_Unsigned;
      Positive               : in Boolean := True)
      return                   Fraction
   is
   begin
      if (Denominator.Last_Significant_Digit = First_Indice)
        and then (Denominator.Mantissa (First_Indice) = 0)
      then
         -- These two tests are equivalent to the test Denominator =
         -- Big_Unsigned_Zero but it's much faster that way.
         raise Division_By_Zero;
      end if;
      return Simplify_Fraction ((Positive, Numerator, Denominator));
   end CreateFraction;

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   function CreateFraction
     (Numerator, Denominator : in Big_Signed)
      return                   Fraction
   is
   begin
      if (Denominator.Last_Significant_Digit = First_Indice)
        and then (Denominator.Mantissa (First_Indice) = 0)
      then
         -- These two tests are equivalent to the test Denominator =
         -- Big_Signed_Zero but it's much faster that way.
         raise Division_By_Zero;
      end if;
      return Simplify_Fraction
               (
        (Numerator.Positive = Denominator.Positive,
         Signed2Unsigned (Numerator),
         Signed2Unsigned (Denominator)));
   end CreateFraction;

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   function Get_Numerator (x : in Fraction) return Big_Unsigned is
   begin
      return x.Numerator;
   end Get_Numerator;

   pragma Inline (Get_Numerator);

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   function Get_Denominator (x : in Fraction) return Big_Unsigned is
   begin
      return x.Denominator;
   end Get_Denominator;

   pragma Inline (Get_Denominator);

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   function Is_Positive (x : in Fraction) return Boolean is
   begin
      return x.Positive;
   end Is_Positive;

   pragma Inline (Is_Positive);

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   procedure Swap_Fractions (x, y : in out Fraction) is
      Temp : constant Fraction := x;
   begin
      x := y;
      y := Temp;
   end Swap_Fractions;

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   function Inverse_Fraction (x : in Fraction) return Fraction is
   begin
      -- This function returns 1/x.
      -- If the numerator of x is equal to 0, this function must raise the
      -- exception Division_By_Zero.
      if (x.Numerator.Last_Significant_Digit = First_Indice)
        and then (x.Numerator.Mantissa (First_Indice) = 0)
      then
         -- If (x.Numerator = Big_Unsigned_Zero) THEN... It's much faster that
         -- way.
         raise Division_By_Zero;
      else
         return (x.Positive, x.Denominator, x.Numerator);
      end if;
   end Inverse_Fraction;

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   function "*" (x, y : in Fraction) return Fraction is
      Temp1 : constant Big_Unsigned := GCF (x.Numerator, y.Denominator);
      Temp2 : constant Big_Unsigned := GCF (y.Numerator, x.Denominator);
   begin
      return
        (x.Positive = y.Positive,
         (x.Numerator / Temp1) * (y.Numerator / Temp2),
         (x.Denominator / Temp2) * (y.Denominator / Temp1));
   exception
      when others =>
         raise Fraction_Overflow;
   end "*";

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   function "/" (x, y : in Fraction) return Fraction is
   begin
      -- The function Inverse_Fraction can raise the exception
      --"DIVISION_BY_ZERO"
      -- if y.Numerator = Big_Unsigned_Zero.
      return (x * Inverse_Fraction (y));
   end "/";

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   -- a/b + c/d = a/(x*b') + c/(x*d'),
   -- with b = x*b' and d = x*d' and x = GCF(b,d)
   -- So a/b + c/d = (a/b'+ c/d') * 1/x
   -- a/b + c/d = (a*d' + c*b') / (x * b'* d')
   -- x*b' = b
   -- So a/b + c/d = (a*d' + c*b') / (b*d')
   --
   -- The fractions a/b and c/d are supposed to be already simplified, so : -
   -- GCF(a, b) = 1 ==> GCF(a*d', b*d') = d'
   --      - GCF(c, d) = 1 ==> GCF(c, d') = 1
   --      - GCF(b', b) = b' ==> GCF(c*b', b*d') = b'
   --
   -- Furthermore, GCF(b', d') = 1 So, GCF(a*d' + c*b', b*d') = 1

   function "+" (x, y : in Fraction) return Fraction is
      Result  : Fraction;
      TempGCF : constant Big_Unsigned := GCF (x.Denominator, y.Denominator);
      Temp    : constant Big_Unsigned := y.Denominator / TempGCF;
      Tmp1    : Big_Unsigned          := x.Numerator * Temp;
      Tmp2    : constant Big_Unsigned :=
         y.Numerator * (x.Denominator / TempGCF);
   begin
      if (x.Positive = y.Positive) then
         -- The 2 fractions have the same sign.
         Result.Positive  := x.Positive;
         Result.Numerator := Tmp1 + Tmp2;
      else
         -- The 2 fractions have opposite signs.
         if (Tmp1 < Tmp2) then
            Result.Positive  := y.Positive;
            Result.Numerator := Tmp2 - Tmp1;
         else   -- Tmp1 >= Tmp2
            Result.Positive  := x.Positive;
            Result.Numerator := Tmp1 - Tmp2;
         end if;
      end if;
      if (Result.Numerator.Last_Significant_Digit = First_Indice) then
         if (Result.Numerator.Mantissa (First_Indice) = 0) then
            -- IF (Result.Numerator = Big_Unsigned_Zero) THEN...
            Result.Positive    := True;
            Result.Denominator := Big_Unsigned_One;
         else
            Result.Denominator := x.Denominator * Temp;
            if (Result.Numerator.Mantissa (First_Indice) > 1) then
               Tmp1               :=
                  GCF (Result.Numerator, Result.Denominator);
               Result.Numerator   := Result.Numerator / Tmp1;
               Result.Denominator := Result.Denominator / Tmp1;
            end if;
         end if;
         return Result;
      end if;
      -- The three following lines allow to avoid some overflows.
      Tmp1               := GCF (Result.Numerator, x.Denominator);
      Result.Numerator   := Result.Numerator / Tmp1;
      Result.Denominator := (x.Denominator / Tmp1) * Temp;
      return Result;
   exception
      when others =>
         raise Fraction_Overflow;
   end "+";

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   function "-" (x, y : in Fraction) return Fraction is
      Temp_y : Fraction := y;
   -- Just inverse the sign of y (Temp_y := -y) and then -y is added to x. So
   -- we have not to rewrite a new algorithm for this function.
   begin
      Temp_y.Positive := not (Temp_y.Positive);
      return (x + Temp_y);
   end "-";

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   procedure PUT (x : in Fraction) is
   begin
      if (x.Positive = False) then
         Put ('-');
      end if;
      PUT (x.Numerator);
      if (x.Denominator.Last_Significant_Digit /= First_Indice)
        or else (x.Denominator.Mantissa (First_Indice) /= 1)
      then
         -- IF (x.Denominator /= Big_Unsigned_One) THEN...
         Put ('/');
         PUT (x.Denominator);
      end if;
   end PUT;

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   procedure PUT_LINE (x : in Fraction) is
   begin
      PUT (x);
      New_Line;
   end PUT_LINE;

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   procedure PUT (File : in File_Type; x : in Fraction) is
   begin
      if (x.Positive = False) then
         Put (File, '-');
      end if;
      PUT (File, x.Numerator);
      if (x.Denominator.Last_Significant_Digit /= First_Indice)
        or else (x.Denominator.Mantissa (First_Indice) /= 1)
      then
         -- IF (x.Denominator /= Big_Unsigned_One) THEN...
         Put (File, '/');
         PUT (File, x.Denominator);
      end if;
   end PUT;

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   procedure PUT_LINE (File : in File_Type; x : in Fraction) is
   begin
      PUT (File, x);
      New_Line (File);
   end PUT_LINE;

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   function UString2Fraction (x : in Unbounded_String) return Fraction is
      Numerator, Denominator : Unbounded_String;
      Result                 : Fraction;
      Index                  : Natural;
   begin
      Index := Char_Count (x, '/');
      if (Index = 0) then
         Result := CreateFraction (UString2Signed (x), Big_Signed_One);
         return Result;
      elsif (Index = 1) then
         Index       := First_Matching_Char_Position (x, '/');
         Numerator   := Copy (x, 1, Index - 1);
         Denominator := Copy (x, Index + 1, Length (x) - Index);
         if (Length (Numerator) = 0)
           or else (Length (Denominator) = 0)
         then
            raise Parameter_Error;
         end if;
         Result :=
            CreateFraction
              (UString2Signed (Numerator),
               UString2Signed (Denominator));
         return Result;
      else
         raise Parameter_Error;
      end if;
   exception
      when others =>
         raise Parameter_Error;
   end UString2Fraction;

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   procedure GET_LINE (x : out Fraction) is
      Temp_String : Unbounded_String;
   begin
      Get_Line (Temp_String);
      x := UString2Fraction (Temp_String);
   end GET_LINE;

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   procedure GET_LINE (File : in File_Type; x : out Fraction) is
      Temp_String : Unbounded_String;
   begin
      Get_Line (File, Temp_String);
      x := UString2Fraction (Temp_String);
   end GET_LINE;

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   function "**" (x : in Fraction; y : in Big_Unsigned) return Fraction is
   begin
      if (y.Last_Significant_Digit = First_Indice) then
         if (y.Mantissa (First_Indice) = 0) then
            -- IF (y = Big_Unsigned_Zero) THEN ...
            return (True, Big_Unsigned_One, Big_Unsigned_One);       -- Returns
                                                                     -- 1/1.
         elsif (y.Mantissa (First_Indice) = 1) then
            return x;
         end if;
      end if;
      declare
         Temp_Result : Fraction;
      begin
         Temp_Result.Numerator   := x.Numerator ** y;
         Temp_Result.Denominator := x.Denominator ** y;
         Temp_Result.Positive    := x.Positive
                                   or else (y.Mantissa (First_Indice) mod 2 =
                                            0);
         -- Note : if x is positive, the result is always positive.
         -- If x is negative, the result is positive if the exponent is a
         --multiple
         -- of 2.
         return Temp_Result;
      end;
   exception
      when others =>
         raise Fraction_Overflow;
   end "**";

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   function "**" (x : in Fraction; y : in Big_Signed) return Fraction is
      Result : Fraction;
   begin
      Result := x ** Signed2Unsigned (y);
      if (y.Positive = False) then
         Result := Inverse_Fraction (Result);
      end if;
      return Result;
   end "**";

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   function "**" (x : in Fraction; y : in My_Type) return Fraction is
   begin
      if (y = 0) then
         return (True, Big_Unsigned_One, Big_Unsigned_One);       -- Returns
                                                                  --1/1.
      elsif (y = 1) then
         return x;
      end if;
      declare
         Temp_Result : Fraction;
      begin
         Temp_Result.Numerator   := x.Numerator ** y;
         Temp_Result.Denominator := x.Denominator ** y;
         Temp_Result.Positive    := x.Positive or else (y mod 2 = 0);
         -- Note : if x is positive, the result is always positive.
         -- If x is negative, the result is positive if the exponent is a
         --multiple
         -- of 2.
         return Temp_Result;
      end;
   exception
      when others =>
         raise Fraction_Overflow;
   end "**";

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   function "**" (x : in Fraction; y : in Index_Type) return Fraction is
   begin
      if (y = 0) then
         return (True, Big_Unsigned_One, Big_Unsigned_One);       -- Returns
                                                                  --1/1.
      elsif (y = 1) then
         return x;
      end if;
      declare
         Temp_Result : Fraction;
      begin
         Temp_Result.Numerator   := x.Numerator ** y;
         Temp_Result.Denominator := x.Denominator ** y;
         Temp_Result.Positive    := x.Positive or else (y mod 2 = 0);
         -- Note : if x is positive, the result is always positive.
         -- If x is negative, the result is positive if the exponent is a
         --multiple
         -- of 2.
         return Temp_Result;
      end;
   exception
      when others =>
         raise Fraction_Overflow;
   end "**";

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   function "*" (x : in Fraction; y : in Big_Unsigned) return Fraction is
   begin
      if (y.Last_Significant_Digit = First_Indice) then
         if (y.Mantissa (First_Indice) = 0) then
            return (x.Positive, Big_Unsigned_Zero, Big_Unsigned_One);
         elsif (y.Mantissa (First_Indice) = 1) then
            return x;
         end if;
      end if;
      declare
         Temp : constant Big_Unsigned := GCF (y, x.Denominator);
      begin
         return (x.Positive, (y / Temp) * x.Numerator, x.Denominator / Temp);
      end;
   exception
      when others =>
         raise Fraction_Overflow;
   end "*";

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   function "*" (x : in Fraction; y : in Big_Signed) return Fraction is
      Result : Fraction;
      Temp_y : constant Big_Unsigned := Signed2Unsigned (y);
      Temp   : constant Big_Unsigned := GCF (Temp_y, x.Denominator);
   begin
      if (y.Last_Significant_Digit = First_Indice) then
         if (y.Mantissa (First_Indice) = 0) then
            return (x.Positive, Big_Unsigned_Zero, Big_Unsigned_One);
         elsif (y.Mantissa (First_Indice) = 1) then
            return ((x.Positive = y.Positive), x.Numerator, x.Denominator);
         end if;
      end if;
      Result.Positive    := (x.Positive = y.Positive);
      Result.Numerator   := x.Numerator * (Temp_y / Temp);
      Result.Denominator := x.Denominator / Temp;
      return Result;
   exception
      when others =>
         raise Fraction_Overflow;
   end "*";

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   function "/" (x : in Fraction; y : in Big_Unsigned) return Fraction is
      Temp : Big_Unsigned;
   begin
      if (y.Last_Significant_Digit = First_Indice)
        and then (y.Mantissa (First_Indice) = 0)
      then
         raise Division_By_Zero;
      end if;
      Temp := GCF (x.Numerator, y);
      return (x.Positive, x.Numerator / Temp, (y / Temp) * x.Denominator);
   exception
      when others =>
         raise Fraction_Overflow;
   end "/";

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   function "/" (x : in Fraction; y : in Big_Signed) return Fraction is
      Temp   : Big_Unsigned;
      Temp_y : constant Big_Unsigned := Signed2Unsigned (y);
   begin
      if (y.Last_Significant_Digit = First_Indice)
        and then (y.Mantissa (First_Indice) = 0)
      then
         raise Division_By_Zero;
      end if;
      Temp := GCF (x.Numerator, Temp_y);
      return
        (x.Positive = y.Positive,
         x.Numerator / Temp,
         (Temp_y / Temp) * x.Denominator);
   exception
      when others =>
         raise Fraction_Overflow;
   end "/";

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   function "<" (Left, Right : in Fraction) return Boolean is
   --
   -- Be careful : do not convert the fraction to a real type. If so, there
   --will
   -- be a truncation, so the result of this function could be mathematically
   -- false.
   --
   begin
      if (Left.Positive /= Right.Positive) then
         -- If the 2 fractions have different signs, the comparison is easy...
         return Right.Positive;
      --
      -- Now, we must study the case where the two fractions have the same
      --sign.
      --
      -- First, some simple cases ...
      elsif (Left.Denominator = Right.Denominator) then
         if (Left.Numerator = Right.Numerator) then
            return False;
         else
            return (Left.Positive = (Left.Numerator < Right.Numerator));
         end if;
      end if;
      -- The case (Left.Denominator = Right.Denominator) has already been
      -- studied.
      if (Left.Numerator <= Right.Numerator) then
         return not (Left.Positive = (Left.Denominator < Right.Denominator));
      elsif (Left.Numerator.Last_Significant_Digit = First_Indice)
        and then (Left.Numerator.Mantissa (First_Indice) = 0)
      then
         -- Remark : the situation where Left.Numerator = Right.Numerator has
         -- already been studied, so Right.Numerator /= Big_Unsigned_Zero.
         -- Furthermore, at this stade, the 2 fractions have the same sign.
         -- That means that Left < Right, so the result is always TRUE. Note :
         -- by default, a fraction with a numerator equal to zero has a sign
         -- set to Positive.
         return True;
      end if;
      --
      -- Now, when the two fractions have the same sign, we must study the
      -- general case (Left.Numerator /= Right.Numerator AND Left.Denominator
      -- /= Right.Denominator).
      --
      if (Left.Numerator < Left.Denominator)
        and then (Right.Numerator < Right.Denominator)
      then
         -- If Left < Right, then Inverse_Fraction(Right) <
         --Inverse_Fraction(Left).
         return (Inverse_Fraction (Right) < Inverse_Fraction (Left));
      end if;
      --
      -- Now, at least one of the 2 fractions has an absolute value greater
      --than 1.
      --
      declare
         Temp_x             : Fraction := Left;
         Temp_y             : Fraction := Right;
         Tempx, Tempy, Temp : Big_Unsigned;
      begin
         --
         -- If Tempx = Tempy, then we must study further the fractions Temp_x
         -- and Temp_y.
         --
         loop
            Big_Div
              (Temp_x.Numerator,
               Temp_x.Denominator,
               Tempx,
               Temp_x.Numerator);
            Big_Div
              (Temp_y.Numerator,
               Temp_y.Denominator,
               Tempy,
               Temp_y.Numerator);
            -- Don't forget that Tempx and Tempy are "only" big integers, which
            -- represent the truncated value of Left and Right, respectively.
            if (Tempx /= Tempy) then
               return (Temp_x.Positive = (Tempx < Tempy));
            end if;
            Temp_x := Simplify_Fraction (Temp_x);
            -- Now, Temp_x < 1
            Temp_y := Simplify_Fraction (Temp_y);
            -- Now, Temp_y < 1
            if (Temp_x.Numerator > Temp_y.Numerator) then
               Temp := Big_Unsigned_Last / Temp_x.Numerator;
            else
               -- Temp_x.Numerator <= Temp_y.Numerator
               Temp := Big_Unsigned_Last / Temp_y.Numerator;
            end if;
            -- Temp_x.Numerator <= Big_Unsigned_Last and Temp_y.Numerator <=
            -- Big_Unsigned_Last, so Temp >= Big_Unsigned_One.
            if (Temp.Last_Significant_Digit > First_Indice)
              or else (Temp.Mantissa (First_Indice) > 1)
            then
               -- These two tests are the same test than (Temp >
               --Big_Unsigned_One),
               -- but they are much faster.
               -- In this case, we can multiply Temp_x and Temp_y by Temp,
               -- without raising Big_Number_Overflow.
               -- Then, we can calculate Tempx and Tempy
               -- for a more precise comparison.
               Temp_x := Temp_x * Temp;
               Temp_y := Temp_y * Temp;
            else
               -- Here, Tempx = Tempy, and Temp = Big_Unsigned_One. If Left <
               -- Right, then Inverse_Fraction(Left) > Inverse_Fraction(Right).
               return (Inverse_Fraction (Right) < Inverse_Fraction (Left));
            end if;
         end loop;
      end;     -- END of DECLARE
   end "<";

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   function ">" (Left, Right : in Fraction) return Boolean is
   begin
      -- If you want to know the result of Left > Right, just return the result
      -- of Right < Left...
      return (Left < Right);
   end ">";

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   function "<=" (Left, Right : in Fraction) return Boolean is
      -- Left <= Right is the same than (NOT (Left > Right)), so it's the same
      -- than (NOT (Right < Left)).
      Temp_Result : constant Boolean := not (Right < Left);
   begin
      return Temp_Result;
   end "<=";

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   function ">=" (Left, Right : in Fraction) return Boolean is
      -- Left >= Right is the same than (NOT (Left < Right)).
      Temp_Result : constant Boolean := not (Left < Right);
   begin
      return Temp_Result;
   end ">=";

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   function Opposite (x : in Fraction) return Fraction is
      Temp_Result : Fraction := x;
   begin
      if (Temp_Result.Numerator.Last_Significant_Digit /= First_Indice)
        or else (Temp_Result.Numerator.Mantissa (First_Indice) /= 0)
      then
         -- If Temp_Result /= 0, then inverse the sign. Note that if x = 0, by
         -- convention, the sign of x is always set to positive.
         Temp_Result.Positive := not (Temp_Result.Positive);
      end if;
      return Temp_Result;
   end Opposite;

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   function Absolute (x : in Fraction) return Fraction is
   begin
      if (x.Positive = True) then
         return x;
      else
         return Opposite (x);
      end if;
   end Absolute;

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   function Is_Null (x : in Fraction) return Boolean is
      Temp_Result : Boolean;
   begin
      Temp_Result := (x.Numerator.Last_Significant_Digit = First_Indice)
                    and then (x.Numerator.Mantissa (First_Indice) = 0);
      return Temp_Result;
   end Is_Null;

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   procedure Float_in_UString2Fraction
     (x          : in Unbounded_String;
      Nb_Periods : in Positive;
      Result     : out Fraction;
      Error      : out Boolean)
   is

      function Convert is new Int_Number2Big_Unsigned (Natural);

      function Find_a_Period
        (s    : in Unbounded_String)
         return Unbounded_String
      is
         -- This function looks for a period in the number represented by the
         -- string s. If a period is found, it is returned in a string. If no
         -- period is found, the returned result is the string s.
         --
         -- Here, the period returned is not the smaller. For example, in the
         -- number "1345345345345", the returned period is "345345", even if
         -- the smaller period is "345".
         --
         s1, s2                    : Unbounded_String;
         Size_s                    : constant Positive := Length (s);
         Temp_Size, Index1, Index2 : Natural;
      begin
         if (Size_s < 2) then
            return s;
         else
            Temp_Size := Size_s / 2;
            -- Begin to look for the greatest period which could be found
            loop
               Index1    := Size_s - 2 * Temp_Size + 1;
               Index2    := Size_s - Temp_Size + 1;
               s1        := Copy (s, Index1, Temp_Size);
               s2        := Copy (s, Index2, Temp_Size);
               Temp_Size := Temp_Size - 1;
               exit when (s1 = s2) or else (Temp_Size = 0);
            end loop;

            if (s1 /= s2) then
               return s;
            else
               return s2;
            end if;
         end if;
      end Find_a_Period;

      -------------------------------------------------------------------------
      -------

      function Count_the_Number_of_Periods
        (s, period : in Unbounded_String)
         return      Natural
      is
      begin
         if (s = period) then
            return 1;
         end if;
         declare
            Index       : Natural;
            Number      : Natural          := 1;
            String_Size : constant Natural := Length (s);
            Period_Size : constant Natural := Length (period);
            Max_Number  : constant Natural := String_Size / Period_Size;
            Temp_String : Unbounded_String;
         begin
            loop
               exit when (Number > Max_Number);
               Index       := String_Size - Number * Period_Size + 1;
               Temp_String := Copy (s, Index, Period_Size);
               if (Temp_String = period) then
                  Number := Number + 1;
               else
                  exit;
               end if;
            end loop;
            return (Number - 1);
         end;
      end Count_the_Number_of_Periods;

      -------------------------------------------------------------------------
      -------

      function Find_the_Smallest_Period
        (s    : in Unbounded_String)
         return Unbounded_String
      is
         Max_Period, s1, s2, temp : Unbounded_String;
         Size_Max_Period          : Positive;
         Index1, Index2           : Natural;
         Temp_Size                : Natural := 1;
         Count                    : Natural;
      -- Be careful. Suppose that s = "1.852333852333852333". "852333" is the
      -- real period, not "3".
      begin
         Max_Period := Find_a_Period (s);
         if (s = Max_Period) then
            return s;
         else
            Size_Max_Period := Length (Max_Period);
            -- It is certain that Max_Period is a period. Try to find a smaller
            -- period (s2).
            loop
               Index1 := Size_Max_Period - 2 * Temp_Size + 1;
               Index2 := Size_Max_Period - Temp_Size + 1;
               s1     := Copy (Max_Period, Index1, Temp_Size);
               s2     := Copy (Max_Period, Index2, Temp_Size);
               if (s1 = s2) then
                  -- Verify that Max_Period is a repetition of s1.
                  Count := Count_the_Number_of_Periods (Max_Period, s1);
                  temp  := Null_Unbounded_String;
                  for I in  1 .. Count loop
                     temp := temp & s1;
                  end loop;
                  -- If Max_Period is a repetition of s1, Temp should be equal
                  --to Max_Period.
                  if (Max_Period = temp) then
                     return s1;
                  end if;
               end if;
               if (Temp_Size < Size_Max_Period / 2) then
                  -- There must be at least 2 small periods in Max_Period, so
                  -- the size of this small period is smaller or equal to
                  -- Size_Max_Period / 2.
                  Temp_Size := Temp_Size + 1;
               else
                  return Max_Period;
               end if;
            end loop;
         end if;
      end Find_the_Smallest_Period;

      -------------------------------------------------------------------------
      -------

      function Prepare_String
        (Str  : in Unbounded_String)
         return Unbounded_String
      is
         Temp_Result : Unbounded_String := Del_Spaces (Str);
      begin
         Temp_Result := Char_Replace (Temp_Result, 'e', 'E');
         Temp_Result := Char_Replace (Temp_Result, ',', '.');
         return Temp_Result;
      end Prepare_String;

      -------------------------------------------------------------------------
      -------

      function Numeric_String (Str : in Unbounded_String) return Boolean is
         I             : Natural                   := 1;
         Temp_Str      : constant Unbounded_String :=
            Del_Character (Str, '.');
         String_Length : constant Natural          := Length (Temp_Str);
      -- The number of '.' character has been checked in the procedure
      -- "Verify_String_And_Separe_Exponent_And_Mantissa", so we don't have to
      -- check it now (we are sure that there is 0 or 1 '.' in this string).
      begin
         loop
            exit when (I > String_Length);
            if (Element (Temp_Str, I) not  in '0' .. '9') then
               return False;
            end if;
            I := I + 1;
         end loop;
         return True;
      end Numeric_String;

      -------------------------------------------------------------------------
      -------

      function Trim_Leading_Zero
        (Str  : in Unbounded_String)
         return Unbounded_String
      is
         Temp_String : Unbounded_String := Null_Unbounded_String;
         I           : Natural          := 0;
         Length_Str  : Natural          := Length (Str);
         Char        : Character;
      begin
         if (Length_Str = 0) then
            return Temp_String;
         end if;
         loop
            I := I + 1;
            exit when (I > Length_Str);
            Char := Element (Str, I);
            exit when (Char /= '0');
         end loop;
         Temp_String := Copy (Str, I, Length_Str - I + 1);
         if (Element (Temp_String, 1) = '.') then
            Temp_String := '0' & Temp_String;
         end if;
         I := First_Matching_Char_Position (Str, '.');
         if (I /= 0) then
            Length_Str := Length (Temp_String);
            loop
               Char := Element (Str, Length_Str);
               if (Char = '0') or else (Char = '.') then
                  Length_Str  := Length_Str - 1;
                  Temp_String := Copy (Temp_String, 1, Length_Str);
               else
                  exit;
               end if;
            end loop;
         end if;
         return Temp_String;
      end Trim_Leading_Zero;

      -------------------------------------------------------------------------
      -------

      procedure Verify_String_And_Separe_Exponent_And_Mantissa
        (Str                        : in Unbounded_String;
         Exponent_Str, Mantissa_Str : out Unbounded_String;
         Positive, Valid            : out Boolean)
      is
         -- Verify if Str is a valid string for conversion in a fraction.
         Mantissa, Exponent : Unbounded_String := Null_Unbounded_String;
         Count_E            : constant Natural := Char_Count (Str, 'E');
         I                  : Natural;
         Temp_Valid         : Boolean;
      begin
         Mantissa_Str := Mantissa;
         Exponent_Str := Exponent;
         if (Count_E > 1) then
            Valid := False;
            return;
         end if;
         if (Count_E = 1) then
            I        := First_Matching_Char_Position (Str, 'E');
            Mantissa := Copy (Str, 1, I - 1);
            Exponent := Copy (Str, I + 1, Length (Str) - I);
         else
            Mantissa := Str;
         end if;
         Mantissa := Trim_Leading_Zero (Mantissa);
         Exponent := Trim_Leading_Zero (Exponent);
         -- You can't have more than one '.', or more than 1 sign (+ or -) in
         -- the mantissa.
         if (Char_Count (Mantissa, '.') > 1)
           or else (Char_Count (Mantissa, '+') + Char_Count (Mantissa, '-') >
                    1)
         then
            Valid := False;
            return;
         end if;
         -- You can't have more than 1 sign (+ or -) in the exponent, where no
         -- '.' is authorized.
         if (Char_Count (Exponent, '.') > 0)
           or else (Char_Count (Exponent, '+') + Char_Count (Exponent, '-') >
                    1)
         then
            Valid := False;
            return;
         end if;
         if (Char_Count (Mantissa, '-') = 1) then
            Positive := False;
         else
            Positive := True;
         end if;
         Mantissa   := Del_Character (Mantissa, '-');
         Mantissa   := Del_Character (Mantissa, '+');
         Exponent   := Del_Character (Exponent, '-');
         Exponent   := Del_Character (Exponent, '+');
         Temp_Valid :=
           (Numeric_String (Mantissa) and then Numeric_String (Exponent));
         if (Temp_Valid = True) then
            Mantissa_Str := Mantissa;
            Exponent_Str := Exponent;
         end if;
         Valid := Temp_Valid;
      end Verify_String_And_Separe_Exponent_And_Mantissa;

      -------------------------------------------------------------------------
      -------

      function Calc_Value (x : in Unbounded_String) return Big_Unsigned is
         Temp_Result : Big_Unsigned          := Big_Unsigned_Zero;
         Ten         : constant Big_Unsigned := Convert (10);
      begin
         for I in  1 .. Length (x) loop
            Temp_Result := Temp_Result * Ten +
                           Convert
                              (Character'Pos (Element (x, I)) -
                               Character'Pos ('0'));
         end loop;
         return Temp_Result;
      end Calc_Value;

      -------------------------------------------------------------------------
      -------

      procedure Calculate_The_Exponent
        (Exponent_String         : in Unbounded_String;
         Mantissa_String         : in out Unbounded_String;
         Exponent_Positive       : out Boolean;
         Exponent_Absolute_Value : out Big_Unsigned)
      is
         I                            : Natural;
         Temp_Exponent_Positive       : Boolean          := True;
         Temp_Unsigned, Temp_Exponent : Big_Unsigned;
         Exponent_Str                 : Unbounded_String := Exponent_String;
      begin
         -- Look for the sign and the value of the exponent
         --
         if (Length (Exponent_Str) /= 0)
           and then (Element (Exponent_Str, 1) = '-')
         then
            Temp_Exponent_Positive := False;
            Exponent_Str           :=
               Copy (Exponent_Str, 2, Length (Exponent_Str) - 1);
         end if;
         Temp_Exponent := Calc_Value (Exponent_Str);
         --
         -- If there is a "." in the mantissa, check its position and modify
         -- the exponent, because the '.' will be removed.
         --
         I := First_Matching_Char_Position (Mantissa_String, '.');
         if (I /= 0) then                       -- There is a '.' in
                                                --Mantissa_String
            Temp_Unsigned := Convert (Length (Mantissa_String) - I);
            if (Temp_Exponent_Positive = False) then
               Temp_Exponent := Temp_Exponent + Temp_Unsigned;
            else
               if (Temp_Unsigned > Temp_Exponent) then
                  Temp_Exponent_Positive := False;
                  Temp_Exponent          := Temp_Unsigned - Temp_Exponent;
               else
                  Temp_Exponent := Temp_Exponent - Temp_Unsigned;
               end if;
            end if;
            --
            -- Now, the exponent is computed. The "." can be deleted in the
            -- mantissa.
            --
            Mantissa_String := Copy (Mantissa_String, 1, I - 1) &
                               Copy
                                  (Mantissa_String,
                                   I + 1,
                                   Length (Mantissa_String) - I);
         end if;
         Exponent_Positive       := Temp_Exponent_Positive;
         Exponent_Absolute_Value := Temp_Exponent;
      end Calculate_The_Exponent;

   ----------------------------------------------------------------------------
   ----

   begin
      declare
      -- In procedures which require a lot of lines of code, I prefer to
      --declare
      -- the variables only when it is necessary. This method eliminates a
      --cause
      -- of side effects during the sub-procedures.
         Temp_Str                                 : Unbounded_String :=
            Prepare_String (x);
         I                                        : Natural;
         Temp_Unsigned, Exponent                  : Big_Unsigned;
         Temp, Exponent_Str, Mantissa_Str, Period : Unbounded_String;
         Valid, Exponent_Positive                 : Boolean;
         Temp_Result                              : Fraction;
      begin
         -- If Nb_Periods < 2 or if the string is not valid, then exit. Remark
         -- : the function
         Verify_String_And_Separe_Exponent_And_Mantissa
           (Temp_Str,
            Exponent_Str,
            Mantissa_Str,
            Temp_Result.Positive,
            Valid);
         ------------------------------------------
         -- In case the parameters are not valid --
         ------------------------------------------
         if (Valid = False) then
            Error := True;
            -- Set Result to an impossible value (0/0), so, in case of error,
            -- and if the parameter Error is not checked after a call to this
            -- procedure, the program will hang.
            Result.Numerator   := Big_Unsigned_Zero;
            Result.Denominator := Big_Unsigned_Zero;
            Result.Positive    := True;
            return;
         else
            Error := False;
         end if;

         -- Now, Error = FALSE, and there is no situation that could change it.
         -- Note that the sign of the result has already been set.
         ---------------------------------
         -- Computation of the exponent --
         ---------------------------------
         Calculate_The_Exponent
           (Exponent_Str,
            Mantissa_Str,
            Exponent_Positive,
            Exponent);
         -------------------------------------------------------------
         -- Now, search for the period                              --
         -- and calculate the number of repetitions of this period. --
         -------------------------------------------------------------
         Period := Find_the_Smallest_Period (Mantissa_Str);
         I      := Count_the_Number_of_Periods (Mantissa_Str, Period);
         -----------------------------------------------
         -- Now, we can really calculate the fraction --
         -----------------------------------------------
         if (Period = Null_Unbounded_String) or else (I < Nb_Periods) then
            Temp_Result.Numerator   := Calc_Value (Mantissa_Str);
            Temp_Result.Denominator := Convert (1);
         else
            Temp                  :=
               Copy
                 (Mantissa_Str,
                  1,
                  Length (Mantissa_Str) - I * Length (Period));
            Temp_Result.Numerator := Calc_Value (Temp & Period) -
                                     Calc_Value (Temp);
            Temp                  := Null_Unbounded_String;
            for J in  1 .. Length (Period) loop
               Temp := Temp & "9";
            end loop;
            Temp_Result.Denominator := Calc_Value (Temp);
            Temp_Result             := Simplify_Fraction (Temp_Result);
            --
            -- We must modify the exponent :
            --   - the length of the numerator has (I-1)*LENGTH(Period) digits
            --less;
            --   - the length of the denominator has (LENGTH(Period)) digits
            --more
            -- So we have to add I*LENGTH(Period) to the exponent.
            --
            Temp_Unsigned := Convert (I) * Convert (Length (Period));
            if (Exponent_Positive = True) then
               Exponent := Exponent + Temp_Unsigned;
            else
               if (Temp_Unsigned > Exponent) then
                  Exponent_Positive := True;
                  Exponent          := Temp_Unsigned - Exponent;
               else
                  Exponent := Exponent - Temp_Unsigned;
               end if;
            end if;
         end if;
         --
         -- If Exponent /= 0 ...
         --
         if (Exponent.Last_Significant_Digit /= First_Indice)
           or else (Exponent.Mantissa (First_Indice) /= 0)
         then
            -- This combination of tests is equivalent to the test (Exponent /=
            -- Big_Unsigned_Zero), but it is executed faster.
            Exponent := Convert (10) ** Exponent;
            if (Exponent_Positive = True) then
               Temp_Result.Numerator := Temp_Result.Numerator * Exponent;
            else
               Temp_Result.Denominator := Temp_Result.Denominator * Exponent;
            end if;
            -- The fraction has been modified, so enventually it could be
            --simplified.
            Temp_Result := Simplify_Fraction (Temp_Result);
         end if;
         Result := Temp_Result;
      end;                   -- END DECLARE
   end Float_in_UString2Fraction;

   --**************************************************************************
   --****
   --**************************************************************************
   --****

end Big_Fraction;
