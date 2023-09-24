--*******************************************************
--***********************
--                                                                            *
--*
--  File:        BINUUNNU.ADB
--**
--  Description: Big_Number.Unsigned_Number package body
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
package body Unsigned_Number is

   --**************************************************************************
   --****
   --**************************************************************************
   --****
   --**                              Comparisons
   --**
   --**
   --**
   --** The comparisons ">", "<=" and ">=" can be done by a call to "<"
   --**
   --** (see below for explanations). The advantage is that we have only one
   --**
   --** function to write and test, the 3 others depend on it.
   --**
   --**************************************************************************
   --****
   --**************************************************************************
   --****

   function "<" (Left, Right : in Big_Unsigned) return Boolean is
      -- Remark : in order to understand this function, you must remind that
      --the
      -- coding of the digits of x and y, in their mantissa, looks like the
      -- "Big_Endian" coding.
      Indice : Index_Type := Left.Last_Significant_Digit;
   begin
      if (Indice /= Right.Last_Significant_Digit) then
         return (Indice < Right.Last_Significant_Digit);
      end if;
      -- Now, Left.Last_Significant_Digit = Right.Last_Significant_Digit
      while (Left.Mantissa (Indice) = Right.Mantissa (Indice))
        and then (Indice > First_Indice)
      loop
         Indice := Indice - 1;
      end loop;
      return (Left.Mantissa (Indice) < Right.Mantissa (Indice));
   end "<";

   --**************************************************************************
   --****

   function ">" (Left, Right : in Big_Unsigned) return Boolean is
   begin
      -- (Left > Right) is the same comparison than (Right < Left)...
      return (Right < Left);
   end ">";

   --**************************************************************************
   --****

   function "<=" (Left, Right : in Big_Unsigned) return Boolean is
   -- (Left <= Right) = NOT (Right < Left)
   begin
      return not (Right < Left);
   end "<=";

   --**************************************************************************
   --****

   function ">=" (Left, Right : in Big_Unsigned) return Boolean is
   begin
      -- (Left >= Right) = NOT (Left < Right).
      return not (Left < Right);
   end ">=";

   --**************************************************************************
   --****

   function Is_Null (x : in Big_Unsigned) return Boolean is
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

   function Min (x, y : in Big_Unsigned) return Big_Unsigned is
   begin
      if (x < y) then
         return x;
      else
         return y;
      end if;
   end Min;

   --**************************************************************************
   --****

   function Max (x, y : in Big_Unsigned) return Big_Unsigned is
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
   --**                            Common functions
   --**
   --**************************************************************************
   --****
   --**************************************************************************
   --****

   procedure Swap1 (x, y : in out Big_Unsigned) is
      Temp : constant Big_Unsigned := x;
   begin
      x := y;
      y := Temp;
   end Swap1;

   --**************************************************************************
   --****

   procedure Swap2 (x, y : in out Big_Unsigned) is
      Max_Index : constant Index_Type :=
         Max (x.Last_Significant_Digit, y.Last_Significant_Digit);
      Temp      : My_Type;
   begin
      for I in  First_Indice .. Max_Index loop
         Temp           := x.Mantissa (I);
         x.Mantissa (I) := y.Mantissa (I);
         y.Mantissa (I) := Temp;
      end loop;
   end Swap2;

   --**************************************************************************
   --****

   function Shift_Right
     (Value  : in Largest_Unsigned;
      Amount : in Natural)
      return   Largest_Unsigned;
   function Shift_Left
     (Value  : in Largest_Unsigned;
      Amount : in Natural)
      return   Largest_Unsigned;

   function Shift_Right
     (Value  : in My_Type;
      Amount : in Natural)
      return   My_Type;
   pragma Import (Intrinsic, Shift_Right);

   function Shift_Left
     (Value  : in My_Type;
      Amount : in Natural)
      return   My_Type;
   pragma Import (Intrinsic, Shift_Left);

   --**************************************************************************
   --****

   procedure Inc (x : in out Big_Unsigned) is
      Temp_Index : Index_Type := First_Indice;
   begin
      while (x.Mantissa (Temp_Index) = Greatest_Digit) loop
         -- If x.Mantissa(Temp_Index) = Greatest_Digit, set it to 0 and try to
         -- increment the next digit.
         if (Temp_Index = Last_Indice) then
            -- In this case, the next digit can't be incremented.
            raise Big_Number_Overflow;
         end if;
         x.Mantissa (Temp_Index) := 0;
         Temp_Index              := Temp_Index + 1;
      end loop;
      x.Mantissa (Temp_Index) := x.Mantissa (Temp_Index) + 1;
      if (Temp_Index > x.Last_Significant_Digit) then
         x.Last_Significant_Digit := Temp_Index;
      end if;
   end Inc;

   --**************************************************************************
   --****

   procedure Dec (x : in out Big_Unsigned) is
      Temp_Index : Index_Type := First_Indice;
   begin
      if (x.Last_Significant_Digit = First_Indice)
        and then (x.Mantissa (First_Indice) = 0)
      then
         -- If x = Big_Unsigned_Zero...
         raise Big_Unsigned_Substraction_Error;
      end if;
      while (x.Mantissa (Temp_Index) = 0) loop
         -- If x.Mantissa(Temp_Index) = 0, set it to Greatest_Digit and try to
         -- decrement the next digit.
         x.Mantissa (Temp_Index) := Greatest_Digit;
         Temp_Index              := Temp_Index + 1;
      end loop;
      x.Mantissa (Temp_Index) := x.Mantissa (Temp_Index) - 1;
      if (Temp_Index > First_Indice)
        and then (x.Mantissa (Temp_Index) = 0)
        and then (Temp_Index = x.Last_Significant_Digit)
      then
         x.Last_Significant_Digit := Temp_Index - 1;
      end if;
   end Dec;

   --**************************************************************************
   --****

   function "+" (x, y : in Big_Unsigned) return Big_Unsigned is
      Min_Index   : constant Index_Type :=
         Min (x.Last_Significant_Digit, y.Last_Significant_Digit);
      Max_Index   : constant Index_Type :=
         Max (x.Last_Significant_Digit, y.Last_Significant_Digit);
      Temp_Index  : Index_Type          := Min_Index;
      Carry       : Largest_Unsigned    := 0;
      Temp_Elt    : Largest_Unsigned;
      Temp_Result : Big_Unsigned;
   begin
      for i in  (Max_Index + 1) .. Last_Indice loop
         Temp_Result.Mantissa (i) := 0;
      end loop;
      Temp_Result.Last_Significant_Digit := Max_Index;
      for i in  First_Indice .. Min_Index loop
         -- Remark : if i = First_Indice, Carry is 0.
         Temp_Elt := Largest_Unsigned (x.Mantissa (i)) +
                     Largest_Unsigned (y.Mantissa (i)) +
                     Carry;
         Carry    := Shift_Right (Temp_Elt, My_Type_Size);
         -- Carry := Temp_Elt / Base
         Temp_Result.Mantissa (i) := My_Type (Temp_Elt and Base_Minus_One);
         -- := Temp_Elt MOD Base
      end loop;
      if (Min_Index /= Max_Index) then
         if (x.Last_Significant_Digit > Min_Index) then
            Temp_Result.Mantissa (Min_Index + 1 .. Max_Index) :=
              x.Mantissa (Min_Index + 1 .. Max_Index);
         else
            Temp_Result.Mantissa (Min_Index + 1 .. Max_Index) :=
              y.Mantissa (Min_Index + 1 .. Max_Index);
         end if;
      end if;
      while (Carry > 0) and then (Temp_Index < Last_Indice) loop
         Temp_Index := Temp_Index + 1;
         if (Temp_Result.Mantissa (Temp_Index) < Greatest_Digit) then
            Carry                             := 0;
            Temp_Result.Mantissa (Temp_Index) :=
              Temp_Result.Mantissa (Temp_Index) + 1;
            if (Max_Index < Temp_Index) then
               Temp_Result.Last_Significant_Digit := Temp_Index;
            end if;
         else
            if (Temp_Index = Last_Indice) then
               raise Big_Number_Overflow;
            end if;
            Temp_Result.Mantissa (Temp_Index) := 0;
         end if;
      end loop;
      return Temp_Result;
   end "+";

   --**************************************************************************
   --****

   function "-" (x, y : in Big_Unsigned) return Big_Unsigned is
   begin
      if (x < y) then
         raise Big_Unsigned_Substraction_Error;
      end if;
      --
      -- Common situation : x > y.
      -- We must calculate x - y.
      --
      declare
         Nb_x        : constant Index_Type := x.Last_Significant_Digit;
         Nb_y        : constant Index_Type := y.Last_Significant_Digit;
         Temp_Index  : Index_Type;
         Carry       : My_Type             := 0;
         Temp_Result : Big_Unsigned;
      begin
         for i in  First_Indice .. Nb_y loop
            if (x.Mantissa (i) > y.Mantissa (i))
              or else (x.Mantissa (i) = y.Mantissa (i) and then Carry = 0)
            then
               -- This test is equivalent to (x.Mantissa(i) >= y.Mantissa(i) +
               --Carry),
               -- but this last expression could raise a CONSTRAINT_ERROR if
               -- y.Mantissa(i) = My_Type'LAST and Carry = 1.
               Temp_Result.Mantissa (i) := x.Mantissa (i) -
                                           y.Mantissa (i) -
                                           Carry;
               Carry                    := 0;
            else  -- x.Mantissa(i) < y.Mantissa(i) + Carry
               -- Temp_Result.Mantissa(i):=Base + x.Mantissa(i) -
               --y.Mantissa(i) - Carry
               -- Base = My_Type'LAST + 1
               -- ==> Temp_Result.Mantissa(i)
               --                :=x.Mantissa(i)+(My_Type'LAST-y.Mantissa(i))+(
               --1-Carry)
               -- My_Type'LAST = Greatest_Digit My_Type'LAST-y.Mantissa(i) =
               -- Greatest_Digit XOR y.Mantissa(i) ==> Temp_Result.Mantissa(i)
               --          :=x.Mantissa(i)+(Greatest_Digit XOR
               --y.Mantissa(i))+(1-Carry)
               -- Don't change the order of operations, because that could
               --cause
               -- overflows.
               Temp_Result.Mantissa (i) := x.Mantissa (i) +
                                           (Greatest_Digit xor
                                            y.Mantissa (i)) +
                                           (1 - Carry);
               -- Then Carry must be set to 1.
               Carry := 1;
            end if;
         end loop;
         Temp_Index := Nb_y + 1;
         while (Carry = 1) loop
            if (x.Mantissa (Temp_Index) >= Carry) then
               Temp_Result.Mantissa (Temp_Index) := x.Mantissa (Temp_Index) -
                                                    Carry;
               Carry                             := 0;
            else
               -- x.Mantissa(Temp_Index) < Carry. This situation can occur only
               -- if x.Mantissa(i) = 0 and if Carry is already equal to 1.
               Temp_Result.Mantissa (Temp_Index) := Greatest_Digit;
               -- Greatest_Digit := My_Type'LAST; It is not necessary to add
               -- "Carry := 1;", because Carry is already set to 1.
            end if;
            Temp_Index := Temp_Index + 1;
         end loop;
         for i in  Temp_Index .. Nb_x loop
            Temp_Result.Mantissa (i) := x.Mantissa (i);
         end loop;
         for i in  (Nb_x + 1) .. Last_Indice loop
            Temp_Result.Mantissa (i) := 0;
         end loop;
         Temp_Index := Nb_x;
         while (Temp_Result.Mantissa (Temp_Index) = 0)
           and then (Temp_Index > First_Indice)
         loop
            Temp_Index := Temp_Index - 1;
         end loop;
         Temp_Result.Last_Significant_Digit := Temp_Index;
         return Temp_Result;
      end;
   end "-";

   --**************************************************************************
   --****

   function Shift_Left
     (x    : in Big_Unsigned;
      Nb   : in Index_Type)
      return Big_Unsigned
   is
   begin
      -- IF (Nb > Last_Indice) OR ELSE (x.Last_Significant_Digit + Nb >
      --Last_Indice) THEN
      --   RAISE Big_Number_Overflow;
      -- END IF;
      -- In these lines of code, (x.Last_Significant_Digit + Nb) could raise
      -- an overflow, so they are replaced by the following lines :
      if (Nb > Last_Indice)
        or else (x.Last_Significant_Digit > Last_Indice - Nb)
      then
         raise Big_Number_Overflow;
      end if;
      declare
         Result            : Big_Unsigned;
         Last_Digit_Indice : constant Index_Type :=
            x.Last_Significant_Digit + Nb;
      begin
         if (Nb /= 0) then
            for I in  First_Indice .. (Nb - 1) loop
               Result.Mantissa (I) := 0;
            end loop;
         end if;
         if (Last_Digit_Indice /= Last_Indice) then
            for I in  (Last_Digit_Indice + 1) .. Last_Indice loop
               Result.Mantissa (I) := 0;
            end loop;
         end if;
         Result.Mantissa (Nb .. Last_Digit_Indice) :=
           x.Mantissa (First_Indice .. x.Last_Significant_Digit);
         Result.Last_Significant_Digit             := Last_Digit_Indice;
         return Result;
      end;
   end Shift_Left;

   --**************************************************************************
   --****

   function "*" (x : in Big_Unsigned; y : in My_Type) return Big_Unsigned is
   begin
      if (y = 0) then
         return Big_Unsigned_Zero;
      elsif (y = 1) then
         return x;
      end if;
      declare
         Nb_x   : constant Index_Type       := x.Last_Significant_Digit;
         Temp_y : constant Largest_Unsigned := Largest_Unsigned (y);
         Carry  : Largest_Unsigned          := 0;
         Result : Big_Unsigned;
      begin
         if (Nb_x < Last_Indice) then
            for I in  (Nb_x + 1) .. Last_Indice loop
               Result.Mantissa (I) := 0;
            end loop;
         end if;
         for I in  First_Indice .. Nb_x loop
            Carry               :=
              Largest_Unsigned (x.Mantissa (I)) * Temp_y + Carry;
            Result.Mantissa (I) :=
              My_Type (Carry and Largest_Unsigned (My_Type'Last));
            Carry               := Shift_Right (Carry, My_Type_Size);   -- :=
                                                                        --Carry
                                                                        -- DIV
                                                                        --Base
         end loop;
         if (Carry = 0) then
            Result.Last_Significant_Digit := Nb_x;
         else
            if (Nb_x < Last_Indice) then
               Result.Last_Significant_Digit := Nb_x + 1;
               Result.Mantissa (Nb_x + 1)    := My_Type (Carry);
            else
               raise Big_Number_Overflow;
            end if;
         end if;
         return Result;
      end;             -- END of the DECLARE block
   end "*";

   --**************************************************************************
   --****

   function Special_Mul
     (x          : in Big_Unsigned;
      y          : in My_Type;
      Left_Shift : in Index_Type)
      return       Big_Unsigned
   is
   -- Internal function
   -- This function returns x * y, with a left shift of Left_Shift digits.
   -- algorithm similar to the one used for "*"(x : IN Big_Unsigned; y : IN
   --My_Type)
   begin
      if (y = 0) then
         return Big_Unsigned_Zero;
      end if;
      declare
         Nb_x            : constant Index_Type       :=
           x.Last_Significant_Digit;
         Expected_Indice : constant Index_Type       := Nb_x + Left_Shift;
         Temp_y          : constant Largest_Unsigned := Largest_Unsigned (y);
         Carry           : Largest_Unsigned          := 0;
         Result          : Big_Unsigned;
      begin
         if (Left_Shift > First_Indice) then
            for I in  First_Indice .. (Left_Shift - 1) loop
               Result.Mantissa (I) := 0;
            end loop;
         end if;
         if (Expected_Indice < Last_Indice) then
            for I in  (Expected_Indice + 1) .. Last_Indice loop
               Result.Mantissa (I) := 0;
            end loop;
         end if;
         for I in  First_Indice .. Nb_x loop
            Carry                            :=
              Largest_Unsigned (x.Mantissa (I)) * Temp_y + Carry;
            Result.Mantissa (I + Left_Shift) :=
              My_Type (Carry and Largest_Unsigned (My_Type'Last));
            Carry                            :=
               Shift_Right (Carry, My_Type_Size);        -- := Carry DIV Base
         end loop;
         if (Carry = 0) then
            Result.Last_Significant_Digit := Expected_Indice;
         else
            if (Expected_Indice < Last_Indice) then
               Result.Last_Significant_Digit         := Expected_Indice + 1;
               Result.Mantissa (Expected_Indice + 1) := My_Type (Carry);
            else
               raise Big_Number_Overflow;
            end if;
         end if;
         return Result;
      end;             -- END of the DECLARE block
   end Special_Mul;

   pragma Inline (Special_Mul);

   --**************************************************************************
   --****

   function "*" (x, y : in Big_Unsigned) return Big_Unsigned is
      Lastx  : constant Index_Type := x.Last_Significant_Digit;
      Lasty  : constant Index_Type := y.Last_Significant_Digit;
      Result : Big_Unsigned;
   begin
      if (Lastx = First_Indice) then
         return (y * x.Mantissa (First_Indice));
      end if;
      if (Lastx > Last_Indice - Lasty) then
         raise Big_Number_Overflow;
      end if;
      Result := x * y.Mantissa (First_Indice);
      -- Result is initialized here, because I don't want to waste execution
      -- time before, during the computation of the simple cases.
      for I in  1 .. Lasty loop
         Result := Result + Special_Mul (x, y.Mantissa (I), I);
      end loop;
      return Result;
   exception
      when others =>
         raise Big_Number_Overflow;
   end "*";

   --**************************************************************************
   --****

   function "**" (x : in Big_Unsigned; y : in My_Type) return Big_Unsigned is
   begin
      if (y = 0) then
         return Big_Unsigned_One;
      end if;
      if ((x.Last_Significant_Digit = First_Indice)
         and then (x.Mantissa (First_Indice) <= 1))
        or else (y = 1)
      then
         -- Here, y /= 0.
         -- So, if y = 1 or x = 0 or x = 1, the result of (x ** y) is always x.
         return x;
      end if;
      declare
         Temp_Result : Big_Unsigned := x;
      begin
         for I in  2 .. (y / 2) loop
            Temp_Result := Temp_Result * x;
         end loop;
         Temp_Result := Temp_Result * Temp_Result;
         if (y mod 2 = 1) then
            Temp_Result := Temp_Result * x;
         end if;
         return Temp_Result;
      end;
   end "**";

   --**************************************************************************
   --****

   function "**"
     (x    : in Big_Unsigned;
      y    : in Index_Type)
      return Big_Unsigned
   is
   begin
      if (y = 0) then
         return Big_Unsigned_One;
      end if;
      if ((x.Last_Significant_Digit = First_Indice)
         and then (x.Mantissa (First_Indice) <= 1))
        or else (y = 1)
      then
         -- Here, y /= 0.
         -- So, if y = 1 or x = 0 or x = 1, the result of (x ** y) is always x.
         return x;
      end if;
      declare
         Temp_Result : Big_Unsigned := x;
      begin
         for I in  2 .. (y / 2) loop
            Temp_Result := Temp_Result * x;
         end loop;
         Temp_Result := Temp_Result * Temp_Result;
         if (y mod 2 = 1) then
            Temp_Result := Temp_Result * x;
         end if;
         return Temp_Result;
      end;
   end "**";

   --**************************************************************************
   --****

   function "**" (x, y : in Big_Unsigned) return Big_Unsigned is
      Temp_Result : Big_Unsigned := x;
      Indice      : Big_Unsigned := Big_Unsigned_One;
   begin
      if (y.Last_Significant_Digit = First_Indice) then
         if (y.Mantissa (First_Indice) = 0) then
            -- If y = 0, x^y = x^0 = 1, even if x = 0.
            return Big_Unsigned_One;
         else
            return x ** My_Type (y.Mantissa (First_Indice));
         end if;
      end if;
      while (Indice < y) loop
         Inc (Indice);
         Temp_Result := Temp_Result * x;
      end loop;
      return Temp_Result;
   end "**";

   --**************************************************************************
   --****

   function "**"
     (x    : in Big_Unsigned;
      y    : in Big_Signed)
      return Big_Unsigned
   is
   begin
      if (y.Positive = False) then     -- Here, we test if (y <
                                       --Big_Signed_Zero).
         return Big_Unsigned_Zero;
      end if;
      declare
         Temp_y : constant Big_Unsigned := Signed2Unsigned (y);
      begin
         return (x ** Temp_y);
      end;
   end "**";

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   procedure Big_Div
     (x, y            : in Big_Unsigned;
      Quot, Remainder : out Big_Unsigned)
   is
      Lasty : constant Index_Type := y.Last_Significant_Digit;
   begin
      -- analysis of simple cases.
      if (Lasty = First_Indice) then
         case y.Mantissa (First_Indice) is
            when 0 =>
               raise Division_By_Zero;
            when 1 =>
               Quot      := x;
               Remainder := Big_Unsigned_Zero;
               return;
            when others =>
               declare
                  Temp_Remainder : My_Type;
                  Temp_y         : constant My_Type :=
                     y.Mantissa (First_Indice);
               begin
                  -- If y has only one significant item in its mantissa, we use
                  -- the function Short_Div, which is faster. See below for the
                  -- implementation of Short_Div.
                  Short_Div (x, Temp_y, Quot, Temp_Remainder);
                  Remainder :=
                    (Last_Significant_Digit => First_Indice,
                     Mantissa               => (Temp_Remainder, others => 0));
                  return;
               end;
         end case;
      end if;
      if (x.Last_Significant_Digit = First_Indice)
        and then (x.Mantissa (First_Indice) = 0)
      then
         Quot      := Big_Unsigned_Zero;
         Remainder := Big_Unsigned_Zero;
         return;
      elsif (x < y) then
         Quot      := Big_Unsigned_Zero;
         Remainder := x;
         return;
      elsif (x = y) then
         Quot      := Big_Unsigned_One;
         Remainder := Big_Unsigned_Zero;
         return;
      end if;
      -- Now, there is only the case where (x > y) and (y /= 0).
      declare

         function Two_Biggest_Digits
           (x    : in Big_Unsigned)
            return Largest_Unsigned
         is
            Lastx : constant Index_Type := x.Last_Significant_Digit;
         begin
            -- We don't have to check if there is only one digit : because of
            -- the algorithm, there are at least two digits.
            return Largest_Unsigned (x.Mantissa (Lastx - 1)) +
                   Shift_Left
                      (Largest_Unsigned (x.Mantissa (Lastx)),
                       My_Type_Size);
         end Two_Biggest_Digits;

         ----------------------------------------------------------------------
         --------

         Two_Biggest_Digits_of_y      : constant Largest_Unsigned :=
            Two_Biggest_Digits (y);
         Temp_Unsigned                : Big_Unsigned;
         Temp_Quot                    : Big_Unsigned              :=
           Big_Unsigned_Zero;
         Temp_Remainder               : Big_Unsigned              := x;
         Temp_Index, Last_Indice_Quot : Index_Type;
         Carry                        : My_Type;
         Guess_Digit                  : My_Type;

         ----------------------------------------------------------------------
         --------

         function Last_Indice_of_the_Quotient return Index_Type is
            -- The function Last_Indice_of_the_Quotient is used to calculate
            --the
            -- number of items of the quotient and, so, the indice of the
            --array (used
            -- to represent the mantissa) where we have to put the guessed
            --digit.
            Diff   : constant Index_Type :=
               Temp_Remainder.Last_Significant_Digit - Lasty;
            Indice : Index_Type          := Lasty;
         begin
            while (Temp_Remainder.Mantissa (Indice + Diff) =
                   y.Mantissa (Indice))
              and then (Indice > First_Indice)
            loop
               Indice := Indice - 1;
            end loop;
            if (Temp_Remainder.Mantissa (Indice + Diff) <
                y.Mantissa (Indice))
              and then (Diff > First_Indice)
            then
               return Diff - 1;
            else
               return Diff;
            end if;
         end Last_Indice_of_the_Quotient;

         ----------------------------------------------------------------------
         --------

         function Guess return My_Type is
            Last       : constant Index_Type       :=
              Temp_Remainder.Last_Significant_Digit;
            Temp_Rem   : constant Largest_Unsigned :=
               Two_Biggest_Digits (Temp_Remainder);
            Temp_Digit : Largest_Unsigned;
         begin
            -- In this function, Last >= Lasty, and lasty >= 1.
            if (Last_Indice_Quot = Last - Lasty) then
               Temp_Digit := Temp_Rem / Two_Biggest_Digits_of_y;
               if (Lasty >= 2)
                 and then (Temp_Rem mod Two_Biggest_Digits_of_y <
                           Shift_Right
                              (Largest_Unsigned (y.Mantissa (Lasty - 2)) *
                               Temp_Digit,
                               My_Type_Size))
               then
                  Temp_Digit := Temp_Digit - 1;
               end if;
               return My_Type (Temp_Digit);
            end if;
            -- We have now to study the case where Indice_Digit = Last - Lasty
            --- 1.
            -- Search for Temp_Digit, so Temp_Remainder / y = Temp_Digit
            declare
               Temp_y1   : constant Largest_Unsigned :=
                  Largest_Unsigned (y.Mantissa (Lasty));
               Temp_y2   : constant Largest_Unsigned :=
                  Largest_Unsigned (y.Mantissa (Lasty - 1));
               Max_Digit : Largest_Unsigned          := Temp_Rem / Temp_y1;
               Min_Digit : Largest_Unsigned          :=
                  Temp_Rem / (Temp_y1 + 1);
               Temp      : Largest_Unsigned;
            begin
               if (Max_Digit > Base_Minus_One) then
                  Max_Digit := Base_Minus_One;
               end if;
               loop
                  if (Max_Digit = Min_Digit) then
                     return My_Type (Min_Digit);
                  end if;
                  if (Max_Digit - Min_Digit = 1) then
                     Temp := Shift_Right (Temp_y2 * Max_Digit, My_Type_Size) +
                             (Temp_y1 * Max_Digit);
                     if (Temp <= Temp_Rem) then
                        return My_Type (Max_Digit);
                     else
                        return My_Type (Min_Digit);
                     end if;
                  end if;
                  -- Now, Max_Digit - Min_Digit >= 2
                  Temp_Digit := Shift_Right (Max_Digit + Min_Digit, 1);
                  -- Temp_Digit := (Max_Digit + Min_Digit) / 2;
                  Temp := Shift_Right (Temp_y2 * Temp_Digit, My_Type_Size) +
                          (Temp_y1 * Temp_Digit);
                  if (Temp = Temp_Rem) then
                     return My_Type (Temp_Digit);
                  elsif (Temp > Temp_Rem) then
                     Max_Digit := Temp_Digit;
                     if ((Temp - Temp_Rem) mod Temp_y1 > 0) then
                        Temp := (Temp - Temp_Rem) / Temp_y1 + 1;
                     else
                        Temp := (Temp - Temp_Rem) / Temp_y1;
                     end if;
                     if (Temp <= Temp_Digit) then
                        Min_Digit := Temp_Digit - Temp;
                     end if;
                  else   -- Temp < Temp_Rem
                     Min_Digit := Temp_Digit;
                     Temp      := (Temp_Rem - Temp) / Temp_y1;
                     if (Base_Minus_One - Temp_Digit >= Temp) then
                        Max_Digit := Temp_Digit + Temp;
                     end if;
                  end if;
               end loop;
            end;
         end Guess;

      -------------------------------------------------------------------------
      -----

      begin
         Temp_Quot.Last_Significant_Digit := Last_Indice_of_the_Quotient;
         while (y < Temp_Remainder) loop
            Last_Indice_Quot := Last_Indice_of_the_Quotient;
            Guess_Digit      := Guess;
            --
            -- Guess_Digit is the real digit or the real digit plus one.
            -- Temp_Unsigned := y * Guess_Digit * (Base**Last_Indice_Quot)
            --
            Temp_Unsigned := Special_Mul (y, Guess_Digit, Last_Indice_Quot);
            while (Temp_Remainder < Temp_Unsigned) loop
               -- Decrement Guess_Digit and calculate the new value of
               --Temp_Unsigned
               -- We have not to test if Guess_Digit = 0,
               -- because Temp_Unsigned would be equal to zero,
               -- so it could not be greater than Temp_Remainder.
               Guess_Digit := Guess_Digit - 1;
               -- The following lines are a kind of inlining for calculating
               -- Temp_Unsigned - y * (Base**Last_Indice_Quot)
               Carry := 0;
               for i in  First_Indice .. Lasty loop
                  Temp_Index := i + Last_Indice_Quot;
                  if (Temp_Unsigned.Mantissa (Temp_Index) > y.Mantissa (i))
                    or else (Temp_Unsigned.Mantissa (Temp_Index) =
                             y.Mantissa (i)
                            and then Carry = 0)
                  then
                     Temp_Unsigned.Mantissa (Temp_Index) :=
                       Temp_Unsigned.Mantissa (Temp_Index) -
                       y.Mantissa (i) -
                       Carry;
                     Carry                               := 0;
                  else  -- Temp_Unsigned.Mantissa(Temp_Index) < y.Mantissa(i)
                        --+ Carry
                     Temp_Unsigned.Mantissa (Temp_Index) :=
                       Temp_Unsigned.Mantissa (Temp_Index) +
                       (Greatest_Digit xor y.Mantissa (i)) +
                       (1 - Carry);
                     Carry                               := 1;
                  end if;
               end loop;
               -- During the last preceding loop, Temp_Index was equal to
               -- y.Last_Significant_Digit + Last_Indice_Quot.
               if (Carry = 1) then
                  Temp_Index                          := Temp_Index + 1;
                  Temp_Unsigned.Mantissa (Temp_Index) :=
                    Temp_Unsigned.Mantissa (Temp_Index) - Carry;
               end if;
               while (Temp_Unsigned.Mantissa (
                    Temp_Unsigned.Last_Significant_Digit) =
                      0)
                 and then (Temp_Unsigned.Last_Significant_Digit >
                           First_Indice)
               loop
                  Temp_Unsigned.Last_Significant_Digit :=
                    Temp_Unsigned.Last_Significant_Digit - 1;
               end loop;
            end loop;
            Carry := 0;
            --
            -- Calculate the new value of Temp_Remainder, which will be
            -- Temp_Remainder - Temp_Unsigned
            --
            for i in  Last_Indice_Quot .. Temp_Unsigned.Last_Significant_Digit
            loop
               if (Temp_Remainder.Mantissa (i) > Temp_Unsigned.Mantissa (i))
                 or else (Temp_Remainder.Mantissa (i) =
                          Temp_Unsigned.Mantissa (i)
                         and then Carry = 0)
               then
                  Temp_Remainder.Mantissa (i) := Temp_Remainder.Mantissa (i) -
                                                 Temp_Unsigned.Mantissa (i) -
                                                 Carry;
                  Carry                       := 0;
               else  -- Temp_Remainder.Mantissa(i) < Temp_Unsigned.Mantissa(i)
                     --+ Carry
                  Temp_Remainder.Mantissa (i) := Temp_Remainder.Mantissa (i) +
                                                 (Greatest_Digit xor
                                                  Temp_Unsigned.Mantissa (i)) +
                                                 (1 - Carry);
                  Carry                       := 1;
               end if;
            end loop;
            if (Carry = 1) then
               Temp_Index                           :=
                 Temp_Unsigned.Last_Significant_Digit + 1;
               Temp_Remainder.Mantissa (Temp_Index) :=
                 Temp_Remainder.Mantissa (Temp_Index) - Carry;
            end if;
            while (Temp_Remainder.Mantissa (
                 Temp_Remainder.Last_Significant_Digit) =
                   0)
              and then (Temp_Remainder.Last_Significant_Digit > First_Indice)
            loop
               Temp_Remainder.Last_Significant_Digit :=
                 Temp_Remainder.Last_Significant_Digit - 1;
            end loop;
            --
            -- Now, we have the correct values of Guess_Digit and
            --Temp_Remainder.
            --
            Temp_Quot.Mantissa (Last_Indice_Quot) := Guess_Digit;
         end loop;
         Quot      := Temp_Quot;
         Remainder := Temp_Remainder;
      end;        -- DECLARE
   end Big_Div;

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   function "/" (x, y : in Big_Unsigned) return Big_Unsigned is
      Quot, Remainder : Big_Unsigned;
   begin
      Big_Div (x, y, Quot, Remainder);
      return Quot;
   end "/";

   --**************************************************************************
   --****

   function "mod" (x, y : in Big_Unsigned) return Big_Unsigned is
      Quot, Remainder : Big_Unsigned;
   begin
      Big_Div (x, y, Quot, Remainder);
      return Remainder;
   end "mod";

   --**************************************************************************
   --****

   procedure Short_Div
     (x         : in Big_Unsigned;
      y         : in My_Type;
      Quot      : out Big_Unsigned;
      Remainder : out My_Type)
   is
      Lastx     : constant Index_Type       := x.Last_Significant_Digit;
      Temp_Quot : Big_Unsigned              := Big_Unsigned_Zero;
      Carry     : Largest_Unsigned          := 0;
      Temp      : Largest_Unsigned;
      Temp_y    : constant Largest_Unsigned := Largest_Unsigned (y);
   begin
      if (y = 0) then
         raise Division_By_Zero;
      end if;
      if (y = 1) then
         Quot      := x;
         Remainder := 0;
         return;
      end if;
      for Indice in reverse  First_Indice .. Lastx loop
         Temp                        :=
           Largest_Unsigned (x.Mantissa (Indice)) +
           Shift_Left (Carry, My_Type_Size);
         Temp_Quot.Mantissa (Indice) := My_Type (Temp / Temp_y);
         Carry                       := Temp mod Temp_y;
      end loop;
      if (Lastx > 0) and then (Temp_Quot.Mantissa (Lastx) = 0) then
         Temp_Quot.Last_Significant_Digit := Lastx - 1;
      else
         Temp_Quot.Last_Significant_Digit := Lastx;
      end if;
      Quot      := Temp_Quot;
      Remainder := My_Type (Carry);
   end Short_Div;

   --**************************************************************************
   --****

   function "/" (x : in Big_Unsigned; y : in My_Type) return Big_Unsigned is
      Quot      : Big_Unsigned;
      Remainder : My_Type;
   begin
      Short_Div (x, y, Quot, Remainder);
      return Quot;
   end "/";

   --**************************************************************************
   --****

   function "mod" (x : in Big_Unsigned; y : in My_Type) return My_Type is
      Quot      : Big_Unsigned;
      Remainder : My_Type;
   begin
      Short_Div (x, y, Quot, Remainder);
      return Remainder;
   end "mod";

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

   function Number_of_Bytes (x : in Big_Unsigned) return Largest_Unsigned is
      Temp1 : Largest_Unsigned;
      Temp2 : My_Type := x.Mantissa (x.Last_Significant_Digit);
   begin
      Temp1 := Largest_Unsigned (x.Last_Significant_Digit) *
               (My_Type_Size / 8);
      while (Temp2 /= 0) loop
         Temp2 := Shift_Right (Temp2, 8);
         Temp1 := Temp1 + 1;
      end loop;
      return Temp1;
   end Number_of_Bytes;

   --**************************************************************************
   --****

   function Number_of_Bits (x : in Big_Unsigned) return Largest_Unsigned is
      Temp1 : Largest_Unsigned;
      Temp2 : My_Type := x.Mantissa (x.Last_Significant_Digit);
   begin
      Temp1 := Largest_Unsigned (x.Last_Significant_Digit) * My_Type_Size;
      while (Temp2 /= 0) loop
         Temp2 := Shift_Right (Temp2, 1);
         Temp1 := Temp1 + 1;
      end loop;
      return Temp1;
   end Number_of_Bits;

   --**************************************************************************
   --****

   function Square_Root (x : in Big_Unsigned) return Big_Unsigned is
      Result, Temp_Result : Big_Unsigned;
      -- Remark :
      --   In order to avoid the use of a fifth variable, Diff is initialized
      --   3.
      Diff : Big_Unsigned :=
        (Last_Significant_Digit => First_Indice,
         Mantissa               => (3, others => 0));
   begin
      if (x.Last_Significant_Digit = First_Indice) then
         if (x.Mantissa (First_Indice) = 0) then
            return Big_Unsigned_Zero;
         elsif (x.Mantissa (First_Indice) <= 3) then
            return Big_Unsigned_One;
         end if;
      end if;

      -- Boucle de calcul
      Result := x / 2;
      while (Diff.Last_Significant_Digit > First_Indice)
        or else ((Diff.Last_Significant_Digit = First_Indice)
                and then (Diff.Mantissa (First_Indice) > 1))
      loop
         Temp_Result := Result;
         Result      := (Temp_Result + x / Temp_Result) / 2;
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

   function Factorial (x : in Big_Unsigned) return Big_Unsigned is
      Indice, Result : Big_Unsigned := Big_Unsigned_One;
   begin
      if (x.Last_Significant_Digit = First_Indice)
        and then (x.Mantissa (First_Indice) <= 1)
      then
         return Big_Unsigned_One;
      end if;
      while (Indice < x) loop
         Inc (Indice);
         Result := Result * Indice;
      end loop;
      return Result;
   end Factorial;

   --**************************************************************************
   --****

   function GCF (x, y : in Big_Unsigned) return Big_Unsigned is
      -- GCF = greatest common factor

      function Temp_GCF (A, B : in Big_Unsigned) return Big_Unsigned is
         N, Temp_A, Temp : Big_Unsigned;
         Temp_B          : Big_Unsigned := B;
      begin
         Big_Div (A, B, Temp, N);
         loop
            if (N.Last_Significant_Digit = First_Indice) then
               case (N.Mantissa (First_Indice)) is
                  when 0 =>
                     return Temp_B;         -- IF (N = Big_Unsigned_Zero)...
                  when 1 =>
                     return N;              -- IF (N = Big_Unsigned_One)...
                  when others =>
                     null;
               end case;
            end if;
            Temp_A := Temp_B;
            Temp_B := N;
            Big_Div (Temp_A, Temp_B, Temp, N);
            -- In this loop, temp is only a temporary variable and hasn't any
            -- effect in this algorithm. Now, N = Temp_A MOD Temp_B.
         end loop;
      end Temp_GCF;

   begin    -- Function GCF
      if (x.Last_Significant_Digit = First_Indice)
        and then (x.Mantissa (First_Indice) = 0)
      then
         -- IF x = Big_Unsigned_Zero THEN
         return y;
      elsif (x = y)
        or else ((y.Last_Significant_Digit = First_Indice)
                and then (y.Mantissa (First_Indice) = 0))
      then
         return x;
      elsif (x < y) then
         return Temp_GCF (y, x);
      else
         return Temp_GCF (x, y);
      end if;
   end GCF;

   --**************************************************************************
   --****

   function LCM (x, y : in Big_Unsigned) return Big_Unsigned is
   -- finds the Least Common Multiple between 2 arguments : LCM
   begin
      return (x / GCF (x, y)) * y;
   end LCM;

   --**************************************************************************
   --****

   function "and" (x, y : in Big_Unsigned) return Big_Unsigned is
      Temp       : Big_Unsigned := x;
      Temp_Index : Index_Type   :=
         Max (x.Last_Significant_Digit, y.Last_Significant_Digit);
   begin
      for I in  First_Indice .. y.Last_Significant_Digit loop
         Temp.Mantissa (I) := Temp.Mantissa (I) and y.Mantissa (I);
      end loop;
      while (Temp_Index > First_Indice)
        and then (Temp.Mantissa (Temp_Index) = 0)
      loop
         Temp_Index := Temp_Index - 1;
      end loop;
      Temp.Last_Significant_Digit := Temp_Index;
      return Temp;
   end "and";

   --**************************************************************************
   --****

   function "xor" (x, y : in Big_Unsigned) return Big_Unsigned is
      Temp       : Big_Unsigned := x;
      Temp_Index : Index_Type   :=
         Max (x.Last_Significant_Digit, y.Last_Significant_Digit);
   begin
      for I in  First_Indice .. y.Last_Significant_Digit loop
         Temp.Mantissa (I) := Temp.Mantissa (I) xor y.Mantissa (I);
      end loop;
      while (Temp_Index > First_Indice)
        and then (Temp.Mantissa (Temp_Index) = 0)
      loop
         Temp_Index := Temp_Index - 1;
      end loop;
      Temp.Last_Significant_Digit := Temp_Index;
      return Temp;
   end "xor";

   --**************************************************************************
   --****

   function NCR (N, R : in Big_Unsigned) return Big_Unsigned is
      -- NCR(N,R) = N!/[R!(N-R)!]
      -- In this algorythm, we research of the maximum (max_R) and the minimum
      -- (Min_R) of R and (N-R). We have : NCR(N,R) = [N! / Max_R!] / Min_R!;
      -- Then, nCr(N,R) is considered as a fraction, even if its result is an
      -- integer : Temp_NCR_Numerator / Temp_NCR_Denominator with
      -- Temp_NCR_Numerator = N! / Max_R! and Temp_NCR_Denominateur = Min_R!
      Temp_N               : constant Big_Unsigned := N - R;
      Max_R                : constant Big_Unsigned := Max (R, Temp_N);
      Min_R                : constant Big_Unsigned := Min (R, Temp_N);
      Indice               : Big_Unsigned          := Big_Unsigned_One;
      Temp_NCR_Numerator   : Big_Unsigned          :=
         Max_R + Big_Unsigned_One;
      Temp_NCR_Denominator : Big_Unsigned          := Big_Unsigned_One;
      Temp_Num, Temp_Den   : Big_Unsigned;

      procedure Simplify (x, y : in out Big_Unsigned) is
         Temp : constant Big_Unsigned := GCF (x, y);
      begin
         x := x / Temp;
         y := y / Temp;
      end Simplify;

   begin
      if (N = R) then
         return Big_Unsigned_One;
      end if;
      if (N < R) then
         return Big_Unsigned_Zero;
      end if;
      -- Now, N > R.
      while (Indice < Min_R) loop
         Temp_Num := Max_R + Indice;
         Inc (Temp_Num);
         Temp_Den := Indice;
         Inc (Temp_Den);
         Simplify (Temp_Den, Temp_Num);
         Simplify (Temp_NCR_Denominator, Temp_Num);
         Simplify (Temp_NCR_Numerator, Temp_Den);
         Temp_NCR_Numerator   := Temp_NCR_Numerator * Temp_Num;
         Temp_NCR_Denominator := Temp_NCR_Denominator * Temp_Den;
         Inc (Indice);
      end loop;
      return Temp_NCR_Numerator;
   end NCR;

   --**************************************************************************
   --****

   function NPR (N, R : in Big_Unsigned) return Big_Unsigned is
      Temp_Result : Big_Unsigned := Big_Unsigned_One;
      Index       : Big_Unsigned;
   begin
      if (N < R) then
         return Big_Unsigned_Zero;
      end if;
      if (R.Last_Significant_Digit = First_Indice)
        and then (R.Mantissa (First_Indice) = 0)
      then
         return Big_Unsigned_One;
      end if;
      Index := N - R;
      while (Index < N) loop
         Inc (Index);
         Temp_Result := Temp_Result * Index;
      end loop;
      return Temp_Result;
   end NPR;

   --**************************************************************************
   --****

   function K (N, P : in Big_Unsigned) return Big_Unsigned is
      Temp : Big_Unsigned;
   begin
      -- K(N,P) = NCR(N+P-1);
      -- We have to calculate Temp = N+P-1. The 3 following lines allows to
      -- avoid an overflow, in the case if N+P = Big_Unsigned_Last + 1 : in
      -- this situation, Temp will be Big_Unsigned_Last.
      Temp := Max (N, P);
      Dec (Temp);
      Temp := Temp + Min (N, P);
      -- Now, Temp = N+P-1.
      return NCR (Temp, P);
   end K;

   --**************************************************************************
   --****

end Unsigned_Number;
