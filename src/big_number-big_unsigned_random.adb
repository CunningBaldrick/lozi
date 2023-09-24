--
-- Permission to use, copy, modify, and distribute this software and its
-- documentation for any purpose and without fee is hereby granted, provided
-- that the above copyright and authorship notice appear in all copies and that
-- both that copyright notice and this permission notice appear in supporting
-- documentation.
--
-- The ARA makes no representations about the suitability of this software for
-- any purpose. It is provided "as is" without express or implied warranty.
--
--*****************************************************************************
--*
--                                                                            *
--*
--  File:        BNBIUNRA.ADB
--**
--  Description: Big_Number.Big_Unsigned_RANDOM package body
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
package body Big_Unsigned_Random is

   -- In this package, there are in fact two separate pseudo-random number
   -- generators (PRNG) : one for index, one for digits. They are combined to
   -- get a PRNG for Big_Unsigned : so, the last significant digit of the
   -- Big_Unsigned is set independantly of the digits.

   My_Type_Random_Seed : Largest_Unsigned;
   Index_Random_Seed   : Largest_Unsigned;

   A : constant Largest_Unsigned := 125;      -- 125 = 5^3
   B : constant Largest_Unsigned := Largest_Unsigned'Last / A;
   -- Be careful : B must be greater or equal to Nb_Item and Base.

   procedure Randomize is
      t       : Time;
      Year    : Year_Number;
      Month   : Month_Number;    -- 1 to 12
      Day     : Day_Number;        -- 0 to 31
      Seconds : Day_Duration;  -- 0 to 86399
   begin
      t := Clock;
      Split (t, Year, Month, Day, Seconds);
      My_Type_Random_Seed := Largest_Unsigned (Seconds) +
                             Largest_Unsigned (Day) +
                             Largest_Unsigned (Month) +
                             Largest_Unsigned (Year);
      Index_Random_Seed   := Largest_Unsigned (Day) mod Largest_Unsigned (Month);
      Index_Random_Seed   := 2 * Largest_Unsigned (Seconds) + Index_Random_Seed;
      Index_Random_Seed   := 2 * Index_Random_Seed + 1;
      if ((My_Type_Random_Seed mod 2) = 0) then
         My_Type_Random_Seed := My_Type_Random_Seed + 1;
      end if;
   end Randomize;

   function Random (x : in Index_Type) return Index_Type is
      Max : constant Largest_Unsigned := Largest_Unsigned (x);
   begin
      Index_Random_Seed := (A * Index_Random_Seed) mod B;
      if (Max <= Largest_Unsigned'Last / 4) then
         return Index_Type (Index_Random_Seed mod (Max + 1));
      else
         loop
            Index_Random_Seed := (A * Index_Random_Seed) mod B;
            exit when (Index_Random_Seed <= Max);
         end loop;
         return Index_Type (Index_Random_Seed);
      end if;
   end Random;

   function Random return My_Type is
   begin
      My_Type_Random_Seed := (A * My_Type_Random_Seed) mod B;
      return My_Type (My_Type_Random_Seed and
                      Largest_Unsigned (Greatest_Digit));
      -- Returns My_Type_Random_Seed MOD Base
   end Random;

   function Random (x : in My_Type) return My_Type is
      Result : My_Type;
   begin
      if (x = 0) then
         return 0;
      elsif (x < 2 ** (My_Type_Size / 4)) then
         Result := Random;
         return (Result mod (x + 1));
      end if;
      loop
         Result := Random;
         if (Result <= x) then
            exit;
         end if;
      end loop;
      return Result;
   end Random;

   function Random (x : in Index_Type) return Big_Unsigned is
      Temp_Result : Big_Unsigned;
      Temp        : Index_Type := x;
   begin
      for I in  First_Indice .. Temp loop
         Temp_Result.Mantissa (I) := Random;
      end loop;
      for I in  (Temp + 1) .. Last_Indice loop
         Temp_Result.Mantissa (I) := 0;
      end loop;
      while (Temp > First_Indice)
        and then (Temp_Result.Mantissa (Temp) = 0)
      loop
         Temp := Temp - 1;
      end loop;
      Temp_Result.Last_Significant_Digit := Temp;
      return Temp_Result;
   end Random;

   function Random return Big_Unsigned is
      Temp : Index_Type := Random (Last_Indice);
   begin
      return Random (Temp);
   end Random;

   function Random (x : in Big_Unsigned) return Big_Unsigned is
      Temp_Unsigned              : Big_Unsigned;
      Temp_Index                 : Index_Type;
      Temp_Boolean               : Boolean             := True;
      Temp_MyType1, Temp_MyType2 : My_Type;
      Last_x                     : constant Index_Type :=
        x.Last_Significant_Digit;
   -- Be careful, we can't use the following algorithm :
   --
   -- IF (x = Big_Unsigned_Zero) THEN
   --   RETURN Big_Unsigned_Zero;
   -- END IF;
   -- Temp : Big_Unsigned := Random;
   -- IF (x = Big_Unsigned_Last) THEN
   --   RETURN Temp;
   -- END IF;
   -- RETURN Temp MOD (x + Big_Unsigned_One)
   --
   -- If we do so, and if (x > Big_Unsigned_Last), the distribution won't be
   -- uniform, because all the numbers between 0 and (Big_Unsigned_Last - x)
   -- will have a greater probability than numbers between (Big_Unsigned_Last -
   -- x) and x.
   --
   -- So, we choose a random number for the index, between First_Indice and
   -- x.Last_Significant_Digit. Then, we loop in order to have a random number
   -- below or equal to x.
   --
   -- This algorithm is slower, but there is a better uniform distribution.
   --
   begin
      if (x.Last_Significant_Digit = First_Indice)
        and then (x.Mantissa (First_Indice) = 0)
      then
         -- If x = 0...
         return Big_Unsigned_Zero;
      end if;
      if (Last_x = First_Indice) then
         Temp_Index := First_Indice;
      else
         Temp_Index := Random (Last_x);
      end if;
      if (Temp_Index < Last_x) then
         Temp_Unsigned := Random (Temp_Index);
      else
         -- Temp_Index = Last_x
         while (Temp_Boolean = True) loop
            Temp_MyType1                        := x.Mantissa (Temp_Index);
            Temp_MyType2                        := Random (Temp_MyType1);
            Temp_Unsigned.Mantissa (Temp_Index) := Temp_MyType2;
            if (Temp_Index = First_Indice) then
               exit;
            end if;
            Temp_Boolean := (Temp_MyType1 = Temp_MyType2);
            Temp_Index   := Temp_Index - 1;
         end loop;
         if (Temp_Index > First_Indice) then
            Temp_Unsigned := Temp_Unsigned + Random (Temp_Index - 1);
         end if;
         Temp_Index := Last_x;
         while (Temp_Index > First_Indice)
           and then (Temp_Unsigned.Mantissa (Temp_Index) = 0)
         loop
            Temp_Index := Temp_Index - 1;
         end loop;
         Temp_Unsigned.Last_Significant_Digit := Temp_Index;
      end if;
      return Temp_Unsigned;
   end Random;

   function Random (x, y : in Big_Unsigned) return Big_Unsigned is
      Mini : constant Big_Unsigned := Min (x, y);
      Diff : constant Big_Unsigned := Max (x, y) - Mini;
   begin
      return Mini + Random (Diff);
   end Random;

begin
   if (Largest_Unsigned (Index_Type'Last) >= B) then
      raise Instanciation_Error;
   end if;
   Randomize;
end Big_Unsigned_Random;
