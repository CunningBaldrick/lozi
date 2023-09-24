--**************************************************************
--****************
--                                                                            *
--*
--  File:        BINUGECO.ADB
--**
--  Description: Big_Number.Generic_Conversion package body
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
--  Copyright (c) J‚r“me Delcourt, 1998, 1999, 2000
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
package body Generic_Conversion is

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   function Mod_Number2Big_Unsigned
     (x    : in Mod_Generic)
      return Big_Unsigned
   is
      Result : Big_Unsigned := Big_Unsigned_Zero;
      function Shift_Right
        (Value  : in Mod_Generic;
         Amount : in Natural)
         return   Mod_Generic;
      pragma Import (Intrinsic, Shift_Right);
   begin
      if (Mod_Generic'Size <= My_Type_Size) then
         -- No problem : My_Type'LAST >= Mod_Generic'LAST. Note that
         -- Result.Last_Significant_Digit was already initialized to
         -- First_Indice, since Result was initialized to Big_Unsigned_Zero.
         Result.Mantissa (First_Indice) := My_Type (x);
         return Result;
      end if;
      -- Now, we must check the case where the size of x is bigger than the
      -- size of My_Type (note : the mantisse of a Big_Unsigned variable is
      -- coded by an array of My_Type elements).
      declare
         Remainder : Mod_Generic := x;
         Index     : Index_Type  := First_Indice;
      begin
         loop
            Result.Mantissa (Index) :=
              My_Type (Remainder and Mod_Generic (My_Type'Last));
            -- Note that My_Type'LAST = Base - 1
            Remainder := Shift_Right (Remainder, My_Type_Size);
            exit when (Remainder = 0) or else (Index = Last_Indice);
            Index := Index + 1;
         end loop;
         if (Remainder > 0) then
            -- You must think of a stupid situation, where the total number of
            --bits
            -- in the mantissa of the result is smaller than the number of
            --significant
            -- bits of x.
            -- [It doesn't worth to use this package, which may be buggy, if
            --the ADA
            -- compiler can provide a type (predefined or defined by a user
            --modular
            -- type) with more bits in the mantissa and with bug-free related
            -- functions.]
            raise Conversion_Error;
         end if;
         Result.Last_Significant_Digit := Index;
         return Result;
      end;
   end Mod_Number2Big_Unsigned;

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   function Mod_Number2Big_Signed
     (x        : in Mod_Generic;
      Positive : in Boolean := True)
      return     Big_Signed
   is
      function My_Conversion is new Mod_Number2Big_Unsigned (Mod_Generic);
      Result : Big_Signed;
   begin
      Result := Unsigned2Signed (My_Conversion (x), Positive);
      if (x = 0) then
         Result.Positive := True;
      end if;
      return Result;
   end Mod_Number2Big_Signed;

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   function Int_Number2Big_Unsigned
     (x    : in Int_Generic)
      return Big_Unsigned
   is
      Result : Big_Unsigned := Big_Unsigned_Zero;
      function Shift_Right
        (Value  : in Int_Generic;
         Amount : in Natural)
         return   Int_Generic;
      pragma Import (Intrinsic, Shift_Right);
   begin
      if (x < 0) then
         raise Conversion_Error;
      elsif (Int_Generic'Size <= My_Type_Size + 1) then
         -- No problem : My_Type'LAST > Int_Generic'LAST, because, for integer
         --types,
         -- there is one bit reserved to store the sign.
         Result.Mantissa (First_Indice) := My_Type (x);
         return Result;
      end if;
      -- Now, we must check the case where the size of x is bigger than the
      -- size of My_Type (note : the mantissa of a Big_Unsigned variable is
      -- coded by an array of My_Type elements).
      declare
         pragma Warnings (Off);
         TmpBase : constant Int_Generic := Int_Generic (Base);
         pragma Warnings (On);
         -- Remark : Base = My_Type'LAST + 1; Here, Int_Generic'LAST >
         -- My_Type'LAST, So, Int_Generic'LAST >= Base
         Remainder : Int_Generic := x;
         Index     : Index_Type  := First_Indice;
      begin
         loop
            Result.Mantissa (Index) := My_Type (Remainder mod TmpBase);
            Remainder               := Shift_Right (Remainder, My_Type_Size);
            exit when (Remainder = 0) or else (Index = Last_Indice);
            Index := Index + 1;
         end loop;
         if (Remainder > 0) then
            -- You must think of a stupid situation, where the total number of
            --bits
            -- in the mantisse of the result is smaller than the number of
            --significant
            -- bits of x.
            -- [It doesn't worth to use this package, which may be buggy, if
            --the ADA
            -- compiler can provide a type (predefined or defined by a user
            --modular
            -- type) with more bits in the mantissa and with bug-free related
            -- functions.]
            raise Conversion_Error;
         end if;
         Result.Last_Significant_Digit := Index;
         return Result;
      end;
   end Int_Number2Big_Unsigned;

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   function Int_Number2Big_Signed (x : in Int_Generic) return Big_Signed is
      function My_Conversion is new Int_Number2Big_Unsigned (Int_Generic);
   begin
      return Unsigned2Signed (My_Conversion (abs (x)), x >= 0);
   end Int_Number2Big_Signed;

   --**************************************************************************
   --****
   --**************************************************************************
   --****

end Generic_Conversion;
