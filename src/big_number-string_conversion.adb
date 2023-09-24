--******************************************************
--************************
--                                                                            *
--*
--  File:        BINUSTCO.ADB
--**
--  Description: Big_Number.String_Conversion package body
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
package body String_Conversion is

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   subtype Numeric_Digit is Character range '0' .. '9';

   --**************************************************************************
   --****
   --**************************************************************************
   --****
   --**           Conversion of strings to Big_Unsigned or Big_Signed
   --**
   --**************************************************************************
   --****
   --**************************************************************************
   --****

   function String_Size_Required (x : in String) return Natural is
      Temp_Size : Natural           := 0;
      x_First   : constant Positive := x'First;
      Char      : Character;
   begin
      for I in  x'Range loop
         Char := x (I);
         if not (Char in Numeric_Digit) then
            if ((Char = '-') or else (Char = '+')) then
               -- A sign is authorized only for the first character
               if (I /= x_First) then
                  raise Conversion_Error;
               end if;
            elsif (Char /= ' ') and then (Char /= '_') then
               raise Conversion_Error;
            end if;
         else
            -- Now, Char is in '0'..'9'.
            Temp_Size := Temp_Size + 1;
         end if;
      end loop;
      return Temp_Size;
   exception
      when others =>
         raise Conversion_Error;
   end String_Size_Required;

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   function Str2Unsigned (x : in String) return Big_Unsigned is
      Temp_Char  : Character;
      Temp_Digit : Numeric_Digit;
      Temp       : Big_Unsigned     := Big_Unsigned_Zero;
      Pos0       : constant Integer := Numeric_Digit'Pos ('0');
      function Integer2BU is new Int_Number2Big_Unsigned (Integer);
   begin
      for I in  x'Range loop
         Temp_Char  := x (I);
         Temp_Digit := Numeric_Digit (Temp_Char);
         Temp       := (Temp * 10) +
                       Integer2BU (Numeric_Digit'Pos (Temp_Digit) - Pos0);
      end loop;
      return Temp;
   exception
      when others =>
         raise Conversion_Error;
   end Str2Unsigned;

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   procedure Del_Spaces_And_Underscore (x : in String; y : out String) is
      Dernier   : constant Positive := x'Last;
      J         : Positive          := y'First;
      Temp_Char : Character;
   begin
      for I in  x'Range loop
         Temp_Char := x (I);
         if (Temp_Char /= ' ') and then (Temp_Char /= '_') then
            y (J) := Temp_Char;
            J     := J + 1;
         end if;
      end loop;
   end Del_Spaces_And_Underscore;

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   function String2Unsigned (x : in String) return Big_Unsigned is
      String_Size                 : Natural;
      Temp_Positive, Sign_Present : Boolean;
   begin
      String_Size := String_Size_Required (x);
      if (String_Size = 0) then
         return Big_Unsigned_Zero;
      end if;
      declare
         First_Char : constant Character := x (x'First);
      begin
         Sign_Present  := (First_Char = '+') or else (First_Char = '-');
         Temp_Positive := (First_Char = '+')
                         or else (First_Char in Numeric_Digit);
      end;
      if (Temp_Positive = False) then
         raise Conversion_Error;
      end if;
      declare
         Temp_String : String (1 .. Positive (String_Size));
      begin
         if (Sign_Present = True) then
            Del_Spaces_And_Underscore
              (x (x'First + 1 .. x'Last),
               Temp_String);
         else
            Del_Spaces_And_Underscore (x, Temp_String);
         end if;
         return Str2Unsigned (Temp_String);
      end;
   exception
      when others =>
         raise Conversion_Error;
   end String2Unsigned;

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   function String2Signed (x : in String) return Big_Signed is
      String_Size   : Natural;
      Sign_Present  : Boolean;
      Temp_Positive : Boolean;
   begin
      String_Size := String_Size_Required (x);
      if (String_Size = 0) then
         return Big_Signed_Zero;
      end if;
      declare
         First_Char : constant Character := x (x'First);
      begin
         Sign_Present  := (First_Char = '+') or else (First_Char = '-');
         Temp_Positive := (First_Char = '+')
                         or else (First_Char in Numeric_Digit);
      end;
      declare
         Temp_String : String (1 .. Positive (String_Size));
      begin
         if (Sign_Present = True) then
            Del_Spaces_And_Underscore
              (x (x'First + 1 .. x'Last),
               Temp_String);
         else
            Del_Spaces_And_Underscore (x, Temp_String);
         end if;
         return Unsigned2Signed (Str2Unsigned (Temp_String), Temp_Positive);
      end;
   exception
      when others =>
         raise Conversion_Error;
   end String2Signed;

   --**************************************************************************
   --****
   --**************************************************************************
   --****
   --**       Conversion of unbounded strings to Big_Unsigned or Big_Signed
   --**
   --**************************************************************************
   --****
   --**************************************************************************
   --****

   function UStr2Unsigned (x : in Unbounded_String) return Big_Unsigned is
      Temp_Char  : Character;
      Temp_Digit : Numeric_Digit;
      Temp       : Big_Unsigned     := Big_Unsigned_Zero;
      Pos0       : constant Integer := Numeric_Digit'Pos ('0');
      function Integer2BU is new Int_Number2Big_Unsigned (Integer);
   begin
      for I in  1 .. Length (x) loop
         Temp_Char  := Element (x, I);
         Temp_Digit := Numeric_Digit (Temp_Char);
         Temp       := (Temp * 10) +
                       Integer2BU (Numeric_Digit'Pos (Temp_Digit) - Pos0);
      end loop;
      return Temp;
   exception
      when others =>
         raise Conversion_Error;
   end UStr2Unsigned;

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   function UString2Unsigned (x : in Unbounded_String) return Big_Unsigned is
      Positive, Sign_Present : Boolean;
      Temp_x                 : Unbounded_String := Del_Spaces (x);
      Size1, Size2           : Integer;
   begin
      Temp_x := Del_Character (Temp_x, '_');
      if (Length (Temp_x) = 0) then
         return Big_Unsigned_Zero;
      end if;
      declare
         First_Char : constant Character := Element (x, 1);
      begin
         Sign_Present := (First_Char = '+') or else (First_Char = '-');
         Positive     := (First_Char = '+')
                        or else (First_Char in Numeric_Digit);
      end;
      if ((Sign_Present = True) and then (Length (Temp_x) = 1))
         -- If x contains only a sign (+ or -)
        or else (Positive = False)
      then
         -- or if the number is negative, this string can't be converted to a
         -- Big_Unsigned.
         raise Conversion_Error;
      end if;
      -- Now, the number is positive.
      Size1  := Length (Temp_x);
      Temp_x := Del_Character (Temp_x, '+');
      Temp_x := Del_Character (Temp_x, '-');
      Size2  := Length (Temp_x);
      if (Size1 - Size2 > 1) then
         -- If there are 2 or more signs in Temp_x...
         raise Conversion_Error;
      end if;
      return UStr2Unsigned (Temp_x);
   exception
      when others =>
         raise Conversion_Error;
   end UString2Unsigned;

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   function UString2Signed (x : in Unbounded_String) return Big_Signed is
      Positive, Sign_Present : Boolean;
      Temp_x                 : Unbounded_String := Del_Spaces (x);
      Size1, Size2           : Integer;
   begin
      Temp_x := Del_Character (Temp_x, '_');
      if (Length (Temp_x) = 0) then
         return Big_Signed_Zero;
      end if;
      declare
         First_Char : constant Character := Element (x, 1);
      begin
         Sign_Present := (First_Char = '+') or else (First_Char = '-');
         Positive     := (First_Char = '+')
                        or else (First_Char in Numeric_Digit);
      end;
      if ((Sign_Present = True) and then (Length (Temp_x) = 1)) then
         -- If x contains only a sign (+ or -), this string can't be converted
         -- to a Big_Unsigned.
         raise Conversion_Error;
      end if;
      -- Now, the sign is known.
      Size1  := Length (Temp_x);
      Temp_x := Del_Character (Temp_x, '+');
      Temp_x := Del_Character (Temp_x, '-');
      Size2  := Length (Temp_x);
      if (Size1 - Size2 > 1) then
         -- If there are 2 or more signs in Temp_x...
         raise Conversion_Error;
      end if;
      return Unsigned2Signed (UStr2Unsigned (Temp_x), Positive);
   exception
      when others =>
         raise Conversion_Error;
   end UString2Signed;

   --**************************************************************************
   --****
   --**************************************************************************
   --****
   --**      Conversion of Big_Unsigned or Big_Signed to unbounded strings
   --**
   --**************************************************************************
   --****
   --**************************************************************************
   --****

   function Big_Unsigned2UString
     (x    : in Big_Unsigned;
      Base : in My_Type := 10)
      return Unbounded_String
   is
      type Trans is array (My_Type range 0 .. 35) of Character;
      Table     : constant Trans   :=
        ('0',
         '1',
         '2',
         '3',
         '4',
         '5',
         '6',
         '7',
         '8',
         '9',
         'A',
         'B',
         'C',
         'D',
         'E',
         'F',
         'G',
         'H',
         'I',
         'J',
         'K',
         'L',
         'M',
         'N',
         'O',
         'P',
         'Q',
         'R',
         'S',
         'T',
         'U',
         'V',
         'W',
         'X',
         'Y',
         'Z');
      Result    : Unbounded_String := Null_Unbounded_String;
      Temp_x    : Big_Unsigned     := x;
      Remainder : My_Type;
   begin
      if (Base > 35) or else (Base < 2) then
         raise Base_Error;
      end if;
      if (Temp_x.Last_Significant_Digit = First_Indice)
        and then (Temp_x.Mantissa (First_Indice) = 0)
      then
         Result := To_Unbounded_String ("0");
         return Result;
      end if;
      while (Temp_x /= Big_Unsigned_Zero) loop
         Short_Div (Temp_x, Base, Temp_x, Remainder);
         Result := Table (Remainder) & Result;
      end loop;
      return Result;
   end Big_Unsigned2UString;

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   function Big_Signed2UString
     (x    : in Big_Signed;
      Base : in My_Type := 10)
      return Unbounded_String
   is
      Temp : constant Big_Unsigned := Signed2Unsigned (x);
   begin
      if (x.Positive = False) then
         return "-" & Big_Unsigned2UString (Temp, Base);
      else
         return Big_Unsigned2UString (Temp, Base);
      end if;
   end Big_Signed2UString;

   --**************************************************************************
   --****
   --**************************************************************************
   --****

end String_Conversion;
