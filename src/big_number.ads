--**************************************************************
--****************
--                                                                            *
--*
--  File:        BIGNUMBE.ADS
--**
--  Description: Big_Number package specification
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
--  59000 Lille
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
--                                                                            *
--*
-- Please, don't care of my poor english in the comments (I'm french...).
--**
-- Don't hesitate to report any bug, comment, idea or improvement to my
--**
-- e-mail, which is sikander@club-internet.fr.
--**
--                                                                            *
--*
-- I tried to give a understandable name to all the functions or variables
--**
-- used in this package (the beta-test functions were initially written with
--**
-- some names in french). In case of problem with the choosen names, tell it
--**
-- to me.
--**
--                                                                            *
--*
-- You can distribute this package, but at no charge at all, and only if
--**
-- you specify where you modify it (if you do so). Don't remove the comments.
--**
--                                                                            *
--*
--*****************************************************************************
--*

with System;
with Calendar;
with Text_IO;
with Ada.Strings.Unbounded;
with UStrings;

generic
   type Index_Type is mod <>;
   Nb_Item : Index_Type;

package Big_Number is

   --
   -- This package contains two main "sub-packages", one for big unsigned
   -- integer numbers (package Unsigned_Number), another for big signed numbers
   -- (package Signed_Number).
   --
   -- Seven other "sub-packages" are defined :
   --  * Package Conversion, for the conversion of big integers, signed or
   --    unsigned : conversion of one of this type of integers to the other or
   --    to a "LONG_LONG_FLOAT" variable;
   --  * Generic package Generic_Conversion for the conversion of a predefined
   --    type of integer in ADA (one function can be used to convert a modular
   --    type) to a big integer type (signed or unsigned);
   --  * String_Conversion, for the conversion between string of numeric
   --    characters and the types Big_Signed or Big_Unsigned;
   --  * Big_Signed_IO : for the output of a Big_Signed on the screen; *
   --  Big_Unsigned_IO : for the output of a Big_Unsigned on the screen; *
   --  Big_Fraction (complete version only); * Big_Unsigned_Random (complete
   --  version only).
   --
   -- This package is generic. It allows you to use big integers, signed or
   -- unsigned.The mantissa of these numbers is represented by an array of
   -- unsigned integers (the size of these integers is defined by My_Type_Size
   --:
   -- see the note below).
   --
   -- For the instanciation of this package, Index_Type represents the type
   -- of array index, and Nb_Item represents the number of elements in this
   -- array. So you will be able to use numbers coded on My_Type_Size*Nb_Item
   -- bits. Even if Nb_Item can be 1, it should be greater or equal to 3,
   --because
   -- TypeSize is set to the half of the greatest size for an integer :
   -- if Nb_Item is equal to 1 or 2, the corresponding types are usually
   -- already defined by the compiler.
   --
   -- The elements of this array can take every value allowed for a TypeSize
   -- bit number : they are not limited to a power of 10. Some other packages
   -- could use the limitation to a power of 10 in order to facilitate the IOs
   -- of these big numbers, but this has the inconvenient to waste some memory.
   --
   -- Example of instanciation :
   --
   -- WITH ModInt;     USE ModInt;
   -- WITH BigNumber;
   --
   -- PACKAGE Big_Integers IS NEW BigNumber(Byte, 64);
   --
   -- I suppose that the type Byte is defined in the package ModInt by TYPE
   -- Byte IS MOD 2**8; FOR Byte'SIZE USE 8;
   --
   -- Then, the package Big_Integers will allow you to use big integers with a
   -- mantissa of (64 * My_Type_Size / 8) bytes.
   --
   --
   -- Remark : be careful when you instanciate this package.
   --
   --  1) Choose very well the parameters of instanciation, because each
   --     big number can occupy a lot of memory. For example, if Nb_Item =
   --8000,
   --     the mantissa of each variable of the type Big_Unsigned will
   --     take 8000 * (My_Type_Size/8) bytes (the sign is a separate field of
   --     the record).
   --  2) Ideally, Nb_Item should be greater than or equal to 3. Otherwise,
   --     the big integers managed by this package have the same size than
   --     types predefined by your compiler : there will be only one advantage,
   --     the type Fraction.
   --

   type Largest_Unsigned is mod System.Max_Binary_Modulus;

   My_Type_Size : constant := Largest_Unsigned'Size / 2;

   type My_Type is mod 2 ** My_Type_Size;
   for My_Type'Size use My_Type_Size;

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   type Mantiss is array (0 .. Nb_Item - 1) of My_Type;

   type Big_Signed is record
      Positive               : Boolean;
      Last_Significant_Digit : Index_Type;
      Mantissa               : Mantiss;
   end record;

   type Big_Unsigned is private;
   type Fraction is private;

   ------------------------------
   -- Definition of exceptions --
   ------------------------------

   Big_Number_Overflow : exception;
   -- This exception is raised is raised in case of overflow during an
   -- operation on 2 big numbers.

   Division_By_Zero : exception;
   -- This exception is raised during a tentative of division by 0 in a
   -- calculus on big numbers.

   Parameter_Error : exception;
   -- This exception is raised in case of a bad parameter (for example, a
   -- parameter below 0 with the function Factorial(x : IN Big_Signed).

   --------------------------
   -- Constant definitions --
   --------------------------

   Big_Unsigned_Zero  : constant Big_Unsigned;   -- = 0
   Big_Unsigned_One   : constant Big_Unsigned;    -- = 1
   Big_Unsigned_Two   : constant Big_Unsigned;    -- = 2
   Big_Unsigned_First : constant Big_Unsigned;  -- = 0
   Big_Unsigned_Last  : constant Big_Unsigned;   -- = "Big_Unsigned'LAST"

   Big_Signed_Zero  : constant Big_Signed;   -- = 0
   Big_Signed_One   : constant Big_Signed;    -- = 1
   Big_Signed_First : constant Big_Signed;  -- = "Big_Signed'FIRST"
   Big_Signed_Last  : constant Big_Signed;   -- = "Big_Signed'LAST"

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   package Conversion is

      function Unsigned2Signed
        (x        : in Big_Unsigned;
         Positive : in Boolean := True)
         return     Big_Signed;

      function Signed2Unsigned (x : in Big_Signed) return Big_Unsigned;

      function Big_Unsigned2Long_Long_Float
        (x    : in Big_Unsigned)
         return Long_Long_Float;

      function Big_Signed2Long_Long_Float
        (x    : in Big_Signed)
         return Long_Long_Float;

   end Conversion;

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   use Conversion;

   package Generic_Conversion is

      Conversion_Error : exception;
      -- This exception is raised if an error occurs during a conversion (for
      -- example if you try to convert an integer below 0 to a Big_Unsigned).

      -------------------------------------------------------------------------
      -------

      generic
         type Mod_Generic is mod <>;

      function Mod_Number2Big_Unsigned
        (x    : in Mod_Generic)
         return Big_Unsigned;

      -------------------------------------------------------------------------
      -------

      generic
         type Mod_Generic is mod <>;

      function Mod_Number2Big_Signed
        (x        : in Mod_Generic;
         Positive : in Boolean := True)
         return     Big_Signed;

      -------------------------------------------------------------------------
      -------

      generic
         type Int_Generic is range <>;

      function Int_Number2Big_Unsigned
        (x    : in Int_Generic)
         return Big_Unsigned;

      -------------------------------------------------------------------------
      -------

      generic
         type Int_Generic is range <>;

      function Int_Number2Big_Signed (x : in Int_Generic) return Big_Signed;

      -------------------------------------------------------------------------
      -------

   end Generic_Conversion;

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   use Conversion;

   package Unsigned_Number is

      --------------------------
      -- Exception definition --
      --------------------------

      Big_Unsigned_Substraction_Error : exception;
      -- This exception is raised when you try to compute x - y, where y is
      -- greater than x (a Big_Unsigned variable can't have a negative value).

      -----------------
      -- Comparisons --
      -----------------

      function "<" (Left, Right : in Big_Unsigned) return Boolean;
      function ">" (Left, Right : in Big_Unsigned) return Boolean;
      function "<=" (Left, Right : in Big_Unsigned) return Boolean;
      function ">=" (Left, Right : in Big_Unsigned) return Boolean;

      function Is_Null (x : in Big_Unsigned) return Boolean;

      function Min (x, y : in Big_Unsigned) return Big_Unsigned;
      function Max (x, y : in Big_Unsigned) return Big_Unsigned;

      ----------------------
      -- Common functions --
      ----------------------

      procedure Inc (x : in out Big_Unsigned);  -- Set x to x + 1.
      procedure Dec (x : in out Big_Unsigned);  -- Set x to x - 1.

      procedure Swap1 (x, y : in out Big_Unsigned);
      procedure Swap2 (x, y : in out Big_Unsigned);
      -- Swap1 and Swap2 swaps the contents of x and y. Swap2 is faster than
      -- Swap1 when x and y are slightly lower than Big_Unsigned_Last.
      -- Otherwise, you should prefer Swap1. So examine the usual conditions in
      -- your program in order choose the right function.
      function "+" (x, y : in Big_Unsigned) return Big_Unsigned;
      function "-" (x, y : in Big_Unsigned) return Big_Unsigned;
      function "*"
        (x    : in Big_Unsigned;
         y    : in My_Type)
         return Big_Unsigned;
      function "*" (x, y : in Big_Unsigned) return Big_Unsigned;
      function "**"
        (x    : in Big_Unsigned;
         y    : in Index_Type)
         return Big_Unsigned;
      function "**"
        (x    : in Big_Unsigned;
         y    : in My_Type)
         return Big_Unsigned;
      -- This function returns (x ** y). If the result is too big, the function
      -- raises the exception Big_Number_Overflow. If y = 0, this function
      -- returns Big_Unsigned_One.
      function "**" (x, y : in Big_Unsigned) return Big_Unsigned;
      -- This function returns (x ** y). If the result is too big, the function
      -- raises the exception Big_Number_Overflow. If y = Big_Unsigned_Zero,
      -- this function returns Big_Unsigned_One.
      function "**"
        (x    : in Big_Unsigned;
         y    : in Big_Signed)
         return Big_Unsigned;
      -- This function returns (x ** y). If the result is too big, the function
      -- raises the exception Big_Number_Overflow. If y = Big_Signed_Zero, this
      -- function returns Big_Unsigned_One. If y < Big_Signed_Zero, this
      -- function returns Big_Unsigned_Zero.
      function "/" (x, y : in Big_Unsigned) return Big_Unsigned;
      -- Returns x/y
      function "mod" (x, y : in Big_Unsigned) return Big_Unsigned;
      -- Returns x MOD y
      function "/"
        (x    : in Big_Unsigned;
         y    : in My_Type)
         return Big_Unsigned;
      -- Returns x/y
      function "mod" (x : in Big_Unsigned; y : in My_Type) return My_Type;
      -- Returns x MOD y
      procedure Big_Div
        (x, y            : in Big_Unsigned;
         Quot, Remainder : out Big_Unsigned);
      -- This procedure returns Quot and Remainder, so : x = Quot * y +
      -- Remainder, and Quot * y < x < (Quot + 1) * y.
      procedure Short_Div
        (x         : in Big_Unsigned;
         y         : in My_Type;
         Quot      : out Big_Unsigned;
         Remainder : out My_Type);
      -- This procedure returns Quot and Remainder, so : x = Quot * y +
      -- Remainder, and Quot * y < x < (Quot + 1) * y.

      -----------------------
      -- Special functions --
      -----------------------

      function Number_of_Bytes
        (x    : in Big_Unsigned)
         return Largest_Unsigned;
      -- This function returns the number of bytes in the mantissa of x.
      function Number_of_Bits (x : in Big_Unsigned) return Largest_Unsigned;
      -- This function returns the number of bits in the mantissa of x.

      function Square_Root (x : in Big_Unsigned) return Big_Unsigned;
      function Factorial (x : in Big_Unsigned) return Big_Unsigned;
      -- This function returns x!.
      function GCF (x, y : in Big_Unsigned) return Big_Unsigned;
      -- This function returns the greatest common factor of x and y.
      function LCM (x, y : in Big_Unsigned) return Big_Unsigned;
      -- This function returns the least common factor of x and y.

      function "and" (x, y : in Big_Unsigned) return Big_Unsigned;
      -- This function returns x and y.
      function "xor" (x, y : in Big_Unsigned) return Big_Unsigned;
      -- This function returns x xor y.

      function NCR (N, R : in Big_Unsigned) return Big_Unsigned;
      -- NCR = N! / [R! * (N-R)!]
      function NPR (N, R : in Big_Unsigned) return Big_Unsigned;
      function K (N, P : in Big_Unsigned) return Big_Unsigned;
      -- K(N, P) = NCR(N+P-1,P)

   end Unsigned_Number;

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   use Conversion;

   package Signed_Number is

      -----------------
      -- Comparisons --
      -----------------

      function "<" (Left, Right : in Big_Signed) return Boolean;
      function ">" (Left, Right : in Big_Signed) return Boolean;
      function "<=" (Left, Right : in Big_Signed) return Boolean;
      function ">=" (Left, Right : in Big_Signed) return Boolean;

      function Is_Null (x : in Big_Signed) return Boolean;

      function Min (x, y : in Big_Signed) return Big_Signed;
      function Max (x, y : in Big_Signed) return Big_Signed;

      ----------------------
      -- Common functions --
      ----------------------

      procedure Inc (x : in out Big_Signed);  -- Set x to x + 1.
      procedure Dec (x : in out Big_Signed);  -- Set x to x - 1.

      procedure Swap1 (x, y : in out Big_Signed);
      procedure Swap2 (x, y : in out Big_Signed);
      -- Swap1 and Swap2 swaps the contents of x and y.
      -- Swap2 is faster than Swap1 when the absolute value of x and y are
      -- slightly lower than Big_Signed_Last. Otherwise, you should prefer
      --Swap1.
      -- So examine the usual conditions in your program in order
      -- choose the right function.
      function "+" (x, y : in Big_Signed) return Big_Signed;
      function "-" (x, y : in Big_Signed) return Big_Signed;
      function "*" (x, y : in Big_Signed) return Big_Signed;
      function "**"
        (x    : in Big_Signed;
         y    : in Index_Type)
         return Big_Signed;
      function "**" (x : in Big_Signed; y : in My_Type) return Big_Signed;
      -- This function returns (x ** y). If the result is too big, the function
      -- raises the exception Big_Number_Overflow. If y = 0, this function
      -- returns Big_Signed_One. By definition, Index_Type is a modular type,
      -- so y can't be negative.
      function "**"
        (x    : in Big_Signed;
         y    : in Big_Unsigned)
         return Big_Signed;
      -- This function returns (x ** y). If the result is too big, the function
      -- raised the exception Big_Number_Overflow. If y = Big_Unsigned_Zero,
      -- this function returns Big_Signed_One. By definition, y can't be
      -- negative.
      function "**" (x, y : in Big_Signed) return Big_Signed;
      -- This function returns (x ** y) if y is positive. If y is < 0, the
      -- function returns Big_Unsigned_Zero. If y = Big_Signed_Zero, this
      -- function returns Big_Signed_One; If the result is too big, the
      -- function raises the exception Big_Number_Overflow.
      function "/" (x, y : in Big_Signed) return Big_Signed;
      function "mod" (x, y : in Big_Signed) return Big_Signed;
      -- This function returns a number, always positive, so that (x - Result)
      -- is an exact multiple of y.
      function "/" (x : in Big_Signed; y : in My_Type) return Big_Signed;
      function "mod" (x : in Big_Signed; y : in My_Type) return My_Type;
      -- This function returns a number, always positive, so that (x - Result)
      -- is an exact multiple of y.
      function Opposite (x : in Big_Signed) return Big_Signed;
      -- Returns -x.
      function Absolute (x : in Big_Signed) return Big_Signed;
      -- Returns -x if x < 0.
      -- Returns x if x >=0.
      procedure Big_Div
        (x, y            : in Big_Signed;
         Quot, Remainder : out Big_Signed);
      -- This procedure returns Quot and Remainder so that : x = Quot * y +
      -- Remainder, and Absolute(Quot * y) < Absolute(x) < Absolute(Quot + 1) *
      -- y)

      -----------------------
      -- Special functions --
      -----------------------

      function Number_of_Bytes (x : in Big_Signed) return Largest_Unsigned;
      -- This function returns the number of bytes in the mantissa of x.
      function Number_of_Bits (x : in Big_Signed) return Largest_Unsigned;
      -- This function returns the number of bits in the mantissa of x.

      function Square_Root (x : in Big_Signed) return Big_Signed;
      function Factorial (x : in Big_Signed) return Big_Signed;
      -- This function returns x!.
      -- If x is strictly lower than 0, the exception Parameter_Error is
      --raised.
      function GCF (x, y : in Big_Signed) return Big_Signed;
      -- This function returns the greatest common factor of x and y.
      function LCM (x, y : in Big_Signed) return Big_Signed;
      -- This function returns the least common factor of x and y.

   end Signed_Number;

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   use Generic_Conversion;
   use Unsigned_Number;
   use Ada.Strings.Unbounded;
   use UStrings;

   package String_Conversion is

      Conversion_Error : exception;
      -- This exception is raised if an error occurs during a conversion.

      Base_Error : exception;
      -- This exception is raised when the base parameter is greater than 36,
      -- or when this parameter is lower than 2.

      function String2Unsigned (x : in String) return Big_Unsigned;
      function String2Signed (x : in String) return Big_Signed;
      -- These two functions convert an integer in base 10, given as a string,
      -- respectively in a Big_Unsigned or a Big_Signed.

      function UString2Unsigned
        (x    : in Unbounded_String)
         return Big_Unsigned;
      function UString2Signed (x : in Unbounded_String) return Big_Signed;
      -- These two functions convert an integer in base 10, given as a string,
      -- respectively in a Big_Unsigned or a Big_Signed.

      function Big_Unsigned2UString
        (x    : in Big_Unsigned;
         Base : in My_Type := 10)
         return Unbounded_String;
      -- This function returns the UNBOUNDED_STRING corresponding to the
      -- Big_Unsigned number x, using the base Base. By default, Base = 10.
      function Big_Signed2UString
        (x    : in Big_Signed;
         Base : in My_Type := 10)
         return Unbounded_String;
      -- This function returns the UNBOUNDED_STRING corresponding to the
      -- Big_Signed number x, using the base Base. By default, Base = 10.

   end String_Conversion;

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   use Unsigned_Number;
   use String_Conversion;
   use Text_IO;
   use UStrings;

   package Big_Unsigned_IO is

      procedure GET_LINE (x : out Big_Unsigned);
      -- This procedure waits for a Big_Unsigned, to be entered in base 10 on
      --the
      -- keyboard.
      procedure GET_LINE (File : in File_Type; x : out Big_Unsigned);
      -- This procedure reads a Big_Unsigned in a text file.

      procedure PUT (x : in Big_Unsigned; Base : in My_Type := 10);
      -- This procedure writes on screen the variable x, using the indicated
      -- base (by default, it's base 10).
      procedure PUT
        (File : in File_Type;
         x    : in Big_Unsigned;
         Base : in My_Type := 10);
      -- This procedure writes the variable x in a text file, using the
      -- indicated base (by default, it's base 10).

      procedure PUT_LINE (x : in Big_Unsigned; Base : in My_Type := 10);
      -- This procedure writes on screen the variable x, using the
      -- indicated base (by default, it's base 10), and then go to the next
      --line.
      procedure PUT_LINE
        (File : in File_Type;
         x    : in Big_Unsigned;
         Base : in My_Type := 10);
      -- This procedure writes the variable x in a text file, using the
      -- indicated base (by default, it's base 10), and then go to the next
      --line.

   end Big_Unsigned_IO;

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   use Signed_Number;
   use String_Conversion;
   use Text_IO;
   use UStrings;

   package Big_Signed_IO is

      procedure GET_LINE (x : out Big_Signed);
      -- This procedure waits a Big_Signed, to be entered in base 10 on the
      -- keyboard.
      procedure GET_LINE (File : in File_Type; x : out Big_Signed);
      -- This procedure reads a Big_signed in a text file.

      procedure PUT (x : in Big_Signed; Base : in My_Type := 10);
      -- This procedure writes on screen the variable x, using the indicated
      -- base (by default, it's base 10).
      procedure PUT
        (File : in File_Type;
         x    : in Big_Signed;
         Base : in My_Type := 10);
      -- This procedure writes the variable x in a text file, using the
      -- indicated base (by default, it's base 10).

      procedure PUT_LINE (x : in Big_Signed; Base : in My_Type := 10);
      -- This procedure writes on screen the variable x, using the
      -- indicated base (by default, it's base 10), and then go to the next
      --line.
      procedure PUT_LINE
        (File : in File_Type;
         x    : in Big_Signed;
         Base : in My_Type := 10);
      -- This procedure writes the variable x in a text file, using the
      -- indicated base (by default, it's base 10), and then go to the next
      --line.

   end Big_Signed_IO;

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   use Unsigned_Number;
   use Big_Unsigned_IO;
   use Ada.Strings.Unbounded;
   use UStrings;

   package Big_Fraction is

      -- Remark : for all the operations on fractions, the function or the
      --procedure
      -- return the simplified form of the result. For example, 1/4 + 1/4 =
      --2/4,
      -- but the function "+" returns 1/2 (2/4 = 1/2). The simplification of
      --the
      -- result requires some more execution time, but it allows to avoid some
      -- overflows.

      Fraction_Overflow : exception;

      function CreateFraction
        (Numerator, Denominator : in Big_Unsigned;
         Positive               : in Boolean := True)
         return                   Fraction;
      function CreateFraction
        (Numerator, Denominator : in Big_Signed)
         return                   Fraction;

      function Get_Numerator (x : in Fraction) return Big_Unsigned;
      function Get_Denominator (x : in Fraction) return Big_Unsigned;
      function Is_Positive (x : in Fraction) return Boolean;

      function Inverse_Fraction (x : in Fraction) return Fraction;
      -- This function returns 1/x.

      -- No comments for the following functions...
      function "*" (x, y : in Fraction) return Fraction;
      function "/" (x, y : in Fraction) return Fraction;
      function "+" (x, y : in Fraction) return Fraction;
      function "-" (x, y : in Fraction) return Fraction;
      function "**" (x : in Fraction; y : in Big_Unsigned) return Fraction;
      function "**" (x : in Fraction; y : in Big_Signed) return Fraction;
      function "**" (x : in Fraction; y : in Index_Type) return Fraction;
      function "**" (x : in Fraction; y : in My_Type) return Fraction;
      function "*" (x : in Fraction; y : in Big_Unsigned) return Fraction;
      function "*" (x : in Fraction; y : in Big_Signed) return Fraction;
      function "/" (x : in Fraction; y : in Big_Unsigned) return Fraction;
      function "/" (x : in Fraction; y : in Big_Signed) return Fraction;

      function "<" (Left, Right : in Fraction) return Boolean;
      function ">" (Left, Right : in Fraction) return Boolean;
      function "<=" (Left, Right : in Fraction) return Boolean;
      function ">=" (Left, Right : in Fraction) return Boolean;

      function Opposite (x : in Fraction) return Fraction;
      -- Returns -x.
      function Absolute (x : in Fraction) return Fraction;
      -- Returns -x if x < 0.
      -- Returns x if x >=0.
      function Is_Null (x : in Fraction) return Boolean;

      procedure PUT (x : in Fraction);
      -- This procedure puts x on the screen.
      procedure PUT_LINE (x : in Fraction);
      -- This procedure puts x on the screen and then go to the next line.

      procedure PUT (File : in File_Type; x : in Fraction);
      -- This procedure puts x in a text file.
      procedure PUT_LINE (File : in File_Type; x : in Fraction);
      -- This procedure puts x in a text file and then go to the next line.

      function UString2Fraction (x : in Unbounded_String) return Fraction;

      procedure GET_LINE (x : out Fraction);
      -- This procedure gets x and then go to the next line. The exception
      -- Parameter_Error is raised in case of error (for example if there are 2
      -- or more '/').
      procedure GET_LINE (File : in File_Type; x : out Fraction);
      -- This procedure gets x in a text file and then go to the next line. The
      -- exception Parameter_Error is raised in case of error (for example if
      -- there are 2 or more '/').

      procedure Float_in_UString2Fraction
        (x          : in Unbounded_String;
         Nb_Periods : in Positive;
         Result     : out Fraction;
         Error      : out Boolean);
      -- This procedure tries to get the fraction corresponding to the float
      -- point number contained in the string x. INPUT :
      --    - x : the string
      --    - Nb_Periods :  the minimal number of periods to find in the string
      --                    if you want the function to consider that number
      --as a
      --                    fraction. For example, the string "5.345345345345"
      --                    contains the period "345" four times.
      --                    The string "5.3453453453457" is considered to
      --contain
      --                    no period, even if there are 4 repetitions of
      --"345",
      --                    because these repetitions are followed by "7",
      --which
      --                    DOES NOT belong to this sequence.
      --
      -- OUTPUT :
      --    - Result : the fraction (only if Error = FALSE)
      --    - Error : TRUE if x isn't a valid string or if Nb_Periods = 1,
      --              FALSE otherwise.
      --
      -- An valid string is : "(+/-)nnn.nnnnn[(E:e(+/-))nnnnn]".
      -- The mantissa is composed of any decimal digit and may have only one
      --sign
      -- at the beginning. The mantissa may be a real number.
      -- If there is an exponent, it must start with E or e, may have only one
      -- sign and MUST be an integer.
      -- In the mantissa or in the exponent, the spaces and the character "_"
      -- are accepted, and they will have no consequence concerning the result.
      --
      -- Remarks :
      -- 1) If Nb_Periods = 4, 1.353535 (3 repetitions of "35") will be
      --considered
      --    as the exact value of the fraction 270_707 / 200_000, but with the
      --    number 1.35353535 (which has 4 repetitions of "35"), the result
      --will be
      --    the fraction 134/99.
      -- 2) The parameter Nb_Periods must always be equal or greater than 2.
      --    There is no upper limit for Nb_Periods.
      -- 3) Be careful : if there is an error and if you don't check it
      --    (by regarding the boolean returned by the procedure), you
      --    may have a DIVISION_BY_ZERO exception in a further operation,
      --    because, in this case, the function sets the result to 0/0.
      --    That was not done to plague you, but just to help you to detect
      --    errors in your programs : I think that, during the tests,
      --    it's better to have an exception than a bad result which you may
      --    never detect. Furthermore, exceptions can be handled.
      --

   end Big_Fraction;

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   use Calendar;
   use Unsigned_Number;

   package Big_Unsigned_Random is

      Instanciation_Error : exception;
      -- This exception is raised when Index_Type'LAST > (2**TypeSize - 1) /
      --125.
      -- It is due to the implementation : in the package Big_Unsigned_Random,
      -- ForMultiplication'LAST / 125 must be greater than Index_Type'LAST.

      procedure Randomize;
      -- This procedure resets the pseudo-random number generator. It is
      -- automatically executed at the start of the program.

      function Random return Big_Unsigned;
      -- This function returns a Big_Unsigned between Big_Unsigned_Zero and
      -- Big_Unsigned_Last.

      function Random (x : in Big_Unsigned) return Big_Unsigned;
      -- This function returns a Big_Unsigned between Big_Unsigned_Zero and x.
      -- If x = Big_Unsigned_Zero, it returns Big_Unsigned_Zero.

      function Random (x, y : in Big_Unsigned) return Big_Unsigned;
      -- This function returns a Big_Unsigned between Min(x,y) and Max(x,y).

   end Big_Unsigned_Random;

   --**************************************************************************
   --****
   --**************************************************************************
   --****
   --**************************************************************************
   --****
   --**************************************************************************
   --****

private
   type Big_Unsigned is record
      Last_Significant_Digit : Index_Type;
      Mantissa               : Mantiss;
   end record;

   type Fraction is record
      Positive    : Boolean;
      Numerator   : Big_Unsigned;
      Denominator : Big_Unsigned;
   end record;

   --
   -- By convention, the lower digit is in Mantissa(0), and the greater one is
   -- in Mantissa(Nb_Elt-1). This looks like a "BigEndian" coding.
   --

   Big_Unsigned_Zero  : constant Big_Unsigned :=
     (Last_Significant_Digit => 0,
      Mantissa               => (others => My_Type (0)));
   Big_Unsigned_One   : constant Big_Unsigned :=
     (Last_Significant_Digit => 0,
      Mantissa               => (1, others => My_Type (0)));
   Big_Unsigned_Two   : constant Big_Unsigned :=
     (Last_Significant_Digit => 0,
      Mantissa               => (2, others => My_Type (0)));
   Big_Unsigned_First : constant Big_Unsigned := Big_Unsigned_Zero;
   Big_Unsigned_Last  : constant Big_Unsigned :=
     (Last_Significant_Digit => Nb_Item - 1,
      Mantissa               => (others => My_Type'Last));

   Big_Signed_Zero  : constant Big_Signed :=
     (Positive               => True,
      Last_Significant_Digit => 0,
      Mantissa               => (others => My_Type (0)));
   Big_Signed_One   : constant Big_Signed :=
     (Positive               => True,
      Last_Significant_Digit => 0,
      Mantissa               => (1, others => My_Type (0)));
   Big_Signed_First : constant Big_Signed :=
     (Positive               => False,
      Last_Significant_Digit => Nb_Item - 1,
      Mantissa               => (others => My_Type'Last));
   Big_Signed_Last  : constant Big_Signed :=
     (Positive               => True,
      Last_Significant_Digit => Nb_Item - 1,
      Mantissa               => (others => My_Type'Last));

   --**************************************************************************
   --****
   --**************************************************************************
   --****

end Big_Number;
