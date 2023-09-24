with Interfaces.Fortran; use Interfaces.Fortran;

--  Calculate an extremal eigenvalue/eigenvector pair for a sparse matrix
--  using implicitly restarted Arnoldi iteration.  The matrix must be at
--  least 3x3.
package Arnoldi is
   pragma Elaborate_Body;

   Arnoldi_Error : exception;

   type Vector is array (Fortran_Integer range <>) of Double_Precision;
   pragma Convention (Fortran, Vector);

   generic
      with procedure Iterate (
        Source : in     Vector;
        Target :    out Vector
      );
      --  Apply the matrix to Source and store the result in Target.
      --  The values of Source'First and Target'First may vary on each call.
   procedure Extremal_Eigenvector (
     First, Last    : in     Fortran_Integer; -- vector index range
     Real_Part      :    out Vector;
     Imaginary_Part :    out Vector;
     Eigenvalue_R_P :    out Double_Precision;
     Eigenvalue_I_P :    out Double_Precision;
     Tolerance      : in     Double_Precision
   );
end Arnoldi;
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
procedure Calculate_Entropy;
--  Calculate entropy given A_Numerator, A_Denominator, B_Numerator and
--  B_Denominator on the command line.
--  Entropy is calculated using natural logarithms.
with Ada.Numerics.Generic_Complex_Types;
with Interfaces.Fortran;

package Fortran_Complex_Types
  is new Ada.Numerics.Generic_Complex_Types (Interfaces.Fortran.Double_Precision);
with Multi_precision_integers; use Multi_precision_integers;

pragma Elaborate_All (Multi_precision_integers);

package Gautier_Support is
   pragma Elaborate_Body;

   function Multi (small: Basic_int; n: Index_int) return Multi_int;
end Gautier_Support;
function GCD (Left, Right : Integer) return Integer;
--  Manipulate the rounding mode used for IEEE arithmetic.
package IEEE is
   pragma Elaborate_Body;

   Unsupported_Mode : exception;

   type Rounding_Mode is (To_Nearest, Upwards, Downwards, Towards_Zero);

   function Get_Rounding_Mode return Rounding_Mode;

   procedure Set_Rounding_Mode (Mode : Rounding_Mode);
end IEEE;
package Integers.IO is
   pragma Elaborate_Body;

   procedure Put (Source : Integer_Type);
end Integers.IO;
with Big_Number;

pragma Elaborate_All (Big_Number);

package Integers is
   pragma Elaborate_Body;

   type Integer_Type is private;

   Zero : constant Integer_Type; -- Additive identity.
   One  : constant Integer_Type; -- Multiplicative identity.

   function To_Integer_Type (X : Integer) return Integer_Type;
   pragma Inline (To_Integer_Type);

   function "="  (Left, Right : Integer_Type) return Boolean;
   function "<"  (Left, Right : Integer_Type) return Boolean;
   function "<=" (Left, Right : Integer_Type) return Boolean;
   function ">"  (Left, Right : Integer_Type) return Boolean;
   function ">=" (Left, Right : Integer_Type) return Boolean;

   function Sign (Right : Integer_Type) return Integer;
   --  Returns -1, 0 or 1 according to whether Right < 0, = 0, or > 0

   function Is_Negative (Right : Integer_Type) return Boolean;
   --  Equivalent to "<" (Right, Zero), but more efficient.

   function Is_Positive (Right : Integer_Type) return Boolean;
   --  Equivalent to ">" (Right, Zero), but more efficient.

   function Is_Zero (Right : Integer_Type) return Boolean;
   --  Equivalent to "=" (Right, Zero), but more efficient.

   function Negate (Right : Integer_Type) return Integer_Type;
   --  Equivalent to "-" (Zero, Right), but more efficient.

   function "abs" (Right : Integer_Type) return Integer_Type;

   function "+"  (Left, Right : Integer_Type) return Integer_Type;
   function "-"  (Left, Right : Integer_Type) return Integer_Type;
   function "*"  (Left, Right : Integer_Type) return Integer_Type;
   function "/"  (Left, Right : Integer_Type) return Integer_Type;

   function "rem"  (Left, Right : Integer_Type) return Integer_Type;

   function GCD (Left, Right : Integer_Type) return Integer_Type;
private
   type Index_Type is mod 2**8;
   for Index_Type'Size use 8;

   package Big_Numbers is new Big_Number (
     Index_Type,
     128
   );

   type Integer_Type is new Big_Numbers.Big_Signed;

   Zero : constant Integer_Type := Integer_Type (Big_Numbers.Big_Signed_Zero);
   One  : constant Integer_Type := Integer_Type (Big_Numbers.Big_Signed_One);

   pragma Inline ("=");
   pragma Inline ("<");
   pragma Inline ("<=");
   pragma Inline (">");
   pragma Inline (">=");
   pragma Inline (Sign);
   pragma Inline (Is_Negative);
   pragma Inline (Is_Positive);
   pragma Inline (Is_Zero);
   pragma Inline (Negate);
   pragma Inline ("abs");
   pragma Inline ("+");
   pragma Inline ("-");
   pragma Inline ("*");
   pragma Inline ("/");
   pragma Inline ("rem");
end Integers;
with Points; use Points;
with Polygons; use Polygons;

function Intersect (
  Polygon : Polygon_Type;
  Point   : Point_Type
) return Polygon_Type;
pragma Elaborate_Body (Intersect);
--  Returns the intersection of a convex polygon with the open halfspace
--  consisting of those points whose dot product with Point is strictly
--  positive.  Does not work for non-convex polygons.
with Points; use Points;
with Polygons; use Polygons;

procedure Intersects_Halfspace (
  Polygon  : in     Polygon_Type;
  Point    : in     Point_Type;
  Forward  :    out Boolean;
  Backward :    out Boolean
);
pragma Elaborate_Body (Intersects_Halfspace);
--  Determines whether a convex polygon intersects the forward open
--  halfspace consisting of those points whose dot product with Point
--  is strictly positive, and/or the backward open halfspace consisting
--  of those points whose dot product with Point is strictly negative.
--  Does not work for non-convex polygons.
with Integers; use Integers;

--  Linear algebra for integer matrices.
generic
   Num_Rows, Num_Cols : Positive;
package Linear_Algebra is
   pragma Elaborate_Body;

   subtype Row_Index is Positive range 1 .. Num_Rows;
   subtype Col_Index is Positive range 1 .. Num_Cols;
   subtype Rank_Range is Natural range 0 .. Positive'Min (Num_Rows, Num_Cols);

   type Matrix_Type is array (Row_Index, Col_Index) of Integer_Type;

   type Row_Permutation is array (Row_Index) of Row_Index;

   type Col_Permutation is array (Col_Index) of Col_Index;

   procedure Row_Reduce (
     Matrix : in out Matrix_Type;
     Rows   :    out Row_Permutation;
     Cols   :    out Col_Permutation;
     Rank   :    out Rank_Range;
     Strict : in     Boolean
   );
end Linear_Algebra;
with Transition_Matrices;

function Lower_Transition_Matrix
  return Transition_Matrices.Transition_Matrix_Pointer;
--  Returns a transition matrix with entropy less than or equal to that of
--  the current partition.  The vertices consist of pairs (P, {S1, S2})
--  where P is a polygon of the partition, and S1 and S2 are non-adjacent
--  sides of P.  There is a transition from (P, {S1, S2}) to (Q, {T1, T2})
--  if and only if the images of S1 and S2 both intersect both T1' and T2'
--  where T1' and T2' are the two sides of the convex hull of {T1, T2}
--  that are not T1 or T2.  The idea is that the image of the convex hull
--  of {S1, S2} crosses the convex hull of {T1, T2} transversally.
pragma Elaborate_Body (Lower_Transition_Matrix);
with Integers; use Integers;
with Points;   use Points;
with Polygons; use Polygons;

package Lozi is
   pragma Elaborate_Body;

   Lozi_Error : exception;

   Left_Halfspace  : constant Point_Type;
   Right_Halfspace : constant Point_Type;

   procedure Map (Point : in out Point_Type);

   procedure Map (Polygon : in out Polygon_Type);
   --  Warning: if the image of an edge is longer than or equal to half the
   --  sphere, then it should be subdivided.  This routine does NOT perform
   --  any such subdivision.  However, if all vertices are in the region
   --  z >= 0, as they are when calculating entropy, then subdivision is never
   --  needed.

   function Preserves_Orientation return Boolean;
   pragma Inline (Preserves_Orientation);

   procedure Set_Parameters (
     A_Numerator   : Integer_Type;
     A_Denominator : Integer_Type;
     B_Numerator   : Integer_Type;
     B_Denominator : Integer_Type
   );
   --  Raises Lozi_Error if both the numerator and denominator of A
   --  (respectively, B) are zero, or if both denominators are zero.
private
   Left_Halfspace  : constant Point_Type := Create (Negate (One), Zero, Zero);
   Right_Halfspace : constant Point_Type := Create (One,          Zero, Zero);
end Lozi;
------------------------------------------------------------------------------
--  File:            mupreint.ads
--  Description:     Multiple precision integers package
--
--  Date/version:    17-Feb-2002 / 19-Feb-2001 / 7.XII.1999
--
--                   Revised 14.XI.1999 with :
--                     a) procedures (no stack, nor copy !)
--                     b) new data structure
--
--                   First (operators only) XII.1996 - V.1997
--
--  Author:          G. de Montmollin
--                   Gautier.deMontmollin@Winterthur.ch
--
--  Thanks to:       Duncan Sands
--
--  Division algorithm adaptated from BigInt 1.0 library,
--  by Stephen Adams, that refers to
--  D. E. Knuth, the Art of computer programming
--  volume 2, "Seminumerical Algorithms"
--  section 4.3.1, "Multiple-Precision Arithmetic"
--
------------------------------------------------------------------------------

package Multi_precision_integers is

  -- Integers for values --

  subtype Basic_int is Integer;
  subtype Long_basic_int is Long_Integer; -- works even if long is not long

  bitsblock: constant:= Long_basic_int'Size / 2 - 2;
  -- -1 to avoid sign, -1 to allow carry for add/sub
  cardblock: constant:= 2 ** ( bitsblock ); -- # possible values
  -- With cardblock as power of 2, the MOD are optimized to AND
  -- and *, / are optimized to shifts.
  maxblock:  constant:= cardblock -1;
  subtype Block is Basic_int range 0 .. maxblock;

  -- Integers for indices --

  type Index_int is new integer;  
  type Block_array is array( index_int range <> ) of Basic_int;
  
  type Multi_int(n: Index_int) is record
    blk:       Block_array( 0..n ); -- the n blocks with ABSOLUTE value
    neg:       Boolean;             -- negative flag
    zero:      Boolean:=True;       -- zero flag (supercedes the other fields)
    last_used: Index_int;           -- the others blocks are supposed 0
  end record;

  -- NB the `zero' field supercedes EVERY other information (last_used, neg)

----------------------------------------------------------------------------
--   Format of type multi_int.blk: ( i_0, i_1, ..., i_k, *, ..., * )      --
--   i_0..i_k are >=0 ; others (*) are treated as 0                       --
----------------------------------------------------------------------------

----- Informations, conversions, filling

  -- Convert basic_int to multi_int
  function Multi(small: Basic_int) return Multi_int;

  -- Convert multi_int to basic_int (when possible, else: Cannot_fit raised)
  function Basic(large: Multi_int) return Basic_int;

  -- Fill an multi_int of greater array dimension with a smaller one
  procedure Fill(what:out Multi_int; with_smaller: Multi_int);

  -- Test procedure, to check a number's integrity
  procedure Test( m: Multi_int; test_last: Boolean:= true );

  ---------------------------------------------------------------------------
  -------- Arithmetic operators.                                   ----------
  -------- For speed, the "procedure" variants should be preffered ----------
  ---------------------------------------------------------------------------

  ---------------------------
  ----- Unary operators -----
  ---------------------------

  procedure Opp(i: in out multi_int);
  function "+" (i: multi_int) return multi_int;
  function "-" (i: multi_int) return multi_int;

  procedure Abso(i: in out multi_int);
  function "ABS" (i: multi_int) return multi_int;

  function Sign(i: multi_int) return basic_int;
  function Even(i: multi_int) return boolean;
  function Odd (i: multi_int) return boolean;
  
  ----------------------------
  ----- Binary operators -----
  ----------------------------

  procedure Add(i1,i2: in multi_int; i3: in out multi_int);

  function "+" (i1,i2: multi_int) return multi_int;
  function "+" (i1: multi_int; i2: basic_int) return multi_int;
  function "+" (i1: basic_int; i2: multi_int) return multi_int;

  procedure Sub     (i1,i2: in multi_int; i3: in out multi_int);
  procedure Subtract(i1,i2: in multi_int; i3: in out multi_int)
    renames Sub;

  function "-" (i1,i2: multi_int) return multi_int;
  function "-" (i1: multi_int; i2: basic_int) return multi_int;
  function "-" (i1: basic_int; i2: multi_int) return multi_int;

  procedure Multiply(i1,i2: in multi_int; i3: in out multi_int);
  procedure Mult    (i1,i2: in multi_int; i3: in out multi_int)
    renames Multiply;

  function "*" (i1,i2: multi_int) return multi_int;
  function "*" (i1: multi_int; i2: basic_int) return multi_int;
  function "*" (i1: basic_int; i2: multi_int) return multi_int;

  procedure Div_Rem (i1: in     multi_int; i2: in     basic_int;
                     q : in out multi_int;  r: in out basic_int);
  procedure Div_Rem (i1,i2: in multi_int; q,r: in out multi_int);

  function "/" (i1,i2: multi_int) return multi_int;
  function "/" (i1: multi_int; i2: basic_int) return multi_int;
  function "Rem" (i1,i2: multi_int) return multi_int;
  function "Rem" (i1: multi_int; i2: basic_int) return multi_int;
  function "Rem" (i1: multi_int; i2: basic_int) return basic_int;
  function "Mod" (i1,i2: multi_int) return multi_int;
  function "Mod" (i1: multi_int; i2: basic_int) return multi_int;
  function "Mod" (i1: multi_int; i2: basic_int) return basic_int;
  
  procedure Power (i: multi_int; n: Natural; ipn: out multi_int);
  
  function "**" (i: multi_int; n: Natural) return multi_int;

  function Equal (i1,i2: multi_int) return boolean;
  function Equal (i1: multi_int; i2:basic_int) return boolean;
  function ">" (i1,i2: multi_int) return Boolean;
  function ">" (i1: multi_int; i2:basic_int) return Boolean;
  function "<" (i1,i2: multi_int) return Boolean;
  function "<" (i1: multi_int; i2:basic_int) return Boolean;
  function ">=" (i1,i2: multi_int) return Boolean;
  function ">=" (i1: multi_int; i2:basic_int) return Boolean;
  function "<=" (i1,i2: multi_int) return Boolean; 
  function "<=" (i1: multi_int; i2:basic_int) return Boolean;

  Cannot_fit, Empty_multi_int : exception;

  Array_too_small : exception;

  Result_undersized: exception;

  Division_by_zero: exception;

  Zero_power_zero: exception;

end Multi_precision_integers;
------------------------------------------------------------------------------
--  File:            muprinio.ads
--  Description:     Supplement of package 'Multi_precision_integers: I/O
--  Date/version:    15-Feb-2002 / 22.XI.1999 / 22.12.1996
--  Author:          Gautier.deMontmollin@Winterthur.ch
------------------------------------------------------------------------------
with Text_IO;     use Text_IO;
with Multi_precision_integers;
use  Multi_precision_integers;

package Multi_precision_integers_IO is

  Default_Base: Number_Base := 10;

  -- Returns the number of digits in the specified base:
  function Chiffres(i: multi_int; base: number_base:= 10) return Natural;

  -- Returns the image of i in the specified base:
  function Str(i: multi_int; base: number_base:= 10) return String;  

  -- Returns the value of number in string:
  function Val(s: String) return multi_int;

  -- Output to file, in block format:
  procedure Put_in_blocks(File  : in File_Type;
                          Item  : in multi_int);

  -- Output to standard input, in block format:
  procedure Put_in_blocks(Item  : in multi_int);


  ---- The following mimic the Text_IO.Integer_IO 

  -- Get from file:
  procedure Get(File  : in  File_Type;
                Item  : out multi_int;
                Width : in Field := 0);

  -- Get from standard input:
  procedure Get(Item  : out multi_int;
                Width : in  Field := 0);

  -- Put to file:
  procedure Put(File  : in File_Type;
                Item  : in multi_int;
                Width : in Field := 0;
                Base  : in Number_Base := Default_Base);
  -- Width=0 : no default formatting, since inpredicatble length

  -- Put to standard output:
  procedure Put(Item  : in multi_int;
                Width : in Field := 0;
                Base  : in Number_Base := Default_Base);
  -- Width=0 : no default formatting, since inpredicatble length

  -- Get from string:
  procedure Get(From : in  String;
                Item : out multi_int;
                Last : out Positive);

  -- Put to string:
  procedure Put(To   : out String;
                Item : in multi_int;
                Base : in Number_Base := Default_Base);

end Multi_precision_integers_IO;
package Partition.Debug is
   function Is_Partition (Strict : Boolean) return Boolean;
   --  Checks that we really have a partition of Z >= 0 into
   --  convex polygons.  Checks that the partition consists
   --  of convex polygons.  If Strict = True, all vertices
   --  must be extreme points.  Checks that each polygon has
   --  Z coordinate >= 0.  Checks that each edge belongs to
   --  exactly two polygons, and that the polygons are disjoint,
   --  except for edges entirely contained in the circle Z = 0.
   --  Checks that no partition element crosses X = 0.
end Partition.Debug;
package Partition.IO is
   procedure Put_Line (Element : Element_Type);
   procedure Put_Partition;
end Partition.IO;
with Polygons; use Polygons;
with Symbolics; use Symbolics;

generic
   with procedure Action (
     From_Polygon : Polygon_Type;
     From_Index   : Positive;
     From_Image   : Polygon_Type;
     To_Polygon   : Polygon_Type;
     To_Index     : Positive;
     Total_Symbol : Sequence_Type
   );
procedure Partition.Process_Transitions;
pragma Elaborate_Body (Partition.Process_Transitions);
--  Action is called for every pair (From_Polygon, To_Polygon) of polygons
--  in the partition for which the image of From_Polygon intersects To_Polygon.
--  The image of From_Polygon is passed in as From_Image.  The positions of
--  the polygons in the partition are given in From_Index and To_Index.  The
--  symbolic sequence of From_Polygon is equal to Total_Symbol with the last
--  symbol dropped; the symbolic sequence of To_Polygon equals Total_Symbol
--  with the first element dropped.
procedure Partition.Refine;
--  Iterate each partition element and refine it (into at most two pieces)
--  by intersecting it with existing partition elements.  These intersections
--  can also be obtained by intersecting with the left half-space (symbol 'L')
--  and the right half-space (symbol 'R').  Memory associated with the original
--  partition elements is freed.
pragma Elaborate_Body (Partition.Refine);
with Polygons; use Polygons;
with Symbolics; use Symbolics;

--  Maintains a list of partition elements.  They need not actually form a
--  partition of the space.  There must be at most one element with a given
--  symbol sequence.
package Partition is
   pragma Elaborate_Body;

   Partition_Error : exception;

   type Element_Type (Depth : Positive; Size  : Natural) is private;
   --  A partition element: a convex polygon and its symbolic sequence.
   --  Size is the number of vertices of the polygon.  Depth is the
   --  length of the symbolic sequence.

   function "=" (Left, Right : Element_Type) return Boolean is abstract;
   --  Equality is NOT implemented.

   function Create_Element (
     Polygon : Polygon_Type;
     Symbols : Sequence_Type
   ) return Element_Type;
   pragma Inline (Create_Element);

   function Get_Polygon (Element : Element_Type) return Polygon_Type;
   pragma Inline (Get_Polygon);

   function Get_Symbols (Element : Element_Type) return Sequence_Type;
   pragma Inline (Get_Symbols);

   procedure Add_Element (Element : Element_Type);
   --  Raises Partition_Error if the list already contains an element with
   --  the given symbolic sequence, or a symbolic sequence of a different
   --  length.

   procedure Delete_All_Elements;

   type Index_List is array (Natural range <>) of Positive;

   procedure Delete_Elements (Indices : Index_List);
   --  The elements to be deleted, listed in increasing order.  Elements
   --  are numbered starting from 1.

   function Element_Count return Natural;
   pragma Inline (Element_Count);
   --  The number of elements in the partition.
private
   type Element_Pointer is access Element_Type;

   type Element_Type (Depth : Positive; Size : Natural) is record
      Next    : Element_Pointer;
      Polygon : Polygon_Type (Size);
      Symbols : Sequence_Type (1 .. Depth);
   end record;

   function Get_First return Element_Pointer;
   pragma Inline (Get_First);

   procedure Set_First (First_Element : Element_Pointer);
   pragma Inline (Set_First);
end Partition;
package Points.IO is
   procedure Put_Line (Source : Point_Type);
end Points.IO;
with Integers;

package Points is
   pragma Elaborate_Body;

   type Coordinate_Type is (x, y, z);

   type Point_Type is private;
   --  A point in R^3 with integer coordinates.

   function Is_Zero (Right : Point_Type) return Boolean;

   function "="  (Left, Right : Point_Type) return Boolean is abstract;
   --  NOT defined so as to be sure we never use "=" when we mean Equivalent.

   function Equivalent (Left, Right : Point_Type) return Boolean;
   --  True if Left is a positive (not necessarily integer) multiple of Right.

   function "+"  (Left, Right : Point_Type) return Point_Type;
   pragma Inline ("+");

   function "-"  (Left, Right : Point_Type) return Point_Type;
   pragma Inline ("-");

   function Create (
     X_Value : Integers.Integer_Type;
     Y_Value : Integers.Integer_Type;
     Z_Value : Integers.Integer_Type
   ) return Point_Type;
   pragma Inline (Create);

   function Cross_Product (Left, Right : Point_Type) return Point_Type;
   pragma Inline (Cross_Product);

   function Dot_Product (Left, Right : Point_Type)
     return Integers.Integer_Type;
   pragma Inline (Dot_Product);

   function Get_Component (
     Coordinate : Coordinate_Type;
     Point      : Point_Type
   ) return Integers.Integer_Type;
   pragma Inline (Get_Component);

   function Linear_Combination (
     Multiplier1 : Integers.Integer_Type;
     Point1      : Point_Type;
     Multiplier2 : Integers.Integer_Type;
     Point2      : Point_Type
   ) return Point_Type;
   pragma Inline (Linear_Combination);
   --  Returns Multiplier1 * Point1 + Multiplier2 * Point2.

   function Negate  (Right : Point_Type) return Point_Type;
   pragma Inline (Negate);

   procedure Reduce (Point : in out Point_Type);
   --  Replaces a point with the shortest equivalent one.

   procedure Set_Components (
     X_Value : in     Integers.Integer_Type;
     Y_Value : in     Integers.Integer_Type;
     Z_Value : in     Integers.Integer_Type;
     Point   : in out Point_Type
   );
   pragma Inline (Set_Components);
private
   type Point_Type is array (Coordinate_Type) of Integers.Integer_Type;
   pragma Pack (Point_Type);
end Points;
package Polygons.IO is
   procedure Put_Line (Source : Polygon_Type);
end Polygons.IO;
with Points;

package Polygons is
   --  Polygons in the sphere.

   pragma Elaborate_Body;

   type Polygon_Type (Number_Of_Vertices : Natural) is private;
   --  The boundary of the polygon is given by pieces of geodesic.
   --  The boundary pieces are the shortest geodesics from one vertex
   --  to the next.  The polygon is considered to be the open set to the
   --  left of the boundary.  Successive points should be distinct.
   --  Successive points should not be opposing points of the sphere
   --  (since there is then no well-defined shortest geodesic between them).
   --  The polygon should either be the Empty_Polygon or have non-empty
   --  interior.

   Empty_Polygon : constant Polygon_Type;

   function "=" (Left, Right : Polygon_Type) return Boolean is abstract;
   --  Equality is NOT implemented.

   type Point_Array_Type is array (Positive range <>) of Points.Point_Type;

   function Create (Vertex_List : Point_Array_Type) return Polygon_Type;
   pragma Inline (Create);
   --  Make a polygon from a list of vertices.  There is an edge from each
   --  vertex to the next, and from the last to the first.

   function Get_Vertex (
     Position : Integer;
     Polygon  : Polygon_Type
   ) return Points.Point_Type;
   pragma Inline (Get_Vertex);
   --  Wraps around if Position > Number_Of_Vertices.

   function Is_Empty (Polygon : Polygon_Type) return Boolean;
   pragma Inline (Is_Empty);

   function Is_Polygon (
     Strict  : Boolean;
     Polygon : Polygon_Type
   ) return Boolean;
   --  Checks that Polygon is a convex polygon with interior.  If
   --  Strict = True then checks that all vertices are extreme points.

   procedure Reverse_Orientation (Polygon : in out Polygon_Type);
   --  Returns the polygon that is the complement of the previous one.
   --  The new and old polygons have the property that the vertex at
   --  position M for the old polygon is the vertex at position -M for
   --  the new polygon.

   procedure Set_Vertex (
     Position : in     Integer;
     Value    : in     Points.Point_Type;
     Polygon  : in out Polygon_Type
   );
   pragma Inline (Set_Vertex);
   --  Wraps around if Position > Number_Of_Vertices.
private
   pragma Pack (Point_Array_Type);

   type Polygon_Type (Number_Of_Vertices : Natural) is record
      Vertices : Point_Array_Type (1 .. Number_Of_Vertices);
   end record;
   pragma Pack (Polygon_Type);

   Empty_Point_Array : Point_Array_Type (1 .. 0);

   Empty_Polygon : constant Polygon_Type := (
     Number_Of_Vertices => 0,
     Vertices           => Empty_Point_Array
   );
end Polygons;
procedure Print_Lower (Lower_Bound : Float);
pragma Elaborate_Body (Print_Lower);
procedure Print_Upper (Upper_Bound : Float);
pragma Elaborate_Body (Print_Upper);
procedure Print_Usage;
pragma Elaborate_Body (Print_Usage);
with Points; use Points;

function Segments_Intersect (
  X1, Y1 : Point_Type;
  X2, Y2 : Point_Type
) return Boolean;
--  Returns True if and only if there exist non-negative numbers a, b, c, d
--  such that a*X1 + b*Y1 = c*X2 + d*Y2, one of a,b is non-zero and one of
--  c,d is non-zero.
with Linear_Algebra;

pragma Elaborate_All (Linear_Algebra);

package Segments_Support is new Linear_Algebra (3, 4);
package Symbolics.IO is
   procedure Put (Symbols : Sequence_Type);
end Symbolics.IO;
package Symbolics is
   type Symbol_Type is (L, R);
   for Symbol_Type'Size use 1; -- beware of GNAT bugs with this

   type Sequence_Type is array (Natural range <>) of Symbol_Type;
   pragma Pack (Sequence_Type); -- beware of GNAT bugs with this

   function "<"  (Left, Right : Sequence_Type) return Boolean;
   function "<=" (Left, Right : Sequence_Type) return Boolean;
   function ">"  (Left, Right : Sequence_Type) return Boolean;
   function ">=" (Left, Right : Sequence_Type) return Boolean;
private
   pragma Inline ("<");
   pragma Inline ("<=");
   pragma Inline (">");
   pragma Inline (">=");
end Symbolics;
package Transition_Matrices.IO is
   procedure Put_Line (Matrix : Transition_Matrix_Type);

   procedure Put_Line (
     Matrix   : Transition_Matrix_Type;
     Vertices : Vertex_List
   );
end Transition_Matrices.IO;
generic
  type Vertex_List is array (Natural range <>) of Positive;

  with procedure SCC_Action (
    Matrix   : Transition_Matrix_Type;
    Vertices : Vertex_List
  );

  with procedure Wander_Action (
    Matrix   : Transition_Matrix_Type;
    Vertices : Vertex_List
  );
procedure Transition_Matrices.SCC (Matrix : Transition_Matrix_Type);
pragma Elaborate_Body (Transition_Matrices.SCC);
--  Decomposes the transition matrix into Strongly Connected Components (SCCs).
--  A vertex is in an SCC if and only if there is a path from that vertex to
--  itself.  Two vertices are in the same SCC if and only if there is a path
--  from the first to the second, and also a path from the second to the first.
--  A vertex not in any SCC is called a wandering vertex.  SCCs are also known
--  as irreducible components.  SCC_Action is called once for each SCC, with
--  Vertices containing the indices of the vertices in the SCC, sorted from
--  smallest to largest.  The value of Vertices'First may vary on each call.
--  Once SCC_Action has been called for each SCC, Wander_Action is called with
--  Vertices containing the indices of all wandering vertices, sorted from
--  smallest to largest.  No value is guaranteed for Vertices'First.
package Transition_Matrices.Spectral_Radius is
   --  Return guaranteed bounds for the spectral radius (Low <= SR <= High).

   procedure Estimate (
     Matrix    : in     Transition_Matrix_Type;
     Low, High :    out Float
   );
   pragma Inline (Estimate);
   --  Estimate the spectral radius of the entire matrix.  Applies
   --  Component_Estimate to each strongly connected component (SCC).

   procedure Component_Estimate (
     Matrix    : in     Transition_Matrix_Type;
     Vertices  : in     Vertex_List;
     Low, High :    out Float
   );
   pragma Inline (Component_Estimate);
   --  Estimate the spectral radius of the submatrix with rows and columns in
   --  Vertices.  Uses an implicitly restarted Arnoldi method to find an
   --  approximate Perron-Frobenius eigenvector.
end Transition_Matrices.Spectral_Radius;
with Ada.Finalization;
with Transition_Matrix_Rows;

package Transition_Matrices is
   pragma Elaborate_Body;

   type Transition_Matrix_Type (Size : Natural) is limited private;

   type Transition_Matrix_Pointer is access Transition_Matrix_Type;

   procedure Clear_Transition (
     From   : in     Positive;
     To     : in     Positive;
     Matrix : in out Transition_Matrix_Type
   );
   pragma Inline (Clear_Transition);

   procedure Set_Transition (
     From   : in     Positive;
     To     : in     Positive;
     Matrix : in out Transition_Matrix_Type
   );
   pragma Inline (Set_Transition);

   function Transition_Exists (
     From   : Positive;
     To     : Positive;
     Matrix : Transition_Matrix_Type
   ) return Boolean;
   pragma Inline (Transition_Exists);

   type Vertex_List is array (Natural range <>) of Positive;

private

   type Row_List is array (Positive range <>) of Transition_Matrix_Rows.Matrix_Row;

   type Transition_Matrix_Type (Size : Natural) is new Ada.Finalization.Limited_Controlled with record
      Rows : Row_List (1 .. Size);
   end record;

   procedure Finalize (Matrix : in out Transition_Matrix_Type);

end Transition_Matrices;
package Transition_Matrix_Rows is
   pragma Elaborate_Body;

   type Matrix_Row is limited private;
   --  A row of a transition matrix.  Each row entry is either zero or one.

   procedure Clear_All (Row : in out Matrix_Row);
   pragma Inline (Clear_All);
   --  Reset all row entries to zero.  While there is no need to call this
   --  before using a row for the first time, it MUST BE CALLED when finished
   --  with a row in order to free memory - MEMORY IS NOT AUTOMATICALLY FREED.

   procedure Clear_Column (
     Row    : in out Matrix_Row;
     Column : in     Positive
   );
   --  Set the row entry for this column to zero.

   procedure Set_Column (
     Row    : in out Matrix_Row;
     Column : in     Positive
   );
   --  Set the row entry for this column to one.

   function Column_Is_Set (
     Row    : Matrix_Row;
     Column : Positive
   ) return Boolean;

   generic
      with procedure Action (
        Column : in     Positive;
        Stop   : in out Boolean
      );
   procedure Visit_Non_Zero_Columns (Row : Matrix_Row);
   --  Call Action with the column number of each non-zero row entry.  If the
   --  user sets Stop to True then any remaining columns are skipped.

private

   type Column_List is array (Positive range <>) of Positive;

   type Column_List_Pointer is access Column_List;

   Maximum_Transitions_On_Stack : constant := 6;

   subtype On_Stack_List is Column_List (1 .. Maximum_Transitions_On_Stack);

   type Matrix_Row is record
      Non_Zeros : Natural := 0;
      On_Stack  : On_Stack_List;
      On_Heap   : Column_List_Pointer; -- internally indexed from one
   end record;
   --  Implemented as an ordered list of column numbers with non-zero entries.
   --  It is rare to have more than a handful of non-zeros (three is already
   --  uncommon).  Optimize the common case by only allocating memory if the
   --  number of non-zeros exceeds Maximum_Transitions_On_Stack.

end Transition_Matrix_Rows;
with Partition;
with Transition_Matrices.SCC;
with Trim_Support;

pragma Elaborate_All (Transition_Matrices.SCC);

procedure Trim_Partition is new Transition_Matrices.SCC (
  Vertex_List   => Partition.Index_List,
  SCC_Action    => Trim_Support.Null_Action,
  Wander_Action => Trim_Support.Delete_Wandering
);
--  Remove wandering polygons.
with Partition; use Partition;
with Transition_Matrices; use Transition_Matrices;

package Trim_Support is
   pragma Elaborate_Body (Trim_Support);

   procedure Null_Action (
     Matrix   : Transition_Matrix_Type;
     Vertices : Index_List
   );
   pragma Inline (Null_Action);

   procedure Delete_Wandering (
     Matrix   : Transition_Matrix_Type;
     Vertices : Partition.Index_List
   );
   pragma Inline (Delete_Wandering);
end Trim_Support;
with Transition_Matrices;

function Upper_Transition_Matrix
  return Transition_Matrices.Transition_Matrix_Pointer;
--  Returns a transition matrix with entropy at least as big as that of the
--  current partition.  The vertices are the polygons of the partition.
--  There is a transition from a polygon P to a polygon Q if and only if
--  the image of the interior of P intersects the interior of Q.
pragma Elaborate_Body (Upper_Transition_Matrix);
--*****************************************************************************
--*
--*****************************************************************************
--*

--
-- Copyright (C) 1996 Ada Resource Association (ARA), Columbus, Ohio.
-- Author: David A. Wheeler
--
-- The function COPY and all the character related functions have been
-- added by Jerome DELCOURT.
--

with Text_IO, Ada.Strings.Unbounded; use Text_IO, Ada.Strings.Unbounded;

package UStrings is

   -- This package provides a simpler way to work with type
   -- Unbounded_String, since this type will be used very often.
   -- Most users will want to ALSO with "ADA.STRINGS.UNBOUNDED".
   -- Ideally this would be a child package of "ADA.STRINGS.UNBOUNDED".
   --

   -- This package provides the following simplifications:
   --  + Shortens the type name from "UNBOUNDED_STRING" to "Ustring".
   --  + Creates shorter function names for To_Unbounded_String, i.e.
   --    To_Ustring(U) and U(S).  "U" is not a very readable name, but
   --    it's such a common operation that a short name seems appropriate
   --    (this function is needed every time a String constant is used).
   --    It also creates S(U) as the reverse of U(S).
   --  + Adds other subprograms, currently just "Swap".
   --  + Other packages can use this package to provide other simplifications.

   subtype Ustring is Unbounded_String;

   function To_Ustring (Source : in String) return Unbounded_String renames
     To_Unbounded_String;
   function U (Source : in String) return Unbounded_String renames
     To_Unbounded_String;
   function S (Source : in Unbounded_String) return String renames To_String;

   -- "Swap" is important for reuse in some other packages, so we'll define it.

   procedure Swap (Left, Right : in out Unbounded_String);

   function Empty (S : Unbounded_String) return Boolean;
   -- returns True if Length(S)=0.
   pragma Inline (Empty);

   --*****************
   --* I/O Routines. *
   --*****************

   procedure Get_Line (File : in File_Type; Item : out Unbounded_String);
   procedure Get_Line (Item : out Unbounded_String);

   procedure Put (File : in File_Type; Item : in Unbounded_String);
   procedure Put (Item : in Unbounded_String);

   procedure Put_Line (File : in File_Type; Item : in Unbounded_String);
   procedure Put_Line (Item : in Unbounded_String);

   function Copy
     (s     : in Unbounded_String;
      Index : in Positive;
      Size  : in Natural)
      return  Unbounded_String;
   -- This function returns the unbounded string contained in s, which starts
   -- at the character number Index, and which has Size character. If the
   --string
   -- is not long enough, the result may have less the Size character.
   -- If s is empty, or if Index if greater than the size of the string s,
   -- the result is an empty string.

   -- *******************************
   -- * Character related functions *
   -- *******************************

   function Del_Spaces (Item : in Unbounded_String) return Unbounded_String;
   -- Removes all the spaces from the strings.
   function Del_Character
     (Item : in Unbounded_String;
      Char : in Character)
      return Unbounded_String;
   -- Removes all the occurences of Char from the string.

   function Char_Count
     (S    : in Unbounded_String;
      Char : in Character)
      return Natural;
   -- Counts the number of Char in this string.

   function First_Matching_Char_Position
     (S    : in Unbounded_String;
      Char : in Character)
      return Natural;
   -- Finds the first occurence of the character in the string.
   -- Returns 0 if none found.

   function Last_Matching_Char_Position
     (S    : in Unbounded_String;
      Char : in Character)
      return Natural;
   -- Finds the last occurence of the character in the string.
   -- Returns 0 if none found.

   function Char_Replace
     (S                             : in Unbounded_String;
      Char_to_be_replaced, New_char : in Character)
      return                          Unbounded_String;
   -- Counts the number of Char in this string.

end UStrings;
with Ada.Exceptions;
with Ada.Unchecked_Deallocation;

package body Arnoldi is
   type Vector_Pointer is access Vector;

   type Integer_Array is array (Fortran_Integer range <>) of Fortran_Integer;
   pragma Convention (Fortran, Integer_Array);

   type Logical_Array is array (Fortran_Integer range <>) of Logical;
   pragma Convention (Fortran, Logical_Array);

   type Matrix is array (Fortran_Integer range <>, Fortran_Integer range <>)
     of Double_Precision;
   pragma Convention (Fortran, Matrix);

   type Matrix_Pointer is access Matrix;

   --  Importing from C rather than Fortran works around a GNAT bug.
   procedure dnaupd (
     IDO       : access Fortran_Integer;
     BMAT      : in     Fortran_Character;
     N         : access Fortran_Integer;
     WHICH     : in     Fortran_Character;
     NEV       : access Fortran_Integer;
     TOL       : access Double_Precision;
     RESID     : in out Vector;
     NCV       : access Fortran_Integer;
     V         :    out Matrix;
     LDV       : access Fortran_Integer;
     IPARAM    : in out Integer_Array;
     IPNTR     :    out Integer_Array;
     WORKD     : in out Vector;
     WORKL     :    out Vector;
     LWORKL    : access Fortran_Integer;
     INFO      : access Fortran_Integer;
     BMAT_LEN  : in     Long_Integer;
     WHICH_LEN : in     Long_Integer
   );
   pragma Import (C, dnaupd, "dnaupd_");

   procedure dneupd (
     RVEC       : access Logical;
     HOWMNY     : in     Fortran_Character;
     SELEC      : in out Logical_Array;
     DR         :    out Vector;
     DI         :    out Vector;
     Z          :    out Matrix;
     LDZ        : access Fortran_Integer;
     SIGMAR     : access Double_Precision;
     SIGMAI     : access Double_Precision;
     WORKEV     :    out Vector;
     BMAT       : in     Fortran_Character;
     N          : access Fortran_Integer;
     WHICH      : in     Fortran_Character;
     NEV        : access Fortran_Integer;
     TOL        : access Double_Precision;
     RESID      : in out Vector;
     NCV        : access Fortran_Integer;
     V          :    out Matrix;
     LDV        : access Fortran_Integer;
     IPARAM     : in out Integer_Array;
     IPNTR      :    out Integer_Array;
     WORKD      : in out Vector;
     WORKL      :    out Vector;
     LWORKL     : access Fortran_Integer;
     INFO       : access Fortran_Integer;
     HOWMNY_LEN : in     Long_Integer;
     BMAT_LEN   : in     Long_Integer;
     WHICH_LEN  : in     Long_Integer
   );
   pragma Import (C, dneupd, "dneupd_");

   procedure Free is new Ada.Unchecked_Deallocation (
     Vector,
     Vector_Pointer
   );

   procedure Free is new Ada.Unchecked_Deallocation (
     Matrix,
     Matrix_Pointer
   );

   --------------------------
   -- Extremal_Eigenvector --
   --------------------------
   procedure Extremal_Eigenvector (
     First, Last    : in     Fortran_Integer;
     Real_Part      :    out Vector;
     Imaginary_Part :    out Vector;
     Eigenvalue_R_P :    out Double_Precision;
     Eigenvalue_I_P :    out Double_Precision;
     Tolerance      : in     Double_Precision
   ) is
      ido    : aliased  Fortran_Integer := 0;
      bmat   : constant Fortran_Character := To_Fortran ("I");
      n      : aliased  Fortran_Integer := Last - First + 1;
      which  : constant Fortran_Character := To_Fortran ("LM");
      nev    : aliased  Fortran_Integer := 1;
      tol    : aliased  Double_Precision := Tolerance;
      ncv    : aliased  Fortran_Integer := Fortran_Integer'Min (3, n);
      v      : Matrix_Pointer;
      ldv    : aliased  Fortran_Integer := n;
      iparam : Integer_Array (1 .. 11);
      ipntr  : Integer_Array (1 .. 14);
      workd  : Vector_Pointer;
      lworkl : aliased  Fortran_Integer := 3*ncv**2 + 6*ncv;
      workl  : Vector (1 .. lworkl);
      info   : aliased  Fortran_Integer := 0; -- Use random initial vector

      rvec   : aliased  Logical := True; -- Compute Ritz vectors
      howmny : constant Fortran_Character := To_Fortran ("A");
      selec  : Logical_Array (1 .. ncv);
      dr     : Vector (1 .. nev + 1);
      di     : Vector (1 .. nev + 1);
      sigmar : aliased  Double_Precision := 0.0;
      sigmai : aliased  Double_Precision := 0.0;
      workev : Vector (1 .. 3*ncv);
   begin
      if Real_Part'First > First or Imaginary_Part'First > First or
        Real_Part'Last < Last or Imaginary_Part'Last < Last then
         Ada.Exceptions.Raise_Exception (
           Arnoldi_Error'Identity, "mismatched vectors"
         );
      end if;

      if n < 3 then
         Ada.Exceptions.Raise_Exception (
           Arnoldi_Error'Identity, "problem too small (need 3 x 3 or bigger)"
         );
      else -- n >= 3
         v     := new Matrix (1 .. n, 1 .. ncv);
         workd := new Vector (1 .. 3*n);

         iparam (1) := 1;
         iparam (3) := 1000;
         iparam (7) := 1;

         loop
            dnaupd (ido'Access, bmat, n'Access, which, nev'Access, tol'Access,
              Real_Part, ncv'Access, v.all, ldv'Access, iparam, ipntr,
              workd.all, workl, lworkl'Access, info'Access, bmat'Length,
              which'Length
            );

            exit when ido /= -1 and ido /= 1;

            Iterate (
              Source => workd (ipntr (1) .. ipntr (1) + n - 1),
              Target => workd (ipntr (2) .. ipntr (2) + n - 1)
            );
         end loop;

         if info < 0 then
            Ada.Exceptions.Raise_Exception (
              Arnoldi_Error'Identity,
              "dnaupd error, info is:" & Fortran_Integer'Image (info)
            );
         end if;

         dneupd (rvec'Access, howmny, selec, dr, di, v.all, ldv'Access,
           sigmar'Access, sigmai'Access, workev, bmat, n'Access, which,
           nev'Access, tol'Access, Real_Part, ncv'Access, v.all, ldv'Access,
           iparam, ipntr, workd.all, workl, lworkl'Access, info'Access,
           howmny'Length, bmat'Length, which'Length
         );

         if info /= 0 then
            Ada.Exceptions.Raise_Exception (
              Arnoldi_Error'Identity,
              "dneupd error, info is:" & Fortran_Integer'Image (info)
            );
         end if;

         Free (workd);

         Eigenvalue_R_P := dr (1);
         Eigenvalue_I_P := di (1);

         declare
            Base : constant Fortran_Integer := First - 1;
         begin
            for I in 1 .. n loop
               Real_Part (Base + I) := v (I, 1);
            end loop;

            if Eigenvalue_I_P = 0.0 then
               --  Ritz value/vector is real
               for I in 1 .. n loop
                  Imaginary_Part (Base + I) := 0.0;
               end loop;
            else
               --  Ritz value/vector is complex
               for I in 1 .. n loop
                  Imaginary_Part (Base + I) := v (I, 2);
               end loop;
            end if;
         end;

         Free (v);
      end if;
   exception
      when others =>
         Free (workd);
         Free (v);
         raise;
   end Extremal_Eigenvector;
end Arnoldi;
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
--*********************************************************
--*********************
--                                                                            *
--*
--  File:        BNBISIIO.ADB
--**
--  Description: Big_Number.Big_Signed_IO package body
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
package body Big_Signed_IO is

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   procedure GET_LINE (x : out Big_Signed) is
      Temp_String : Unbounded_String;
   begin
      Get_Line (Temp_String);
      x := UString2Signed (Temp_String);
   end GET_LINE;

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   procedure GET_LINE (File : in File_Type; x : out Big_Signed) is
      Temp_String : Unbounded_String;
   begin
      Get_Line (File, Temp_String);
      x := UString2Signed (Temp_String);
   end GET_LINE;

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   procedure PUT (x : in Big_Signed; Base : in My_Type := 10) is
   begin
      Put (Big_Signed2UString (x, Base));
   end PUT;

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   procedure PUT
     (File : in File_Type;
      x    : in Big_Signed;
      Base : in My_Type := 10)
   is
   begin
      Put (File, Big_Signed2UString (x, Base));
   end PUT;

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   procedure PUT_LINE (x : in Big_Signed; Base : in My_Type := 10) is
   begin
      Put_Line (Big_Signed2UString (x, Base));
   end PUT_LINE;

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   procedure PUT_LINE
     (File : in File_Type;
      x    : in Big_Signed;
      Base : in My_Type := 10)
   is
   begin
      Put_Line (File, Big_Signed2UString (x, Base));
   end PUT_LINE;

   --**************************************************************************
   --****
   --**************************************************************************
   --****

end Big_Signed_IO;
--***********************************************************
--*******************
--                                                                            *
--*
--  File:        BNBIUNIO.ADB
--**
--  Description: Big_Number.Big_Unsigned_IO package body
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
package body Big_Unsigned_IO is

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   procedure GET_LINE (x : out Big_Unsigned) is
      Temp_String : Unbounded_String;
   begin
      Get_Line (Temp_String);
      x := UString2Unsigned (Temp_String);
   end GET_LINE;

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   procedure GET_LINE (File : in File_Type; x : out Big_Unsigned) is
      Temp_String : Unbounded_String;
   begin
      Get_Line (File, Temp_String);
      x := UString2Unsigned (Temp_String);
   end GET_LINE;

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   procedure PUT (x : in Big_Unsigned; Base : in My_Type := 10) is
   begin
      Put (Big_Unsigned2UString (x, Base));
   end PUT;

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   procedure PUT
     (File : in File_Type;
      x    : in Big_Unsigned;
      Base : in My_Type := 10)
   is
   begin
      Put (File, Big_Unsigned2UString (x, Base));
   end PUT;

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   procedure PUT_LINE (x : in Big_Unsigned; Base : in My_Type := 10) is
   begin
      Put_Line (Big_Unsigned2UString (x, Base));
   end PUT_LINE;

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   procedure PUT_LINE
     (File : in File_Type;
      x    : in Big_Unsigned;
      Base : in My_Type := 10)
   is
   begin
      Put_Line (File, Big_Unsigned2UString (x, Base));
   end PUT_LINE;

   --**************************************************************************
   --****
   --**************************************************************************
   --****

end Big_Unsigned_IO;
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
--**************************************************************
--****************
--                                                                            *
--*
--  File:        BINUMCON.ADB
--**
--  Description: Big_Number.Conversion package body
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
package body Conversion is

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   function Unsigned2Signed
     (x        : in Big_Unsigned;
      Positive : in Boolean := True)
      return     Big_Signed
   is
      Temp : Big_Signed;
   begin
      Temp.Last_Significant_Digit := x.Last_Significant_Digit;
      Temp.Mantissa               := x.Mantissa;
      Temp.Positive               := Positive;
      if Temp.Last_Significant_Digit = 0 and Temp.Mantissa (0) = 0 then
         Temp.Positive := True; -- Make sure 0 is positive
      end if;
      return Temp;
   end Unsigned2Signed;

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   function Signed2Unsigned (x : in Big_Signed) return Big_Unsigned is
      Temp : Big_Unsigned;
   begin
      Temp.Last_Significant_Digit := x.Last_Significant_Digit;
      Temp.Mantissa               := x.Mantissa;
      return Temp;
   end Signed2Unsigned;

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   function Big_Unsigned2Long_Long_Float
     (x    : in Big_Unsigned)
      return Long_Long_Float
   is
      Temp_Result : Long_Long_Float := 0.0;
   begin
      for I in reverse  First_Indice .. x.Last_Significant_Digit loop
         Temp_Result := Temp_Result * LLF_Base +
                        Long_Long_Float (x.Mantissa (I));
      end loop;
      return Temp_Result;
   end Big_Unsigned2Long_Long_Float;

   --**************************************************************************
   --****
   --**************************************************************************
   --****

   function Big_Signed2Long_Long_Float
     (x    : in Big_Signed)
      return Long_Long_Float
   is
      Temp_Result : Long_Long_Float := 0.0;
   begin
      for I in reverse  First_Indice .. x.Last_Significant_Digit loop
         Temp_Result := Temp_Result * LLF_Base +
                        Long_Long_Float (x.Mantissa (I));
      end loop;
      if (x.Positive = False) then
         Temp_Result := -Temp_Result;
      end if;
      return Temp_Result;
   end Big_Signed2Long_Long_Float;

   --**************************************************************************
   --****
   --**************************************************************************
   --****

end Conversion;
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
--*****************************************************************************
--*
--                                                                            *
--*
--  File:        BIGNUMBE.ADB
--**
--  Description: Big_Number package body
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

package body Big_Number is

   --**************************************************************************
   --**
   --**               Declarations of local types and constants
   --**
   --**************************************************************************
   --**

   ---------------------
   -- Local constants --
   ---------------------

   First_Indice : constant Index_Type := 0;
   Last_Indice  : constant Index_Type := Nb_Item - 1;

   Base            : constant Largest_Unsigned := 2 ** My_Type'Size;
   Base_Minus_One  : constant Largest_Unsigned := Base - 1;
   LLF_Base        : constant Long_Long_Float  := Long_Long_Float (Base);
   LLF_Base_Square : constant Long_Long_Float  := LLF_Base * LLF_Base;
   Greatest_Digit  : constant My_Type          := My_Type'Last;

   --**************************************************************************
   --**
   --**************************************************************************
   --**

   function Min (x, y : in Index_Type) return Index_Type is
   begin
      if (x < y) then
         return x;
      else
         return y;
      end if;
   end Min;

   --**************************************************************************
   --**

   function Max (x, y : in Index_Type) return Index_Type is
   begin
      if (x < y) then
         return y;
      else
         return x;
      end if;
   end Max;

   --**************************************************************************
   --**

   package body Conversion is separate;
   package body Generic_Conversion is separate;
   package body String_Conversion is separate;
   package body Unsigned_Number is separate;
   package body Signed_Number is separate;
   package body Big_Unsigned_IO is separate;
   package body Big_Signed_IO is separate;
   package body Big_Fraction is separate;
   package body Big_Unsigned_Random is separate;

--****************************************************************************
--****************************************************************************

begin
   if (My_Type_Size < 8) then
      Text_IO.Put_Line
        ("The package Big_Number has been modified in a wrong way.");
      Text_IO.Put_Line
        ("The type My_Type must have a size greater or equal to 8 bits.");
      raise Program_Error;
   end if;
end Big_Number;
with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Float_Text_IO;
with Ada.Integer_Text_IO;
with Ada.Numerics.Elementary_Functions;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Arnoldi;
with Integers;
with Lower_Transition_Matrix;
with Lozi;
with Partition;
with Partition.IO;
with Partition.Refine;
with Points;
with Polygons;
with Print_Lower;
with Print_Upper;
with Print_Usage;
with Symbolics;
with Transition_Matrices;
with Transition_Matrices.Spectral_Radius;
with Trim_Partition;
with Upper_Transition_Matrix;

procedure Calculate_Entropy is
   use Ada;

   procedure Free is new Unchecked_Deallocation (
     Transition_Matrices.Transition_Matrix_Type,
     Transition_Matrices.Transition_Matrix_Pointer
   );

   Depth : Positive := 1; -- length of symbol sequences

   Best_Upper_Radius : Float := 2.0; -- Maximum possible spectral radius
   Best_Lower_Radius : Float := 1.0; -- Minimum possible spectral radius
   Best_Upper_Entropy : Float :=
     Numerics.Elementary_Functions.Log (Best_Upper_Radius);
   Best_Lower_Entropy : Float :=
     Numerics.Elementary_Functions.Log (Best_Lower_Radius);
   Old_Best_Upper_Entropy : Float := Best_Upper_Entropy;
   Old_Best_Lower_Entropy : Float := Best_Lower_Entropy;

   Accuracy   : Float;
   Default_Accuracy : constant := 0.01;
   Average_Improvement : Float := 1.0;
   Decay_Multiplier : constant := 0.9;
   Decay_Constant   : constant := 1.0 - Decay_Multiplier;
   Smallest_Improvement : Float;

   Num_Digits : Positive;

   Verbose : Boolean;

   Count : Natural;

   Low, High : Float;
begin
   Process_Command_Line :
      declare
         First_Numeric_Parameter : Positive;
      begin
         if Command_Line.Argument_Count < 4 or
            Command_Line.Argument_Count > 6 then
            Print_Usage;
            return;
         end if;

         if Command_Line.Argument_Count = 4 then
            First_Numeric_Parameter := 1;
         end if;

         if Command_Line.Argument_Count = 6 then
            First_Numeric_Parameter := 2;
         end if;

         if Command_Line.Argument_Count = 5 then
            if Command_Line.Argument (1) = "-v" then
               First_Numeric_Parameter := 2;
            else
               First_Numeric_Parameter := 1;
            end if;
         end if;

         if Command_Line.Argument (1) = "-v" then
            Verbose := True;
         else
            Verbose := False;
         end if;

         if First_Numeric_Parameter + 4 <= Command_Line.Argument_Count then
            begin
               Accuracy := Float'Value (
                 Command_Line.Argument (First_Numeric_Parameter + 4)
               );
               if Accuracy < Float'Model_Epsilon then
                  Accuracy := Float'Model_Epsilon;
               end if;
               if Accuracy > 1.0 then
                  Accuracy := 1.0;
               end if;
            exception
               when Constraint_Error =>
                  Text_IO.Put (Text_IO.Standard_Error, "Error: ");
                  Text_IO.Put (Text_IO.Standard_Error,
                    "accuracy must be a floating point number");
                  Text_IO.New_Line (Text_IO.Standard_Error);
                  Text_IO.Flush (Text_IO.Standard_Error);
                  return;
            end;
         else
            Accuracy := Default_Accuracy;
         end if;

         Num_Digits := Positive (
           -Float'Floor (
             Numerics.Elementary_Functions.Log (
               Accuracy / 2.0,
               Base => 10.0
             )
           )
         );

         Get_Parameters :
            declare
               A_Numerator   : Integer;
               A_Denominator : Integer;
               B_Numerator   : Integer;
               B_Denominator : Integer;
            begin
               begin
                  A_Numerator   := Integer'Value (
                    Command_Line.Argument (First_Numeric_Parameter)
                  );
                  A_Denominator := Integer'Value (
                    Command_Line.Argument (First_Numeric_Parameter + 1)
                  );
                  B_Numerator   := Integer'Value (
                    Command_Line.Argument (First_Numeric_Parameter + 2)
                  );
                  B_Denominator := Integer'Value (
                    Command_Line.Argument (First_Numeric_Parameter + 3)
                  );
               exception
                  when Constraint_Error =>
                     Text_IO.Put (Text_IO.Standard_Error, "Error: ");
                     Text_IO.Put (Text_IO.Standard_Error,
                       "Lozi parameters must be integers");
                     Text_IO.New_Line (Text_IO.Standard_Error);
                     Text_IO.Flush (Text_IO.Standard_Error);
                     return;
               end;

               begin
                  Lozi.Set_Parameters (
                    Integers.To_Integer_Type (A_Numerator),
                    Integers.To_Integer_Type (A_Denominator),
                    Integers.To_Integer_Type (B_Numerator),
                    Integers.To_Integer_Type (B_Denominator)
                  );
               exception
                  when others =>
                     Text_IO.Put (Text_IO.Standard_Error, "Error: ");
                     Text_IO.Put (Text_IO.Standard_Error,
                       "invalid parameter values");
                     Text_IO.New_Line (Text_IO.Standard_Error);
                     Text_IO.Flush (Text_IO.Standard_Error);
                     return;
               end;

               if Verbose then
                  Text_IO.Put_Line ("-- Parameters --");
                  Text_IO.Put ("A = ");
                  Integer_Text_IO.Put (A_Numerator, Width => 0);
                  Text_IO.Put (" / ");
                  Integer_Text_IO.Put (A_Denominator, Width => 0);
                  Text_IO.New_Line;
                  Text_IO.Put ("B = ");
                  Integer_Text_IO.Put (B_Numerator, Width => 0);
                  Text_IO.Put (" / ");
                  Integer_Text_IO.Put (B_Denominator, Width => 0);
                  Text_IO.New_Line;
                  Text_IO.Put ("Accuracy = ");
                  Float_Text_IO.Put (Accuracy, Aft => Num_Digits, Exp => 0);
                  Text_IO.New_Line (2);
                  Text_IO.Flush;
               end if;
            end Get_Parameters;
      end Process_Command_Line;

   Smallest_Improvement := Accuracy / Decay_Constant / 100.0;

   Initialize_Partition :
      declare
         use Integers;
         use Partition;
         use Symbolics;
      begin
         Partition.Add_Element (
           Create_Element (
             Polygon =>
               Polygons.Create (
                 (
                   Points.Create (Zero,         Zero,         One),
                   Points.Create (Negate (One), Zero,         Zero),
                   Points.Create (Zero,         Negate (One), Zero)
                 )
             ),
             Symbols =>
               (
                 1 => L,
                 2 => L
               )
           )
         );

         Partition.Add_Element (
           Create_Element (
             Polygon =>
               Polygons.Create (
                 (
                   Points.Create (Zero,         Zero,         One),
                   Points.Create (Zero,         Negate (One), Zero),
                   Points.Create (One,          Zero,         Zero)
                 )
             ),
             Symbols =>
               (
                 1 => L,
                 2 => R
               )
           )
         );

         Partition.Add_Element (
           Create_Element (
             Polygon =>
               Polygons.Create (
                 (
                   Points.Create (Zero,         Zero,         One),
                   Points.Create (Zero,         One,          Zero),
                   Points.Create (Negate (One), Zero,         Zero)
                 )
             ),
             Symbols =>
               (
                 1 => R,
                 2 => L
               )
           )
         );

         Partition.Add_Element (
           Create_Element (
             Polygon =>
               Polygons.Create (
                 (
                   Points.Create (Zero,         Zero,         One),
                   Points.Create (One,          Zero,         Zero),
                   Points.Create (Zero,         One,          Zero)
                 )
             ),
             Symbols =>
               (
                 1 => R,
                 2 => R
               )
           )
         );
      end Initialize_Partition;

   if Verbose then
      Text_IO.Put_Line ("-- Initial Partition --");
      Partition.IO.Put_Partition;
      Text_IO.New_Line;
      Text_IO.Flush;
   end if;

   Estimate_Spectral_Radius :
      begin
         if Verbose then
            Text_IO.Put_Line ("=> Calculating spectral radii");
            Text_IO.New_Line;
            Text_IO.Flush;
         end if;

         Calculate_Spectral_Radius :
            loop
               if Verbose then
                  Text_IO.Put_Line ("  => Computing upper transition matrix");
                  Text_IO.Flush;
               end if;

               Calculate_Upper_Transition_Matrix :
                  declare
                     Matrix : Transition_Matrices.Transition_Matrix_Pointer :=
                       Upper_Transition_Matrix;
                     Spectral_Radius : Float;
                  begin
                     if Verbose then
                        Text_IO.Put_Line ("  <= Done");
                        Text_IO.New_Line;
                        Text_IO.Put_Line (
                          "  Size:" & Integer'Image (Matrix.Size)
                        );
                        Text_IO.New_Line;
                        Text_IO.Put_Line ("  => Computing spectral radius");
                        Text_IO.Flush;
                     end if;

                     --  Calculate the spectral radius of the transition matrix
                     begin
                        Transition_Matrices.Spectral_Radius.Estimate (
                          Matrix.all,
                          Low,
                          High
                        );
                        Spectral_Radius := High;
                        if Spectral_Radius < Best_Upper_Radius then
                           Best_Upper_Radius := Spectral_Radius;
                           Old_Best_Upper_Entropy := Best_Upper_Entropy;
                           Best_Upper_Entropy :=
                             Numerics.Elementary_Functions.Log (
                               Best_Upper_Radius
                             );
                        end if;

                        if Verbose then
                           Text_IO.Put_Line ("  <= Done");
                           Text_IO.New_Line;
                           Text_IO.Put (" ");
                           Print_Lower (Low);
                           Text_IO.Put (" <= spectral radius <=");
                           Print_Upper (High);
                           Text_IO.New_Line;
                           Text_IO.Put ("  Entropy (natural logarithm):");
                           Float_Text_IO.Put (
                             Numerics.Elementary_Functions.Log (
                               Spectral_Radius
                             ),
                             Aft => Num_Digits + 2,
                             Exp => 0
                           );
                           Text_IO.New_Line (2);
                           Text_IO.Flush;
                        end if;
                     exception
                        when Arnoldi.Arnoldi_Error =>
                           Text_IO.Put_Line ("  <= Failed");
                           Text_IO.New_Line;
                           Text_IO.Flush;
                        when E : others =>
                           Text_IO.Put_Line ("  <= Failed");
                           Text_IO.New_Line;
                           Text_IO.Put_Line (
                             Exceptions.Exception_Information (E)
                           );
                           Text_IO.New_Line;
                           Text_IO.Flush;
                           raise;
                     end;

                     if Verbose then
                        Text_IO.Put_Line ("  => Trimming matrix");
                        Text_IO.Flush;
                     end if;

                     begin
                        Trim_Partition (Matrix.all);

                        if Verbose then
                           Text_IO.Put_Line ("  <= Done");
                           Text_IO.New_Line;
                           Text_IO.Put_Line (
                             "  New size:" & Integer'Image (
                               Partition.Element_Count
                             )
                           );
                           Text_IO.New_Line;
                           Text_IO.Flush;
                        end if;

                     exception
                        when E : others =>
                           Text_IO.Put_Line ("  <= Failed");
                           Text_IO.New_Line;
                           Text_IO.Put_Line (
                             Exceptions.Exception_Information (E)
                           );
                           Text_IO.New_Line;
                           raise;
                     end;

                     Free (Matrix);

                  end Calculate_Upper_Transition_Matrix;

               exit Calculate_Spectral_Radius
                  when Average_Improvement < Smallest_Improvement or
                       Best_Upper_Entropy - Best_Lower_Entropy <= Accuracy;

               if Verbose then
                  Text_IO.Put_Line ("  => Computing lower transition matrix");
                  Text_IO.Flush;
               end if;

               Calculate_Lower_Transition_Matrix :
                  declare
                     Matrix : Transition_Matrices.Transition_Matrix_Pointer :=
                       Lower_Transition_Matrix;
                     Spectral_Radius : Float;
                  begin
                     if Verbose then
                        Text_IO.Put_Line ("  <= Done");
                        Text_IO.New_Line;
                        Text_IO.Put_Line (
                          "  Size:" & Integer'Image (Matrix.Size)
                        );
                        Text_IO.New_Line;
                        Text_IO.Flush;
                     end if;

                     if Verbose then
                        Text_IO.Put_Line ("  => Computing spectral radius");
                        Text_IO.Flush;
                     end if;

                     begin
                        Transition_Matrices.Spectral_Radius.Estimate (
                          Matrix.all,
                          Low,
                          High
                        );
                        Spectral_Radius := Low;

                        if Spectral_Radius > Best_Lower_Radius then
                           Best_Lower_Radius := Spectral_Radius;
                           Old_Best_Lower_Entropy := Best_Lower_Entropy;
                           Best_Lower_Entropy :=
                             Numerics.Elementary_Functions.Log (
                               Best_Lower_Radius
                             );
                        end if;

                        if Verbose then
                           Text_IO.Put_Line ("  <= Done");
                           Text_IO.New_Line;
                           Text_IO.Put (" ");
                           Print_Lower (Low);
                           Text_IO.Put (" <= spectral radius <=");
                           Print_Upper (High);
                           if Spectral_Radius > 0.0 then
                              Text_IO.New_Line;
                              Text_IO.Put ("  Entropy (natural logarithm):");
                              Float_Text_IO.Put (
                                Numerics.Elementary_Functions.Log (
                                  Spectral_Radius
                                ),
                                Aft => Num_Digits + 2,
                                Exp => 0
                              );
                           end if;
                           Text_IO.New_Line (2);
                           Text_IO.Flush;
                        end if;
                     exception
                        when Arnoldi.Arnoldi_Error =>
                           Text_IO.Put_Line ("  <= Failed");
                           Text_IO.New_Line;
                           Text_IO.Flush;
                        when E : others =>
                           Text_IO.Put_Line ("  <= Failed");
                           Text_IO.New_Line;
                           Text_IO.Put_Line (
                             Exceptions.Exception_Information (E)
                           );
                           Text_IO.New_Line;
                           Text_IO.Flush;
                           raise;
                     end;

                     Free (Matrix);
                  end Calculate_Lower_Transition_Matrix;

               pragma Assert (Best_Lower_Radius <= Best_Upper_Radius);

               if Verbose then
                  Text_IO.Put ("  Best estimate so far:");
                  Print_Lower (Best_Lower_Entropy);
                  Text_IO.Put (" <= Entropy <=");
                  Print_Upper (Best_Upper_Entropy);
                  Text_IO.New_Line (2);
                  Text_IO.Flush;
               end if;

               Average_Improvement := Decay_Multiplier * Average_Improvement +
                 (Old_Best_Upper_Entropy - Old_Best_Lower_Entropy) -
                 (Best_Upper_Entropy - Best_Lower_Entropy);

               exit Calculate_Spectral_Radius
                  when Average_Improvement < Smallest_Improvement or
                       Best_Upper_Entropy - Best_Lower_Entropy <= Accuracy;

               if Verbose then
                  Text_IO.Put_Line ("  => Refining partition");
                  Text_IO.Flush;
               end if;

               Partition.Refine;
               Count := Partition.Element_Count;
               Depth := Depth + 1;

               if Verbose then
                  Text_IO.Put_Line ("  <= Done");
                  Text_IO.New_Line;

                  Text_IO.Put ("  Partition depth: ");
                  Integer_Text_IO.Put (Depth, Width => 0);
                  Text_IO.New_Line;
                  Text_IO.Put ("  Number of partition elements: ");
                  Integer_Text_IO.Put (Count, Width => 0);
                  Text_IO.New_Line (2);

                  Text_IO.Flush;
               end if;
            end loop Calculate_Spectral_Radius;
      end Estimate_Spectral_Radius;

   if Verbose then
      Text_IO.Put_Line ("<= Done");
      Text_IO.New_Line;
      Text_IO.Put ("Best upper spectral radius:");
      Print_Upper (Best_Upper_Radius);
      Text_IO.New_Line;
      Text_IO.Put ("Best lower spectral radius:");
      Print_Lower (Best_Lower_Radius);
      Text_IO.New_Line (2);
      Text_IO.Put ("Entropy (natural logarithm):");
   end if;

   Print_Lower (Best_Lower_Entropy);
   Text_IO.Put (" <= Entropy <=");
   Print_Upper (Best_Upper_Entropy);
   Text_IO.New_Line;
   Text_IO.Flush;

   Partition.Delete_All_Elements;
exception
   when E : others =>
      if Verbose then
         Text_IO.Set_Output (Text_IO.Standard_Error);
         Text_IO.Put_Line ("------------------");
         Text_IO.Put_Line ("-- BUG DETECTED --");
         Text_IO.Put_Line ("------------------");
         Text_IO.Put_Line (Exceptions.Exception_Information (E));
         Text_IO.New_Line;
      end if;

      if Verbose then
         Text_IO.Put ("Best entropy estimate so far: ");
      end if;

      Print_Lower (Best_Lower_Entropy);
      Text_IO.Put (" <= Entropy <=");
      Print_Upper (Best_Upper_Entropy);

      if not Verbose then
         Text_IO.Put (" (error)");
      end if;
      Text_IO.New_Line;

      if Verbose then
         Text_IO.Put ("Partition depth reached: ");
         Integer_Text_IO.Put (Depth, Width => 0);
         Text_IO.New_Line;
      end if;

      Text_IO.Flush;

      Partition.Delete_All_Elements;
end Calculate_Entropy;
with Multi_precision_integers; use Multi_precision_integers;

package body Gautier_Support is
   function Multi (small: Basic_int; n: Index_int) return Multi_int is
      Result : Multi_int (n);
   begin
      Fill (Result, Multi (small));
      return Result;
   end Multi;
end Gautier_Support;
---------
-- GCD --
---------

function GCD (Left, Right : Integer) return Integer is
   A : Integer := abs Left;
   B : Integer := abs Right;
   C : Integer;
begin
   if A = B then
      return A;
   elsif A < B then
      C := A;
      A := B;
      B := C;
   end if;

   loop
      if B = 0 then
         return A;
      end if;

      C := B;
      B := A rem B;
      A := C;
   end loop;
end GCD;
with Interfaces.C; use Interfaces.C;

package body IEEE is
   function tonearest return int;
   pragma Import (C, tonearest);

   function upward return int;
   pragma Import (C, upward);

   function downward return int;
   pragma Import (C, downward);

   function towardzero return int;
   pragma Import (C, towardzero);

   Value : constant array (Rounding_Mode) of int := (
     To_Nearest   => tonearest,
     Upwards      => upward,
     Downwards    => downward,
     Towards_Zero => towardzero
   );

   -----------------------
   -- Get_Rounding_Mode --
   -----------------------
   function fegetround return int;
   pragma Import (C, fegetround);

   function Get_Rounding_Mode return Rounding_Mode is
      Result : int;
   begin
      Result := fegetround;

      for Mode in Rounding_Mode loop
         if Value (Mode) = Result then
            return Mode;
         end if;
      end loop;

      raise Program_Error;
   end Get_Rounding_Mode;

   -----------------------
   -- Set_Rounding_Mode --
   -----------------------
   function fesetround (round : int) return int;
   pragma Import (C, fesetround);

   procedure Set_Rounding_Mode (Mode : Rounding_Mode) is
      Result : int;
   begin
      Result := fesetround (Value (Mode));

      if Result /= 0 then
         raise Unsupported_Mode;
      end if;
   end Set_Rounding_Mode;
end IEEE;
package body Integers.IO is
   ---------
   -- Put --
   ---------
   procedure Put (Source : Integer_Type) is
   begin
      Big_Numbers.Big_Signed_IO.PUT (Big_Numbers.Big_Signed (Source));
   end Put;
end Integers.IO;
package body Integers is
   use Big_Numbers;
   use Signed_Number;

   -------
   -- = --
   -------
   function "="  (Left, Right : Integer_Type) return Boolean is
   begin
      return "=" (Big_Signed (Left), Big_Signed (Right));
   end "=";

   -------
   -- < --
   -------
   function "<"  (Left, Right : Integer_Type) return Boolean is
   begin
      return "<" (Big_Signed (Left), Big_Signed (Right));
   end "<";

   --------
   -- <= --
   --------
   function "<=" (Left, Right : Integer_Type) return Boolean is
   begin
      return "<=" (Big_Signed (Left), Big_Signed (Right));
   end "<=";

   -------
   -- > --
   -------
   function ">"  (Left, Right : Integer_Type) return Boolean is
   begin
      return ">" (Big_Signed (Left), Big_Signed (Right));
   end ">";

   --------
   -- >= --
   --------
   function ">=" (Left, Right : Integer_Type) return Boolean is
   begin
      return ">=" (Big_Signed (Left), Big_Signed (Right));
   end ">=";

   ----------
   -- Sign --
   ----------
   function Sign (Right : Integer_Type) return Integer is
   begin
      if Is_Null (Big_Signed (Right)) then
         return 0;
      elsif Right.Positive then
         return 1;
      else
         return -1;
      end if;
   end Sign;

   -----------------
   -- Is_Negative --
   -----------------
   function Is_Negative (Right : Integer_Type) return Boolean is
   begin
      return Sign (Right) < 0;
   end Is_Negative;

   -----------------
   -- Is_Positive --
   -----------------
   function Is_Positive (Right : Integer_Type) return Boolean is
   begin
      return Sign (Right) > 0;
   end Is_Positive;

   -------------
   -- Is_Zero --
   -------------
   function Is_Zero (Right : Integer_Type) return Boolean is
   begin
      return Is_Null (Big_Signed (Right));
   end Is_Zero;

   ------------
   -- Negate --
   ------------
   function Negate (Right : Integer_Type) return Integer_Type is
   begin
      return Integer_Type (Opposite (Big_Signed (Right)));
   end Negate;

   ---------
   -- abs --
   ---------
   function "abs" (Right : Integer_Type) return Integer_Type is
   begin
      return Integer_Type (Absolute (Big_Signed (Right)));
   end "abs";

   -------
   -- + --
   -------
   function "+"  (Left, Right : Integer_Type) return Integer_Type is
   begin
      return Integer_Type ("+" (Big_Signed (Left), Big_Signed (Right)));
   end "+";

   -------
   -- - --
   -------
   function "-"  (Left, Right : Integer_Type) return Integer_Type is
   begin
      return Integer_Type ("-" (Big_Signed (Left), Big_Signed (Right)));
   end "-";

   -------
   -- * --
   -------
   function "*"  (Left, Right : Integer_Type) return Integer_Type is
   begin
      return Integer_Type ("*" (Big_Signed (Left), Big_Signed (Right)));
   end "*";

   -------
   -- / --
   -------
   function "/"  (Left, Right : Integer_Type) return Integer_Type is
   begin
      return Integer_Type ("/" (Big_Signed (Left), Big_Signed (Right)));
   end "/";

   ---------
   -- rem --
   ---------
   function "rem"  (Left, Right : Integer_Type) return Integer_Type is
      Quotient, Result : Big_Signed;
   begin
      Big_Div (Big_Signed (Left), Big_Signed (Right), Quotient, Result);
      return Integer_Type (Result);
   end "rem";

   ---------------------
   -- To_Integer_Type --
   ---------------------
   function Convert_Natural (X : Natural) return Integer_Type;

   function Convert_Natural (X : Natural) return Integer_Type is
   begin
      if X = 0 then
         return Zero;
      elsif X = 1 then
         return One;
      else
         declare
            Spare : Integer_Type := Convert_Natural (X / 2);
         begin
            Spare := Spare + Spare;
            if X mod 2 = 0 then
               return Spare;
            else
               return Spare + One;
            end if;
         end;
      end if;
   end Convert_Natural;

   function To_Integer_Type (X : Integer) return Integer_Type is
   begin
      if X >= 0 then
         return Convert_Natural (X);
      else
         return Negate (Convert_Natural (-X));
      end if;
   end To_Integer_Type;

   ---------
   -- GCD --
   ---------
   function GCD (Left, Right : Integer_Type) return Integer_Type is
      A : Integer_Type := abs Left;
      B : Integer_Type := abs Right;
      C : Integer_Type;
   begin
      if A = B then
         return A;
      elsif A < B then
         C := A;
         A := B;
         B := C;
      end if;

      loop
         if Is_Zero (B) then
            return A;
         end if;

         C := B;
         B := A rem B;
         A := C;
      end loop;
   end GCD;
end Integers;
with Integers;

function Intersect (
  Polygon : Polygon_Type;
  Point   : Point_Type
) return Polygon_Type is
   use Integers;

   function Interpolate (X, Y : Point_Type) return Point_Type;
   --  Find a point on the hyperplane perpendicular to Point on the
   --  geodesic between X and Y

   function Interpolate (X, Y : Point_Type) return Point_Type is
      Lambda_X : constant Integer_Type := Dot_Product (X, Point);
      Lambda_Y : constant Integer_Type := Dot_Product (Y, Point);
   begin
      if Is_Zero (Lambda_X) and Is_Zero (Lambda_Y) then
         return X; -- X and Y both in hyperplane.
      elsif Lambda_Y > Lambda_X then
         return Linear_Combination (Lambda_Y, X, Negate (Lambda_X), Y);
      else
         return Linear_Combination (Negate (Lambda_Y), X, Lambda_X, Y);
      end if;
   end Interpolate;

   function Is_Positive (X : Point_Type) return Boolean;
   pragma Inline (Is_Positive);

   function Is_Positive (X : Point_Type) return Boolean is
   begin
      return Is_Positive (Dot_Product (Point, X));
   end Is_Positive;

   First_Positive : Natural := 0;
   --  First positive vertex.

   First_Negative : Natural := 0;
   --  First non-positive vertex following First_Positive.

   N : Natural renames Polygon.Number_Of_Vertices;

   Num_Positive : Positive;
begin
   if Is_Empty (Polygon) then
      return Polygon;
   end if;

   --  Since Polygon has non-empty interior, we must have N >= 3.
   pragma Assert (N >= 3);

   --  Set up First_Positive and First_Negative.
   --  Avoid evaluating Dot_Products more than we have to.

   declare
      Boundary_Count : Natural := 0;
   begin
      for I in 1 .. N loop
         declare
            Dot : Integer_Type renames
              Dot_Product (Point, Get_Vertex (I, Polygon));
         begin
            if Is_Positive (Dot) then
               First_Positive := I;
               exit;
            elsif Is_Zero (Dot) then
               Boundary_Count := Boundary_Count + 1;
            end if;
         end;
      end loop;

      if First_Positive = 0 then -- No positive points.
         if Boundary_Count < N then
            --  Polygon does not intersect the halfspace.
            return Empty_Polygon;
         else -- The polygon's boundary coincides with that of the halfspace.
            if Is_Positive (
              Cross_Product (Get_Vertex (1, Polygon), Get_Vertex (2, Polygon))
            ) then
               return Polygon; -- Polygon and halfspace coincide.
            else
               return Empty_Polygon; -- Polygon is complement of the halfspace.
            end if;
         end if;
      end if;
   end;

   if First_Positive = 1 then
   --  Step backward from 1 to find first positive vertex.
      for I in reverse 2 .. N loop
         if not Is_Positive (Get_Vertex (I, Polygon)) then
            First_Positive := I + 1;
            exit;
         end if;
      end loop;

      if First_Positive = 1 then -- Polygon entirely contained in halfspace.
         return Polygon;
      else
         --  Find first non-positive vertex; avoid reevaluating Is_Positive
         --  at vertex number First_Positive - 1, which we know to be false.
         for I in 2 .. First_Positive - 2 loop
            if not Is_Positive (Get_Vertex (I, Polygon)) then
               First_Negative := I;
               exit;
            end if;
         end loop;

         if First_Negative = 0 then
            First_Negative := First_Positive - 1;
         end if;

         if First_Positive = N + 1 then
            First_Positive := 1;
         end if;
      end if;
   else -- First_Positive > 1.
      for I in First_Positive + 1 .. N loop
         if not Is_Positive (Get_Vertex (I, Polygon)) then
            First_Negative := I;
            exit;
         end if;
      end loop;

      if First_Negative = 0 then
         First_Negative := 1;
      end if;
   end if;

   Num_Positive := (First_Negative - First_Positive) mod N;

   declare
      procedure Copy_Positive (Target : in out Polygon_Type);
      pragma Inline (Copy_Positive);

      procedure Copy_Positive (Target : in out Polygon_Type) is
      begin
         for I in 1 .. Num_Positive loop
            Set_Vertex (
              I,
              Get_Vertex (First_Positive + I - 1, Polygon),
              Target
            );
         end loop;
      end Copy_Positive;

      New_First : constant Point_Type := Interpolate (
        Get_Vertex (First_Positive - 1, Polygon),
        Get_Vertex (First_Positive, Polygon)
      );
      New_Last : constant Point_Type := Interpolate (
        Get_Vertex (First_Negative - 1, Polygon),
        Get_Vertex (First_Negative, Polygon)
      );
   begin
      if Equivalent (New_First, New_Last) then
         declare
            Result : Polygon_Type (Num_Positive + 1);
         begin
            Copy_Positive (Result);

            Set_Vertex (Num_Positive + 1, New_Last, Result);

            return Result;
         end;
      elsif Equivalent (New_First, Negate (New_Last)) then
         --  Opposing points: need to subdivide.
         declare
            Result : Polygon_Type (Num_Positive + 3);
         begin
            Copy_Positive (Result);

            Set_Vertex (Num_Positive + 1, New_Last, Result);

            Set_Vertex (
              Num_Positive + 2,
              Cross_Product (New_First, Point),
              Result
            );

            Set_Vertex (Num_Positive + 3, New_First, Result);

            return Result;
         end;
      else
         declare
            Result : Polygon_Type (Num_Positive + 2);
         begin
            Copy_Positive (Result);

            Set_Vertex (Num_Positive + 1, New_Last, Result);
            Set_Vertex (Num_Positive + 2, New_First, Result);

            return Result;
         end;
      end if;
   end;
end Intersect;
with Integers;

procedure Intersects_Halfspace (
  Polygon  : in     Polygons.Polygon_Type;
  Point    : in     Points.Point_Type;
  Forward  :    out Boolean;
  Backward :    out Boolean
) is
   use Integers;

   N : Natural renames Polygon.Number_Of_Vertices;
begin
   Forward  := False;
   Backward := False;

   if Is_Empty (Polygon) then
      return;
   end if;

   --  Since Polygon has non-empty interior, we must have N >= 3.
   pragma Assert (N >= 3);

   for I in 1 .. N loop
      declare
         Dot : Integer_Type renames
           Dot_Product (Point, Get_Vertex (I, Polygon));
      begin
         if not Forward and then Is_Positive (Dot) then
            Forward := True;
         end if;

         if not Backward and then Is_Negative (Dot) then
            Backward := True;
         end if;
      end;

      if Forward and Backward then
         return;
      end if;
   end loop;

   if Forward or Backward then
      return;
   end if;

   --  The polygon's boundary coincides with that of the halfspace.
   Forward := Is_Positive (
                Dot_Product (
                  Point,
                  Cross_Product (
                    Get_Vertex (1, Polygon),
                    Get_Vertex (2, Polygon)
                  )
                )
              );
   Backward := not Forward;
end Intersects_Halfspace;
package body Linear_Algebra is
   procedure Row_Reduce (
     Matrix : in out Matrix_Type;
     Rows   :    out Row_Permutation;
     Cols   :    out Col_Permutation;
     Rank   :    out Rank_Range;
     Strict : in     Boolean
   ) is
      Zeros : array (Row_Index, Col_Index) of Boolean;

      Col_Count : array (Col_Index) of Integer;
      Row_Count : array (Row_Index) of Integer;

      Top, Bottom : Row_Index;
      Left, Right : Col_Index;

      M, N : Natural;

      Zero_Rows, Zero_Cols : Natural;
   begin
      Rank := 0;

      for I in Row_Index loop
         Rows (I) := I;
      end loop;

      for J in Col_Index loop
         Cols (J) := J;
      end loop;

      Top := Row_Index'First;
      Bottom := Row_Index'Last;

      Left := Col_Index'First;
      Right := Col_Index'Last;

      loop
         M := Bottom - Top + 1;
         N := Right - Left + 1;

         exit when M = 0 or N = 0;

         for I in Top .. Bottom loop
            Row_Count (I) := 0;
         end loop;

         for J in Left .. Right loop
            Col_Count (J) := 0;
         end loop;

         for J in Left .. Right loop
            for I in Top .. Bottom loop
               declare
                  R : constant Row_Index := Rows (I);
                  C : constant Col_Index := Cols (J);
                  Z : constant Boolean := Is_Zero (Matrix (R, C));
               begin
                  Zeros (R, C) := Z;
                  if Z then
                     Row_Count (I) := Row_Count (I) + 1;
                     Col_Count (J) := Col_Count (J) + 1;
                  end if;
               end;
            end loop;
         end loop;

         Zero_Rows := 0;
         for I in Top .. Bottom loop
            if Row_Count (I) = N then
               Row_Count (I) := -1; -- ensure zero rows come last
               Zero_Rows := Zero_Rows + 1;
            end if;
         end loop;

         Zero_Cols := 0;
         for J in Left .. Right loop
            if Col_Count (J) = M then
               Zero_Cols := Zero_Cols + 1;
            end if;
         end loop;

         M := M - Zero_Rows;
         N := N - Zero_Cols;

         pragma Assert (
           ((M = 0 or N = 0) and (M = 0 and N = 0)) or (M /= 0 and N /= 0)
         );

         exit when M = 0 or N = 0;

         Rank := Rank + 1;

         --  Bubble sort the rows.  Non-zero rows with most zeros come first.
         declare
            Switched : Boolean;
            Temp1 : Integer;
            Temp2 : Row_Index;
         begin
            loop
               Switched := False;

               for I in Top .. Bottom - 1 loop
                  if Row_Count (I) < Row_Count (I + 1) then
                     Temp1 := Row_Count (I);
                     Temp2 := Rows (I);
                     Row_Count (I) := Row_Count (I + 1);
                     Rows (I) := Rows (I + 1);
                     Row_Count (I + 1) := Temp1;
                     Rows (I + 1) := Temp2;
                     Switched := True;
                  end if;
               end loop;

               exit when not Switched;
            end loop;
         end;

         --  Bubble sort the columns.  Rows with least zeros come first.
         declare
            Switched : Boolean;
            Temp1 : Integer;
            Temp2 : Col_Index;
         begin
            loop
               Switched := False;

               for J in Left .. Right - 1 loop
                  if Col_Count (J) > Col_Count (J + 1) then
                     Temp1 := Col_Count (J);
                     Temp2 := Cols (J);
                     Col_Count (J) := Col_Count (J + 1);
                     Cols (J) := Cols (J + 1);
                     Col_Count (J + 1) := Temp1;
                     Cols (J + 1) := Temp2;
                     Switched := True;
                  end if;
               end loop;

               exit when not Switched;
            end loop;
         end;

         Bottom := Top + M - 1;
         Right := Left + N - 1;

         --  Ensure non-zero top-left element
         if Zeros (Rows (Top), Cols (Left)) then
            declare
               L_Col : constant Col_Index := Cols (Left);
               Temp1 : Integer;
               Temp2 : Row_Index;
            begin
               for I in Top + 1 .. Bottom loop
                  if not Zeros (Rows (I), L_Col) then
                     Temp1 := Row_Count (Top);
                     Temp2 := Rows (Top);
                     Row_Count (Top) := Row_Count (I);
                     Rows (Top) := Rows (I);
                     Row_Count (I) := Temp1;
                     Rows (I) := Temp2;
                     exit;
                  end if;
               end loop;
            end;
         end if;

         declare
            T_Row : constant Row_Index := Rows (Top);
            L_Col : constant Col_Index := Cols (Left);
            Alpha : Integer_Type renames Matrix (T_Row, L_Col);
            Alpha_Is_One : Boolean;
            Starting_Row : Row_Index;
         begin
            pragma Assert (not Is_Zero (Alpha));

            if Row_Count (Top) = N + Zero_Cols - 1 then
               Alpha := One;
               Alpha_Is_One := True;
            else
               Alpha_Is_One := Alpha = One;
            end if;

            if Strict then
               Starting_Row := Row_Index'First;
            else
               Starting_Row := Top + 1;
            end if;

            for I in Starting_Row .. Bottom loop
               if I /= Top then
                  declare
                     R : constant Row_Index := Rows (I);
                     Alpha_Prime : Integer_Type renames Matrix (R, L_Col);
                  begin
                     if not Is_Zero (Alpha_Prime) then
                        if I < Top and not Alpha_Is_One then
                           for J in Col_Index'First .. Left - 1 loop
                              declare
                                 C : constant Col_Index := Cols (J);
                              begin
                                 Matrix (R, C) := Alpha * Matrix (R, C);
                              end;
                           end loop;

                           for J in Right + 1 .. Col_Index'Last loop
                              declare
                                 C : constant Col_Index := Cols (J);
                              begin
                                 Matrix (R, C) := Alpha * Matrix (R, C);
                              end;
                           end loop;
                        end if;

                        for J in Left + 1 .. Right loop
                           declare
                              C : constant Col_Index := Cols (J);
                           begin
                              if Zeros (T_Row, C) then
                                 if not Alpha_Is_One then
                                    Matrix (R, C) := Alpha * Matrix (R, C);
                                 end if;
                              else
                                 if Alpha_Is_One then
                                    Matrix (R, C) := Matrix (R, C)
                                      - Alpha_Prime * Matrix (T_Row, C);
                                 else
                                    Matrix (R, C) := Alpha * Matrix (R, C)
                                      - Alpha_Prime * Matrix (T_Row, C);
                                 end if;
                              end if;
                           end;
                        end loop;
                        Matrix (R, L_Col) := Zero;
                     end if;
                  end;
               end if;
            end loop;
         end;

         exit when Top = Bottom or Left = Right;

         Top := Top + 1;
         Left := Left + 1;
      end loop;
   end Row_Reduce;
end Linear_Algebra;
with Ada.Unchecked_Deallocation;
with Lozi;
with Partition;
with Partition.Process_Transitions;
with Points;
with Polygons;
with Segments_Intersect;
with Symbolics;

function Lower_Transition_Matrix
  return Transition_Matrices.Transition_Matrix_Pointer is
   use Partition;
   use Points;
   use Polygons;
   use Symbolics;
   use Transition_Matrices;

   Polygon_Count : constant Natural := Element_Count;

   type Side_Type;

   type Side_Pointer is access Side_Type;

   type Side_Type is record
      First  : Positive;
      Second : Positive;
      Number : Positive;
      Next   : Side_Pointer;
   end record;

   type Side_Array is array (Positive range <>) of Side_Pointer;

   type Side_Array_Pointer is access Side_Array;

   Polygon_Sides : Side_Array_Pointer := new Side_Array (1 .. Polygon_Count);

   Last_Number : Natural := 0;

   function Get_Number (Polygon_Index, Side_1, Side_2 : Positive)
     return Positive;

   function Get_Number (Polygon_Index, Side_1, Side_2 : Positive)
     return Positive is
      Side : Side_Pointer := Polygon_Sides (Polygon_Index);
      First_Side : constant Positive := Positive'Min (Side_1, Side_2);
      Second_Side : constant Positive := Positive'Max (Side_1, Side_2);
   begin
      while Side /= null loop
         if Side.First = First_Side and Side.Second = Second_Side then
            return Side.Number;
         end if;
         Side := Side.Next;
      end loop;

      Last_Number := Last_Number + 1;
      Polygon_Sides (Polygon_Index) := new Side_Type'(
        First  => First_Side,
        Second => Second_Side,
        Number => Last_Number,
        Next   => Polygon_Sides (Polygon_Index)
      );
      return Last_Number;
   end Get_Number;

   type Edge_Type;

   type Edge_Pointer is access Edge_Type;

   type Edge_Type is record
      From : Positive;
      To   : Positive;
      Next : Edge_Pointer;
   end record;

   Edge_List : Edge_Pointer;

   procedure Add_Edge (
     From_Index, From_Side_1, From_Side_2 : Positive;
     To_Index,   To_Side_1,   To_Side_2   : Positive
   );
   pragma Inline (Add_Edge);

   procedure Add_Edge (
     From_Index, From_Side_1, From_Side_2 : Positive;
     To_Index,   To_Side_1,   To_Side_2   : Positive
   ) is
      From_Number : constant Positive :=
        Get_Number (From_Index, From_Side_1, From_Side_2);
      To_Number : constant Positive :=
        Get_Number (To_Index, To_Side_1, To_Side_2);
   begin
      Edge_List := new Edge_Type'(
        From => From_Number,
        To   => To_Number,
        Next => Edge_List
      );
   end Add_Edge;

   procedure Action (
     From_Polygon : Polygon_Type;
     From_Index   : Positive;
     From_Image   : Polygon_Type;
     To_Polygon   : Polygon_Type;
     To_Index     : Positive;
     Total_Symbol : Sequence_Type
   );

   procedure Action (
     From_Polygon : Polygon_Type;
     From_Index   : Positive;
     From_Image   : Polygon_Type;
     To_Polygon   : Polygon_Type;
     To_Index     : Positive;
     Total_Symbol : Sequence_Type
   ) is
      pragma Unreferenced (Total_Symbol);

      A, B, C, D, AP, BP, CP, DP : Point_Type;
      Orientation_Preserving : constant Boolean := Lozi.Preserves_Orientation;
   begin
      for K in 1 .. To_Polygon.Number_Of_Vertices - 2 loop
         AP := Get_Vertex (K, To_Polygon);
         BP := Get_Vertex (K + 1, To_Polygon);

         for L in K + 2 .. To_Polygon.Number_Of_Vertices loop
            exit when K = 1 and L = To_Polygon.Number_Of_Vertices;
            CP := Get_Vertex (L, To_Polygon);
            DP := Get_Vertex (L + 1, To_Polygon);

            Search_Source :
            for I in 1 .. From_Polygon.Number_Of_Vertices - 2 loop
               if Orientation_Preserving then
                  A := Get_Vertex (I, From_Image);
                  B := Get_Vertex (I + 1, From_Image);
               else
                  A := Get_Vertex (-I, From_Image);
                  B := Get_Vertex (-(I + 1), From_Image);
               end if;

               for J in I + 2 .. From_Polygon.Number_Of_Vertices loop
                  exit when I = 1 and J = From_Polygon.Number_Of_Vertices;
                  if Orientation_Preserving then
                     C := Get_Vertex (J, From_Image);
                     D := Get_Vertex (J + 1, From_Image);
                  else
                     C := Get_Vertex (-J, From_Image);
                     D := Get_Vertex (-(J + 1), From_Image);
                  end if;
                  if Segments_Intersect (A, B, BP, CP) and then
                       Segments_Intersect (A, B, DP, AP) and then
                         Segments_Intersect (C, D, BP, CP) and then
                           Segments_Intersect (C, D, DP, AP) then
                     Add_Edge (From_Index, I, J, To_Index, K, L);
                     --  For each From_Index, there is at most one transition
                     --  to a given (To_Index, K, L), thus the following jump.
                     exit Search_Source;
                  end if;
               end loop;

            end loop Search_Source;

         end loop;

      end loop;
   end Action;

   procedure Calculate_Transitions is new Process_Transitions (Action);

   Matrix : Transition_Matrix_Pointer;
begin
   Calculate_Transitions;

   declare
      procedure Free is new Ada.Unchecked_Deallocation (
        Side_Type,
        Side_Pointer
      );

      procedure Free is new Ada.Unchecked_Deallocation (
        Side_Array,
        Side_Array_Pointer
      );

      Side, Old_Side : Side_Pointer;
   begin
      for I in Polygon_Sides'Range loop
         Side := Polygon_Sides (I);
         while Side /= null loop
            Old_Side := Side;
            Side := Side.Next;
            Free (Old_Side);
         end loop;
      end loop;

      Free (Polygon_Sides);
   end;

   Matrix := new Transition_Matrix_Type (Last_Number);

   --  Calculate the matrix
   declare
      procedure Free is new Ada.Unchecked_Deallocation (
        Edge_Type,
        Edge_Pointer
      );

      Edge, Old_Edge : Edge_Pointer;
   begin
      Edge := Edge_List;
      while Edge /= null loop
         Set_Transition (
           From   => Edge.From,
           To     => Edge.To,
           Matrix => Matrix.all
         );
         Old_Edge := Edge;
         Edge := Edge.Next;
         Free (Old_Edge);
      end loop;
   end;

   return Matrix;
end Lower_Transition_Matrix;
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
-----------------------------------------------------------------------------
--  File: mupreint.adb; see specification (mupreint.ads)
-----------------------------------------------------------------------------
-- 15-Feb-2002: "zero" bugs fixed by Duncan Sands


-- To-do/bug symbol: !!!

-- with Text_IO; use Text_IO;  --   <--- still here for debugging purposes

package body Multi_precision_integers is
--  package IIO is new Integer_IO(integer); use IIO; --   <--- for debugging

  DEBUG: constant Boolean:= True;

  Internal_error: exception;
  Not_done: exception;

  type compar is (smaller, equal, greater);

  function Min(a,b:index_int) return index_int is
    begin if a<b then return a; else return b; end if; end;

  function Max(a,b:index_int) return index_int is
    begin if a>b then return a; else return b; end if; end;

  procedure Test( m: multi_int; test_last: boolean:= true ) is
    last_nz: index_int:= 0;
    Negative_block, Overflown_block,
    Last_index_has_zero,
    Field_last_outside_range, Field_last_is_negative: exception;
    begin
      if m.zero then return; end if; -- 0, nothing to test
      if m.last_used > m.n then raise Field_last_outside_range; end if;
      if m.last_used <   0 then raise Field_last_is_negative; end if;
      for i in 0 .. m.last_used loop
        if m.blk(i) < 0 then
          raise Negative_block;
        end if;
        if not (m.blk(i) in block) then
          raise Overflown_block;
        end if;
        if m.blk(i) /= 0 then
          last_nz:= i;
        end if;
      end loop;
      if test_last and then 0 < last_nz and then last_nz < m.last_used then
        raise Last_index_has_zero;
      end if; 
    end Test;

  -- Another names (because of randomness in DEC Ada trace-back's line numbers)
  procedure Testarossa( m: multi_int ) is begin Test(m); end;
  procedure Testaverde( m: multi_int ) is begin Test(m); end;
  procedure Testazzuro( m: multi_int ) is begin Test(m); end;

  procedure Reduce_last_nonzero( n: in out multi_int ) is
    old_last: index_int:= n.last_used;
  begin
    if DEBUG then Test(n, test_last=> false); end if;

    if n.zero then   -- We avoid de-zeroing accidentally
      return;        -- and returning a false non-zero with rubbish :-)
    end if;

    n.zero:= True;
    n.last_used:= 0;
    for i in 0 .. old_last loop
      if n.blk(i) /= 0 then
        n.zero:= False;
        n.last_used:= i;      -- NB: can be 0
      end if;
    end loop;
  end Reduce_last_nonzero;

  function Compare_absolute (i1,i2: multi_int) return compar is
    l1, l2: index_int;
  begin
    -- On ne compare que ABS(i1) et ABS(i2)
    l1:= i1.last_used;
    l2:= i2.last_used;
    if l1 > l2 then         -- i1 a plus de blocs non nuls
      return greater;
    elsif l1 < l2 then      -- i1 a moins de blocs non nuls
      return smaller;
    else                       -- i1 et i2 ont le meme nb de blocs
      for i in reverse 0 .. l1 loop -- on parcourt du + signifiant au -
        if    i1.blk(i) > i2.blk(i) then -- <<chiffre>> de i1 plus grand
          return greater;
        elsif i1.blk(i) < i2.blk(i) then -- <<chiffre>> de i1 plus petit
          return smaller;
        end if;
        -- M\^emes chiffres -> au suivant!
      end loop;
      -- Bon, les 2 nombres sont egaux!
      return equal;
    end if;
  end Compare_absolute;

----- Informations, conversions

  function Multi(small: basic_int) return multi_int is
    abss: basic_int:= ABS(small);
    reste: basic_int;
    negs: boolean:= small < 0;
    Conversion_overflow : exception;
    
  begin

    if abss<= maxblock then
      return ( n=>         0,          -- 1 bloc suffit
               blk=>      (0=> abss),  -- le bloc contient le nombre
               neg=>       negs,
               zero=>      small = 0,
               last_used=> 0
             );
    else
      reste:= abss  /  cardblock;
      if reste <= maxblock then
        return ( n=>         1,            -- il faut 2 blocs
                 blk=>      (0=> abss MOD cardblock,   -- bloc 0
                             1=> reste),               -- bloc 1
                 neg=>       negs,
                 zero=>      false,
                 last_used=> 1
               );
      else
        if reste / cardblock > maxblock then
           Raise Conversion_overflow;
        end if;

        return ( n=>         2,  -- il faut 3 blocs (e.g. 31 bits 15+15+1)
                 blk=>      (0=> abss MOD cardblock,   -- bloc 0
                             1=> reste MOD cardblock,  -- bloc 1
                             2=> reste  /  cardblock), -- bloc 2
                 neg=>       negs,
                 zero=>      false,
                 last_used=> 2
               );
      end if;
    end if;
  end;

  -- Convert multi_int to basic_int (when possible, else: Cannot_fit raised)
  function Basic(large:multi_int) return basic_int is
  begin
    if large.zero then return 0; end if; -- <- 17-Feb-2002
    if large.last_used > 0 then raise Cannot_fit; end if;
    return large.blk(0);
  end;

  -- 14-Feb-2002: "zero" bug fixed by Duncan Sands
  procedure Fill(what: out multi_int; with_smaller:multi_int) is
    l: constant index_int:= with_smaller.last_used;
  begin
    if DEBUG then Test(with_smaller); end if;
    what.zero:= with_smaller.zero;

    if with_smaller.zero then
       return;
    end if;

    if what.n < l then
       raise Array_too_small;   -- contenant trop petit
    end if;

    for i in 0 .. l loop -- copie
       what.blk (i) := with_smaller.blk(i);
    end loop;

    what.neg:=  with_smaller.neg;
    what.last_used:= l;
  end Fill;

---------------------------
----- Unary operators -----
---------------------------

  function "+" (i: multi_int) return multi_int is begin return i; end;

  procedure Opp(i: in out multi_int) is
  begin
    i.neg:= NOT i.neg; -- -0 possible
  end Opp;

  function "-" (i: multi_int) return multi_int is 
    res: multi_int(i.n):= i; -- copy + stack :-(
  begin
    Opp(res);
    return res;
  end "-";

  procedure Abso(i: in out multi_int) is
  begin
    i.neg:= False;
  end Abso;

  function "Abs" (i: multi_int) return multi_int is
    abs_i: multi_int(i.n):= i; -- copy + stack :-(
  begin
    if DEBUG then Test(i); end if;
    abs_i.neg:= False;
    return abs_i;
  end "Abs";

  function Sign(i: multi_int) return basic_int is
  begin
    if    i.zero then return  0;
    elsif i.neg  then return -1;
    else              return +1;
    end if;
  end Sign;
  
  function Even(i: multi_int) return boolean is
  begin
    return      i.zero  or else  i.blk(0) MOD 2 = 0;
  end Even;

  function Odd (i: multi_int) return boolean is
  begin
    return (NOT i.zero) and then i.blk(0) MOD 2 = 1;
  end Odd;

----------------------------
----- Binary operators -----
----------------------------

  -- Internal algorithm to add two numbers AS POSITIVE ( > 0 ) !

  procedure Add_absolute(i1,i2: in multi_int; i3: out multi_int) is
    l1: constant index_int:= i1.last_used;
    l2: constant index_int:= i2.last_used;
    min_ind: constant index_int:= Min( l1, l2 );
    max_ind: constant index_int:= Max( l1, l2 );
    retenue_finale, s: basic_int:= 0;

  begin
    if DEBUG then Test(i1); Test(i2); end if;

    if max_ind > i3.n then raise Result_undersized; end if; -- 17-Feb-2002
    
    -- (1) On additionne sur le <<support commun>>
    for ind in 0 .. min_ind loop   
      s:= i1.blk(ind) + i2.blk(ind) + 
              s / cardblock;                            --  (retenue)
      i3.blk(ind):=  s MOD cardblock;
    end loop;

    -- (2) On poursuit au besoin si i1 a plus de blocs...
    if l1 > min_ind then
      for ind in min_ind+1 .. max_ind loop
        s:= i1.blk(ind) +
                s / cardblock;                          --  (retenue)
        i3.blk(ind):=  s MOD cardblock;
      end loop;
    -- ... ou bien si i2 en a plus.
    elsif l2 > min_ind then
      for ind in min_ind+1 .. max_ind loop
        s:= i2.blk(ind) +
                s / cardblock;                          --  (retenue)
        i3.blk(ind):=  s MOD cardblock;
      end loop;
    end if;

    -- (3) Il peut rester une retenue

    retenue_finale:= s / cardblock;
    if retenue_finale /= 0 then
      if max_ind+1 > i3.n then raise Result_undersized; end if; -- 17-Feb-2002
      i3.blk(max_ind+1):= retenue_finale;
      i3.last_used:= max_ind+1;
    else
      i3.last_used:= max_ind;
    end if;

    -- (4) i3 = i1+i2 > 0
    i3.neg:= False;
    i3.zero:= False;

  end Add_absolute;

  -- Internal algorithm to subtract two numbers AS POSITIVE ( > 0 ) !

  procedure Sub_absolute(i1,i2: in multi_int; i3: in out multi_int;
                         sgn: out boolean) is
    l1: constant index_int:= i1.last_used;
    l2: constant index_int:= i2.last_used;
    max_ind: constant index_int:= Max( l1, l2 );
    ai, bi, s: basic_int;

  begin
    if DEBUG then Test(i1); Test(i2); end if;

    if max_ind > i3.n then raise Result_undersized; end if; -- 17-Feb-2002

    i3.last_used:= 0;
    i3.zero:= true;
    s:= 0;

    -- (1) Soustraction avec retenue
    for ind in 0 .. max_ind loop
      if ind <= l1 then ai:= i1.blk(ind);     else ai:= 0; end if;
      if ind <= l2 then bi:= i2.blk(ind) + s; else bi:= s; end if;

      if ai < bi then
        ai:= ai + cardblock;
        s:= 1;
      else
        s:= 0;
      end if;
      
      i3.blk(ind):= ai-bi;
      if ai-bi /= 0 then    -- au passage, on corrige .last_used et .zero
        i3.last_used:= ind;
        i3.zero:= False;
      end if;
    end loop;
    
    -- (2) Traitement de la derni\`ere retenue
    if s = 0 then
      i3.neg := False;
      sgn    := False;
    else
      i3.neg := True;
      sgn    := True;
      i3.last_used:= 0;
      s:= 1; -- on fait "9-chaque chiffre" et on ajoute 1 au tout (s=retenue)
      for i in 0 .. max_ind loop
        s:= maxblock - i3.blk(i) + s;
        i3.blk(i):= s MOD cardblock;
        if i3.blk(i) /= 0 then
          i3.last_used:= i;
        end if;
        s:= s / cardblock;
      end loop;
    end if;

  end Sub_absolute;

  procedure Add(i1,i2: in multi_int; i3: in out multi_int) is
    sgn: Boolean;
  begin
    -- (1) Les cas o\`u i1 ou i2 = 0
    if    i1.zero and i2.zero then i3.zero:= True;
    elsif i1.zero then Fill( i3, i2 );
    elsif i2.zero then Fill( i3, i1 );

    -- (2) Maintenant: i1 /= 0 et i2 /= 0; on regarde les signes

    -- (2.1) Facile: i1 et i2 de m\^eme signe
    elsif i1.neg = i2.neg then
      Add_absolute( i1,i2, i3 ); -- On fait comme si i1>0 et i2>0
      i3.neg:= i1.neg;           -- et on met le bon signe

    -- (2.2) i1 < 0, i2 > 0, donc i3 = i2 - abs(i1)
    elsif i1.neg and not i2.neg then
      Sub_absolute( i2,i1, i3, sgn);

    -- (2.3) i1 > 0, i2 < 0, donc i3 = i1 - abs(i2)
    elsif i2.neg and not i1.neg then
      Sub_absolute( i1,i2, i3, sgn );
    end if;

  end Add;

  function "+" (i1,i2: multi_int) return multi_int is
    somme: multi_int( Max(i1.n, i2.n) + 1 );
  begin
    Add( i1,i2, somme );
    return somme;
  end "+";

  procedure Sub(i1,i2: in multi_int; i3: in out multi_int) is
    sgn: Boolean;
  begin
    -- (1) Les cas o\`u i1 ou i2 = 0
    if    i1.zero and i2.zero then i3.zero:= True;
    elsif i1.zero then Fill( i3, i2 ); i3.neg:= NOT i2.neg;
    elsif i2.zero then Fill( i3, i1 );

    -- (2) Maintenant: i1 /= 0 et i2 /= 0; on regarde les signes

    -- (2.1) Facile: i1 et i2 de m\^eme signe
    elsif i1.neg = i2.neg then
      Sub_absolute( i1,i2, i3, sgn ); -- On fait comme si i1>0 et i2>0
      if i1.neg then                  -- et on met le bon signe
        i3.neg:= NOT sgn;
      else
        i3.neg:= sgn;
      end if;

    -- (2.2) i1 < 0, i2 > 0, donc i3 = i1-i2 = - (abs(i1) + abs(i2))
    elsif i1.neg and not i2.neg then
      Add_absolute( i1,i2, i3 );
      i3.neg:= True;

    -- (2.3) i1 > 0, i2 < 0, donc i3 = i1-i2 = i1 + (-i2) = i1 + abs(i2)
    elsif i2.neg and not i1.neg then
      Add_absolute( i1,i2, i3 );
    end if;

  end Sub;

  function "-" (i1,i2: multi_int) return multi_int is
    diff: multi_int( Max(i1.n, i2.n) + 1); -- +1: retenue possible (add_abs.)
  begin
    Sub( i1,i2, diff );
    return diff;
  end "-";

  function "+" (i1: multi_int; i2: basic_int) return multi_int is
  begin return i1 + Multi(i2); end;

  function "+" (i1: basic_int; i2: multi_int) return multi_int is
  begin return Multi(i1) + i2; end;

  function "-" (i1: multi_int; i2: basic_int) return multi_int is
  begin return i1 - Multi(i2); end;

  function "-" (i1: basic_int; i2: multi_int) return multi_int is
  begin return Multi(i1) - i2; end;
    
  procedure Multiply(i1,i2: in multi_int; i3: in out multi_int) is
    l1: index_int:= i1.last_used;
    l2: index_int:= i2.last_used;
    d,s : long_basic_int;
    k   : index_int;
    res : block_array( i3.blk'range );
    -- res: buffer to avoid problems with Multiply(i,j,i) or Multiply(j,i,i)
  begin
    if DEBUG then Test(i1); Test(i2); end if;
    if l1+l2+2 > i3.n then raise Result_undersized; end if; -- 17-Feb-2002

    if i1.zero or i2.zero then
      i3.zero:= True;
      Return;
    end if;

    for k in res'range loop res(k):= 0; end loop;
    i3.zero:= False;
    i3.last_used:= i3.n;

    for j in 0..l1 loop
      d:= 0;
      k:= j;
      for i in 0..(l2+1) loop
        if i <= l2 then
          d:= d / cardblock +
              long_basic_int(i1.blk(j)) * long_basic_int(i2.blk(i));
        else
          d:= d / cardblock;
        end if;
        s:= long_basic_int(res(k)) + d MOD cardblock;
        res(k):= block(s MOD cardblock); -- somme
        res(k+1):= res(k+1) + block(s / cardblock); -- retenue
        k:= k + 1;
      end loop;
    end loop;

    i3.blk:= res;
    Reduce_last_nonzero( i3 );

    i3.neg:= i1.neg /= i2.neg;

  end Multiply;

  function "*" (i1,i2: multi_int) return multi_int is
  begin
    if i1.zero or i2.zero then
      return Multi(0);
    else
      declare
        prod: Multi_int( i1.last_used + i2.last_used + 2 );
      begin
        Multiply( i1,i2, prod );
        Return prod;
      end;
    end if;
  end "*";

  function "*" (i1: multi_int; i2: basic_int) return multi_int is
  begin return i1 * Multi(i2); end;

  function "*" (i1: basic_int; i2: multi_int) return multi_int is
  begin return Multi(i1) * i2; end;

----- Begin of DIVISION part -----

  -- Interne: Division et reste en 1 coup

  procedure Div_Rem(a,b: long_basic_int; q,r: in out long_basic_int) is
    Conflict_with_REM: exception;
  begin
    q:= a / b;
    r:= a - b*q;
    if DEBUG and then r /= (a rem b) then
      Raise Conflict_with_REM;
    end if;
  end Div_Rem;

  procedure Divide_absolute_normalized ( u: in out multi_int;
                                         v: in     multi_int;
                                         q: in out multi_int  ) is
    qi: index_int      := u.last_used - v.last_used - 1; -- was: q.n; D.S. Feb-2002
    v1: long_basic_int := long_basic_int(v.blk(v.last_used  ));
    v2: long_basic_int := long_basic_int(v.blk(v.last_used-1));

    udigits : block_array renames u.blk;
    vdigits : block_array renames v.blk;

    vlast     : index_int      := v.last_used;
    v1L       : long_basic_int := long_basic_int(v1);
    guess     : long_basic_int ;
    comparand : long_basic_int ;

    function Divide_subtract ( ustart: index_int ) return block is
      ui    : index_int      := ustart;
      carry : long_basic_int := 0;

    begin
      if guess = 0 then
        return 0;
      end if;

      -- On soustrait (le chiffre du quotient) * diviseur au dividende

      for vi in 0 .. vlast loop
        declare
          prod: long_basic_int:= long_basic_int(vdigits(vi)) * guess + carry;
          bpro: block         := block(prod MOD cardblock);
          diff: basic_int     := udigits(ui) - bpro;
        begin
          if diff < 0 then
            udigits(ui) := diff + cardblock;
            carry := (prod / cardblock) + 1;
          else
            udigits(ui) := diff;
            carry := (prod / cardblock);
          end if;
          ui:= ui + 1;
        end;
      end loop;
  
      if carry = 0 then
        return  block(guess MOD cardblock);
      end if;
  
      declare
        diff: basic_int := udigits(ui) - basic_int(carry MOD cardblock);
      begin
          if diff < 0 then
            udigits(ui) := diff + cardblock; -- carry generated
          else
            udigits(ui) := diff;
            return block(guess MOD cardblock);
          end if;
      end;

      -- Carry was generated
      declare
        icarry: basic_int := 0;
      begin
        ui := ustart;
        for vi in 0 .. vlast loop
          declare
            sum: basic_int := vdigits(vi) + udigits(ui) + icarry;
          begin
            udigits(ui) := sum MOD cardblock;
            ui:= ui + 1;
            icarry := sum / cardblock;
          end;
        end loop;

        if icarry = 1 then
          udigits(ui) := (udigits(ui)+1) MOD cardblock;
        end if;
      end;

      return block( (guess-1) MOD cardblock );

    end Divide_subtract;
    
  begin -- Divide_absolute_normalized
  
    -- In this algorithm, we are using q's contents although,
    -- for a while, q.zero = True
  
    for i in q.blk'range loop q.blk(i):= 0; end loop;
    q.last_used:= qi; -- was: q.n; D.S. Feb-2002
    q.zero:= True;

    for j in reverse vlast+1 .. u.last_used loop
      declare
        uj : long_basic_int := long_basic_int(udigits(j));
        uj1: long_basic_int := long_basic_int(udigits(j-1));
        uj2: long_basic_int := long_basic_int(udigits(j-2));
        ujL: long_basic_int;
        rmL: long_basic_int;
      begin
--        if uj = v1 then

-- Code BigInt suspect (resultat faux)
--          guess := cardblock-1;
--          comparand := uj1 * cardblock + uj2 + v1;

-- Code du cas general, adapte au fait que uj=v1=v1L
--          -- ujL / uj = cardblock, donc...
--          guess := cardblock;
--          -- ujL rem uj = uj1
--          comparand := (uj1 * cardblock) + uj2;

--        else -- cas general

          ujL := uj * cardblock + uj1;
          Div_Rem( ujL, v1L, guess, rmL );          
          comparand := (rmL * cardblock) + uj2;
--        end if;
        
        while comparand < v2 * guess loop
          guess:= guess - 1;
          comparand:= comparand + v1L * cardblock;
          exit when comparand > cardblock * cardblock;
        end loop;
        
        q.blk(qi) := Divide_subtract( j - vlast - 1 );

        if DEBUG and then NOT (q.blk(qi) in block) then
          raise Internal_error;
        end if;

        if q.zero and then q.blk(qi) /= 0 then -- n'arrive que 0 ou 1 fois
          q.zero:= false;
          q.last_used:= qi;
        end if;

        qi:= qi - 1;
      end;
      
    end loop; -- j

    if DEBUG then Test(q); end if;

    end Divide_absolute_normalized;

  -- Calculate u/v

  procedure Divide_absolute ( u,v: in     multi_int;
                              q,r: in out multi_int  ) is
    shift: integer:= 0;
    v1: block:= v.blk(v.last_used);
    v_zero, v1_zero: exception;
    u_work: multi_int(u.last_used+2);

    procedure Normalization ( source: in     multi_int;
                              target: in out multi_int ) is
      carry: integer:= 0;
      tl: constant index_int:= target.last_used;
      to_left_factor  : constant integer:= 2 ** shift;
      to_right_factor : constant integer:= cardblock / to_left_factor;
      blk:  block;
      no_room_for_carry: exception;
    begin
      for i in 0 .. source.last_used loop
        blk:= source.blk(i);
        target.blk(i) := ((blk MOD to_right_factor) * to_left_factor) + carry;
        carry         :=   blk  /  to_right_factor;
      end loop;
      -- tabuler les MOD et / pour faire AND et shift !!!
      if source.last_used < tl then
        target.blk(source.last_used+1):= carry;
      end if;
      for i in source.last_used+2 .. tl  loop
        target.blk(i):= 0;
      end loop;
    end Normalization;

    procedure Unnormalization ( m: in out multi_int) is
      carry: integer:= 0;
      to_right_factor : constant integer:= 2 ** shift;
      to_left_factor  : constant integer:= cardblock / to_right_factor;
      blk:  block;
    begin
      for i in reverse 0 .. m.last_used loop
        blk:= m.blk(i);
        m.blk(i) := (blk  /  to_right_factor) + carry;
        carry    := (blk MOD to_right_factor) * to_left_factor;
      end loop;
    end Unnormalization;

  begin -- Divide_absolute (multi u / multi v)

    if DEBUG then
      if v.zero then raise v_zero; end if;
      if v1=0 then raise v1_zero; end if;
    end if;

    -- Calculate shift needed to normalize
    u_work.last_used:= u_work.n;
    u_work.zero:= false;
    while v1 < cardblock/2 loop
      shift:= shift + 1;
      v1:= v1 * 2;
    end loop;
    if shift = 0 then                  -- no shift needed
      u_work.blk( 0 .. u.last_used ):= u.blk( 0 .. u.last_used );
      u_work.blk( u.last_used+1 .. u_work.last_used):= (0,0);
      Divide_absolute_normalized( u_work,v, q );
    else
      declare                          -- shift needed
        v_work: multi_int(v.last_used);
      begin
        v_work.last_used:= v_work.n;
        Normalization( u, u_work );
        Normalization( v, v_work );
        Reduce_last_nonzero( v_work );
        Divide_absolute_normalized( u_work,v_work, q );
        Unnormalization( u_work );
      end;
    end if;
    u_work.neg:= false; -- check friendly
    q.neg:= false; -- check friendly
    Reduce_last_nonzero( u_work );
    Fill( r, u_work );

  end Divide_absolute;

  procedure Divide_absolute_big_small ( u:   in     multi_int;
                                        v:   in     basic_int;
                                        q:   in out multi_int;
                                        r:   in out basic_int ) is
    lr: long_basic_int:= 0;
    ln: long_basic_int;
    lv: long_basic_int:= long_basic_int(v);
    Quotient_constraint_error: exception;
    last_u_nz:  constant index_int:= u.last_used;
    u_zero: constant boolean:= u.zero;
    -- in case u and q are the same variables
  begin
    if q.n < last_u_nz then raise Quotient_constraint_error; end if;
    q.last_used:= 0;
    q.neg:= false;
    q.zero:= True;
    if u_zero then
      r:= 0;
    else
      for i in reverse 0 .. last_u_nz loop
        ln:= long_basic_int(u.blk(i)) + lr * cardblock;
        lr:=             ln MOD lv;
        q.blk(i):= block(ln  /  lv);
        if q.zero and then q.blk(i)/= 0 then 
          q.last_used:= i;
          q.zero:= False;
        end if;
      end loop;
      r:= basic_int(lr);
    end if;
  end Divide_absolute_big_small;

  procedure Solve_signs_for_Div_Rem (i1n,i2n: in boolean; qn,rn: out boolean) is
  begin
    -- Invariant: i1= i2*q+r   on cherche (pos) = (pos)*(pos)+(pos)

    if i1n and i2n then        -- i1<0;  i2<0  (-i1) = (-i2) *  q  + (-r)
      qn:= False; -- Quotient > 0
--      rn:= True;  -- Reste    < 0
    elsif i1n then             -- i1<0;  i2>0  (-i1) =   i2  *(-q) + (-r)
      qn:= True;  -- Quotient < 0
--      rn:= True;  -- Reste    < 0
    elsif i2n then             -- i1>0;  i2<0    i1  = (-i2) *(-q) +   r
      qn:= True;  -- Quotient < 0
--      rn:= False; -- Reste    > 0
    else                       -- i1>0;  i2>0    i1  =   i2  *  q  +   r
      qn:= False; -- Quotient > 0
--      rn:= False; -- Reste    > 0
    end if;
    -- on observe que... "(A rem B) has the sign of A " ARM 4.5.5
    -- en effet on peut mettre:
    rn:= i1n;
  end Solve_signs_for_Div_Rem;

  procedure Div_Rem (i1: in     multi_int; i2: in     basic_int;
                     q : in out multi_int;  r: in out basic_int) is
    i1_neg: constant boolean:= i1.neg;
    -- in case i1 and q are the same variables
    rneg: boolean;
  begin
    if DEBUG then Test(i1); end if;
    if i2=0 then Raise Division_by_zero; end if;

    if i1.zero then -- 15-Feb-2002: 0/i2
      q.zero:= True;
      r:= 0;
      return;
    end if;

    Divide_absolute_big_small( i1, Abs(i2), q,r );

    Solve_signs_for_Div_Rem( i1_neg,i2<0, q.neg, rneg );
    if rneg then r:= -r; end if;

  end Div_Rem;

  procedure Div_Rem (i1,i2: in multi_int; q,r: in out multi_int) is
    Remainder_constraint_error: exception;
    l1: constant index_int:= i1.last_used;
    l2: constant index_int:= i2.last_used;
  begin
    if DEBUG then Test(i1); Test(i2); end if;
    if i2.zero then Raise Division_by_zero; end if;
    
    if i1.zero then -- 15-Feb-2002: 0/i2
      q.zero:= True;
      r.zero:= True;
      return;
    end if;

    if q.n < l1 - l2 or r.n < Max( l1, l2 ) then -- 17-Feb-2002
      raise Result_undersized;
    end if; 

    if l2 = 0 then
      if l1 = 0 then      -- On a affaire a une ridicule division d'entiers
         q.blk(0):= i1.blk(0) / i2.blk(0);
         r.blk(0):= i1.blk(0) - i2.blk(0)*q.blk(0);

         q.blk(0):= Abs(q.blk(0)); -- signes mis a la sortie...
         q.zero:= q.blk(0) = 0;
         q.last_used:= 0;
      else                -- multi / entier
         Div_Rem ( i1, i2.blk(0), q, r.blk(0) );
      end if;
      r.blk(0):= Abs(r.blk(0)); -- signes mis a la sortie...
      r.zero:= r.blk(0) = 0;
      r.last_used:= 0;

    else  -- multi / multi

      case Compare_absolute(i2 , i1) is

        when greater =>
          q.zero:= True;    -- q:=  0;
          q.last_used:= 0;
          q.neg:= false;

          Fill( r, i1 );  -- r:= i1, q:=0  car i1 = 0 * i2 (>i1 en v.abs) + r
          Return;

        when equal =>
          Fill( q, Multi(1) );
          Fill( r, Multi(0) );
          
        when smaller => -- cas <<normal>>: diviseur < dividende
          
          if l2 > r.n then
            Raise Remainder_constraint_error;
          end if;

          Divide_absolute( i1,i2, q,r );
          if DEBUG then Testazzuro(i1); Testaverde(i2); end if;
          Reduce_last_nonzero( r );
          if DEBUG then Test(q); Testarossa(r); end if;

      end case;
    end if;

    Solve_signs_for_Div_Rem( i1.neg,i2.neg, q.neg,r.neg );
  end Div_Rem;

  function "/" (i1,i2: multi_int) return multi_int is
    q: Multi_int( Max( 0, i1.last_used - i2.last_used + 1) );
    r: Multi_int( Max( i1.last_used, i2.last_used) + 2 );
  begin
    Div_Rem(i1,i2, q,r);
    return q;
  end "/";

  function "/" (i1: multi_int; i2: basic_int) return multi_int is
    q: multi_int(i1.last_used + 1);
    r: basic_int;
  begin
    Div_Rem(i1,i2, q,r);
    return q;
  end "/";

  function "Rem" (i1,i2: multi_int) return multi_int is
    q: multi_int(Max(0,i1.last_used - i2.last_used + 1));
    r: multi_int(Max(i1.last_used,i2.last_used) + 2);
  begin
    Div_Rem(i1,i2, q,r);
    return r;
  end "Rem";

  function "Rem" (i1: multi_int; i2: basic_int) return multi_int is
  begin return i1 Rem Multi(i2); end "Rem";

  function "Rem" (i1: multi_int; i2: basic_int) return basic_int is
    q: multi_int(i1.last_used + 1);
    r: basic_int;
  begin
    Div_Rem(i1,i2, q,r);
    return r;
  end "Rem";

  function "Mod" (i1,i2: multi_int) return multi_int is
    q: multi_int(Max(0,i1.last_used - i2.last_used + 1));
    r: multi_int(Max(i1.last_used,i2.last_used) + 2);
  begin
    -- Ada RM, 4.5.5 Multiplying Operators
    -- (8)
    -- The signed integer modulus operator is defined such that
    -- the result of A mod B has the sign of B and an absolute value
    -- less than the absolute value of B; in addition, for some signed
    -- integer value N, this result satisfies the relation:
    -- (9) A = B*N + (A mod B)

    Div_Rem(i1,i2, q,r);
    if r.zero or else i2.neg = r.neg then  --  (A rem B) est nul ou
      return r;     -- a le meme signe que B, donc (A mod B) = (A rem B)
    else  -- signe opposes
      return i2+r;  -- alors (B + (A rem B)) est le bon candidat
    end if;
  end "Mod";

  function "Mod" (i1: multi_int; i2: basic_int) return multi_int is
  begin return i1 Mod Multi(i2); end "Mod";

  function "Mod" (i1: multi_int; i2: basic_int) return basic_int is
    r: basic_int:= i1 Rem i2;
  begin
    if r=0 or else (i2<0) = (r<0) then  --  (A rem B) est nul ou
      return r;     -- a le meme signe que B, donc (A mod B) = (A rem B)
    else  -- signe opposes
      return i2+r;  -- alors (B + (A rem B)) est le bon candidat
    end if;
  end "Mod";

----- End of DIVISION part ------

----- Begin of POWER part -------

  procedure Power (i: multi_int; n: Natural; ipn: out multi_int) is
    max_ipn_last: Index_int; -- 17-Feb-2002
  begin
    if i.zero then
      if n=0 then
        raise Zero_power_zero;
      else
        Fill( ipn, Multi(0) ); -- the 0**n = 0 case (17-Feb-2002)
        return;
      end if;
    end if;
    
    max_ipn_last:= ((1+i.last_used) * Index_int(n)-1)+2;
    if ipn.n < max_ipn_last then
      raise Result_undersized;
    end if;

    case n is
      when 0 => Fill( ipn, Multi(1) ); -- the i**0 = 1 case
      when 1 => Fill( ipn, i);         -- the i**1 = i case
      when others =>
        declare
          nn: natural:= n-1;
          i0, ii: Multi_int( max_ipn_last );
        begin
          Fill(i0, i);
          Fill(ii, i0 );

          while nn > 0 loop
            if nn MOD 2 = 0 then -- x^(2 c) = (x^2) ^c
              Mult(i0,i0, i0);
              nn:= nn / 2;
            else
              Mult(i0,ii, ii);
              nn:= nn - 1;
            end if;
          end loop;
          Fill( ipn, ii);
        end;
    end case;
  end Power;

  function "**" (i: multi_int; n: Natural) return multi_int is
    ipn: Multi_int( (1+i.last_used) * index_int(n)+2 );
  begin
    Power(i,n,ipn);
    return ipn;
  end "**";

----- End of POWER part ---------

----- Comparisons

  function Equal (i1,i2: multi_int) return boolean is
  begin
    if i1.zero and then i2.zero then
      return True;
    end if;

    if i1.zero = i2.zero and then
       i1.neg  = i2.neg  and then
       i1.last_used = i2.last_used then
      Return i1.blk(0..i1.last_used) = i2.blk(0..i2.last_used);
    else
      Return False;
    end if;
  end Equal;

  function Equal (i1: multi_int; i2:basic_int) return boolean is
  begin
    return Equal( i1, Multi(i2) );
  end Equal;

  function ">" (i1,i2: multi_int) return Boolean is
  begin
    -- (1) Cas \'evident o\`u:         i1 <= i2
    if (i1.zero or i1.neg) and then             -- i1 <= 0 et
       (i2.zero or not i2.neg) then             -- i2 >= 0
        return False;
    end if;
    
    -- (2.1) Cas \'evident o\`u:       i1 > i2
    if ((not i1.zero) and not i1.neg) and then  -- i1 > 0 et
       (i2.zero or i2.neg) then                 -- i2 <= 0 
        return True;
    end if;

    -- (2.2) Cas \'evident o\`u:       i1 > i2
    if (i1.zero or not i1.neg) and then         -- i1 >= 0 et
       ((not i2.zero) and i2.neg) then          -- i2 < 0 
        return True;
    end if;

    -- Cas faciles resolus:
    -- i1 > i2  -  0  +
    -------------------
    --  -       #  F  F
    --  0       T  F  F
    --  +       T  T  #
    
    -- On a les cas avec "#", o\`u i1 et i2 ont le meme signe

    if i1.neg then
      Return NOT (Compare_absolute (i1,i2) = greater);
    else
      Return     (Compare_absolute (i1,i2) = greater);
    end if;

  end ">";

  function ">" (i1: multi_int; i2:basic_int) return Boolean is
  begin
    return i1 > Multi(i2);
  end ">";

  function "<" (i1,i2: multi_int) return Boolean is
  begin return i2>i1; end;

  function "<" (i1: multi_int; i2:basic_int) return Boolean is
  begin
    return i1 < Multi(i2);
  end "<";

  function ">=" (i1,i2: multi_int) return Boolean is
  begin return not (i2>i1); end;

  function ">=" (i1: multi_int; i2:basic_int) return Boolean is
  begin
    return i1 >= Multi(i2);
  end ">=";

  function "<=" (i1,i2: multi_int) return Boolean is 
  begin return not (i1>i2); end;

  function "<=" (i1: multi_int; i2:basic_int) return Boolean is
  begin
    return i1 <= Multi(i2);
  end "<=";

begin
  if 2*bitsblock >= long_basic_int'size then
    raise constraint_error; -- long_basic_int type is too small !
  end if;
  if bitsblock >= basic_int'size then
    raise constraint_error; -- basic_int type is too small !
  end if;
end Multi_precision_integers;
-----------------------------------------------------------------------------
--  File: muprinio.adb; see specification (muprinio.ads)
-----------------------------------------------------------------------------

package body Multi_precision_integers_IO is

  table: constant array(basic_int'(0)..15) of character:= 
         ('0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F');

   -- 15-Feb-2002: Bugfix case i=0. Spotted by Duncan Sands

  function Chiffres_i_non_nul(i: multi_int; base: number_base:= 10) return natural is
    nombre: multi_int(i.last_used);
    la_base    : constant basic_int :=  basic_int(base);
    nchiffres: natural:= 1;

    procedure Comptage_rapide( C: positive ) is
      test  : multi_int(i.n);
      base_puiss_C: constant multi_int:= Multi( basic_int(base) ) ** C;
    begin
      loop
        Fill(test, nombre / base_puiss_C );	
        exit when test.zero;
        -- quotient non nul, donc on a au moins C chiffres
        Fill(nombre, test);
        nchiffres:= nchiffres + C;
      end loop;
    end Comptage_rapide;

  begin
    Fill(nombre, i);
    Comptage_rapide( 400 );
    Comptage_rapide( 20 );
    loop
      Fill(nombre, nombre / la_base);	
      exit when nombre.zero;
      nchiffres:= nchiffres + 1;
    end loop;
    return nchiffres;
  end Chiffres_i_non_nul;

  function Chiffres(i: multi_int; base: number_base:= 10) return natural is
  begin
    if i.zero then
      return 1;
    else
      return Chiffres_i_non_nul(i,base);
    end if;
  end Chiffres;
  
  function Str(i: multi_int; base: number_base:= 10) return String is
    res: String(1..1 + Chiffres(i,base)):= (others=> 'x');
    nombre : multi_int(i.n):= i;
    chiffre: basic_int;
    la_base: constant basic_int :=  basic_int(base);

    begin
      if nombre.zero or else not nombre.neg then
        res(1):= ' ';
      else
        res(1):= '-';
      end if;
      nombre.neg:= false;

      -- maintenant nombre et base sont >=0, MOD=REM
      for k in reverse 2 .. res'last loop
        Div_Rem( nombre, la_base, nombre, chiffre );
        res(k):= table( chiffre );
        exit when nombre.zero;
      end loop;
      return res;

    end Str;


-- !!! recursion !!!

  function Val(s: String) return multi_int is
    formatting_error: exception;
    begin
      if s="" then
        return Multi(0);
      elsif s(s'first)='-' then
        return -Val(s(2..s'last));
      elsif s(s'first)='+' then
        return  Val(s(2..s'last));
      elsif s(s'last) in '0'..'9' then
        return basic_int'value(s(s'last..s'last)) + 10 *  
               Val(s(s'first..s'last-1));
      else
        raise formatting_error;
      end if;
    end Val;

  procedure Put_in_blocks(File  : in File_Type;
                          Item  : in multi_int) is
    begin
      if Item.neg then put(File,'-'); else put(File,'+'); end if;
      Put(File,'{');
      if Item.n > Item.last_used then
        Put(File, index_int'image(Item.n - Item.last_used) & " unused |");
      end if;
      for k in reverse 0 .. Item.last_used loop
        Put(File, block'image(Item.blk(k)));
        if k>0 then Put(File,'|'); end if;
      end loop;
      Put(File,'}');
    end Put_in_blocks;

  procedure Put_in_blocks(Item  : in multi_int) is
    begin Put_in_blocks( Standard_Output, Item );
    end;


  procedure Get(File  : in  File_Type;
                Item  : out multi_int;
                Width : in Field := 0) is 
    begin 
      Null; -- !!!
    end;
				
  procedure Get(Item  : out multi_int;
                Width : in  Field := 0) is

    begin   Get(Standard_Input, Item, Width);   end Get;


  procedure Put(File  : in File_Type;
                Item  : in multi_int;
                Width : in Field := 0;
                Base  : in Number_Base := Default_Base) is

    begin
      if Width = 0 then       -- No padding required (default) 
        Put(File, Str(Item, Base));
      else                    -- Left padding required -> slow
        declare
          la_chaine: String(1..Width);
        begin
          Put(la_chaine, Item, Base);
          Put(File, la_chaine);
        end;
      end if;
    end Put;
				
  procedure Put(Item  : in multi_int;
                Width : in Field := 0;
                Base  : in Number_Base := Default_Base) is

    begin   Put(Standard_Output, Item, Width, Base);  end Put;
				
  procedure Get(From : in  String;
                Item : out multi_int;
                Last : out Positive) is 
    begin
      Last:= 1;
      Null; -- !!!
    end Get;

				
  procedure Put(To   : out String;
                Item : in multi_int;
                Base : in Number_Base := Default_Base) is

    nchiffres: natural:= Chiffres(Item, Base);
    blancs: String(To'range):= (others=> ' ');

    begin
      if nchiffres > To'Length then
        raise Layout_Error;
      else
        To:= blancs;
        To( To'last - nchiffres .. To'last ):= Str(Item, Base);
      end if;
    end Put;
    
end Multi_precision_integers_IO;
with Integers;
with Points;

package body Partition.Debug is
   use Integers;
   use Points;

   ------------------
   -- Is_Partition --
   ------------------
   function Is_Partition (Strict : Boolean) return Boolean is
      Current_Element : Element_Pointer := Get_First;
   begin
      while Current_Element /= null loop
         declare
            Polygon : Polygon_Type renames Current_Element.Polygon;
            X, Y : Point_Type;
            X_Comp : Integer_Type;
            Z_Comp : Integer_Type;
            Other_Element : Element_Pointer;
            Count : Natural;
            Signum : Integer := 0;
         begin
            if not Is_Polygon (Strict, Polygon) then
               return False;
            end if;

            for I in 1 .. Polygon.Number_Of_Vertices loop
               X := Get_Vertex (I, Polygon);
               Y := Get_Vertex (I + 1, Polygon);
               X_Comp := Get_Component (Points.x, X);
               if Signum = 0 then
                  Signum := Sign (X_Comp);
               else -- Signum /= 0
                  if Is_Positive (X_Comp) and Signum = -1 then
                     return False;
                  elsif Is_Negative (X_Comp) and Signum = 1 then
                     return False;
                  end if;
               end if;

               Z_Comp := Get_Component (z, X);
               if Is_Negative (Z_Comp) then
                  return False;
               end if;
               if Is_Positive (Z_Comp) or
                  Is_Positive (Get_Component (z, Y)) then
                  Count := 0;
                  Other_Element := Get_First;
                  while Other_Element /= null loop
                     if Other_Element /= Current_Element then
                        declare
                           Other_Polygon : Polygon_Type
                             := Get_Polygon (Other_Element.all);
                        begin
                           for J in 1 .. Other_Polygon.Number_Of_Vertices loop
                              if Equivalent (
                                X,
                                Get_Vertex (J, Other_Polygon)
                              ) then

                                 if Equivalent (
                                   Y,
                                   Get_Vertex (J + 1, Other_Polygon)
                                 ) then
                                    return False;
                                 elsif Equivalent (
                                   Y,
                                   Get_Vertex (J - 1, Other_Polygon)
                                 ) then
                                    Count := Count + 1;
                                 end if;

                              end if;
                           end loop;
                        end;
                     end if;
                     Other_Element := Other_Element.Next;
                  end loop;
                  if Count /= 1 then
                     return False;
                  end if;
               end if;
            end loop;
         end;
         Current_Element := Current_Element.Next;
      end loop;

      return True;
   end Is_Partition;
end Partition.Debug;
with Ada.Text_IO; use Ada.Text_IO;
with Polygons.IO; use Polygons.IO;
with Symbolics.IO; use Symbolics.IO;

package body Partition.IO is
   --------------
   -- Put_Line --
   --------------
   procedure Put_Line (Element : Element_Type) is
   begin
      Put ("Polygon: ");
      Put_Line (Get_Polygon (Element));
      New_Line;
      Put ("Symbol sequence: ");
      Put (Get_Symbols (Element));
      New_Line;
   end Put_Line;

   -------------------
   -- Put_Partition --
   -------------------
   procedure Put_Partition is
      Current_Element : Element_Pointer := Get_First;
   begin
      if Current_Element /= null then
         Put_Line (Current_Element.all);
         loop
            Current_Element := Current_Element.Next;
            exit when Current_Element = null;
            New_Line;
            Put_Line ("----");
            New_Line;
            Put_Line (Current_Element.all);
         end loop;
      end if;
   end Put_Partition;
end Partition.IO;
with Intersects_Halfspace;
with Lozi;

procedure Partition.Process_Transitions is
   use Lozi;

   Previous_Element : Element_Pointer := Get_First;
   Previous_Index   : Natural := 1;

   procedure Find_Target (
     Symbol_Sequence : in     Sequence_Type;
     Target_Element  :    out Element_Pointer;
     Target_Index    :    out Positive
   );
   --  Finds the first element with symbol sequence greater than or equal to
   --  Symbol_Sequence.  If there is none, then the first element is returned.

   procedure Find_Target (
     Symbol_Sequence : in     Sequence_Type;
     Target_Element  :    out Element_Pointer;
     Target_Index    :    out Positive
   ) is
   begin
      --  This search method is intended to be efficient if the
      --  partition elements are kept in order.

      if Symbol_Sequence < Get_Symbols (Previous_Element.all) then
         Previous_Element := Get_First;
         Previous_Index   := 1;
      end if;

      while Previous_Element /= null loop
         if Get_Symbols (Previous_Element.all) >= Symbol_Sequence then
            Target_Element := Previous_Element;
            Target_Index   := Previous_Index;
            return;
         end if;
         Previous_Element := Previous_Element.Next;
         Previous_Index   := Previous_Index + 1;
      end loop;

      Previous_Element := Get_First;
      Previous_Index   := 1;

      Target_Element := Previous_Element;
      Target_Index   := Previous_Index;
   end Find_Target;

   Current_Element : Element_Pointer := Get_First;
   Current_Index   : Positive := 1;
begin
   while Current_Element /= null loop
      declare
         Symbol_Sequence : constant Sequence_Type :=
           Get_Symbols (
             Current_Element.all
           );
         Cut_Sequence : constant Sequence_Type :=
           Symbol_Sequence (
             Symbol_Sequence'First + 1 .. Symbol_Sequence'Last
           );
         Left_Sequence  : constant Sequence_Type := Cut_Sequence & L;
         Right_Sequence : constant Sequence_Type := Cut_Sequence & R;
         Left_Element   : Element_Pointer;
         Right_Element  : Element_Pointer;
         Left_Index     : Positive;
         Right_Index    : Positive;
         Left_Exists    : Boolean;
         Right_Exists   : Boolean;
      begin
         Find_Target (Left_Sequence, Left_Element, Left_Index);
         Left_Exists := Left_Sequence = Get_Symbols (Left_Element.all);

         Find_Target (Right_Sequence, Right_Element, Right_Index);
         Right_Exists := Right_Sequence = Get_Symbols (Right_Element.all);

         if Left_Exists or Right_Exists then
            declare
               Current_Polygon : constant Polygon_Type := Get_Polygon (
                 Current_Element.all
               );
               Image_Polygon : Polygon_Type := Current_Polygon;
               Left_Intersects, Right_Intersects : Boolean;
            begin
               Lozi.Map (Image_Polygon);

               Intersects_Halfspace (
                 Image_Polygon,
                 Left_Halfspace,
                 Left_Intersects,
                 Right_Intersects
               );

               if Left_Exists and Left_Intersects then
                  Action (
                    From_Polygon => Current_Polygon,
                    From_Index   => Current_Index,
                    From_Image   => Image_Polygon,
                    To_Polygon   => Get_Polygon (Left_Element.all),
                    To_Index     => Left_Index,
                    Total_Symbol => Symbol_Sequence & L
                  );
               end if;

               if Right_Exists and Right_Intersects then
                  Action (
                    From_Polygon => Current_Polygon,
                    From_Index   => Current_Index,
                    From_Image   => Image_Polygon,
                    To_Polygon   => Get_Polygon (Right_Element.all),
                    To_Index     => Right_Index,
                    Total_Symbol => Symbol_Sequence & R
                  );
               end if;
            end;
         end if;
      end;

      Current_Element := Current_Element.Next;
      Current_Index   := Current_Index + 1;
   end loop;
end Partition.Process_Transitions;
with Intersect;
with Lozi;
with Partition.Process_Transitions;
with Points;
with Polygons;
with Symbolics;

procedure Partition.Refine is
   use Lozi;
   use Points;
   use Polygons;
   use Symbolics;

   First_New, Last_New : Element_Pointer;

   function Get_Halfspace (Symbol : Symbol_Type) return Point_Type;
   pragma Inline (Get_Halfspace);

   function Get_Halfspace (Symbol : Symbol_Type) return Point_Type is
   begin
      if Symbol = L then
         return Left_Halfspace;
      else
         return Right_Halfspace;
      end if;
   end Get_Halfspace;

   procedure Action (
     From_Polygon : Polygon_Type;
     From_Index   : Positive;
     From_Image   : Polygon_Type;
     To_Polygon   : Polygon_Type;
     To_Index     : Positive;
     Total_Symbol : Sequence_Type
   );

   procedure Action (
     From_Polygon : Polygon_Type;
     From_Index   : Positive;
     From_Image   : Polygon_Type;
     To_Polygon   : Polygon_Type;
     To_Index     : Positive;
     Total_Symbol : Sequence_Type
   ) is
      pragma Unreferenced (From_Polygon, From_Index, To_Polygon, To_Index);

      Halfspace : Point_Type renames Get_Halfspace (
        Total_Symbol (Total_Symbol'Last)
      );
      Intersection : Polygon_Type renames Intersect (
        Polygon => From_Image,
        Point   => Halfspace
      );
      New_Element : constant Element_Pointer := new Element_Type'(
        Create_Element (Intersection, Total_Symbol)
      );
   begin
      pragma Assert (not Is_Empty (Intersection));

      if First_New = null then
         First_New := New_Element;
      end if;

      if Last_New /= null then
         Last_New.Next := New_Element;
      end if;

      Last_New := New_Element;
   end Action;

   procedure Calculate_Refinement is new Process_Transitions (Action);
begin
   Calculate_Refinement;

   Delete_All_Elements; -- the old elements

   Set_First (First_New);
end Partition.Refine;
with Ada.Unchecked_Deallocation;

package body Partition is
   First_Element : Element_Pointer;

   procedure Free is new Ada.Unchecked_Deallocation (
     Object => Element_Type,
     Name   => Element_Pointer
   );

   -----------------
   -- Add_Element --
   -----------------
   procedure Add_Element (Element : Element_Type) is
      Current_Element : Element_Pointer := First_Element;
      Next_Element    : Element_Pointer;

      Symbols : Sequence_Type renames Element.Symbols;
   begin
      if Current_Element = null then
         First_Element := new Element_Type '(Element);
         First_Element.Next := null;
         return;
      end if;

      if Current_Element.Depth /= Element.Depth then
         raise Partition_Error;
      end if;

      if Symbols <= Current_Element.Symbols then
         if Symbols = Current_Element.Symbols then
            raise Partition_Error;
         end if;
         First_Element := new Element_Type '(Element);
         First_Element.Next := Current_Element;
         return;
      end if;

      loop
         Next_Element := Current_Element.Next;

         if Next_Element = null then
            Current_Element.Next := new Element_Type '(Element);
            Current_Element.Next.Next := null;
            return;
         end if;

         declare
            Next_Sequence : Sequence_Type renames Next_Element.Symbols;
         begin
            if Symbols <= Next_Sequence then
               if Symbols = Next_Sequence then
                  raise Partition_Error;
               end if;
               Current_Element.Next := new Element_Type '(Element);
               Current_Element.Next.Next := Next_Element;
               return;
            end if;
         end;

         Current_Element := Next_Element;
      end loop;
   end Add_Element;

   --------------------
   -- Create_Element --
   --------------------
   function Create_Element (
     Polygon : Polygons.Polygon_Type;
     Symbols : Sequence_Type
   ) return Element_Type is
   begin
      return (
        Depth   => Symbols'Length,
        Size    => Polygon.Number_Of_Vertices,
        Next    => null,
        Polygon => Polygon,
        Symbols => Symbols
      );
   end Create_Element;

   -------------------------
   -- Delete_All_Elements --
   -------------------------
   procedure Delete_All_Elements is
      Current_Element : Element_Pointer := First_Element;
   begin
      First_Element := null;

      while Current_Element /= null loop
         declare
            Spare : Element_Pointer := Current_Element;
         begin
            Current_Element := Current_Element.Next;
            Free (Spare);
         end;
      end loop;
   end Delete_All_Elements;

   ---------------------
   -- Delete_Elements --
   ---------------------
   procedure Delete_Elements (Indices : Index_List) is
      Current_Element : Element_Pointer := First_Element;
      Current_Index   : Positive := 1;

      Delete_Position : Positive := Indices'First;

      Last_Undeleted : Element_Pointer;

      Next_Element : Element_Pointer;
   begin
      while Current_Element /= null and Delete_Position <= Indices'Last loop
         Next_Element := Current_Element.Next;

         if Indices (Delete_Position) = Current_Index then
            Free (Current_Element);
            Delete_Position := Delete_Position + 1;
         else
            if Last_Undeleted /= null then
               Last_Undeleted.Next := Current_Element;
            else
               First_Element := Current_Element;
            end if;
            Last_Undeleted := Current_Element;
         end if;

         Current_Element := Next_Element;
         Current_Index := Current_Index + 1;
      end loop;

      if Last_Undeleted /= null then
         Last_Undeleted.Next := Current_Element;
      else
         First_Element := Current_Element;
      end if;
   end Delete_Elements;

   ------------------------
   -- Element_Count --
   ------------------------
   --  Could maintain a count at all times, but is it worth it?
   --  Don't forget: Set_First would need calculate a new element count.
   function Element_Count return Natural is
      Count : Natural := 0;
      Current_Element : Element_Pointer := Get_First;
   begin
      while Current_Element /= null loop
         Count := Count + 1;
         Current_Element := Current_Element.Next;
      end loop;

      return Count;
   end Element_Count;

   ---------------
   -- Get_First --
   ---------------
   function Get_First return Element_Pointer is
   begin
      return First_Element;
   end Get_First;

   -----------------
   -- Get_Polygon --
   -----------------
   function Get_Polygon (Element : Element_Type) return Polygon_Type is
   begin
      return Element.Polygon;
   end Get_Polygon;

   -----------------
   -- Get_Symbols --
   -----------------
   function Get_Symbols (Element : Element_Type) return Sequence_Type is
   begin
      return Element.Symbols;
   end Get_Symbols;

   ---------------
   -- Set_First --
   ---------------
   procedure Set_First (First_Element : Element_Pointer) is
   begin
      Partition.First_Element := First_Element;
   end Set_First;
end Partition;
with Ada.Text_IO;
with Integers.IO;

package body Points.IO is
   procedure Put_Line (Source : Point_Type) is
   begin
      Ada.Text_IO.Put ("X: ");
      Integers.IO.Put (Source (x));
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put ("Y: ");
      Integers.IO.Put (Source (y));
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put ("Z: ");
      Integers.IO.Put (Source (z));
      Ada.Text_IO.New_Line;
   end Put_Line;
end Points.IO;
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
with Ada.Integer_Text_IO;
with Ada.Text_IO;
with Points.IO;

package body Polygons.IO is
   procedure Put_Line (Source : Polygon_Type) is
   begin
      Ada.Text_IO.Put ("Number of vertices: ");
      Ada.Integer_Text_IO.Put (Source.Number_Of_Vertices, Width => 0);
      Ada.Text_IO.New_Line;
      for I in 1 .. Source.Number_Of_Vertices loop
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put ("Vertex ");
         Ada.Integer_Text_IO.Put (I, Width => 0);
         Ada.Text_IO.Put_Line (":");
         Points.IO.Put_Line (Get_Vertex (I, Source));
      end loop;
   end Put_Line;
end Polygons.IO;
with Integers;

package body Polygons is
   ------------
   -- Create --
   ------------
   function Create (Vertex_List : Point_Array_Type) return Polygon_Type is
      use Points;
      Result : Polygon_Type := Polygon_Type'(
        Number_Of_Vertices => Vertex_List'Length,
        Vertices           => Vertex_List
      );
   begin
      for I in Result.Vertices'Range loop
         if Is_Zero (Result.Vertices (I)) then
            raise Constraint_Error;
         end if;

         Reduce (Result.Vertices (I));
      end loop;

      return Result;
   end Create;

   ----------------
   -- Get_Vertex --
   ----------------
   function Get_Vertex (
     Position : Integer;
     Polygon  : Polygon_Type
   ) return Points.Point_Type is
   begin
      return Polygon.Vertices (
        (Position - 1) mod Polygon.Number_Of_Vertices + 1
      );
   end Get_Vertex;

   --------------
   -- Is_Empty --
   --------------
   function Is_Empty (Polygon : Polygon_Type) return Boolean is
   begin
      return Polygon.Number_Of_Vertices = 0;
   end Is_Empty;

   ----------------
   -- Is_Polygon --
   ----------------
   function Is_Polygon (
     Strict  : Boolean;
     Polygon : Polygon_Type
   ) return Boolean is
      use Points;
      use Integers;

      X, Y, Z : Point_Type;
      DP : Integer_Type;
   begin
      if Is_Empty (Polygon) then
         return True;
      elsif Polygon.Number_Of_Vertices < 3 then
         return False;
      end if;

      X := Polygon.Vertices (Polygon.Number_Of_Vertices);
      for I in Polygon.Vertices'Range loop
         Y := Polygon.Vertices (I);
         if Equivalent (X, Y) or Equivalent (X, Negate (Y)) then
            return False;
         end if;
         Z := Cross_Product (X, Y);
         X := Y;
         DP := Dot_Product (Get_Vertex (I + 1, Polygon), Z);
         if Is_Negative (DP) or (Strict and Is_Zero (DP)) then
            return False;
         end if;
      end loop;

      return True;
   end Is_Polygon;

   -------------------------
   -- Reverse_Orientation --
   -------------------------
   procedure Reverse_Orientation (Polygon : in out Polygon_Type) is
      Old_Vertices : constant Point_Array_Type := Polygon.Vertices;

      Target : Natural := Polygon.Number_Of_Vertices;
   begin
      for I in Polygon.Vertices'First .. Polygon.Vertices'Last - 1 loop
         Target := Target - 1;
         Polygon.Vertices (Target) := Old_Vertices (I);
      end loop;
   end Reverse_Orientation;

   ----------------
   -- Set_Vertex --
   ----------------
   procedure Set_Vertex (
     Position : in     Integer;
     Value    : in     Points.Point_Type;
     Polygon  : in out Polygon_Type
   ) is
      use Points;
      Reduced_Value : Point_Type := Value;
   begin
      if Is_Zero (Value) then
         raise Constraint_Error;
      end if;

      Reduce (Reduced_Value);

      Polygon.Vertices (
        (Position - 1) mod Polygon.Number_Of_Vertices + 1
      ) := Reduced_Value;
   end Set_Vertex;
end Polygons;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;

procedure Print_Lower (Lower_Bound : Float) is
begin
   if Lower_Bound = 0.0 then
      Put (0.0);
   else
      Put (
        Lower_Bound - 10.0 ** (-Default_Aft), -- round down
        Exp => 0
      );
   end if;
end Print_Lower;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;

procedure Print_Upper (Upper_Bound : Float) is
begin
   if Upper_Bound = 0.0 then
      Put (0.0);
   else
      Put (
        Upper_Bound + 10.0 ** (-Default_Aft), -- round up
        Exp => 0
      );
   end if;
end Print_Upper;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;

procedure Print_Usage is
begin
   Put (Standard_Error, "Usage: ");
   Put (Standard_Error, Command_Name);
   Put (Standard_Error, " [-v]");
   Put (Standard_Error, " A_Numerator A_Denominator");
   Put (Standard_Error, " B_Numerator B_Denominator");
   Put (Standard_Error, " [desired accuracy]");
   New_Line (Standard_Error);
   Flush (Standard_Error);
end Print_Usage;
with Integers; use Integers;
with Segments_Support; use Segments_Support;

function Segments_Intersect (
  X1, Y1 : Point_Type;
  X2, Y2 : Point_Type
) return Boolean is
   M : Matrix_Type;
   R : Row_Permutation;
   C : Col_Permutation;
   Rank : Rank_Range;

   function Different_Group (C1, C2 : Col_Index) return Boolean;
   pragma Inline (Different_Group);

   function Different_Group (C1, C2 : Col_Index) return Boolean is
      AC1 : constant Col_Index := C (C1);
      AC2 : constant Col_Index := C (C2);
   begin
      return (AC1 <= 2 and AC2 >= 3) or (AC1 >= 3 and AC2 <= 2);
   end Different_Group;
begin
   M (1, 1) := Get_Component (x, X1);
   M (2, 1) := Get_Component (y, X1);
   M (3, 1) := Get_Component (z, X1);

   M (1, 2) := Get_Component (x, Y1);
   M (2, 2) := Get_Component (y, Y1);
   M (3, 2) := Get_Component (z, Y1);

   M (1, 3) := Negate (Get_Component (x, X2));
   M (2, 3) := Negate (Get_Component (y, X2));
   M (3, 3) := Negate (Get_Component (z, X2));

   M (1, 4) := Negate (Get_Component (x, Y2));
   M (2, 4) := Negate (Get_Component (y, Y2));
   M (3, 4) := Negate (Get_Component (z, Y2));

   Row_Reduce (M, R, C, Rank, Strict => True);

   case Rank is
      when 0 =>
         --  Vectors were all zero
         return True;
      when 1 =>
         declare
            Zeros : array (1 .. 4) of Boolean;
            Saw_Positive : Boolean := False;
            Saw_Negative : Boolean := False;
            Row_1 : constant Row_Index := R (1);
            Spare : Integer;
         begin
            for J in Col_Index loop
               Spare := Sign (M (Row_1, C (J)));
               Saw_Positive := Saw_Positive or Spare > 0;
               Saw_Negative := Saw_Negative or Spare < 0;
               Zeros (J) := Spare = 0;
               if Saw_Positive and Saw_Negative then
                  return True;
               end if;
            end loop;

            return (Zeros (1) or Zeros (2)) and (Zeros (3) or Zeros (4));
         end;
      when 2 =>
         declare
            Row_1 : constant Row_Index := R (1);
            Row_2 : constant Row_Index := R (2);
            Col_3 : constant Col_Index := C (3);
            Col_4 : constant Col_Index := C (4);
            S11 : constant Integer := Sign (M (Row_1, C (1)));
            S22 : constant Integer := Sign (M (Row_2, C (2)));
            S13 : constant Integer := S11 * Sign (M (Row_1, Col_3));
            S14 : constant Integer := S11 * Sign (M (Row_1, Col_4));
            S23 : constant Integer := S22 * Sign (M (Row_2, Col_3));
            S24 : constant Integer := S22 * Sign (M (Row_2, Col_4));
         begin
            pragma Assert (S11 /= 0 and S22 /= 0);
            if (S13 < 0 and S23 < 0) or (S14 < 0 and S24 < 0) then
               return True;
            end if;

            if (S13 > 0 and S14 > 0) or (S23 > 0 and S24 > 0) then
               return False;
            end if;

            if S13 = 0 and S14 = 0 and S23 = 0 and S24 = 0 then
               return Different_Group (1, 2);
            end if;

            if S13 >= 0 and S14 >= 0 and S23 >= 0 and S24 >= 0 then
               return False;
            end if;

            if S13 <= 0 and S14 <= 0 and S23 <= 0 and S24 <= 0 then
               return True;
            end if;

            if S13 = 0 or S14 = 0 or S23 = 0 or S24 = 0 then
               if (S13 = 0 and S23 = 0) or (S14 = 0 and S24 = 0) then
                  --  Zero column
                  pragma Assert (S13 * S23 < 0 or S14 * S24 < 0);
                  return False;
               end if;

               if (S13 = 0 and S14 = 0) or (S23 = 0 and S24 = 0) then
                  --  Zero row
                  pragma Assert (S13 * S14 < 0 or S23 * S24 < 0);
                  return True;
               end if;

               if (S13 = 0 and S24 = 0) or (S14 = 0 and S23 = 0) then
                  --  Zero diagonal
                  pragma Assert (S13 * S24 < 0 or S14 * S23 < 0);
                  declare
                     Pos_Col : Col_Index;
                     Pos_Row : Row_Index;
                  begin
                     if S13 > 0 or S23 > 0 then
                        Pos_Col := 3;
                     else
                        Pos_Col := 4;
                     end if;

                     if S13 > 0 or S14 > 0 then
                        Pos_Row := 1;
                     else
                        Pos_Row := 2;
                     end if;

                     return Different_Group (Pos_Col, Col_Index (Pos_Row));
                  end;
               end if;

               --  Only one zero element
               if (S13 = 0 and S14 < 0) or (S14 = 0 and S13 < 0) or
                  (S23 = 0 and S24 < 0) or (S24 = 0 and S23 < 0) then
                  pragma Assert (
                    (S13 = 0 and S14 < 0 and S23 < 0 and S24 > 0) or
                    (S23 = 0 and S24 < 0 and S13 < 0 and S14 > 0) or
                    (S14 = 0 and S13 < 0 and S24 < 0 and S23 > 0) or
                    (S24 = 0 and S23 < 0 and S14 < 0 and S13 > 0)
                  );
                  return True;
               end if;

               if (S13 = 0 and S23 > 0) or (S23 = 0 and S13 > 0) or
                  (S14 = 0 and S24 > 0) or (S24 = 0 and S14 > 0) then
                  pragma Assert (
                    (S13 = 0 and S23 > 0 and S14 > 0 and S24 < 0) or
                    (S14 = 0 and S24 > 0 and S13 > 0 and S23 < 0) or
                    (S23 = 0 and S13 > 0 and S24 > 0 and S14 < 0) or
                    (S24 = 0 and S14 > 0 and S23 > 0 and S13 < 0)
                  );
                  return False;
               end if;

               pragma Assert (
                 (S13 = 0 and S14 > 0 and S23 < 0) or
                 (S14 = 0 and S13 > 0 and S24 < 0) or
                 (S23 = 0 and S24 > 0 and S13 < 0) or
                 (S24 = 0 and S23 > 0 and S14 < 0)
               );

               declare
                  Pos_Col : Col_Index;
                  Zero_Row : Row_Index;
               begin
                  if S13 > 0 or S23 > 0 then
                     Pos_Col := 3;
                  else
                     Pos_Col := 4;
                  end if;

                  if S13 = 0 or S14 = 0 then
                     Zero_Row := 1;
                  else
                     Zero_Row := 2;
                  end if;

                  return Different_Group (Pos_Col, Col_Index (Zero_Row));
               end;
            end if;

            declare
               A : Integer_Type renames M (Row_1, Col_3);
               B : Integer_Type renames M (Row_2, Col_3);
               C : Integer_Type renames M (Row_1, Col_4);
               D : Integer_Type renames M (Row_2, Col_4);
               S : constant Integer := Sign (A) * S22;

               AD : constant Integer_Type := A * D;
               BC : constant Integer_Type := B * C;
            begin
               if AD > BC then
                  return S < 0;
               elsif AD /= BC then -- AD < BC
                  return S > 0;
               end if;

               return Different_Group (1, 2);
            end;
         end;
      when 3 =>
         declare
            Col_4 : constant Col_Index := C (4);
            Signs : array (Col_Index) of Integer;
         begin
            Signs (Col_4) := 1;
            for I in Row_Index loop
               declare
                  Row : constant Row_Index := R (I);
                  Col : constant Col_Index := C (Col_Index (I));
                  Spare : Integer;
               begin
                  Spare := Sign (M (Row, Col)) * Sign (M (Row, Col_4));
                  if Spare > 0 then
                     return False;
                  end if;
                  Signs (Col) := -Spare;
               end;
            end loop;
            return (Signs (1) /= 0 or Signs (2) /= 0) and
                   (Signs (3) /= 0 or Signs (4) /= 0);
         end;
   end case;
end Segments_Intersect;
with Ada.Text_IO;

package body Symbolics.IO is
   package Symbol_IO is new Ada.Text_IO.Enumeration_IO (Symbol_Type);
   use Symbol_IO;

   procedure Put (Symbols : Sequence_Type) is
   begin
      for I in Symbols'Range loop
         Put (Symbols (I), Width => 0);
      end loop;
   end Put;
end Symbolics.IO;
package body Symbolics is
   function "<" (Left, Right : Sequence_Type) return Boolean is
      Right_Position : Natural := Right'First;
   begin
      if Left'Length /= Right'Length then
         raise Constraint_Error;
      end if;

      for Left_Position in Left'Range loop
         if Left (Left_Position) /= Right (Right_Position) then
            return Left (Left_Position) < Right (Right_Position);
         end if;

         Right_Position := Right_Position + 1;
      end loop;

      return False;
   end "<";

   function ">=" (Left, Right : Sequence_Type) return Boolean is
   begin
      return not "<" (Left, Right);
   end ">=";

   function ">" (Left, Right : Sequence_Type) return Boolean is
      Right_Position : Natural := Right'First;
   begin
      if Left'Length /= Right'Length then
         raise Constraint_Error;
      end if;

      for Left_Position in Left'Range loop
         if Left (Left_Position) /= Right (Right_Position) then
            return Left (Left_Position) > Right (Right_Position);
         end if;

         Right_Position := Right_Position + 1;
      end loop;

      return False;
   end ">";

   function "<=" (Left, Right : Sequence_Type) return Boolean is
   begin
      return not ">" (Left, Right);
   end "<=";
end Symbolics;
with Ada.Text_IO; use Ada.Text_IO;

package body Transition_Matrices.IO is
   --------------
   -- Put_Line --
   --------------
   procedure Put_Line (Matrix : Transition_Matrix_Type) is
   begin
      for Row in 1 .. Matrix.Size loop
         for Column in 1 .. Matrix.Size loop
            if Transition_Exists (
              From   => Row,
              To     => Column,
              Matrix => Matrix
            ) then
               Put (" 1");
            else
               Put (" 0");
            end if;
         end loop;
         Put ("  (");
   
         declare
            Indices : Index_List renames Matrix.Rows (Row).Indices;
         begin
            for I in Indices'Range loop
               if I > Indices'First then
                  Put (',');
               end if;
               Put (Positive'Image (Indices (I)));
            end loop;
         end;
   
         Put_Line (")");
      end loop;
      New_Line;
   end Put_Line;

   procedure Put_Line (
     Matrix   : Transition_Matrix_Type;
     Vertices : Vertex_List
   ) is
   begin
      for Row in Vertices'Range loop
         for Column in Vertices'Range loop
            if Transition_Exists (
              From   => Vertices (Row),
              To     => Vertices (Column),
              Matrix => Matrix
            ) then
               Put (" 1");
            else
               Put (" 0");
            end if;
         end loop;
         New_Line;
      end loop;
      New_Line;
   end Put_Line;
end Transition_Matrices.IO;
--with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;
--with GCD;
with GNAT.Heap_Sort_G;
with Transition_Matrix_Rows;

--  Algorithm: R. E. Tarjan, Depth-first search and linear graph algorithms,
--  SIAM Journal on Computing, 1(2): 146-160, June 1972.

procedure Transition_Matrices.SCC (Matrix : Transition_Matrix_Type) is
   N : constant Natural := Matrix.Size;

   type List is array (Positive range <>) of Natural;
   type List_Pointer is access List;

   type Vertex_List_Pointer is access Vertex_List;

--   Depth_Number : List_Pointer;
   Low_Link     : List_Pointer;
   Node_Number  : List_Pointer;

   Last_Number : Natural := 0;

   --  Stack (1 .. Stack_Top) contains the current tree.
   --  Stack (Wander_Bottom .. N) contains the current wandering vertex list.
   Stack : Vertex_List_Pointer;
   Stack_Top : Natural := 0;
   Wander_Bottom : Positive := N + 1;

--   GCD_Stack : List_Pointer;

   procedure Free is new Ada.Unchecked_Deallocation (
     List,
     List_Pointer
   );

   procedure Free is new Ada.Unchecked_Deallocation (
     Vertex_List,
     Vertex_List_Pointer
   );

   function Visited (Index : Positive) return Boolean;
   pragma Inline (Visited);

   function Visited (Index : Positive) return Boolean is
   begin
      return Node_Number (Index) /= 0;
   end Visited;

   function On_Stack (Index : Positive) return Boolean;
   pragma Inline (On_Stack);

   function On_Stack (Index : Positive) return Boolean is
   begin
      return Low_Link (Index) <= N;
   end On_Stack;

   function Has_Self_Edge (Index : Positive) return Boolean;
   pragma Inline (Has_Self_Edge);

   function Has_Self_Edge (Index : Positive) return Boolean is
      Found : Boolean := False;

      procedure Compare_Column (
        Column : in     Positive;
        Stop   : in out Boolean
      ) is
      begin
         if Column = Index then
            Found := True;
            Stop  := True;
         end if;
      end Compare_Column;

      procedure Search_For_Column
        is new Transition_Matrix_Rows.Visit_Non_Zero_Columns (Compare_Column);
   begin
      Search_For_Column (Matrix.Rows (Index));

      return Found;
   end Has_Self_Edge;

   procedure Sort_Stack (I, J : Positive);
   --  Sort positions I .. J in Stack

   procedure Sort_Stack (I, J : Positive) is
      Base : constant Natural := I - 1;
      Temp : Positive;

      procedure Move (From, To : Natural);
      pragma Inline (Move);

      procedure Move (From, To : Natural) is
      begin
         Stack (Base + To) := Stack (Base + From);
      end Move;

      function Lt (Op1, Op2 : Natural) return Boolean;
      pragma Inline (Lt);

      function Lt (Op1, Op2 : Natural) return Boolean is
      begin
         return Stack (Base + Op1) < Stack (Base + Op2);
      end Lt;

      package Heap_Sort is new GNAT.Heap_Sort_G (
        Move => Move,
        Lt   => Lt
      );
   begin
      if J <= I then
         return;
      end if;

      --  Stack (Base) used as temporary storage
      Temp := Stack (Base);
      Heap_Sort.Sort (J - Base);
      Stack (Base) := Temp;
   end Sort_Stack;

   procedure Visit (P : Positive);

   procedure Visit (P : Positive) is

      procedure Visit_Edge (
        Q : in     Positive;
        S : in out Boolean
      );
      procedure Visit_Edge (
        Q : in     Positive;
        S : in out Boolean
      ) is
         pragma Unreferenced (S);
      begin
         if not Visited (Q) then
--            Depth_Number (Q) := Depth_Number (P) + 1;
            Visit (Q);
            Low_Link (P) := Positive'Min (Low_Link (P), Low_Link (Q));
         elsif On_Stack (Q) then
--            if Depth_Number (Q) > 0 then
--               Loop_Length := Depth_Number (P) - Depth_Number (Q) + 1;
--               if Loop_GCD = 0 then
--                  Loop_GCD := Loop_Length;
--               else
--                  Loop_GCD := GCD (Loop_GCD, Loop_Length);
--               end if;
--            end if;
            Low_Link (P) := Positive'Min (Low_Link (P), Node_Number (Q));
         end if;
      end Visit_Edge;

      procedure Visit_All_Edges
        is new Transition_Matrix_Rows.Visit_Non_Zero_Columns (Visit_Edge);

--      Loop_Length    : Positive;
--      Loop_GCD       : Natural := 0;
--      Stack_Position : Positive;
   begin
      Last_Number := Last_Number + 1;
      pragma Assert (Last_Number <= N);
      Node_Number (P) := Last_Number;
      Low_Link (P) := Last_Number;

      Stack_Top := Stack_Top + 1;
      pragma Assert (Stack_Top < Wander_Bottom);
--      Stack_Position := Stack_Top;
--      Stack (Stack_Position) := P;
      Stack (Stack_Top) := P;

      Visit_All_Edges (Matrix.Rows (P));

--      Depth_Number (P) := 0;
--      GCD_Stack (Stack_Position) := Loop_GCD;

      if Low_Link (P) = Node_Number (P) then -- Found SCC
--         Loop_GCD := 0;
         Unwind_Stack : for I in reverse 1 .. Stack_Top loop
--            if GCD_Stack (I) /= 0 then
--               if Loop_GCD = 0 then
--	          Loop_GCD := GCD_Stack (I);
--               else
--                  Loop_GCD := GCD (Loop_GCD, GCD_Stack (I));
--               end if;
--	    end if;
            Low_Link (Stack (I)) := N + 1; -- Mark as removed from stack
            if Stack (I) = P then -- SCC in Stack (I .. Stack_Top)
               if Stack_Top > I then
                  Sort_Stack (I, Stack_Top);
--Put_Line ("GCD:" & Natural'Image (Loop_GCD));
                  SCC_Action (Matrix, Stack (I .. Stack_Top));
               else -- Only one node, and it is P
                  if Has_Self_Edge (P) then
--Put_Line ("GCD:" & Natural'Image (Loop_GCD));
                     SCC_Action (Matrix, Stack (I .. I));
                  else -- Wandering vertex
--                     pragma Assert (Loop_GCD = 0);
                     Wander_Bottom := Wander_Bottom - 1;
                     Stack (Wander_Bottom) := P;
                  end if;
               end if;

               Stack_Top := I - 1; -- Remove SCC from stack
               pragma Assert (Stack_Top < Wander_Bottom);
               exit Unwind_Stack;
            end if;
         end loop Unwind_Stack;
      end if;
   end Visit;
begin
--   Depth_Number := new List (1 .. N);
--   GCD_Stack    := new List (1 .. N);
   Low_Link     := new List (1 .. N);
   Node_Number  := new List (1 .. N);

   Stack := new Vertex_List (0 .. N); -- Position 0 used as temporary storage

   for I in 1 .. N loop
--      Depth_Number (I) := 0; -- "Unvisited"
      Low_Link     (I) := N + 1; -- "Not in stack"
      Node_Number  (I) := 0; -- "Unvisited"
   end loop;

   for I in 1 .. N loop
      if not Visited (I) then
--         Depth_Number (I) := 1;
         Visit (I);
      end if;
   end loop;

--   Free (Depth_Number);
--   Free (GCD_Stack);
   Free (Low_Link);
   Free (Node_Number);

   if Wander_Bottom < N then
      Sort_Stack (Wander_Bottom, N);
   end if;

   Wander_Action (Matrix, Stack (Wander_Bottom .. N));

   Free (Stack);
exception
   when others =>
--      Free (Depth_Number);
--      Free (GCD_Stack);
      Free (Low_Link);
      Free (Node_Number);
      Free (Stack);
      raise;
end Transition_Matrices.SCC;
with Ada.Text_IO; use Ada.Text_IO; -- QQ

with Ada.Unchecked_Deallocation;
with Arnoldi; use Arnoldi;
with Fortran_Complex_Types;
with IEEE;
with Interfaces.Fortran; use Interfaces.Fortran;
--  with Transition_Matrices.IO; -- QQ
with Transition_Matrices.SCC;
with Transition_Matrix_Rows;

package body Transition_Matrices.Spectral_Radius is

   Extra_Iterations : constant := 100;
   Power_Iterations : constant := 500;

   type Vector_Pointer is access Vector;

   type Work_Vector is array (Positive range <>) of Double_Precision;

   type Work_Pointer is access Work_Vector;

   procedure Free is new Ada.Unchecked_Deallocation (
     Vector,
     Vector_Pointer
   );

   procedure Free is new Ada.Unchecked_Deallocation (
     Work_Vector,
     Work_Pointer
   );

   function Modulus (X, Y : Double_Precision) return Double_Precision;
   pragma Inline (Modulus);

   function Modulus (X, Y : Double_Precision) return Double_Precision is
   begin
      --  Tricky to get right, so use compiler provided version.
      return Fortran_Complex_Types."abs" (Fortran_Complex_Types.Compose_From_Cartesian (X, Y));
   end Modulus;

   Irreducible_2x2 : constant array (Boolean, Boolean) of Float := (
     False => (
       False => 1.0,
       True  => 1.618033988749894848204586834365638117720309  -- golden ratio
     ),
     True => (
       False => 1.618033988749894848204586834365638117720309, -- golden ratio
       True  => 2.0
     )
   );

   procedure Component_Estimate (
     Matrix    : in     Transition_Matrix_Type;
     Vertices  : in     Vertex_List;
     Low, High :    out Float;
     Storage   : in     Work_Pointer
   );

   procedure Component_Estimate (
     Matrix    : in     Transition_Matrix_Type;
     Vertices  : in     Vertex_List;
     Low, High :    out Float;
     Storage   : in     Work_Pointer
   ) is
      N : constant Fortran_Integer := Vertices'Length;

      function Vertex (Index : Fortran_Integer) return Positive;
      pragma Inline (Vertex);

      function Vertex (Index : Fortran_Integer) return Positive is
      begin
         return Vertices (Vertices'First + Positive (Index) - 1);
      end Vertex;
   begin
      if N <= 0 then
         raise Constraint_Error;
      elsif N = 1 then
         if Transition_Exists (Vertex (1), Vertex (1), Matrix) then
            Low  := 1.0;
            High := 1.0;
         else
            Low  := 0.0;
            High := 0.0;
         end if;
      elsif N = 2 then
         declare
            I1  : constant Positive := Vertex (1);
            I2  : constant Positive := Vertex (2);
            T11 : constant Boolean  := Transition_Exists (I1, I1, Matrix);
            T12 : constant Boolean  := Transition_Exists (I1, I2, Matrix);
            T21 : constant Boolean  := Transition_Exists (I2, I1, Matrix);
            T22 : constant Boolean  := Transition_Exists (I2, I2, Matrix);
         begin
            if T12 and T21 then -- irreducible component
               High := Irreducible_2x2 (T11, T22);
               Low  := High;
               if T11 xor T22 then -- golden ratio
                  --  Could do better if we knew how the machine rounds
                  Low  := Float'Pred (Low);
                  High := Float'Succ (High);
               end if;
            elsif T11 or T22 then
               Low := 1.0;
               High := 1.0;
            else
               Low := 0.0;
               High := 0.0;
            end if;
         end;
      else -- N > 2
         declare
            Work : Work_Pointer := Storage;

            --  Source and Target are allowed to be the same
            procedure Iterate (
              Source : in     Vector;
              Target :    out Vector
            );

            procedure Iterate (
              Source : in     Vector;
              Target :    out Vector
            ) is
               Source_Base : constant Fortran_Integer := Source'First - 1;
               Target_Base : constant Fortran_Integer := Target'First - 1;
            begin
               for I in Work'Range loop
                  Work (I) := 0.0;
               end loop;

               for I in 1 .. N loop
                  Work (Vertex (I)) := Source (Source_Base + I);
               end loop;

               for I in 1 .. N loop
                  declare
                     Sum : Double_Precision := 0.0;

                     procedure Add_Contribution (
                       Column : in     Positive;
                       Stop   : in out Boolean
                     ) is
                        pragma Unreferenced (Stop);
                     begin
                        Sum := Sum + Work (Column);
                     end Add_Contribution;

                     procedure Sum_Column_Contributions is new
                       Transition_Matrix_Rows.Visit_Non_Zero_Columns (Add_Contribution);
                  begin
                     Sum_Column_Contributions (Matrix.Rows (Vertex (I)));
                     Target (Target_Base + I) := Sum;
                  end;
               end loop;
            end Iterate;

            procedure Compute_Eigenvector is
              new Extremal_Eigenvector (Iterate);

            Real_Part      : Vector_Pointer;
            Imaginary_Part : Vector_Pointer;

            Eigenvalue_R_P : Double_Precision;
            Eigenvalue_I_P : Double_Precision;

            Epsilon : constant Double_Precision := Float'Model_Epsilon;

            Allocate_Work : constant Boolean := (Work = null);

            Initial_Mode : constant IEEE.Rounding_Mode
              := IEEE.Get_Rounding_Mode;

            Iterations : Positive := Extra_Iterations;
         begin
            --  In what follows A denotes the matrix obtained from Matrix
            --  by deleting all rows and columns not in Vertices.  To
            --  estimate the spectral radius of A we exploit the fact that
            --  A is non-negative.  The following facts are consequences
            --  of this.  Fact 1: if v is any strictly positive vector,
            --  then the spectral radius <= max_i (Av)_i / v_i.  Fact 2:
            --  if v is any non-negative vector then min_i (Av)_i / v_i
            --  <= spectral radius, where the minimum is taken over those
            --  values of i for which v_i is non-zero.
            --
            --  The following code works well if A is irreducible, which
            --  is the case we care about since the main routine Estimate
            --  calls this one only for irreducible components.  If A is
            --  irreducible then we get a perfect estimate for the spectral
            --  radius when v is a non-negative eigenvector of A with
            --  eigenvalue lambda, where lambda is the spectral radius.
            --  The Perron-Frobenius theorem tells us that such a vector
            --  always exists when A is non-negative.  If A is irreducible
            --  then v is strictly positive, and both of the above facts can
            --  be used.  They give lambda <= spectral radius <= lambda.
            --
            --  In order to find v it is enough to find any eigenvector w
            --  of A whose eigenvalue mu satisfies |mu| = lambda.  Here mu
            --  and w may be complex.  Such an eigenvalue mu is called an
            --  extremal eigenvalue; the eigenvector w is called an extremal
            --  eigenvector.  If we have such a w then by taking absolute
            --  values of the components of w we obtain v.  This is based
            --  on the following result: if A is irreducible and w is an
            --  extremal eigenvector for A then A|w|=lambda|w| where |w| is
            --  the vector with components |w|_i = |w_i|.
            if Allocate_Work then
               Work := new Work_Vector (1 .. Matrix.Size);
            end if;

            Real_Part      := new Vector (1 .. N);
            Imaginary_Part := new Vector (1 .. N);

            --  Find an extremal eigenvector
            begin
               Compute_Eigenvector (
                 First          => 1,
                 Last           => N,
                 Real_Part      => Real_Part.all,
                 Imaginary_Part => Imaginary_Part.all,
                 Eigenvalue_R_P => Eigenvalue_R_P,
                 Eigenvalue_I_P => Eigenvalue_I_P,
                 Tolerance      => Epsilon
               );

               --  Take absolute values
               for I in 1 .. N loop
                  Real_Part (I) := Modulus (Real_Part (I), Imaginary_Part (I));
               end loop;
            exception
               when others =>
                  Put_Line ("    Arpack failed - trying power method");
                  for I in 1 .. N loop
                     Real_Part (I) := 1.0;
                  end loop;
                  Iterations := Power_Iterations;
            end;

            --  Improve the estimate by iterating a few (!) times
            for J in 1 .. Iterations loop
               Iterate (Real_Part.all, Real_Part.all);
               declare
                  Max : Double_Precision := Epsilon;
               begin
                  for I in 1 .. N loop
                     if Real_Part (I) > Max then
                        Max := Real_Part (I);
                     end if;
                  end loop;

                  for I in 1 .. N loop
                     Real_Part (I) := Real_Part (I) / Max;
                  end loop;
               end;
            end loop;

            --  High: we add Epsilon to each component

            IEEE.Set_Rounding_Mode (IEEE.Upwards);

            for I in 1 .. N loop
               Imaginary_Part (I) := Real_Part (I) + Epsilon;
            end loop;

            Iterate (Imaginary_Part.all, Imaginary_Part.all);

            declare
               Estimate : Double_Precision := 0.0;
               Divisor  : Double_Precision;
               --  Force the compiler to store the result in memory rather
               --  than in a register.  This makes the correctness of the
               --  algorithm easier to analyse, since registers can have
               --  more precision than the Double_Precision type;
               pragma Volatile (Divisor);
            begin
               for I in 1 .. N loop
                  Divisor := Real_Part (I) + Epsilon;
                  Estimate := Double_Precision'Max (
                    Estimate,
                    Imaginary_Part (I) / Divisor
                  );
               end loop;

               High := Float (Estimate);
            end;

            --  Low: we set any component less than Epsilon to zero

            IEEE.Set_Rounding_Mode (IEEE.Downwards);

            for I in 1 .. N loop
               if Real_Part (I) > Epsilon then
                  Imaginary_Part (I) := Real_Part (I);
               else
                  Imaginary_Part (I) := 0.0;
               end if;
            end loop;

            Iterate (Imaginary_Part.all, Imaginary_Part.all);

            declare
               Estimate : Double_Precision;
               Estimate_Set : Boolean := False;
            begin
               for I in 1 .. N loop
                  if Real_Part (I) > Epsilon then
                     if Estimate_Set then
                        Estimate := Double_Precision'Min (
                          Estimate,
                          Imaginary_Part (I) / Real_Part (I)
                        );
                     else
                        Estimate := Imaginary_Part (I) / Real_Part (I);
                        Estimate_Set := True;
                     end if;
                  end if;
               end loop;

               if Estimate_Set then
                  Low := Float (Estimate);
               else
                  Low := 0.0;
               end if;
            end;

            IEEE.Set_Rounding_Mode (Initial_Mode);

--  if High - Low > 2.0 then
--  declare
--     Max : Double_Precision := 0.0;
--     Min : Double_Precision := Double_Precision'Last;
--  begin
--  for I in 1 .. N loop
--     if Real_Part (I) > Max then
--        Max := Real_Part (I);
--     end if;
--     if Real_Part (I) < Min then
--        Min := Real_Part (I);
--     end if;
--  end loop;
--  for I in 1 .. N loop
--     Real_Part (I) := Real_Part (I) / Max;
--  end loop;
--  Min := Min / Max;
--  Put_Line ("Min:" & Double_Precision'Image (Min));
--  end;
--  for I in 1 .. N loop
--  Put_Line (Double_Precision'Image (Real_Part (I)));
--  end loop;
--  New_Line;
--  IO.Put_Line (Matrix, Vertices);
--  end if;
            Free (Real_Part);
            Free (Imaginary_Part);
            if Allocate_Work then
               Free (Work);
            end if;
         exception
            when others =>
               IEEE.Set_Rounding_Mode (Initial_Mode);
--  if N < 10 then
--  IO.Put_Line (Matrix, Vertices);
--  end if;
               Free (Real_Part);
               Free (Imaginary_Part);
               if Allocate_Work then
                  Free (Work);
               end if;
               raise;
         end;
      end if;
   end Component_Estimate;

   procedure Component_Estimate (
     Matrix    : in     Transition_Matrix_Type;
     Vertices  : in     Vertex_List;
     Low, High :    out Float
   ) is
   begin
      Component_Estimate (Matrix, Vertices, Low, High, null);
   end Component_Estimate;

   --------------
   -- Estimate --
   --------------
   procedure Estimate (
     Matrix     : in     Transition_Matrix_Type;
     Low, High  :    out Float
   ) is
      Work : Work_Pointer;

      procedure Process_SCC (
        Matrix   : Transition_Matrix_Type;
        Vertices : Vertex_List
      );
      pragma Inline (Process_SCC);

      procedure Process_SCC (
        Matrix   : Transition_Matrix_Type;
        Vertices : Vertex_List
      ) is
         SCC_Low, SCC_High : Float;
      begin
         Component_Estimate (Matrix, Vertices, SCC_Low, SCC_High, Work);

         Low  := Float'Max (SCC_Low, Low);
         High := Float'Max (SCC_High, High);
      end Process_SCC;

      procedure Process_Wandering_Vertices (
        Matrix   : Transition_Matrix_Type;
        Vertices : Vertex_List
      );
      pragma Inline (Process_Wandering_Vertices);

      procedure Process_Wandering_Vertices (
        Matrix   : Transition_Matrix_Type;
        Vertices : Vertex_List
      ) is
         pragma Unreferenced (Matrix, Vertices);
      begin
         null;
      end Process_Wandering_Vertices;

      procedure Decompose is new Transition_Matrices.SCC (
         Vertex_List   => Vertex_List,
         SCC_Action    => Process_SCC,
         Wander_Action => Process_Wandering_Vertices
      );
   begin
      Work := new Work_Vector (1 .. Matrix.Size);

      Low  := 0.0;
      High := 0.0;

      Decompose (Matrix);

      Free (Work);
   exception
      when others =>
         Free (Work);
         raise;
   end Estimate;
end Transition_Matrices.Spectral_Radius;
package body Transition_Matrices is

   use Transition_Matrix_Rows;

   ----------------------
   -- Clear_Transition --
   ----------------------

   procedure Clear_Transition (
     From   : in     Positive;
     To     : in     Positive;
     Matrix : in out Transition_Matrix_Type
   ) is
   begin
      Clear_Column (Matrix.Rows (From), To);
   end Clear_Transition;


   --------------------
   -- Set_Transition --
   --------------------

   procedure Set_Transition (
     From   : in     Positive;
     To     : in     Positive;
     Matrix : in out Transition_Matrix_Type
   ) is
   begin
      Set_Column (Matrix.Rows (From), To);
   end Set_Transition;


   -----------------------
   -- Transition_Exists --
   -----------------------

   function Transition_Exists (
     From   : Positive;
     To     : Positive;
     Matrix : Transition_Matrix_Type
   ) return Boolean is
   begin
      return Column_Is_Set (Matrix.Rows (From), To);
   end Transition_Exists;


   --------------
   -- Finalize --
   --------------

   procedure Finalize (Matrix : in out Transition_Matrix_Type) is
   begin
      for Row in Matrix.Rows'Range loop
         Clear_All (Matrix.Rows (Row));
      end loop;
   end Finalize;

end Transition_Matrices;
with Ada.Unchecked_Deallocation;

package body Transition_Matrix_Rows is

   Minimum_Heap_Size : constant := 10;

   procedure Free is new Ada.Unchecked_Deallocation (Column_List, Column_List_Pointer);

   -------------------------------
   -- Ensure_Heap_Is_Big_Enough --
   -------------------------------

   procedure Ensure_Heap_Is_Big_Enough (
     Row   : in out Matrix_Row;
     Index : in     Positive
   );
   --  Ensure that On_Heap contains the elements 1 .. Index by enlarging it if
   --  necessary.

   procedure Ensure_Heap_Is_Big_Enough (
     Row   : in out Matrix_Row;
     Index : in     Positive
   ) is
      Old_Heap : Column_List_Pointer := Row.On_Heap;
   begin
      if Old_Heap = null or else Old_Heap'Last < Index then
         Row.On_Heap := new Column_List (1 .. Positive'Max (2 * Index, Minimum_Heap_Size));
         --  Copy old values into new storage.
         for I in 1 .. Row.Non_Zeros - Row.On_Stack'Last loop
            Row.On_Heap (I) := Old_Heap (I);
         end loop;
         Free (Old_Heap);
      end if;
   end Ensure_Heap_Is_Big_Enough;


   -------------------
   -- Get_Last_Used --
   -------------------

   procedure Get_Last_Used (
     Row   : in     Matrix_Row;
     Stack :    out Natural;
     Heap  :    out Natural
   );
   pragma Inline (Get_Last_Used);
   --  Calculate the last elements of on-stack and on-heap storage used by the
   --  row (or zero if none used).

   procedure Get_Last_Used (
     Row   : in     Matrix_Row;
     Stack :    out Natural;
     Heap  :    out Natural
   ) is
   begin
      Stack := Natural'Min (Row.Non_Zeros, Row.On_Stack'Last);
      Heap  := Integer'Max (Row.Non_Zeros - Row.On_Stack'Last, 0);
   end Get_Last_Used;


   ---------------
   -- Clear_All --
   ---------------

   procedure Clear_All (Row : in out Matrix_Row) is
   begin
      Free (Row.On_Heap);
      Row.Non_Zeros := 0;
   end Clear_All;


   ------------------
   -- Clear_Column --
   ------------------

   procedure Clear_Column (
     Row    : in out Matrix_Row;
     Column : in     Positive
   ) is
      Stack_Last, Heap_Last : Natural;
   begin
      Get_Last_Used (Row, Stack => Stack_Last, Heap => Heap_Last);

      --  Look for the column on the stack.
      for I in 1 .. Stack_Last loop
         if Row.On_Stack (I) > Column then
            --  Not set.
            return;
         end if;

         if Row.On_Stack (I) = Column then
            --  Delete the column from the list.
            for J in I + 1 .. Stack_Last loop
               Row.On_Stack (J - 1) := Row.On_Stack (J);
            end loop;
            if Heap_Last > 0 then
               Row.On_Stack (Row.On_Stack'Last) := Row.On_Heap (1);
               if Heap_Last > 1 then
                  for J in 2 .. Heap_Last loop
                     Row.On_Heap (J - 1) := Row.On_Heap (J);
                  end loop;
               else
                  Free (Row.On_Heap);
               end if;
            end if;
            Row.Non_Zeros := Row.Non_Zeros - 1;
            return;
         end if;
      end loop;

      --  Look for the column on the heap.
      for I in 1 .. Heap_Last loop
         if Row.On_Heap (I) > Column then
            --  Not set.
            return;
         end if;

         if Row.On_Heap (I) = Column then
            --  Delete the column from the list.
            if Heap_Last > 1 then
               for J in I + 1 .. Heap_Last loop
                  Row.On_Heap (J - 1) := Row.On_Heap (J);
               end loop;
            else
               Free (Row.On_Heap);
            end if;
            Row.Non_Zeros := Row.Non_Zeros - 1;
            return;
         end if;
      end loop;
   end Clear_Column;


   -------------------
   -- Column_Is_Set --
   -------------------

   function Column_Is_Set (
     Row    : Matrix_Row;
     Column : Positive
   ) return Boolean is
      Stack_Last, Heap_Last : Natural;
   begin
      Get_Last_Used (Row, Stack => Stack_Last, Heap => Heap_Last);

      --  Look for the column on the stack.
      for I in 1 .. Stack_Last loop
         if Row.On_Stack (I) > Column then
            return False;
         end if;

         if Row.On_Stack (I) = Column then
            return True;
         end if;
      end loop;

      --  Look for the column on the heap.
      for I in 1 .. Heap_Last loop
         if Row.On_Heap (I) > Column then
            return False;
         end if;

         if Row.On_Heap (I) = Column then
            return True;
         end if;
      end loop;

      return False;
   end Column_Is_Set;


   ----------------
   -- Set_Column --
   ----------------

   procedure Set_Column (
     Row    : in out Matrix_Row;
     Column : in     Positive
   ) is
      Stack_Last, Heap_Last : Natural;
   begin
      Get_Last_Used (Row, Stack => Stack_Last, Heap => Heap_Last);

      --  Look for the column on the stack.
      for I in 1 .. Stack_Last loop
         if Row.On_Stack (I) = Column then
            --  Already set.
            return;
         end if;

         if Row.On_Stack (I) > Column then
            --  Insert the column before this list element.
            if Stack_Last = Row.On_Stack'Last then
               --  Move the last stack element to the heap.
               Ensure_Heap_Is_Big_Enough (Row, Heap_Last + 1);
               for J in reverse 1 .. Heap_Last loop
                  Row.On_Heap (J + 1) := Row.On_Heap (J);
               end loop;
               Row.On_Heap (1) := Row.On_Stack (Row.On_Stack'Last);
               for J in reverse I .. Row.On_Stack'Last - 1 loop
                  Row.On_Stack (J + 1) := Row.On_Stack (J);
               end loop;
            else
               for J in reverse I .. Stack_Last loop
                  Row.On_Stack (J + 1) := Row.On_Stack (J);
               end loop;
            end if;
            Row.On_Stack (I) := Column;
            Row.Non_Zeros := Row.Non_Zeros + 1;
            return;
         end if;
      end loop;

      --  Look for the column on the heap.
      for I in 1 .. Heap_Last loop
         if Row.On_Heap (I) = Column then
            --  Already set.
            return;
         end if;

         if Row.On_Heap (I) > Column then
            --  Insert the column before this list element.
            Ensure_Heap_Is_Big_Enough (Row, Heap_Last + 1);
            for J in reverse I .. Heap_Last loop
               Row.On_Heap (J + 1) := Row.On_Heap (J);
            end loop;
            Row.On_Heap (I) := Column;
            Row.Non_Zeros := Row.Non_Zeros + 1;
            return;
         end if;
      end loop;

      --  Append the column to the end of the list.
      if Stack_Last < Row.On_Stack'Last then
         --  It can be stored on the stack.
         Row.On_Stack (Stack_Last + 1) := Column;
      else
         --  It must be stored on the heap.
         Ensure_Heap_Is_Big_Enough (Row, Heap_Last + 1);
         Row.On_Heap (Heap_Last + 1) := Column;
      end if;
      Row.Non_Zeros := Row.Non_Zeros + 1;
   end Set_Column;


   ----------------------------
   -- Visit_Non_Zero_Columns --
   ----------------------------

   procedure Visit_Non_Zero_Columns (Row : Matrix_Row) is
      Stack_Last, Heap_Last : Natural;
      Stop : Boolean := False;
   begin
      Get_Last_Used (Row, Stack => Stack_Last, Heap => Heap_Last);
      for I in 1 .. Stack_Last loop
         Action (Row.On_Stack (I), Stop);
         if Stop then
            return;
         end if;
      end loop;
      for I in 1 .. Heap_Last loop
         Action (Row.On_Heap (I), Stop);
         if Stop then
            return;
         end if;
      end loop;
   end Visit_Non_Zero_Columns;

end Transition_Matrix_Rows;
package body Trim_Support is
   procedure Null_Action (
     Matrix   : Transition_Matrix_Type;
     Vertices : Index_List
   ) is
      pragma Unreferenced (Matrix, Vertices);
   begin
      null;
   end Null_Action;

   procedure Delete_Wandering (
     Matrix   : Transition_Matrix_Type;
     Vertices : Partition.Index_List
   ) is
      pragma Unreferenced (Matrix);
   begin
      Partition.Delete_Elements (Vertices);
   end Delete_Wandering;
end Trim_Support;
with Partition;
with Partition.Process_Transitions;
with Polygons;
with Symbolics;

function Upper_Transition_Matrix
  return Transition_Matrices.Transition_Matrix_Pointer is
   use Partition;
   use Polygons;
   use Symbolics;
   use Transition_Matrices;

   Matrix : constant Transition_Matrix_Pointer
     := new Transition_Matrix_Type (Element_Count);

   procedure Action (
     From_Polygon : Polygon_Type;
     From_Index   : Positive;
     From_Image   : Polygon_Type;
     To_Polygon   : Polygon_Type;
     To_Index     : Positive;
     Total_Symbol : Sequence_Type
   );
   pragma Inline (Action);

   procedure Action (
     From_Polygon : Polygon_Type;
     From_Index   : Positive;
     From_Image   : Polygon_Type;
     To_Polygon   : Polygon_Type;
     To_Index     : Positive;
     Total_Symbol : Sequence_Type
   ) is
      pragma Unreferenced (From_Polygon, From_Image, To_Polygon, Total_Symbol);
   begin
      Set_Transition (
        From   => From_Index,
        To     => To_Index,
        Matrix => Matrix.all
      );
   end Action;

   procedure Calculate_Matrix is new Process_Transitions (Action);
begin
   Calculate_Matrix;

   return Matrix;
end Upper_Transition_Matrix;
--
-- Permission to use, copy, modify, and distribute this software and its
-- documentation for any purpose and without fee is hereby granted,
-- provided that the above copyright and authorship notice appear in all
-- copies and that both that copyright notice and this permission notice
-- appear in supporting documentation.
--
-- The ARA makes no representations about the suitability of this software
-- for any purpose.  It is provided "as is" without express
-- or implied warranty.
--
--*****************************************************************************
--*
--*****************************************************************************
--*

--
-- Copyright (C) 1996 Ada Resource Association (ARA), Columbus, Ohio.
-- Author: David A. Wheeler
--

package body UStrings is

   Input_Line_Buffer_Length : constant := 1024;
   -- If an input line is longer, Get_Line will recurse to read in the line.

   procedure Swap (Left, Right : in out Unbounded_String) is
      -- Implement Swap.  This is the portable but slow approach.
      Temporary : constant Unbounded_String := Left;
   begin
      Left  := Right;
      Right := Temporary;
   end Swap;

   function Empty (S : in Unbounded_String) return Boolean is
   -- returns True if Length(S)=0.
   begin
      return (Length (S) = 0);
   end Empty;

   ------------------------------------------------------------------------
   -- Implement UNBOUNDED_STRING I/O by calling Text_IO String routines. --
   ------------------------------------------------------------------------

   -- Get_Line gets a line of text, limited only by the maximum number of
   -- characters in an UNBOUNDED_STRING.  It reads characters into a buffer
   -- and if that isn't enough, recurses to read the rest.

   procedure Get_Line (File : in File_Type; Item : out Unbounded_String) is

      function More_Input return Unbounded_String is
         Input : String (1 .. Input_Line_Buffer_Length);
         Last  : Natural;
      begin
         Get_Line (File, Input, Last);
         if (Last < Input'Last) then
            return To_Unbounded_String (Input (1 .. Last));
         else
            return To_Unbounded_String (Input (1 .. Last)) & More_Input;
         end if;
      end More_Input;

   begin
      Item := More_Input;
   end Get_Line;

   procedure Get_Line (Item : out Unbounded_String) is
   begin
      Get_Line (Current_Input, Item);
   end Get_Line;

   procedure Put (File : in File_Type; Item : in Unbounded_String) is
   begin
      Put (File, To_String (Item));
   end Put;

   procedure Put (Item : in Unbounded_String) is
   begin
      Put (Current_Output, To_String (Item));
   end Put;

   procedure Put_Line (File : in File_Type; Item : in Unbounded_String) is
   begin
      Put (File, Item);
      New_Line (File);
   end Put_Line;

   procedure Put_Line (Item : in Unbounded_String) is
   begin
      Put (Current_Output, Item);
      New_Line;
   end Put_Line;

   --*******************
   --* Other functions *
   --*******************

   function Copy
     (s     : in Unbounded_String;
      Index : in Positive;
      Size  : in Natural)
      return  Unbounded_String
   is
      Temp_Result   : Unbounded_String := Null_Unbounded_String;
      Source_Length : constant Natural := Length (s);
   begin
      if (Size = 0) then
         return Temp_Result;
      end if;
      declare
         Temp_Index  : Positive := Index;
         Result_Size : Natural  := 0;
         Number      : Positive;
      begin
         if (Positive (Size) + Index > Source_Length) then
            Number := Source_Length - Index + 1;
         else
            Number := Positive (Size);
         end if;
         for I in  1 .. Number loop
            Temp_Result := Temp_Result & Element (s, Temp_Index);
            Temp_Index  := Temp_Index + 1;
         end loop;
         return Temp_Result;
      end;
   end Copy;

   --*******************************
   --* Character related functions *
   --*******************************

   function Del_Spaces (Item : in Unbounded_String) return Unbounded_String is
      Result      : Unbounded_String := Null_Unbounded_String;
      Item_Length : constant Natural := Length (Item);
      Temp_Char   : Character;
   begin
      for I in  1 .. Item_Length loop
         Temp_Char := Element (Item, I);
         if (Temp_Char /= ' ') then
            Result := Result & Temp_Char;
         end if;
      end loop;
      return Result;
   end Del_Spaces;

   function Del_Character
     (Item : in Unbounded_String;
      Char : in Character)
      return Unbounded_String
   is
      Result      : Unbounded_String := Null_Unbounded_String;
      Item_Length : constant Natural := Length (Item);
      Temp_Char   : Character;
   begin
      for I in  1 .. Item_Length loop
         Temp_Char := Element (Item, I);
         if (Temp_Char /= Char) then
            Result := Result & Temp_Char;
         end if;
      end loop;
      return Result;
   end Del_Character;

   function Char_Count
     (S    : in Unbounded_String;
      Char : in Character)
      return Natural
   is
      Temp_Result   : Natural          := 0;
      String_Length : constant Natural := Length (S);
   begin
      for I in  1 .. String_Length loop
         if (Element (S, I) = Char) then
            Temp_Result := Temp_Result + 1;
         end if;
      end loop;
      return Temp_Result;
   end Char_Count;

   function First_Matching_Char_Position
     (S    : in Unbounded_String;
      Char : in Character)
      return Natural
   is
      String_Length : constant Natural := Length (S);
      I             : Natural          := 1;
   begin
      loop
         exit when (I > String_Length);
         if (Element (S, I) = Char) then
            return I;
         end if;
         I := I + 1;
      end loop;
      return 0;
   end First_Matching_Char_Position;

   function Last_Matching_Char_Position
     (S    : in Unbounded_String;
      Char : in Character)
      return Natural
   is
      String_Length : constant Natural := Length (S);
      I             : Natural          := String_Length;
   begin
      loop
         exit when (I = 0);
         if (Element (S, I) = Char) then
            return I;
         end if;
         I := I - 1;
      end loop;
      return 0;
   end Last_Matching_Char_Position;

   function Char_Replace
     (S                             : in Unbounded_String;
      Char_to_be_replaced, New_Char : in Character)
      return                          Unbounded_String
   is
      Temp_Result : Unbounded_String := Null_Unbounded_String;
      Temp        : Unbounded_String := S;
      I           : Natural;
   begin
      I := First_Matching_Char_Position (S, Char_to_be_replaced);
      while (I /= 0) loop
         Temp_Result := Copy (Temp, 1, I - 1) & New_char;
         Temp        := Copy (Temp, I + 1, Length (Temp) - I);
         I           :=
            First_Matching_Char_Position (Temp, Char_to_be_replaced);
      end loop;
      Temp_Result := Temp_Result & Temp;
      return Temp_Result;
   end Char_Replace;

   --**************************************************************************
   --****
   --**************************************************************************
   --****

end UStrings;

--
-- Permission to use, copy, modify, and distribute this software and its
-- documentation for any purpose and without fee is hereby granted,
-- provided that the above copyright and authorship notice appear in all
-- copies and that both that copyright notice and this permission notice
-- appear in supporting documentation.
--
-- The ARA makes no representations about the suitability of this software
-- for any purpose.  It is provided "as is" without express
-- or implied warranty.
--
