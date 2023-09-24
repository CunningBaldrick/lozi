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
