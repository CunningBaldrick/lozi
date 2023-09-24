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
