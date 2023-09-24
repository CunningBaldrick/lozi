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
