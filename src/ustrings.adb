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
