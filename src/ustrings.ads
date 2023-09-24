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
