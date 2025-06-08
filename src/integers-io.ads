package Integers.IO is
   pragma Elaborate_Body;

   function Parse (Source : String) return Integer_Type;

   procedure Put (Source : Integer_Type);
end Integers.IO;
