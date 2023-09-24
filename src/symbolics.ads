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
