with Ada.Finalization;

package IEEE is
   pragma Elaborate_Body;

   --  Routines for manipulating the rounding mode used for IEEE arithmetic,
   --  plus some elementary function implementations that correctly follow
   --  the IEEE's rounding rules.

   Unsupported_Mode : exception;

   type Rounding_Mode is (To_Nearest, Upwards, Downwards, Towards_Zero);

   function Get_Rounding_Mode return Rounding_Mode;

   procedure Set_Rounding_Mode (Mode : Rounding_Mode);


   type Rounding_Section (Mode : Rounding_Mode) is new
     Ada.Finalization.Limited_Controlled with record
      Old_Mode : Rounding_Mode;
   end record;
   overriding procedure Initialize (Rounding : in out Rounding_Section);
   overriding procedure Finalize (Rounding : in out Rounding_Section);
   --  Helper that sets the rounding mode to Mode when initialized, and
   --  sets it back to whatever it was before when finalized.


   function Convert (
     I : Long_Integer;
     R : Rounding_Mode
   ) return Float;
   --  Convert I to Float type, correctly rounde according to the given
   --  rounding mode.  Rounding occurs if I is very big.

   function Divide (
     Num, Den : Float;
     R : Rounding_Mode
   ) return Float;
   --  Returns Num / Den, correctly rounded according to the given rounding
   --  mode.

   function Pow (
     X, Y : Float;
     R : Rounding_Mode
   ) return Float;
   --  X ^ Y, correctly rounded according to the given rounding mode.

   function Root_N (
     X : Float;
     N : Positive;
     R : Rounding_Mode
   ) return Float with Pre => R in Upwards | Downwards;
   --  Returns X ^ (1/N), rounded according to the given rounding mode.
   --  NOTE: Only Upwards and Downwards rounding is supported.
   --  NOTE: The rounding is not "correct", in the sense that the result
   --  may not be optimal, for example when rounding up there may be a
   --  smaller floating point number which is also >= the mathematical
   --  X ^ (1/N), so would have been more optimal to return.

   function Sqrt (
     X : Float;
     R : Rounding_Mode
   ) return Float;
   --  Sqrt (X), correctly rounded according to the given rounding mode.

   function Sum (
     X, Y : Float;
     R : Rounding_Mode
   ) return Float;
   --  Returns X + Y, correctly rounded according to the given rounding mode.

end IEEE;
