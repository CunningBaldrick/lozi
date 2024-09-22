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


   function Correctly_Rounded_Sqrt (X : Float) return Float;
   --  Sqrt that properly follows IEEE rounding rules.

end IEEE;
