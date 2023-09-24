--  Manipulate the rounding mode used for IEEE arithmetic.
package Unroll is
   pragma Elaborate_Body;

   Unsupported_Mode : exception;

   type Rounding_Mode is (To_Nearest, Upwards, Downwards, Towards_Zero);

   function Get_Rounding_Mode return Rounding_Mode;

end;
