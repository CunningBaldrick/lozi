with Ada.Numerics.Elementary_Functions;
with Interfaces.C; use Interfaces.C;

package body IEEE is
   function tonearest return int;
   pragma Import (C, tonearest);

   function upward return int;
   pragma Import (C, upward);

   function downward return int;
   pragma Import (C, downward);

   function towardzero return int;
   pragma Import (C, towardzero);

   Value : constant array (Rounding_Mode) of int := (
     To_Nearest   => tonearest,
     Upwards      => upward,
     Downwards    => downward,
     Towards_Zero => towardzero
   );

   ----------------------------
   -- Correctly_Rounded_Sqrt --
   ----------------------------

   function Correctly_Rounded_Sqrt (X : Float) return Float
     renames Ada.Numerics.Elementary_Functions.Sqrt;
   --  In practice all Sqrt implementations round correctly, because this has
   --  been an IEEE requirement for ages.

   -----------------------
   -- Get_Rounding_Mode --
   -----------------------

   function fegetround return int;
   pragma Import (C, fegetround);

   function Get_Rounding_Mode return Rounding_Mode is
      Result : int;
   begin
      Result := fegetround;

      for Mode in Rounding_Mode loop
         if Value (Mode) = Result then
            return Mode;
         end if;
      end loop;

      raise Program_Error;
   end Get_Rounding_Mode;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Rounding : in out Rounding_Section) is
   begin
      Set_Rounding_Mode (Rounding.Old_Mode);
   end Finalize;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (Rounding : in out Rounding_Section) is
   begin
      Rounding.Old_Mode := Get_Rounding_Mode;
      Set_Rounding_Mode (Rounding.Mode);
   end Initialize;

   -----------------------
   -- Set_Rounding_Mode --
   -----------------------

   function fesetround (round : int) return int;
   pragma Import (C, fesetround);

   procedure Set_Rounding_Mode (Mode : Rounding_Mode) is
      Result : int;
   begin
      Result := fesetround (Value (Mode));

      if Result /= 0 then
         raise Unsupported_Mode;
      end if;
   end Set_Rounding_Mode;
end IEEE;
