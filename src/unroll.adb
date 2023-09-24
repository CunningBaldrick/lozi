with Interfaces.C; use Interfaces.C;

package body Unroll is
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
end;
