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

   function cr_powf(X, Y : Float) return Float with Import,
     Convention => C, Link_Name => "cr_powf";

   -------------
   -- Convert --
   -------------

   function Convert (
     I : Long_Integer;
     R : Rounding_Mode
   ) return Float is
      Rounding : Rounding_Section (R) with Unreferenced;
   begin
      return Float (I);
   end Convert;

   ------------
   -- Divide --
   ------------

   function Divide (
     Num, Den : Float;
     R : Rounding_Mode
   ) return Float is
      Rounding : Rounding_Section (R) with Unreferenced;
   begin
      return Num / Den;
   end Divide;

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

   ---------
   -- Pow --
   ---------

   function Pow (
     X, Y : Float;
     R : Rounding_Mode
   ) return Float is
      Rounding : Rounding_Section (R) with Unreferenced;
   begin
      return cr_powf (X, Y);
   end Pow;

   ------------
   -- Root_N --
   ------------

   function Root_N (
     X : Float;
     N : Positive;
     R : Rounding_Mode
   ) return Float is
      N_Lo, N_Hi, Pow_Lo, Pow_Hi : Float;
   begin
      if N = 1 then
         return X;
      elsif N = 2 then
         return Sqrt (X, R);
      end if;

      if X = 0.0 then
         return X;
      end if;

      if not (X > 0.0) then
         raise Constraint_Error;
      end if;

      --  If N is huge then converting it to Float may round.
      N_Lo := Convert (Long_Integer (N), Downwards);
      N_Hi := Convert (Long_Integer (N), Upwards);

      --  Now compute upper and lower estimates for 1/N.
      Pow_Hi := Divide (1.0, N_Lo, Upwards);
      Pow_Lo := Divide (1.0, N_Hi, Downwards);
      pragma Assert (Pow_Lo <= Pow_Hi and Pow_Hi <= 1.0);

      return (case R is
        when Downwards =>
          (if X >= 1.0 then Pow (X, Pow_Lo, R)
           else Pow (X, Pow_Hi, R)),
        when Upwards =>
          (if X >= 1.0 then Pow (X, Pow_Hi, R)
           else Pow (X, Pow_Lo, R)),
        when others =>
          raise Program_Error
      );
   end Root_N;

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

   ----------
   -- Sqrt --
   ----------

   function Sqrt (
     X : Float;
     R : Rounding_Mode
   ) return Float is
      Rounding : Rounding_Section (R) with Unreferenced;
   begin
      --  In practice all Sqrt implementations round correctly, because this
      --  has been an IEEE requirement for ages.
      return Ada.Numerics.Elementary_Functions.Sqrt (X);
   end Sqrt;

   ---------
   -- Sum --
   ---------

   function Sum (
     X, Y : Float;
     R : Rounding_Mode
   ) return Float is
      Rounding : Rounding_Section (R) with Unreferenced;
   begin
      return X + Y;
   end Sum;

end IEEE;
