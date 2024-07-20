---------
-- GCD --
---------

function GCD (Left, Right : Integer) return Positive is
   A : Natural := abs Left;
   B : Natural := abs Right;
   C : Natural;
begin
   if A = B then
      if A = 0 then
         raise Constraint_Error with "gcd not defined";
      end if;
      return A;
   elsif A < B then
      C := A;
      A := B;
      B := C;
   end if;

   loop
      if B = 0 then
         return A;
      end if;

      C := B;
      B := A rem B;
      A := C;
   end loop;
end GCD;
