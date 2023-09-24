---------
-- GCD --
---------

function GCD (Left, Right : Integer) return Integer is
   A : Integer := abs Left;
   B : Integer := abs Right;
   C : Integer;
begin
   if A = B then
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
