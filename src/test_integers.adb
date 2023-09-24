with Integers;

procedure Test_Integers is

   use Integers;

   procedure Assert (B : Boolean);
   procedure Assert (B : Boolean) is
   begin
      if not B then
         raise Program_Error;
      end if;
   end;

   procedure Iff (B, C : Boolean);
   procedure Iff (B, C : Boolean) is
   begin
      Assert (B = C);
   end Iff;

   function GCD (L, R : Integer) return Integer;
   function GCD (L, R : Integer) return Integer is
      A : Integer := abs L;
      B : Integer := abs R;
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

   procedure Test_Two (I, J : Integer);
   procedure Test_Two (I, J : Integer) is
      S : Integer;
   begin
      Iff (I = 0, To_Integer_Type (I) = Zero);
      Iff (I = 1, To_Integer_Type (I) = One);
      Iff (I = J, To_Integer_Type (I) = To_Integer_Type (J));
      Iff (I < J, To_Integer_Type (I) < To_Integer_Type (J));
      Iff (I <= J, To_Integer_Type (I) <= To_Integer_Type (J));
      Iff (I > J, To_Integer_Type (I) > To_Integer_Type (J));
      Iff (I >= J, To_Integer_Type (I) >= To_Integer_Type (J));

      S := Sign (To_Integer_Type (I));
      Iff (I < 0, S < 0);
      Iff (I = 0, S = 0);
      Iff (I > 0, S > 0);

      Iff (Is_Negative (To_Integer_Type (I)), I < 0);
      Iff (Is_Positive (To_Integer_Type (I)), I > 0);
      Iff (Is_Zero (To_Integer_Type (I)), I = 0);

      if I /= Integer'First then
         Assert (To_Integer_Type (abs I) = abs (To_Integer_Type (I)));
      end if;

      begin
         S := I + J;
      exception
         when others =>
            goto Skip_Add;
      end;
      Assert (To_Integer_Type (I) + To_Integer_Type (J) = To_Integer_Type (S));

    <<Skip_Add>>
      begin
         S := I - J;
      exception
         when others =>
            goto Skip_Sub;
      end;
      Assert (To_Integer_Type (I) - To_Integer_Type (J) = To_Integer_Type (S));

    <<Skip_Sub>>
      begin
         S := I * J;
      exception
         when others =>
            goto Skip_Mul;
      end;
      Assert (To_Integer_Type (I) * To_Integer_Type (J) = To_Integer_Type (S));

    <<Skip_Mul>>
      begin
         S := I / J;
      exception
         when others =>
            goto Skip_Div;
      end;
      Assert (To_Integer_Type (I) / To_Integer_Type (J) = To_Integer_Type (S));

    <<Skip_Div>>
      begin
         S := GCD (I, J);
      exception
         when others =>
            goto Skip_GCD;
      end;
      Assert (GCD (To_Integer_Type (I), To_Integer_Type (J)) = To_Integer_Type (S));

    <<Skip_GCD>>
      return;
   end Test_Two;

   Special : constant array (Positive range <>) of Integer := (
     Integer'First, Integer'First + 1, Integer'First + 2,
     Integer'Last, Integer'Last - 1, Integer'Last - 2
   );
begin
   for I in Integer'(-1000) .. 1000 loop
      for J in I .. 1000 loop
         Test_Two (I, J);
         if I /= J then
            Test_Two (J, I);
         end if;
      end loop;
      for J_Index in Special'Range loop
         Test_Two (I, Special (J_Index));
         Test_Two (Special (J_Index), I);
      end loop;
   end loop;
   for I_Index in Special'Range loop
      for J_Index in Special'Range loop
         Test_Two (Special (I_Index), Special (J_Index));
         if I_Index /= J_Index then
            Test_Two (Special (J_Index), Special (I_Index));
         end if;
      end loop;
   end loop;
end Test_Integers;
