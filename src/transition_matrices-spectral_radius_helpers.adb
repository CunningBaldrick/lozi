with IEEE;
with Interfaces.Fortran; use Interfaces.Fortran;
with Transition_Matrices.Iterate;

package body Transition_Matrices.Spectral_Radius_Helpers is

   type Long_Integer_Work_Vector is array (Positive range <>) of Long_Integer;

   procedure Iterate_Exact is new Transition_Matrices.Iterate
     (Long_Integer, 0, Long_Integer_Work_Vector);
   --  Iterate with perfect precision by using integers.

   function Vertex (
     Primitive : Vertex_List;
     Index : Fortran_Integer
   ) return Positive with Inline;

   -----------------------------
   -- Primitive_1x1_Unreduced --
   -----------------------------

   procedure Primitive_1x1_Unreduced (
     Matrix    : in     Transition_Matrix_Type;
     Primitive : in     Vertex_List;
     Period    : in     Positive;
     Low, High :    out Float;
     Storage   :    out System.Storage_Elements.Storage_Array
   ) is
      Long_Integer_Storage : Long_Integer_Work_Vector (1 .. 2 * Matrix.Size)
        with Import, Address => Storage'Address;

      subtype Long_Integer_Matrix_Vector is
        Long_Integer_Work_Vector (1 .. Matrix.Size);

      Input : Long_Integer_Matrix_Vector with Import;
      for Input'Address use Long_Integer_Storage (1)'Address;
      Output : Long_Integer_Matrix_Vector with Import;
      for Output'Address use Long_Integer_Storage (Matrix.Size + 1)'Address;

      T11 : Long_Integer; -- Must be positive (primitive component).
   begin
      --  Input is [0, ..., 0, 1, 0, ..., 0]
      for V of Input loop
         V := 0;
      end loop;
      Input (Vertex (Primitive, 1)) := 1;

      --  Iterate Period times.
      for Count in 1 .. Period loop
         Iterate_Exact (Input => Input, Output => Output, Matrix => Matrix);
         if Count < Period then
            Input := Output;
         end if;
      end loop;

      --  The 1x1 transition matrix for the primitive component is exactly
      --    [T11]
      T11 := Output (Vertex (Primitive, 1));
      pragma Assert (T11 > 0, "Not a primitive component?");

      --  The spectral radius of the Period'th power of Matrix is exactly
      --  T11, but if this is huge then converting to Float may involve
      --  rounding, so make sure to end up with Low <= T11 <= High.
      declare
         Rounding : IEEE.Rounding_Section (IEEE.Downwards) with Unreferenced;
      begin
         Low := Float (T11);
      end;

      declare
         Rounding : IEEE.Rounding_Section (IEEE.Upwards) with Unreferenced;
      begin
         High := Float (T11);
      end;
      pragma Assert (Low <= High);
   end Primitive_1x1_Unreduced;

   -----------------------------
   -- Primitive_2x2_Unreduced --
   -----------------------------

   procedure Primitive_2x2_Unreduced (
     Matrix    : in     Transition_Matrix_Type;
     Primitive : in     Vertex_List;
     Period    : in     Positive;
     Low, High :    out Float;
     Storage   :    out System.Storage_Elements.Storage_Array
   ) is
      Long_Integer_Storage : Long_Integer_Work_Vector (1 .. 2 * Matrix.Size)
        with Import, Address => Storage'Address;

      subtype Long_Integer_Matrix_Vector is
        Long_Integer_Work_Vector (1 .. Matrix.Size);

      Input : Long_Integer_Matrix_Vector with Import;
      for Input'Address use Long_Integer_Storage (1)'Address;
      Output : Long_Integer_Matrix_Vector with Import;
      for Output'Address use Long_Integer_Storage (Matrix.Size + 1)'Address;

      T11, T12, T21, T22 : Long_Integer; -- Must be non-negative.
      Discriminant : Long_Integer;
      Sqrt_Discriminant, Spectral_Radius : Float;
   begin
      --  FIXME Is the computed matrix actually transposed (harmless)?

      --  First input is [0, ..., 1, 0, ..., 0]
      for V of Input loop
         V := 0;
      end loop;
      Input (Vertex (Primitive, 1)) := 1;

      --  Iterate Period times.
      for Count in 1 .. Period loop
         Iterate_Exact (Input => Input, Output => Output, Matrix => Matrix);
         if Count < Period then
            Input := Output;
         end if;
      end loop;
      T11 := Output (Vertex (Primitive, 1));
      T12 := Output (Vertex (Primitive, 2));

      --  Second input is [0, ..., 0, 1, ..., 0]
      for V of Input loop
         V := 0;
      end loop;
      Input (Vertex (Primitive, 2)) := 1;

      --  Iterate Period times.
      for Count in 1 .. Period loop
         Iterate_Exact (Input => Input, Output => Output, Matrix => Matrix);
         if Count < Period then
            Input := Output;
         end if;
      end loop;
      T21 := Output (Vertex (Primitive, 1));
      T22 := Output (Vertex (Primitive, 2));

      --  The 2x2 transition matrix for the primitive component is exactly
      --    [T11, T12]
      --    [T21, T22]
      pragma Assert (T11 >= 0 and T12 >= 0 and T21 >= 0 and T22 >= 0,
        "Not a primitive component?");
      Discriminant := (T11 - T22) ** 2 + 4 * T12 * T21;
      pragma Assert (Discriminant >= 0);

      --  The spectral radius of the Period'th power of Matrix is exactly
      --    Exact_SR := (T11 + T22 + Sqrt(Discriminant)) / 2
      --  but computing this may lose precision in every step, so make sure
      --  to end up with Low <= Exact_SR <= High.
      for Mode in IEEE.Upwards .. IEEE.Downwards loop
         declare
            Rounding : IEEE.Rounding_Section (Mode) with Unreferenced;
         begin
            Sqrt_Discriminant
              := IEEE.Correctly_Rounded_Sqrt (Float (Discriminant));
            Spectral_Radius := (Float (T11 + T22) + Sqrt_Discriminant) / 2.0;
            case Mode is
               when IEEE.Downwards =>
                  Low := Spectral_Radius;
               when IEEE.Upwards =>
                  High := Spectral_Radius;
            end case;
         end;
      end loop;
      pragma Assert (Low <= High);
   end Primitive_2x2_Unreduced;

   ------------
   -- Vertex --
   ------------

   function Vertex (
     Primitive : Vertex_List;
     Index : Fortran_Integer
   ) return Positive is
   begin
      return Primitive (Primitive'First + Positive (Index) - 1);
   end Vertex;

end Transition_Matrices.Spectral_Radius_Helpers;
