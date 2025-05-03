with Ada.Exceptions;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Text_IO;
with Arnoldi;
with IEEE;
with Fortran_Complex_Types;
with Transition_Matrices.Multiply;

package body Transition_Matrices.Spectral_Radius_Helpers is

   use IEEE;
   use Interfaces.Fortran;
   use Vertices;

   Arnoldi_Extra_Multiplications : constant := 100;
   --  Number of extra multiplications to perform on approximate eigenvectors
   --  found using the Arnoldi method.

   Power_Multiplications_If_Arnoldi_Failed : constant := 500;
   --  If the Arnoldi method fails then we fall back on just multiplying a
   --  lot to find an approximate Perron-Frobenious eigenvector.  This is
   --  the number of multiplications to perform.

   package Double_Functions is new
     Ada.Numerics.Generic_Elementary_Functions (Double_Precision);
   --  In general you don't want to use these because they don't do/support
   --  proper rounding.  Here they are only used to calculate Best_Epsilon.

   Super_Small : constant Double_Precision
     := Double_Precision'Model_Epsilon;
   Best_Epsilon : constant Double_Precision
     := Double_Functions.Sqrt (Super_Small);
   Relax_Tolerance_By_On_Fail : constant := 16.0;
   Worst_Epsilon : constant Double_Precision
     := Double_Precision (Sqrt (Float'Model_Epsilon, To_Nearest));
   --  We first try to find an extremal eigenvector to precision Best_Epsilon,
   --  but if that doesn't work then we relax the precision by a factor of
   --  Relax_Tolerance_By_On_Fail and try again until Worst_Epsilon is reached.

   type Double_Work_Array is array (Positive range <>) of Double_Precision;
   type Long_Integer_Work_Array is array (Positive range <>) of Long_Integer;

   type Double_Vector is array (Vertex_Number range <>) of Double_Precision;
   type Long_Integer_Vector is array (Vertex_Number range <>) of Long_Integer;

   procedure Multiply_Exact is new Transition_Matrices.Multiply
     (Long_Integer, 0, Long_Integer_Vector);
   --  Multiply with perfect precision by using integers.

   procedure Multiply_Inexact is new Transition_Matrices.Multiply
     (Double_Precision, 0.0, Double_Vector);
   --  Inexact multiplication using floating point numbers.

   function Modulus (X, Y : Double_Precision) return Double_Precision is
      (Fortran_Complex_Types."abs" (Fortran_Complex_Types.
        Compose_From_Cartesian (X, Y)));
   --  Complex modulus.  Tricky to get right, so use compiler provided version.

   -------------------------
   -- Primitive_Unreduced --
   -------------------------

   procedure Primitive_Unreduced (
     Matrix    : in     Transition_Matrix_Type;
     Primitive : in     Vertex_List;
     Period    : in     Positive;
     Low, High :    out Float;
     Storage   :    out SSE.Storage_Array
   ) is
   begin
      case Primitive'Length is
         when 1 =>
            Primitive_1x1_Unreduced
              (Matrix, Primitive, Period, Low, High, Storage);
         when 2 =>
            Primitive_2x2_Unreduced
              (Matrix, Primitive, Period, Low, High, Storage);
         when others =>
            Primitive_3x3_Or_Bigger_Unreduced
              (Matrix, Primitive, Period, Low, High, Storage);
      end case;
   end Primitive_Unreduced;

   -----------------------------
   -- Primitive_1x1_Unreduced --
   -----------------------------

   procedure Primitive_1x1_Unreduced (
     Matrix    : in     Transition_Matrix_Type;
     Primitive : in     Vertex_List;
     Period    : in     Positive;
     Low, High :    out Float;
     Storage   :    out SSE.Storage_Array
   ) is
      Long_Integer_Storage : Long_Integer_Work_Array
        (1 .. 2 * Integer (Matrix.Last_Row))
          with Import, Address => Storage'Address;

      subtype Vector_Type is
        Long_Integer_Vector (1 .. Matrix.Last_Row);

      Input : Vector_Type with Import;
      for Input'Address use Long_Integer_Storage (1)'Address;
      Output : Vector_Type with Import;
      for Output'Address use Long_Integer_Storage
        (Integer (Matrix.Last_Row) + 1)'Address;

      T11 : Long_Integer; -- Must be positive (primitive component).
   begin
      --  Input is [0, ..., 0, 1, 0, ..., 0]
      for V of Input loop
         V := 0;
      end loop;
      Input (Primitive (Primitive'First)) := 1;

      --  Multiply Period times.
      for Count in 1 .. Period loop
         Multiply_Exact (Input => Input, Output => Output, Matrix => Matrix);
         if Count < Period then
            Input := Output;
         end if;
      end loop;

      --  The 1x1 transition matrix for the primitive component is exactly
      --    [T11]
      T11 := Output (Primitive (Primitive'First));
      pragma Assert (T11 > 0, "Not a primitive component?");

      --  The spectral radius of the Period'th power of Matrix is exactly
      --  T11, but if this is huge then converting to Float may involve
      --  rounding, so make sure to end up with Low <= T11 <= High.
      Low := Convert (T11, Downwards);
      High := Convert (T11, Upwards);
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
     Storage   :    out SSE.Storage_Array
   ) is
      Long_Integer_Storage : Long_Integer_Work_Array
        (1 .. 2 * Integer (Matrix.Last_Row))
          with Import, Address => Storage'Address;

      subtype Vector_Type is Long_Integer_Vector (1 .. Matrix.Last_Row);

      Input : Vector_Type with Import;
      for Input'Address use Long_Integer_Storage (1)'Address;
      Output : Vector_Type with Import;
      for Output'Address use Long_Integer_Storage
        (Integer (Matrix.Last_Row) + 1)'Address;

      T11, T12, T21, T22 : Long_Integer; -- Must be non-negative.
      Discriminant : Long_Integer;
      Sqrt_Discriminant, Spectral_Radius : Float;
   begin
      --  FIXME Is the computed matrix actually transposed (harmless)?

      --  First input is [0, ..., 1, 0, ..., 0]
      for V of Input loop
         V := 0;
      end loop;
      Input (Primitive (Primitive'First)) := 1;

      --  Multiply Period times.
      for Count in 1 .. Period loop
         Multiply_Exact (Input => Input, Output => Output, Matrix => Matrix);
         if Count < Period then
            Input := Output;
         end if;
      end loop;
      T11 := Output (Primitive (Primitive'First));
      T12 := Output (Primitive (Primitive'First + 1));

      --  Second input is [0, ..., 0, 1, ..., 0]
      for V of Input loop
         V := 0;
      end loop;
      Input (Primitive (Primitive'First + 1)) := 1;

      --  Multiply Period times.
      for Count in 1 .. Period loop
         Multiply_Exact (Input => Input, Output => Output, Matrix => Matrix);
         if Count < Period then
            Input := Output;
         end if;
      end loop;
      T21 := Output (Primitive (Primitive'First));
      T22 := Output (Primitive (Primitive'First + 1));

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
      for Mode in Upwards .. Downwards loop
         Sqrt_Discriminant := Sqrt (Convert (Discriminant, Mode), Mode);
         Spectral_Radius := Divide (
           Num => Sum (
             X => Convert (T11 + T22, Mode),
             Y => Sqrt_Discriminant,
             R => Mode
           ),
           Den => 2.0,
           R   => Mode
         );
         case Mode is
            when Downwards =>
               Low := Spectral_Radius;
            when Upwards =>
               High := Spectral_Radius;
         end case;
      end loop;
      pragma Assert (Low <= High);
   end Primitive_2x2_Unreduced;

   ---------------------------------------
   -- Primitive_3x3_Or_Bigger_Unreduced --
   ---------------------------------------

   procedure Primitive_3x3_Or_Bigger_Unreduced (
     Matrix    : in     Transition_Matrix_Type;
     Primitive : in     Vertex_List;
     Period    : in     Positive;
     Low, High :    out Float;
     Storage   :    out SSE.Storage_Array
   ) is
      Double_Precision_Storage : Double_Work_Array
        (1 .. 4 * Integer (Matrix.Last_Row))
          with Import, Address => Storage'Address;

      subtype Big_Vector_Type is Double_Vector (1 .. Matrix.Last_Row);

      subtype Vector_Type is Arnoldi.Vector (1 .. Primitive'Length);
      --  A vector for the primitive component.

      procedure Multiply (
        Source : in     Vector_Type;
        Target :    out Vector_Type
      );
      --  Source and Target are vectors for the primitive component, so
      --  may be much shorter than Matrix.Last_Row.  This multiplies them using
      --  the matrix from the primitive component to itself, which is the
      --  restriction of Matrix^Period to the primitive component.

      pragma Warnings (Off,
        "writable actual for ""Source"" overlaps with actual for ""Target""");
      --  It is OK for Source and Target to be the same when calling Multiply.

      procedure Multiply (
        Source : in     Vector_Type;
        Target :    out Vector_Type
      ) is
         function Vertex (
           Primitive : Vertex_List;
           Index : Fortran_Integer
         ) return Vertex_Number is
         begin
            return Primitive (Primitive'First + Positive (Index) - 1);
         end Vertex;

         Input : Big_Vector_Type with Import;
         for Input'Address use Double_Precision_Storage (1)'Address;
         Output : Big_Vector_Type with Import;
         for Output'Address use Double_Precision_Storage
           (Integer (Matrix.Last_Row) + 1)'Address;
      begin
         --  Turn the primitive component vector Source into a vector the size
         --  of the entire matrix, by putting zero for nodes outside of the
         --  primitive component, and the corresponding component of Source
         --  for nodes in the primitive component.
         for V of Input loop
            V := 0.0;
         end loop;
         for I in Source'Range loop
            Input (Vertex (Primitive, I)) := Source (I);
         end loop;

         --  Multiply it by the full matrix Period times.
         for Count in 1 .. Period loop
            Multiply_Inexact
              (Input => Input, Output => Output, Matrix => Matrix);
            if Count < Period then
               Input := Output;
            end if;
         end loop;

         --  Extract the components corresponding to the primitive component
         --  into Target.
         for I in Target'Range loop
            Target (I) := Output (Vertex (Primitive, I));
         end loop;
      end Multiply;

      procedure Compute_Eigenvector is
        new Arnoldi.Extremal_Eigenvector (Multiply);

      Real_Part : Vector_Type with Import;
      for Real_Part'Address use Double_Precision_Storage
        (2 * Integer (Matrix.Last_Row) + 1)'Address;
      Imaginary_Part : Vector_Type with Import;
      for Imaginary_Part'Address use Double_Precision_Storage
        (3 * Integer (Matrix.Last_Row) + 1)'Address;

      Eigenvalue_R_P : Double_Precision;
      Eigenvalue_I_P : Double_Precision;

      Epsilon : Double_Precision;

      Multiplications : Positive := Arnoldi_Extra_Multiplications;
   begin
      --  In what follows A denotes the matrix obtained from Matrix
      --  by deleting all rows and columns not in Vertices.  To
      --  estimate the spectral radius of A we exploit the fact that
      --  A is non-negative.  The following facts are consequences
      --  of this.  Fact 1: if v is any strictly positive vector,
      --  then the spectral radius <= max_i (Av)_i / v_i.  Fact 2:
      --  if v is any non-negative vector then min_i (Av)_i / v_i
      --  <= spectral radius, where the minimum is taken over those
      --  values of i for which v_i is non-zero.
      --
      --  The following code works well if A is irreducible, which
      --  is the case we care about since the main routine Estimate
      --  calls this one only for irreducible components.  If A is
      --  irreducible then we get a perfect estimate for the spectral
      --  radius when v is a non-negative eigenvector of A with
      --  eigenvalue lambda, where lambda is the spectral radius.
      --  The Perron-Frobenius theorem tells us that such a vector
      --  always exists when A is non-negative.  If A is irreducible
      --  then v is strictly positive, and both of the above facts can
      --  be used.  They give lambda <= spectral radius <= lambda.
      --
      --  In order to find v it is enough to find any eigenvector w
      --  of A whose eigenvalue mu satisfies |mu| = lambda.  Here mu
      --  and w may be complex.  Such an eigenvalue mu is called an
      --  extremal eigenvalue; the eigenvector w is called an extremal
      --  eigenvector.  If we have such a w then by taking absolute
      --  values of the components of w we obtain v.  This is based
      --  on the following result: if A is irreducible and w is an
      --  extremal eigenvector for A then A|w|=lambda|w| where |w| is
      --  the vector with components |w|_i = |w_i|.

      --  Find an extremal eigenvector
      Epsilon := Best_Epsilon;
      loop
         begin
            Compute_Eigenvector (
              First          => Vector_Type'First,
              Last           => Vector_Type'Last,
              Real_Part      => Real_Part,
              Imaginary_Part => Imaginary_Part,
              Eigenvalue_R_P => Eigenvalue_R_P,
              Eigenvalue_I_P => Eigenvalue_I_P,
              Tolerance      => Epsilon
            );

            --  Take absolute values
            for I in Vector_Type'Range loop
               Real_Part (I) := Modulus (Real_Part (I), Imaginary_Part (I));
            end loop;

            exit; -- Success
         exception
            when E : others =>
               Ada.Text_IO.Put_Line ("    Arpack failed (" &
                 Ada.Exceptions.Exception_Message (E) &
                   ") at tolerance" & Epsilon'Image);
               for I in Vector_Type'Range loop
                  Real_Part (I) := 1.0;
               end loop;

               Multiplications := Power_Multiplications_If_Arnoldi_Failed;
               --  Perform extra power multiplications even if Arnoldi succeeds
               --  with a lower precision, in an attempt to boost the precision
               --  back up again.
         end;

         Epsilon := Relax_Tolerance_By_On_Fail * Epsilon;

         if Epsilon > Worst_Epsilon then
            Ada.Text_IO.Put_Line ("    Falling back on power method");
            exit;
         end if;
      end loop;

      --  At this point the extremal eigenvector is stored in Real_Part,
      --  leaving Imaginary_Part as a handy storage area.

      --  Improve the estimate by multiplying a few (!) times.
      for J in 1 .. Multiplications loop
         --  Rescale the vector to have L-infinity norm 1.  This stops the
         --  elements from growing to something enormous as we iterate, and
         --  also brings the elements of the original extremal eigenvector,
         --  which are often quite small, up to a decent size the first time
         --  we get here.
         declare
            Max : Double_Precision := Super_Small;
         begin
            for I in Vector_Type'Range loop
               if Real_Part (I) > Max then
                  Max := Real_Part (I);
               end if;
            end loop;

            for I in Vector_Type'Range loop
               Real_Part (I) := Real_Part (I) / Max;
            end loop;
         end;

         Multiply (Real_Part, Real_Part);
      end loop;

      --  High.  We add a tiny value to each component in case it is zero.
      declare
         Rounding : Rounding_Section (Upwards) with Unreferenced;

         Estimate : Double_Precision := 0.0;
         Divisor  : Double_Precision;
         --  Force the compiler to store the result in memory rather
         --  than in a register.  This makes the correctness of the
         --  algorithm easier to analyse, since registers can have
         --  more precision than the Double_Precision type;
         pragma Volatile (Divisor);
      begin
         for I in Vector_Type'Range loop
            Imaginary_Part (I) := Real_Part (I) + Super_Small;
         end loop;

         Multiply (Imaginary_Part, Imaginary_Part);

         for I in Vector_Type'Range loop
            Divisor := Real_Part (I) + Super_Small;
            Estimate := Double_Precision'Max (
              Estimate,
              Imaginary_Part (I) / Divisor
            );
         end loop;

         High := Float (Estimate);
      end;

      --  Low: we set any tiny components to zero
      declare
         Rounding : Rounding_Section (Downwards) with Unreferenced;

         Estimate : Double_Precision;
         Estimate_Set : Boolean := False;
      begin
         for I in Vector_Type'Range loop
            if Real_Part (I) > Super_Small then
               Imaginary_Part (I) := Real_Part (I);
            else
               Imaginary_Part (I) := 0.0;
            end if;
         end loop;

         Multiply (Imaginary_Part, Imaginary_Part);

         for I in Vector_Type'Range loop
            if Real_Part (I) > Super_Small then
               if Estimate_Set then
                  Estimate := Double_Precision'Min (
                    Estimate,
                    Imaginary_Part (I) / Real_Part (I)
                  );
               else
                  Estimate := Imaginary_Part (I) / Real_Part (I);
                  Estimate_Set := True;
               end if;
            end if;
         end loop;

         if Estimate_Set then
            Low := Float (Estimate);
         else
            Low := 0.0;
         end if;
      end;
   end Primitive_3x3_Or_Bigger_Unreduced;

end Transition_Matrices.Spectral_Radius_Helpers;
