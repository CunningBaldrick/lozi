with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO; -- QQ

with Ada.Unchecked_Deallocation;
with Arnoldi; use Arnoldi;
with Fortran_Complex_Types;
with IEEE;
with Interfaces.Fortran; use Interfaces.Fortran;
--  with Transition_Matrices.IO; -- QQ
with Transition_Matrices.SCC;
with Transition_Matrix_Rows;

package body Transition_Matrices.Spectral_Radius is

   Extra_Iterations : constant := 100;
   Power_Iterations : constant := 500;

   type Vector_Pointer is access Vector;

   type Work_Vector is array (Positive range <>) of Double_Precision;

   type Work_Pointer is access Work_Vector;

   procedure Free is new Ada.Unchecked_Deallocation (
     Vector,
     Vector_Pointer
   );

   procedure Free is new Ada.Unchecked_Deallocation (
     Work_Vector,
     Work_Pointer
   );

   function Modulus (X, Y : Double_Precision) return Double_Precision;
   pragma Inline (Modulus);

   function Modulus (X, Y : Double_Precision) return Double_Precision is
   begin
      --  Tricky to get right, so use compiler provided version.
      return Fortran_Complex_Types."abs" (Fortran_Complex_Types.Compose_From_Cartesian (X, Y));
   end Modulus;

   Irreducible_2x2 : constant array (Boolean, Boolean) of Float := (
     False => (
       False => 1.0,
       True  => 1.618033988749894848204586834365638117720309  -- golden ratio
     ),
     True => (
       False => 1.618033988749894848204586834365638117720309, -- golden ratio
       True  => 2.0
     )
   );

   procedure Component_Estimate (
     Matrix    : in     Transition_Matrix_Type;
     Vertices  : in     Vertex_List;
     Low, High :    out Float;
     Storage   : in     Work_Pointer
   );

   procedure Component_Estimate (
     Matrix    : in     Transition_Matrix_Type;
     Vertices  : in     Vertex_List;
     Low, High :    out Float;
     Storage   : in     Work_Pointer
   ) is
      N : constant Fortran_Integer := Vertices'Length;

      function Vertex (Index : Fortran_Integer) return Positive;
      pragma Inline (Vertex);

      function Vertex (Index : Fortran_Integer) return Positive is
      begin
         return Vertices (Vertices'First + Positive (Index) - 1);
      end Vertex;
   begin
      if N <= 0 then
         raise Constraint_Error;
      elsif N = 1 then
         if Transition_Exists (Vertex (1), Vertex (1), Matrix) then
            Low  := 1.0;
            High := 1.0;
         else
            Low  := 0.0;
            High := 0.0;
         end if;
      elsif N = 2 then
         declare
            I1  : constant Positive := Vertex (1);
            I2  : constant Positive := Vertex (2);
            T11 : constant Boolean  := Transition_Exists (I1, I1, Matrix);
            T12 : constant Boolean  := Transition_Exists (I1, I2, Matrix);
            T21 : constant Boolean  := Transition_Exists (I2, I1, Matrix);
            T22 : constant Boolean  := Transition_Exists (I2, I2, Matrix);
         begin
            if T12 and T21 then -- irreducible component
               High := Irreducible_2x2 (T11, T22);
               Low  := High;
               if T11 xor T22 then -- golden ratio
                  --  Could do better if we knew how the machine rounds
                  Low  := Float'Pred (Low);
                  High := Float'Succ (High);
               end if;
            elsif T11 or T22 then
               Low := 1.0;
               High := 1.0;
            else
               Low := 0.0;
               High := 0.0;
            end if;
         end;
      else -- N > 2
         declare
            Work : Work_Pointer := Storage;

            --  Source and Target are allowed to be the same
            procedure Iterate (
              Source : in     Vector;
              Target :    out Vector
            );

            procedure Iterate (
              Source : in     Vector;
              Target :    out Vector
            ) is
               Source_Base : constant Fortran_Integer := Source'First - 1;
               Target_Base : constant Fortran_Integer := Target'First - 1;
            begin
               for I in Work'Range loop
                  Work (I) := 0.0;
               end loop;

               for I in 1 .. N loop
                  Work (Vertex (I)) := Source (Source_Base + I);
               end loop;

               for I in 1 .. N loop
                  declare
                     Sum : Double_Precision := 0.0;

                     procedure Add_Contribution (
                       Column : in     Positive;
                       Stop   : in out Boolean
                     ) is
                        pragma Unreferenced (Stop);
                     begin
                        Sum := Sum + Work (Column);
                     end Add_Contribution;

                     procedure Sum_Column_Contributions is new
                       Transition_Matrix_Rows.Visit_Non_Zero_Columns (Add_Contribution);
                  begin
                     Sum_Column_Contributions (Matrix.Rows (Vertex (I)));
                     Target (Target_Base + I) := Sum;
                  end;
               end loop;
            end Iterate;

            procedure Compute_Eigenvector is
              new Extremal_Eigenvector (Iterate);

            Real_Part      : Vector_Pointer;
            Imaginary_Part : Vector_Pointer;

            Eigenvalue_R_P : Double_Precision;
            Eigenvalue_I_P : Double_Precision;

            Epsilon : constant Double_Precision := Float'Model_Epsilon;

            Allocate_Work : constant Boolean := (Work = null);

            Initial_Mode : constant IEEE.Rounding_Mode
              := IEEE.Get_Rounding_Mode;

            Iterations : Positive := Extra_Iterations;
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
            if Allocate_Work then
               Work := new Work_Vector (1 .. Matrix.Size);
            end if;

            Real_Part      := new Vector (1 .. N);
            Imaginary_Part := new Vector (1 .. N);

            --  Find an extremal eigenvector
            begin
               Compute_Eigenvector (
                 First          => 1,
                 Last           => N,
                 Real_Part      => Real_Part.all,
                 Imaginary_Part => Imaginary_Part.all,
                 Eigenvalue_R_P => Eigenvalue_R_P,
                 Eigenvalue_I_P => Eigenvalue_I_P,
                 Tolerance      => Epsilon
               );

               --  Take absolute values
               for I in 1 .. N loop
                  Real_Part (I) := Modulus (Real_Part (I), Imaginary_Part (I));
               end loop;
            exception
               when E : others =>
                  Put_Line ("    Arpack failed (" & Exception_Message (E) &
                    ") - trying power method");
                  for I in 1 .. N loop
                     Real_Part (I) := 1.0;
                  end loop;
                  Iterations := Power_Iterations;
            end;

            --  Improve the estimate by iterating a few (!) times
            for J in 1 .. Iterations loop
               Iterate (Real_Part.all, Real_Part.all);
               declare
                  Max : Double_Precision := Epsilon;
               begin
                  for I in 1 .. N loop
                     if Real_Part (I) > Max then
                        Max := Real_Part (I);
                     end if;
                  end loop;

                  for I in 1 .. N loop
                     Real_Part (I) := Real_Part (I) / Max;
                  end loop;
               end;
            end loop;

            --  High: we add Epsilon to each component

            IEEE.Set_Rounding_Mode (IEEE.Upwards);

            for I in 1 .. N loop
               Imaginary_Part (I) := Real_Part (I) + Epsilon;
            end loop;

            Iterate (Imaginary_Part.all, Imaginary_Part.all);

            declare
               Estimate : Double_Precision := 0.0;
               Divisor  : Double_Precision;
               --  Force the compiler to store the result in memory rather
               --  than in a register.  This makes the correctness of the
               --  algorithm easier to analyse, since registers can have
               --  more precision than the Double_Precision type;
               pragma Volatile (Divisor);
            begin
               for I in 1 .. N loop
                  Divisor := Real_Part (I) + Epsilon;
                  Estimate := Double_Precision'Max (
                    Estimate,
                    Imaginary_Part (I) / Divisor
                  );
               end loop;

               High := Float (Estimate);
            end;

            --  Low: we set any component less than Epsilon to zero

            IEEE.Set_Rounding_Mode (IEEE.Downwards);

            for I in 1 .. N loop
               if Real_Part (I) > Epsilon then
                  Imaginary_Part (I) := Real_Part (I);
               else
                  Imaginary_Part (I) := 0.0;
               end if;
            end loop;

            Iterate (Imaginary_Part.all, Imaginary_Part.all);

            declare
               Estimate : Double_Precision;
               Estimate_Set : Boolean := False;
            begin
               for I in 1 .. N loop
                  if Real_Part (I) > Epsilon then
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

            IEEE.Set_Rounding_Mode (Initial_Mode);

--  if High - Low > 2.0 then
--  declare
--     Max : Double_Precision := 0.0;
--     Min : Double_Precision := Double_Precision'Last;
--  begin
--  for I in 1 .. N loop
--     if Real_Part (I) > Max then
--        Max := Real_Part (I);
--     end if;
--     if Real_Part (I) < Min then
--        Min := Real_Part (I);
--     end if;
--  end loop;
--  for I in 1 .. N loop
--     Real_Part (I) := Real_Part (I) / Max;
--  end loop;
--  Min := Min / Max;
--  Put_Line ("Min:" & Double_Precision'Image (Min));
--  end;
--  for I in 1 .. N loop
--  Put_Line (Double_Precision'Image (Real_Part (I)));
--  end loop;
--  New_Line;
--  IO.Put_Line (Matrix, Vertices);
--  end if;
            Free (Real_Part);
            Free (Imaginary_Part);
            if Allocate_Work then
               Free (Work);
            end if;
         exception
            when others =>
               IEEE.Set_Rounding_Mode (Initial_Mode);
--  if N < 10 then
--  IO.Put_Line (Matrix, Vertices);
--  end if;
               Free (Real_Part);
               Free (Imaginary_Part);
               if Allocate_Work then
                  Free (Work);
               end if;
               raise;
         end;
      end if;
   end Component_Estimate;

   procedure Component_Estimate (
     Matrix    : in     Transition_Matrix_Type;
     Vertices  : in     Vertex_List;
     Low, High :    out Float
   ) is
   begin
      Component_Estimate (Matrix, Vertices, Low, High, null);
   end Component_Estimate;

   --------------
   -- Estimate --
   --------------
   procedure Estimate (
     Matrix     : in     Transition_Matrix_Type;
     Low, High  :    out Float
   ) is
      Work : Work_Pointer;

      procedure Process_SCC (
        Matrix   : Transition_Matrix_Type;
        Vertices : Vertex_List
      );
      pragma Inline (Process_SCC);

      procedure Process_SCC (
        Matrix   : Transition_Matrix_Type;
        Vertices : Vertex_List
      ) is
         SCC_Low, SCC_High : Float;
      begin
         Component_Estimate (Matrix, Vertices, SCC_Low, SCC_High, Work);

         Low  := Float'Max (SCC_Low, Low);
         High := Float'Max (SCC_High, High);
      end Process_SCC;

      procedure Process_Wandering_Vertices (
        Matrix   : Transition_Matrix_Type;
        Vertices : Vertex_List
      );
      pragma Inline (Process_Wandering_Vertices);

      procedure Process_Wandering_Vertices (
        Matrix   : Transition_Matrix_Type;
        Vertices : Vertex_List
      ) is
         pragma Unreferenced (Matrix, Vertices);
      begin
         null;
      end Process_Wandering_Vertices;

      procedure Decompose is new Transition_Matrices.SCC (
         Vertex_List   => Vertex_List,
         SCC_Action    => Process_SCC,
         Wander_Action => Process_Wandering_Vertices
      );
   begin
      Work := new Work_Vector (1 .. Matrix.Size);

      Low  := 0.0;
      High := 0.0;

      Decompose (Matrix);

      Free (Work);
   exception
      when others =>
         Free (Work);
         raise;
   end Estimate;
end Transition_Matrices.Spectral_Radius;
