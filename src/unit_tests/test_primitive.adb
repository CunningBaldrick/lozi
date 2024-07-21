with GCD;
with System;
with Transition_Matrices.Iterate;
with Transition_Matrices.Primitive;
with Transition_Matrices.SCC;

package body Test_Primitive is

   use Transition_Matrices;

   type Vertex_List is array (Natural range <>) of Positive;

   procedure Check_One (
     Matrix   : Transition_Matrix_Type;
     Vertices : Vertex_List;
     Expected_Vertices : Vertex_List;
     Expected_Period   : Positive
   );

   procedure Validate (Matrix : Transition_Matrix_Type);
   --  Check that SCC followed by Primitive makes sense.

   procedure Validate_SCC (
     Matrix : Transition_Matrix_Type;
     SCC    : Vertex_List
   );
   --  Check that Primitive makes sense for the given SCC.

   ---------------
   -- Check_One --
   ---------------

   procedure Check_One (
     Matrix   : Transition_Matrix_Type;
     Vertices : Vertex_List;
     Expected_Vertices : Vertex_List;
     Expected_Period   : Positive
   ) is
      Was_Checked : Boolean := False;

      procedure Do_Check (
        Matrix   : Transition_Matrix_Type;
        Vertices : Vertex_List;
        Period   : Positive
      ) is
      begin
         pragma Assert (not Was_Checked, "Called twice");
         Was_Checked := True;
         pragma Assert (System."=" (Do_Check.Matrix'Address,
           Check_One.Matrix'Address), "Matrix not passed through");
         pragma Assert (Period = Expected_Period, "Wrong period" & Period'Img &
           "; expected" & Expected_Period'Img);
         pragma Assert (Vertices = Expected_Vertices, "Wrong vertices");
      end Do_Check;

      procedure Do_Primitive is new Primitive (Vertex_List, Do_Check);
   begin
      Do_Primitive (Matrix, Vertices);
      pragma Assert (Was_Checked, "Not called");
   end Check_One;

   -------------------
   -- Test_Specific --
   -------------------

   procedure Test_Specific is
      --  Check some specific cases.
   begin
      declare
         M : Transition_Matrix_Type (0); -- 0x0
         V : Vertex_List (1 .. 0);
      begin
         --  Empty.
         Check_One (M, V, V, 1);
      end;

      declare
         M : Transition_Matrix_Type (1); -- 1x1 but empty vertex list.
         V : Vertex_List (1 .. 0);
      begin
         --  Empty.
         Check_One (M, V, V, 1);
      end;

      declare
         M : Transition_Matrix_Type (1); -- 1x1
         V : constant Vertex_List (1 .. 1) := (1 => 1);
      begin
         --  Need a self-transition to form a SCC.
         Set_Transition (From => 1, To => 1, Matrix => M);
         Check_One (M, V, V, 1);
      end;

      declare
         M : Transition_Matrix_Type (2); -- 2x2
         V : constant Vertex_List (1 .. 2) := (1 => 1, 2 => 2);
      begin
         --  No cycle.
         Set_Transition (From => 1, To => 1, Matrix => M);
         Set_Transition (From => 1, To => 2, Matrix => M);
         Set_Transition (From => 2, To => 1, Matrix => M);
         Check_One (M, V, V, 1);
      end;

      declare
         M : Transition_Matrix_Type (2); -- 2x2
         V : constant Vertex_List (1 .. 2) := (1 => 1, 2 => 2);
         E : constant Vertex_List (1 .. 1) := (1 => 1);
      begin
         --  Cycle of period 2.
         Set_Transition (From => 1, To => 2, Matrix => M);
         Set_Transition (From => 2, To => 1, Matrix => M);
         Check_One (M, V, E, 2);
      end;

      declare
         M : Transition_Matrix_Type (2); -- 2x2
         V : constant Vertex_List (1 .. 1) := (1 => 2);
         E : constant Vertex_List (1 .. 1) := (1 => 2);
      begin
         Set_Transition (From => 2, To => 1, Matrix => M);
         Set_Transition (From => 2, To => 2, Matrix => M);
         Check_One (M, V, E, 1);
      end;

      declare
         M : Transition_Matrix_Type (3); -- 3x3
         V : constant Vertex_List (1 .. 3) := (1 => 1, 2 => 2, 3 => 3);
         E : constant Vertex_List (1 .. 1) := (1 => 1);
      begin
         --  Cycle of period 2 (NB: 2 doesn't divide 3).
         Set_Transition (From => 1, To => 2, Matrix => M);
         Set_Transition (From => 1, To => 3, Matrix => M);
         Set_Transition (From => 2, To => 1, Matrix => M);
         Set_Transition (From => 3, To => 1, Matrix => M);
         Check_One (M, V, E, 2);
      end;

   end Test_Specific;

   ---------------------
   -- Test_Systematic --
   ---------------------

   procedure Test_Systematic is
   begin
      --  Generate every possible small transition matrix and check it.
      for Size in 0 .. 4 loop
         declare
            Transitions : array (1 .. Size * Size) of Boolean
              := (others => False); -- "Binary number"
            Incremented : Boolean;
          begin
             loop -- Executes 2 ** (Size^2) times.
                declare
                   M : Transition_Matrix_Type (Size);
                begin
                   for I in 1 .. Size loop
                      for J in 1 .. Size loop
                         if Transitions ((I - 1) * Size + J) then
                            Set_Transition (From => I, To => J, Matrix => M);
                         end if;
                      end loop;
                   end loop;

                   Validate (M);
                end;

                --  Increment the "binary number".
                Incremented := False;
                for I in Transitions'Range loop
                   if Transitions (I) = False then
                      Transitions (I) := True;
                      Incremented := True;
                      exit;
                   end if;
                   Transitions (I) := False;
                end loop;
                exit when not Incremented;
             end loop;
          end;
      end loop;
   end Test_Systematic;

   --------------
   -- Test_All --
   --------------

   procedure Test_All is
   begin
      Test_Specific;
      Test_Systematic;
   end Test_All;

   --------------
   -- Validate --
   --------------

   procedure Validate (Matrix : Transition_Matrix_Type) is
      Visited : array (1 .. Matrix.Size) of Boolean := (others => False);

      procedure Check_Disjoint (
        Matrix   : Transition_Matrix_Type;
        Vertices : Vertex_List
      ) is
         pragma Unreferenced (Matrix);
      begin
         for V of Vertices loop
            pragma Assert (not Visited (V));
            Visited (V) := True;
         end loop;
      end Check_Disjoint;

      procedure SCC_Action (
        Matrix   : Transition_Matrix_Type;
        Vertices : Vertex_List
      ) is
      begin
         Check_Disjoint (Matrix, Vertices);
         Validate_SCC (Matrix, Vertices);
      end SCC_Action;

      procedure Wander_Action (
        Matrix   : Transition_Matrix_Type;
        Vertices : Vertex_List
      ) is
      begin
         Check_Disjoint (Matrix, Vertices);
      end Wander_Action;

      procedure Do_SCC is new SCC (Vertex_List, SCC_Action, Wander_Action);
   begin
      Do_SCC (Matrix);
      pragma Assert (for all V of Visited => V);
   end Validate;

   ------------------
   -- Validate_SCC --
   ------------------

   procedure Validate_SCC (
     Matrix  : Transition_Matrix_Type;
     SCC    : Vertex_List
   ) is
      type Vector_Base is array (Positive range <>) of Integer;
      subtype Vector_Type is Vector_Base (1 .. Matrix.Size);

      function Compute_Period return Positive;
      --  Compute the the gcd of the lengths of all cycles in the SCC in an
      --  inefficient but straightforward way.

      procedure Iterate (Vec : in out Vector_Type);
      --  Iterate the given vector using the transition matrix Matrix
      --  while performing sanity checks.

      procedure Unchecked_Iterate is new Transition_Matrices.Iterate
        (Integer, 0, Vector_Base);
      --  Iterate the given vector using the transition matrix Matrix.
      --  Helper for Iterate.

      -------------
      -- Iterate --
      -------------

      procedure Iterate (Vec : in out Vector_Type) is
         W : Vector_Type;
         R : Vector_Type;
      begin
         --  Any paths that lead out of the SCC should never reenter it.
         --  Check this by removing from Vec anything that is outside SCC
         --  (W is the result) and check that iterating Vec and W results
         --  in the same thing.
         W := (others => 0);
         for V of SCC loop
            W (V) := Vec (V);
         end loop;
         Unchecked_Iterate (Vec, R, Matrix);
         Vec := R;
         Unchecked_Iterate (W, R, Matrix);
         W := R;
         for V of SCC loop
            --  Paths that leave the SCC should never reenter it.
            pragma Assert (W (V) = Vec (V), "Not a SCC?");
         end loop;
      end Iterate;

      --------------------
      -- Compute_Period --
      --------------------

      function Compute_Period return Positive is
         Period_Has_Value : Boolean := False;
         Period : Positive := Positive'Last;
      begin
         if SCC'Length = 0 then
            return 1; -- By convention.
         end if;

         for Node in SCC'Range loop
            --  For each node in the SCC, look for cycles starting from this
            --  node with length at most the number of nodes in the SCC.  It
            --  is enough to consider simple cycles (i.e. where each node is
            --  visited at most once), as non-simple cycles can be decomposed
            --  into multiple simple ones, so the gcd does not change.  As any
            --  cycle longer than the size of the SCC is not simple, there is
            --  no need to consider cycles longer than this.
            declare
               Vec : Vector_Type;
            begin
               Vec := (others => 0);
               Vec (SCC (Node)) := 1;

               for Count in 1 .. SCC'Length loop
                  Iterate (Vec);
                  if Vec (SCC (Node)) /= 0 then
                     --  There is a path from Node to itself with length Count.
                     if Period_Has_Value then
                        Period := GCD (Period, Count);
                     else
                        Period_Has_Value := True;
                        Period := Count;
                     end if;
                  end if;
               end loop;
            end;
         end loop;
         if Period_Has_Value then
            return Period;
         end if;
         raise Program_Error with "Failed to compute period";
      end Compute_Period;

      ----------------------
      -- Primitive_Action --
      ----------------------

      Was_Called : Boolean := False;

      procedure Primitive_Action (
        Matrix    : Transition_Matrix_Type;
        Primitive : Vertex_List;
        Period    : Positive
      ) is
         Visited : array (1 .. Matrix.Size) of Boolean := (others => False);

         Vec : Vector_Type;
      begin
         Was_Called := True;

         declare
            Expected_Period : constant Positive := Compute_Period;
         begin
            pragma Assert (Period = Expected_Period, "Wrong period" &
              Period'Img & "; expected" & Expected_Period'Img);
         end;

         Vec := (others => 0);
         for V of Primitive loop
            Vec (V) := 1;
            Visited (V) := True;
         end loop;
         for I in 1 .. Period - 1 loop
            Iterate (Vec);

            for V of SCC loop
               if Vec (V) > 0 then
                  pragma Assert (not Visited (V), "Period too long");
                  Visited (V) := True;
               end if;
            end loop;
         end loop;

         for V of SCC loop
            pragma Assert (Visited (V), "Period too short");
         end loop;

         Iterate (Vec);
         for V of Primitive loop
            Vec (V) := 0;
         end loop;
         for V of SCC loop
            pragma Assert (Vec (V) = 0);
         end loop;
      end Primitive_Action;

      procedure Do_Primitive is new Primitive (Vertex_List, Primitive_Action);
   begin
      Do_Primitive (Matrix, SCC);
      pragma Assert (Was_Called, "Primitive_Action not called");
   end Validate_SCC;

end Test_Primitive;
