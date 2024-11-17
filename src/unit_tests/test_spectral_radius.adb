with IEEE;
with System.Storage_Elements;
with Transition_Matrices.Multiply;
with Transition_Matrices.Primitive;
with Transition_Matrices.SCC;
with Transition_Matrices.Spectral_Radius_Helpers;

package body Test_Spectral_Radius is

   use IEEE;
   use Transition_Matrices;
   use Transition_Matrices.Spectral_Radius_Helpers;

   Max_Systematic_Matrix_Size : constant := 4;
   Num_Power_Iterations : constant := 15;
   Arnoldi_Accuracy : constant Float := 0.0001;

   subtype Vertex_List is Transition_Matrices.Vertex_List;

   procedure Validate (Matrix : Transition_Matrix_Type);
   --  Check that SCC followed by Primitive makes sense.

   procedure Validate_SCC (
     Matrix : Transition_Matrix_Type;
     SCC    : Vertex_List
   );
   --  Check that Primitive makes sense for the given SCC.

   ---------------------
   -- Test_Systematic --
   ---------------------

   procedure Test_Systematic is
   begin
      --  Generate every possible small transition matrix and check it.
      for Size in 0 .. Max_Systematic_Matrix_Size loop
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
      Test_Systematic;
   end Test_All;

   --------------
   -- Validate --
   --------------

   procedure Validate (Matrix : Transition_Matrix_Type) is
      procedure SCC_Action (
        Matrix   : Transition_Matrix_Type;
        Vertices : Vertex_List
      ) is
      begin
         Validate_SCC (Matrix, Vertices);
      end SCC_Action;

      procedure Wander_Action (
        Matrix   : Transition_Matrix_Type;
        Vertices : Vertex_List
      ) is null;

      procedure Do_SCC is new SCC (Vertex_List, SCC_Action, Wander_Action);
   begin
      Do_SCC (Matrix);
   end Validate;

   ------------------
   -- Validate_SCC --
   ------------------

   procedure Validate_SCC (
     Matrix  : Transition_Matrix_Type;
     SCC    : Vertex_List
   ) is
      Storage : System.Storage_Elements.Storage_Array
        (1 .. System.Storage_Elements.Storage_Offset
          (Required_Storage_Length (Matrix)));

      type Vector_Base is array (Positive range <>) of Long_Integer;
      subtype Vector_Type is Vector_Base (1 .. Matrix.Size);

      procedure Multiply is new Transition_Matrices.Multiply
        (Long_Integer, 0, Vector_Base);
      --  Multiply the given vector using the transition matrix Matrix.

      ----------------------
      -- Primitive_Action --
      ----------------------

      procedure Primitive_Action (
        Matrix    : Transition_Matrix_Type;
        Primitive : Vertex_List;
        Period    : Positive
      ) is
         Input, Output, Start : Vector_Type;

         Max_Num, Max_Den, Min_Num, Min_Den, This_Num, This_Den : Long_Integer;

         Low, High, Est_Low, Est_High : Float;
      begin
         --  Estimate the spectral radius using the power method.
         for V of Input loop
            V := 0;
         end loop;
         for P of Primitive loop
            Input (P) := 1;
         end loop;

         for Count in 1 .. Num_Power_Iterations * Period loop
            Multiply (Input => Input, Output => Output, Matrix => Matrix);
            Input := Output;
         end loop;

         --  Our approximate Perron-Frobenious eigenvector.
         Start := Output;
         --  Multiply it Period times.
         for Count in 1 .. Period loop
            Multiply (Input => Input, Output => Output, Matrix => Matrix);
            Input := Output;
         end loop;

         --  Find the maximum of Output (P) / Start (P) as a rational number
         --  Max_Num / Max_Den over the primitive component.  Likewise
         --  Min_Num / Min_Den is the minimum of Output (P) / Start (P).
         Max_Num := -1; Min_Num := -1; -- Indicates no value.
         for P of Primitive loop
            This_Num := Output (P);
            This_Den := Start (P);

            if Max_Num < 0 then
               Max_Num := This_Num;
               Max_Den := This_Den;
            elsif Max_Num * This_Den < This_Num * Max_Den then
               --  This_Num / This_Den is bigger than Max_Num / Max_Den.
               Max_Num := This_Num;
               Max_Den := This_Den;
            end if;

            if Min_Num < 0 then
               Min_Num := This_Num;
               Min_Den := This_Den;
            elsif Min_Num * This_Den > This_Num * Min_Den then
               --  This_Num / This_Den is smaller than Min_Num / Min_Den.
               Min_Num := This_Num;
               Min_Den := This_Den;
            end if;
         end loop;

         Primitive_Unreduced (Matrix, Primitive, Period, Low, High, Storage);
         if Low > High then
            raise Program_Error;
         end if;

         --  Compute the largest floating point number <= Min_Num / Min_Den.
         Est_Low := Divide (Convert (Min_Num, Downwards),
           Convert (Min_Den, Upwards), Downwards);

         --  Compute the smallest floating point number >= Max_Num / Max_Den.
         Est_High := Divide (Convert (Max_Num, Upwards),
           Convert (Max_Den, Downwards), Upwards);

         if Est_Low > Est_High then
            raise Program_Error;
         end if;

         if Primitive'Length < 3 then
            --  Estimate Low .. High should be pretty much perfect.
            if Low < Est_Low then
               raise Program_Error with "Poor low estimate";
            end if;
            if High > Est_High then
               raise Program_Error with "Poor high estimate";
            end if;
         else
            --  Estimate Low .. High should be pretty good, if not perfect.
            if abs (High - Low) > Arnoldi_Accuracy then
               raise Program_Error with "Poor Arnoldi accuracy";
            end if;

            --  The real value is in the range Est_Low .. Est_High.  Require
            --  it to be logically possible that some point in Low .. High
            --  is close to the real value.
            if Low <= Est_Low and High >= Est_High then
               null; --  [Low, High] contains real value.  OK.
            elsif High <= Est_Low then
               --  [Low, High] to the left of [Est_Low, Est_High].
               if Est_Low - High > Arnoldi_Accuracy then
                  raise Program_Error with "Arnoldi far from correct value";
               end if;
            elsif Est_High <= Low then
               --  [Low, High] to the right of [Est_Low, Est_High].
               if Low - Est_High > Arnoldi_Accuracy then
                  raise Program_Error with "Arnoldi far from correct value";
               end if;
            else
               pragma Assert (
                 (Est_Low < Low and Low < Est_High)
               or
                 (Est_Low < High and High < Est_High)
               );
               null; -- [Est_Low, Est_High] contains [Low, High].  OK.
            end if;
         end if;
      end Primitive_Action;

      procedure Do_Primitive is new Primitive (Vertex_List, Primitive_Action);
   begin
      Do_Primitive (Matrix, SCC);
   end Validate_SCC;

end Test_Spectral_Radius;
