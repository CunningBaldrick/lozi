with Ada.Unchecked_Deallocation;
with IEEE;
with System.Storage_Elements;
with Transition_Matrices.Primitive;
with Transition_Matrices.SCC;
with Transition_Matrices.Spectral_Radius_Helpers;

package body Transition_Matrices.Spectral_Radius is

   use IEEE;
   use Spectral_Radius_Helpers;
   use System.Storage_Elements;

   type Work_Pointer is access Storage_Array;

   procedure Free is new Ada.Unchecked_Deallocation
     (Storage_Array, Work_Pointer);

   procedure Primitive_Component_Estimate (
     Matrix    : in     Transition_Matrix_Type;
     Primitive : in     Vertex_List;
     Period    : in     Positive;
     Low, High :    out Float;
     Storage   :    out Storage_Array
   ) with Pre => Storage'Length >= Required_Storage_Length (Matrix);
   --  Estimates the spectral radius of (the orbit of) the primitive component
   --  with rows and columns in Primitive, by estimating the spectral radius
   --  of Matrix^Period restricted to Primitive, and returning the Period'th
   --  root.  The point of working with primitive components is that, thanks
   --  to removing circular symmetry, the Arnoldi method converges better.

   ----------------------------------
   -- Primitive_Component_Estimate --
   ----------------------------------

   procedure Primitive_Component_Estimate (
     Matrix    : in     Transition_Matrix_Type;
     Primitive : in     Vertex_List;
     Period    : in     Positive;
     Low, High :    out Float;
     Storage   :    out Storage_Array
   ) is
   begin
      if Primitive'Length = 0 then
         raise Constraint_Error;
      end if;

      Primitive_Unreduced (
        Matrix    => Matrix,
        Primitive => Primitive,
        Period    => Period,
        Low       => Low,
        High      => High,
        Storage   => Storage
      );
      --  Estimate the spectral radius of Matrix^Period acting on the
      --  primitive component.

      Low := Root_N (Low, Period, Downwards);
      High := Root_N (High, Period, Upwards);
      --  The spectral radius of Matrix acting on the orbit of the
      --  primitive period is the Period'th root.
   end Primitive_Component_Estimate;

   --------------
   -- Estimate --
   --------------

   procedure Estimate (
     Matrix     : in     Transition_Matrix_Type;
     Low, High  :    out Float
   ) is
      Work : Work_Pointer;

      procedure Process_SCC (
        Matrix : Transition_Matrix_Type;
        SCC    : Vertex_List
      );

      procedure Process_SCC (
        Matrix : Transition_Matrix_Type;
        SCC    : Vertex_List
      ) is
         procedure Process_Primitive (
           Matrix    : Transition_Matrix_Type;
           Primitive : Vertex_List;
           Period    : Positive
         ) is
            SCC_Low, SCC_High : Float;
         begin
            Primitive_Component_Estimate
              (Matrix, Primitive, Period, SCC_Low, SCC_High, Work.all);
            Low  := Float'Max (SCC_Low, Low);
            High := Float'Max (SCC_High, High);
         end Process_Primitive;

         procedure Decycle is new Transition_Matrices.Primitive
           (Vertex_List, Process_Primitive);
         --  Finds the period of the irreducible component and a set of nodes
         --  forming a primitive component of that period, and uses those to
         --  estimate the spectral-radius of the strongly connected component.
      begin
         Decycle (Matrix, SCC);
      end Process_SCC;

      procedure Process_Wandering_Vertices (
        Matrix   : Transition_Matrix_Type;
        Vertices : Vertex_List
      ) is null;

      procedure Decompose is new Transition_Matrices.SCC (
         Vertex_List   => Vertex_List,
         SCC_Action    => Process_SCC,
         Wander_Action => Process_Wandering_Vertices
      );
      --  Decomposes the matrix into irreducible (strongly connected)
      --  components, estimates the spectral radius of each, and then
      --  deduces from these an estimate of the spectral radius of the
      --  entire matrix.
   begin
      Work := new Storage_Array (1 .. Required_Storage_Length (Matrix));

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
