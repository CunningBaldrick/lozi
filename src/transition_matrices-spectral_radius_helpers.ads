with Interfaces.Fortran;
with System.Storage_Elements;
with Vertices;

package Transition_Matrices.Spectral_Radius_Helpers is

   package SSE renames System.Storage_Elements;
   use type SSE.Storage_Count;

   function Required_Storage_Length (Matrix : Transition_Matrix_Type)
     return SSE.Storage_Count is (4 * SSE.Storage_Count (Matrix.Last_Row) *
       SSE.Storage_Count'Max (Interfaces.Fortran.Double_Precision
         'Max_Size_In_Storage_Elements, Long_Integer
           'Max_Size_In_Storage_Elements));
   --  How much work memory needs to be supplied to the routines below.

   procedure Primitive_1x1_Unreduced (
     Matrix    : in     Transition_Matrix_Type;
     Primitive : in     Vertices.Vertex_List;
     Period    : in     Positive;
     Low, High :    out Float;
     Storage   :    out SSE.Storage_Array
   ) with Pre => Primitive'Length = 1 and
     Storage'Length >= Required_Storage_Length (Matrix);
   --  Estimates the spectral radius of the Period'th power of Matrix,
   --  restricted to the given 1x1 primitive component.

   procedure Primitive_2x2_Unreduced (
     Matrix    : in     Transition_Matrix_Type;
     Primitive : in     Vertices.Vertex_List;
     Period    : in     Positive;
     Low, High :    out Float;
     Storage   :    out SSE.Storage_Array
   ) with Pre => Primitive'Length = 2 and
     Storage'Length >= Required_Storage_Length (Matrix);
   --  Estimates the spectral radius of the Period'th power of Matrix,
   --  restricted to the given 2x2 primitive component.

   procedure Primitive_3x3_Or_Bigger_Unreduced (
     Matrix    : in     Transition_Matrix_Type;
     Primitive : in     Vertices.Vertex_List;
     Period    : in     Positive;
     Low, High :    out Float;
     Storage   :    out SSE.Storage_Array
   ) with Pre => Primitive'Length >= 3 and
     Storage'Length >= Required_Storage_Length (Matrix);
   --  Estimates the spectral radius of the Period'th power of Matrix,
   --  restricted to the given primitive component, which should be 3x3
   --  or bigger.  While the 1x1 and 2x2 routines above produce pretty
   --  much perfect estimates, this one may produce poor (but correct)
   --  estimates in tough cases.  Uses an implicitly restarted Arnoldi
   --  method to find an approximate Perron-Frobenius eigenvector, then
   --  rigorously estimates the spectral radius using it.

   procedure Primitive_Unreduced (
     Matrix    : in     Transition_Matrix_Type;
     Primitive : in     Vertices.Vertex_List;
     Period    : in     Positive;
     Low, High :    out Float;
     Storage   :    out SSE.Storage_Array
   ) with Pre => Primitive'Length >= 1 and
     Storage'Length >= Required_Storage_Length (Matrix);
   --  Helper that just calls the right routine from those above.

end Transition_Matrices.Spectral_Radius_Helpers;
