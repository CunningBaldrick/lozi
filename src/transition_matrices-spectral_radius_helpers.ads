with Interfaces.Fortran;
with System.Storage_Elements;

package Transition_Matrices.Spectral_Radius_Helpers is

   function Required_Storage_Length (Matrix : Transition_Matrix_Type)
     return Natural is (2 * Matrix.Size * Integer'Max
       (Interfaces.Fortran.Double_Precision'Max_Size_In_Storage_Elements,
         Long_Integer'Max_Size_In_Storage_Elements));
   --  How much work memory needs to be supplied to the routines below.

   procedure Primitive_1x1_Unreduced (
     Matrix    : in     Transition_Matrix_Type;
     Primitive : in     Vertex_List;
     Period    : in     Positive;
     Low, High :    out Float;
     Storage   :    out System.Storage_Elements.Storage_Array
   ) with Pre => Primitive'Length = 1 and
     Storage'Length >= Required_Storage_Length (Matrix);
   --  Estimates the spectral radius of the Period'th power of Matrix,
   --  restricted to the given 1x1 primitive component.

   procedure Primitive_2x2_Unreduced (
     Matrix    : in     Transition_Matrix_Type;
     Primitive : in     Vertex_List;
     Period    : in     Positive;
     Low, High :    out Float;
     Storage   :    out System.Storage_Elements.Storage_Array
   ) with Pre => Primitive'Length = 2 and
     Storage'Length >= Required_Storage_Length (Matrix);
   --  Estimates the spectral radius of the Period'th power of Matrix,
   --  restricted to the given 2x2 primitive component.

end Transition_Matrices.Spectral_Radius_Helpers;
