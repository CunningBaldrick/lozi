package Transition_Matrices.Spectral_Radius is

   procedure Estimate (
     Matrix    : in     Transition_Matrix_Type;
     Low, High :    out Float
   );
   pragma Inline (Estimate);
   --  Estimate the spectral radius of the entire matrix.  It is guaranteed
   --  that Low <= Spectral-Radius <= High.

end Transition_Matrices.Spectral_Radius;
