package Transition_Matrices.Spectral_Radius is
   --  Return guaranteed bounds for the spectral radius (Low <= SR <= High).

   procedure Estimate (
     Matrix    : in     Transition_Matrix_Type;
     Low, High :    out Float
   );
   pragma Inline (Estimate);
   --  Estimate the spectral radius of the entire matrix.  Applies
   --  Component_Estimate to each strongly connected component (SCC).

   procedure Component_Estimate (
     Matrix    : in     Transition_Matrix_Type;
     Vertices  : in     Vertex_List;
     Low, High :    out Float
   );
   pragma Inline (Component_Estimate);
   --  Estimate the spectral radius of the submatrix with rows and columns in
   --  Vertices.  Uses an implicitly restarted Arnoldi method to find an
   --  approximate Perron-Frobenius eigenvector.
end Transition_Matrices.Spectral_Radius;
