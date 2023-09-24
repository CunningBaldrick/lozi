with Interfaces.Fortran; use Interfaces.Fortran;

--  Calculate an extremal eigenvalue/eigenvector pair for a sparse matrix
--  using implicitly restarted Arnoldi iteration.  The matrix must be at
--  least 3x3.
package Arnoldi is
   pragma Elaborate_Body;

   Arnoldi_Error : exception;

   type Vector is array (Fortran_Integer range <>) of Double_Precision;
   pragma Convention (Fortran, Vector);

   generic
      with procedure Iterate (
        Source : in     Vector;
        Target :    out Vector
      );
      --  Apply the matrix to Source and store the result in Target.
      --  The values of Source'First and Target'First may vary on each call.
   procedure Extremal_Eigenvector (
     First, Last    : in     Fortran_Integer; -- vector index range
     Real_Part      :    out Vector;
     Imaginary_Part :    out Vector;
     Eigenvalue_R_P :    out Double_Precision;
     Eigenvalue_I_P :    out Double_Precision;
     Tolerance      : in     Double_Precision
   );
end Arnoldi;
