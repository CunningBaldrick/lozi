with Ada.Exceptions;
with Ada.Unchecked_Deallocation;

package body Arnoldi is
   type Vector_Pointer is access Vector;

   type Integer_Array is array (Fortran_Integer range <>) of Fortran_Integer;
   pragma Convention (Fortran, Integer_Array);

   type Logical_Array is array (Fortran_Integer range <>) of Logical;
   pragma Convention (Fortran, Logical_Array);

   type Matrix is array (Fortran_Integer range <>, Fortran_Integer range <>)
     of Double_Precision;
   pragma Convention (Fortran, Matrix);

   type Matrix_Pointer is access Matrix;

   --  Importing from C rather than Fortran works around a GNAT bug.
   procedure dnaupd (
     IDO       : access Fortran_Integer;
     BMAT      : in     Fortran_Character;
     N         : access Fortran_Integer;
     WHICH     : in     Fortran_Character;
     NEV       : access Fortran_Integer;
     TOL       : access Double_Precision;
     RESID     : in out Vector;
     NCV       : access Fortran_Integer;
     V         :    out Matrix;
     LDV       : access Fortran_Integer;
     IPARAM    : in out Integer_Array;
     IPNTR     :    out Integer_Array;
     WORKD     : in out Vector;
     WORKL     :    out Vector;
     LWORKL    : access Fortran_Integer;
     INFO      : access Fortran_Integer;
     BMAT_LEN  : in     Long_Integer;
     WHICH_LEN : in     Long_Integer
   );
   pragma Import (C, dnaupd, "dnaupd_");

   procedure dneupd (
     RVEC       : access Logical;
     HOWMNY     : in     Fortran_Character;
     SELEC      : in out Logical_Array;
     DR         :    out Vector;
     DI         :    out Vector;
     Z          :    out Matrix;
     LDZ        : access Fortran_Integer;
     SIGMAR     : access Double_Precision;
     SIGMAI     : access Double_Precision;
     WORKEV     :    out Vector;
     BMAT       : in     Fortran_Character;
     N          : access Fortran_Integer;
     WHICH      : in     Fortran_Character;
     NEV        : access Fortran_Integer;
     TOL        : access Double_Precision;
     RESID      : in out Vector;
     NCV        : access Fortran_Integer;
     V          :    out Matrix;
     LDV        : access Fortran_Integer;
     IPARAM     : in out Integer_Array;
     IPNTR      :    out Integer_Array;
     WORKD      : in out Vector;
     WORKL      :    out Vector;
     LWORKL     : access Fortran_Integer;
     INFO       : access Fortran_Integer;
     HOWMNY_LEN : in     Long_Integer;
     BMAT_LEN   : in     Long_Integer;
     WHICH_LEN  : in     Long_Integer
   );
   pragma Import (C, dneupd, "dneupd_");

   procedure Free is new Ada.Unchecked_Deallocation (
     Vector,
     Vector_Pointer
   );

   procedure Free is new Ada.Unchecked_Deallocation (
     Matrix,
     Matrix_Pointer
   );

   --------------------------
   -- Extremal_Eigenvector --
   --------------------------
   procedure Extremal_Eigenvector (
     First, Last    : in     Fortran_Integer;
     Real_Part      :    out Vector;
     Imaginary_Part :    out Vector;
     Eigenvalue_R_P :    out Double_Precision;
     Eigenvalue_I_P :    out Double_Precision;
     Tolerance      : in     Double_Precision
   ) is
      mode    : constant Fortran_Integer := 1; -- Standard eigenvalue problem.
      ido     : aliased  Fortran_Integer := 0;
      bmat    : constant Fortran_Character := To_Fortran ("I");
      n       : aliased  Fortran_Integer := Last - First + 1;
      which   : constant Fortran_Character := To_Fortran ("LM");
      nev     : aliased  Fortran_Integer := 1;
      tol     : aliased  Double_Precision := Tolerance;
      ncv     : aliased  Fortran_Integer
        := Fortran_Integer'Min (n, -- dnaupd maximim ncv
          Fortran_Integer'Max (nev + 2, -- dnaupd minimum ncv
            2 * nev + 1 -- our choice
        ));
      v       : Matrix_Pointer;
      ldv     : aliased  Fortran_Integer := n;
      maxiter : constant Fortran_Integer := 1000;
      iparam  : Integer_Array (1 .. 11);
      ipntr   : Integer_Array (1 .. 14);
      workd   : Vector_Pointer;
      lworkl  : aliased  Fortran_Integer := 3*ncv**2 + 6*ncv;
      workl   : Vector (1 .. lworkl);
      info    : aliased  Fortran_Integer := 0; -- Use random initial vector

      rvec    : aliased  Logical := True; -- Compute Ritz vectors
      howmny  : constant Fortran_Character := To_Fortran ("A");
      selec   : Logical_Array (1 .. ncv);
      dr      : Vector (1 .. nev + 1);
      di      : Vector (1 .. nev + 1);
      sigmar  : aliased  Double_Precision := 0.0;
      sigmai  : aliased  Double_Precision := 0.0;
      workev  : Vector (1 .. 3*ncv);
   begin
      if Real_Part'First > First or Imaginary_Part'First > First or
        Real_Part'Last < Last or Imaginary_Part'Last < Last then
         Ada.Exceptions.Raise_Exception (
           Arnoldi_Error'Identity, "mismatched vectors"
         );
      end if;

      if n < 3 then
         Ada.Exceptions.Raise_Exception (
           Arnoldi_Error'Identity, "problem too small (need 3 x 3 or bigger)"
         );
      else -- n >= 3
         v     := new Matrix (1 .. n, 1 .. ncv);
         workd := new Vector (1 .. 3*n);

         iparam (1) := 1; -- ishift
         iparam (3) := maxiter;
         iparam (7) := mode;

         loop
            dnaupd (ido'Access, bmat, n'Access, which, nev'Access, tol'Access,
              Real_Part, ncv'Access, v.all, ldv'Access, iparam, ipntr,
              workd.all, workl, lworkl'Access, info'Access, bmat'Length,
              which'Length
            );

            exit when ido /= -1 and ido /= 1;

            Iterate (
              Source => workd (ipntr (1) .. ipntr (1) + n - 1),
              Target => workd (ipntr (2) .. ipntr (2) + n - 1)
            );
         end loop;

         if info < 0 then
            Ada.Exceptions.Raise_Exception (
              Arnoldi_Error'Identity,
              "dnaupd error, info is " & Fortran_Integer'Image (info)
            );
         end if;

         pragma Warnings (Off, -- See dneupd documentation for Z and V.
           "writable actual for ""Z"" overlaps with actual for ""V""");
         dneupd (rvec'Access, howmny, selec, dr, di, v.all, ldv'Access,
           sigmar'Access, sigmai'Access, workev, bmat, n'Access, which,
           nev'Access, tol'Access, Real_Part, ncv'Access, v.all, ldv'Access,
           iparam, ipntr, workd.all, workl, lworkl'Access, info'Access,
           howmny'Length, bmat'Length, which'Length
         );

         if info /= 0 then
            Ada.Exceptions.Raise_Exception (
              Arnoldi_Error'Identity,
              "dneupd error, info is " & Fortran_Integer'Image (info)
            );
         end if;

         Free (workd);

         Eigenvalue_R_P := dr (1);
         Eigenvalue_I_P := di (1);

         declare
            Base : constant Fortran_Integer := First - 1;
         begin
            for I in 1 .. n loop
               Real_Part (Base + I) := v (I, 1);
            end loop;

            if Eigenvalue_I_P = 0.0 then
               --  Ritz value/vector is real
               for I in 1 .. n loop
                  Imaginary_Part (Base + I) := 0.0;
               end loop;
            else
               --  Ritz value/vector is complex
               for I in 1 .. n loop
                  Imaginary_Part (Base + I) := v (I, 2);
               end loop;
            end if;
         end;

         Free (v);
      end if;
   exception
      when others =>
         Free (workd);
         Free (v);
         raise;
   end Extremal_Eigenvector;
end Arnoldi;
