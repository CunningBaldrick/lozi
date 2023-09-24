with Ada.Numerics.Generic_Complex_Types;
with Interfaces.Fortran;

package Fortran_Complex_Types
  is new Ada.Numerics.Generic_Complex_Types (Interfaces.Fortran.Double_Precision);
