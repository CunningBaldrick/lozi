with Multi_precision_integers; use Multi_precision_integers;

package body Gautier_Support is
   function Multi (small: Basic_int; n: Index_int) return Multi_int is
      Result : Multi_int (n);
   begin
      Fill (Result, Multi (small));
      return Result;
   end Multi;
end Gautier_Support;
