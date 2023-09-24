with Multi_precision_integers; use Multi_precision_integers;

pragma Elaborate_All (Multi_precision_integers);

package Gautier_Support is
   pragma Elaborate_Body;

   function Multi (small: Basic_int; n: Index_int) return Multi_int;
end Gautier_Support;
