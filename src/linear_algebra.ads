with Integers; use Integers;

--  Linear algebra for integer matrices.
generic
   Num_Rows, Num_Cols : Positive;
package Linear_Algebra is
   pragma Elaborate_Body;

   subtype Row_Index is Positive range 1 .. Num_Rows;
   subtype Col_Index is Positive range 1 .. Num_Cols;
   subtype Rank_Range is Natural range 0 .. Positive'Min (Num_Rows, Num_Cols);

   type Matrix_Type is array (Row_Index, Col_Index) of Integer_Type;

   type Row_Permutation is array (Row_Index) of Row_Index;

   type Col_Permutation is array (Col_Index) of Col_Index;

   procedure Row_Reduce (
     Matrix : in out Matrix_Type;
     Rows   :    out Row_Permutation;
     Cols   :    out Col_Permutation;
     Rank   :    out Rank_Range;
     Strict : in     Boolean
   );
end Linear_Algebra;
