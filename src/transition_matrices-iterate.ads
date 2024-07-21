generic
   type Weight_Type is private; -- Might be integer or floating point.
   Zero : Weight_Type;
   type Weight_Vector is array (Positive range <>) of Weight_Type;
   with function "+" (Left, Right : Weight_Type) return Weight_Type is <>;
procedure Transition_Matrices.Iterate (
  Input  : in     Weight_Vector;
  Output :    out Weight_Vector; -- Should be different to Source.
  Matrix : in     Transition_Matrix_Type
);
--  Transitions the input weights along graph edges according to the matrix,
--  and stores the result in Output.  This corresponds to multiplying Matrix
--  by Input on the left, or Output = transpose(Matrix) * Input.
