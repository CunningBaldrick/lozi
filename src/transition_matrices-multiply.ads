with Vertices;

generic
   type Weight_Type is private; -- Might be integer or floating point.
   Zero : Weight_Type;
   type Weight_Vector is array (Vertices.Vertex_Number range <>) of Weight_Type;
   with function "+" (Left, Right : Weight_Type) return Weight_Type is <>;
procedure Transition_Matrices.Multiply (
  Input  : in     Weight_Vector;
  Output :    out Weight_Vector; -- Should be different to Source.
  Matrix : in     Transition_Matrix_Type
);
--  Computes Output = Matrix * Input.  Note that this is not the same thing
--  as transitioning the input weights along graph edges according to the
--  matrix: that corresponds to transpose(Matrix) * Input.
