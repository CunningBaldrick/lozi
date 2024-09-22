generic
  type Vertex_List is array (Natural range <>) of Positive;

  with procedure Primitive_Action (
    Matrix   : Transition_Matrix_Type;
    Vertices : Vertex_List;
    Period   : Positive
  );
procedure Transition_Matrices.Primitive (
  Matrix : Transition_Matrix_Type;
  SCC    : Vertex_List -- Must be ordered.
);
pragma Elaborate_Body (Transition_Matrices.Primitive);
