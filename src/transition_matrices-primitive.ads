with Vertices;

generic
  with procedure Primitive_Action (
    Matrix   : Transition_Matrix_Type;
    Vertices : Standard.Vertices.Vertex_List;
    Period   : Positive
  );
procedure Transition_Matrices.Primitive (
  Matrix : Transition_Matrix_Type;
  SCC    : Standard.Vertices.Vertex_List -- Must be ordered.
);
pragma Elaborate_Body (Transition_Matrices.Primitive);
