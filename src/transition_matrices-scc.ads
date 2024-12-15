with Vertices;

generic
  with procedure SCC_Action (
    Matrix   : Transition_Matrix_Type;
    Vertices : Standard.Vertices.Vertex_List
  );

  with procedure Wander_Action (
    Matrix   : Transition_Matrix_Type;
    Vertices : Standard.Vertices.Vertex_List
  );
procedure Transition_Matrices.SCC (Matrix : Transition_Matrix_Type);
pragma Elaborate_Body (Transition_Matrices.SCC);
--  Decomposes the transition matrix into Strongly Connected Components (SCCs).
--  A vertex is in an SCC if and only if there is a path from that vertex to
--  itself.  Two vertices are in the same SCC if and only if there is a path
--  from the first to the second, and also a path from the second to the first.
--  A vertex not in any SCC is called a wandering vertex.  SCCs are also known
--  as irreducible components.  SCC_Action is called once for each SCC, with
--  Vertices containing the indices of the vertices in the SCC, sorted from
--  smallest to largest.  The value of Vertices'First may vary on each call.
--  Once SCC_Action has been called for each SCC, Wander_Action is called with
--  Vertices containing the indices of all wandering vertices, sorted from
--  smallest to largest.  No value is guaranteed for Vertices'First.
