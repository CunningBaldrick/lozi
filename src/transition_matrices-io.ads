with Vertices;

package Transition_Matrices.IO is
   procedure Put_Line (Matrix : Transition_Matrix_Type);

   procedure Put_Line (
     Matrix   : Transition_Matrix_Type;
     Vertices : Standard.Vertices.Vertex_List
   );
end Transition_Matrices.IO;
