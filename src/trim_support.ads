with Transition_Matrices;
with Vertices;

package Trim_Support is

   pragma Elaborate_Body (Trim_Support);

   procedure Null_Action (
     Matrix   : Transition_Matrices.Transition_Matrix_Type;
     Vertices : Standard.Vertices.Vertex_List
   );
   pragma Inline (Null_Action);

   procedure Delete_Wandering (
     Matrix   : Transition_Matrices.Transition_Matrix_Type;
     Vertices : Standard.Vertices.Vertex_List
   );
   pragma Inline (Delete_Wandering);

end Trim_Support;
