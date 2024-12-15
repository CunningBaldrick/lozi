with Partition;

package body Trim_Support is

   use Transition_Matrices;
   use Vertices;

   procedure Null_Action (
     Matrix   : Transition_Matrix_Type;
     Vertices : Vertex_List
   ) is
      pragma Unreferenced (Matrix, Vertices);
   begin
      null;
   end Null_Action;

   procedure Delete_Wandering (
     Matrix   : Transition_Matrix_Type;
     Vertices : Vertex_List
   ) is
      pragma Unreferenced (Matrix);
   begin
      Partition.Delete_Elements (Vertices);
   end Delete_Wandering;

end Trim_Support;
