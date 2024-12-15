with Partition;
with Partition.Process_Transitions;
with Polygons;
with Symbolics;
with Vertices;

function Upper_Transition_Matrix
  return Transition_Matrices.Transition_Matrix_Pointer is
   use Partition;
   use Polygons;
   use Symbolics;
   use Transition_Matrices;
   use Vertices;

   Matrix : constant Transition_Matrix_Pointer
     := new Transition_Matrix_Type (Last_Row =>
       Extended_Vertex_Number (Element_Count));

   procedure Action (
     From_Polygon : Polygon_Type;
     From_Index   : Vertex_Number;
     From_Image   : Polygon_Type;
     To_Polygon   : Polygon_Type;
     To_Index     : Vertex_Number;
     Total_Symbol : Sequence_Type
   );
   pragma Inline (Action);

   procedure Action (
     From_Polygon : Polygon_Type;
     From_Index   : Vertex_Number;
     From_Image   : Polygon_Type;
     To_Polygon   : Polygon_Type;
     To_Index     : Vertex_Number;
     Total_Symbol : Sequence_Type
   ) is
      pragma Unreferenced (From_Polygon, From_Image, To_Polygon, Total_Symbol);
   begin
      Set_Transition (
        From   => From_Index,
        To     => To_Index,
        Matrix => Matrix.all
      );
   end Action;

   procedure Calculate_Matrix is new Process_Transitions (Action);
begin
   Calculate_Matrix;

   return Matrix;
end Upper_Transition_Matrix;
