with Partition;
with Partition.Process_Transitions;
with Polygons;
with Symbolics;

function Upper_Transition_Matrix
  return Transition_Matrices.Transition_Matrix_Pointer is
   use Partition;
   use Polygons;
   use Symbolics;
   use Transition_Matrices;

   Matrix : constant Transition_Matrix_Pointer
     := new Transition_Matrix_Type (Element_Count);

   procedure Action (
     From_Polygon : Polygon_Type;
     From_Index   : Positive;
     From_Image   : Polygon_Type;
     To_Polygon   : Polygon_Type;
     To_Index     : Positive;
     Total_Symbol : Sequence_Type
   );
   pragma Inline (Action);

   procedure Action (
     From_Polygon : Polygon_Type;
     From_Index   : Positive;
     From_Image   : Polygon_Type;
     To_Polygon   : Polygon_Type;
     To_Index     : Positive;
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
