package body Trim_Support is

   procedure Null_Action (
     Matrix   : Transition_Matrix_Type;
     Vertices : Index_List
   ) is
      pragma Unreferenced (Matrix, Vertices);
   begin
      null;
   end Null_Action;

   procedure Delete_Wandering (
     Matrix   : Transition_Matrix_Type;
     Vertices : Partition.Index_List
   ) is
      pragma Unreferenced (Matrix);
   begin
      Partition.Delete_Elements (Vertices);
   end Delete_Wandering;

end Trim_Support;
