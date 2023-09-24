with Partition; use Partition;
with Transition_Matrices; use Transition_Matrices;

package Trim_Support is

   pragma Elaborate_Body (Trim_Support);

   procedure Null_Action (
     Matrix   : Transition_Matrix_Type;
     Vertices : Index_List
   );
   pragma Inline (Null_Action);

   procedure Delete_Wandering (
     Matrix   : Transition_Matrix_Type;
     Vertices : Partition.Index_List
   );
   pragma Inline (Delete_Wandering);

end Trim_Support;
