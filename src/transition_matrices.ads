with Ada.Finalization;
with Transition_Matrix_Rows;
with Vertices;

package Transition_Matrices is
   pragma Elaborate_Body;

   type Transition_Matrix_Type (Last_Row : Vertices.Extended_Vertex_Number)
     is limited private;

   type Transition_Matrix_Pointer is access Transition_Matrix_Type;

   procedure Clear_Transition (
     From   : in     Vertices.Vertex_Number;
     To     : in     Vertices.Vertex_Number;
     Matrix : in out Transition_Matrix_Type
   );
   pragma Inline (Clear_Transition);

   procedure Set_Transition (
     From   : in     Vertices.Vertex_Number;
     To     : in     Vertices.Vertex_Number;
     Matrix : in out Transition_Matrix_Type
   );
   pragma Inline (Set_Transition);

   function Transition_Exists (
     From   : Vertices.Vertex_Number;
     To     : Vertices.Vertex_Number;
     Matrix : Transition_Matrix_Type
   ) return Boolean;
   pragma Inline (Transition_Exists);

private

   type Row_List is array (Vertices.Vertex_Number range <>)
     of Transition_Matrix_Rows.Matrix_Row;

   type Transition_Matrix_Type (Last_Row : Vertices.Extended_Vertex_Number)
     is new Ada.Finalization.Limited_Controlled with record
      Rows : Row_List (1 .. Last_Row);
   end record;

   procedure Finalize (Matrix : in out Transition_Matrix_Type);

end Transition_Matrices;
