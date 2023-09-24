with Ada.Finalization;
with Transition_Matrix_Rows;

package Transition_Matrices is
   pragma Elaborate_Body;

   type Transition_Matrix_Type (Size : Natural) is limited private;

   type Transition_Matrix_Pointer is access Transition_Matrix_Type;

   procedure Clear_Transition (
     From   : in     Positive;
     To     : in     Positive;
     Matrix : in out Transition_Matrix_Type
   );
   pragma Inline (Clear_Transition);

   procedure Set_Transition (
     From   : in     Positive;
     To     : in     Positive;
     Matrix : in out Transition_Matrix_Type
   );
   pragma Inline (Set_Transition);

   function Transition_Exists (
     From   : Positive;
     To     : Positive;
     Matrix : Transition_Matrix_Type
   ) return Boolean;
   pragma Inline (Transition_Exists);

   type Vertex_List is array (Natural range <>) of Positive;

private

   type Row_List is array (Positive range <>) of Transition_Matrix_Rows.Matrix_Row;

   type Transition_Matrix_Type (Size : Natural) is new Ada.Finalization.Limited_Controlled with record
      Rows : Row_List (1 .. Size);
   end record;

   procedure Finalize (Matrix : in out Transition_Matrix_Type);

end Transition_Matrices;
