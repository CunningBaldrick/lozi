package body Transition_Matrices is

   use Transition_Matrix_Rows;
   use Vertices;

   ----------------------
   -- Clear_Transition --
   ----------------------

   procedure Clear_Transition (
     From   : in     Vertex_Number;
     To     : in     Vertex_Number;
     Matrix : in out Transition_Matrix_Type
   ) is
   begin
      Clear_Column (Matrix.Rows (From), To);
   end Clear_Transition;


   --------------------
   -- Set_Transition --
   --------------------

   procedure Set_Transition (
     From   : in     Vertex_Number;
     To     : in     Vertex_Number;
     Matrix : in out Transition_Matrix_Type
   ) is
   begin
      Set_Column (Matrix.Rows (From), To);
   end Set_Transition;


   -----------------------
   -- Transition_Exists --
   -----------------------

   function Transition_Exists (
     From   : Vertex_Number;
     To     : Vertex_Number;
     Matrix : Transition_Matrix_Type
   ) return Boolean is
   begin
      return Column_Is_Set (Matrix.Rows (From), To);
   end Transition_Exists;


   --------------
   -- Finalize --
   --------------

   procedure Finalize (Matrix : in out Transition_Matrix_Type) is
   begin
      for Row in Matrix.Rows'Range loop
         Clear_All (Matrix.Rows (Row));
      end loop;
   end Finalize;

end Transition_Matrices;
