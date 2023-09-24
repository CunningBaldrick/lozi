package body Transition_Matrices is

   use Transition_Matrix_Rows;

   ----------------------
   -- Clear_Transition --
   ----------------------

   procedure Clear_Transition (
     From   : in     Positive;
     To     : in     Positive;
     Matrix : in out Transition_Matrix_Type
   ) is
   begin
      Clear_Column (Matrix.Rows (From), To);
   end Clear_Transition;


   --------------------
   -- Set_Transition --
   --------------------

   procedure Set_Transition (
     From   : in     Positive;
     To     : in     Positive;
     Matrix : in out Transition_Matrix_Type
   ) is
   begin
      Set_Column (Matrix.Rows (From), To);
   end Set_Transition;


   -----------------------
   -- Transition_Exists --
   -----------------------

   function Transition_Exists (
     From   : Positive;
     To     : Positive;
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
