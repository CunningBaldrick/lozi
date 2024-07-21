procedure Transition_Matrices.Iterate (
  Input  : in     Weight_Vector;
  Output :    out Weight_Vector;
  Matrix : in     Transition_Matrix_Type
) is
begin
   for Weight of Output loop
      Weight := Zero;
   end loop;

   for I in Matrix.Rows'Range loop
      declare
         Weight : constant Weight_Type := Input (I);

         procedure Accumulate (
           Column : in     Positive;
           Stop   : in out Boolean
         ) is
            pragma Unreferenced (Stop);
         begin
            Output (Column) := Output (Column) + Weight;
         end Accumulate;

         procedure Accumulate_Row is new
           Transition_Matrix_Rows.Visit_Non_Zero_Columns (Accumulate);
      begin
         if Weight /= Zero then
            Accumulate_Row (Matrix.Rows (I));
         end if;
      end;
   end loop;
end Transition_Matrices.Iterate;
