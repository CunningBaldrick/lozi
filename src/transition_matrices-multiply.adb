procedure Transition_Matrices.Multiply (
  Input  : in     Weight_Vector;
  Output :    out Weight_Vector;
  Matrix : in     Transition_Matrix_Type
) is
begin
   for I in Matrix.Rows'Range loop
      declare
         Sum : Weight_Type := Zero;

         procedure Accumulate (
           Column : in     Positive;
           Stop   : in out Boolean
         ) is
            pragma Unreferenced (Stop);
         begin
            Sum := Sum + Input (Column);
         end Accumulate;

         procedure Sum_Column_Contributions is new
           Transition_Matrix_Rows.Visit_Non_Zero_Columns (Accumulate);
      begin
         Sum_Column_Contributions (Matrix.Rows (I));
         Output (I) := Sum;
      end;
   end loop;
end Transition_Matrices.Multiply;
