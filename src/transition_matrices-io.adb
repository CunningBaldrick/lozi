with Ada.Text_IO; use Ada.Text_IO;

package body Transition_Matrices.IO is
   use Vertices;

   --------------
   -- Put_Line --
   --------------
   procedure Put_Line (Matrix : Transition_Matrix_Type) is
   begin
      for Row in 1 .. Matrix.Last_Row loop
         for Column in 1 .. Matrix.Last_Row loop
            if Transition_Exists (
              From   => Row,
              To     => Column,
              Matrix => Matrix
            ) then
               Put (" 1");
            else
               Put (" 0");
            end if;
         end loop;
         Put ("  (");
   
         declare
            use Transition_Matrix_Rows;

            This_Row : Matrix_Row renames Matrix.Rows (Row);
            Position : Cursor := Get_First (This_Row);
            Need_Comma : Boolean := False;
         begin
            while Has_Column (Position) loop
               if Need_Comma then
                  Put (',');
                  Need_Comma := False;
               end if;
               Put (Vertex_Number'Image (Get_Column (This_Row, Position)));
               Position := Get_Next (This_Row, Position);
            end loop;
         end;
   
         Put_Line (")");
      end loop;
      New_Line;
   end Put_Line;

   procedure Put_Line (
     Matrix   : Transition_Matrix_Type;
     Vertices : Vertex_List
   ) is
   begin
      for Row in Vertices'Range loop
         for Column in Vertices'Range loop
            if Transition_Exists (
              From   => Vertices (Row),
              To     => Vertices (Column),
              Matrix => Matrix
            ) then
               Put (" 1");
            else
               Put (" 0");
            end if;
         end loop;
         New_Line;
      end loop;
      New_Line;
   end Put_Line;
end Transition_Matrices.IO;
