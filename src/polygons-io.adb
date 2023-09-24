with Ada.Integer_Text_IO;
with Ada.Text_IO;
with Points.IO;

package body Polygons.IO is
   procedure Put_Line (Source : Polygon_Type) is
   begin
      Ada.Text_IO.Put ("Number of vertices: ");
      Ada.Integer_Text_IO.Put (Source.Number_Of_Vertices, Width => 0);
      Ada.Text_IO.New_Line;
      for I in 1 .. Source.Number_Of_Vertices loop
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put ("Vertex ");
         Ada.Integer_Text_IO.Put (I, Width => 0);
         Ada.Text_IO.Put_Line (":");
         Points.IO.Put_Line (Get_Vertex (I, Source));
      end loop;
   end Put_Line;
end Polygons.IO;
