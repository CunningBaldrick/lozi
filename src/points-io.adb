with Ada.Text_IO;
with Integers.IO;

package body Points.IO is
   procedure Put_Line (Source : Point_Type) is
   begin
      Ada.Text_IO.Put ("X: ");
      Integers.IO.Put (Source (x));
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put ("Y: ");
      Integers.IO.Put (Source (y));
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put ("Z: ");
      Integers.IO.Put (Source (z));
      Ada.Text_IO.New_Line;
   end Put_Line;
end Points.IO;
