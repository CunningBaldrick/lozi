with Ada.Text_IO; use Ada.Text_IO;
with Polygons.IO; use Polygons.IO;
with Symbolics.IO; use Symbolics.IO;

package body Partition.IO is
   --------------
   -- Put_Line --
   --------------
   procedure Put_Line (Element : Element_Type) is
   begin
      Put ("Polygon: ");
      Put_Line (Get_Polygon (Element));
      New_Line;
      Put ("Symbol sequence: ");
      Put (Get_Symbols (Element));
      New_Line;
   end Put_Line;

   -------------------
   -- Put_Partition --
   -------------------
   procedure Put_Partition is
      Current_Element : Element_Pointer := Get_First;
   begin
      if Current_Element /= null then
         Put_Line (Current_Element.all);
         loop
            Current_Element := Current_Element.Next;
            exit when Current_Element = null;
            New_Line;
            Put_Line ("----");
            New_Line;
            Put_Line (Current_Element.all);
         end loop;
      end if;
   end Put_Partition;
end Partition.IO;
