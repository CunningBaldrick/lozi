with Intersect;
with Lozi;
with Partition.Process_Transitions;
with Points;
with Polygons;
with Symbolics;

procedure Partition.Refine is
   use Lozi;
   use Points;
   use Polygons;
   use Symbolics;

   First_New, Last_New : Element_Pointer;

   function Get_Halfspace (Symbol : Symbol_Type) return Point_Type;
   pragma Inline (Get_Halfspace);

   function Get_Halfspace (Symbol : Symbol_Type) return Point_Type is
   begin
      if Symbol = L then
         return Left_Halfspace;
      else
         return Right_Halfspace;
      end if;
   end Get_Halfspace;

   procedure Action (
     From_Polygon : Polygon_Type;
     From_Index   : Positive;
     From_Image   : Polygon_Type;
     To_Polygon   : Polygon_Type;
     To_Index     : Positive;
     Total_Symbol : Sequence_Type
   );

   procedure Action (
     From_Polygon : Polygon_Type;
     From_Index   : Positive;
     From_Image   : Polygon_Type;
     To_Polygon   : Polygon_Type;
     To_Index     : Positive;
     Total_Symbol : Sequence_Type
   ) is
      pragma Unreferenced (From_Polygon, From_Index, To_Polygon, To_Index);

      Halfspace : Point_Type renames Get_Halfspace (
        Total_Symbol (Total_Symbol'Last)
      );
      Intersection : Polygon_Type renames Intersect (
        Polygon => From_Image,
        Point   => Halfspace
      );
      New_Element : constant Element_Pointer := new Element_Type'(
        Create_Element (Intersection, Total_Symbol)
      );
   begin
      pragma Assert (not Is_Empty (Intersection));

      if First_New = null then
         First_New := New_Element;
      end if;

      if Last_New /= null then
         Last_New.Next := New_Element;
      end if;

      Last_New := New_Element;
   end Action;

   procedure Calculate_Refinement is new Process_Transitions (Action);
begin
   Calculate_Refinement;

   Delete_All_Elements; -- the old elements

   Set_First (First_New);
end Partition.Refine;
