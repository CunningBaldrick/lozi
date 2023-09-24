with Integers;

procedure Intersects_Halfspace (
  Polygon  : in     Polygons.Polygon_Type;
  Point    : in     Points.Point_Type;
  Forward  :    out Boolean;
  Backward :    out Boolean
) is
   use Integers;

   N : Natural renames Polygon.Number_Of_Vertices;
begin
   Forward  := False;
   Backward := False;

   if Is_Empty (Polygon) then
      return;
   end if;

   --  Since Polygon has non-empty interior, we must have N >= 3.
   pragma Assert (N >= 3);

   for I in 1 .. N loop
      declare
         Dot : Integer_Type renames
           Dot_Product (Point, Get_Vertex (I, Polygon));
      begin
         if not Forward and then Is_Positive (Dot) then
            Forward := True;
         end if;

         if not Backward and then Is_Negative (Dot) then
            Backward := True;
         end if;
      end;

      if Forward and Backward then
         return;
      end if;
   end loop;

   if Forward or Backward then
      return;
   end if;

   --  The polygon's boundary coincides with that of the halfspace.
   Forward := Is_Positive (
                Dot_Product (
                  Point,
                  Cross_Product (
                    Get_Vertex (1, Polygon),
                    Get_Vertex (2, Polygon)
                  )
                )
              );
   Backward := not Forward;
end Intersects_Halfspace;
