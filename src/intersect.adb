with Integers;

function Intersect (
  Polygon : Polygon_Type;
  Point   : Point_Type
) return Polygon_Type is
   use Integers;

   function Interpolate (X, Y : Point_Type) return Point_Type;
   --  Find a point on the hyperplane perpendicular to Point on the
   --  geodesic between X and Y

   function Interpolate (X, Y : Point_Type) return Point_Type is
      Lambda_X : constant Integer_Type := Dot_Product (X, Point);
      Lambda_Y : constant Integer_Type := Dot_Product (Y, Point);
   begin
      if Is_Zero (Lambda_X) and Is_Zero (Lambda_Y) then
         return X; -- X and Y both in hyperplane.
      elsif Lambda_Y > Lambda_X then
         return Linear_Combination (Lambda_Y, X, Negate (Lambda_X), Y);
      else
         return Linear_Combination (Negate (Lambda_Y), X, Lambda_X, Y);
      end if;
   end Interpolate;

   function Is_Positive (X : Point_Type) return Boolean;
   pragma Inline (Is_Positive);

   function Is_Positive (X : Point_Type) return Boolean is
   begin
      return Is_Positive (Dot_Product (Point, X));
   end Is_Positive;

   First_Positive : Natural := 0;
   --  First positive vertex.

   First_Negative : Natural := 0;
   --  First non-positive vertex following First_Positive.

   N : Natural renames Polygon.Number_Of_Vertices;

   Num_Positive : Positive;
begin
   if Is_Empty (Polygon) then
      return Polygon;
   end if;

   --  Since Polygon has non-empty interior, we must have N >= 3.
   pragma Assert (N >= 3);

   --  Set up First_Positive and First_Negative.
   --  Avoid evaluating Dot_Products more than we have to.

   declare
      Boundary_Count : Natural := 0;
   begin
      for I in 1 .. N loop
         declare
            Dot : Integer_Type renames
              Dot_Product (Point, Get_Vertex (I, Polygon));
         begin
            if Is_Positive (Dot) then
               First_Positive := I;
               exit;
            elsif Is_Zero (Dot) then
               Boundary_Count := Boundary_Count + 1;
            end if;
         end;
      end loop;

      if First_Positive = 0 then -- No positive points.
         if Boundary_Count < N then
            --  Polygon does not intersect the halfspace.
            return Empty_Polygon;
         else -- The polygon's boundary coincides with that of the halfspace.
            if Is_Positive (
              Cross_Product (Get_Vertex (1, Polygon), Get_Vertex (2, Polygon))
            ) then
               return Polygon; -- Polygon and halfspace coincide.
            else
               return Empty_Polygon; -- Polygon is complement of the halfspace.
            end if;
         end if;
      end if;
   end;

   if First_Positive = 1 then
   --  Step backward from 1 to find first positive vertex.
      for I in reverse 2 .. N loop
         if not Is_Positive (Get_Vertex (I, Polygon)) then
            First_Positive := I + 1;
            exit;
         end if;
      end loop;

      if First_Positive = 1 then -- Polygon entirely contained in halfspace.
         return Polygon;
      else
         --  Find first non-positive vertex; avoid reevaluating Is_Positive
         --  at vertex number First_Positive - 1, which we know to be false.
         for I in 2 .. First_Positive - 2 loop
            if not Is_Positive (Get_Vertex (I, Polygon)) then
               First_Negative := I;
               exit;
            end if;
         end loop;

         if First_Negative = 0 then
            First_Negative := First_Positive - 1;
         end if;

         if First_Positive = N + 1 then
            First_Positive := 1;
         end if;
      end if;
   else -- First_Positive > 1.
      for I in First_Positive + 1 .. N loop
         if not Is_Positive (Get_Vertex (I, Polygon)) then
            First_Negative := I;
            exit;
         end if;
      end loop;

      if First_Negative = 0 then
         First_Negative := 1;
      end if;
   end if;

   Num_Positive := (First_Negative - First_Positive) mod N;

   declare
      procedure Copy_Positive (Target : in out Polygon_Type);
      pragma Inline (Copy_Positive);

      procedure Copy_Positive (Target : in out Polygon_Type) is
      begin
         for I in 1 .. Num_Positive loop
            Set_Vertex (
              I,
              Get_Vertex (First_Positive + I - 1, Polygon),
              Target
            );
         end loop;
      end Copy_Positive;

      New_First : constant Point_Type := Interpolate (
        Get_Vertex (First_Positive - 1, Polygon),
        Get_Vertex (First_Positive, Polygon)
      );
      New_Last : constant Point_Type := Interpolate (
        Get_Vertex (First_Negative - 1, Polygon),
        Get_Vertex (First_Negative, Polygon)
      );
   begin
      if Equivalent (New_First, New_Last) then
         declare
            Result : Polygon_Type (Num_Positive + 1);
         begin
            Copy_Positive (Result);

            Set_Vertex (Num_Positive + 1, New_Last, Result);

            return Result;
         end;
      elsif Equivalent (New_First, Negate (New_Last)) then
         --  Opposing points: need to subdivide.
         declare
            Result : Polygon_Type (Num_Positive + 3);
         begin
            Copy_Positive (Result);

            Set_Vertex (Num_Positive + 1, New_Last, Result);

            Set_Vertex (
              Num_Positive + 2,
              Cross_Product (New_First, Point),
              Result
            );

            Set_Vertex (Num_Positive + 3, New_First, Result);

            return Result;
         end;
      else
         declare
            Result : Polygon_Type (Num_Positive + 2);
         begin
            Copy_Positive (Result);

            Set_Vertex (Num_Positive + 1, New_Last, Result);
            Set_Vertex (Num_Positive + 2, New_First, Result);

            return Result;
         end;
      end if;
   end;
end Intersect;
