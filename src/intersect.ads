with Points; use Points;
with Polygons; use Polygons;

function Intersect (
  Polygon : Polygon_Type;
  Point   : Point_Type
) return Polygon_Type;
pragma Elaborate_Body (Intersect);
--  Returns the intersection of a convex polygon with the open halfspace
--  consisting of those points whose dot product with Point is strictly
--  positive.  Does not work for non-convex polygons.
