with Points; use Points;
with Polygons; use Polygons;

procedure Intersects_Halfspace (
  Polygon  : in     Polygon_Type;
  Point    : in     Point_Type;
  Forward  :    out Boolean;
  Backward :    out Boolean
);
pragma Elaborate_Body (Intersects_Halfspace);
--  Determines whether a convex polygon intersects the forward open
--  halfspace consisting of those points whose dot product with Point
--  is strictly positive, and/or the backward open halfspace consisting
--  of those points whose dot product with Point is strictly negative.
--  Does not work for non-convex polygons.
