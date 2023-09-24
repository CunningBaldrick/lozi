with Points;

package Polygons is
   --  Polygons in the sphere.

   pragma Elaborate_Body;

   type Polygon_Type (Number_Of_Vertices : Natural) is private;
   --  The boundary of the polygon is given by pieces of geodesic.
   --  The boundary pieces are the shortest geodesics from one vertex
   --  to the next.  The polygon is considered to be the open set to the
   --  left of the boundary.  Successive points should be distinct.
   --  Successive points should not be opposing points of the sphere
   --  (since there is then no well-defined shortest geodesic between them).
   --  The polygon should either be the Empty_Polygon or have non-empty
   --  interior.

   Empty_Polygon : constant Polygon_Type;

   function "=" (Left, Right : Polygon_Type) return Boolean is abstract;
   --  Equality is NOT implemented.

   type Point_Array_Type is array (Positive range <>) of Points.Point_Type;

   function Create (Vertex_List : Point_Array_Type) return Polygon_Type;
   pragma Inline (Create);
   --  Make a polygon from a list of vertices.  There is an edge from each
   --  vertex to the next, and from the last to the first.

   function Get_Vertex (
     Position : Integer;
     Polygon  : Polygon_Type
   ) return Points.Point_Type;
   pragma Inline (Get_Vertex);
   --  Wraps around if Position > Number_Of_Vertices.

   function Is_Empty (Polygon : Polygon_Type) return Boolean;
   pragma Inline (Is_Empty);

   function Is_Polygon (
     Strict  : Boolean;
     Polygon : Polygon_Type
   ) return Boolean;
   --  Checks that Polygon is a convex polygon with interior.  If
   --  Strict = True then checks that all vertices are extreme points.

   procedure Reverse_Orientation (Polygon : in out Polygon_Type);
   --  Returns the polygon that is the complement of the previous one.
   --  The new and old polygons have the property that the vertex at
   --  position M for the old polygon is the vertex at position -M for
   --  the new polygon.

   procedure Set_Vertex (
     Position : in     Integer;
     Value    : in     Points.Point_Type;
     Polygon  : in out Polygon_Type
   );
   pragma Inline (Set_Vertex);
   --  Wraps around if Position > Number_Of_Vertices.
private
   pragma Pack (Point_Array_Type);

   type Polygon_Type (Number_Of_Vertices : Natural) is record
      Vertices : Point_Array_Type (1 .. Number_Of_Vertices);
   end record;
   pragma Pack (Polygon_Type);

   Empty_Point_Array : Point_Array_Type (1 .. 0);

   Empty_Polygon : constant Polygon_Type := (
     Number_Of_Vertices => 0,
     Vertices           => Empty_Point_Array
   );
end Polygons;
