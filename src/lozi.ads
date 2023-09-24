with Integers; use Integers;
with Points;   use Points;
with Polygons; use Polygons;

package Lozi is
   pragma Elaborate_Body;

   Lozi_Error : exception;

   Left_Halfspace  : constant Point_Type;
   Right_Halfspace : constant Point_Type;

   procedure Map (Point : in out Point_Type);

   procedure Map (Polygon : in out Polygon_Type);
   --  Warning: if the image of an edge is longer than or equal to half the
   --  sphere, then it should be subdivided.  This routine does NOT perform
   --  any such subdivision.  However, if all vertices are in the region
   --  z >= 0, as they are when calculating entropy, then subdivision is never
   --  needed.

   function Preserves_Orientation return Boolean;
   pragma Inline (Preserves_Orientation);

   procedure Set_Parameters (
     A_Numerator   : Integer_Type;
     A_Denominator : Integer_Type;
     B_Numerator   : Integer_Type;
     B_Denominator : Integer_Type
   );
   --  Raises Lozi_Error if both the numerator and denominator of A
   --  (respectively, B) are zero, or if both denominators are zero.
private
   Left_Halfspace  : constant Point_Type := Create (Negate (One), Zero, Zero);
   Right_Halfspace : constant Point_Type := Create (One,          Zero, Zero);
end Lozi;
