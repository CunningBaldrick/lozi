with Polygons; use Polygons;
with Symbolics; use Symbolics;

generic
   with procedure Action (
     From_Polygon : Polygon_Type;
     From_Index   : Positive;
     From_Image   : Polygon_Type;
     To_Polygon   : Polygon_Type;
     To_Index     : Positive;
     Total_Symbol : Sequence_Type
   );
procedure Partition.Process_Transitions;
pragma Elaborate_Body (Partition.Process_Transitions);
--  Action is called for every pair (From_Polygon, To_Polygon) of polygons
--  in the partition for which the image of From_Polygon intersects To_Polygon.
--  The image of From_Polygon is passed in as From_Image.  The positions of
--  the polygons in the partition are given in From_Index and To_Index.  The
--  symbolic sequence of From_Polygon is equal to Total_Symbol with the last
--  symbol dropped; the symbolic sequence of To_Polygon equals Total_Symbol
--  with the first element dropped.
