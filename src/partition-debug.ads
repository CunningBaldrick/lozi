package Partition.Debug is
   function Is_Partition (Strict : Boolean) return Boolean;
   --  Checks that we really have a partition of Z >= 0 into
   --  convex polygons.  Checks that the partition consists
   --  of convex polygons.  If Strict = True, all vertices
   --  must be extreme points.  Checks that each polygon has
   --  Z coordinate >= 0.  Checks that each edge belongs to
   --  exactly two polygons, and that the polygons are disjoint,
   --  except for edges entirely contained in the circle Z = 0.
   --  Checks that no partition element crosses X = 0.
end Partition.Debug;
