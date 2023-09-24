procedure Partition.Refine;
--  Iterate each partition element and refine it (into at most two pieces)
--  by intersecting it with existing partition elements.  These intersections
--  can also be obtained by intersecting with the left half-space (symbol 'L')
--  and the right half-space (symbol 'R').  Memory associated with the original
--  partition elements is freed.
pragma Elaborate_Body (Partition.Refine);
