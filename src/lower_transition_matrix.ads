with Transition_Matrices;

function Lower_Transition_Matrix
  return Transition_Matrices.Transition_Matrix_Pointer;
--  Returns a transition matrix with entropy less than or equal to that of
--  the current partition.  The vertices consist of pairs (P, {S1, S2})
--  where P is a polygon of the partition, and S1 and S2 are non-adjacent
--  sides of P.  There is a transition from (P, {S1, S2}) to (Q, {T1, T2})
--  if and only if the images of S1 and S2 both intersect both T1' and T2'
--  where T1' and T2' are the two sides of the convex hull of {T1, T2}
--  that are not T1 or T2.  The idea is that the image of the convex hull
--  of {S1, S2} crosses the convex hull of {T1, T2} transversally.
pragma Elaborate_Body (Lower_Transition_Matrix);
