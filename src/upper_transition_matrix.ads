with Transition_Matrices;

function Upper_Transition_Matrix
  return Transition_Matrices.Transition_Matrix_Pointer;
--  Returns a transition matrix with entropy at least as big as that of the
--  current partition.  The vertices are the polygons of the partition.
--  There is a transition from a polygon P to a polygon Q if and only if
--  the image of the interior of P intersects the interior of Q.
pragma Elaborate_Body (Upper_Transition_Matrix);
