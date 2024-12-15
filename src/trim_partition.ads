with Transition_Matrices.SCC;
with Trim_Support;

pragma Elaborate_All (Transition_Matrices.SCC);

procedure Trim_Partition is new Transition_Matrices.SCC (
  SCC_Action    => Trim_Support.Null_Action,
  Wander_Action => Trim_Support.Delete_Wandering
);
--  Remove wandering polygons.
