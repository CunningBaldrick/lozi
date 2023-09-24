with Points; use Points;

function Segments_Intersect (
  X1, Y1 : Point_Type;
  X2, Y2 : Point_Type
) return Boolean;
--  Returns True if and only if there exist non-negative numbers a, b, c, d
--  such that a*X1 + b*Y1 = c*X2 + d*Y2, one of a,b is non-zero and one of
--  c,d is non-zero.
