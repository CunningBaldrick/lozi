with Ada.Text_IO; use Ada.Text_IO;
with Integers; use Integers;
with Integers.IO; use Integers.IO;
with Linear_Algebra;

procedure Row_Test is
   package ML is new Linear_Algebra (4, 4);
   use ML;

   Two   : constant Integer_Type := One + One;
   Three : constant Integer_Type := Two + One;
   Four  : constant Integer_Type := Two + Two;

   Matrix : Matrix_Type;

   Rows : Row_Permutation;
   Cols : Col_Permutation;

   Rank : Natural;
begin
   Matrix (1, 1) := One;
   Matrix (1, 2) := Zero;
   Matrix (1, 3) := Three;
   Matrix (1, 4) := Zero;

   Matrix (2, 1) := Four;
   Matrix (2, 2) := Three;
   Matrix (2, 3) := Zero;
   Matrix (2, 4) := Two;

   Matrix (3, 1) := Three;
   Matrix (3, 2) := Four;
   Matrix (3, 3) := Two;
   Matrix (3, 4) := Two;

   Matrix (4, 1) := Three;
   Matrix (4, 2) := One;
   Matrix (4, 3) := Two;
   Matrix (4, 4) := Four;

   Row_Reduce (Matrix, Rows, Cols, Rank, True);

   for I in Row_Index loop
      for J in Col_Index loop
         Put (Matrix (Rows (I), Cols (J)));
         Put ("  ");
      end loop;
      New_Line;
   end loop;

   New_Line;

   for I in Row_Index loop
      Put (Row_Index'Image (I) & " =>" & Row_Index'Image (Rows (I)) & ',');
   end loop;

   New_Line (2);

   for J in Col_Index loop
      Put (Col_Index'Image (J) & " =>" & Col_Index'Image (Cols (J)) & ',');
   end loop;

   New_Line (2);

   Put_Line ("Rank:" & Natural'Image (Rank));
end Row_Test;
