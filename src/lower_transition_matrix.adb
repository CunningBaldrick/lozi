with Ada.Unchecked_Deallocation;
with Lozi;
with Partition;
with Partition.Process_Transitions;
with Points;
with Polygons;
with Segments_Intersect;
with Symbolics;
with Vertices;

function Lower_Transition_Matrix
  return Transition_Matrices.Transition_Matrix_Pointer is
   use Partition;
   use Points;
   use Polygons;
   use Symbolics;
   use Transition_Matrices;
   use Vertices;

   Polygon_Count : constant Extended_Vertex_Number
     := Extended_Vertex_Number (Element_Count);

   type Side_Type;

   type Side_Pointer is access Side_Type;

   type Side_Type is record
      First  : Positive;
      Second : Positive;
      Number : Vertex_Number;
      Next   : Side_Pointer;
   end record;

   type Side_Array is array (Vertex_Number range <>) of Side_Pointer;

   type Side_Array_Pointer is access Side_Array;

   Polygon_Sides : Side_Array_Pointer := new Side_Array (1 .. Polygon_Count);

   Last_Number : Extended_Vertex_Number := 0;

   function Get_Number (Polygon_Index : Vertex_Number;
     Side_1, Side_2 : Positive) return Vertex_Number;

   function Get_Number (Polygon_Index : Vertex_Number;
     Side_1, Side_2 : Positive) return Vertex_Number is
      Side : Side_Pointer := Polygon_Sides (Polygon_Index);
      First_Side : constant Positive := Positive'Min (Side_1, Side_2);
      Second_Side : constant Positive := Positive'Max (Side_1, Side_2);
   begin
      while Side /= null loop
         if Side.First = First_Side and Side.Second = Second_Side then
            return Side.Number;
         end if;
         Side := Side.Next;
      end loop;

      Last_Number := Last_Number + 1;
      Polygon_Sides (Polygon_Index) := new Side_Type'(
        First  => First_Side,
        Second => Second_Side,
        Number => Last_Number,
        Next   => Polygon_Sides (Polygon_Index)
      );
      return Last_Number;
   end Get_Number;

   type Edge_Type;

   type Edge_Pointer is access Edge_Type;

   type Edge_Type is record
      From : Vertex_Number;
      To   : Vertex_Number;
      Next : Edge_Pointer;
   end record;

   Edge_List : Edge_Pointer;

   procedure Add_Edge (
     From_Index : Vertex_Number;
     From_Side_1, From_Side_2 : Positive;
     To_Index : Vertex_Number;
     To_Side_1, To_Side_2 : Positive
   );
   pragma Inline (Add_Edge);

   procedure Add_Edge (
     From_Index : Vertex_Number;
     From_Side_1, From_Side_2 : Positive;
     To_Index : Vertex_Number;
     To_Side_1, To_Side_2 : Positive
   ) is
      From_Number : constant Vertex_Number :=
        Get_Number (From_Index, From_Side_1, From_Side_2);
      To_Number : constant Vertex_Number :=
        Get_Number (To_Index, To_Side_1, To_Side_2);
   begin
      Edge_List := new Edge_Type'(
        From => From_Number,
        To   => To_Number,
        Next => Edge_List
      );
   end Add_Edge;

   procedure Action (
     From_Polygon : Polygon_Type;
     From_Index   : Vertex_Number;
     From_Image   : Polygon_Type;
     To_Polygon   : Polygon_Type;
     To_Index     : Vertex_Number;
     Total_Symbol : Sequence_Type
   );

   procedure Action (
     From_Polygon : Polygon_Type;
     From_Index   : Vertex_Number;
     From_Image   : Polygon_Type;
     To_Polygon   : Polygon_Type;
     To_Index     : Vertex_Number;
     Total_Symbol : Sequence_Type
   ) is
      pragma Unreferenced (Total_Symbol);

      A, B, C, D, AP, BP, CP, DP : Point_Type;
      Orientation_Preserving : constant Boolean := Lozi.Preserves_Orientation;
   begin
      for K in 1 .. To_Polygon.Number_Of_Vertices - 2 loop
         AP := Get_Vertex (K, To_Polygon);
         BP := Get_Vertex (K + 1, To_Polygon);

         for L in K + 2 .. To_Polygon.Number_Of_Vertices loop
            exit when K = 1 and L = To_Polygon.Number_Of_Vertices;
            CP := Get_Vertex (L, To_Polygon);
            DP := Get_Vertex (L + 1, To_Polygon);

            Search_Source :
            for I in 1 .. From_Polygon.Number_Of_Vertices - 2 loop
               if Orientation_Preserving then
                  A := Get_Vertex (I, From_Image);
                  B := Get_Vertex (I + 1, From_Image);
               else
                  A := Get_Vertex (-I, From_Image);
                  B := Get_Vertex (-(I + 1), From_Image);
               end if;

               for J in I + 2 .. From_Polygon.Number_Of_Vertices loop
                  exit when I = 1 and J = From_Polygon.Number_Of_Vertices;
                  if Orientation_Preserving then
                     C := Get_Vertex (J, From_Image);
                     D := Get_Vertex (J + 1, From_Image);
                  else
                     C := Get_Vertex (-J, From_Image);
                     D := Get_Vertex (-(J + 1), From_Image);
                  end if;
                  if Segments_Intersect (A, B, BP, CP) and then
                       Segments_Intersect (A, B, DP, AP) and then
                         Segments_Intersect (C, D, BP, CP) and then
                           Segments_Intersect (C, D, DP, AP) then
                     Add_Edge (From_Index, I, J, To_Index, K, L);
                     --  For each From_Index, there is at most one transition
                     --  to a given (To_Index, K, L), thus the following jump.
                     exit Search_Source;
                  end if;
               end loop;

            end loop Search_Source;

         end loop;

      end loop;
   end Action;

   procedure Calculate_Transitions is new Process_Transitions (Action);

   Matrix : Transition_Matrix_Pointer;
begin
   Calculate_Transitions;

   declare
      procedure Free is new Ada.Unchecked_Deallocation (
        Side_Type,
        Side_Pointer
      );

      procedure Free is new Ada.Unchecked_Deallocation (
        Side_Array,
        Side_Array_Pointer
      );

      Side, Old_Side : Side_Pointer;
   begin
      for I in Polygon_Sides'Range loop
         Side := Polygon_Sides (I);
         while Side /= null loop
            Old_Side := Side;
            Side := Side.Next;
            Free (Old_Side);
         end loop;
      end loop;

      Free (Polygon_Sides);
   end;

   Matrix := new Transition_Matrix_Type (Last_Number);

   --  Calculate the matrix
   declare
      procedure Free is new Ada.Unchecked_Deallocation (
        Edge_Type,
        Edge_Pointer
      );

      Edge, Old_Edge : Edge_Pointer;
   begin
      Edge := Edge_List;
      while Edge /= null loop
         Set_Transition (
           From   => Edge.From,
           To     => Edge.To,
           Matrix => Matrix.all
         );
         Old_Edge := Edge;
         Edge := Edge.Next;
         Free (Old_Edge);
      end loop;
   end;

   return Matrix;
end Lower_Transition_Matrix;
