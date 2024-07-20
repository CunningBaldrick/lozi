with GCD;
with Transition_Matrix_Rows;

procedure Transition_Matrices.Primitive (
  Matrix   : Transition_Matrix_Type;
  Vertices : Vertex_List
) is
   use Transition_Matrix_Rows;

   Period : Natural := 0; -- 0 indicates "not set".
   --  Largest integer that divides the length of every loop in the subgraph.

   procedure Update_Period (Jump : Integer);
   --  If Period already has a value then replace it with gcd(Period, Jump).
   --  Otherwise use the absolute value of Jump for Period.

   type Node_Index is new Positive range Vertices'Range;
   --  "Vertex" refers to rows/columns of the original matrix/graph.
   --  "Node" refers to rows/columns of the submatrix/subgraph given
   --  by Vertices.

   function Vertex (Node : Node_Index) return Positive;
   --  Returning the original matrix vertex corresponding to this node of the
   --  submatrix.

   generic
      with procedure Action (
        Child : in     Node_Index;
        Stop  : in out Boolean
      );
   procedure Visit_Children (Node : Node_Index);

   -------------------
   -- Update_Period --
   -------------------

   procedure Update_Period (Jump : Integer) is
   begin
      Period := GCD (Period, Jump);
   end Update_Period;

   function Vertex (Node : Node_Index) return Positive is
   begin
      return Vertices (Integer (Node));
   end Vertex;

   --------------------
   -- Visit_Children --
   --------------------

   procedure Visit_Children (Node : Node_Index) is
      Last : Natural := 0;
      --  Last value of Column passed to Dispatch.  Used for sanity checking.

      Next : Node_Index'Base := Node_Index'First;
      --  Previous matched column was smaller than Vertex (Next).

      procedure Dispatch (
        Column : in     Positive;
        Stop   : in out Boolean
      ) is
         procedure Increment_Next is
         begin
            Next := Next + 1;
            if Next > Node_Index'Last then
               Stop := True; -- Neither this nor later columns in submatrix.
            end if;
         end Increment_Next;
      begin
         pragma Assert (Column > Last);
         Last := Column;
         --  Sanity check that the column number only increases.

         pragma Assert (not Stop);
         loop
            exit when Column < Vertex (Next); -- Not in submatrix, try next col.
            if Column = Vertex (Next) then
               Action (Next, Stop);
               Increment_Next;
               exit; -- Move on to next column.
            end if;
            Increment_Next; -- Neither this nor later columns in submatrix.
            exit when Stop;
         end loop;
      end Dispatch;

      procedure Do_Visit is new Visit_Non_Zero_Columns (Dispatch);
   begin
      Do_Visit (Matrix.Rows (Vertex (Node)));
   end Visit_Children;

   Depths : array (Node_Index) of Integer := (Node_Index => -1);

   Current_Depth : Natural;

   procedure Visit (V : Node_Index) is

      procedure Visit_Child (
        Child : in     Node_Index;
        Stop  : in out Boolean
      ) is
      begin
         Current_Depth := Current_Depth + 1;
         Visit (Child);
         Current_Depth := Current_Depth - 1;
         Stop := Period = 1; -- Early exit if Period at smallest possible.
      end Visit_Child;

      procedure Do_Children is new Visit_Children (Visit_Child);
   begin
      if Depths (V) >= 0 then
         Update_Period (Current_Depth - Depths (V));
         return;
      end if;

      Depths (V) := Current_Depth;
      Do_Children (V);
   end Visit;

begin
   if Vertices'Length = 0 then
      Primitive_Action (Matrix, Vertices, 1);
      return;
   end if;

   pragma Assert ((for all I in Vertices'Range => Vertices (I) <= Matrix.Size),
     "Vertex outside of matrix");
   pragma Assert ((for all I in Vertices'First .. Vertices'Last -1 =>
     Vertices (I) < Vertices (I + 1)), "Vertices not ordered");

   Current_Depth := 0;
   Visit (Node_Index'First);

   pragma Assert (Period > 0, "Not a SCC");

   if Period = 1 then
      Primitive_Action (Matrix, Vertices, 1);
      return;
   end if;

   --  Two vertices are in the same primitive block iff their depth differs
   --  by a multiple of Period.  Find the primitive block with the smallest
   --  number of vertices and pass that to Primitive_Action.
   declare
      Counts : array (0 .. Period - 1) of Natural := (others => 0);

      Group : Natural;

      Min_Count : Natural;
      Min_Group : Natural;
   begin
      for D of Depths loop
         pragma Assert (D >= 0, "Not all vertices visited");
         Group := D rem Period;
         Counts (Group) := Counts (Group) + 1;
      end loop;

      Min_Count := Natural'Last;
      for Group in Counts'Range loop
         if Counts (Group) < Min_Count then
            Min_Count := Counts (Group);
            Min_Group := Group;
         end if;
      end loop;
      pragma Assert (Min_Count > 0, "Wrong Period");

      declare
         Min_Vertices : Vertex_List (1 .. Min_Count);
         Target : Natural := 0;
      begin
         for I in Node_Index loop
            if Depths (I) rem Period = Min_Group then
               Target := Target + 1;
               Min_Vertices (Target) := Vertex (I);
            end if;
         end loop;
         pragma Assert (Target = Min_Count);
         Primitive_Action (Matrix, Min_Vertices, Period);
      end;
   end;
end Transition_Matrices.Primitive;
