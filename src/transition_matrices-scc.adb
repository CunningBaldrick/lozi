with Ada.Unchecked_Deallocation;
with GNAT.Heap_Sort_G;
with Transition_Matrix_Rows;

--  Underlying algorithm by D. J. Pearce, An Improved Algorithm for Finding
--  the Strongly Connected Components of a Directed Graph, Victoria University
--  Technical Report, 2005.
--  Non-recursive implementation inspired by BTF_STRONGCOMP by Tim Davis,
--  University of Florida, with support from Sandia National Laboratories.

procedure Transition_Matrices.SCC (Matrix : Transition_Matrix_Type) is
   use Transition_Matrix_Rows;
   use Vertices;

   N : constant Natural := Natural (Matrix.Last_Row); -- |V|

   subtype Vertices is Vertex_Number range 1 .. Matrix.Last_Row; -- V

   type List is array (Vertices) of Natural;
   type List_Pointer is access List;

   type Vertex_List_Pointer is access Vertex_List;

   type Activation_Record is record
      Vertex   : Vertices;
      Out_Edge : Cursor;
      Root     : Boolean;--QQ eliminate
   end record;

   type Extended_Stack_Index_Type is new Integer range 0 .. N;
   subtype Stack_Index_Type is Extended_Stack_Index_Type
     range 1 .. Extended_Stack_Index_Type'Last;
   type Call_Stack_Type is array (Stack_Index_Type) of Activation_Record;
   type Call_Stack_Access is access Call_Stack_Type;

   Vertex_Stack : Vertex_List_Pointer;

   Call_Stack : Call_Stack_Access;
   Call_Stack_Pointer : Extended_Stack_Index_Type;

   rindex : List_Pointer;
   index  : Positive;
   S      : Natural;

   procedure Free is new Ada.Unchecked_Deallocation (
     Call_Stack_Type,
     Call_Stack_Access
   );

   procedure Free is new Ada.Unchecked_Deallocation (
     List,
     List_Pointer
   );

   procedure Free is new Ada.Unchecked_Deallocation (
     Vertex_List,
     Vertex_List_Pointer
   );

   function Has_Self_Edge (index : Vertex_Number) return Boolean;
   pragma Inline (Has_Self_Edge);

   function Has_Self_Edge (index : Vertex_Number) return Boolean is
      Found : Boolean := False;

      procedure Compare_Column (
        Column : in     Vertex_Number;
        Stop   : in out Boolean
      ) is
      begin
         if Column = index then
            Found := True;
            Stop  := True;
         end if;
      end Compare_Column;

      procedure Search_For_Column is new Visit_Non_Zero_Columns (Compare_Column);
   begin
      Search_For_Column (Matrix.Rows (index));

      return Found;
   end Has_Self_Edge;


   procedure Sort_Vertex_Stack (I, J : Positive);
   --  Sort positions I .. J in Vertex_Stack

   procedure Sort_Vertex_Stack (I, J : Positive) is
      Base : constant Natural := I - 1;
      Temp : Vertex_Number;

      procedure Move (From, To : Natural);
      pragma Inline (Move);

      procedure Move (From, To : Natural) is
      begin
         Vertex_Stack (Base + To) := Vertex_Stack (Base + From);
      end Move;

      function Lt (Op1, Op2 : Natural) return Boolean;
      pragma Inline (Lt);

      function Lt (Op1, Op2 : Natural) return Boolean is
      begin
         return Vertex_Stack (Base + Op1) < Vertex_Stack (Base + Op2);
      end Lt;

      package Heap_Sort is new GNAT.Heap_Sort_G (
        Move => Move,
        Lt   => Lt
      );
   begin
      if J <= I then
         return;
      end if;

      --  Vertex_Stack (Base) used as temporary storage
      Temp := Vertex_Stack (Base);
      Heap_Sort.Sort (J - Base);
      Vertex_Stack (Base) := Temp;
   end Sort_Vertex_Stack;


   ----------
   -- push --
   ----------

   procedure push (
     v : in     Vertex_Number;
     S : in out Natural
   );

   procedure push (
     v : in     Vertex_Number;
     S : in out Natural
   ) is
   begin
      S := S + 1;
      Vertex_Stack (S) := v;
   end push;


   ----------------------
   -- Call_Stack_Empty --
   ----------------------

   function Call_Stack_Empty return Boolean;
   pragma Inline (Call_Stack_Empty);

   function Call_Stack_Empty return Boolean is
   begin
      return Call_Stack_Pointer = 0;
   end Call_Stack_Empty;


   ---------------
   -- Unvisited --
   ---------------

   function Unvisited (V : Vertices) return Boolean;
   pragma Inline (Unvisited);

   function Unvisited (V : Vertices) return Boolean is
   begin
      return rindex (V) = 0;
   end Unvisited;


   -----------------
   -- Push_Vertex --
   -----------------

   procedure Push_Vertex (V : Vertices);
   procedure Push_Vertex (V : Vertices) is
   begin
      pragma Assert (Unvisited (V));
      Call_Stack_Pointer := Call_Stack_Pointer + 1;
      Call_Stack (Call_Stack_Pointer).Vertex := V;
   end Push_Vertex;


   -----------
   -- visit --
   -----------

   procedure visit (v : Vertices) is
      I : Natural;
   begin
      Push_Vertex (v);

      loop
      <<Recurse>>
         declare
            Vertex   : constant Vertices := Call_Stack (Call_Stack_Pointer).Vertex;
            Row      : Matrix_Row renames Matrix.Rows (Vertex);
            Root     : Boolean renames Call_Stack (Call_Stack_Pointer).Root;
            Out_Edge : Cursor;
            Target   : Vertices;
         begin
            if Unvisited (Vertex) then
               --  The vertex is being visited for the first time - perform prework.

               --  Line 6
               Root := True;

               --  Line 7
               rindex (Vertex) := index;
               index := index + 1;

               --  Start at the first edge coming out of the vertex.
               Call_Stack (Call_Stack_Pointer).Out_Edge := Get_First (Row);
            end if;

            Out_Edge := Call_Stack (Call_Stack_Pointer).Out_Edge;
            while Has_Column (Out_Edge) loop
               Target := Get_Column (Row, Out_Edge);

               --  Line 9
               if Unvisited (Target) then
                  --  The vertex Target has not been visited.
                  --  Start a new depth-first starting from Target.

                  --  Remember which edge we are at.
                  Call_Stack (Call_Stack_Pointer).Out_Edge := Out_Edge;
                  Push_Vertex (Target);
                  goto Recurse;

               --  Line 10
               elsif rindex (Target) < rindex (Vertex) then
                  rindex (Vertex) := rindex (Target);
                  Root := False;
               end if;

               Out_Edge := Get_Next (Row, Out_Edge);
            end loop;

            --  All nodes reachable from Vertex have been visited.

            --  Line 11
            if root then

               --  Line 12
               index := index - 1;

               --  Lines 13 - 16
               I := S;
               while I >= 1 and then rindex (Vertex) <= rindex (Vertex_Stack (I)) loop
                  declare
                     w : Vertex_Number;
                  begin
                     --  Line 14
                     w := Vertex_Stack (I); -- w in SCC with Vertex

                     --  Line 15
                     rindex (w) := N - 1;
                  end;

                  --  Line 16
                  index := index - 1;

                  I := I - 1;
               end loop;

               --  SCC is Vertex and Vertex_Stack (I + 1 .. S)
               if I < S then
                  Vertex_Stack (S + 1) := Vertex;
                  Sort_Vertex_Stack (I + 1, S + 1);
                  SCC_Action (Matrix, Vertex_Stack (I + 1 .. S + 1));

                  --  Line 17, indicate SCC
                  rindex (Vertex) := N - 1;
               else -- only one vertex, and it is Vertex
                  if Has_Self_Edge (Vertex) then
                     SCC_Action (Matrix, (1 => Vertex));

                     --  Line 17, indicate SCC
                     rindex (Vertex) := N - 1;
                  else -- wandering vertex

                     --  Line 17, indicate wandering vertex
                     rindex (Vertex) := N;
                  end if;
               end if;
               S := I; -- pop the SCC off the Vertex_Stack

            --  Line 19
            else
               --  Line 20
               push (Vertex, S);
            end if;
         end;

         Call_Stack_Pointer := Call_Stack_Pointer - 1;
         exit when Call_Stack_Empty;
      end loop;
   end visit;

begin
   if N = 0 then
      return;
   end if;

   Vertex_Stack := new Vertex_List (0 .. N); -- Position 0 used as temporary storage
   Call_Stack   := new Call_Stack_Type;
   Call_Stack_Pointer := 0;

   rindex := new List;

   --  Line 1
   for v in Vertices loop
      rindex (v) := 0;
   end loop;

   --  Line 2
   S     := 0;
   index := 1;

   --  Line 3
   for v in Vertices loop
      if Unvisited (v) then
         visit (v);
      end if;
   end loop;

   --  Process wandering vertices
   S := 0;
   for v in Vertices loop
      if rindex (v) = N then
         push (v, S);
      end if;
   end loop;
   if S /= 0 then
      Sort_Vertex_Stack (1, S);
      Wander_Action (Matrix, Vertex_Stack (1 .. S));
   end if;

   Free (rindex);
   Free (Call_Stack);
   Free (Vertex_Stack);
exception
   when others =>
      Free (rindex);
      Free (Call_Stack);
      Free (Vertex_Stack);
      raise;
end Transition_Matrices.SCC;
