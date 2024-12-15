with Ada.Text_IO;
with Transition_Matrices.SCC;
with Vertices;

procedure Test_SCC is
   use Ada.Text_IO;
   use Vertices;

   Max_Size : constant := 4; -- test all possible graphs with this many nodes or less.
begin
   for Last_Row in Extended_Vertex_Number range 0 .. Max_Size loop
      Put_Line ("Testing" & Last_Row'Img & " node graphs");

      declare
         Matrix : array (1 .. Last_Row, 1 .. Last_Row) of Boolean
           := (others => (others => False));
      begin
      <<Test_Next>>
         --  Test the graph.
         declare
            use Transition_Matrices;

            Next_SCC_Index : Positive := 1;

            Nodes : array (1 .. Last_Row) of Natural := (others => 0);

            procedure SCC_Action (
              Matrix   : Transition_Matrix_Type;
              Vertices : Vertex_List
            ) is
               subtype Vertex_Index is Integer range Vertices'Range;

               Scheduled : array (Vertex_Index) of Boolean;
               Reachable : array (Vertex_Index) of Boolean;
               Work_List : array (1 .. Vertices'Length) of Vertex_Index;
               Next_Work : Natural := 0;

               Vertex : Vertex_Index;
            begin
               --  Check that SCC's are not empty.
               if Vertices'Length = 0 then
                  raise Program_Error;
               end if;

               for V in Vertex_Index loop
                  Nodes (Vertices (V)) := Next_SCC_Index;
               end loop;
               Next_SCC_Index := Next_SCC_Index + 1;

               --  Check that every node in the SCC is reachable from every other.
               Scheduled := (others => False);
               Reachable := (others => False);
               Next_Work := Next_Work + 1;
               Work_List (Next_Work) := Vertices'First;
               Scheduled (Vertices'First) := True;

               while Next_Work > 0 loop
                  Vertex := Work_List (Next_Work);
                  Next_Work := Next_Work - 1;
                  pragma Assert (Scheduled (Vertex));

                  declare
                     Row : constant Vertex_Number := Vertices (Vertex);
                     Column : Vertex_Number;
                  begin
                     for Target in Vertex_Index loop
                        Column := Vertices (Target);
                        if Transition_Exists (Row, Column, Matrix) then
                           Reachable (Target) := True;
                           if not Scheduled (Target) then
                              Next_Work := Next_Work + 1;
                              Work_List (Next_Work) := Target;
                              Scheduled (Target) := True;
                           end if;
                        end if;
                     end loop;
                     end;
               end loop;

               for I in Reachable'Range loop
                  if not Reachable (I) then
                     raise Program_Error;
                  end if;
               end loop;

               --  Check that every node in the SCC is backwards reachable from every other.
               Scheduled := (others => False);
               Reachable := (others => False);
               Next_Work := Next_Work + 1;
               Work_List (Next_Work) := Vertices'First;
               Scheduled (Vertices'First) := True;

               while Next_Work > 0 loop
                  Vertex := Work_List (Next_Work);
                  Next_Work := Next_Work - 1;
                  pragma Assert (Scheduled (Vertex));

                  declare
                     Row : constant Vertex_Number := Vertices (Vertex);
                     Column : Vertex_Number;
                  begin
                     for Target in Vertex_Index loop
                        Column := Vertices (Target);
                        --  Use transpose matrix for backwards reachability.
                        if Transition_Exists (Column, Row, Matrix) then
                           Reachable (Target) := True;
                           if not Scheduled (Target) then
                              Next_Work := Next_Work + 1;
                              Work_List (Next_Work) := Target;
                              Scheduled (Target) := True;
                           end if;
                        end if;
                     end loop;
                     end;
               end loop;

               for I in Reachable'Range loop
                  if not Reachable (I) then
                     raise Program_Error;
                  end if;
               end loop;
            end SCC_Action;

            procedure Wander_Action (
              Matrix   : Transition_Matrix_Type;
              Vertices : Vertex_List
            ) is
               subtype Vertex_Index is Vertex_Number range 1 .. Last_Row;

               Scheduled : array (Vertex_Index) of Boolean;
               Work_List : array (1 .. Last_Row) of Vertex_Index;
               Next_Work : Extended_Vertex_Number := 0;

               Vertex : Vertex_Index;
            begin
               for V in Vertices'Range loop
                  -- Treat each wandering node like its own SCC.
                  Nodes (Vertices (V)) := Next_SCC_Index;
                  Next_SCC_Index := Next_SCC_Index + 1;
               end loop;

               --  Check that wandering vertices are not reachable from themselves.
               for Initial in Vertices'Range loop
                  Scheduled := (others => False);
                  Next_Work := Next_Work + 1;
                  Work_List (Next_Work) := Vertices (Initial);
                  Scheduled (Vertices (Initial)) := True;

                  while Next_Work > 0 loop
                     Vertex := Work_List (Next_Work);
                     Next_Work := Next_Work - 1;
                     pragma Assert (Scheduled (Vertex));

                     declare
                        Row : constant Vertex_Number := Vertex;
                        Column : Vertex_Number;
                     begin
                        for Target in Vertex_Index loop
                           Column := Target;
                           if Transition_Exists (Row, Column, Matrix) then
                              if Column = Vertices (Initial) then
                                 raise Program_Error;
                              end if;
                              if not Scheduled (Target) then
                                 Next_Work := Next_Work + 1;
                                 Work_List (Next_Work) := Target;
                                 Scheduled (Target) := True;
                              end if;
                           end if;
                        end loop;
                        end;
                  end loop;
               end loop;
            end Wander_Action;

            procedure Calculate_SCC is new SCC
              (SCC_Action, Wander_Action);

            TM : Transition_Matrix_Type (Last_Row);
         begin
            for Row in Matrix'Range (1) loop
               for Column in Matrix'Range (2) loop
                  if Matrix (Row, Column) then
                     Set_Transition (Row, Column, TM);
                  end if;
               end loop;
            end loop;

            -- Sanity check
            for Row in Matrix'Range (1) loop
               for Column in Matrix'Range (2) loop
                  if Matrix (Row, Column) /= Transition_Exists (Row, Column, TM) then
                     raise Program_Error;
                  end if;
               end loop;
            end loop;


            Calculate_SCC (TM);

            --  Check that every node is either in a SCC or is wandering.
            for N in Nodes'Range loop
               if Nodes (N) = 0 then
                  raise Program_Error;
               end if;
            end loop;

            --  Check that the graph reduced modulo the equivalence relationship
            --  "in the same SCC" has no cycles except for self-edges.  For this
            --  purpose each wandering vertices is considered to be a SCC.
            declare
               RSize : constant Natural := Next_SCC_Index - 1;
               subtype Vertex_Index is Integer range 1 .. RSize;
               RMatrix : array (Vertex_Index, Vertex_Index) of Boolean := (others => (others => False));
               Scheduled : array (Vertex_Index) of Boolean;
               Work_List : array (1 .. RSize) of Vertex_Index;
               Next_Work : Natural := 0;

               Vertex : Vertex_Index;
            begin
               for Row in Matrix'Range (1) loop
                  for Column in Matrix'Range (2) loop
                     if Nodes (Row) /= Nodes (Column) and Matrix (Row, Column) then
                        RMatrix (Nodes (Row), Nodes (Column)) := True;
                     end if;
                  end loop;
               end loop;

               for Initial in Vertex_Index loop
                  Scheduled := (others => False);
                  Next_Work := Next_Work + 1;
                  Work_List (Next_Work) := Initial;
                  Scheduled (Initial) := True;

                  while Next_Work > 0 loop
                     Vertex := Work_List (Next_Work);
                     Next_Work := Next_Work - 1;
                     pragma Assert (Scheduled (Vertex));

                     declare
                        Row : constant Positive := Vertex;
                        Column : Positive;
                     begin
                        for Target in Vertex_Index loop
                           Column := Target;
                           if RMatrix (Row, Column) then
                              if Column = Initial then
                                 raise Program_Error;
                              end if;
                              if not Scheduled (Target) then
                                 Next_Work := Next_Work + 1;
                                 Work_List (Next_Work) := Target;
                                 Scheduled (Target) := True;
                              end if;
                           end if;
                        end loop;
                        end;
                  end loop;
               end loop;
            end;
         end;

         --  Increment the value of Matrix, viewed as a binary number.
         for Row in Matrix'Range (1) loop
            for Column in Matrix'Range (2) loop
               if Matrix (Row, Column) = False then
                  Matrix (Row, Column) := True;
                  goto Test_Next;
               end if;
               Matrix (Row, Column) := False;
            end loop;
         end loop;

         -- Tested all possible graphs - fall through.
      end;
   end loop;
end Test_SCC;
