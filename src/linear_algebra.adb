package body Linear_Algebra is
   procedure Row_Reduce (
     Matrix : in out Matrix_Type;
     Rows   :    out Row_Permutation;
     Cols   :    out Col_Permutation;
     Rank   :    out Rank_Range;
     Strict : in     Boolean
   ) is
      Zeros : array (Row_Index, Col_Index) of Boolean;

      Col_Count : array (Col_Index) of Integer;
      Row_Count : array (Row_Index) of Integer;

      Top, Bottom : Row_Index;
      Left, Right : Col_Index;

      M, N : Natural;

      Zero_Rows, Zero_Cols : Natural;
   begin
      Rank := 0;

      for I in Row_Index loop
         Rows (I) := I;
      end loop;

      for J in Col_Index loop
         Cols (J) := J;
      end loop;

      Top := Row_Index'First;
      Bottom := Row_Index'Last;

      Left := Col_Index'First;
      Right := Col_Index'Last;

      loop
         M := Bottom - Top + 1;
         N := Right - Left + 1;

         exit when M = 0 or N = 0;

         for I in Top .. Bottom loop
            Row_Count (I) := 0;
         end loop;

         for J in Left .. Right loop
            Col_Count (J) := 0;
         end loop;

         for J in Left .. Right loop
            for I in Top .. Bottom loop
               declare
                  R : constant Row_Index := Rows (I);
                  C : constant Col_Index := Cols (J);
                  Z : constant Boolean := Is_Zero (Matrix (R, C));
               begin
                  Zeros (R, C) := Z;
                  if Z then
                     Row_Count (I) := Row_Count (I) + 1;
                     Col_Count (J) := Col_Count (J) + 1;
                  end if;
               end;
            end loop;
         end loop;

         Zero_Rows := 0;
         for I in Top .. Bottom loop
            if Row_Count (I) = N then
               Row_Count (I) := -1; -- ensure zero rows come last
               Zero_Rows := Zero_Rows + 1;
            end if;
         end loop;

         Zero_Cols := 0;
         for J in Left .. Right loop
            if Col_Count (J) = M then
               Zero_Cols := Zero_Cols + 1;
            end if;
         end loop;

         M := M - Zero_Rows;
         N := N - Zero_Cols;

         pragma Assert (
           ((M = 0 or N = 0) and (M = 0 and N = 0)) or (M /= 0 and N /= 0)
         );

         exit when M = 0 or N = 0;

         Rank := Rank + 1;

         --  Bubble sort the rows.  Non-zero rows with most zeros come first.
         declare
            Switched : Boolean;
            Temp1 : Integer;
            Temp2 : Row_Index;
         begin
            loop
               Switched := False;

               for I in Top .. Bottom - 1 loop
                  if Row_Count (I) < Row_Count (I + 1) then
                     Temp1 := Row_Count (I);
                     Temp2 := Rows (I);
                     Row_Count (I) := Row_Count (I + 1);
                     Rows (I) := Rows (I + 1);
                     Row_Count (I + 1) := Temp1;
                     Rows (I + 1) := Temp2;
                     Switched := True;
                  end if;
               end loop;

               exit when not Switched;
            end loop;
         end;

         --  Bubble sort the columns.  Rows with least zeros come first.
         declare
            Switched : Boolean;
            Temp1 : Integer;
            Temp2 : Col_Index;
         begin
            loop
               Switched := False;

               for J in Left .. Right - 1 loop
                  if Col_Count (J) > Col_Count (J + 1) then
                     Temp1 := Col_Count (J);
                     Temp2 := Cols (J);
                     Col_Count (J) := Col_Count (J + 1);
                     Cols (J) := Cols (J + 1);
                     Col_Count (J + 1) := Temp1;
                     Cols (J + 1) := Temp2;
                     Switched := True;
                  end if;
               end loop;

               exit when not Switched;
            end loop;
         end;

         Bottom := Top + M - 1;
         Right := Left + N - 1;

         --  Ensure non-zero top-left element
         if Zeros (Rows (Top), Cols (Left)) then
            declare
               L_Col : constant Col_Index := Cols (Left);
               Temp1 : Integer;
               Temp2 : Row_Index;
            begin
               for I in Top + 1 .. Bottom loop
                  if not Zeros (Rows (I), L_Col) then
                     Temp1 := Row_Count (Top);
                     Temp2 := Rows (Top);
                     Row_Count (Top) := Row_Count (I);
                     Rows (Top) := Rows (I);
                     Row_Count (I) := Temp1;
                     Rows (I) := Temp2;
                     exit;
                  end if;
               end loop;
            end;
         end if;

         declare
            T_Row : constant Row_Index := Rows (Top);
            L_Col : constant Col_Index := Cols (Left);
            Alpha : Integer_Type renames Matrix (T_Row, L_Col);
            Alpha_Is_One : Boolean;
            Starting_Row : Row_Index;
         begin
            pragma Assert (not Is_Zero (Alpha));

            if Row_Count (Top) = N + Zero_Cols - 1 then
               Alpha := One;
               Alpha_Is_One := True;
            else
               Alpha_Is_One := Alpha = One;
            end if;

            if Strict then
               Starting_Row := Row_Index'First;
            else
               Starting_Row := Top + 1;
            end if;

            for I in Starting_Row .. Bottom loop
               if I /= Top then
                  declare
                     R : constant Row_Index := Rows (I);
                     Alpha_Prime : Integer_Type renames Matrix (R, L_Col);
                  begin
                     if not Is_Zero (Alpha_Prime) then
                        if I < Top and not Alpha_Is_One then
                           for J in Col_Index'First .. Left - 1 loop
                              declare
                                 C : constant Col_Index := Cols (J);
                              begin
                                 Matrix (R, C) := Alpha * Matrix (R, C);
                              end;
                           end loop;

                           for J in Right + 1 .. Col_Index'Last loop
                              declare
                                 C : constant Col_Index := Cols (J);
                              begin
                                 Matrix (R, C) := Alpha * Matrix (R, C);
                              end;
                           end loop;
                        end if;

                        for J in Left + 1 .. Right loop
                           declare
                              C : constant Col_Index := Cols (J);
                           begin
                              if Zeros (T_Row, C) then
                                 if not Alpha_Is_One then
                                    Matrix (R, C) := Alpha * Matrix (R, C);
                                 end if;
                              else
                                 if Alpha_Is_One then
                                    Matrix (R, C) := Matrix (R, C)
                                      - Alpha_Prime * Matrix (T_Row, C);
                                 else
                                    Matrix (R, C) := Alpha * Matrix (R, C)
                                      - Alpha_Prime * Matrix (T_Row, C);
                                 end if;
                              end if;
                           end;
                        end loop;
                        Matrix (R, L_Col) := Zero;
                     end if;
                  end;
               end if;
            end loop;
         end;

         exit when Top = Bottom or Left = Right;

         Top := Top + 1;
         Left := Left + 1;
      end loop;
   end Row_Reduce;
end Linear_Algebra;
