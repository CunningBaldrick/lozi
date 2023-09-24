with Integers; use Integers;
with Segments_Support; use Segments_Support;

function Segments_Intersect (
  X1, Y1 : Point_Type;
  X2, Y2 : Point_Type
) return Boolean is
   M : Matrix_Type;
   R : Row_Permutation;
   C : Col_Permutation;
   Rank : Rank_Range;

   function Different_Group (C1, C2 : Col_Index) return Boolean;
   pragma Inline (Different_Group);

   function Different_Group (C1, C2 : Col_Index) return Boolean is
      AC1 : constant Col_Index := C (C1);
      AC2 : constant Col_Index := C (C2);
   begin
      return (AC1 <= 2 and AC2 >= 3) or (AC1 >= 3 and AC2 <= 2);
   end Different_Group;
begin
   M (1, 1) := Get_Component (x, X1);
   M (2, 1) := Get_Component (y, X1);
   M (3, 1) := Get_Component (z, X1);

   M (1, 2) := Get_Component (x, Y1);
   M (2, 2) := Get_Component (y, Y1);
   M (3, 2) := Get_Component (z, Y1);

   M (1, 3) := Negate (Get_Component (x, X2));
   M (2, 3) := Negate (Get_Component (y, X2));
   M (3, 3) := Negate (Get_Component (z, X2));

   M (1, 4) := Negate (Get_Component (x, Y2));
   M (2, 4) := Negate (Get_Component (y, Y2));
   M (3, 4) := Negate (Get_Component (z, Y2));

   Row_Reduce (M, R, C, Rank, Strict => True);

   case Rank is
      when 0 =>
         --  Vectors were all zero
         return True;
      when 1 =>
         declare
            Zeros : array (1 .. 4) of Boolean;
            Saw_Positive : Boolean := False;
            Saw_Negative : Boolean := False;
            Row_1 : constant Row_Index := R (1);
            Spare : Integer;
         begin
            for J in Col_Index loop
               Spare := Sign (M (Row_1, C (J)));
               Saw_Positive := Saw_Positive or Spare > 0;
               Saw_Negative := Saw_Negative or Spare < 0;
               Zeros (J) := Spare = 0;
               if Saw_Positive and Saw_Negative then
                  return True;
               end if;
            end loop;

            return (Zeros (1) or Zeros (2)) and (Zeros (3) or Zeros (4));
         end;
      when 2 =>
         declare
            Row_1 : constant Row_Index := R (1);
            Row_2 : constant Row_Index := R (2);
            Col_3 : constant Col_Index := C (3);
            Col_4 : constant Col_Index := C (4);
            S11 : constant Integer := Sign (M (Row_1, C (1)));
            S22 : constant Integer := Sign (M (Row_2, C (2)));
            S13 : constant Integer := S11 * Sign (M (Row_1, Col_3));
            S14 : constant Integer := S11 * Sign (M (Row_1, Col_4));
            S23 : constant Integer := S22 * Sign (M (Row_2, Col_3));
            S24 : constant Integer := S22 * Sign (M (Row_2, Col_4));
         begin
            pragma Assert (S11 /= 0 and S22 /= 0);
            if (S13 < 0 and S23 < 0) or (S14 < 0 and S24 < 0) then
               return True;
            end if;

            if (S13 > 0 and S14 > 0) or (S23 > 0 and S24 > 0) then
               return False;
            end if;

            if S13 = 0 and S14 = 0 and S23 = 0 and S24 = 0 then
               return Different_Group (1, 2);
            end if;

            if S13 >= 0 and S14 >= 0 and S23 >= 0 and S24 >= 0 then
               return False;
            end if;

            if S13 <= 0 and S14 <= 0 and S23 <= 0 and S24 <= 0 then
               return True;
            end if;

            if S13 = 0 or S14 = 0 or S23 = 0 or S24 = 0 then
               if (S13 = 0 and S23 = 0) or (S14 = 0 and S24 = 0) then
                  --  Zero column
                  pragma Assert (S13 * S23 < 0 or S14 * S24 < 0);
                  return False;
               end if;

               if (S13 = 0 and S14 = 0) or (S23 = 0 and S24 = 0) then
                  --  Zero row
                  pragma Assert (S13 * S14 < 0 or S23 * S24 < 0);
                  return True;
               end if;

               if (S13 = 0 and S24 = 0) or (S14 = 0 and S23 = 0) then
                  --  Zero diagonal
                  pragma Assert (S13 * S24 < 0 or S14 * S23 < 0);
                  declare
                     Pos_Col : Col_Index;
                     Pos_Row : Row_Index;
                  begin
                     if S13 > 0 or S23 > 0 then
                        Pos_Col := 3;
                     else
                        Pos_Col := 4;
                     end if;

                     if S13 > 0 or S14 > 0 then
                        Pos_Row := 1;
                     else
                        Pos_Row := 2;
                     end if;

                     return Different_Group (Pos_Col, Col_Index (Pos_Row));
                  end;
               end if;

               --  Only one zero element
               if (S13 = 0 and S14 < 0) or (S14 = 0 and S13 < 0) or
                  (S23 = 0 and S24 < 0) or (S24 = 0 and S23 < 0) then
                  pragma Assert (
                    (S13 = 0 and S14 < 0 and S23 < 0 and S24 > 0) or
                    (S23 = 0 and S24 < 0 and S13 < 0 and S14 > 0) or
                    (S14 = 0 and S13 < 0 and S24 < 0 and S23 > 0) or
                    (S24 = 0 and S23 < 0 and S14 < 0 and S13 > 0)
                  );
                  return True;
               end if;

               if (S13 = 0 and S23 > 0) or (S23 = 0 and S13 > 0) or
                  (S14 = 0 and S24 > 0) or (S24 = 0 and S14 > 0) then
                  pragma Assert (
                    (S13 = 0 and S23 > 0 and S14 > 0 and S24 < 0) or
                    (S14 = 0 and S24 > 0 and S13 > 0 and S23 < 0) or
                    (S23 = 0 and S13 > 0 and S24 > 0 and S14 < 0) or
                    (S24 = 0 and S14 > 0 and S23 > 0 and S13 < 0)
                  );
                  return False;
               end if;

               pragma Assert (
                 (S13 = 0 and S14 > 0 and S23 < 0) or
                 (S14 = 0 and S13 > 0 and S24 < 0) or
                 (S23 = 0 and S24 > 0 and S13 < 0) or
                 (S24 = 0 and S23 > 0 and S14 < 0)
               );

               declare
                  Pos_Col : Col_Index;
                  Zero_Row : Row_Index;
               begin
                  if S13 > 0 or S23 > 0 then
                     Pos_Col := 3;
                  else
                     Pos_Col := 4;
                  end if;

                  if S13 = 0 or S14 = 0 then
                     Zero_Row := 1;
                  else
                     Zero_Row := 2;
                  end if;

                  return Different_Group (Pos_Col, Col_Index (Zero_Row));
               end;
            end if;

            declare
               A : Integer_Type renames M (Row_1, Col_3);
               B : Integer_Type renames M (Row_2, Col_3);
               C : Integer_Type renames M (Row_1, Col_4);
               D : Integer_Type renames M (Row_2, Col_4);
               S : constant Integer := Sign (A) * S22;

               AD : constant Integer_Type := A * D;
               BC : constant Integer_Type := B * C;
            begin
               if AD > BC then
                  return S < 0;
               elsif AD /= BC then -- AD < BC
                  return S > 0;
               end if;

               return Different_Group (1, 2);
            end;
         end;
      when 3 =>
         declare
            Col_4 : constant Col_Index := C (4);
            Signs : array (Col_Index) of Integer;
         begin
            Signs (Col_4) := 1;
            for I in Row_Index loop
               declare
                  Row : constant Row_Index := R (I);
                  Col : constant Col_Index := C (I);
                  Spare : Integer;
               begin
                  Spare := Sign (M (Row, Col)) * Sign (M (Row, Col_4));
                  if Spare > 0 then
                     return False;
                  end if;
                  Signs (Col) := -Spare;
               end;
            end loop;
            return (Signs (1) /= 0 or Signs (2) /= 0) and
                   (Signs (3) /= 0 or Signs (4) /= 0);
         end;
   end case;
end Segments_Intersect;
