with Ada.Numerics.Discrete_Random;
with Transition_Matrix_Rows;
use Transition_Matrix_Rows;

procedure TTMR is
--   type Num_Loops is range 0 .. 30;
--   package Num_Random is new Ada.Numerics.Discrete_Random (Num_Loops);
--   NR : Num_Random.Generator;
   subtype Column_Range is Positive range 1 .. 10;
   package Column_Random is new Ada.Numerics.Discrete_Random (Column_Range);
   CR : Column_Random.Generator;
   package Coin_Random is new Ada.Numerics.Discrete_Random (Boolean);
   TR : Coin_Random.Generator;
begin
   loop
      declare
         Row : Matrix_Row;
         Col : Positive;
         Model : array (Column_Range) of Boolean := (others => False);
      begin
--         for L in 1 .. Num_Random.Random (NR) loop
         loop
            Col := Column_Random.Random (CR);
            if Coin_Random.Random (TR) or Coin_Random.Random (TR) then
               Set_Column (Row, Col);
               Model (Col) := True;
            else
               Clear_Column (Row, Col);
               Model (Col) := False;
            end if;
            for C in Column_Range loop
               if Model (C) /= Column_Is_Set (Row, C) then
                  raise Program_Error;
               end if;
            end loop;
         end loop;
         Clear_All (Row);
      end;
   end loop;
end;
