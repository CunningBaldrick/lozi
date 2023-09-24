package body Symbolics is
   function "<" (Left, Right : Sequence_Type) return Boolean is
      Right_Position : Natural := Right'First;
   begin
      if Left'Length /= Right'Length then
         raise Constraint_Error;
      end if;

      for Left_Position in Left'Range loop
         if Left (Left_Position) /= Right (Right_Position) then
            return Left (Left_Position) < Right (Right_Position);
         end if;

         Right_Position := Right_Position + 1;
      end loop;

      return False;
   end "<";

   function ">=" (Left, Right : Sequence_Type) return Boolean is
   begin
      return not "<" (Left, Right);
   end ">=";

   function ">" (Left, Right : Sequence_Type) return Boolean is
      Right_Position : Natural := Right'First;
   begin
      if Left'Length /= Right'Length then
         raise Constraint_Error;
      end if;

      for Left_Position in Left'Range loop
         if Left (Left_Position) /= Right (Right_Position) then
            return Left (Left_Position) > Right (Right_Position);
         end if;

         Right_Position := Right_Position + 1;
      end loop;

      return False;
   end ">";

   function "<=" (Left, Right : Sequence_Type) return Boolean is
   begin
      return not ">" (Left, Right);
   end "<=";
end Symbolics;
