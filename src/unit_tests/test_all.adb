with Row_Test;
with Test_Integers;
with Test_Primitive;
with Test_SCC;

procedure Test_All is
begin
   Row_Test;
   Test_Integers;
   Test_Primitive.Test_All;
   Test_SCC;
end Test_All;
