with Row_Test;
with Test_Integer_Parsing;
with Test_Integers;
with Test_Primitive;
with Test_SCC;
with Test_Spectral_Radius;

procedure Test_All is
begin
   Row_Test;
   Test_Integer_Parsing;
   Test_Integers;
   Test_Primitive.Test_All;
   Test_SCC;
   Test_Spectral_Radius.Test_All;
end Test_All;
