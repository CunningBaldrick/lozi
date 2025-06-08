with Integers.IO;

procedure Test_Integer_Parsing is
   use Integers;

   function Parse_By_Hand (Source : String) return Integer_Type is
      Negative : Boolean;
   begin
      --  Skip leading spaces.
      for I in Source'Range loop
         if Source (I) /= ' ' then
            --  Start the real parsing here.

            declare
               Ten : constant Integer_Type := To_Integer_Type (10);

               Digit : constant array (Character range '0' .. '9')
                 of Integer_Type := (
                   '0' => To_Integer_Type (0),
                   '1' => To_Integer_Type (1),
                   '2' => To_Integer_Type (2),
                   '3' => To_Integer_Type (3),
                   '4' => To_Integer_Type (4),
                   '5' => To_Integer_Type (5),
                   '6' => To_Integer_Type (6),
                   '7' => To_Integer_Type (7),
                   '8' => To_Integer_Type (8),
                   '9' => To_Integer_Type (9)
                 );

               Result : Integer_Type := Zero;

               procedure Incorporate (C : Character) is
               begin
                  if C not in '0' .. '9' then
                     raise Constraint_Error with "Invalid character " & C;
                  end if;
                  Result := Ten * Result + Digit (C);
               end Incorporate;
            begin
               if Source (I) = '-' then
                  Negative := True;
                  if I = Source'Last then
                     raise Constraint_Error with "No digits";
                  end if;
               else
                  Negative := False;
                  Incorporate (Source (I));
               end if;
               for J in I + 1 .. Source'Last loop
                  Incorporate (Source (J));
               end loop;
               if Negative then
                  return Negate (Result);
               else
                  return Result;
               end if;
            end;
         end if;
      end loop;
      raise Constraint_Error with "Blank";
   end Parse_By_Hand;

   procedure Validate (Source : String) is
      By_Hand, From_IO : Integer_Type;

      By_Hand_Success, From_IO_Success : Boolean;
   begin
      From_IO_Success := False;
      begin
         From_IO := IO.Parse (Source);
         From_IO_Success := True;
      exception
         when Constraint_Error =>
            null;
      end;

      By_Hand_Success := False;
      begin
         By_Hand := Parse_By_Hand (Source);
         By_Hand_Success := True;
      exception
         when Constraint_Error =>
            null;
      end;

      if From_IO_Success /= By_Hand_Success then
         if From_IO_Success then
            raise Program_Error with "Parsing should have failed: " & Source;
         else
            raise Program_Error with "Parsing failed: " & Source;
         end if;
      elsif From_IO_Success then
         if From_IO /= By_Hand then
            raise Program_Error with "Parsed to wrong value: " & Source;
         end if;
      end if;
   end Validate;

begin
   Validate ("");
   Validate ("-");
   Validate ("0");
   Validate ("1");
   Validate ("-1");
   Validate (" ");
   Validate ("  0");
   Validate ("   1");
   Validate ("    -1");

   Validate ("01");
   Validate ("-0");

   for Length in 1 .. 1_000 loop
      declare
         Source : String (1 .. Length);
         Digit  : Integer := 0;
      begin
         for C of Source loop
            C := Character'Val (Character'Pos ('0') + Digit);
            Digit := (Digit + 1) mod 10;
         end loop;
         Validate (Source);
         Validate ('-' & Source);
      end;
   end loop;
end Test_Integer_Parsing;
