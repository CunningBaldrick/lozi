with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Float_Text_IO;
with Ada.Integer_Text_IO;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Arnoldi;
with IEEE;
with Integers.IO;
with Lower_Transition_Matrix;
with Lozi;
with Partition;
with Partition.IO;
with Partition.Refine;
with Points;
with Polygons;
with Print_Lower;
with Print_Upper;
with Print_Usage;
with Symbolics;
with Transition_Matrices;
with Transition_Matrices.Spectral_Radius;
with Trim_Partition;
with Upper_Transition_Matrix;

procedure Calculate_Entropy is

   use Ada;
   use IEEE;

   procedure Free is new Unchecked_Deallocation (
     Transition_Matrices.Transition_Matrix_Type,
     Transition_Matrices.Transition_Matrix_Pointer
   );

   Depth : Positive := 1; -- length of symbol sequences

   Best_Upper_Radius : Float := 2.0; -- Maximum possible spectral radius
   Best_Lower_Radius : Float := 1.0; -- Minimum possible spectral radius
   Best_Upper_Entropy : Float := Log (Best_Upper_Radius, Upwards);
   Best_Lower_Entropy : Float := Log (Best_Lower_Radius, Downwards);
   Old_Best_Upper_Entropy : Float := Best_Upper_Entropy;
   Old_Best_Lower_Entropy : Float := Best_Lower_Entropy;

   Accuracy   : Float;
   Default_Accuracy : constant := 0.01;
   Average_Improvement : Float := 1.0;
   Decay_Multiplier : constant := 0.9;
   Decay_Constant   : constant := 1.0 - Decay_Multiplier;
   Smallest_Improvement : Float;

   Num_Digits : Positive;

   Verbose : Boolean;

   Count : Natural;

   Low, High : Float;
begin
   Process_Command_Line :
      declare
         First_Numeric_Parameter : Positive;
         With_Accuracy : Boolean;
         Skew_Lozi : Boolean;
         Found : Boolean;
      begin
         if Command_Line.Argument_Count < 4 or
            Command_Line.Argument_Count > 8 then
            Print_Usage;
            return;
         end if;

         if Command_Line.Argument (1) = "-v" then
            Verbose := True;
            First_Numeric_Parameter := 2;
         else
            Verbose := False;
            First_Numeric_Parameter := 1;
         end if;

         --  Work out from the argument count whether this is a skew lozi map
         --  and whether there's an accuracy term.
         Found := False;
         for Accuracy in Boolean loop
            for Skew in Boolean loop
               if Command_Line.Argument_Count =
                 (if Verbose then 1 else 0) +
                 4 + -- Two for "A" plus two for "B"
                 (if Skew then 2 else 0) + -- Two for "C"
                 (if Accuracy then 1 else 0)
               then
                  if Found then
                     raise Program_Error with "Ambiguous configuration";
                  end if;
                  With_Accuracy := Accuracy;
                  Skew_Lozi := Skew;
                  Found := True;
               end if;
            end loop;
         end loop;
         if not Found then
            Print_Usage;
            return;
         end if;

         if With_Accuracy then
            begin
               Accuracy := Float'Value (
                 Command_Line.Argument (Command_Line.Argument_Count)
               );
               if Accuracy < Float'Model_Epsilon then
                  Accuracy := Float'Model_Epsilon;
               end if;
               if Accuracy > 1.0 then
                  Text_IO.Put_Line (Text_IO.Standard_Error,
                    "Warning: accuracy lowered to 1.0");
                  Accuracy := 1.0;
               end if;
            exception
               when Constraint_Error =>
                  Text_IO.Put (Text_IO.Standard_Error, "Error: ");
                  Text_IO.Put (Text_IO.Standard_Error,
                    "accuracy must be a floating point number");
                  Text_IO.New_Line (Text_IO.Standard_Error);
                  Text_IO.Flush (Text_IO.Standard_Error);
                  return;
            end;
         else
            Accuracy := Default_Accuracy;
         end if;

         Num_Digits := Positive (
           -Float'Floor (
             --  Neither accuracy nor the rounding used matters here.
             Log (Accuracy / 2.0, To_Nearest) /
               Log (10.0, To_Nearest)
           )
         );

         Get_Parameters :
            declare
               A_Numerator   : Integers.Integer_Type;
               A_Denominator : Integers.Integer_Type;
               B_Numerator   : Integers.Integer_Type;
               B_Denominator : Integers.Integer_Type;
               C_Numerator   : Integers.Integer_Type;
               C_Denominator : Integers.Integer_Type;
            begin
               begin
                  A_Numerator   := Integers.IO.Parse (
                    Command_Line.Argument (First_Numeric_Parameter)
                  );
                  A_Denominator := Integers.IO.Parse (
                    Command_Line.Argument (First_Numeric_Parameter + 1)
                  );
                  B_Numerator   := Integers.IO.Parse (
                    Command_Line.Argument (First_Numeric_Parameter + 2)
                  );
                  B_Denominator := Integers.IO.Parse (
                    Command_Line.Argument (First_Numeric_Parameter + 3)
                  );
                  if Skew_Lozi then
                     C_Numerator   := Integers.IO.Parse (
                       Command_Line.Argument (First_Numeric_Parameter + 4)
                     );
                     C_Denominator := Integers.IO.Parse (
                       Command_Line.Argument (First_Numeric_Parameter + 5)
                     );
                  else
                     C_Numerator := Integers.Zero;
                     C_Denominator := Integers.One;
                  end if;
               exception
                  when Constraint_Error =>
                     Text_IO.Put (Text_IO.Standard_Error, "Error: ");
                     Text_IO.Put (Text_IO.Standard_Error,
                       "Lozi parameters must be integers");
                     Text_IO.New_Line (Text_IO.Standard_Error);
                     Text_IO.Flush (Text_IO.Standard_Error);
                     return;
               end;

               begin
                  Lozi.Set_Parameters (
                    A_Numerator   => A_Numerator,
                    A_Denominator => A_Denominator,
                    B_Numerator   => B_Numerator,
                    B_Denominator => B_Denominator,
                    C_Numerator   => C_Numerator,
                    C_Denominator => C_Denominator
                  );
               exception
                  when others =>
                     Text_IO.Put (Text_IO.Standard_Error, "Error: ");
                     Text_IO.Put (Text_IO.Standard_Error,
                       "invalid parameter values");
                     Text_IO.New_Line (Text_IO.Standard_Error);
                     Text_IO.Flush (Text_IO.Standard_Error);
                     return;
               end;

               if Verbose then
                  Text_IO.Put_Line ("-- Parameters --");
                  Text_IO.Put ("A = ");
                  Integers.IO.Put (A_Numerator);
                  Text_IO.Put (" / ");
                  Integers.IO.Put (A_Denominator);
                  Text_IO.New_Line;
                  Text_IO.Put ("B = ");
                  Integers.IO.Put (B_Numerator);
                  Text_IO.Put (" / ");
                  Integers.IO.Put (B_Denominator);
                  Text_IO.New_Line;
                  Text_IO.Put ("C = ");
                  Integers.IO.Put (C_Numerator);
                  Text_IO.Put (" / ");
                  Integers.IO.Put (C_Denominator);
                  Text_IO.New_Line;
                  Text_IO.Put ("Accuracy = ");
                  Float_Text_IO.Put (Accuracy, Aft => Num_Digits, Exp => 0);
                  Text_IO.New_Line (2);
                  Text_IO.Flush;
               end if;
            end Get_Parameters;
      end Process_Command_Line;

   Smallest_Improvement := Accuracy / Decay_Constant / 100.0;

   Initialize_Partition :
      declare
         use Integers;
         use Partition;
         use Symbolics;
      begin
         Partition.Add_Element (
           Create_Element (
             Polygon =>
               Polygons.Create (
                 (
                   Points.Create (Zero,         Zero,         One),
                   Points.Create (Negate (One), Zero,         Zero),
                   Points.Create (Zero,         Negate (One), Zero)
                 )
             ),
             Symbols =>
               (
                 1 => L,
                 2 => L
               )
           )
         );

         Partition.Add_Element (
           Create_Element (
             Polygon =>
               Polygons.Create (
                 (
                   Points.Create (Zero,         Zero,         One),
                   Points.Create (Zero,         Negate (One), Zero),
                   Points.Create (One,          Zero,         Zero)
                 )
             ),
             Symbols =>
               (
                 1 => L,
                 2 => R
               )
           )
         );

         Partition.Add_Element (
           Create_Element (
             Polygon =>
               Polygons.Create (
                 (
                   Points.Create (Zero,         Zero,         One),
                   Points.Create (Zero,         One,          Zero),
                   Points.Create (Negate (One), Zero,         Zero)
                 )
             ),
             Symbols =>
               (
                 1 => R,
                 2 => L
               )
           )
         );

         Partition.Add_Element (
           Create_Element (
             Polygon =>
               Polygons.Create (
                 (
                   Points.Create (Zero,         Zero,         One),
                   Points.Create (One,          Zero,         Zero),
                   Points.Create (Zero,         One,          Zero)
                 )
             ),
             Symbols =>
               (
                 1 => R,
                 2 => R
               )
           )
         );
      end Initialize_Partition;

   if Verbose then
      Text_IO.Put_Line ("-- Initial Partition --");
      Partition.IO.Put_Partition;
      Text_IO.New_Line;
      Text_IO.Flush;
   end if;

   Estimate_Spectral_Radius :
      begin
         if Verbose then
            Text_IO.Put_Line ("=> Calculating spectral radii");
            Text_IO.New_Line;
            Text_IO.Flush;
         end if;

         Calculate_Spectral_Radius :
            loop
               if Verbose then
                  Text_IO.Put_Line ("  => Computing upper transition matrix");
                  Text_IO.Flush;
               end if;

               Calculate_Upper_Transition_Matrix :
                  declare
                     Matrix : Transition_Matrices.Transition_Matrix_Pointer :=
                       Upper_Transition_Matrix;
                     Spectral_Radius : Float;
                  begin
                     if Verbose then
                        Text_IO.Put_Line ("  <= Done");
                        Text_IO.New_Line;
                        Text_IO.Put_Line ("  Size:" & Matrix.Last_Row'Img);
                        Text_IO.New_Line;
                        Text_IO.Put_Line ("  => Computing spectral radius");
                        Text_IO.Flush;
                     end if;

                     --  Calculate the spectral radius of the transition matrix
                     begin
                        Transition_Matrices.Spectral_Radius.Estimate (
                          Matrix.all,
                          Low,
                          High
                        );
                        Spectral_Radius := High;
                        if Spectral_Radius < Best_Upper_Radius then
                           Best_Upper_Radius := Spectral_Radius;
                           Old_Best_Upper_Entropy := Best_Upper_Entropy;
                           Best_Upper_Entropy :=
                             Log (Best_Upper_Radius, Upwards);
                        end if;

                        if Verbose then
                           Text_IO.Put_Line ("  <= Done");
                           Text_IO.New_Line;
                           Text_IO.Put (" ");
                           Print_Lower (Low);
                           Text_IO.Put (" <= spectral radius <=");
                           Print_Upper (High);
                           Text_IO.New_Line;
                           Text_IO.Put ("  Entropy (natural logarithm):");
                           Float_Text_IO.Put (
                             --  Rounding doesn't matter, purely informative.
                             Log (Spectral_Radius, To_Nearest),
                             Aft => Num_Digits + 2,
                             Exp => 0
                           );
                           Text_IO.New_Line (2);
                           Text_IO.Flush;
                        end if;
                     exception
                        when Arnoldi.Arnoldi_Error =>
                           Text_IO.Put_Line ("  <= Failed");
                           Text_IO.New_Line;
                           Text_IO.Flush;
                        when E : others =>
                           Text_IO.Put_Line ("  <= Failed");
                           Text_IO.New_Line;
                           Text_IO.Put_Line (
                             Exceptions.Exception_Information (E)
                           );
                           Text_IO.New_Line;
                           Text_IO.Flush;
                           raise;
                     end;

                     if Verbose then
                        Text_IO.Put_Line ("  => Trimming matrix");
                        Text_IO.Flush;
                     end if;

                     begin
                        Trim_Partition (Matrix.all);

                        if Verbose then
                           Text_IO.Put_Line ("  <= Done");
                           Text_IO.New_Line;
                           Text_IO.Put_Line (
                             "  New size:" & Integer'Image (
                               Partition.Element_Count
                             )
                           );
                           Text_IO.New_Line;
                           Text_IO.Flush;
                        end if;

                     exception
                        when E : others =>
                           Text_IO.Put_Line ("  <= Failed");
                           Text_IO.New_Line;
                           Text_IO.Put_Line (
                             Exceptions.Exception_Information (E)
                           );
                           Text_IO.New_Line;
                           raise;
                     end;

                     Free (Matrix);

                  end Calculate_Upper_Transition_Matrix;

               exit Calculate_Spectral_Radius
                  when Average_Improvement < Smallest_Improvement or
                       Best_Upper_Entropy - Best_Lower_Entropy <= Accuracy;

               if Verbose then
                  Text_IO.Put_Line ("  => Computing lower transition matrix");
                  Text_IO.Flush;
               end if;

               Calculate_Lower_Transition_Matrix :
                  declare
                     Matrix : Transition_Matrices.Transition_Matrix_Pointer :=
                       Lower_Transition_Matrix;
                     Spectral_Radius : Float;
                  begin
                     if Verbose then
                        Text_IO.Put_Line ("  <= Done");
                        Text_IO.New_Line;
                        Text_IO.Put_Line ("  Size:" & Matrix.Last_Row'Img);
                        Text_IO.New_Line;
                        Text_IO.Flush;
                     end if;

                     if Verbose then
                        Text_IO.Put_Line ("  => Computing spectral radius");
                        Text_IO.Flush;
                     end if;

                     begin
                        Transition_Matrices.Spectral_Radius.Estimate (
                          Matrix.all,
                          Low,
                          High
                        );
                        Spectral_Radius := Low;

                        if Spectral_Radius > Best_Lower_Radius then
                           Best_Lower_Radius := Spectral_Radius;
                           Old_Best_Lower_Entropy := Best_Lower_Entropy;
                           Best_Lower_Entropy :=
                             Log (Best_Lower_Radius, Downwards);
                        end if;

                        if Verbose then
                           Text_IO.Put_Line ("  <= Done");
                           Text_IO.New_Line;
                           Text_IO.Put (" ");
                           Print_Lower (Low);
                           Text_IO.Put (" <= spectral radius <=");
                           Print_Upper (High);
                           if Spectral_Radius > 0.0 then
                              Text_IO.New_Line;
                              Text_IO.Put ("  Entropy (natural logarithm):");
                              Float_Text_IO.Put (
                                --  Rounding doesn't matter, purely informative.
                                Log (Spectral_Radius, To_Nearest),
                                Aft => Num_Digits + 2,
                                Exp => 0
                              );
                           end if;
                           Text_IO.New_Line (2);
                           Text_IO.Flush;
                        end if;
                     exception
                        when Arnoldi.Arnoldi_Error =>
                           Text_IO.Put_Line ("  <= Failed");
                           Text_IO.New_Line;
                           Text_IO.Flush;
                        when E : others =>
                           Text_IO.Put_Line ("  <= Failed");
                           Text_IO.New_Line;
                           Text_IO.Put_Line (
                             Exceptions.Exception_Information (E)
                           );
                           Text_IO.New_Line;
                           Text_IO.Flush;
                           raise;
                     end;

                     Free (Matrix);
                  end Calculate_Lower_Transition_Matrix;

               pragma Assert (Best_Lower_Radius <= Best_Upper_Radius);

               if Verbose then
                  Text_IO.Put ("  Best estimate so far:");
                  Print_Lower (Best_Lower_Entropy);
                  Text_IO.Put (" <= Entropy <=");
                  Print_Upper (Best_Upper_Entropy);
                  Text_IO.New_Line (2);
                  Text_IO.Flush;
               end if;

               Average_Improvement := Decay_Multiplier * Average_Improvement +
                 (Old_Best_Upper_Entropy - Old_Best_Lower_Entropy) -
                 (Best_Upper_Entropy - Best_Lower_Entropy);

               exit Calculate_Spectral_Radius
                  when Average_Improvement < Smallest_Improvement or
                       Best_Upper_Entropy - Best_Lower_Entropy <= Accuracy;

               if Verbose then
                  Text_IO.Put_Line ("  => Refining partition");
                  Text_IO.Flush;
               end if;

               Partition.Refine;
               Count := Partition.Element_Count;
               Depth := Depth + 1;

               if Verbose then
                  Text_IO.Put_Line ("  <= Done");
                  Text_IO.New_Line;

                  Text_IO.Put ("  Partition depth: ");
                  Integer_Text_IO.Put (Depth, Width => 0);
                  Text_IO.New_Line;
                  Text_IO.Put ("  Number of partition elements: ");
                  Integer_Text_IO.Put (Count, Width => 0);
                  Text_IO.New_Line (2);

                  Text_IO.Flush;
               end if;
            end loop Calculate_Spectral_Radius;
      end Estimate_Spectral_Radius;

   if Verbose then
      Text_IO.Put_Line ("<= Done");
      Text_IO.New_Line;
      Text_IO.Put ("Best upper spectral radius:");
      Print_Upper (Best_Upper_Radius);
      Text_IO.New_Line;
      Text_IO.Put ("Best lower spectral radius:");
      Print_Lower (Best_Lower_Radius);
      Text_IO.New_Line (2);
      Text_IO.Put ("Entropy (natural logarithm):");
   end if;

   Print_Lower (Best_Lower_Entropy);
   Text_IO.Put (" <= Entropy <=");
   Print_Upper (Best_Upper_Entropy);
   Text_IO.New_Line;
   Text_IO.Flush;

   Partition.Delete_All_Elements;
exception
   when E : others =>
      if Verbose then
         Text_IO.Set_Output (Text_IO.Standard_Error);
         Text_IO.Put_Line ("------------------");
         Text_IO.Put_Line ("-- BUG DETECTED --");
         Text_IO.Put_Line ("------------------");
         Text_IO.Put_Line (Exceptions.Exception_Information (E));
         Text_IO.New_Line;
      end if;

      if Verbose then
         Text_IO.Put ("Best entropy estimate so far: ");
      end if;

      Print_Lower (Best_Lower_Entropy);
      Text_IO.Put (" <= Entropy <=");
      Print_Upper (Best_Upper_Entropy);

      if not Verbose then
         Text_IO.Put (" (error)");
      end if;
      Text_IO.New_Line;

      if Verbose then
         Text_IO.Put ("Partition depth reached: ");
         Integer_Text_IO.Put (Depth, Width => 0);
         Text_IO.New_Line;
      end if;

      Text_IO.Flush;

      Partition.Delete_All_Elements;
end Calculate_Entropy;
