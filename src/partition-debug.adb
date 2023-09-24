with Integers;
with Points;

package body Partition.Debug is
   use Integers;
   use Points;

   ------------------
   -- Is_Partition --
   ------------------
   function Is_Partition (Strict : Boolean) return Boolean is
      Current_Element : Element_Pointer := Get_First;
   begin
      while Current_Element /= null loop
         declare
            Polygon : Polygon_Type renames Current_Element.Polygon;
            X, Y : Point_Type;
            X_Comp : Integer_Type;
            Z_Comp : Integer_Type;
            Other_Element : Element_Pointer;
            Count : Natural;
            Signum : Integer := 0;
         begin
            if not Is_Polygon (Strict, Polygon) then
               return False;
            end if;

            for I in 1 .. Polygon.Number_Of_Vertices loop
               X := Get_Vertex (I, Polygon);
               Y := Get_Vertex (I + 1, Polygon);
               X_Comp := Get_Component (Points.x, X);
               if Signum = 0 then
                  Signum := Sign (X_Comp);
               else -- Signum /= 0
                  if Is_Positive (X_Comp) and Signum = -1 then
                     return False;
                  elsif Is_Negative (X_Comp) and Signum = 1 then
                     return False;
                  end if;
               end if;

               Z_Comp := Get_Component (z, X);
               if Is_Negative (Z_Comp) then
                  return False;
               end if;
               if Is_Positive (Z_Comp) or
                  Is_Positive (Get_Component (z, Y)) then
                  Count := 0;
                  Other_Element := Get_First;
                  while Other_Element /= null loop
                     if Other_Element /= Current_Element then
                        declare
                           Other_Polygon : Polygon_Type
                             := Get_Polygon (Other_Element.all);
                        begin
                           for J in 1 .. Other_Polygon.Number_Of_Vertices loop
                              if Equivalent (
                                X,
                                Get_Vertex (J, Other_Polygon)
                              ) then

                                 if Equivalent (
                                   Y,
                                   Get_Vertex (J + 1, Other_Polygon)
                                 ) then
                                    return False;
                                 elsif Equivalent (
                                   Y,
                                   Get_Vertex (J - 1, Other_Polygon)
                                 ) then
                                    Count := Count + 1;
                                 end if;

                              end if;
                           end loop;
                        end;
                     end if;
                     Other_Element := Other_Element.Next;
                  end loop;
                  if Count /= 1 then
                     return False;
                  end if;
               end if;
            end loop;
         end;
         Current_Element := Current_Element.Next;
      end loop;

      return True;
   end Is_Partition;
end Partition.Debug;
