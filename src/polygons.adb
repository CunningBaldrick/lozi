with Integers;

package body Polygons is
   ------------
   -- Create --
   ------------
   function Create (Vertex_List : Point_Array_Type) return Polygon_Type is
      use Points;
      Result : Polygon_Type := Polygon_Type'(
        Number_Of_Vertices => Vertex_List'Length,
        Vertices           => Vertex_List
      );
   begin
      for I in Result.Vertices'Range loop
         if Is_Zero (Result.Vertices (I)) then
            raise Constraint_Error;
         end if;

         Reduce (Result.Vertices (I));
      end loop;

      return Result;
   end Create;

   ----------------
   -- Get_Vertex --
   ----------------
   function Get_Vertex (
     Position : Integer;
     Polygon  : Polygon_Type
   ) return Points.Point_Type is
   begin
      return Polygon.Vertices (
        (Position - 1) mod Polygon.Number_Of_Vertices + 1
      );
   end Get_Vertex;

   --------------
   -- Is_Empty --
   --------------
   function Is_Empty (Polygon : Polygon_Type) return Boolean is
   begin
      return Polygon.Number_Of_Vertices = 0;
   end Is_Empty;

   ----------------
   -- Is_Polygon --
   ----------------
   function Is_Polygon (
     Strict  : Boolean;
     Polygon : Polygon_Type
   ) return Boolean is
      use Points;
      use Integers;

      X, Y, Z : Point_Type;
      DP : Integer_Type;
   begin
      if Is_Empty (Polygon) then
         return True;
      elsif Polygon.Number_Of_Vertices < 3 then
         return False;
      end if;

      X := Polygon.Vertices (Polygon.Number_Of_Vertices);
      for I in Polygon.Vertices'Range loop
         Y := Polygon.Vertices (I);
         if Equivalent (X, Y) or Equivalent (X, Negate (Y)) then
            return False;
         end if;
         Z := Cross_Product (X, Y);
         X := Y;
         DP := Dot_Product (Get_Vertex (I + 1, Polygon), Z);
         if Is_Negative (DP) or (Strict and Is_Zero (DP)) then
            return False;
         end if;
      end loop;

      return True;
   end Is_Polygon;

   -------------------------
   -- Reverse_Orientation --
   -------------------------
   procedure Reverse_Orientation (Polygon : in out Polygon_Type) is
      Old_Vertices : constant Point_Array_Type := Polygon.Vertices;

      Target : Natural := Polygon.Number_Of_Vertices;
   begin
      for I in Polygon.Vertices'First .. Polygon.Vertices'Last - 1 loop
         Target := Target - 1;
         Polygon.Vertices (Target) := Old_Vertices (I);
      end loop;
   end Reverse_Orientation;

   ----------------
   -- Set_Vertex --
   ----------------
   procedure Set_Vertex (
     Position : in     Integer;
     Value    : in     Points.Point_Type;
     Polygon  : in out Polygon_Type
   ) is
      use Points;
      Reduced_Value : Point_Type := Value;
   begin
      if Is_Zero (Value) then
         raise Constraint_Error;
      end if;

      Reduce (Reduced_Value);

      Polygon.Vertices (
        (Position - 1) mod Polygon.Number_Of_Vertices + 1
      ) := Reduced_Value;
   end Set_Vertex;
end Polygons;
