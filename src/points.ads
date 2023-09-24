with Integers;

package Points is
   pragma Elaborate_Body;

   type Coordinate_Type is (x, y, z);

   type Point_Type is private;
   --  A point in R^3 with integer coordinates.

   function Is_Zero (Right : Point_Type) return Boolean;

   function "="  (Left, Right : Point_Type) return Boolean is abstract;
   --  NOT defined so as to be sure we never use "=" when we mean Equivalent.

   function Equivalent (Left, Right : Point_Type) return Boolean;
   --  True if Left is a positive (not necessarily integer) multiple of Right.

   function "+"  (Left, Right : Point_Type) return Point_Type;
   pragma Inline ("+");

   function "-"  (Left, Right : Point_Type) return Point_Type;
   pragma Inline ("-");

   function Create (
     X_Value : Integers.Integer_Type;
     Y_Value : Integers.Integer_Type;
     Z_Value : Integers.Integer_Type
   ) return Point_Type;
   pragma Inline (Create);

   function Cross_Product (Left, Right : Point_Type) return Point_Type;
   pragma Inline (Cross_Product);

   function Dot_Product (Left, Right : Point_Type)
     return Integers.Integer_Type;
   pragma Inline (Dot_Product);

   function Get_Component (
     Coordinate : Coordinate_Type;
     Point      : Point_Type
   ) return Integers.Integer_Type;
   pragma Inline (Get_Component);

   function Linear_Combination (
     Multiplier1 : Integers.Integer_Type;
     Point1      : Point_Type;
     Multiplier2 : Integers.Integer_Type;
     Point2      : Point_Type
   ) return Point_Type;
   pragma Inline (Linear_Combination);
   --  Returns Multiplier1 * Point1 + Multiplier2 * Point2.

   function Negate  (Right : Point_Type) return Point_Type;
   pragma Inline (Negate);

   procedure Reduce (Point : in out Point_Type);
   --  Replaces a point with the shortest equivalent one.

   procedure Set_Components (
     X_Value : in     Integers.Integer_Type;
     Y_Value : in     Integers.Integer_Type;
     Z_Value : in     Integers.Integer_Type;
     Point   : in out Point_Type
   );
   pragma Inline (Set_Components);
private
   type Point_Type is array (Coordinate_Type) of Integers.Integer_Type;
   pragma Pack (Point_Type);
end Points;
