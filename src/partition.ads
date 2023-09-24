with Polygons; use Polygons;
with Symbolics; use Symbolics;

--  Maintains a list of partition elements.  They need not actually form a
--  partition of the space.  There must be at most one element with a given
--  symbol sequence.
package Partition is
   pragma Elaborate_Body;

   Partition_Error : exception;

   type Element_Type (Depth : Positive; Size  : Natural) is private;
   --  A partition element: a convex polygon and its symbolic sequence.
   --  Size is the number of vertices of the polygon.  Depth is the
   --  length of the symbolic sequence.

   function "=" (Left, Right : Element_Type) return Boolean is abstract;
   --  Equality is NOT implemented.

   function Create_Element (
     Polygon : Polygon_Type;
     Symbols : Sequence_Type
   ) return Element_Type;
   pragma Inline (Create_Element);

   function Get_Polygon (Element : Element_Type) return Polygon_Type;
   pragma Inline (Get_Polygon);

   function Get_Symbols (Element : Element_Type) return Sequence_Type;
   pragma Inline (Get_Symbols);

   procedure Add_Element (Element : Element_Type);
   --  Raises Partition_Error if the list already contains an element with
   --  the given symbolic sequence, or a symbolic sequence of a different
   --  length.

   procedure Delete_All_Elements;

   type Index_List is array (Natural range <>) of Positive;

   procedure Delete_Elements (Indices : Index_List);
   --  The elements to be deleted, listed in increasing order.  Elements
   --  are numbered starting from 1.

   function Element_Count return Natural;
   pragma Inline (Element_Count);
   --  The number of elements in the partition.
private
   type Element_Pointer is access Element_Type;

   type Element_Type (Depth : Positive; Size : Natural) is record
      Next    : Element_Pointer;
      Polygon : Polygon_Type (Size);
      Symbols : Sequence_Type (1 .. Depth);
   end record;

   function Get_First return Element_Pointer;
   pragma Inline (Get_First);

   procedure Set_First (First_Element : Element_Pointer);
   pragma Inline (Set_First);
end Partition;
