with Ada.Unchecked_Deallocation;

package body Partition is
   First_Element : Element_Pointer;

   procedure Free is new Ada.Unchecked_Deallocation (
     Object => Element_Type,
     Name   => Element_Pointer
   );

   -----------------
   -- Add_Element --
   -----------------
   procedure Add_Element (Element : Element_Type) is
      Current_Element : Element_Pointer := First_Element;
      Next_Element    : Element_Pointer;

      Symbols : Sequence_Type renames Element.Symbols;
   begin
      if Current_Element = null then
         First_Element := new Element_Type '(Element);
         First_Element.Next := null;
         return;
      end if;

      if Current_Element.Depth /= Element.Depth then
         raise Partition_Error;
      end if;

      if Symbols <= Current_Element.Symbols then
         if Symbols = Current_Element.Symbols then
            raise Partition_Error;
         end if;
         First_Element := new Element_Type '(Element);
         First_Element.Next := Current_Element;
         return;
      end if;

      loop
         Next_Element := Current_Element.Next;

         if Next_Element = null then
            Current_Element.Next := new Element_Type '(Element);
            Current_Element.Next.Next := null;
            return;
         end if;

         declare
            Next_Sequence : Sequence_Type renames Next_Element.Symbols;
         begin
            if Symbols <= Next_Sequence then
               if Symbols = Next_Sequence then
                  raise Partition_Error;
               end if;
               Current_Element.Next := new Element_Type '(Element);
               Current_Element.Next.Next := Next_Element;
               return;
            end if;
         end;

         Current_Element := Next_Element;
      end loop;
   end Add_Element;

   --------------------
   -- Create_Element --
   --------------------
   function Create_Element (
     Polygon : Polygons.Polygon_Type;
     Symbols : Sequence_Type
   ) return Element_Type is
   begin
      return (
        Depth   => Symbols'Length,
        Size    => Polygon.Number_Of_Vertices,
        Next    => null,
        Polygon => Polygon,
        Symbols => Symbols
      );
   end Create_Element;

   -------------------------
   -- Delete_All_Elements --
   -------------------------
   procedure Delete_All_Elements is
      Current_Element : Element_Pointer := First_Element;
   begin
      First_Element := null;

      while Current_Element /= null loop
         declare
            Spare : Element_Pointer := Current_Element;
         begin
            Current_Element := Current_Element.Next;
            Free (Spare);
         end;
      end loop;
   end Delete_All_Elements;

   ---------------------
   -- Delete_Elements --
   ---------------------
   procedure Delete_Elements (Indices : Index_List) is
      Current_Element : Element_Pointer := First_Element;
      Current_Index   : Positive := 1;

      Delete_Position : Positive := Indices'First;

      Last_Undeleted : Element_Pointer;

      Next_Element : Element_Pointer;
   begin
      while Current_Element /= null and Delete_Position <= Indices'Last loop
         Next_Element := Current_Element.Next;

         if Indices (Delete_Position) = Current_Index then
            Free (Current_Element);
            Delete_Position := Delete_Position + 1;
         else
            if Last_Undeleted /= null then
               Last_Undeleted.Next := Current_Element;
            else
               First_Element := Current_Element;
            end if;
            Last_Undeleted := Current_Element;
         end if;

         Current_Element := Next_Element;
         Current_Index := Current_Index + 1;
      end loop;

      if Last_Undeleted /= null then
         Last_Undeleted.Next := Current_Element;
      else
         First_Element := Current_Element;
      end if;
   end Delete_Elements;

   ------------------------
   -- Element_Count --
   ------------------------
   --  Could maintain a count at all times, but is it worth it?
   --  Don't forget: Set_First would need calculate a new element count.
   function Element_Count return Natural is
      Count : Natural := 0;
      Current_Element : Element_Pointer := Get_First;
   begin
      while Current_Element /= null loop
         Count := Count + 1;
         Current_Element := Current_Element.Next;
      end loop;

      return Count;
   end Element_Count;

   ---------------
   -- Get_First --
   ---------------
   function Get_First return Element_Pointer is
   begin
      return First_Element;
   end Get_First;

   -----------------
   -- Get_Polygon --
   -----------------
   function Get_Polygon (Element : Element_Type) return Polygon_Type is
   begin
      return Element.Polygon;
   end Get_Polygon;

   -----------------
   -- Get_Symbols --
   -----------------
   function Get_Symbols (Element : Element_Type) return Sequence_Type is
   begin
      return Element.Symbols;
   end Get_Symbols;

   ---------------
   -- Set_First --
   ---------------
   procedure Set_First (First_Element : Element_Pointer) is
   begin
      Partition.First_Element := First_Element;
   end Set_First;
end Partition;
