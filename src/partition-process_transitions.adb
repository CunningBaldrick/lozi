with Intersects_Halfspace;
with Lozi;

procedure Partition.Process_Transitions is
   use Lozi;
   use Vertices;

   Previous_Element : Element_Pointer := Get_First;
   Previous_Index   : Extended_Vertex_Number := 1;

   procedure Find_Target (
     Symbol_Sequence : in     Sequence_Type;
     Target_Element  :    out Element_Pointer;
     Target_Index    :    out Vertex_Number
   );
   --  Finds the first element with symbol sequence greater than or equal to
   --  Symbol_Sequence.  If there is none, then the first element is returned.

   procedure Find_Target (
     Symbol_Sequence : in     Sequence_Type;
     Target_Element  :    out Element_Pointer;
     Target_Index    :    out Vertex_Number
   ) is
   begin
      --  This search method is intended to be efficient if the
      --  partition elements are kept in order.

      if Symbol_Sequence < Get_Symbols (Previous_Element.all) then
         Previous_Element := Get_First;
         Previous_Index   := 1;
      end if;

      while Previous_Element /= null loop
         if Get_Symbols (Previous_Element.all) >= Symbol_Sequence then
            Target_Element := Previous_Element;
            Target_Index   := Previous_Index;
            return;
         end if;
         Previous_Element := Previous_Element.Next;
         Previous_Index   := Previous_Index + 1;
      end loop;

      Previous_Element := Get_First;
      Previous_Index   := 1;

      Target_Element := Previous_Element;
      Target_Index   := Previous_Index;
   end Find_Target;

   Current_Element : Element_Pointer := Get_First;
   Current_Index   : Vertex_Number := 1;
begin
   while Current_Element /= null loop
      declare
         Symbol_Sequence : constant Sequence_Type :=
           Get_Symbols (
             Current_Element.all
           );
         Cut_Sequence : constant Sequence_Type :=
           Symbol_Sequence (
             Symbol_Sequence'First + 1 .. Symbol_Sequence'Last
           );
         Left_Sequence  : constant Sequence_Type := Cut_Sequence & L;
         Right_Sequence : constant Sequence_Type := Cut_Sequence & R;
         Left_Element   : Element_Pointer;
         Right_Element  : Element_Pointer;
         Left_Index     : Vertex_Number;
         Right_Index    : Vertex_Number;
         Left_Exists    : Boolean;
         Right_Exists   : Boolean;
      begin
         Find_Target (Left_Sequence, Left_Element, Left_Index);
         Left_Exists := Left_Sequence = Get_Symbols (Left_Element.all);

         Find_Target (Right_Sequence, Right_Element, Right_Index);
         Right_Exists := Right_Sequence = Get_Symbols (Right_Element.all);

         if Left_Exists or Right_Exists then
            declare
               Current_Polygon : constant Polygon_Type := Get_Polygon (
                 Current_Element.all
               );
               Image_Polygon : Polygon_Type := Current_Polygon;
               Left_Intersects, Right_Intersects : Boolean;
            begin
               Lozi.Map (Image_Polygon);

               Intersects_Halfspace (
                 Image_Polygon,
                 Left_Halfspace,
                 Left_Intersects,
                 Right_Intersects
               );

               if Left_Exists and Left_Intersects then
                  Action (
                    From_Polygon => Current_Polygon,
                    From_Index   => Current_Index,
                    From_Image   => Image_Polygon,
                    To_Polygon   => Get_Polygon (Left_Element.all),
                    To_Index     => Left_Index,
                    Total_Symbol => Symbol_Sequence & L
                  );
               end if;

               if Right_Exists and Right_Intersects then
                  Action (
                    From_Polygon => Current_Polygon,
                    From_Index   => Current_Index,
                    From_Image   => Image_Polygon,
                    To_Polygon   => Get_Polygon (Right_Element.all),
                    To_Index     => Right_Index,
                    Total_Symbol => Symbol_Sequence & R
                  );
               end if;
            end;
         end if;
      end;

      Current_Element := Current_Element.Next;
      Current_Index   := Current_Index + 1;
   end loop;
end Partition.Process_Transitions;
