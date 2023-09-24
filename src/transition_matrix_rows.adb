with Ada.Unchecked_Deallocation;

package body Transition_Matrix_Rows is

   Minimum_Heap_Size : constant := 10;

   procedure Free is new Ada.Unchecked_Deallocation (Column_List, Column_List_Pointer);

   -------------------------------
   -- Ensure_Heap_Is_Big_Enough --
   -------------------------------

   procedure Ensure_Heap_Is_Big_Enough (
     Row   : in out Matrix_Row;
     Index : in     Positive
   );
   --  Ensure that On_Heap contains the elements 1 .. Index by enlarging it if
   --  necessary.

   procedure Ensure_Heap_Is_Big_Enough (
     Row   : in out Matrix_Row;
     Index : in     Positive
   ) is
      Old_Heap : Column_List_Pointer := Row.On_Heap;
   begin
      if Old_Heap = null or else Old_Heap'Last < Index then
         Row.On_Heap := new Column_List (1 .. Positive'Max (2 * Index, Minimum_Heap_Size));
         --  Copy old values into new storage.
         for I in 1 .. Row.Non_Zeros - Row.On_Stack'Last loop
            Row.On_Heap (I) := Old_Heap (I);
         end loop;
         Free (Old_Heap);
      end if;
   end Ensure_Heap_Is_Big_Enough;


   -------------------
   -- Get_Last_Used --
   -------------------

   procedure Get_Last_Used (
     Row   : in     Matrix_Row;
     Stack :    out Natural;
     Heap  :    out Natural
   );
   pragma Inline (Get_Last_Used);
   --  Calculate the last elements of on-stack and on-heap storage used by the
   --  row (or zero if none used).

   procedure Get_Last_Used (
     Row   : in     Matrix_Row;
     Stack :    out Natural;
     Heap  :    out Natural
   ) is
   begin
      Stack := Natural'Min (Row.Non_Zeros, Row.On_Stack'Last);
      Heap  := Integer'Max (Row.Non_Zeros - Row.On_Stack'Last, 0);
   end Get_Last_Used;


   ---------------
   -- Clear_All --
   ---------------

   procedure Clear_All (Row : in out Matrix_Row) is
   begin
      Free (Row.On_Heap);
      Row.Non_Zeros := 0;
   end Clear_All;


   ------------------
   -- Clear_Column --
   ------------------

   procedure Clear_Column (
     Row    : in out Matrix_Row;
     Column : in     Positive
   ) is
      Stack_Last, Heap_Last : Natural;
   begin
      Get_Last_Used (Row, Stack => Stack_Last, Heap => Heap_Last);

      --  Look for the column on the stack.
      for I in 1 .. Stack_Last loop
         if Row.On_Stack (I) > Column then
            --  Not set.
            return;
         end if;

         if Row.On_Stack (I) = Column then
            --  Delete the column from the list.
            for J in I + 1 .. Stack_Last loop
               Row.On_Stack (J - 1) := Row.On_Stack (J);
            end loop;
            if Heap_Last > 0 then
               Row.On_Stack (Row.On_Stack'Last) := Row.On_Heap (1);
               if Heap_Last > 1 then
                  for J in 2 .. Heap_Last loop
                     Row.On_Heap (J - 1) := Row.On_Heap (J);
                  end loop;
               else
                  Free (Row.On_Heap);
               end if;
            end if;
            Row.Non_Zeros := Row.Non_Zeros - 1;
            return;
         end if;
      end loop;

      --  Look for the column on the heap.
      for I in 1 .. Heap_Last loop
         if Row.On_Heap (I) > Column then
            --  Not set.
            return;
         end if;

         if Row.On_Heap (I) = Column then
            --  Delete the column from the list.
            if Heap_Last > 1 then
               for J in I + 1 .. Heap_Last loop
                  Row.On_Heap (J - 1) := Row.On_Heap (J);
               end loop;
            else
               Free (Row.On_Heap);
            end if;
            Row.Non_Zeros := Row.Non_Zeros - 1;
            return;
         end if;
      end loop;
   end Clear_Column;


   -------------------
   -- Column_Is_Set --
   -------------------

   function Column_Is_Set (
     Row    : Matrix_Row;
     Column : Positive
   ) return Boolean is
      Stack_Last, Heap_Last : Natural;
   begin
      Get_Last_Used (Row, Stack => Stack_Last, Heap => Heap_Last);

      --  Look for the column on the stack.
      for I in 1 .. Stack_Last loop
         if Row.On_Stack (I) > Column then
            return False;
         end if;

         if Row.On_Stack (I) = Column then
            return True;
         end if;
      end loop;

      --  Look for the column on the heap.
      for I in 1 .. Heap_Last loop
         if Row.On_Heap (I) > Column then
            return False;
         end if;

         if Row.On_Heap (I) = Column then
            return True;
         end if;
      end loop;

      return False;
   end Column_Is_Set;


   ----------------
   -- Set_Column --
   ----------------

   procedure Set_Column (
     Row    : in out Matrix_Row;
     Column : in     Positive
   ) is
      Stack_Last, Heap_Last : Natural;
   begin
      Get_Last_Used (Row, Stack => Stack_Last, Heap => Heap_Last);

      --  Look for the column on the stack.
      for I in 1 .. Stack_Last loop
         if Row.On_Stack (I) = Column then
            --  Already set.
            return;
         end if;

         if Row.On_Stack (I) > Column then
            --  Insert the column before this list element.
            if Stack_Last = Row.On_Stack'Last then
               --  Move the last stack element to the heap.
               Ensure_Heap_Is_Big_Enough (Row, Heap_Last + 1);
               for J in reverse 1 .. Heap_Last loop
                  Row.On_Heap (J + 1) := Row.On_Heap (J);
               end loop;
               Row.On_Heap (1) := Row.On_Stack (Row.On_Stack'Last);
               for J in reverse I .. Row.On_Stack'Last - 1 loop
                  Row.On_Stack (J + 1) := Row.On_Stack (J);
               end loop;
            else
               for J in reverse I .. Stack_Last loop
                  Row.On_Stack (J + 1) := Row.On_Stack (J);
               end loop;
            end if;
            Row.On_Stack (I) := Column;
            Row.Non_Zeros := Row.Non_Zeros + 1;
            return;
         end if;
      end loop;

      --  Look for the column on the heap.
      for I in 1 .. Heap_Last loop
         if Row.On_Heap (I) = Column then
            --  Already set.
            return;
         end if;

         if Row.On_Heap (I) > Column then
            --  Insert the column before this list element.
            Ensure_Heap_Is_Big_Enough (Row, Heap_Last + 1);
            for J in reverse I .. Heap_Last loop
               Row.On_Heap (J + 1) := Row.On_Heap (J);
            end loop;
            Row.On_Heap (I) := Column;
            Row.Non_Zeros := Row.Non_Zeros + 1;
            return;
         end if;
      end loop;

      --  Append the column to the end of the list.
      if Stack_Last < Row.On_Stack'Last then
         --  It can be stored on the stack.
         Row.On_Stack (Stack_Last + 1) := Column;
      else
         --  It must be stored on the heap.
         Ensure_Heap_Is_Big_Enough (Row, Heap_Last + 1);
         Row.On_Heap (Heap_Last + 1) := Column;
      end if;
      Row.Non_Zeros := Row.Non_Zeros + 1;
   end Set_Column;


   ----------------------------
   -- Visit_Non_Zero_Columns --
   ----------------------------

   procedure Visit_Non_Zero_Columns (Row : Matrix_Row) is
      Stack_Last, Heap_Last : Natural;
      Stop : Boolean := False;
   begin
      Get_Last_Used (Row, Stack => Stack_Last, Heap => Heap_Last);
      for I in 1 .. Stack_Last loop
         Action (Row.On_Stack (I), Stop);
         if Stop then
            return;
         end if;
      end loop;
      for I in 1 .. Heap_Last loop
         Action (Row.On_Heap (I), Stop);
         if Stop then
            return;
         end if;
      end loop;
   end Visit_Non_Zero_Columns;


   ----------------
   -- Get_Column --
   ----------------

   function Get_Column (
     Row      : Matrix_Row;
     Position : Cursor
   ) return Positive is
   begin
      if Position > 0 then
         return Row.On_Stack (Positive (Position));
      elsif Position < 0 then
         return Row.On_Heap (Positive (-Position));
      else
         raise Constraint_Error with "Invalid column";
      end if;
   end Get_Column;


   ---------------
   -- Get_First --
   ---------------

   function Get_First (Row : Matrix_Row) return Cursor is
      Stack_Last, Heap_Last : Natural;
   begin
      Get_Last_Used (Row, Stack => Stack_Last, Heap => Heap_Last);
      if Stack_Last > 0 then
         return 1;
      elsif Heap_Last > 0 then
         return -1;
      else
         return 0;
      end if;
   end Get_First;


   --------------
   -- Get_Next --
   --------------

   function Get_Next (
     Row      : Matrix_Row;
     Position : Cursor
   ) return Cursor is
      Stack_Last, Heap_Last : Natural;
   begin
      Get_Last_Used (Row, Stack => Stack_Last, Heap => Heap_Last);
      if Position > 0 then
         if Positive (Position) < Stack_Last then
            return Position + 1;
         elsif Heap_Last > 0 then
            return -1;
         else
            return 0;
         end if;
      elsif Position < 0 then
         if Heap_Last > Positive (-Position) then
            return Position - 1;
         else
            return 0;
         end if;
      else
         raise Constraint_Error with "Invalid column";
      end if;
   end Get_Next;


   ----------------
   -- Has_Column --
   ----------------

   function Has_Column (Position : Cursor) return Boolean is
   begin
      return Position /= 0;
   end Has_Column;

end Transition_Matrix_Rows;
