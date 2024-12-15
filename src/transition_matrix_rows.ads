with Vertices;

package Transition_Matrix_Rows is
   pragma Elaborate_Body;

   type Matrix_Row is limited private;
   --  A row of a transition matrix.  Each row entry is either zero or one.

   procedure Clear_All (Row : in out Matrix_Row);
   pragma Inline (Clear_All);
   --  Reset all row entries to zero.  While there is no need to call this
   --  before using a row for the first time, it MUST BE CALLED when finished
   --  with a row in order to free memory - MEMORY IS NOT AUTOMATICALLY FREED.

   procedure Clear_Column (
     Row    : in out Matrix_Row;
     Column : in     Vertices.Vertex_Number
   );
   --  Set the row entry for this column to zero.

   procedure Set_Column (
     Row    : in out Matrix_Row;
     Column : in     Vertices.Vertex_Number
   );
   --  Set the row entry for this column to one.

   function Column_Is_Set (
     Row    : Matrix_Row;
     Column : Vertices.Vertex_Number
   ) return Boolean;

   generic
      with procedure Action (
        Column : in     Vertices.Vertex_Number;
        Stop   : in out Boolean
      );
   procedure Visit_Non_Zero_Columns (Row : Matrix_Row);
   --  Call Action with the column number of each non-zero row entry.  If the
   --  user sets Stop to True then any remaining columns are skipped.  Columns
   --  are visited in column order (column 1 before column 2 etc).


   type Cursor is private;
   --  A pointer to a column.

   function Get_Column (
     Row      : Matrix_Row;
     Position : Cursor
   ) return Vertices.Vertex_Number;
   pragma Inline (Get_Column);
   --  Return the column the cursor is currently pointing to.

   function Get_First (Row : Matrix_Row) return Cursor;
   --  Get a cursor pointing to the first non-zero column.

   function Get_Next (
     Row      : Matrix_Row;
     Position : Cursor
   ) return Cursor;
   --  Advance the cursor to the next non-zero-column.  Columns
   --  are visited in column order (column 1 before column 2 etc).

   function Has_Column (Position : Cursor) return Boolean;
   pragma Inline (Has_Column);
   --  Whether the cursor is pointing to a valid column.

private

   type Column_List is array (Positive range <>) of Vertices.Vertex_Number;

   type Column_List_Pointer is access Column_List;

   Maximum_Transitions_On_Stack : constant := 6;

   subtype On_Stack_List is Column_List (1 .. Maximum_Transitions_On_Stack);

   type Matrix_Row is record
      Non_Zeros : Natural := 0;
      On_Stack  : On_Stack_List;
      On_Heap   : Column_List_Pointer; -- internally indexed from one
   end record;
   --  Implemented as an ordered list of column numbers with non-zero entries.
   --  It is rare to have more than a handful of non-zeros (three is already
   --  uncommon).  Optimize the common case by only allocating memory if the
   --  number of non-zeros exceeds Maximum_Transitions_On_Stack.

   type Cursor is new Integer;
   --  Positive if pointing to a stack element, negative if pointing to a heap
   --  element, and zero if not pointing to a valid column.

end Transition_Matrix_Rows;
