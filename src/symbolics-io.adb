with Ada.Text_IO;

package body Symbolics.IO is
   package Symbol_IO is new Ada.Text_IO.Enumeration_IO (Symbol_Type);
   use Symbol_IO;

   procedure Put (Symbols : Sequence_Type) is
   begin
      for I in Symbols'Range loop
         Put (Symbols (I), Width => 0);
      end loop;
   end Put;
end Symbolics.IO;
