package Vertices is

   type Extended_Vertex_Number is new Natural;
   subtype Vertex_Number is Extended_Vertex_Number
     range 1 .. Extended_Vertex_Number'Last;

   type Vertex_List is array (Natural range <>) of Vertex_Number;

end Vertices;
