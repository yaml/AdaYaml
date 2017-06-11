with Interfaces.C;

package Yaml is
   pragma Pure;

   Lexer_Error : exception;
   Parser_Error : exception;

   subtype Mark_Position is Interfaces.C.size_t
     range 1 .. Interfaces.C.size_t'Last;

   type Mark is record
      Index, Line, Column : Mark_Position;
   end record with Convention => C;
end Yaml;
