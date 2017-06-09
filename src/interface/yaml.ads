
package Yaml is
   pragma Pure;

   Lexer_Error : exception;
   Parser_Error : exception;

   type Mark is record
      Index, Line, Column : Positive;
   end record;
end Yaml;
