--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

package Yaml is
   pragma Pure;

   --  occurs when the lexical analysis of a YAML character streams discovers
   --  ill-formed input.
   Lexer_Error : exception;

   --  occurs when the syntactic analysis of a YAML token stream discovers an
   --  ill-formed input.
   Parser_Error : exception;

   --  occurs when an ill-formed event stream is tried to be presented.
   Presenter_Error : exception;

   --  occurs when data cannot be written to a destination.
   Destination_Error : exception;

   --  all positions in a mark start at 1
   subtype Mark_Position is Positive;

   --  a position in the input stream.
   type Mark is record
      Index, Line, Column : Mark_Position;
   end record with Convention => C;

   --  the version of the library. major and minor version correspond to the
   --  YAML version, the patch version is local to this implementation.
   function Version_Major return Natural with Inline;
   function Version_Minor return Natural with Inline;
   function Version_Patch return Natural with Inline;
end Yaml;
