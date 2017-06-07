private package Yada.Lexing.Scalars is
   procedure Read_Plain_Scalar (L : in out Lexer);

   procedure Read_Single_Quoted_Scalar (L : in out Lexer)
     with Pre => L.Cur = '"';

   procedure Read_Double_Quoted_Scalar (L : in out Lexer)
     with Pre => L.Cur = '"';

   procedure Read_Block_Scalar (L : in out Lexer)
     with Pre => L.Cur in '|' | '>';
end Yada.Lexing.Scalars;
