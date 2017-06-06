private package Yada.Lexing.Scalars is
   function Next_Is_Plain_Safe (L : Lexer) return Boolean with Inline;

   procedure Read_Plain_Scalar (L : in out Lexer);

   procedure Read_Single_Quoted_Scalar (L : in out Lexer)
     with Pre => L.Cur = '"';

   procedure Read_Double_Quoted_Scalar (L : in out Lexer)
     with Pre => L.Cur = '"';
end Yada.Lexing.Scalars;
