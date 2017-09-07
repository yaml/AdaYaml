--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

private package Yaml.Lexer.Evaluation is
   procedure Read_Plain_Scalar (L : in out Instance; T : out Token);

   procedure Read_Single_Quoted_Scalar (L : in out Instance; T : out Token)
     with Pre => L.Cur = ''';

   procedure Read_Double_Quoted_Scalar (L : in out Instance; T : out Token)
     with Pre => L.Cur = '"';

   procedure Read_Block_Scalar (L : in out Instance; T : out Token)
     with Pre => L.Cur in '|' | '>';

   procedure Read_URI (L : in out Instance; Restricted : Boolean);
end Yaml.Lexer.Evaluation;
