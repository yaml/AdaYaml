--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Yaml.Lexer.Buffering_Test;
with Yaml.Lexer.Tokenization_Test;
with Yaml.Parser.Event_Test;

package body Yaml.Loading_Tests.Suite is
   Result : aliased AUnit.Test_Suites.Test_Suite;
   Buffering_TC : aliased Lexer.Buffering_Test.TC;
   Tokenization_TC : aliased Lexer.Tokenization_Test.TC;
   Event_TC : aliased Parser.Event_Test.TC;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   begin
      AUnit.Test_Suites.Add_Test (Result'Access, Buffering_TC'Access);
      AUnit.Test_Suites.Add_Test (Result'Access, Tokenization_TC'Access);
      AUnit.Test_Suites.Add_Test (Result'Access, Event_TC'Access);
      return Result'Access;
   end Suite;
end Yaml.Loading_Tests.Suite;
