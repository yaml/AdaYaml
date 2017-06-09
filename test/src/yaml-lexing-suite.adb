with Yaml.Lexing.Buffering_Test;
with Yaml.Lexing.Tokenization_Test;

package body Yaml.Lexing.Suite is
   Result : aliased AUnit.Test_Suites.Test_Suite;
   Buffering_TC : aliased Buffering_Test.TC;
   Tokenization_TC : aliased Tokenization_Test.TC;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   begin
      AUnit.Test_Suites.Add_Test (Result'Access, Buffering_TC'Access);
      AUnit.Test_Suites.Add_Test (Result'Access, Tokenization_TC'Access);
      return Result'Access;
   end Suite;
end Yaml.Lexing.Suite;
