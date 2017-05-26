with Yada.Lexing.Buffering_Test;

package body Yada.Lexing.Suite is
   Result : aliased AUnit.Test_Suites.Test_Suite;
   Buffering_TC : aliased Buffering_Test.TC;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   begin
      AUnit.Test_Suites.Add_Test (Result'Access, Buffering_TC'Access);
      return Result'Access;
   end Suite;
end Yada.Lexing.Suite;
