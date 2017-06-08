with Yada.Strings.Chunk_Test;

package body Yada.Strings.Suite is
   Result : aliased AUnit.Test_Suites.Test_Suite;
   Chunk_TC : aliased Chunk_Test.TC;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   begin
      AUnit.Test_Suites.Add_Test (Result'Access, Chunk_TC'Access);
      return Result'Access;
   end Suite;
end Yada.Strings.Suite;
