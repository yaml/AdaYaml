--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Yaml.Dom.Dumping.Test;

package body Yaml.Dumping_Tests.Suite is
   Result : aliased AUnit.Test_Suites.Test_Suite;
   Dom_Dumping_TC : aliased Dom.Dumping.Test.TC;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   begin
      AUnit.Test_Suites.Add_Test (Result'Access, Dom_Dumping_TC'Access);
      return Result'Access;
   end Suite;
end Yaml.Dumping_Tests.Suite;
