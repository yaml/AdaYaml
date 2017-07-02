--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Yaml.Parsing.Event_Test;

package body Yaml.Parsing.Suite is
   Result : aliased AUnit.Test_Suites.Test_Suite;
   Event_TC : aliased Event_Test.TC;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   begin
      AUnit.Test_Suites.Add_Test (Result'Access, Event_TC'Access);
      return Result'Access;
   end Suite;
end Yaml.Parsing.Suite;
