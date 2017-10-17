--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Yaml.Annotation_Test;

package body Yaml.Transformation_Tests.Suite is
   Result : aliased AUnit.Test_Suites.Test_Suite;
   Annotation_TC : aliased Annotation_Test.TC;

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   begin
      AUnit.Test_Suites.Add_Test (Result'Access, Annotation_TC'Access);
      return Result'Access;
   end Suite;
end Yaml.Transformation_Tests.Suite;
