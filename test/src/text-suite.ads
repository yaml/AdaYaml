--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with AUnit.Test_Suites;
package Text.Suite is
   function Suite return AUnit.Test_Suites.Access_Test_Suite;
end Text.Suite;
