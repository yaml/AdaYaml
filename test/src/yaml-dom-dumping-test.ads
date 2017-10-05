--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with AUnit; use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

package Yaml.Dom.Dumping.Test is
   type TC is new Test_Cases.Test_Case with record
      Pool : Text.Pool.Reference;
   end record;

   overriding procedure Register_Tests (T : in out TC);
   overriding procedure Set_Up (T : in out TC);

   function Name (T : TC) return Message_String;

   procedure Plain_Scalar_Document (T : in out Test_Cases.Test_Case'Class);
   procedure Quoted_Scalar_Document (T : in out Test_Cases.Test_Case'Class);
   procedure Explicit_Document (T : in out Test_Cases.Test_Case'Class);
end Yaml.Dom.Dumping.Test;
