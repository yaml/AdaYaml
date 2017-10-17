--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with AUnit; use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;
with Ada.Containers.Indefinite_Vectors;

package Yaml.Annotation_Test is
   subtype Test_Case_Name is String (1 .. 4);
   package Test_Case_Vectors is new Ada.Containers.Indefinite_Vectors (Positive, Test_Case_Name);

   type TC is new Test_Cases.Test_Case with record
      Test_Cases : Test_Case_Vectors.Vector;
      Cur : Positive;
   end record;

   overriding procedure Register_Tests (T : in out TC);

   function Name (T : TC) return Message_String;

   procedure Execute_Next_Test (T : in out Test_Cases.Test_Case'Class);
   procedure Execute_Error_Test (T : in out Test_Cases.Test_Case'Class);
end Yaml.Annotation_Test;
