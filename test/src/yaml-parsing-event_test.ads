with AUnit; use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;
with Ada.Containers.Vectors;

package Yaml.Parsing.Event_Test is
   subtype Test_Case_Name is String (1 .. 4);
   package Test_Case_Vectors is new Ada.Containers.Vectors (Positive, Test_Case_Name);

   type TC is new Test_Cases.Test_Case with record
      Test_Cases : Test_Case_Vectors.Vector;
      Cur : Positive;
   end record;

   overriding procedure Register_Tests (T : in out TC);

   function Name (T : TC) return Message_String;

   procedure Execute_Next_Test (T : in out Test_Cases.Test_Case'Class);
end Yaml.Parsing.Event_Test;
