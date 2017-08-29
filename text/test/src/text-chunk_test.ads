--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with AUnit; use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;
with Text.Pool;

package Text.Chunk_Test is
   type TC is new Test_Cases.Test_Case with record
      P : Pool.Reference;
   end record;

   overriding procedure Register_Tests (T : in out TC);
   overriding procedure Set_Up (T : in out TC);

   function Name (T : TC) return Message_String;

   procedure Test_One_String (T : in out Test_Cases.Test_Case'Class);
   procedure Test_Two_Strings (T : in out Test_Cases.Test_Case'Class);
end Text.Chunk_Test;
