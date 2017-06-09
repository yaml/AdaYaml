with AUnit; use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

package Yaml.Lexing.Buffering_Test is
   type TC is new Test_Cases.Test_Case with record
      Pool : Strings.String_Pool;
   end record;

   overriding procedure Register_Tests (T : in out TC);
   overriding procedure Set_Up (T : in out TC);

   function Name (T : TC) return Message_String;

   procedure Test_File_Without_Refill (T : in out Test_Cases.Test_Case'Class);
   procedure Test_File_With_Single_Refill (T : in out Test_Cases.Test_Case'Class);
end Yaml.Lexing.Buffering_Test;
