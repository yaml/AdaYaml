with AUnit; use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

package Yada.Lexing.Charset_Test is
   type TC is new Test_Cases.Test_Case with null record;

   overriding procedure Register_Tests (T : in out TC);

   function Name (T : TC) return Message_String;

   procedure Test_Line_Ends (T : in out Test_Cases.Test_Case'Class);
end Yada.Lexing.Charset_Test;
