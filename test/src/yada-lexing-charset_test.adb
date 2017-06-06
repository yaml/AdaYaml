with AUnit.Assertions; use AUnit.Assertions;

package body Yada.Lexing.Charset_Test is
   procedure Register_Tests (T : in out TC) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine
        (T, Test_Line_Ends'Access, "Line end characters");
   end Register_Tests;

   function Name (T : TC) return Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Ada 2012 subtype tests for Lexer");
   end Name;

   procedure Test_Line_Ends (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      Assert (not ('a' in Line_End), "'a' is in Line_End!");
      Assert (End_Of_Input in Line_End, "End_Of_Input is not in Line_End!");
   end Test_Line_Ends;
end Yada.Lexing.Charset_Test;
