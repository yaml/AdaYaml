with AUnit; use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

package Yada.Lexing.Tokenization_Test is
   type TC is new Test_Cases.Test_Case with null record;

   overriding procedure Register_Tests (T : in out TC);

   function Name (T : TC) return Message_String;

   procedure Empty_Document (T : in out Test_Cases.Test_Case'Class);
   procedure Single_Line_Scalar (T : in out Test_Cases.Test_Case'Class);
   procedure Multiline_Scalar (T : in out Test_Cases.Test_Case'Class);
   procedure Single_Line_Mapping (T : in out Test_Cases.Test_Case'Class);
   procedure Multiline_Mapping (T : in out Test_Cases.Test_Case'Class);
   procedure Explicit_Mapping (T : in out Test_Cases.Test_Case'Class);
   procedure Sequence (T : in out Test_Cases.Test_Case'Class);
   procedure Single_Quoted_Scalar (T : in out Test_Cases.Test_Case'Class);
   procedure Multiline_Single_Quoted_Scalar (T : in out Test_Cases.Test_Case'Class);
   procedure Double_Quoted_Scalar (T : in out Test_Cases.Test_Case'Class);
   procedure Multiline_Double_Quoted_Scalar (T : in out Test_Cases.Test_Case'Class);
   procedure Escape_Sequences (T : in out Test_Cases.Test_Case'Class);

   procedure Block_Scalar (T : in out Test_Cases.Test_Case'Class);
   procedure Block_Scalars (T : in out Test_Cases.Test_Case'Class);
end Yada.Lexing.Tokenization_Test;
