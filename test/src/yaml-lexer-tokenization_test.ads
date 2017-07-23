--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with AUnit; use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

package Yaml.Lexer.Tokenization_Test is
   type TC is new Test_Cases.Test_Case with record
      Pool : Text.Pool.Reference;
   end record;

   overriding procedure Register_Tests (T : in out TC);
   overriding procedure Set_Up (T : in out TC);

   function Name (T : TC) return Message_String;

   procedure Empty_Document (T : in out Test_Cases.Test_Case'Class);
   procedure Single_Line_Scalar (T : in out Test_Cases.Test_Case'Class);
   procedure Multiline_Scalar (T : in out Test_Cases.Test_Case'Class);
   procedure Single_Line_Mapping (T : in out Test_Cases.Test_Case'Class);
   procedure Multiline_Mapping (T : in out Test_Cases.Test_Case'Class);
   procedure Explicit_Mapping (T : in out Test_Cases.Test_Case'Class);
   procedure Sequence (T : in out Test_Cases.Test_Case'Class);
   procedure Sequence_With_Block_Mappings (T : in out Test_Cases.Test_Case'Class);
   procedure Single_Quoted_Scalar (T : in out Test_Cases.Test_Case'Class);
   procedure Multiline_Single_Quoted_Scalar (T : in out Test_Cases.Test_Case'Class);
   procedure Double_Quoted_Scalar (T : in out Test_Cases.Test_Case'Class);
   procedure Multiline_Double_Quoted_Scalar (T : in out Test_Cases.Test_Case'Class);
   procedure Escape_Sequences (T : in out Test_Cases.Test_Case'Class);
   procedure Block_Scalar (T : in out Test_Cases.Test_Case'Class);
   procedure Block_Scalars (T : in out Test_Cases.Test_Case'Class);
   procedure Directives (T : in out Test_Cases.Test_Case'Class);
   procedure Markers (T : in out Test_Cases.Test_Case'Class);
   procedure Flow_Indicators (T : in out Test_Cases.Test_Case'Class);
   procedure Adjacent_Map_Values (T : in out Test_Cases.Test_Case'Class);
   procedure Tag_Handles (T : in out Test_Cases.Test_Case'Class);
   procedure Verbatim_Tag_Handle (T : in out Test_Cases.Test_Case'Class);
   procedure Anchors_And_Aliases (T : in out Test_Cases.Test_Case'Class);
   procedure Empty_Lines (T : in out Test_Cases.Test_Case'Class);

end Yaml.Lexer.Tokenization_Test;
