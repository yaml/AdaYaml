--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Ada.Finalization;
with Ada.Unchecked_Deallocation;
with AUnit.Assertions; use AUnit.Assertions;

package body Yaml.Lexer.Tokenization_Test is
   use type Text.Reference;

   subtype Evaluated_Token is Token_Kind with Static_Predicate =>
     Evaluated_Token in Scalar_Token_Kind | Suffix | Verbatim_Tag;
   subtype Short_Lexeme_Token is Token_Kind with Static_Predicate =>
     Short_Lexeme_Token in Unknown_Directive | Anchor | Alias;
   subtype Full_Lexeme_Token is Token_Kind with Static_Predicate =>
     Full_Lexeme_Token in Directive_Param | Tag_Handle;
   subtype Value_Token is Token_Kind with Static_Predicate =>
     Value_Token in Evaluated_Token | Short_Lexeme_Token | Full_Lexeme_Token;
   subtype Empty_Token is Token_Kind with Static_Predicate =>
     not (Empty_Token in Value_Token | Indentation);

   type Token_With_Value_Holder (Kind : Token_Kind) is record
      Refcount : Natural := 1;
      case Kind is
         when Value_Token => Value : Text.Reference;
         when Indentation => Depth : Natural;
         when others      => null;
      end case;
   end record;

   type Token_With_Value_Holder_Access is access Token_With_Value_Holder;

   type Token_With_Value is new Ada.Finalization.Controlled with record
      Reference : Token_With_Value_Holder_Access;
   end record;

   overriding procedure Adjust (Object : in out Token_With_Value);
   overriding procedure Finalize (Object : in out Token_With_Value);

   procedure Adjust (Object : in out Token_With_Value) is
   begin
      if Object.Reference /= null then
         Object.Reference.Refcount := Object.Reference.Refcount + 1;
      end if;
   end Adjust;

   procedure Finalize (Object : in out Token_With_Value) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Token_With_Value_Holder, Token_With_Value_Holder_Access);
      Reference : Token_With_Value_Holder_Access := Object.Reference;
   begin
      Object.Reference := null;
      if Reference /= null then
         Reference.Refcount := Reference.Refcount - 1;
         if Reference.Refcount = 0 then
            Free (Reference);
         end if;
      end if;
   end Finalize;

   function To_String (T : Token_With_Value) return String is
    (T.Reference.Kind'Img &
      (case T.Reference.Kind is
          when Value_Token => '(' & Escaped (T.Reference.Value) & ')',
          when Indentation => '(' & T.Reference.Depth'Img & ')',
          when others => ""));

   type Token_List is array (Positive range <>) of Token_With_Value;

   function TI (Indent : Natural) return Token_With_Value is
     (Token_With_Value'(Ada.Finalization.Controlled with Reference =>
                             new Token_With_Value_Holder'(Refcount => 1,
                                                          Kind => Indentation,
                                                          Depth => Indent)));
   function With_String (Tok : Value_Token; S : String;
                         T : in out Test_Cases.Test_Case'Class)
                         return Token_With_Value is
   begin
      return V : constant Token_With_Value :=
        (Ada.Finalization.Controlled with Reference => new Token_With_Value_Holder (Kind => Tok)) do
         V.Reference.Refcount := 1;
         V.Reference.Value := TC (T).Pool.From_String (S);
      end return;
   end With_String;

   function TPS (T : in out Test_Cases.Test_Case'Class;
                Content : String) return Token_With_Value is
     (With_String (Plain_Scalar, Content, T));
   function TSQS (T : in out Test_Cases.Test_Case'Class;
                  Content : String) return Token_With_Value is
     (With_String (Single_Quoted_Scalar, Content, T));
   function TDQS (T : in out Test_Cases.Test_Case'Class;
                  Content : String) return Token_With_Value is
     (With_String (Double_Quoted_Scalar, Content, T));
   function TLS (T : in out Test_Cases.Test_Case'Class;
                Content : String) return Token_With_Value is
     (With_String (Literal_Scalar, Content, T));
   function TFS (T : in out Test_Cases.Test_Case'Class;
                Content : String) return Token_With_Value is
     (With_String (Folded_Scalar, Content, T));
   TStrE : constant Token_With_Value :=
     (Ada.Finalization.Controlled with Reference =>
         new Token_With_Value_Holder'(Refcount => 1, Kind => Stream_End));
   TMK : constant Token_With_Value :=
     (Ada.Finalization.Controlled with Reference =>
         new Token_With_Value_Holder'(Refcount => 1, Kind => Map_Key_Ind));
   TMV : constant Token_With_Value :=
     (Ada.Finalization.Controlled with Reference =>
         new Token_With_Value_Holder'(Refcount => 1, Kind => Map_Value_Ind));
   TMS : constant Token_With_Value :=
     (Ada.Finalization.Controlled with Reference =>
         new Token_With_Value_Holder'(Refcount => 1, Kind => Flow_Map_Start));
   TME : constant Token_With_Value :=
     (Ada.Finalization.Controlled with Reference =>
         new Token_With_Value_Holder'(Refcount => 1, Kind => Flow_Map_End));
   TSep : constant Token_With_Value :=
     (Ada.Finalization.Controlled with Reference =>
         new Token_With_Value_Holder'(Refcount => 1, Kind => Flow_Separator));
   TSS : constant Token_With_Value :=
     (Ada.Finalization.Controlled with Reference =>
         new Token_With_Value_Holder'(Refcount => 1, Kind => Flow_Seq_Start));
   TSE : constant Token_With_Value :=
     (Ada.Finalization.Controlled with Reference =>
         new Token_With_Value_Holder'(Refcount => 1, Kind => Flow_Seq_End));
   TSI : constant Token_With_Value :=
     (Ada.Finalization.Controlled with Reference =>
         new Token_With_Value_Holder'(Refcount => 1, Kind => Seq_Item_Ind));
   TYD : constant Token_With_Value :=
     (Ada.Finalization.Controlled with Reference =>
         new Token_With_Value_Holder'(Refcount => 1, Kind => Yaml_Directive));
   TTD : constant Token_With_Value :=
     (Ada.Finalization.Controlled with Reference =>
         new Token_With_Value_Holder'(Refcount => 1, Kind => Tag_Directive));
   TDirE : constant Token_With_Value :=
     (Ada.Finalization.Controlled with Reference =>
         new Token_With_Value_Holder'(Refcount => 1, Kind => Directives_End));
   TDocE : constant Token_With_Value :=
     (Ada.Finalization.Controlled with Reference =>
         new Token_With_Value_Holder'(Refcount => 1, Kind => Document_End));

   function TDP (T : in out Test_Cases.Test_Case'Class; Content : String)
                 return Token_With_Value is
     (With_String (Directive_Param, Content, T));
   function TTH (T : in out Test_Cases.Test_Case'Class; Content : String)
                 return Token_With_Value is
     (With_String (Tag_Handle, Content, T));
   function TTV (T : in out Test_Cases.Test_Case'Class; Content : String)
                 return Token_With_Value is
     (With_String (Verbatim_Tag, Content, T));
   function TTU (T : in out Test_Cases.Test_Case'Class; Content : String)
                 return Token_With_Value is
     (With_String (Suffix, Content, T));
   function TUD (T : in out Test_Cases.Test_Case'Class; Content : String)
                 return Token_With_Value is
     (With_String (Unknown_Directive, Content, T));
   function TAn (T : in out Test_Cases.Test_Case'Class; Content : String)
                 return Token_With_Value is
     (With_String (Anchor, Content, T));
   function TAli (T : in out Test_Cases.Test_Case'Class; Content : String)
                  return Token_With_Value is
     (With_String (Alias, Content, T));

   function To_String (L : Instance; T : Token_Kind) return String is
     (T'Img & (case T is
         when Evaluated_Token => '(' & Escaped (L.Value) & ')',
         when Short_Lexeme_Token => '(' & Escaped (Short_Lexeme (L)) & ')',
         when Full_Lexeme_Token => '(' & Escaped (Full_Lexeme (L)) & ')',
         when Empty_Token => "",
         when Indentation => '(' & Natural'(L.Pos - L.Line_Start - 1)'Img & ')'));

   procedure Assert_Equals (P : Text.Pool.Reference;
                            Input : String; Expected : Token_List) is
      L : Instance;
      I : Natural := 0;
   begin
      Init (L, Input, P);
      for Expected_Token of Expected loop
         I := I + 1;
         declare
            T : constant Token := Next_Token (L);
         begin
            Assert (T.Kind = Expected_Token.Reference.Kind,
                    "Wrong token kind at #" & I'Img & ": Expected " &
                      To_String (Expected_Token) & ", got " &
                      To_String (L, T.Kind));
            if T.Kind = Expected_Token.Reference.Kind then
               case T.Kind is
               when Evaluated_Token =>
                  Assert (L.Value = Expected_Token.Reference.Value,
                          "Wrong content at #" & I'Img & ": Expected " &
                            Escaped (Expected_Token.Reference.Value) &
                            ", got " & Escaped (L.Value));
               when Full_Lexeme_Token =>
                  Assert (Full_Lexeme (L) = Expected_Token.Reference.Value,
                          "Wrong " & T.Kind'Img & " at #" & I'Img & ": Expected " &
                            Escaped (Expected_Token.Reference.Value) &
                            ", got " & Escaped (Full_Lexeme (L)));
               when Short_Lexeme_Token =>
                  Assert (Short_Lexeme (L) = Expected_Token.Reference.Value,
                          "Wrong " & T.Kind'Img & "at #" & I'Img & ": Expected " &
                            Escaped (Expected_Token.Reference.Value) &
                            ", got " & Escaped (Short_Lexeme (L)));
               when Indentation =>
                  Assert (L.Pos - L.Line_Start - 1 = Expected_Token.Reference.Depth,
                          "Wrong indentation at #" & I'Img & ": Expected" &
                            Expected_Token.Reference.Depth'Img & ", got " &
                            Integer'(L.Pos - L.Line_Start - 2)'Img);
               when Empty_Token => null;
               end case;
            end if;
         end;
      end loop;
   end Assert_Equals;

   procedure Register_Tests (T : in out TC) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Empty_Document'Access, "Empty document");
      Register_Routine (T, Single_Line_Scalar'Access, "Single line scalar");
      Register_Routine (T, Multiline_Scalar'Access, "Multiline scalar");
      Register_Routine (T, Single_Line_Mapping'Access, "Single line mapping");
      Register_Routine (T, Multiline_Mapping'Access, "Multiline mapping");
      Register_Routine (T, Explicit_Mapping'Access, "Explicit mapping");
      Register_Routine (T, Sequence'Access, "Sequence");
      Register_Routine (T, Sequence_With_Block_Mappings'Access, "Sequence with block mappings");
      Register_Routine (T, Single_Quoted_Scalar'Access, "Single-line single quoted scalar");
      Register_Routine (T, Multiline_Single_Quoted_Scalar'Access, "Multiline single quoted scalar");
      Register_Routine (T, Double_Quoted_Scalar'Access, "Single-line double quoted scalar");
      Register_Routine (T, Multiline_Double_Quoted_Scalar'Access, "Multiline double quoted scalar");
      Register_Routine (T, Escape_Sequences'Access, "Escape sequences");
      Register_Routine (T, Block_Scalar'Access, "Block scalar");
      Register_Routine (T, Block_Scalars'Access, "Block scalars");
      Register_Routine (T, Directives'Access, "Directives");
      Register_Routine (T, Markers'Access, "Markers and unknown directives");
      Register_Routine (T, Flow_Indicators'Access, "Flow indicators");
      Register_Routine (T, Adjacent_Map_Values'Access, "Adjacent map values (JSON-style)");
      Register_Routine (T, Tag_Handles'Access, "Tag handles");
      Register_Routine (T, Verbatim_Tag_Handle'Access, "Verbatim tag handle");
      Register_Routine (T, Anchors_And_Aliases'Access, "Anchors and aliases");
      Register_Routine (T, Empty_Lines'Access, "Empty lines");
   end Register_Tests;

   function Name (T : TC) return Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Tokenization tests for Lexer");
   end Name;

   procedure Set_Up (T : in out TC) is
   begin
      T.Pool.Create (8092);
   end Set_Up;

   procedure Empty_Document (T : in out Test_Cases.Test_Case'Class) is
   begin
      Assert_Equals (TC (T).Pool, "", (1 => TStrE));
   end Empty_Document;

   procedure Single_Line_Scalar (T : in out Test_Cases.Test_Case'Class) is
   begin
      Assert_Equals (TC (T).Pool, "scalar",
                     (TI (0), TPS (T, "scalar"), TStrE));
   end Single_Line_Scalar;

   procedure Multiline_Scalar (T : in out Test_Cases.Test_Case'Class) is
   begin
      Assert_Equals (TC (T).Pool, "scalar" & Line_Feed & "  line two",
                     (TI (0), TPS (T, "scalar line two"), TStrE));
   end Multiline_Scalar;

   procedure Single_Line_Mapping (T : in out Test_Cases.Test_Case'Class) is
   begin
      Assert_Equals (TC (T).Pool, "key: value",
                     (TI (0), TPS (T, "key"), TMV, TPS (T, "value"), TStrE));
   end Single_Line_Mapping;

   procedure Multiline_Mapping (T : in out Test_Cases.Test_Case'Class) is
   begin
      Assert_Equals (TC (T).Pool, "key:" & Line_Feed & "  value",
                     (TI (0), TPS (T, "key"), TMV, TI (2), TPS (T, "value"),
                      TStrE));
   end Multiline_Mapping;

   procedure Explicit_Mapping (T : in out Test_Cases.Test_Case'Class) is
   begin
      Assert_Equals (TC (T).Pool, "? key" & Line_Feed & ": value",
                     (TI (0), TMK, TPS (T, "key"), TI (0), TMV,
                      TPS (T, "value"), TStrE));
   end Explicit_Mapping;

   procedure Sequence (T : in out Test_Cases.Test_Case'Class) is
   begin
      Assert_Equals (TC (T).Pool, "- a" & Line_Feed & "- b",
                     (TI (0), TSI, TPS (T, "a"), TI (0), TSI, TPS (T, "b"),
                      TStrE));
   end Sequence;

   procedure Sequence_With_Block_Mappings (T : in out Test_Cases.Test_Case'Class) is
   begin
      Assert_Equals (TC (T).Pool, "-" & Line_Feed & "  avg:  0.228",
                     (TI (0), TSI, TI (2), TPS (T, "avg"), TMV,
                      TPS (T, "0.228"), TStrE));
   end Sequence_With_Block_Mappings;

   procedure Single_Quoted_Scalar (T : in out Test_Cases.Test_Case'Class) is
   begin
      Assert_Equals (TC (T).Pool, "'quoted scalar'",
                     (TI (0), TSQS (T, "quoted scalar"), TStrE));
   end Single_Quoted_Scalar;

   procedure Multiline_Single_Quoted_Scalar
     (T : in out Test_Cases.Test_Case'Class) is
   begin
      Assert_Equals (TC (T).Pool, "'quoted" & Line_Feed &
                       "  multi line  " & Line_Feed & Line_Feed & "scalar'",
                     (TI (0),
                      TSQS (T, "quoted multi line" & Line_Feed & "scalar"),
                      TStrE));
   end Multiline_Single_Quoted_Scalar;

   procedure Double_Quoted_Scalar (T : in out Test_Cases.Test_Case'Class) is
   begin
      Assert_Equals (TC (T).Pool, """quoted scalar""",
                     (TI (0), TDQS (T, "quoted scalar"), TStrE));
   end Double_Quoted_Scalar;

   procedure Multiline_Double_Quoted_Scalar
     (T : in out Test_Cases.Test_Case'Class) is
   begin
      Assert_Equals (TC (T).Pool, """quoted" & Line_Feed &
                       "  multi line  " & Line_Feed & Line_Feed & "scalar""",
                     (TI (0),
                      TDQS (T, "quoted multi line" & Line_Feed & "scalar"),
                      TStrE));
   end Multiline_Double_Quoted_Scalar;

   procedure Escape_Sequences (T : in out Test_Cases.Test_Case'Class) is
   begin
      Assert_Equals (TC (T).Pool, """\n\x31\u0032\U00000033""",
                     (TI (0), TDQS (T, Line_Feed & "123"), TStrE));
   end Escape_Sequences;

   procedure Block_Scalar (T : in out Test_Cases.Test_Case'Class) is
   begin
      Assert_Equals (TC (T).Pool, "|" & Line_Feed & "  a" & Line_Feed &
                       Line_Feed & "  b" & Line_Feed & " # comment",
                     (TI (0),
                      TLS (T, "a" & Line_Feed & Line_Feed & "b" & Line_Feed),
                      TStrE));
   end Block_Scalar;

   procedure Block_Scalars (T : in out Test_Cases.Test_Case'Class) is
   begin
      Assert_Equals (TC (T).Pool, "one : >2-" & Line_Feed & "   foo" &
                       Line_Feed & "  bar" & Line_Feed & "two: |+" & Line_Feed &
                       " bar" & Line_Feed & "  baz" & Line_Feed & Line_Feed,
                     (TI (0), TPS (T, "one"), TMV, TFS (T, " foo bar"), TI (0),
                      TPS (T, "two"), TMV,
                      TLS (T, "bar" & Line_Feed & " baz" & Line_Feed & Line_Feed),
                      TStrE));
   end Block_Scalars;

   procedure Directives (T : in out Test_Cases.Test_Case'Class) is
   begin
      Assert_Equals (TC (T).Pool, "%YAML 1.3" & Line_Feed & "---" &
                       Line_Feed & "%TAG" & Line_Feed & "..." & Line_Feed &
                       Line_Feed & "%TAG ! example%20.html",
                     (TYD, TDP (T, "1.3"), TDirE, TI (0), TPS (T, "%TAG"),
                      TDocE, TTD, TTH (T, "!"), TTU (T, "example .html"),
                      TStrE));
   end Directives;

   procedure Markers (T : in out Test_Cases.Test_Case'Class) is
   begin
      Assert_Equals (TC (T).Pool, "---" & Line_Feed & "---" & Line_Feed &
                       "..." & Line_Feed & "%UNKNOWN warbl",
                     (TDirE, TDirE, TDocE, TUD (T, "UNKNOWN"),
                      TDP (T, "warbl"), TStrE));
   end Markers;

   procedure Flow_Indicators (T : in out Test_Cases.Test_Case'Class) is
   begin
      Assert_Equals (TC (T).Pool, "bla]: {c: d, [e]: f}",
                     (TI (0), TPS (T, "bla]"), TMV, TMS, TPS (T, "c"), TMV,
                      TPS (T, "d"), TSep, TSS, TPS (T, "e"), TSE, TMV,
                      TPS (T, "f"), TME, TStrE));
   end Flow_Indicators;

   procedure Adjacent_Map_Values (T : in out Test_Cases.Test_Case'Class) is
   begin
      Assert_Equals (TC (T).Pool, "{""foo"":bar, [1]" & Line_Feed &
                       ":egg}",
                     (TI (0), TMS, TDQS (T, "foo"), TMV, TPS (T, "bar"), TSep,
                      TSS, TPS (T, "1"), TSE, TMV, TPS (T, "egg"), TME, TStrE));
   end Adjacent_Map_Values;

   procedure Tag_Handles (T : in out Test_Cases.Test_Case'Class) is
   begin
      Assert_Equals (TC (T).Pool, "- !!str string" & Line_Feed &
                       "- !local%21 local" & Line_Feed & "- !e! e",
                     (TI (0), TSI, TTH (T, "!!"), TTU (T, "str"),
                      TPS (T, "string"), TI (0), TSI, TTH (T, "!"),
                      TTU (T, "local!"), TPS (T, "local"), TI (0), TSI,
                      TTH (T, "!e!"), TTU (T, ""), TPS (T, "e"), TStrE));
   end Tag_Handles;

   procedure Verbatim_Tag_Handle (T : in out Test_Cases.Test_Case'Class) is
   begin
      Assert_Equals (TC (T).Pool, "!<tag:yaml.org,2002:str> string",
                     (TI (0), TTV (T, "tag:yaml.org,2002:str"),
                      TPS (T, "string"), TStrE));
   end Verbatim_Tag_Handle;

   procedure Anchors_And_Aliases (T : in out Test_Cases.Test_Case'Class) is
   begin
      Assert_Equals (TC (T).Pool, "&a foo: {&b b: *a, *b : c}",
                     (TI (0), TAn (T, "a"), TPS (T, "foo"), TMV, TMS,
                      TAn (T, "b"), TPS (T, "b"), TMV, TAli (T, "a"), TSep,
                      TAli (T, "b"), TMV, TPS (T, "c"), TME, TStrE));
   end Anchors_And_Aliases;

   procedure Empty_Lines (T : in out Test_Cases.Test_Case'Class) is
   begin
      Assert_Equals (TC (T).Pool, "block: foo" & Line_Feed & Line_Feed &
                       "  bar" & Line_Feed & Line_Feed & "    baz" & Line_Feed &
                       "flow: {" & Line_Feed & "  foo" & Line_Feed & Line_Feed &
                       "  bar: baz" & Line_Feed & Line_Feed & Line_Feed &
                       "  mi" & Line_Feed & "}",
                     (TI (0), TPS (T, "block"), TMV,
                      TPS (T, "foo" & Line_Feed & "bar" & Line_Feed & "baz"),
                      TI (0), TPS (T, "flow"), TMV, TMS,
                      TPS (T, "foo" & Line_Feed & "bar"), TMV,
                      TPS (T, "baz" & Line_Feed & Line_Feed & "mi"),
                     TME, TStrE));
   end Empty_Lines;
end Yaml.Lexer.Tokenization_Test;
