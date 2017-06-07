with Ada.Finalization;
with Ada.Unchecked_Deallocation;
with AUnit.Assertions; use AUnit.Assertions;

package body Yada.Lexing.Tokenization_Test is
   subtype Evaluated_Token is Token with Static_Predicate =>
     Evaluated_Token in Scalar | Tag_Uri | Verbatim_Tag;
   subtype Short_Lexeme_Token is Token with Static_Predicate =>
     Short_Lexeme_Token in Unknown_Directive | Anchor | Alias;
   subtype Full_Lexeme_Token is Token with Static_Predicate =>
     Full_Lexeme_Token in Directive_Param | Tag_Handle;
   subtype Value_Token is Token with Static_Predicate =>
     Value_Token in Evaluated_Token | Short_Lexeme_Token | Full_Lexeme_Token;
   subtype Empty_Token is Token with Static_Predicate =>
     not (Empty_Token in Value_Token | Indentation);

   type String_Access is access String;

   type Token_With_Value_Holder (Kind : Token) is record
      Refcount : Natural := 1;
      case Kind is
         when Value_Token => Content : String_Access;
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
      procedure Free_S is new Ada.Unchecked_Deallocation (String, String_Access);
      procedure Free_T is new Ada.Unchecked_Deallocation
        (Token_With_Value_Holder, Token_With_Value_Holder_Access);
      Reference : Token_With_Value_Holder_Access := Object.Reference;
   begin
      Object.Reference := null;
      if Reference /= null then
         Reference.Refcount := Reference.Refcount - 1;
         if Reference.Refcount = 0 then
            if Reference.Kind in Value_Token then
               Free_S (Reference.Content);
            end if;
            Free_T (Reference);
         end if;
      end if;
   end Finalize;

   function To_String (T : Token_With_Value) return String is
    (T.Reference.Kind'Img &
      (case T.Reference.Kind is
          when Value_Token => '(' & Escaped (T.Reference.Content.all) & ')',
          when Indentation => '(' & T.Reference.Depth'Img & ')',
          when others => ""));

   type Token_List is array (Positive range <>) of Token_With_Value;

   function TI (Indent : Natural) return Token_With_Value is
     (Token_With_Value'(Ada.Finalization.Controlled with Reference =>
                             new Token_With_Value_Holder'(Refcount => 1,
                                                          Kind => Indentation,
                                                          Depth => Indent)));
   function With_String (T : Value_Token; S : String) return Token_With_Value is
   begin
      return V : constant Token_With_Value :=
        (Ada.Finalization.Controlled with Reference => new Token_With_Value_Holder (Kind => T)) do
         V.Reference.Refcount := 1;
         V.Reference.Content := new String (S'Range);
         V.Reference.Content.all := S;
      end return;
   end With_String;

   function TS (Content : String) return Token_With_Value is
     (With_String (Scalar, Content));
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

   function TDP (Content : String) return Token_With_Value is
     (With_String (Directive_Param, Content));
   function TTH (Content : String) return Token_With_Value is
     (With_String (Tag_Handle, Content));
   function TTV (Content : String) return Token_With_Value is
     (With_String (Verbatim_Tag, Content));
   function TTU (Content : String) return Token_With_Value is
     (With_String (Tag_Uri, Content));
   function TUD (Content : String) return Token_With_Value is
     (With_String (Unknown_Directive, Content));
   function TAn (Content : String) return Token_With_Value is
     (With_String (Anchor, Content));
   function TAli (Content : String) return Token_With_Value is
     (With_String (Alias, Content));

   function To_String (L : Lexer; T : Token) return String is
     (T'Img & (case T is
         when Evaluated_Token => '(' & Escaped (L.Content.all) & ')',
         when Short_Lexeme_Token => '(' & Escaped (Short_Lexeme (L)) & ')',
         when Full_Lexeme_Token => '(' & Escaped (Full_Lexeme (L)) & ')',
         when Empty_Token => "",
         when Indentation => '(' & Natural'(L.Pos - L.Line_Start - 1)'Img & ')'));

   procedure Assert_Equals (Input : String; Expected : Token_List) is
      L : Lexer := From_String (Input);
      I : Natural := 0;
   begin
      for Expected_Token of Expected loop
         I := I + 1;
         declare
            T : constant Token := Next_Token (L);
         begin
            Assert (T = Expected_Token.Reference.Kind,
                    "Wrong token kind at #" & I'Img & ": Expected " &
                      To_String (Expected_Token) & ", got " &
                      To_String (L, T));
            if T = Expected_Token.Reference.Kind then
               case T is
               when Evaluated_Token =>
                  Assert (L.Content.all = Expected_Token.Reference.Content.all,
                          "Wrong content at #" & I'Img & ": Expected " &
                            Escaped (Expected_Token.Reference.Content.all) &
                            ", got " & Escaped (L.Content.all));
               when Full_Lexeme_Token =>
                  Assert (Full_Lexeme (L) = Expected_Token.Reference.Content.all,
                          "Wrong " & T'Img & " at #" & I'Img & ": Expected " &
                            Escaped (Expected_Token.Reference.Content.all) &
                            ", got " & Escaped (Full_Lexeme (L)));
               when Short_Lexeme_Token =>
                  Assert (Short_Lexeme (L) = Expected_Token.Reference.Content.all,
                          "Wrong " & T'Img & "at #" & I'Img & ": Expected " &
                            Escaped (Expected_Token.Reference.Content.all) &
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

   procedure Empty_Document (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      Assert_Equals ("", (1 => TStrE));
   end Empty_Document;

   procedure Single_Line_Scalar (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      Assert_Equals ("scalar", (TI (0), TS ("scalar"), TStrE));
   end Single_Line_Scalar;

   procedure Multiline_Scalar (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      Assert_Equals ("scalar" & Line_Feed & "  line two",
                     (TI (0), TS ("scalar line two"), TStrE));
   end Multiline_Scalar;

   procedure Single_Line_Mapping (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      Assert_Equals ("key: value", (TI (0), TS ("key"), TMV, TS ("value"),
                     TStrE));
   end Single_Line_Mapping;

   procedure Multiline_Mapping (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      Assert_Equals ("key:" & Line_Feed & "  value",
                     (TI (0), TS ("key"), TMV, TI (2), TS ("value"), TStrE));
   end Multiline_Mapping;

   procedure Explicit_Mapping (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      Assert_Equals ("? key" & Line_Feed & ": value",
                     (TI (0), TMK, TS ("key"), TI (0), TMV, TS ("value"),
                      TStrE));
   end Explicit_Mapping;

   procedure Sequence (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      Assert_Equals ("- a" & Line_Feed & "- b",
                     (TI (0), TSI, TS ("a"), TI (0), TSI, TS ("b"), TStrE));
   end Sequence;

   procedure Single_Quoted_Scalar (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      Assert_Equals ("'quoted scalar'", (TI (0), TS ("quoted scalar"), TStrE));
   end Single_Quoted_Scalar;

   procedure Multiline_Single_Quoted_Scalar
     (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      Assert_Equals ("'quoted" & Line_Feed & "  multi line  " & Line_Feed &
                       Line_Feed & "scalar'",
                     (TI (0), TS ("quoted multi line" & Line_Feed & "scalar"),
                      TStrE));
   end Multiline_Single_Quoted_Scalar;

   procedure Double_Quoted_Scalar (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      Assert_Equals ("""quoted scalar""", (TI (0), TS ("quoted scalar"), TStrE));
   end Double_Quoted_Scalar;

   procedure Multiline_Double_Quoted_Scalar
     (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      Assert_Equals ("""quoted" & Line_Feed & "  multi line  " & Line_Feed &
                       Line_Feed & "scalar""",
                     (TI (0), TS ("quoted multi line" & Line_Feed & "scalar"),
                      TStrE));
   end Multiline_Double_Quoted_Scalar;

   procedure Escape_Sequences (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      Assert_Equals ("""\n\x31\u0032\U00000033""",
                     (TI (0), TS (Line_Feed & "123"), TStrE));
   end Escape_Sequences;

   procedure Block_Scalar (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      Assert_Equals ("|" & Line_Feed & "  a" & Line_Feed & Line_Feed & "  b" &
                       Line_Feed & " # comment",
                     (TI (0),
                      TS ("a" & Line_Feed & Line_Feed & "b" & Line_Feed),
                      TStrE));
   end Block_Scalar;

   procedure Block_Scalars (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      Assert_Equals ("one : >2-" & Line_Feed & "   foo" & Line_Feed & "  bar" &
                       Line_Feed & "two: |+" & Line_Feed & " bar" & Line_Feed &
                       "  baz" & Line_Feed & Line_Feed,
                     (TI (0), TS ("one"), TMV, TS (" foo bar"), TI (0),
                      TS ("two"), TMV,
                      TS ("bar" & Line_Feed & " baz" & Line_Feed & Line_Feed),
                      TStrE));
   end Block_Scalars;

   procedure Directives (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      Assert_Equals ("%YAML 1.3" & Line_Feed & "---" & Line_Feed & "%TAG" &
                       Line_Feed & "..." & Line_Feed & Line_Feed &
                       "%TAG ! example%20.html",
                     (TYD, TDP ("1.3"), TDirE, TI (0), TS ("%TAG"), TDocE,
                      TTD, TTH ("!"), TTU ("example .html"), TStrE));
   end Directives;

   procedure Markers (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      Assert_Equals ("---" & Line_Feed & "---" & Line_Feed & "..." &
                       Line_Feed & "%UNKNOWN warbl",
                     (TDirE, TDirE, TDocE, TUD ("UNKNOWN"),
                      TDP ("warbl"), TStrE));
   end Markers;

   procedure Flow_Indicators (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      Assert_Equals ("bla]: {c: d, [e]: f}",
                     (TI (0), TS ("bla]"), TMV, TMS, TS ("c"), TMV, TS ("d"),
                      TSep, TSS, TS ("e"), TSE, TMV, TS ("f"), TME, TStrE));
   end Flow_Indicators;

   procedure Adjacent_Map_Values (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      Assert_Equals ("{""foo"":bar, [1]" & Line_Feed & ":egg}",
                     (TI (0), TMS, TS ("foo"), TMV, TS ("bar"), TSep, TSS,
                      TS ("1"), TSE, TMV, TS ("egg"), TME, TStrE));
   end Adjacent_Map_Values;

   procedure Tag_Handles (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      Assert_Equals ("- !!str string" & Line_Feed & "- !local%21 local" &
                       Line_Feed & "- !e! e",
                     (TI (0), TSI, TTH ("!!"), TTU ("str"), TS ("string"),
                      TI (0), TSI, TTH ("!"), TTU ("local!"), TS ("local"),
                      TI (0), TSI, TTH ("!e!"), TTU (""), TS ("e"), TStrE));
   end Tag_Handles;

   procedure Verbatim_Tag_Handle (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      Assert_Equals ("!<tag:yaml.org,2002:str> string",
                     (TI (0), TTV ("tag:yaml.org,2002:str"), TS ("string"),
                      TStrE));
   end Verbatim_Tag_Handle;

   procedure Anchors_And_Aliases (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      Assert_Equals ("&a foo: {&b b: *a, *b : c}",
                     (TI (0), TAn ("a"), TS ("foo"), TMV, TMS, TAn ("b"),
                      TS ("b"), TMV, TAli ("a"), TSep, TAli ("b"), TMV,
                      TS ("c"), TME, TStrE));
   end Anchors_And_Aliases;

   procedure Empty_Lines (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      Assert_Equals ("block: foo" & Line_Feed & Line_Feed & "  bar" &
                       Line_Feed & Line_Feed & "    baz" & Line_Feed &
                       "flow: {" & Line_Feed & "  foo" & Line_Feed & Line_Feed &
                       "  bar: baz" & Line_Feed & Line_Feed & Line_Feed &
                       "  mi" & Line_Feed & "}",
                     (TI (0), TS ("block"), TMV, TS ("foo" & Line_Feed & "bar" &
                        Line_Feed & "baz"), TI (0), TS ("flow"), TMV, TMS,
                     TS ("foo" & Line_Feed & "bar"), TMV, TS ("baz" & Line_Feed & Line_Feed & "mi"),
                     TME, TStrE));
   end Empty_Lines;
end Yada.Lexing.Tokenization_Test;
