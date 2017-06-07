with Ada.Finalization;
with Ada.Unchecked_Deallocation;
with AUnit.Assertions; use AUnit.Assertions;

package body Yada.Lexing.Tokenization_Test is
   subtype Value_Token is Token with Static_Predicate =>
     Value_Token in Directive_Param | Unknown_Directive | Scalar | Tag_Handle |
                    Tag_Suffix | Literal_Tag | Anchor | Alias;
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
   TSE : constant Token_With_Value :=
     (Ada.Finalization.Controlled with Reference =>
         new Token_With_Value_Holder'(Refcount => 1, Kind => Stream_End));
   TMK : constant Token_With_Value :=
     (Ada.Finalization.Controlled with Reference =>
         new Token_With_Value_Holder'(Refcount => 1, Kind => Map_Key_Ind));
   TMV : constant Token_With_Value :=
     (Ada.Finalization.Controlled with Reference =>
         new Token_With_Value_Holder'(Refcount => 1, Kind => Map_Value_Ind));
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
                      T'Img);
            if T = Expected_Token.Reference.Kind then
               case T is
               when Scalar =>
                  Assert (L.Scalar_Content.all = Expected_Token.Reference.Content.all,
                          "Wrong scalar content at #" & I'Img & ": Expected " &
                            Escaped (Expected_Token.Reference.Content.all) & ", got " &
                            Escaped (L.Scalar_Content.all));
               when Directive_Param =>
                  Assert (Full_Lexeme (L) = Expected_Token.Reference.Content.all,
                          "Wrong directive param at #" & I'Img & ": Expected " &
                            Escaped (Expected_Token.Reference.Content.all) & ", got " &
                            Escaped (Full_Lexeme (L)));
               when Indentation =>
                  Assert (L.Pos - L.Line_Start - 1 = Expected_Token.Reference.Depth,
                          "Wrong indentation at #" & I'Img & ": Expected" &
                            Expected_Token.Reference.Depth'Img & ", got " &
                            Integer'(L.Pos - L.Line_Start - 2)'Img);
               when Empty_Token => null;
               when others =>
                  Assert (False, "Not implemented: " & T'Img);
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
   end Register_Tests;

   function Name (T : TC) return Message_String is
      pragma Unreferenced (T);
   begin
      return AUnit.Format ("Tokenization tests for Lexer");
   end Name;

   procedure Empty_Document (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      Assert_Equals ("", (1 => TSE));
   end Empty_Document;

   procedure Single_Line_Scalar (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      Assert_Equals ("scalar", (TI (0), TS ("scalar"), TSE));
   end Single_Line_Scalar;

   procedure Multiline_Scalar (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      Assert_Equals ("scalar" & Line_Feed & "  line two",
                     (TI (0), TS ("scalar line two"), TSE));
   end Multiline_Scalar;

   procedure Single_Line_Mapping (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      Assert_Equals ("key: value", (TI (0), TS ("key"), TMV, TS ("value"), TSE));
   end Single_Line_Mapping;

   procedure Multiline_Mapping (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      Assert_Equals ("key:" & Line_Feed & "  value",
                     (TI (0), TS ("key"), TMV, TI (2), TS ("value"), TSE));
   end Multiline_Mapping;

   procedure Explicit_Mapping (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      Assert_Equals ("? key" & Line_Feed & ": value",
                     (TI (0), TMK, TS ("key"), TI (0), TMV, TS ("value"), TSE));
   end Explicit_Mapping;

   procedure Sequence (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      Assert_Equals ("- a" & Line_Feed & "- b",
                     (TI (0), TSI, TS ("a"), TI (0), TSI, TS ("b"), TSE));
   end Sequence;

   procedure Single_Quoted_Scalar (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      Assert_Equals ("'quoted scalar'", (TI (0), TS ("quoted scalar"), TSE));
   end Single_Quoted_Scalar;

   procedure Multiline_Single_Quoted_Scalar
     (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      Assert_Equals ("'quoted" & Line_Feed & "  multi line  " & Line_Feed &
                       Line_Feed & "scalar'",
                     (TI (0), TS ("quoted multi line" & Line_Feed & "scalar"),
                      TSE));
   end Multiline_Single_Quoted_Scalar;

   procedure Double_Quoted_Scalar (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      Assert_Equals ("""quoted scalar""", (TI (0), TS ("quoted scalar"), TSE));
   end Double_Quoted_Scalar;

   procedure Multiline_Double_Quoted_Scalar
     (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      Assert_Equals ("""quoted" & Line_Feed & "  multi line  " & Line_Feed &
                       Line_Feed & "scalar""",
                     (TI (0), TS ("quoted multi line" & Line_Feed & "scalar"),
                      TSE));
   end Multiline_Double_Quoted_Scalar;

   procedure Escape_Sequences (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      Assert_Equals ("""\n\x31\u0032\U00000033""",
                     (TI (0), TS (Line_Feed & "123"), TSE));
   end Escape_Sequences;

   procedure Block_Scalar (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
   begin
      Assert_Equals ("|" & Line_Feed & "  a" & Line_Feed & Line_Feed & "  b" &
                       Line_Feed & " # comment",
                     (TI (0), TS ("a" & Line_Feed & Line_Feed & "b" & Line_Feed), TSE));
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
                      TSE));
   end Block_Scalars;
end Yada.Lexing.Tokenization_Test;
